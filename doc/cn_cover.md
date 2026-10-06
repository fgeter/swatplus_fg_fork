# Cover-driven daily curve number (cn_cover)

SWAT+ normally takes `cn2` from one fixed `cntable.lum` row per land use, and after that only
soil water moves the daily curve number. A corn field has the same `cn2` the day after spring
tillage as under a full August canopy.

The cn_cover approach keeps the table value as the **average over the simulation**, but moves
`cn2` every day with the surface cover: above-ground residue plus the near-surface part of the
living canopy. Fields run off more when bare or freshly tilled and less under a full canopy. The
soil-water machinery downstream (`curno`, `sq_dailycn`) is untouched.

It is off by default.

## Why the table value is treated as an average

The NRCS tables (NEH Part 630 ch. 9, §630.0901) were built by finding, for small single-cover
watersheds, the CN that reproduced the runoff of the **storms producing the annual floods**, then
averaging across watersheds. A row such as *row crops, straight row, good* is therefore the
median CN of a management system over a year's flood storms, not the state of the surface on any
day. NEH 630 ch. 10 (§630.1002(d)) names "cover density, stage of growth" among the causes of the
scatter around that median; SWAT+ otherwise attributes all of that scatter to soil moisture.

## Turning it on

`codes.bsn` column **`cn`** — previously unused, written as 0 by every editor:

| value | behaviour |
|---|---|
| `0` | current behaviour. Nothing in `cn_cover_module` is allocated; output is bit-identical to a build without this feature. |
| `1` | cover method. **Requires `plants.cov`** in the project directory. |
| `2` | as `1`, plus the daily audit file `cn_cover.out`. |

Any other value is an error stop at startup. With `cn = 1` or `2`, the defaults below apply; the
optional `cn_cover.prm` overrides them.

## The curve

### Cover indices

| symbol | meaning |
|---|---|
| `c_rsd` | residue cover, `1 - exp(-k_rsd * rsd)`, with `rsd` the above-ground residue in kg/ha |
| `c_bio` | near-surface living-biomass cover: the APEX ground-cover S-curve on `sum(ab_gr * exp(-k_ns * cht))` |
| `c_tot` | combined cover, `1 - (1-c_rsd)(1-c_bio)` |
| `x_tot` | `min(c_tot / c_sat, 1)`: all cover |
| `x_bio` | `min(c_bio / c_sat, x_tot)`: living canopy only |

### Three points, two legs

```
cn2 = cn_hi + x_tot * (cn_mid - cn_hi) + x_bio * (cn_lo - cn_mid)

cn_hi  = CN(fallow-poor row)          + offset        bare: no residue, no canopy
cn_mid = CN(fallow-good-residue row)  + offset        full residue cover, no canopy
cn_lo  = CN(table) * (1 - lo_pct)                     full cover under a full canopy
offset = CN(table) - CN(non-residue straight row of the same family and condition)
```

* **Residue leg (high → middle).** Any cover, residue or canopy, moves `cn2` from the bare value
  toward the middle point. Residue alone goes no further than the middle.
* **Canopy leg (middle → low).** Only living canopy carries `cn2` on to the low end.

Without the canopy requirement, residue alone saturates the cover index for about half of all
days, and the low end becomes the typical state instead of the extreme.

All CNs are read in the column for the HRU's hydrologic soil group. The fallow rows are
`fal_res_p` / `fal_res_g` in SWAT+-editor tables and `fal_p` / `fal_g` in HUC8-constructor tables;
both are recognised.

### The treatment offset

The fallow rows exist only for straight-row land. Contouring, terracing and crop-residue
treatments carry an advantage over straight row, and that advantage should survive when the field
is bare. Both anchors are therefore shifted by the land use's offset from the **non-residue**
straight row (`strow` in editor tables, `sr` in HUC8 tables).

Examples, HSG B (fallow-poor 85, fallow-good 83), `lo_pct` 0.057:

| land-use row | table | reference | offset | cn_hi | cn_mid | cn_lo |
|---|---|---|---|---|---|---|
| `rc_sr_g` | 78 | `rc_sr_g` 78 | 0 | 85 | 83 | 73.6 |
| `rc_sr_cr_g` | 75 | `rc_sr_g` 78 | −3 | 82 | 80 | 70.7 |
| `rc_c_t_cr_g` | 70 | `rc_sr_g` 78 | −8 | 77 | 75 | 66.0 |

**Why the non-residue reference.** Measuring `rc_sr_cr_g` against `rc_sr_cr_g` itself (`off_ref`
1) gives it the same anchors as plain straight row. Its 3-point table residue credit then
disappears at the anchors, while simulated residue cover credits residue again: residue is
counted twice. On the calibrated Raccoon watershed, that left crop-residue rows on HSG B at +16%
unfrozen-day runoff and plain rows on HSG C at −10%, cancelling only in the basin total. With the
non-residue reference (`off_ref` 2, the default), crop-residue and plain rows on the same soil
agree within 1–2%.

### Which HRUs follow the curve

An HRU follows the curve if its `cntable.lum` row has a hydrologic condition (`_p` / `_f` / `_g`)
and a non-residue straight-row reference in its family: row crops, small grains, close-seeded
crops, legumes. Every other HRU is held at its table value every day:

* pasture, meadow and brush, whose condition is defined by grazing;
* woods, urban, residential-impervious, roads, farmsteads;
* fallow land uses.

### Frozen days

On frozen soil (layer 2 at or below 0 °C), `sq_dailycn` switches to its frozen-soil branch, which
still scales from `smx` and therefore from `cn2`. Winter residue would hold `cn2` near the low end
and cut frozen-day runoff by 26–32%, an effect cover has no physical claim to. With `frz_hold = 1`
(the default), `cn2` is held at the table value on every day `sq_dailycn` treats as frozen.

### Choosing `lo_pct`

The high and middle points come from the table; `lo_pct` is the only fitted value. It is chosen
so that annual outlet flow on a calibrated watershed matches the static curve number. The default,
**0.057**, was fitted on the calibrated full Raccoon dataset (7949 HRUs, 1997–2006).

**`lo_pct` is not the Rawls et al. (1980) residue effect.** Rawls measured the percent change in CN
from residue against storms on the *same crop and season* with no residue. Canopy was present on
both sides of that comparison, so it cancels. His ~10% (natural rain) / 12.5% (simulated) is a
residue-leg quantity. It is in fact larger than the residue leg here (the fallow rows put full
residue about 2.3% below bare). `lo_pct` instead sets the full-canopy CN against the table
*average*, a quantity the paper does not measure.

Use one `lo_pct` per basin. Only outlet flow constrains it, and fitting it per land use against
the static model's runoff would pull each row back to static behaviour.

## Results on the calibrated Raccoon watershed

Full dataset, 7949 HRUs, 1997–2006, defaults vs the static curve number (`cn_curve 3`), with the
calibration's `cn2 abschg -3.078` applied to both:

| | change |
|---|---|
| annual outlet flow | −0.01% (monthly NSE vs static 0.998) |
| water yield | +0.07% |
| surface runoff | +0.6%, offset by tile flow −1.1% and percolation −0.7% |
| outlet sediment | +2.9% |
| nitrate in surface runoff | +12.9% |

The volume barely changes; the timing does:

| month | J | F | M | A | M | J | J | A | S | O | N | D |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| outlet flow, % | +2.5 | +0.9 | +0.8 | +1.8 | +5.5 | +3.3 | −1.9 | −6.8 | −8.8 | −7.3 | +1.3 | +7.9 |
| outlet sediment, % | +3.2 | +0.5 | +0.8 | +4.9 | +13.1 | +11.5 | +1.2 | −13.8 | −19.6 | −16.9 | −2.8 | +21.2 |

Runoff moves from the canopy months into the seedbed and post-harvest months, carrying more
sediment and, after spring fertiliser, more nitrate. `lo_pct` acts mainly on July–October; May,
June and December are set by the residue leg.

**Known limitation:** curve-following HRUs on HSG B gain about 10% unfrozen-day runoff and those on
HSG C about 0%. The fallow-good-residue row sits 5 points above row crops on B (83 vs 78) but 3 on
C (88 vs 85), so B fields swing further toward bare.

## Calibration is preserved

`curno` rebuilds the retention curve from `cn2` wholesale. Writing a fresh `cn2` every day would
therefore discard every `cn2` entry in `calibration.cal`, every `cnup` management operation, the
`cn_update` decision-table action, and `pl_burnop`'s fire adjustment.

Instead, the difference between `cn2` as the routine finds it and `cn2` as it left it the day
before is exactly what other code did. It accumulates into a per-HRU offset that is re-applied on
every write. A land-use change restarts the ledger, because the base row set changes with it. The
offset is visible in the `cn2_off` column of `cn_cover.out`.

## `plants.cov`

Required whenever `cn > 0`. It maps each plant to a `cntable.lum` family and gives its residue
cover coefficient. It may be a subset of `plants.plt`, in any order.

```
plants.cov: <provenance line>
name        cn_family   k_rsd
corn        rc          0.
soyb        rc          0.
wwht        sg          6.64e-4
brom        pastg       0.
```

**Every column carries a value on every row.** There are no optional columns and no short rows —
a row with fewer than three fields is an error stop naming the row and its text. Reading each
record into a buffer first is what makes that an error: a list-directed read straight off the unit
would run on into the *next* record to satisfy the missing item and silently swallow the following
plant.

* `cn_family` is the **`cntable.lum` row-name prefix**, validated at startup against the families
  actually present in that project's `cntable.lum`. Row names are not standardised across dataset
  generators — the SWAT+ editor writes `rc_strow_g`, `pastg_g`, `wood_g`, while the HUC8
  constructor writes `rc_sr_cr_g`, `past_g`, `frst_g` — so the valid tokens are whatever that
  project's table uses. Under the default method its only job is to select the default `k_rsd`;
  the curve itself comes from the HRU's land-use row.
* `k_rsd` (ha/kg) is the residue mass → cover coefficient for that plant. **`0.` means use the
  family default**: 6.64e-4 for the `sg` family, 2.657e-4 otherwise. Those reproduce the NRCS
  thresholds — 20 % cover at 750 lb/ac for row crops and at 300 lb/ac for small grains. Write `0.`,
  not `0`, per the SWAT+ convention for a real field. When several plants leave residue, `k_rsd` is
  weighted by each plant's residue mass.
* Trailing text after the third field is ignored, as in every other SWAT+ input file. There is no
  whole-line comment syntax.
* Both directions of name mismatch are reported to `diagnostics.out`: plants in `plants.plt` with
  no row, and rows matching no plant.

## `cn_cover.prm`

Optional. A title line, a header line, and **one data row with all seven fields**. Without the
file, these defaults apply:

```
cn_cover.prm: <provenance line>
cn_curve  lo_pct  d_mid  hi_row     mid_row    frz_hold  off_ref
       2   0.057    0.0  fal_res_p  fal_res_g         1        2
```

| column | type | default | meaning |
|---|---|---|---|
| `cn_curve` | integer | 2 | 2 = the cover curve above. 3 = every HRU held at its table CN: the same `cn2` as `cn = 0`, but writing `cn_cover_sum.out`, so it serves as the static baseline for comparisons. 1 = the original condition-row method (`cn2` interpolated between the poor and good rows of each plant's family), kept for comparison. |
| `lo_pct` | real, 0 ≤ x < 1 | 0.057 | fractional reduction of the table CN at full cover and full canopy |
| `d_mid` | real, CN points | 0.0 | added to the middle-point anchor |
| `hi_row` | name | `fal_res_p` | `cntable.lum` row for the bare end; `fal_p` is tried if absent |
| `mid_row` | name | `fal_res_g` | row for the middle point; `fal_g` is tried if absent; `none` uses the table CN |
| `frz_hold` | integer | 1 | 1 = hold `cn2` at the table value on frozen days |
| `off_ref` | integer | 2 | 2 = treatment offset against the non-residue straight row; 1 = against the straight row of the same residue status (counts residue twice; for comparison only) |

A malformed row, `cn_curve` outside 1–3, `off_ref` outside 1–2, or `lo_pct` outside [0, 1) is an
error stop. A named anchor row missing from `cntable.lum` is an error stop. The values in effect
are echoed to `diagnostics.out`.

## `cn_cover.out`

Written only at `cn = 2`, one line per active HRU per day:

| column | meaning |
|---|---|
| `rsd` | above-ground residue, kg/ha (`pl_mass%abg_rsd_tot%m`) |
| `bio_ns` | near-surface living biomass, kg/ha — `sum(ab_gr * exp(-k_ns * cht))` |
| `c_rsd` | residue cover fraction |
| `c_bio` | biomass cover fraction |
| `c_tot` | combined cover fraction |
| `cn2_cov` | `cn2` from the curve, before the offset |
| `cn2_off` | accumulated external offset (calibration, `cnup`, burn) |
| `cn2` | what was handed to `curno` |

This file is one line per HRU per day and is not size-managed. Use `cn = 1` for production runs.

## `cn_cover_sum.out`

Written whenever `cn` is 1 or 2. Line 1 records the settings in effect, line 2 the column names,
then one line per HRU, written on the last day of the run. Wetland HRUs, which never call
`surface`, are absent.

| column | units | meaning |
|---|---|---|
| `unit` | — | HRU number |
| `wide` | T/F | whether the HRU followed the curve |
| `area_ha` | ha | HRU area |
| `cn_tbl` | CN | its table value |
| `cn_hi`, `cn_mid` | CN | its curve anchors (0 if not following the curve) |
| `q_unf` | mm | surface runoff summed over unfrozen days, whole simulation |
| `q_frz` | mm | the same over frozen days |

Comparing the area-weighted `q_unf` of a run against a `cn_curve 3` run of the same dataset shows
how much the cover curve moved unfrozen-day runoff.

## Coefficients

All in one block at the top of `src/cn_cover_module.f90`. They are module variables, not
parameters, so they can be calibrated later. Only the curve-shape settings in the table above
(`lo_pct` and the rest) are read from `cn_cover.prm` today.

| symbol | meaning | default | source |
|---|---|---|---|
| `k_ns` | canopy-height decay, 1/m | 0.328 | `ero_cfactor.f90` (APEX) |
| `k_rsd_row` | residue mass → cover, row crops, ha/kg | 2.657e-4 | NRCS 20 % at 750 lb/ac |
| `k_rsd_grain` | same, small grains | 6.64e-4 | NRCS 20 % at 300 lb/ac |
| `c_sat` | cover at which the effect plateaus | 0.60 | Rawls (1980) |
| `cn_floor`, `cn_ceil` | limits on the `cn2` written | 30, 98 | NEH 650-2.15 fn. 4; `cntable.lum` maximum |
| `lo_pct` | full-canopy reduction of the table CN | 0.057 | fitted, calibrated Raccoon |
| `cov_poor/fair/good` | breakpoints for `cn_curve 1` only | 0.50 / 0.625 / 0.75 | NEH 650-2.15 fn. 2, 3 |

## Routines and call sequence

| routine | role |
|---|---|
| `cn_cover_module` | settings, per-HRU state, row-name parsing, `cn_wide` (the curve) |
| `cn_cover_init` | parse `cntable.lum` names into family / treatment / condition; build the straight-row reference maps; resolve anchor rows; open output files |
| `cn_cover_prm_read` | read the optional `cn_cover.prm` |
| `cn_cover_read` | read `plants.cov` |
| `cn_cover_hru_init` | per HRU: soil group, table CN, anchors, whether it follows the curve; restart the offset ledger |
| `cn_cover_update` | daily: cover fractions → `cn2` from the curve → offset ledger → `curno` |
| `cn_cover_tally` | daily: add surface runoff to the frozen / unfrozen totals; write `cn_cover_sum.out` on the last day |

```
proc_db   -> cn_cover_init       parse cntable.lum, read cn_cover.prm and plants.cov  (startup)
cn2_init  -> cn_cover_hru_init   cache the HRU's rows and anchors       (startup + lu_change)
surface   -> cn_cover_update     re-seat cn2, then curno         (daily, before sq_dailycn)
surface   -> cn_cover_tally      frozen / unfrozen runoff totals    (daily, after surfq is final)
```
