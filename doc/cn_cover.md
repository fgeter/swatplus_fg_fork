# Cover-driven daily curve number

SWAT+ normally takes `cn2` from one fixed `cntable.lum` row per land use and never
looks at it again. Everything that moves the daily curve number after that is soil
water. This feature re-selects `cn2` every day from **above-ground residue plus the
near-surface part of the living canopy**, staying inside the hydrologic-condition
rows (`_p` / `_f` / `_g`) of the HRU's own cover family. The soil-water machinery
downstream is untouched.

It is off by default. The full design rationale, the NRCS grounding and the worked
examples live in the design note (`tmp/CN_cover_design.md`, not tracked).

## Turning it on

`codes.bsn` column **`cn`** — previously unused, written as 0 by every editor:

| value | behaviour |
|---|---|
| `0` | current behaviour. Nothing in `cn_cover_module` is allocated; output is bit-identical to a build without this feature. |
| `1` | cover method. **Requires `plants.cov`** in the project directory. |
| `2` | as `1`, plus the daily audit file `cn_cover.out`. |

Any other value is an error stop at startup.

## `plants.cov`

A new optional input file, read from the project directory. It maps each plant to
the `cntable.lum` family whose condition rows it should be interpolated within.
It may be a subset of `plants.plt`, in any order.

```
plants.cov: <provenance line>
name        cn_family   k_rsd
corn        rc          0.
soyb        rc          0.
wwht        sg          6.64e-4
brom        pastg       0.
```

**Every column carries a value on every row.** There are no optional columns and no
short rows — a row with fewer than three fields is an error stop naming the row and
its text. Reading each record into a buffer first is what makes that an error: a
list-directed read straight off the unit would run on into the *next* record to
satisfy the missing item and silently swallow the following plant.

* `cn_family` is the **`cntable.lum` row-name prefix**, validated at startup against
  the families actually present in that project's `cntable.lum`. A typo stops the run
  on day one instead of silently leaving a plant static. Row names are not
  standardised across dataset generators — the SWAT+ editor writes `rc_strow_g`,
  `pastg_g`, `wood_g`, while the HUC8 constructor writes `rc_sr_cr_g`, `past_g`,
  `frst_g` — so the valid tokens are whatever that project's table uses.
* `k_rsd` (ha/kg) is the residue mass → cover coefficient for that plant.
  **`0.` means use the family default**: 6.64e-4 for the `sg` family, 2.657e-4
  otherwise. Those two reproduce the NRCS thresholds — 20 % cover at 750 lb/ac for
  row crops and at 300 lb/ac for small grains. Write `0.`, not `0`, per the SWAT+
  convention for a real field.
* Trailing text after the third field is ignored, exactly as in every other SWAT+
  input file — the list-directed read stops once its io-list is satisfied — so a
  per-row evidence note needs no delimiter. There is no whole-line comment syntax.
* Both directions of name mismatch are reported to `diagnostics.out`: plants in
  `plants.plt` with no row, and rows matching no plant.

## Which HRUs take part

An HRU participates only if the `cntable.lum` row its land use points at has **both a
poor and a good variant for its own treatment**. That single rule holds urban,
farmsteads, meadow, the road rows, the residential percent-impervious rows and bare
fallow static, because none of them carry hydrologic conditions. Within a
participating HRU the growing plants may still pull the family sideways, which is how
a corn → soybean → wheat rotation moves between `rc` and `sg`.

Woods families (`wood`, `woodgr`, `frst`, `frse`, `frsd`, `orch`) are left static.
NRCS keys woods condition to grazing and burning history, not to a cover percentage,
and driving them from cover walks `cn2` down into the range where `curno`'s
`Max(cn1, .4*cnn)` clamp replaces the AMC I fit. The list is one `select case` in
`cn_cover_module.f90` (`fam_is_static`).

## Calibration is preserved

`curno` rebuilds the retention curve from `cn2` wholesale, so writing a fresh `cn2`
every day would discard every `cn2` entry in `calibration.cal`, every `cnup`
management operation, the `cn_update` d-table action and `pl_burnop`'s fire
adjustment. Instead the difference between `cn2` as the routine finds it and `cn2` as
it left it yesterday is exactly what someone else did, and it accumulates into a
per-HRU offset that is re-applied on every write. A land use change restarts the
ledger, because the base row set changes with it.

The offset is visible in the `cn2_off` column of `cn_cover.out`.

## `cn_cover.out`

Written only at `cn = 2`, one line per participating HRU per day:

| column | meaning |
|---|---|
| `rsd` | above-ground residue, kg/ha (`pl_mass%abg_rsd_tot%m`) |
| `bio_ns` | near-surface living biomass, kg/ha — `sum(ab_gr * exp(-k_ns * cht))` |
| `c_rsd` | residue cover fraction — `1 - exp(-k_rsd * rsd)` |
| `c_bio` | biomass cover fraction — APEX ground-cover S-curve on `bio_ns` |
| `c_tot` | combined — `1 - (1-c_rsd)(1-c_bio)` |
| `cn2_cov` | interpolated `cn2`, before the offset |
| `cn2_off` | accumulated external offset (calibration, `cnup`, burn) |
| `cn2` | what was handed to `curno` |

This file is one line per HRU per day and is not size-managed. Use `cn = 1` for
production runs.

## Coefficients

All in one block at the top of `src/cn_cover_module.f90`:

| symbol | meaning | default | source |
|---|---|---|---|
| `k_ns` | canopy-height decay, 1/m | 0.328 | `ero_cfactor.f90` (APEX) |
| `k_rsd_row` | residue mass → cover, row crops, ha/kg | 2.657e-4 | NRCS 20 % at 750 lb/ac |
| `k_rsd_grain` | same, small grains | 6.64e-4 | NRCS 20 % at 300 lb/ac |
| `c_sat` | cover at which the effect plateaus | 0.60 | Rawls (1980) |
| `cov_poor/fair/good` | three-point family breakpoints | 0.50 / 0.625 / 0.75 | NEH 650-2.15 fn. 2, 3 |

Arid and semiarid rangeland uses 0.30 / 0.50 / 0.70 under a different footnote. The
`cntable.lum` row set cannot distinguish it from pasture, so those `sar_*` families
currently use the pasture breakpoints.

## Call sequence

```
proc_db   -> cn_cover_init       parse cntable.lum, read plants.cov      (startup)
cn2_init  -> cn_cover_hru_init   cache the HRU row set                   (startup + lu_change)
surface   -> cn_cover_update     re-seat cn2, then curno                 (daily, before sq_dailycn)
```
