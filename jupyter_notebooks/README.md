# Jupyter Notebooks

Carbon analysis notebooks for SWAT+ output.

## Notebook handling in git

The notebooks in `jupyter_notebooks/` are stored in git **stripped**: no cell outputs, no
execution counts, no `language_info` metadata, and with cell IDs normalized to sequential
integers. Merely opening or re-running a notebook otherwise rewrites every cell ID, which
produces large meaningless diffs and needless merge conflicts.

This is enforced by [`nbstripout`](https://github.com/kynan/nbstripout) as a git clean
filter. The `.gitattributes` entries are committed, but **the filter itself lives in
per-clone local git config and is not carried by the repository.** Once, after cloning and
before opening or editing any notebook, run the following commands in your project root
folder:

```bash
$ pip install nbstripout
$ nbstripout --install
$ git config filter.nbstripout.extrakeys 'metadata.language_info metadata.kernelspec.display_name'
```

The third command is required in addition to `--install`; without it the filter leaves a
`language_info` block containing the local Python version, and a kernel `display_name`
naming whichever environment last ran the notebook (`.venv` in a devcontainer, `Python 3
(ipykernel)` elsewhere). Both churn between machines. Only `display_name` is stripped, not
the whole `kernelspec` -- `name: python3` stays and is what actually selects the kernel.

Verify the filter is active:

```bash
$ nbstripout --status
```

If it reports that nbstripout is not installed, committed notebooks will carry outputs and
randomized cell IDs, reintroducing the conflicts this setup exists to prevent.
