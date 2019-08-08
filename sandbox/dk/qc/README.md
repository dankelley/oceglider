The `qc*.R` files contain a series of versions of a QC (quality-control) app,
designed to work with SeaExplorer data.

The app is in rapid development, as of the late summer of 2019. The author
works with a version named `qcbeta.R`, but others ought to work with the
highest-numbered version, which on Aug 8th is `qc07.R`.

Please note that `.rda` files created by one version might not work fully in a
succeeding version. For example, version `qc08.R` got a new colour-by menu item
called `"instability"`, and that was not computed by `qc07.R`, so a `.rda` file
made by `qc07.R` cannot be coloured in this way by `qc08.R`.

See
https://github.com/dankelley/oceanglider/issues?q=is%3Aissue+is%3Aopen+label%3A%22QC+app%22
for issues related to this app.

