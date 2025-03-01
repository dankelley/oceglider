# oceglider 0.1.19

* Change `read.glider.netcdf()` to handle `rename` argument, in accordance with issue #126.

# oceglider 0.1.18

* Change `read.glider.seaexplorer.raw()` to handle filename and data-frame
values for the `rename` parameter.

# oceglider 0.1.17

* Change `read.glider.seaexplorer.raw()` to accept new parameter `rename`, in accordance with issue #126.

# oceglider 0.1.16

* Add `deleteStartupData()`, and remove the `removeTimeSincePowerOn` parameter
  from `read.glider.seaexplorer.raw()`, in accordance with issue #132.

* Remove `read.glider.seaexplorer.realtime()` because it is supplanted by
  `read.glider.seaexplorer.raw()` (decided during discussion of issue #130).

# oceglider 0.1.15

* Change `read.glider.seaexplorer.raw()` to read data streams separately (issue
  #128).
* Rename `read.glider.seaexplorer.delayed()` to `read.glider.seaexplorer.raw()`
  (decided during discussion of issue #128).

# oceglider 0.1.14

* Change `read.glider.seaexplorer.delayed()` to not interpolate longitude and
  latitude between surfacing events for `level=0` usage (issue #127).

# oceglider 0.1.13

* Change `read.glider.netcdf()` to handle a new parameter named `saveGlobalAttributes` (issue #125).


# oceglider 0.1.12

* Change `plot.glider()` to handle a new parameter named `simplify`.

# oceglider 0.1.11

* Add some sample data for SeaExplorer raw.
* Rewrite vignettes to download files from CPROOF.
* Rename `read.glider.slocum()` as `read.glider.slocum.csv()` to make its (very
  limited) purpose clearer. Also, add notes about the lack of intention to
  maintain this ad-hoc function.

# oceglider 0.1.10

* Rename flag functions to avoid conflict with oce, e.g. the `oce` functions
  named `setFlags()` and `handleFlags()` are replaced with `setGliderFlags()`
  and `handleGliderFlags()` in `oceglider`.
* Correct the formula used to convert oxygen from ml/l to umol/kg, used in
  `swOxygenFrequencyToSaturation()`.

# oceglider 0.1.9

* Change oxygen calibration.  I think the computation was wrong before, but
  I've not checked and so there is no issue for this.

# oceglider 0.1.8

* Change `read.glider.slocum.netcdf()` to default the `debug` parameter (issue
  #101).

# oceglider 0.1.7

* Change to `plot.glider()` fix map axis mix-up (issue #98).
* Make `[["z"]]` and `[["?"]]` work.

# oceglider 0.1.6

* Change `plot.glider()` by adding `colorbylim` argument.
* Require R version 4.1.0 or later (for pipes).

# oceglider 0.1.5

* Rename from `oceanglider` to `oceglider`.
* Add `plot.glider()`.

# oceglider 0.1.4

* Add `read.glider.slocum.netcdf()`
* Revamp README badges

# oceglider 0.1.3

* fix up some test-suite problems identified in today's R

# oceglider 0.1.2

* add `navStateCodes()`
* add `read.glider.netcdf()`
* add `read.seaexplorer.*()`
* rename `read.glider()` to `read.slocum()`

# oceglider 0.1.0

* crude code tested for some Dalhousie Slocum files.
