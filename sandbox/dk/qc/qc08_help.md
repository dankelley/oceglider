**Overview of qc (version 0.8)**

This app is designed to handle SeaExplorer glider data, in a file format used
by the Bedford Institute of Oceanography as of July 2019.  Extension to other
data formats should not be too difficult, but the author is inclined to focus
first on the SeaExplorer data that is being used in his own research program.

This tool is being developed during a period of active use, and the goal is to
make it work well in a practical context, not in some imagined general setting.

In addition to this document, the developers have put some instructional videos
online at https://www.youtube.com/channel/UC5Go198V3B0SGBSju2_bdKA. (Since the
tutorials were created during the development process, some features may not
match the present behaviour.)

Please contact the author at kelley.dan@gmail.com if you see ways that the app
might be altered, to be of more benefit to you and other users.


**Loading data**

1. Use the pull-down menus in the leftmost column to select a glider and
   mission.  This menu is constructed from examination of data available on
this computer. The data are first sought in
`~/Dropbox/data/glider/delayedData`, but if not present there, then
`/data/glider/delayedData` is searched, and finally `~/data/glider/delayedData`
is searched. One of these directories must hold data for this app to work.

2. Once a glider/mission pair is selected, you have two choices:
    * You may click the button labeled `Read` to read the raw data from the
      appropriate subdirectory of `/data/glider/delayedData` (or one of
      several other directories that are examined, as noted above).
    * You may use the pull-down menu that is in the second-from-left column, if
      that menu displays a date and time. This lets you resume a previous
analysis.  (These previous analyses are stored in `.rda` files in the present
directory, when you press the button labeled `Save`.)

**Examining and flagging data**

After the data is loaded, a plot will appear. Above the plot is a status line
that describes the data nearest the hovering location of the mouse.  Menu items
permit a wide variety of plot types, and exploring these can often reveal
spurious data.

In any plot style, pressing and dragging the mouse will flag data within a
selected rectangle. Used carefully, this can be a powerful tool for identifying
spurious data. Since mouse-drag operations cannot be undone, some care
is required. For example, if the conductivity sensor is prone to giving
temporary spurious salinity values, it might be sensible to brush a `C(t)` or
`S(t)` plot, instead of brushing a `TS` plot, because the time-series view
offers a more precise way of identifying temporary problems.

The `Show navState` sequence of buttons that is seen just above the status line
can be used to hide certain navigation states. This can be handy in cases where
near-surface data are often spurious, and are not of interest to the analysis.
Unlike brushing, which permanently flags data, this is a reversible operation;
simply click any unchecked boxes to recover hidden data.

**Keyboard shortcuts**

The following key-press events provide a quick way to navigate graphical views.
(Whether a given event has an effect depends on the focus.)

*Lower-case letters for focus shifts.*

* `n`: go to **n**ext yo (if in yo-focus).
* `p`: go to **p**revious yo (if in yo-focus).
* `m`: switch to **m**ission-focus.
* `y`: switch to **y**o-focus.
* `c`: **c**opy number of yo near mouse, so next yo-focus graph will show it.

*Upper-case letters for plot types.*

* `P`: plot a **P**ressure time-series, i.e. *p(t)*.
* `S`: plot a **Sa**linity time-series, i.e. *S(t)*.
* `T`: plot a **T**emperature-salinity diagram, i.e. *TS*.
* `?`: show this summary.

**Typical workflow**

A typical workflow might involve a cycle:

* Start at mission-focus, with the `Plot` menu set to `p(t)`
  to show a pressure time-series.  Try colourizing by
  distance, latitude, or longitude, and see whether the
  data appear to be sensible for the bottom topography of the domain in
  question.  If there are some odd high pressures, try clicking the button
  marked `Hide S & T outliers`, which may also remove wild pressure points
  if the cause is a problem with the logging computer at high
  sampling rates.
* Set the `Color by` menu to `unstable`, to highlight spots
  where the density profile suggests gravitational instability.
  (These points are plotted on top of other data, so they can
  be seen easily.) Two important cases of instability
  are handled as follows:
  1. If the CTD power is turned on only for a portion of a profile,
  then an averaging procedure may make for odd N2 values near the
  surface or near the bottom. These will show up as bands of
  red or green in a pressure time-series.  In such cases, try
  adjusting the slider named `Hide after power-on` to ignore a time
  interval after power-on.
  2. Another anomalous pattern shows up as red and
  green pairs of horizontal lines in the TS diagram, which indicates
  salinity anomalies.  Handle these by putting the mouse over one of
  points in `mission` focus, typing `s` to select the relevant
  yo, and then typing `y` to go to focus on that yo. Since bad salinity
  is the culprit, it can help to set `Plot` to `S(t)`, in
  which the odd salinity is likely easily seen.  Drag
  the mouse to create a rectangle that encloses points that appear
  to be anomalous, and they will be discarded from future analysis.
* Press `m` to return to mission focus, and continue looking for suspicious
  data. Work back and forth between yo and mission focus views, trying
  whatever combinations of plot types and colourizing schemes seem to
  be informative for your own dataset.
* Explore what happens when you unset `not_navigating`, `surfacing`, etc.,
  buttons. This hides some navigation states from view.  You may find
  that near-surface data are problematic.

Use the pull-down menus if you find these more convenient than using the
keyboard.


**Saving data**

The `Save` button saves the original glider object, but with the `pressure`
flag set to 3 for any data that have been flagged as suspicious, whether by
mouse drag operations, by unchecking `navState` checkboxes, or by other
operations. This file will be available the next time the app is loaded, which
means that work can be done in stages.

**Interface Details**

* Selecting the `Limit Range` checkbox hides pressures less than -5dbar,
  temperatures outside the range -2.5 to 40C, and salinities outside the range
2 to 41.  This is consistent with ARGO-float procedures (see Section 6 of Wong
et al., 2018) except that here the tests are done Conservative Temperature and
Absolute Salinity.

* Selecting the `Hide Outliers` checkbox hides Conservative Temperatures and
  Absolute Salinities that differ by more than 3 standard deviations from their
means.

* Selecting the `Hide T,S outliers` hides 

**References**

* Wong, Annie, Robert Keeley, Thierry Carval, and the Argo Data Management Tam.
  “Argo Quality Control Manual For CTD and Trajectory Data,” 2018.
https://www.oceanbestpractices.net/bitstream/handle/11329/450/32470.pdf?sequence=1&isAllowed=y.

