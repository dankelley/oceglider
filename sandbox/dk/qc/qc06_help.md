**Overview**

This app is designed to handle SeaExplorer glider data, in a file format used
by the Bedford Institute of Oceanography as of July 2019.  Extension to other
data formats should not be too difficult, but the author is inclined to focus
first on the SeaExplorer data that is using in his own research program.

This tool is being developed during a period of active use, and the goal is to
make it work well in a practical context, not in some imagined general setting.

Please contact the author, if you see ways that the app might be altered, to be
of more benefit to you and other users.


**Working this app**

*Loading data*

1. Use the pull-down menus in the leftmost column to select a glider and
   mission.  This menu is constructed from examination of data available on
this computer. The data are first sought in
`~/Dropbox/data/glider/delayedData`, but if not present there, then
`/data/glider/delayedData` is searched, and finally `~/data/glider/delayedData`
is searched. One of these directories must hold data for this app to work.

2. Once a glider/mission pair is selected, you have two choices:
    * You may click the button labeled `Read` to read the raw data from the
      appropriate subdirectory of `/data/glider/delayedData`.
    * You may use the pull-down menu that is in the second-from-left column, if
      that menu displays a date and time. This lets you resume a previous
analysis.  (These previous analyses are stored in `.rda` files in the present
directory, when you press the button labeled `Save`.)

*Examining and flagging data*

After the data are loaded, a plot will appear. Above the plot is a status line
that indicates the data nearest the hovering location of the mouse.  Menu items
permit a wide variety of plot types, and exploring these can often reveal
spurious data.

In any plot style, pressing and dragging the mouse will flag data within a
selected rectangle. Used carefully, this can be a powerful tool for identifying
spurious data. Since mouse-drag operations cannot be undone, though, some care
is required. For example, if the conductivity sensor is prone to giving
temporary spurious salinity values, it might be sensible to brush a `C(t)` or
`S(t)` plot, instead of brushing a `TS` plot, because the time-series view
offers a more precise way of identifying temporary problems.

The `Show navState` sequence of buttons that is seen just above the status line
can be used to hide certain navigation states. This can be handy in cases where
near-surface data are often spurious, and are not of interest to the analysis.
Unlike brushing, which permanently flags data, this is a reversible operation;
simply click any unchecked boxes to recover hidden data.

*Keyboard shortcuts*

The following key-press events provide a quick way to navigate graphical views. (Whether a given event has an effect depends on the focus.)

* `n`: in yo-focus, go to the next yo. (Ignored in mission-focus.)
* `p`: in yo-focus, go to the previous yo. (Ignored in mission-focus.)
* `m`: in yo-focus, switch to mission-focus. (Ignored in mission-focus.)
* `y`: in mission-focus, switch to yo-focus. (Ignored in yo-focus.)
* `s`: in mission-focus, select the yo number under the mouse, so that
the next `y` operation will open a graph for that yo.  (Ignored in
  mission-focus.)
* `?`: provides this summary

*Typical workflow*

A typical workflow for editing wild data involves a cycle:

* Start at mission-focus, often with a TS or S(t) diagram, notice a spot that
  appears anomalous.
* Move the mouse over an anomalous point, and then press `s` to save pointer to
  the yo from which the anomalous data came.
* Switch to yo-focus by pressing `y`.
* Possibly after trying a variety of plot types (and colourizing schemes),
  determine some data that are suspicious and ought to be flagged. Select the
data by pressing the mouse, and dragging it (while still pressed) to another
location, the data within the resultant rectangle being flagged.
* Press `m` to return to mission focus, and continue looking for suspicious
  data.

These steps can all be done with menus, but the keystrokes are much quicker,
and become second nature after a few minutes of use.


*Saving data*

The `Save` button saves the original glider object, but with the `pressure`
flag set to 3 for any data that have been flagged as suspicious, whether by
mouse drag operations, by unchecking `navState` checkboxes, or by other
operations. This file will be available the next time the app is loaded, which
means that work can be done in stages.

