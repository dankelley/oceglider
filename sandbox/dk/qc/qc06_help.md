**Overview**

This app is designed to handle SeaExplorer glider data, in a file format used
by the Bedford Institute of Oceanography as of July 2019.

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

The following key-press events provide a quick way to navigate graphical views.

* `n`: in yo-focus, go to the next yo. (Ignored in mission-focus.)
* `p`: in yo-focus, go to the previous yo. (Ignored in mission-focus.)
* `m`: in yo-focus, switch to mission-focus. (Ignored in mission-focus.)
* `y`: in mission-focus, switch to yo-focus. (Ignored in yo-focus.)


*Typical workflow*

The default plot panel shows pressure as a function of time, colour-coded by
distance from the first yo. This gives a good overview of the dataset, at least
for an analyst who is familiar with the glider mission under study.  In some
cases, there will be wild pressure values, e.g. spikes extending thousands of
metres deep in a region known to be on the continental shelf. The button
labeled `Despike pressure` may remove these points well, but if it fails, the
analyst ought to brush the mouse over those pressure spikes, to flag them and
remove them from view. (Note that clicking the `Despike pressure` action is
reversible, but that brushing data is not.)

Once any wildly spurious pressures are cleaned up, a good next step is to plot
salinity as a function of time, by choosing `S(t)` from the `Plot` menu.
After data are loaded, a plot will appear in the lower part of the app. Above
this is a status line, within which the app will display information about the
data point that is nearest the mouse. The status line can be a very helpful
tool in diagnosing quality control problems with the data. For example, moving
the mouse over a sequence of spurious points might reveal that they are all
within a particular yo.  It is common to double-click on a data point that
seems worthy of more study, and then to change the \code{Focus} from
\code{mission} to \code{yo}, to examine that particular yo in detail.

A half hour of exploration ought to be sufficient for an analyst to become
familiar with how the app can be used in a particular setting.  (For example,
the action for \code{Despike pressure} may be needed routinely in some
datasets, and never in others.)

*Saving data*

The `Save` button saves the original glider object, but with the `pressure`
flag set to 3 for any data that have been flagged as suspicious, whether by
mouse drag operations, `Flag yo` operations, or unchecking `navState`
checkboxes. This file will be available the next time the app is loaded, which
means that work can be done in stages.

**Enhancing this app**

Extension to other data formats should not be too difficult, but the author is
inclined to focus first on the SeaExplorer data that is using in his own
research program.  No attempt will be made for backwards compatibility: if the
data format changes, the app will be changed accordingly. This tool is being
developed during a period of active use, and the goal is to make it work well
in a practical context, not in some imagined general setting.

Please contact the author, if you see ways that the app might be altered, to be
of more benefit to you and other users.


