qc01.R is a shiny app for doing QC of seaexplorer data.

**TO DO**

1. document
2. upon saving, restrict to navState as presently highlighted
3. complete the undo scheme
4. consider whether to add plot zooming. Surprisingly, I'm finding that I
   don't feel any need for this, in my tests to date. I am presently using
brushing to flag data, but I could add a two-value button to toggle the action
to mean zooming. There does not seem to be a way to interpret CTL-drag or
similar. Using a button to zoom and others to translate is not a good option,
given the long time to redraw.


