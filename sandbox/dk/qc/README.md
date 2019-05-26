qc01.R is a shiny app for doing QC of seaexplorer data. It is presently in an
early stage of development, with more finalized features expected by autumn of
2019, after use with real-world data during summer of 2019.

**TO DO**

* [ ] document, perhaps by an instruction panel
* [ ] make it plot when data read in
* [ ] colour by inversion (special case: note in data)
* [ ] colour by yoNumber
* [ ] colour by time
* [ ] hide 'save' button if data not yet loaded. (if fails; try conditionalPanel.
* [ ] implement an undo scheme
* [ ] consider whether to add plot zooming. Surprisingly, I'm finding that I
   don't feel any need for this, in my tests to date. I am presently using
brushing to flag data, but I could add a two-value button to toggle the action
to mean zooming. There does not seem to be a way to interpret CTL-drag or
similar. Using a button to zoom and others to translate is not a good option,
given the long time to redraw.


