qc03.R same feature set as qc02.R but save logical vectors for tests, in hopes of increasing speed.

qc02.R try not using the sidebar. I liked a topbar better, in my whale-collision app

qc01.R is a shiny app for doing QC of seaexplorer data. It is presently in an
early stage of development, with more finalized features expected by autumn of
2019, after use with real-world data during summer of 2019.


**TO DO**

* [ ] need 10X or better speedup in power-on blanking (try debouncing first to test speed)
* [ ] colour by inversion (special case: note in data)
* [ ] implement an undo scheme
* [ ] consider whether to add plot zooming. Surprisingly, I'm finding that I
   don't feel any need for this, in my tests to date. I am presently using
brushing to flag data, but I could add a two-value button to toggle the action
to mean zooming.


