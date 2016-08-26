qcc_ggplot
==========
Rewrite of plot.qcc (from the qcc package) using ggplot and grid.

Provides a complete, seamless replacement for qcc's plot function. Nearly all of qcc's original plotting functionality has been implemented through ggplot2 and grid rather than base graphics.

Usage
---------
Load library qcc, then <code>source()</code> the file qcc.plot.R. The qcc-native plotting function will be replaced in memory, and any future calls during the current session to <code>plot()</code> with a qcc object will utilize this function, including those calls from within <code>qcc()</code>. The resulting object can be re-used and modified within grid graphics.

<code>plot.qcc()</code> will require that the libraries ggplot2, grid and gtable be installed and available.

To Do
--------
* Add back in the ability to control axis text orientation, using <code>axes.las</code>.
* Clean up the layout of the stats panel, especially when resizing to larger plot dimensions.
* Fix bug when adding newdata.
* Clean up x-axis tick labels to avoid overplotting.

History
--------
v 1.0    2014-03-03 First release.

v 1.0.1  2016-01-12 Update for ggplot2 v2.0 compatibility. May break on older versions of ggplot2

v 1.0.2  2016-01-18 Bug fix: add in ability to specify x-axis labels (allowed in qcc; previously unsupported in qcc_ggplot)

v 1.0.3  2016-08-26 Backend cleanup. Assignment of df.indices in data frame was scoped incorrectly, 