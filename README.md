gcc_ggplot
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
