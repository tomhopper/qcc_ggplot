qcc_ggplot
==========
Rewrite of `plot.qcc()` from the _qcc_ package using _ggplot2_ and _grid_. After sourcing this file, when calling `qcc()` with `plot = TRUE` (the default), this version of `plot.qcc()` will automatically be used.

Provides a complete, seamless replacement for qcc's plot function. Nearly all of qcc's original plotting functionality has been implemented through ggplot2 and grid rather than base graphics.

Usage
---------
Load library qcc with `library(qcc)` or `require(qcc)`, then `source()` the file qcc.plot.R. The qcc-native plotting function will be replaced in memory, and any future calls during the current session to `plot()` with a qcc object will utilize this function, including those calls from within `qcc()`. The resulting object can be re-used and modified within grid graphics.

`plot.qcc()` will require that the libraries _ggplot2_, _grid_ and _gtable_ be installed and available.

To Do
--------
* Add back in the ability to control axis text orientation, using <code>axes.las</code>.
* Clean up the layout of the stats panel, especially when resizing to larger plot dimensions.
* Clean up x-axis tick labels to avoid overplotting.
* Add back in the ability to plot user-defined x-axis tick labels, instead of the default index number.

History
--------
#### v 1.0    2014-03-03 

First release.

#### v 1.0.1  2016-01-12 

* Update for ggplot2 v2.0 compatibility. May now break on older versions of ggplot2

#### v 1.0.2  2016-01-18 

* Bug fix: add in ability to specify x-axis labels (allowed in qcc; previously unsupported in qcc_ggplot)

#### v 1.0.3  2016-08-26 

* Backend cleanup. Assignment of df.indices in data frame was scoped incorrectly, 

#### v 1.0.5  2016-11-12 

* Fixed problem with adding new data; qcc() calls with newdata= argument set should work correctly. 
* Fixed backwards compatibility problem with label.limits. Will now accept any text or numeric labels for UCL and LCL labels.
* Added argument label.cl to allow manual setting of center line label
* Fixed incorrect UCL, LCL and CL labels showing in plot info box when label.limits or label.cl was set; not correctly shows the actual calculated values.

#### v 1.0.6 2017-12-28

* Fixed problem with added labels causing an error in ggplot2 code.