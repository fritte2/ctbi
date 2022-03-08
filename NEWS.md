# ctbi (development version)

# ctbi 1.0.1 (2022/03/08)

* F or T have been replaced with FALSE or TRUE

* Example 2 has been added to the help of ctbi

* Value has been added to the help of ctbi.plot and hidd.seq

# ctbi 1.0.0 (2022/03/01)

* ORCID of Francois Ritter has been updated.

* n.bin.min <- floor(bin.size\*(1-bin.max.f.NA)) has been replaced with n.bin.min <- ceiling(bin.size\*(1-bin.max.f.NA)). This means that for bin.max.f.NA = 0.2 (bins with at least 80% of data are accepted) and bin.size = 12 (e.g., 12 months of data per bin), a bin with 9 months of data will be rejected.

* n.bin.min (minimum number of data points for a bin to be accepted) has been added as an output of ctbi in list.main.

* The starting and ending boundaries of the long.term interpolation were treated as NA values. This has been removed.

* The numeric class of data0[,y] has been forced to avoid problems with integers.

* Fixed a minor problem with the error message concerning k.outliers

* Fixed a plotting issue with the y range.
