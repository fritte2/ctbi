# ctbi (development version)

# ctbi 2.0.4 (2022/11/18)

* Fixed a minor mistake concerning the way residuals are handled for limited range of possible values (ex. [0,+Inf[ instead of ]-Inf,+Inf[)
* The description of the functions has been updated
* A temporary DOI has been attached until the review of the original manuscript is finished

# ctbi 2.0.3 (2022/07/27)

* summary.bin has been added to the output of ctbi, and it condenses the information about the size of a bin (bin.size), the minimum number of points for a bin to be accepted (bin.size.min.accepted) and the Stacked Cycle Index (SCI)
* the mean of mean cycle has been subtracted and added to the long.term
* examples have been updated

# ctbi 2.0.2 (2022/07/25)

* summary.outlier has been added to the output of ctbi, and gives information about coeff.outlier (coefficients A,B and C), the lower/upper outlier threshold, the number of points used, the value of m.star.
* a minor mistake has been fixed for the residuals
* the outliers are now present in the column residuals to compare them to the lower/upper threshold

# ctbi 2.0.1 (2022/07/22)

* ctbi.outliers has been changed with ctbi.outlier. This function now only takes an input vector and can handle any univariate datasets (no necessarily based on residuals).
* The Logbox has been updated. g_A(m.star)=0.2294exp(2.9416m.star-0.0512m.star^2-0.0684m.star^3) and g_B(m.star)=1.0585+15.6960m.star-17.3618m.star^2+28.3511m.star^3-11.4726m.star^4 will respectively calculate the A and B coefficients with m.star = (q(0.875)-q(0.625))/(q(0.75)-q(0.25))-0.6165 for right-skewed distributions, bounded by [0,2]. The C coefficients is fixed to 50 (Pearson Family).
* A "gaussian" method for Logbox has been added, with coeff.outlier = c(0.08,2,33) 
* for n < 9, the MAD will be used instead the boxplot rule, with a multiplying factor of 10 (instead of usually 3) to limit the number of false positive to 0.5%.

# ctbi 2.0.0 (2022/06/10)

* The LogBox method has changed from alpha = 1+klog(n) to alpha = Alog(n)+B+C/n
* k.outliers has been replaced by coeffs.outlier = c(A,B,C).
* SCI.min = Inf has been replaced by SCI.min = NA when no values are imputed

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
