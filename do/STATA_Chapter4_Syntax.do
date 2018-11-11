* Prevent "more" messages from appearing
set more off
* Control line length
set linesize 150

********************************************************************************
*******           BEGIN DATA MANIPULATION OF CHAPTER 4 EXAMPLE           *******
*****                 CHANGE "filesave" to your directory                *******
********************************************************************************

* Defining global variable for file location to be replaced in code below
global filesave "C:\Dropbox\PilesOfVariance\Chapter4\STATA"

* Import chapter 4 stacked data
use "$filesave\STATA_Chapter4.dta", clear

********************************************************************************
*******                     BEGIN CHAPTER 4 MODELS                       *******
*******   NOTE: NOT ALL MODELS WILL BE POSSIBLE TO ESTIMATE IN STATA     *******
********************************************************************************

* Save results to separate file
log using $filesave\STATA_Chapter4_Output, replace name(STATA_Chapter4)

display as result "Chapter 4 Example: Means by StudyDay for Positive Mood outcome"
tabulate studyday, summarize(posmood)

display as result "Ch 4: Saturated Means, Unstructured R-Only Model"
display as result "Test for mean differences across days"
mixed posmood i.studyday, ///
                || personid: , noconstant variance reml covariance(unstructured) ///
                residuals(unstructured,t(studyday)),
      estat ic, n(200),
      estat wcorrelation, covariance,
      estat wcorrelation,
      contrast i.studyday, 
      margins  i.studyday, 
      margins  i.studyday, pwcompare(pveffects)

display as result "Ch 4: Empty Means, Unstructured R-Only Model"
display as result "Best-fitting and least parsimonious baseline"
mixed posmood , ///
                || personid: , noconstant variance reml covariance(unstructured) ///
                residuals(unstructured,t(studyday)),
      estat ic, n(200),
      estat wcorrelation, covariance,
      estat wcorrelation,
      estimates store UN,

display as result "Ch 4: Empty Means, Compound Symmetry R-Only Model"
display as result "Worst-fitting and most parsimonious baseline"
mixed posmood , ///
                || personid: , noconstant variance reml covariance(unstructured) ///
                residuals(exchangeable,t(studyday)),
      estat ic, n(200),
      estat wcorrelation, covariance,
      estat wcorrelation,
      estimates store CS,
      lrtest UN CS, 

display as result "Ch 4: Empty Means, First-Order Auto-Regressive R-Only Model"
mixed posmood , ///
                || personid: , noconstant variance reml covariance(unstructured) ///
                residuals(ar1,t(studyday)),
      estat ic, n(200),
      estat wcorrelation, covariance,
      estat wcorrelation,
      estimates store AR1,
      lrtest UN AR1, 

display as result "Ch 4: Empty Means, n-1 Lag Toeplitz R-Only Model"
mixed posmood , ///
                || personid: , noconstant variance reml covariance(unstructured) ///
                residuals(toeplitz6,t(studyday)),
      estat ic, n(200),
      estat wcorrelation, covariance,
      estat wcorrelation,
      estimates store TOEP7,
      lrtest UN TOEP7, 
      lrtest TOEP7 CS, 
      lrtest TOEP7 AR1, 

display as result "Ch 4: Empty Means, Random Intercept in G"
display as result "Diagonal R"
mixed posmood , ///
                || personid: , variance reml covariance(unstructured) ///
                residuals(independent,t(studyday)),
      estat ic, n(200),
      estat icc,
      estat recovariance, relevel(personid),
      estat recovariance, relevel(personid) correlation,
      estat wcorrelation, covariance,
      estat wcorrelation,
      estimates store RIDIAG,
      lrtest UN RIDIAG, force

display as result "Ch 4: Empty Means, Random Intercept in G"
display as result "Diagonal Heterogeneous R"
mixed posmood , ///
                || personid: , variance reml covariance(unstructured) ///
                residuals(banded0,t(studyday)),
      estat ic, n(200),
      estat recovariance, relevel(personid),
      estat recovariance, relevel(personid) correlation,
      estat wcorrelation, covariance,
      estat wcorrelation,
      estimates store RIDIAGH,
      lrtest UN RIDIAGH, force
      lrtest RIDIAGH RIDIAG, force

display as result "Ch 4: Empty Means, Random Intercept in G"
display as result "First-Order Autoregressive R"
mixed posmood , ///
                || personid: , variance reml covariance(unstructured) ///
                residuals(ar1,t(studyday)),
      estat ic, n(200),
      estat recovariance, relevel(personid),
      estat recovariance, relevel(personid) correlation,
      estat wcorrelation, covariance,
      estat wcorrelation,
      estimates store RIAR1,
      lrtest UN RIAR1, force
      lrtest RIAR1 AR1, force
      lrtest RIAR1 RIDIAG, force

display as result "Ch 4: Empty Means, Random Intercept in G"
display as result "1-Lag Toeplitz R"
mixed posmood , ///
                || personid: , variance reml covariance(unstructured) ///
                residuals(toeplitz1,t(studyday)),
      estat ic, n(200),
      estat recovariance, relevel(personid),
      estat recovariance, relevel(personid) correlation,
      estat wcorrelation, covariance,
      estat wcorrelation,
      estimates store RITOEP2,
      lrtest UN RITOEP2, force
      lrtest RITOEP2 RIDIAG, force

display as result "Ch 4: Empty Means, Random Intercept in G"
display as result "2-Lag Toeplitz R"
mixed posmood , ///
                || personid: , variance reml covariance(unstructured) ///
                residuals(toeplitz2,t(studyday)),
      estat ic, n(200),
      estat recovariance, relevel(personid),
      estat recovariance, relevel(personid) correlation,
      estat wcorrelation, covariance,
      estat wcorrelation,
      estimates store RITOEP3,
      lrtest UN RITOEP3, force
      lrtest RITOEP3 RITOEP2, force

display as result "Ch 4: Empty Means, Random Intercept in G"
display as result "3-Lag Toeplitz R"
mixed posmood , ///
                || personid: , variance reml covariance(unstructured) ///
                residuals(toeplitz3,t(studyday)),
      estat ic, n(200),
      estat recovariance, relevel(personid),
      estat recovariance, relevel(personid) correlation,
      estat wcorrelation, covariance,
      estat wcorrelation,
      estimates store RITOEP4,
      lrtest UN RITOEP4, force
      lrtest RITOEP4 RITOEP3, force

display as result "Ch 4: Empty Means, Random Intercept in G"
display as result "4-Lag Toeplitz R"
mixed posmood , ///
                || personid: , variance reml covariance(unstructured) ///
                residuals(toeplitz4,t(studyday)),
      estat ic, n(200),
      estat recovariance, relevel(personid),
      estat recovariance, relevel(personid) correlation,
      estat wcorrelation, covariance,
      estat wcorrelation,
      estimates store RITOEP5,
      lrtest UN RITOEP5, force
      lrtest RITOEP5 RITOEP4, force

display as result "Ch 4: Empty Means, Random Intercept in G"
display as result "5-Lag Toeplitz R"
mixed posmood , ///
                || personid: , variance reml covariance(unstructured) ///
                residuals(toeplitz5,t(studyday)),
      estat ic, n(200),
      estat recovariance, relevel(personid),
      estat recovariance, relevel(personid) correlation,
      estat wcorrelation, covariance,
      estat wcorrelation,
      estimates store RITOEP6,
      lrtest UN RITOEP6, force
      lrtest RITOEP6 RITOEP5, force
 
****** END CHAPTER 4 MODELS ******

* Close log
log close STATA_Chapter4
* Convert log to html using custom downloaded package
log2html $filesave\STATA_Chapter4_Output, replace
