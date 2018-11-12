* Prevent "more" messages from appearing
set more off
* Control line length
set linesize 150

********************************************************************************
*******     BEGIN DATA MANIPULATION OF CHAPTER 3b SIX-OCCASION EXAMPLE   *******
*******               CHANGE "filesave" to your directory                *******
********************************************************************************

* Defining global variable for file location to be replaced in code below
global filesave "C:\Dropbox\PilesOfVariance\Chapter3b\STATA"

* Import chapter 3 six-occasion stacked data
use "$filesave\STATA_Chapter3b.dta", clear

********************************************************************************
*******               BEGIN CHAPTER 3b SIX-OCCASION MODELS               *******
********************************************************************************

* Save results to separate file
log using $filesave\STATA_Chapter3b_Output, replace name(STATA_Chapter3b)

display as result "Chapter 3b Example: Means by session for RT outcome"
tabulate session, summarize(rt)

display as result "Eq 3b.10: Between-Person Independent ANOVA"
mixed rt i.session, ///
           || personid: , noconstant variance reml covariance(unstructured) ///
           residuals(independent,t(session)),
      estat ic, n(101),
      estat wcorrelation, covariance,
      estat wcorrelation,
      contrast i.session, 
      margins  i.session, 
      margins  i.session, pwcompare(pveffects)
      estimates store FitBPANOVA,

display as result "Eq 3b.10: Univariate Repeated Measures ANOVA"
mixed rt i.session, ///
           || personid: , noconstant variance reml covariance(unstructured) ///
           residuals(exchangeable,t(session)),
      estat ic, n(101),
      estat wcorrelation, covariance,
      estat wcorrelation,
      contrast i.session, 
      margins  i.session, 
      margins  i.session, pwcompare(pveffects)
      estimates store FitUnivANOVA,
      lrtest FitUnivANOVA FitBPANOVA, 

display as result "Eq 3b.10: Multivariate Repeated Measures ANOVA"
mixed rt i.session, ///
           || personid: , noconstant variance reml covariance(unstructured) ///
           residuals(unstructured,t(session)),
      estat ic, n(101),
      estat wcorrelation, covariance,
      estat wcorrelation,
      contrast i.session, 
      margins  i.session, 
      margins  i.session, pwcompare(pveffects)
      estimates store FitMultivANOVA,
      lrtest FitMultivANOVA FitUnivANOVA, 
 
****** END CHAPTER 3b MODELS ******

* Close log
log close STATA_Chapter3b
* Convert log to html using custom downloaded package
log2html $filesave\STATA_Chapter3b_Output, replace
