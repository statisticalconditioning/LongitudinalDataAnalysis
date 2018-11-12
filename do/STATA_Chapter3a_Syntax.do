* Prevent "more" messages from appearing
set more off
* Control line length
set linesize 150

********************************************************************************
*******    BEGIN DATA MANIPULATION OF CHAPTER 3a TWO-OCCASION EXAMPLE    *******
*******               CHANGE "filesave" to your directory                *******
********************************************************************************

* Defining global variable for file location to be replaced in code below
global filesave "C:\Dropbox\PilesOfVariance\Chapter3a\STATA"

* Import and stack chapter 3a two-occasion multivariate data
* List time-varying variables first, i(level2ID) j(newtimeID)
use "$filesave\STATA_Chapter3a.dta", clear
reshape long outcome, i(personid) j(time)
label variable time "time: Occasion (1=pre-test, 2=post-test)"
label variable outcome "outcome: Learning Outcome"

* Center predictors for analysis
gen time1 = time - 1 
gen treat = group - 1
label variable time1 "time1: Time (0=pre-test, 1=post-test)"
label variable treat "treat: Treatment Group (0=control, 1=treatment)"

********************************************************************************
*******               BEGIN CHAPTER 3a TWO-OCCASION MODELS               *******
********************************************************************************

* Save results to separate file
log using $filesave\STATA_Chapter3a_Output, replace name(STATA_Chapter3a)

display as result "Chapter 3a Example: Means by group and time for learning outcome"
tabulate group time, summarize(outcome)

display as result "Eq 3a.1: Empty Between-Person Model"
mixed outcome , ///
                || personid: , noconstant variance reml covariance(unstructured) ///
                residuals(independent,t(time)),
      estat ic, n(50),
      estat wcorrelation, covariance,
      estat wcorrelation,

display as result "Eq 3a.2: Empty Within-Person Model"
mixed outcome , ///
                || personid: , noconstant variance reml covariance(unstructured) ///
                residuals(exchangeable,t(time)),
      estat ic, n(50),
      estat wcorrelation, covariance,
      estat wcorrelation,

display as result "Eq 3a.7: Conditional Between-Person Model (top of Eq. 3.7)"
display as result "Manual Contrasts for Time and Group"
mixed outcome c.time1 c.treat c.time1#c.treat, ///
                || personid: , noconstant variance reml covariance(unstructured) ///
                residuals(independent,t(time)),
      estat ic, n(50),
      estat wcorrelation, covariance,
      estat wcorrelation,
      * Mean: Control Group at Pre-Test
      lincom _cons*1 + c.time1*0 + c.treat*0 + c.time1#c.treat*0
      * Mean: Control Group at Post-Test
      lincom _cons*1 + c.time1*1 + c.treat*0 + c.time1#c.treat*0
      * Mean: Treatment Group at Pre-Test
      lincom _cons*1 + c.time1*0 + c.treat*1 + c.time1#c.treat*0
      * Mean: Treatment Group at Post-Test
      lincom _cons*1 + c.time1*1 + c.treat*1 + c.time1#c.treat*1
      * Time  Effect for Control Group
      lincom c.time1*1 + c.time1#c.treat*0
      * Time  Effect for Treatment Group
      lincom c.time1*1 + c.time1#c.treat*1
      * Group Effect at Pre-Test
      lincom c.treat*1 + c.time1#c.treat*0
      * Group Effect at Post-Test
      lincom c.treat*1 + c.time1#c.treat*1

display as result "Eq 3a.7: Conditional Within-Person Model (bottom of Eq. 3.7)"
display as result "Manual Contrasts for Time and Group"
mixed outcome c.time1 c.treat c.time1#c.treat, ///
                || personid: , noconstant variance reml covariance(unstructured) ///
                residuals(exchangeable,t(time)),
      estat ic, n(50),
      estat wcorrelation, covariance,
      estat wcorrelation,
      * Mean: Control Group at Pre-Test
      lincom _cons*1 + c.time1*0 + c.treat*0 + c.time1#c.treat*0
      * Mean: Control Group at Post-Test
      lincom _cons*1 + c.time1*1 + c.treat*0 + c.time1#c.treat*0
      * Mean: Treatment Group at Pre-Test
      lincom _cons*1 + c.time1*0 + c.treat*1 + c.time1#c.treat*0
      * Mean: Treatment Group at Post-Test
      lincom _cons*1 + c.time1*1 + c.treat*1 + c.time1#c.treat*1
      * Time  Effect for Control Group
      lincom c.time1*1 + c.time1#c.treat*0
      * Time  Effect for Treatment Group
      lincom c.time1*1 + c.time1#c.treat*1
      * Group Effect at Pre-Test
      lincom c.treat*1 + c.time1#c.treat*0
      * Group Effect at Post-Test
      lincom c.treat*1 + c.time1#c.treat*1

display as result "Eq 3a.7: Conditional Within-Person Model (bottom of Eq. 3.7)"
display as result "Categorical Predictors for Time and Group"
mixed outcome i.time1 i.treat i.time1#i.treat, ///
                || personid: , noconstant variance reml covariance(unstructured) ///
                residuals(exchangeable,t(time)),
      estat ic, n(50),
      estat wcorrelation, covariance,
      estat wcorrelation,
      contrast i.time1#i.treat, 
      margins  i.time1#i.treat, 
      margins  i.time1#i.treat, pwcompare(pveffects)
      margins  i.time1@i.treat, 
      margins  i.treat@i.time1, 
 
****** END CHAPTER 3a MODELS ******

* Close log
log close STATA_Chapter3a
* Convert log to html using custom downloaded package
log2html $filesave\STATA_Chapter3a_Output, replace
