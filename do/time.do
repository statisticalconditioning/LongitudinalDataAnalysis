/*##############################################################
time.do: STATA script for analysis in Bolger & Laurenceau Chapter 4:
Modeling the Time Course of Continuous Outcomes
##############################################################*/


/*##############################################################
Use cd command to select the directory where the raw data are;
Read in time.csv
##############################################################*/

cd "[insert path to directory containing data file time.csv]"
insheet using "time.csv", comma clear
label define treatlabel 0 "Control", modify
label define treatlabel 1 "Treatment", modify
label values treatment treatlabel
save time, replace

/*##############################################################
Run mixed effects regression model: xtmixed to produce
	output that matches Table 4.1 (fixed effects) and 4.2 (random effects)
Note: xtmixed evaluates fixed effects using a z distribution
	rather than a t distribution. Therefore, p values will be slightly
	smaller and CIs slightly narrower than those in Table 4.1
##############################################################*/
xtmixed intimacy c.time01##c.treatment || id: time01, res(ar1, t(time)) cov(un) reml
predict m1, fitted
save time, replace

/*##############################################################
#Figure 4.2: Panel plots on Time Course for Control Group
##############################################################*/
twoway connect intimacy time if treatment == 0, ///
	by(id, note("") title(Control Group)) ///
	scheme(s1mono) xtitle(Time in Weeks) ytitle(Intimacy) ///
	msize(small) ylabel(, angle(0) nogrid) ///
	xsize(5) ysize(5)
graph save control, replace

/*##############################################################
#Figure 4.2: Panel plots of Time Course for Treatment Group
##############################################################*/
twoway connect intimacy time if treatment == 1, ///
	by(id, note("") title(Treatment Group)) ///
	scheme(s1mono) xtitle(Time in Weeks)  ytitle(Intimacy) ///
	msize(small) ylabel(, angle(0) nogrid) ///
	xsize(5) ysize(5)
graph save treat, replace
/*##############################################################
#Figure 4.2: Panel plots: Control and Treatment Group Combined
##############################################################*/
graph combine control.gph treat.gph, scheme(s1mono) ///
	row(2) xsize(5) ysize(10)


/*##############################################################
#Figure 4.4: Actual and Model-Predicted Time Course: Control Group
##############################################################*/
use time, clear
keep if treatment == 0	
twoway connect intimacy time, msize(small) ///
	msize(vsmall) mcolor(black) lcolor(black) ///
	|| line m1 time, lcolor(black) ///
	by(id, note("") title(Control Group) legend(off))  ///
	scheme(s1mono) xtitle(Time in Weeks) ///
	ylabel(, angle(0) nogrid) ///
	xsize(5) ysize(5) 	
graph save fitcontrol, replace

/*##############################################################
#Figure 4.4: Actual and Model-Predicted Time Course: Treatment Group
##############################################################*/
use time, clear
keep if treatment == 1
twoway connect intimacy time, msize(small) ///
	msize(vsmall) mcolor(black) lcolor(black) ///
	|| line m1 time, lcolor(black) ///
	by(id, note("") title(Treatment Group) legend(off))  ///
	scheme(s1mono) xtitle(Time in Weeks) ///
	ylabel(, angle(0) nogrid) ///
	xsize(5) ysize(5) 	
graph save fittreat, replace
/*##############################################################
#Figure 4.4: Actual and Model-Predicted Time Course: Combined Groups
##############################################################*/
graph combine fitcontrol.gph fittreat.gph, scheme(s1mono) ///
	row(2) xsize(5) ysize(10)

/*##############################################################
#Figure 4.5: Spaghetti Plot of Time Course
##############################################################*/
use time, clear
twoway line m1 time, connect(ascending) scheme(s1mono) ///
	by(treatment, note("") legend(off)) ///
	|| lfit intimacy time, lcolor(black) lwidth(vthick) ///
	xtitle(Time in Weeks) ytitle(Intimacy)
	
	
	