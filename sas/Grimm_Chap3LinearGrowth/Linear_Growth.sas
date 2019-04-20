ODS GRAPHICS OFF;
ODS HTML CLOSE;
ODS LISTING;

DATA nlsy_math_long;
	INFILE 'C:\GRE2016\Data\nlsy_math_long_R.dat';
	INPUT 
		id		female 		low_birth_weight		anti_k1
		math 	grade		occ						age
		men		spring		anti;
RUN;

*Plotting Scores over time;
GOPTIONS
	FTEXT = ARIAL
	CTEXT = BLACK
	HTEXT = 1.5;
PROC GPLOT DATA = nlsy_math_long;
	TITLE1 'PIAT Mathematics by Grade';
	SYMBOL
		Interpol=join
		Color=black
		Repeat=50000
		Value=dot
		Height=.5
		Width=1;
	AXIS1
		LABEL = (F=ARIAL H = 2 A=90 'PIAT Mathematics')
		ORDER = (0 to 90 BY 10)
		MINOR = none;
	AXIS2
		LABEL = (F=ARIAL H = 2 'Grade')
		ORDER = (2 to 8 BY 1)
		OFFSET= (5 PCT, 5 PCT)
		MINOR = none;

	PLOT math * grade = id/NOLEGEND VAXIS= AXIS1 HAXIS=AXIS2;
RUN;

** Smaller Sample;
DATA ids;
	SET nlsy_math_long;
	BY id;
	IF first.id ne 1 THEN DELETE;
	ran_num = RANUNI(20111129);
	KEEP id ran_num;
RUN;
DATA nlsy_math_long1;
	MERGE ids nlsy_math_long;
	BY id;
RUN;

PROC GPLOT DATA = nlsy_math_long1 (WHERE = (ran_num < .05));
	TITLE1 'PIAT Mathematics by Grade';
	SYMBOL
		Interpol=join
		Color=black
		Repeat=50000
		Value=dot
		Height=.5
		Width=1;
	AXIS1
		LABEL = (F=ARIAL H = 2 A=90 'PIAT Mathematics')
		ORDER = (0 to 90 BY 10)
		MINOR = none;
	AXIS2
		LABEL = (F=ARIAL H = 2 'Grade')
		ORDER = (2 to 8 BY 1)
		OFFSET= (5 PCT, 5 PCT)
		MINOR = none;

	PLOT math * grade = id/NOLEGEND VAXIS= AXIS1 HAXIS=AXIS2;
RUN;

*No Growth Model;
PROC MIXED DATA = nlsy_math_long COVTEST;
	CLASS id;
	MODEL math = /SOLUTION DDFM = BW;
	RANDOM INTERCEPT /SUBJECT = id TYPE = UNR;
RUN;

/*
                                    Covariance Parameter Estimates

                                                      Standard         Z
                  Cov Parm     Subject    Estimate       Error     Value      Pr > Z

                  Var(1)       id          47.0056      4.8379      9.72      <.0001
                  Residual                  116.69      4.5484     25.66      <.0001


                                           Fit Statistics

                                -2 Res Log Likelihood         17492.3
                                AIC (smaller is better)       17496.3
                                AICC (smaller is better)      17496.3
                                BIC (smaller is better)       17506.0


                                   Null Model Likelihood Ratio Test

                                     DF    Chi-Square      Pr > ChiSq

                                      1        134.98          <.0001


                                      Solution for Fixed Effects

                                            Standard
                   Effect       Estimate       Error      DF    t Value    Pr > |t|

                   Intercept     45.9145      0.3239     931     141.76      <.0001
*/

*Centering Grade at Second Grade;
DATA nlsy_math_long1;
	SET nlsy_math_long;
	grade_c2 = grade - 2;
RUN;

*Linear Growth Model;
PROC MIXED DATA = nlsy_math_long1 COVTEST;
	CLASS id;
	MODEL math = grade_c2/SOLUTION DDFM = BW;
	RANDOM INTERCEPT grade_c2/SUBJECT = id TYPE = UN GCORR;
RUN;

/*
                                    Covariance Parameter Estimates

                                                      Standard         Z
                  Cov Parm     Subject    Estimate       Error     Value        Pr Z

                  UN(1,1)      id          64.6858      5.6687     11.41      <.0001
                  UN(2,1)      id          -0.1975      1.1521     -0.17      0.8639
                  UN(2,2)      id           0.7397      0.3280      2.26      0.0121
                  Residual                 36.2322      1.8672     19.40      <.0001


                                           Fit Statistics

                                -2 Res Log Likelihood         15941.0
                                AIC (smaller is better)       15949.0
                                AICC (smaller is better)      15949.0
                                BIC (smaller is better)       15968.3


                                   Null Model Likelihood Ratio Test

                                     DF    Chi-Square      Pr > ChiSq

                                      3        736.22          <.0001


                                      Solution for Fixed Effects

                                            Standard
                   Effect       Estimate       Error      DF    t Value    Pr > |t|

                   Intercept     35.2672      0.3552     931      99.28      <.0001
                   grade_c2       4.3396     0.08741    1288      49.65      <.0001
*/

*Fitting Linear Growth Models by Grade using NLMIXED;
*No Growth Model;
PROC NLMIXED DATA = nlsy_math_long; 
     b_1i = beta_1 + d_1i;
     traject = b_1i;
     MODEL math ~ NORMAL(traject, v_u); 
     RANDOM d_1i ~ NORMAL([0], [v_1]) SUBJECT = id; 
     PARMS beta_1 = 40 v_1 = 30 v_u = 50; 
RUN;

/*
                                            Fit Statistics

                               -2 Log Likelihood                  17492
                               AIC (smaller is better)            17498
                               AICC (smaller is better)           17498
                               BIC (smaller is better)            17512


                                         Parameter Estimates

                       Standard
Parameter   Estimate      Error     DF   t Value   Pr > |t|    Alpha      Lower      Upper   Gradient

beta_1       45.9147     0.3240    931    141.72     <.0001     0.05    45.2789    46.5505    0.00009
v_1          46.9175     4.8322    931      9.71     <.0001     0.05    37.4342    56.4008   0.000028
v_u           116.68     4.5479    931     25.66     <.0001     0.05     107.76     125.61   -6.28E-6
*/

*Fitting a Linear Growth Model in SAS using NLMIXED;
PROC NLMIXED DATA = nlsy_math_long; 	
    b_1i = beta_1 + d_1i; 	
    b_2i = beta_2 + d_2i;
    traject = b_1i + b_2i * (grade-2);
    MODEL math ~ NORMAL(traject, v_u); 	
    RANDOM d_1i d_2i ~ NORMAL([0,0], [v_1,
                                      c_21, v_2]) 	
    SUBJECT = id OUT = estimates; 	
    PARMS beta_1 = 20 beta_2 = 6 v_1 = 60 v_2 = .8 c_21 = 0 v_u = 50; 
RUN; 

/*
                                            Fit Statistics

                               -2 Log Likelihood                  15937
                               AIC (smaller is better)            15949
                               AICC (smaller is better)           15949
                               BIC (smaller is better)            15978


                                         Parameter Estimates

                       Standard
Parameter   Estimate      Error     DF   t Value   Pr > |t|    Alpha      Lower      Upper   Gradient

beta_1       35.2673     0.3554    930     99.23     <.0001     0.05    34.5698    35.9648   -0.00024
beta_2        4.3393    0.08831    930     49.14     <.0001     0.05     4.1660     4.5126   -0.00008
v_1          64.5616     5.6594    930     11.41     <.0001     0.05    53.4549    75.6682   -0.00001
v_2           0.7325     0.3273    930      2.24     0.0254     0.05    0.09024     1.3749   -0.00016
c_21         -0.1815     1.1501    930     -0.16     0.8747     0.05    -2.4385     2.0756   -0.00007
v_u          36.2298     1.8666    930     19.41     <.0001     0.05    32.5667    39.8930   0.000022
*/

*Predicted Plot based on OUT=estimate;
PROC SORT DATA = estimates; 	
    BY id; 
RUN; 
DATA estimates1; 	
    SET estimates; 	
    RETAIN; 	
    BY id; 	
    IF first.id = 1 THEN d_1i = estimate;	 	
    IF last.id = 1 THEN d_2i = estimate;  	
    IF last.id = 1 THEN OUTPUT; 	
    KEEP id d_1i d_2i; 
RUN;
DATA nlsy_prediction; 	
    MERGE nlsy_math_long estimates1; 	
    BY id; 	
    pred = (35.2673 + d_1i) + (4.3393 + d_2i) * (grade - 2); 	
    resid = math - pred; 
RUN; 

PROC GPLOT DATA = nlsy_prediction;
	TITLE1 'PIAT Mathematics by Grade - Predictions';
	SYMBOL
		Interpol=join
		Color=black
		Repeat=50000
		Value=dot
		Height=.5
		Width=1;
	AXIS1
		LABEL = (F=ARIAL H = 2 A=90 'PIAT Mathematics - Predictions')
		ORDER = (0 to 90 BY 10)
		MINOR = none;
	AXIS2
		LABEL = (F=ARIAL H = 2 'Grade')
		ORDER = (2 to 8 BY 1)
		OFFSET= (5 PCT, 5 PCT)
		MINOR = none;

	PLOT pred * grade = id/NOLEGEND VAXIS= AXIS1 HAXIS=AXIS2;
RUN;
PROC GPLOT DATA = nlsy_prediction;
	TITLE1 'PIAT Mathematics by Grade - Residuals';
	SYMBOL
		Interpol=join
		Color=black
		Repeat=50000
		Value=dot
		Height=.5
		Width=1;
	AXIS1
		LABEL = (F=ARIAL H = 2 A=90 'PIAT Mathematics - Residuals')
		ORDER = (-20 to 20 BY 5)
		MINOR = none;
	AXIS2
		LABEL = (F=ARIAL H = 2 'Grade')
		ORDER = (2 to 8 BY 1)
		OFFSET= (5 PCT, 5 PCT)
		MINOR = none;

	PLOT resid * grade = id/NOLEGEND VAXIS= AXIS1 HAXIS=AXIS2;
RUN;

PROC MEANS;
	CLASS grade;
	VAR resid;
RUN;
