******************************************************************************
*Project:	ECON 613 HW5
*Created by: Zhixin Yu
*Created:	2019/04/13

******************************************************************************
* Set working environment
clear all
set more off, perm
set scrollbufsize 2000000
* Set working directory.
cd "D:\R\Econ613\Econ613_HW5_ZhixinYu"
pwd



******************************* Assignment 2 *******************************
***************************************************************
* Exercise 1: Data Creation
***************************************************************
set seed 17
* create the variables
set obs 10000
gen X1 = runiform(1,3)
gen X2 = rgamma(3,2)
gen X3 = rbinomial(1,0.3)
gen eps = rnormal(2,1)
gen Y = 0.5+1.2*X1-0.9*X2+0.1*X3+eps
* generate dummy variable
egen Y_mean = mean(Y)
gen ydum = (Y > Y_mean)

***************************************************************
* Exercise 2: OLS
***************************************************************
* Calculate the correlation
correlate Y X1
* 0.2009

* Regression of Y on X
reg Y X1 X2 X3
eststo OLS
esttab OLS, se(3) title(OLS) nonumbers mtitles("OLS")
*
*OLS
*----------------------------
*                      OLS   
*----------------------------
*X1                  1.176***
*                  (0.017)   
*
*X2                 -0.905***
*                  (0.003)   
*
*X3                 0.0173   
*                  (0.022)   
*
*_cons               2.600***
*                  (0.040)   
*----------------------------
*N                   10000   
*----------------------------
*Standard errors in parentheses
* p<0.05, ** p<0.01, *** p<0.001

* Bootstrap
bootstrap, reps(49): regress Y X1 X2 X3
eststo boot49
bootstrap, reps(499): regress Y X1 X2 X3
eststo boot499

* display ols and bootstrap result table
esttab OLS boot49 boot499, se(5) title(bootstrap_se) nonumbers mtitles("OLS" "boot49" "boot499")


***************************************************************
* Exercise 4&5 : Discrete Choice & Marginal Effects
***************************************************************
* Probit Model
probit ydum X1 X2 X3
eststo probit
* Marginal Effects
margins, dydx(*) atmeans

* Logit Model
logit ydum X1 X2 X3
eststo logit
* Marginal Effect
margins, dydx(*) atmeans

* display probit and logit model estimations results
esttab probit logit, se(5) title(Discrete Choice) nonumbers mtitles("probit" "logit")


******************************* Assignment 3 *******************************
***************************************************************
* Exercise 1 : Data Description
***************************************************************
clear
* import product dataset and prepare to merge data
import delimited "C:\Users\Administrator\Desktop\Duke\19 Spring study\Econ 613\hw3\hw3\product.csv"

* summary data
su
* market share by product
tab choice
* market share by product charecteristics (stk)
tab choice if choice <=6
* market share by product charecteristics (tub)
tab choice if choice >6


***************************************************************
* Exercise 2&3&4 : First and Second Model & Marginal Effects
***************************************************************
import delimited "C:\Users\Administrator\Desktop\Duke\19 Spring study\Econ 613\hw3\hw3\product.csv"
sort hhid
save data, replace
clear
* import demos dataset and merge data
import delimited "C:\Users\Administrator\Desktop\Duke\19 Spring study\Econ 613\hw3\hw3\demos.csv"
save datanew, replace
sort hhid

merge hhid using data.dta
* drop columns we won't use
drop fs3_4 fs5 fam_size college whtcollar retired hhid v1 _merge
* genderate index
gen v1 = _n
* rename and reshape dataset
rename (ppk_stk pbb_stk pfl_stk phse_stk pgen_stk pimp_stk pss_tub ppk_tub pfl_tub phse_tub)(c1 c2 c3 c4 c5 c6 c7 c8 c9 c10)
reshape long c, i(v1) j(price)
* generate dummy variable for choice
gen dum = cond(price == choice, 1, 0)


* Conditinal Logit Model
asclogit dum c, case(v1) alternatives(price)
eststo C_logit
esttab C_logit, se(5) title(The effect of price on demand) nonumbers mtitles("C_logit")
* calculate marginal effect
estat mfx

* Multinomial Logit Model
asclogit dum, case(v1) alternatives(price) casevar(income)
eststo M_logit
esttab M_logit, se(5) title(The effect of family income on demand) nonumbers mtitles("M_logit")
* calculate marginal effect
estat mfx

***************************************************************
* Exercise 5 : IIA
***************************************************************
* Mixed Logit Model
asmixlogit dum, random(c) casevar(income) alternatives(price) case(v1)
eststo Mixed_logit
esttab Mixed_logit, se(5) title(Mixed Logit Model) nonumbers mtitles("Mixed_logit_betaf")

* consider an alternative specification
* remove data from one choice(ex: choice 7)
drop if choice == 7
drop if price == 7
asmixlogit dum, random(c) casevar(income) alternatives(price) case(v1)
eststo Mixed_logit7
esttab Mixed_logit7, se(5) title(Alternative Mixed Logit Model) nonumbers mtitles("Mixed_logit_betar")

* calculate IIA
hausman Mixed_logit Mixed_logit7, alleqs constant


******************************* Assignment 4 *******************************
***************************************************************
* Exercise 1 : Data
***************************************************************
clear
* import data
import delimited "C:\Users\Administrator\Downloads\Econ613-master (1)\Econ613-master\Assignments\A4\Koop-Tobias.csv"
* declare the panel identifiers
xtset personid timetrnd
* represent the panel dimension of wages for 5 selected individuals
plot logwage timetrnd if personid == 1
plot logwage timetrnd if personid == 2
plot logwage timetrnd if personid == 3
plot logwage timetrnd if personid == 4
plot logwage timetrnd if personid == 5


***************************************************************
* Exercise 2 : Random Effects
***************************************************************
xtreg logwage educ potexper, re
eststo Panel_RE
esttab Panel_RE, se(5) title(Random effects) nonumbers mtitles("Panel_RE")


***************************************************************
* Exercise 3 : Fixed Effects Model
***************************************************************
* Between
xtreg logwage educ potexper,be
eststo Panel_FE_be

* Within
xtreg logwage educ potexper,fe
eststo Panel_FE_fe

* First difference 
* generate continuous timetrend
drop timetrnd
gen t = _n
rename (t) (timetrnd)
* re-declare the panel identifiers
xtset personid timetrnd
* generate lag time series
gen diff_logwage = D.logwage
gen diff_educ = D.educ
gen diff_potexper = D.potexper
* run regression
xtreg diff_logwage diff_educ diff_potexper, fe
eststo Panel_FE_diff

* display fixed effects model estimations results
esttab Panel_FE_be Panel_FE_fe Panel_FE_diff, se(5) title(Fixed Effects Model) nonumbers mtitles("between" "within" "f_diff")

log close

