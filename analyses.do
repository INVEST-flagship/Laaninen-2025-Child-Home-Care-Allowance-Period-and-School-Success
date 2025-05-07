**************
* ANALYSES

* Markus Laaninen
* majlaan@utu.fi 
* HCA study
*************
cd "W:\markus_laaninen\hca"
do "W:\markus_laaninen\hca\do\master.do"

use "$temp\final_5", clear 

* settings
xtset famid
global covariates i.sukup c.sib_order c.childcount c.agetonext i.mtoim i.viimeinen2 i.fedu4_1 i. pltwcw3 i.pltwcw6 c.fune_total c.mune_total



****************
* Table 2. Descriptive statistics
***************

table (var), statistic(min sukup) statistic(max sukup) statistic(mean sukup) statistic(sd sukup) statistic(fvfreq sukup) statistic(fvpercent sukup)  stat(count sukup) ///
statistic(min koulu3) statistic(max koulu3) statistic(mean koulu3) statistic(sd koulu3) statistic(fvfreq koulu3) statistic(fvpercent koulu3) stat(count koulu3) ///
statistic(min medu4_1) statistic(max medu4_1) statistic(mean medu4_1) statistic(sd medu4_1) statistic(fvfreq medu4_1) statistic(fvpercent medu4_1) stat(count medu4_1) /// 
statistic(min fedu4_1) statistic(max fedu4_1) statistic(mean fedu4_1) statistic(sd fedu4_1) statistic(fvfreq fedu4_1) statistic(fvpercent fedu4_1) stat(count fedu4_1) ///
statistic(min syntyp2) statistic(max syntyp2) statistic(mean syntyp2) statistic(sd syntyp2) statistic(fvfreq syntyp2) statistic(fvpercent syntyp2) stat(count syntyp2) ///
statistic(min tausta6) statistic(max tausta6) statistic(mean tausta6) statistic(sd tausta6) statistic(fvfreq tausta6) statistic(fvpercent tausta6) stat(count tausta6) ///
statistic(min mtoim) statistic(max mtoim) statistic(mean mtoim) statistic(sd mtoim) statistic(fvfreq mtoim) statistic(fvpercent mtoim) stat(count mtoim) ///
statistic(min pltwcw3) statistic(max pltwcw3) statistic(mean pltwcw3) statistic(sd pltwcw3) statistic(fvfreq pltwcw3) statistic(fvpercent pltwcw3) stat(count pltwcw3) ///
statistic(min pltwcw6) statistic(max pltwcw6) statistic(mean pltwcw6) statistic(sd pltwcw6) statistic(fvfreq pltwcw6) statistic(fvpercent pltwcw6) stat(count pltwcw6) ///
statistic(min viimeinen2) statistic(max viimeinen2) statistic(mean viimeinen2) statistic(sd viimeinen2) statistic(fvfreq viimeinen2) statistic(fvpercent viimeinen2)  stat(count viimeinen2) ///
statistic(min std_aikka2) statistic(max std_aikka2) statistic(mean std_aikka2) statistic(sd std_aikka2) stat(count std_aikka2) ///
statistic(min hca_total) statistic(max hca_total) statistic(mean hca_total) statistic(sd hca_total) stat(count hca_total) ///
statistic(min childcount) statistic(max childcount) statistic(mean childcount) statistic(sd childcount) stat(count childcount) ///
statistic(min sib_order) statistic(max sib_order) statistic(mean sib_order) statistic(sd sib_order) stat(count sib_order) ///
statistic(min mune_total) statistic(max mune_total) statistic(mean mune_total) statistic(sd mune_total) stat(count mune_total) ///
statistic(min fune_total) statistic(max fune_total) statistic(mean fune_total) statistic(sd fune_total) stat(count fune_total) 

collect export Table2_1, as(xlsx) replace


table (var) if agetonext !=999, statistic(min agetonext) statistic(max agetonext) statistic(mean agetonext) statistic(sd agetonext) stat(count agetonext) // this separately

collect export Table2_2, as(xlsx) replace


fre agetonext if sfam !=0 & agetonext !=999 // N 

* check anonymity for stat Finland output
fre childcount // 15 or more
fre sib_order // 10th or higher
fre hca_total // 80 or more 
fre agetonext // 65 or more
fre fune_total
fre mune_total
tab mune_total


**********************
* check the outcomes
****************************
* means by birth year 
mean aikka, over(osyntyv)
mean matikka, over(osyntyv) // something with math for the 99 cohort? Ommit this cohort
*****************************

********************
********************
* Regression models
********************
********************

***********
* Table 3: parental education
**********

reg std_aikka2 c.hca_total##i.medu4_1, base
eststo model_a1
estadd local Fe No
estadd local Controls No
estadd scalar ar `=e(r2)'

reg std_aikka2 c.hca_total##i.medu4_1 i.syntyp2 $covariates, base
eststo model_a2
estadd local Fe No
estadd local Controls Yes
estadd scalar ar `=e(r2_a)'

margins, at (hca_total = (0 (10) 80) medu4_1 = (1 2 3))
marginsplot

xtreg std_aikka2 c.hca_total#i.medu4_1, base fe robust
eststo model_a3
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls No
estadd scalar ar `=e(r2)'

xtreg std_aikka2 c.hca_total#i.medu4_1 i.syntyp2 $covariates, base fe robust
eststo model_a4
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls Yes
estadd scalar ar `=e(r2_a)'

* koulu3 
xtreg std_aikka2 c.hca_total#i.koulu3 i.syntyp2 $covariates, base fe robust
eststo model_a5
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls Yes
estadd scalar ar `=e(r2_a)'

esttab model_a1 model_a2 model_a3 model_a4 model_a5 using table3.rtf, ///
se(4) ///
nopar /// 
b(4) ///
mtitles("Model 1" "Model 2" "Model 3" "Model 4") ///
nonumbers ///
varwidth(50) ///
interaction(" X ") ///
title (Table 3. OLS and family fixed-effect models on the association between literacy grade and HCA months by parental education.) ///
s(Fe Controls N Families ar, label("FE" "Controls" "N" "Families" "R-squared / Adjusted R-squared" )) ///
compress /// 
label ///
replace


***********
* Table 4: immigration status
***********

reg std_aikka2 c.hca_total##i.syntyp2, base
eststo model_b1
estadd local Fe No
estadd local Controls No
estadd scalar ar `=e(r2)'

reg std_aikka2 c.hca_total##i.syntyp2 i.medu4_1 $covariates, base
eststo model_b2
estadd local Fe No
estadd local Controls Yes
estadd scalar ar `=e(r2_a)'

xtreg std_aikka2 c.hca_total#i.syntyp2, base fe robust
eststo model_b3
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls No
estadd scalar ar `=e(r2)'

xtreg std_aikka2 c.hca_total#i.syntyp2 i.medu4_1 $covariates if pltwcw6 ==1, base fe robust
eststo model_b4
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls Yes
estadd scalar ar `=e(r2_a)'

esttab model_b1 model_b2 model_b3 model_b4 using table4.rtf, ///
se(4) ///
nopar /// 
b(4) ///
mtitles("Model 1" "Model 2" "Model 3" "Model 4") ///
nonumbers ///
varwidth(50) ///
interaction(" X ") ///
title (Table 4. OLS and family fixed-effect models on the association between literacy grade and HCA months by immigrationstatus.) ///
s(Fe Controls N Families ar, label("FE" "Controls" "N" "Families" "R-squared / Adjusted R-squared" )) ///
compress /// 
label ///
replace


***********
* Table 5: ethnic origins
***********

reg std_aikka2 c.hca_total##i.tausta6, base
eststo model_c1
estadd local Fe No
estadd local Controls No
estadd scalar ar `=e(r2)'

reg std_aikka2 c.hca_total##i.tausta6 i.syntyp2 i.medu4_1 $covariates , base
eststo model_c2
estadd local Fe No
estadd local Controls Yes
estadd scalar ar `=e(r2_a)'

xtreg std_aikka2 c.hca_total#i.tausta6, base fe robust
eststo model_c3
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls No
estadd scalar ar `=e(r2)'

xtreg std_aikka2 c.hca_total#i.tausta6 i.syntyp2 i.medu4_1 $covariates, base fe robust
eststo model_c4
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls Yes
estadd scalar ar `=e(r2_a)'


esttab model_c1 model_c2 model_c3 model_c4 using table5.rtf, ///
se(4) ///
nopar /// 
b(4) ///
mtitles("Model 1" "Model 2" "Model 3" "Model 4") ///
nonumbers ///
varwidth(50) ///
interaction(" X ") ///
title (Table 5. OLS and family fixed-effect models on the association between literacy grade and HCA months by ethnic origins.) ///
s(Fe Controls N Families ar, label("FE" "Controls" "N" "Families" "R-squared / Adjusted R-squared" )) ///
compress /// 
label ///
replace

****************
****************
* Robust checks
****************
****************

**************
* Table A4: three way interaction
**************

reg std_aikka2 c.hca_total##i.syntyp2##i.medu4_1, base
eststo model_d1
estadd local Fe No
estadd local Controls No
estadd scalar ar `=e(r2)'

reg std_aikka2 c.hca_total##i.syntyp2##i.medu4_1 $covariates , base
eststo model_d2
estadd local Fe No
estadd local Controls Yes
estadd scalar ar `=e(r2_a)'

xtreg std_aikka2 c.hca_total#i.syntyp2#i.medu4_1, base fe robust
eststo model_d3
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls No
estadd scalar ar `=e(r2)'

xtreg std_aikka2 c.hca_total#i.syntyp2#i.medu4_1 $covariates , base fe robust
eststo model_d4
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls Yes
estadd scalar ar `=e(r2_a)'

esttab model_d1 model_d2 model_d3 model_d4 using table_a4.rtf, ///
se(4) ///
nopar /// 
b(4) ///
mtitles("Model 1" "Model 2" "Model 3" "Model 4") ///
nonumbers ///
varwidth(50) ///
interaction(" X ") ///
title (Table A4. OLS and family fixed-effect models on the association between literacy grade and HCA months by parental education and immigration status.) ///
s(Fe Controls N Families ar, label("FE" "Controls" "N" "Families" "R-squared / Adjusted R-squared" )) ///
compress /// 
label ///
replace

**************
* Table A5: math and parental education
*************

reg std_matikka2 c.hca_total##i.medu4_1 if osyntyv != 1999, base
eststo model_p1
estadd local Fe No
estadd local Controls No
estadd scalar ar `=e(r2)'

reg std_matikka2 c.hca_total##i.medu4_1 i.syntyp2 $covariates if osyntyv != 1999, base
eststo model_p2
estadd local Fe No
estadd local Controls Yes
estadd scalar ar `=e(r2_a)'

xtreg std_matikka2 c.hca_total#i.medu4_1 if osyntyv != 1999, base fe robust
eststo model_p3
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls No
estadd scalar ar `=e(r2)'

xtreg std_matikka2 c.hca_total#i.medu4_1 i.syntyp2 $covariates if osyntyv != 1999, base fe robust
eststo model_p4
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls Yes
estadd scalar ar `=e(r2_a)'

esttab model_p1 model_p2 model_p3 model_p4 using table_a5.rtf, ///
se(4) ///
nopar /// 
b(4) ///
mtitles("Model 1" "Model 2" "Model 3" "Model 4") ///
nonumbers ///
varwidth(50) ///
interaction(" X ") ///
title (Table A5. OLS and family fixed-effect models on the association between math grade and HCA months by parental education.) ///
s(Fe Controls N Families ar, label("FE" "Controls" "N" "Families" "R-squared / Adjusted R-squared" )) ///
compress /// 
label ///
replace

****************
* Table A6: math and ethnic origins
********************

reg std_matikka2 c.hca_total##i.syntyp2 if osyntyv != 1999, base
eststo model_q1
estadd local Fe No
estadd local Controls No
estadd scalar ar `=e(r2)'

reg std_matikka2 c.hca_total##i.syntyp2 i.medu4_1 $covariates if osyntyv != 1999, base
eststo model_q2
estadd local Fe No
estadd local Controls Yes
estadd scalar ar `=e(r2_a)'

xtreg std_matikka2 c.hca_total#i.syntyp2 if osyntyv != 1999, base fe robust
eststo model_q3
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls No
estadd scalar ar `=e(r2)'

xtreg std_matikka2 c.hca_total#i.syntyp2 i.medu4_1 $covariates if osyntyv != 1999, base fe robust
eststo model_q4
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls Yes
estadd scalar ar `=e(r2_a)'

esttab model_q1 model_q2 model_q3 model_q4 using table_a6.rtf, ///
se(4) ///
nopar /// 
b(4) ///
mtitles("Model 1" "Model 2" "Model 3" "Model 4") ///
nonumbers ///
varwidth(50) ///
interaction(" X ") ///
title (Table A6. OLS and family fixed-effect models on the association between math grade and HCA months by immigration status.) ///
s(Fe Controls N Families ar, label("FE" "Controls" "N" "Families" "R-squared / Adjusted R-squared" )) ///
compress /// 
label ///
replace

**********
* Table A7: hca10 and parental edu
**********

reg std_aikka2 c.hca10##i.medu4_1, base
eststo model_e1
estadd local Fe No
estadd local Controls No
estadd scalar ar `=e(r2)'

reg std_aikka2 c.hca10##i.medu4_1 i.syntyp2 $covariates , base
eststo model_e2
estadd local Fe No
estadd local Controls Yes
estadd scalar ar `=e(r2_a)'

xtreg std_aikka2 c.hca10#i.medu4_1, base fe robust
eststo model_e3
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls No
estadd scalar ar `=e(r2)'

xtreg std_aikka2 c.hca10#i.medu4_1 i.syntyp2  $covariates , base fe robust
eststo model_e4
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls Yes
estadd scalar ar `=e(r2)'

esttab model_e1 model_e2 model_e3 model_e4 using table_a7.rtf, ///
se(4) ///
nopar /// 
b(4) ///
mtitles("Model 1" "Model 2" "Model 3" "Model 4") ///
nonumbers ///
varwidth(50) ///
interaction(" X ") ///
title (Table A7. OLS and family fixed-effect models on the association between literacy grade and HCA groups by parental education.) ///
s(Fe Controls N Families ar, label("FE" "Controls" "N" "Families" "R-squared / Adjusted R-squared" )) ///
compress /// 
label ///
replace


**********
* Table A8: hca10 and ethnic origins
**********

reg std_aikka2 c.hca10##i.syntyp2, base
eststo model_m1
estadd local Fe No
estadd local Controls No
estadd scalar ar `=e(r2)'

reg std_aikka2 c.hca10##i.syntyp2 i.medu4_1 $covariates , base
eststo model_m2
estadd local Fe No
estadd local Controls Yes
estadd scalar ar `=e(r2)'

xtreg std_aikka2 c.hca10#i.syntyp2, base fe robust 
eststo model_m3
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls No
estadd scalar ar `=e(r2)'

xtreg std_aikka c.hca10#i.syntyp2 i.medu4_1  $covariates , base fe robust 
eststo model_m4
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls Yes
estadd scalar ar `=e(r2)'

esttab model_m1 model_m2 model_m3 model_m4 using table_a8.rtf, ///
se(4) ///
nopar /// 
b(4) ///
mtitles("Model 1" "Model 2" "Model 3" "Model 4") ///
nonumbers ///
varwidth(50) ///
interaction(" X ") ///
title (Table A8. OLS and family fixed-effect models on the association between literacy grade and HCA groups by immigration status.) ///
s(Fe Controls N Families ar, label("FE" "Controls" "N" "Families" "R-squared / Adjusted R-squared" )) ///
compress /// 
label ///
replace

******************
* Table A9
*****************

fre koulu3 if sfam !=0 & jani2 ==1 // small sample for basic educated parents group

reg std_aikka2 c.hca_total##i.medu4_1 if /*childcount ==2 &*/ jani2 ==1, base 
eststo model_f1
estadd local Fe No
estadd local Controls No
estadd scalar ar `=e(r2)'

reg std_aikka2 c.hca_total##i.medu4_1 i.syntyp2 $covariates  if /*childcount ==2 &*/ jani2 ==1, base 
eststo model_f2
estadd local Fe No
estadd local Controls Yes
estadd scalar ar `=e(r2)'

xtreg std_aikka2 c.hca_total#i.medu4_1 if /*childcount ==2 &*/ jani2 ==1, base fe robust
eststo model_f3
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls No
estadd scalar ar `=e(r2)'

xtreg std_aikka2 c.hca_total#i.medu4_1 i.syntyp2  $covariates  if /*childcount ==2 &*/ jani2 ==1, base fe robust
eststo model_f4
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls Yes
estadd scalar ar `=e(r2)'

esttab model_f1 model_f2 model_f3 model_f4 using table_a9.rtf, ///
se(4) ///
nopar /// 
b(4) ///
mtitles("Model 1" "Model 2" "Model 3" "Model 4") ///
nonumbers ///
varwidth(50) ///
interaction(" X ") ///
title (Table A9. OLS and family fixed-effect models on the association between literacy grade and HCA months by parental education. Estimates are based on families where younger sibling had more HCA months. ) ///
s(Fe Controls N Families ar, label("FE" "Controls" "N" "Families" "R-squared / Adjusted R-squared" )) ///
compress /// 
label ///
replace


****************
* Table A10
*****************

fre syntyp2 if sfam !=0 & jani2 ==1 // small sample for immigrants

reg std_aikka2 c.hca_total##i.syntyp2 if jani2 ==1, base 
eststo model_g1
estadd local Fe No
estadd local Controls No
estadd scalar ar `=e(r2)'

reg std_aikka2 c.hca_total##i.syntyp2 i.medu4_1 $covariates if jani2 ==1, base 
eststo model_g2
estadd local Fe No
estadd local Controls Yes
estadd scalar ar `=e(r2)'

xtreg std_aikka2 c.hca_total#i.syntyp2 if jani2 ==1, base fe robust 
eststo model_g3
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls No
estadd scalar ar `=e(r2)'

xtreg std_aikka2 c.hca_total#i.syntyp2 i.medu4_1 $covariates if jani2 ==1, base fe robust 
eststo model_g4
estadd scalar Families `=e(N_g)'
estadd local Fe Yes
estadd local Controls Yes
estadd scalar ar `=e(r2)'

esttab model_g1 model_g2 model_g3 model_g4 using table_a10.rtf, ///
se(4) ///
nopar /// 
b(4) ///
mtitles("Model 1" "Model 2" "Model 3" "Model 4") ///
nonumbers ///
varwidth(50) ///
interaction(" X ") ///
title (Table A10. OLS and family fixed-effect models on the association between literacy grade and HCA months by immigration status. Estimates are based on families where younger sibling had more HCA months. ) ///
s(Fe Controls N Families ar, label("FE" "Controls" "N" "Families" "R-squared / Adjusted R-squared" )) ///
compress /// 
label ///
replace



