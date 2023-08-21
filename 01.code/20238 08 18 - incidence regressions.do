********************************************************************************
//  Data prep ------------------------------------------------------------------
********************************************************************************

* tempfile foo
* di "`foo'"

// 0.0 Settings and data loading -----------------------------------------------

set more off
clear all

cd "L:/01.ecsc2022/02.enj2022/00.enj_data/03.stata regs"

use "dt_pl1.dta", replace

svyset keyh [pweight = FEX_C], /*
*/ vce(linearized) /*
*/ singleunit(missing) /*
*/ || keyp

// -----------------------------------------------------------------------------
// 1.0 Creates variables  ------------------------------------------------------
// -----------------------------------------------------------------------------

describe
sum

// -----------------------------------------------------------------------------
* 1.11 General changes in coding 
*replace p6210 = .  if p6210 == 9 // Replace not declared with missing values to garantee ordinal scales
*replace p220 = 0  if p220 == 2   // Recode gender for woman from 2 to 0 - base line references male declarants
*gen lp = 1 // Count for declared justiable problems
*label variable lp  "count for declared justiable problems"

*replace p1324 = 0  if p1324 == 2   // Recode reporting of theft 

// -----------------------------------------------------------------------------
* 2. Full data set descriptives ------------------------------------------------
// ----------------------------------------------------------------------------- 

preserve

duplicates drop keyp, force
keep if a18 == 1
*collapse (sum) fex = FEX_C, by(jp) 
collapse (sum) fex = FEX_C
list

restore

// -----------------------------------------------------------------------------
* 3. drops and sub sets --------------------------------------------------------
// ----------------------------------------------------------------------------- 

*3.1 Drops records with no valid solution or valid response on retaking paths --------------------------------------------------------------------------------

keep if a18 == 1 // only individuals over 17

tab jp, m
drop if missing(jp) // valid declaration records

// -----------------------------------------------------------------------------
* 3. variable sets -------------------------------------------------------------
// ----------------------------------------------------------------------------- 

// var sets --------------------------------------------------------------------
 
label define jp 0 "nondeclarant" 1 "declarant"
gl Y  jp // problem declaration

// demographics ----------------------------------------------------------------

* P220 sex
* P5785 age
* recon self recognition
* dis disabilities
* single marital status
* uage minors in household
* pea activities (work) 
* edug educational level
* P3303 internet connection
* swbi subjective wellbeing 
* P3503S1_1 SWB life
* P3503S2_2 SWB health
* P3503S3_3 SWB economic outlook
* P3503S4_4 SWB work
* P3503S5_5  SWB emotional outlook
* P3503S6_6 SWB relationships
* P1988S1 household strata
* ownedh household ownership
* Clase household type

encode Clase, gen(Clase1) // swaped labels in Clase
replace Clase1 = 0 if Clase1 == 1
replace Clase1 = 1 if Clase1 == 2
drop Clase

table edug // swaped labels in educational level
label list edug
replace edug = 0 if edug == 1
replace edug = 1 if edug == 2 

gl  v_demo  "P220 P5785 recon dis single uage edug pea P3303 swbi ownedh Clase1"

// victimization ---------------------------------------------------------------

* P564 prospects on being a victim in the future
* abuse street abuse and sexual abuse experiences
* vic_2021  victimization 2021
* vic_2022 victimization 2022
* physicalviolence physical violence
* crimereporting crime reporting 

gl  v_vict  "P564 abuse vic_2022"

// perception o security -------------------------------------------------------

* P1182S1 Police
* P1182S2 Military
* P1182S3 Major
* P1181S1 Prosecutor office
* P1181S2 Judges
* P3317S1 ICBF
* P3317S2 Police inspections
* P3317S3 Conciliation centres
* safe_local security perception (local)
* safe_WaN security perception when walking alone at night
* safe_city security perception (municipality/city)

gl  v_perc  "   P1181S1 P1181S2 P1182S1 P3317S3 safe_local safe_WaN safe_city"

// -----------------------------------------------------------------------------
* 4.  descrptves on variable sets ----------------------------------------------
// ----------------------------------------------------------------------------- 

sum $v_demo
sum $v_vict
sum $v_perc

* weighted variance ratios 
rdbalance $Y $v_demo $v_vict $v_perc , /* 
*/ s(N mean sd median min max) /*
*/ wt(FEX_C) /*
*/ f(%9.0g) /*
*/ saving(v01_balance_Vdiffinac.xls, replace) excel

* mean differences
file open table_incd using "v01_balance_Mdiffinac.csv", w replace
file write table_incd /*
*/ "Trait (obs), Nodeclaration, Declaration, s1, s2, n1, n2, p value" _n

foreach var in  $v_demo $v_vict $v_perc {
qui: ttest `var', by($Y)
		local Nodeclaration = round(r(mu_1),0.01)
		local Declaration = round(r(mu_2),0.01)
		local s1 = round(r(sd_1),0.01)
		local s2 = round(r(sd_2),0.01)
		local n1 = round(r(N_1),0.01)
		local n2 = round(r(N_2),0.01)
		local p = round(r(p),0.01)

file write table_incd /*
*/ "`var' (`n'), `Nodeclaration', `Declaration', `s1',`s2',`n1',`n2',`p'" _n
}

file close table_incd

// -----------------------------------------------------------------------------
* 4. regression on variable sets ----------------------------------------------
// ----------------------------------------------------------------------------- 

gl v_all $v_demo $v_vict $v_perc
local vars ""$v_demo" "$v_vict" "$v_perc" "$v_all"" 
local counter = 0

foreach var of local vars  {
    
local counter = `counter' + 1
reg $Y `var' [pweight = FEX_C], vce(robust) 
	est store reg_inc_`counter'
}

reg $Y $v_demo $v_perc [pweight = FEX_C], vce(robust) 
	est store reg_inc_5


esttab/*  
*/ reg_inc_1/*
*/ reg_inc_2/*
*/ reg_inc_3/*
*/ reg_inc_5/*
*/ reg_inc_4/*

*/ using "1.5.1.linear_reg_per_pro robust.rtf", b(%5.3f) se(%5.3f) replace


 * regression conditional on civil issues declarants ---------------------------

preserve

drop if  pjt == 3
drop if  pjt == 1

local vars ""$v_demo" "$v_vict" "$v_perc" "$v_all"" 
local counter = 0

foreach var of local vars  {
    
local counter = `counter' + 1
reg $Y `var' [pweight = FEX_C], vce(robust) 
	est store reg_inc_`counter'
}

reg $Y $v_demo $v_perc [pweight = FEX_C], vce(robust) 
	est store reg_inc_5


esttab/*  
*/ reg_inc_1/*
*/ reg_inc_2/*
*/ reg_inc_3/*
*/ reg_inc_5/*
*/ reg_inc_4/*

*/ using "1.5.2.linear_reg_per_pro robust Civil.rtf", b(%5.3f) se(%5.3f) replace

restore

 * regression conditional on criminal issues declarants ------------------------

preserve

drop if  pjt == 3
drop if  pjt == 2

local vars ""$v_demo" "$v_vict" "$v_perc" "$v_all"" 
local counter = 0

foreach var of local vars  {
    
local counter = `counter' + 1
reg $Y `var' [pweight = FEX_C], vce(robust) 
	est store reg_inc_`counter'
}

reg $Y $v_demo $v_perc [pweight = FEX_C], vce(robust) 
	est store reg_inc_5


esttab/*  
*/ reg_inc_1/*
*/ reg_inc_2/*
*/ reg_inc_3/*
*/ reg_inc_5/*
*/ reg_inc_4/*

*/ using "1.5.3.linear_reg_per_pro robust Crime.rtf", b(%5.3f) se(%5.3f) replace

restore












listcoef
sum $v_demo
predict dec_demo
hist dec_demo

reg $Y $v_perc $v_vict $v_demo  [ pweight = FEX_C] , /*
 */ vce(robust) 
	est store reg151_2
ereturn  list 

listcoef
sum $v_vict 
predict dec_vict 
hist dec_vict 


logit $Y $v_demo [pweight = FEX_C]
listcoef
prchange
prtab swb dis, x(P5785= mean) rest(min)
 predict dec_demo1
 hist dec_demo1
 

 







