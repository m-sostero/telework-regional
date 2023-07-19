* First analysis of teleworkability and telework by region in Europe
* JRC-Eurofound collaborative project
* July 2023
* Enrique Fernández-Macías and Matteo Sostero

set more off
clear

* Local directory for Enrique
cd "C:\Users\a\Documents\teleworkability with eurofound\Regional 2023 EF_JRC"

* Local directory for Matteo
cd "C:\Users\sostema\repositories\Telework regional\Data"

* Load LFS data, produced by Matteo,
* based on John's LFS extraction (LFSreg2018_21finalr.dta)
* and cleaned based on Enrique's initial analysis
use "LFS_regression.dta" 

/*
label variable broadband_shh "Share of households with broadband access (at NUTS 1-2 level)"
label variable work_location "Place of work, relative to place of residence (different region/country)"
label variable physicalinteraction "TWY: physical interaction"
label variable socialinteraction "TWY: social interaction"
*/

**# Data and variable details *******************************************************

* Includes the following countries and occupations (excludes BG, MT, SI, because their ISCO were <3 digits)
tab isco08_3d country

* Includes employees and self-employed (excludes unemployed, outside labour force, family workers and "non applicable")
tab stapro

* Place of work, relative to place of employment
* Comparison based on region of resigence (reg, NUTS0-1-2), region of work (regw NUTS2), and country of work (ctryw)
tab work_location

* Regional connectivity statistics (share of households with broadband access)
* Based on reg (NUTS0-1-2)
version 16: table reg, c(mean broadband_shh)

* Teleworkability indices defined for all ISCO 3-digit occupations (includes recoding of some occupations by Enrique)
version 16: table isco08_3d [aw=coeffy], c(mean physical mean social)

* Generate progressive observation number (used for collapse?)
gen n = 1 


********************************************************************************

* Teleworkability vs actual telework: did the pandemic change the link between the?

* To what an extent does telework vary by occupation? (defined at the isco 3d level)
* We can use the pseudo-continuous variable to do a quick check

levelsof country, local(c_local)
levelsof year, local(y_local)

gen coeff_isco3d = .
foreach c in `c_local' {
	foreach y in `y_local' {
		reg homework_i i.isco08_3d if country == `c' & year == `y' [pw=coeffy], notable
		replace coeff_isco3d = e(r2_a) if country == `c' & year == `y'
	}
}
collapse (mean) coeff_isco3d, by(country year)
ren coeff_* *
reshape wide isco, i(country) j(year)
order country isco*
save "anova_isco_nuts.dta", replace

graph bar (mean) isco3d2018 isco3d2019 isco3d2020 isco3d2021, over(country) bar(1, color(gs13)) bar(2, color(gs10)) bar(3, color(gs6)) bar(4, color(gs2)) legend(off) ysize(2)
graph export "anova_homew_isco3d.png", wid(1024) hei(256) replace

* The explanatory value of isco3d with respect to actual telework varied between
* .1 and .2 before COVID. In most cases, it increased very substantially, to between
* .2 and .4. This on its own makes it more likely that our teleworkability index
* becomes a better predictor of actual telework after COVID.


**# Explaining telework at the individual level ***********************************
* before and after covid, at the individual level, including our index

use "LFS_regression.dta", clear

* Generate some individual-level variables

* Age (young, prime-age, old) x sex
recode age (1/3 = 1 Young) (4/7 = 2 Prime) (8/13 = 3 Old), gen(agecat)
gen yw = agecat == 1 & sex == 2
gen pw = agecat == 2 & sex == 2
gen ow = agecat == 3 & sex == 2
gen ym = agecat == 1 & sex == 1
gen pm = agecat == 2 & sex == 1
gen om = agecat == 3 & sex == 1

* Education level
gen lowed = hatlev >= 0 & hatlev <= 200
gen mided = hatlev >= 300 & hatlev <= 499
gen highed = hatlev >= 500 & hatlev < .

* Part-time, self-employment, temporary contract, non-national
gen pt = ftpt == 2
gen temporary = temp == 2
gen nonnat = countryb != "NAT"
labelbook stapro
fvset base 2 stapro
* (Set employees as the default category)

* Geographic variables:

*capital region/country
labelbook urbrur
gen capital = urbrur == 1

labelbook country
fvset base 6 country
* (Set Germany as the default category)



**# Estimate models, year by year, incrementally ***************************

* Create lists of variables to add incrementally to specifications
vl create twy = (physicalinteraction socialinteraction)
vl create individual = (yw pw ow ym om lowed highed nonnat)
vl create work = (ftpt stapro temporary work_location)

eststo clear

forval i = 2018(1)2021 {
	* B1: teleworkability indices
	quietly: reg homework_any $twy [pw=coeffy] if year == `i'
	eststo B1_`i', title("`i'")
	
	* B2: (B1) + individual characteristics
	quietly: reg homework_any $twy $individual [pw=coeffy] if year == `i'
	eststo B2_`i', title("`i'")
	
	* B3: (B2) + work characteristics
	quietly: reg homework_any $twy $individual i.($work) [pw=coeffy] if year == `i'
	eststo B3_`i', title("`i'")
	
	* B4: (B3) + regional characteristics (not a varlist, because they are of different types)
	quietly: reg homework_any $twy $individual i.($work) i.degurba capital broadband_shh [pw=coeffy] if year == `i'
	eststo B4_`i', title("`i'")
	
	* B5: (B4) + country
	quietly: reg homework_any $twy $individual i.($work) i.degurba capital broadband_shh i.country [pw=coeffy] if year == `i'
	eststo B5_`i', title("`i'")
}


* Preview the different model blocks
// NB: This syntax for model titles requires a recent version of esttab
// ssc install estout, replace
forval b = 1/5 {
	esttab B`b'_*, nonotes not r2 lab compress obslast mlabels(,title)
}

esttab B1_* using ind_models_any.tsv, nonotes not r2 lab compress obslast mlabels(,titles) tab replace
forval b = 2/5 {
	esttab B`b'_* using ind_models_any.tsv, nonotes not r2 lab compress obslast mlabels(,title) tab append
}


* Block 1bis: how does this compare with ISCO at 3 digits?

forval i = 2018(1)2021 {
	quietly: reg homework_any i.isco08_3d [pw=coeffy] if year == `i'
	eststo Year`i', title("`i'")
}
quietly: reg homework_any i.isco08_3d [pw=coeffy]
eststo All

esttab, nonotes drop(*) nocons not r2 lab compress obslast mlabels(,titles)
esttab using occ_models_any.tsv, nonotes drop(*) nocons not r2 lab compress obslast tab append
eststo clear
