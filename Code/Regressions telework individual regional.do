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

* Generate "isco0d" broad occupational classifications, by John (adapted to start from 3-digits)
recode isco08_3d (100/299=1)(300/599=2)(600/799=3)(800/969=4), gen(isco0d)

label define isco0d 1 "White-collar high-skilled"
label define isco0d 2 "White-collar low/mid-skilled", add
label define isco0d 3 "Blue-collar high-skilled", add
label define isco0d 4 "Blue-collar low/mid-skilled", add
label values isco0d isco0d
label variable isco0d "Occupation classes"


* Age (young, prime-age, old) x sex
recode age (1/3 = 1 Young) (4/7 = 2 Prime) (8/13 = 3 Old), gen(agecat)
gen yw = agecat == 1 & sex == 2
gen pw = agecat == 2 & sex == 2
gen ow = agecat == 3 & sex == 2
gen ym = agecat == 1 & sex == 1
gen pm = agecat == 2 & sex == 1
gen om = agecat == 3 & sex == 1

label variable yw "Young women"
label variable pw "Prime-age women"
label variable ow "Old women"
label variable ym "Young men"
label variable pm "Prime-age men"
label variable om "Old men"

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


**# Estimate individual-level pooled models, present side-by-side -------------------------------------

* Create lists of controls to add incrementally to specifications
global twy        c.physicalinteraction c.socialinteraction
global individual yw pw ow ym om lowed highed nonnat 
// i.isco0d
global work       ftpt i.stapro temporary i.work_location
global regional   i.degurba capital broadband_shh i.reglab

* Clear previously-stored individual-level models from memory
eststo drop P*

* P1: teleworkability indices
qui reg homework_any ($twy)##i.year i.country [pw=coeffy]
estadd local fe Country
eststo P1

* P2: (P1) + individual characteristics
qui reg homework_any ($twy)##i.year $individual i.country [pw=coeffy] 
estadd local fe Country
eststo P2

* P3: (P2) + work characteristics
qui reg homework_any ($twy)##i.year $individual $work i.country [pw=coeffy] 
estadd local fe Country
eststo P3

* P4: (P3) + regional characteristics 
qui reg homework_any ($twy)##i.year $individual $work $regional [pw=coeffy]
estadd local fe Region
eststo P4

* P5: (P4) - teleworkability indicators (for comparison) 
qui reg homework_any i.year $individual $work $regional [pw=coeffy]
estadd local fe Region
eststo P5

* export results
* Temporarily change end-of-line delimiter to ";", to wrap esstab instructions across multiple lines
#delim ;
esttab P*, nonotes not r2 lab compress obslast 
	drop(*country* *reglab*) stats(fe r2 N, labels("Geographical FE" "Adjusted R-squared"));

esttab P* using "..\Tables\reg_individual_pooled.tsv", nonotes nomtitles se not r2 lab compress obslast tab replace;

esttab P* using "..\Tables\reg_individual_pooled.rtf", not r2 lab compress obslast 
		drop(*country* *reglab*) stats(fe N r2, labels("Geographical FE" "N" "Adjusted R-squared"))
		title("Person teleworking (binary)") nomtitles replace;
		
esttab P3 using "..\Tables\reg_individual_country_fe.csv", plain lab wide not se nostar noobs keep(*country*) nomtitles nonotes replace;
		
#delim cr



**# Estimate individual-level models, year by year, incrementally ***************************

eststo drop B*

forval y = 2018(1)2021 {
	* B1: teleworkability indices
	qui reg homework_any $twy i.reglab [pw=coeffy] if year == `y'
	eststo B1_`y', title("`y'")
 	estadd local fe_region Yes
	
// 	* B2: (B1) + individual characteristics
// 	qui reg homework_any $twy $individual i.reglab [pw=coeffy] if year == `y'
// 	eststo B2_`y', title("`y'")
// 	estadd local fe_country Yes
//	
// 	* B3: (B2) + work characteristics
// 	qui reg homework_any $twy $individual $work i.reglab [pw=coeffy] if year == `y'
// 	eststo B3_`y', title("`y'")
// 	estadd local fe_region Yes
	
	* B4: (B3) + regional characteristics
	qui reg homework_any $twy $individual $work $regional i.reglab [pw=coeffy] if year == `y'
	eststo B4_`y', title("`y'")
	estadd local fe_region  Yes
}

* Notice that we're using country fixed effects everywhere
* (I tried with xtreg, but it doesn't support individual weights)


* Preview the different model blocks
// NB: This syntax for model titles requires a recent version of esttab
// ssc install estout, replace
forval b = 1/4 {
	esttab B`b'_*, nonotes not r2 lab compress obslast drop(*reglab*) mlabels(,title)
}


* Tables for simple and full individual model
#delim ;
esttab B1_* using "..\Tables\reg_individual_year_simple.tsv", replace tab ///
	nonotes not lab compress obslast mlabels(,titles) ///
	stats(fe_region N r2, labels("Region FE" "N" "Adjusted R-squared")) ///
	title("Probability of person working from home, at least some of the time");
	
esttab B4_* using "..\Tables\reg_individual_year_full.tsv", replace tab ///
	nonotes not lab compress obslast mlabels(,titles) ///
	stats(fe_region N r2, labels("Region FE" "N" "Adjusted R-squared")) ///
	title("Probability of person working from home, at least some of the time");
	
esttab B1_* using "..\Tables\reg_individual_year_simple.rtf", replace ///
	not r2 lab compress obslast drop(*reglab*) mlabels(,titles) ///
	stats(fe_region N r2, labels("Region FE" "N" "Adjusted R-squared")) ///
	title("Probability of person working from home, at least some of the time");
	
esttab B4_* using "..\Tables\reg_individual_year_full.rtf", replace ///
	not r2 lab compress obslast drop(*reglab*) mlabels(,titles) ///
	stats(fe_region N r2, labels("Region FE" "N" "Adjusted R-squared")) ///
	title("Probability of person working from home, at least some of the time");
#delim cr



* Block 1bis: how does this compare with ISCO at 3 digits?

eststo clear
forval i = 2018(1)2021 {
	quietly: reg homework_any i.isco08_3d [pw=coeffy] if year == `i'
	eststo Occup`i', title("`i'")
}
quietly: reg homework_any i.isco08_3d [pw=coeffy]
eststo All

esttab Occup*, nonotes drop(*) nocons not r2 lab compress obslast mlabels(,titles)
esttab using "..\Tables\reg_occupation_year.tsv", nonotes drop(*) nocons not r2 lab compress obslast tab append
esttab using "..\Tables\reg_occupation_year.rtf",         drop(*) nocons not r2 lab compress obslast     append
eststo clear


**# Estimate regional-level models ---------------------------------------------------------------------

use "LFS_regression.dta", clear

* compute regional-level demographics
gen lowed = hatlev >= 0 & hatlev <= 200
gen mided = hatlev >= 300 & hatlev <= 499
gen highed = hatlev >= 500 & hatlev < .
gen city = degurba == 1
gen rural = degurba == 3
gen female = sex == 2
gen pt = ftpt == 2
gen self = stapro == 1
gen temporary = temp == 2
gen nonnat = countryb != "NAT"
recode age (1 = 17) (2 = 22) (3 = 27) (4 = 32) (5 = 37) (6 = 42) (7 = 47) (8 = 52) (9 = 57) (10 = 62) (11 = 67) (12 = 72) (13 = 77), gen(agecont)


* Generate "isco0d" broad occupational classifications, based on code by John (adapted to start from 3-digits)
recode isco08_3d (100/299=1)(300/599=2)(600/799=3)(800/969=4), gen(isco0d)
label define isco0d 1 "White-collar high-skilled"
label define isco0d 2 "White-collar low/mid-skilled", add
label define isco0d 3 "Blue-collar high-skilled", add
label define isco0d 4 "Blue-collar low/mid-skilled", add
label values isco0d isco0d
label variable isco0d "Occupation classes"

* expand the occupation classes in individual variables
tab isco0d, gen(occup_classes)

* drop occupation classes corresponding to NAs (makes globbing later easier)
*drop occup_classes5 occup_classes6


* Define regional-level sectoral variables
gen agric = nace1d == 1
gen manuf = nace1d == 3
gen privservadv = nace > 9 & nace < 15
gen publserv = nace > 14 & nace < 18

* Generate dummies for different work locations (for collapse later)
tab work_location, gen(work_l)
rename work_l2 work_other_region
rename work_l3 work_other_country

* Capital region/country
gen capital = urbrur == 1

* Generate progressive observation number (used for collapse?)
gen n = 1 

* Collapse at the regional, year (and country?) level
collapse (mean) homework_index (mean) homework_any (mean) physical (mean) social /// Dependent vars and teleworkability
	(mean) city (mean) rural (mean) capital (mean) broadband_shh (mean) work_other_region (mean) work_other_country /// Geographic characteristics
	(mean) female (mean) lowed (mean) highed (mean) pt (mean) self (mean) temporary (mean) nonnat (mean) agecont (mean) occup_classes* /// Demographics
	(mean) agric (mean) manuf (mean) privserv (mean) publserv /// Sector
	(rawsum) coeffy (rawsum) n [aw=coeffy], by(reglab year)

* Define variable groups
global twy        c.physicalinteraction c.socialinteraction
global geo    city rural i.capital broadband_shh work_other_region work_other_country
global demo   female lowed highed pt self temporary nonnat agecont
global sector agric manuf privserv publserv


* Estimate pooled regional regressions

eststo clear
* R0: just year, no teleworkability indices
qui reg homework_any i.year [pw=coeffy]
eststo R0

* R1: teleworkability indices
qui reg homework_any ($twy)##i.year [pw=coeffy]
eststo R1

* R2: (R1) + geographic characteristics
qui reg homework_any ($twy)##i.year $geo [pw=coeffy] 
eststo R2

* R3: (R2) + work characteristics
qui reg homework_any ($twy)##i.year $geo $demo [pw=coeffy] 
eststo R3

* R4: (R3) + sector characteristics 
qui reg homework_any ($twy)##i.year $geo $demo $sector [pw=coeffy]
eststo R4

* R5: (R4) - teleworkability indicators (for comparison) 
*qui reg homework_any i.year $geo $demo $sector [pw=coeffy]
*eststo R5


* Show and export regression tables
#delim ;
esttab R*, nonotes not r2 lab compress obslast;

esttab R* using "..\Tables\reg_regional_pooled.tsv", nonotes nomtitles se not r2 lab compress obslast tab replace;

esttab R* using "..\Tables\reg_regional_pooled.rtf", not r2 lab compress obslast 
		title("Share of people teleworking (by NUTS region)") nomtitles replace;
#delim cr




**# Simpler regional-level models ---------------------------------------------------------------------

use "LFS_regression.dta", clear

* compute regional-level demographics
gen lowed = hatlev >= 0 & hatlev <= 200
gen mided = hatlev >= 300 & hatlev <= 499
gen highed = hatlev >= 500 & hatlev < .
gen city = degurba == 1
gen rural = degurba == 3
gen female = sex == 2
gen pt = ftpt == 2
gen self = stapro == 1
gen temporary = temp == 2
gen nonnat = countryb != "NAT"
recode age (1 = 17) (2 = 22) (3 = 27) (4 = 32) (5 = 37) (6 = 42) (7 = 47) (8 = 52) (9 = 57) (10 = 62) (11 = 67) (12 = 72) (13 = 77), gen(agecont)


* Generate "isco0d" broad occupational classifications, based on code by John (adapted to start from 3-digits)
recode isco08_3d (100/299=1)(300/599=2)(600/799=3)(800/969=4), gen(isco0d)
label define isco0d 1 "White-collar high-skilled"
label define isco0d 2 "White-collar low/mid-skilled", add
label define isco0d 3 "Blue-collar high-skilled", add
label define isco0d 4 "Blue-collar low/mid-skilled", add
label values isco0d isco0d
label variable isco0d "Occupation classes"

* expand the occupation classes in individual variables
tab isco0d, gen(occup_classes)

* drop occupation classes corresponding to NAs (makes globbing later easier)
*drop occup_classes5 occup_classes6


* Define regional-level sectoral variables
gen agric = nace1d == 1
gen manuf = nace1d == 3
gen privservadv = nace > 9 & nace < 15
gen publserv = nace > 14 & nace < 18

* Generate dummies for different work locations (for collapse later)
tab work_location, gen(work_l)
rename work_l2 work_other_region
rename work_l3 work_other_country

* Capital region/country
gen capital = urbrur == 1

* Generate progressive observation number (used for collapse?)
gen n = 1 

* Collapse at the regional, year (and country?) level
collapse (mean) homework_index (mean) homework_any (mean) physical (mean) social /// Dependent vars and teleworkability
	(mean) city (mean) rural (mean) capital (mean) broadband_shh (mean) work_other_region (mean) work_other_country /// Geographic characteristics
	(mean) female (mean) lowed (mean) highed (mean) pt (mean) self (mean) temporary (mean) nonnat (mean) agecont (mean) occup_classes* /// Demographics
	(mean) agric (mean) manuf (mean) privserv (mean) publserv /// Sector
	(rawsum) coeffy (rawsum) n [aw=coeffy], by(reglab year)

* Define variable groups
global twy    c.physicalinteraction
global geo    c.city c.rural i.capital c.broadband_shh c.work_other_region c.work_other_country
global demo   c.female c.lowed c.highed c.pt c.self c.temporary c.nonnat c.agecont
*global sector c.agric c.manuf c.privserv c.publserv


* Estimate pooled regional regressions

eststo clear
* R0: just year, no teleworkability indices
qui reg homework_any i.year [pw=coeffy]
eststo R0

* R1: teleworkability indices
qui reg homework_any ($twy)##i.year [pw=coeffy]
eststo R1

* R2: (R1) + geographic characteristics
qui reg homework_any ($twy)##i.year $geo $demo [pw=coeffy] 
eststo R2


* Show and export regression tables
#delim ;
esttab R*, nonotes not r2 lab compress obslast;

esttab R* using "..\Tables\reg_regional_pooled.tsv", nonotes nomtitles se not r2 lab compress obslast tab replace;

esttab R* using "..\Tables\reg_regional_pooled.rtf", not r2 lab compress obslast 
		title("Share of people teleworking (by NUTS region)") nomtitles replace;
#delim cr