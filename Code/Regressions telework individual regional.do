* Analysis of teleworkability and telework by region in Europe
* JRC-Eurofound collaborative project
* Februray 2024
* Enrique Fernández-Macías and Matteo Sostero

set more off
clear

* make sure latest versions of estout is installed
ssc install estout, replace

* Local directory for Enrique
cd "C:\Users\a\Documents\teleworkability with eurofound\Regional 2023 EF_JRC"

* Local directory for Matteo
cd "C:\Users\mso\repositories\telework-regional\Data"

/*
Load LFS data, processed by Matteo,
based on John's LFS extraction (LFSreg2018_22rec.dta)
cleaned by Matteo (based on Enrique's initial analysis). 

Compared to original LFS, it adds:
- urbrur regional typology
- teleworkability variables: physicalinteraction (we call it "technical teleworkability") and socialinteraction
- internet_speed from Ookla-JRC data, at (degurba * NUTS) granularity

For details see R scripts "1- Import LFS data.R" and "9- Combine data.R"
*/

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
* The syntax for 'table' changed with Stata version 17 used at EF; Declare older version to preserve syntax
version 16: table reg, c(mean internet_speed)

* Teleworkability indices defined for all ISCO 3-digit occupations (includes recoding of some occupations by Enrique)
version 16: table isco08_3d [aw=coeffy], c(mean physical mean social)

* Generate progressive observation number (used for collapse?)
gen n = 1 

********************************************************************************

* Teleworkability vs actual telework: did the pandemic change the link between them?

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

graph bar (mean) isco3d2018 isco3d2019 isco3d2020 isco3d2021 isco3d2022, over(country) bar(1, color(gs13)) bar(2, color(gs10)) bar(3, color(gs6)) bar(4, color(gs2)) legend(off) ysize(2)
graph export "anova_homew_isco3d.png", wid(1024) hei(256) replace

* The explanatory value of isco3d with respect to actual telework varied between
* .1 and .2 before COVID. In most cases, it increased very substantially, to between
* .2 and .4. This on its own makes it more likely that our teleworkability index
* becomes a better predictor of actual telework after COVID.


**# Compute regional-level aggregates ---------------------------------------------------------------------

use "LFS_regression.dta", clear

* Define variables for regional-level demographics

* Education levels
gen lowed = hatlev >= 0 & hatlev <= 200
gen mided = hatlev >= 300 & hatlev <= 499
gen highed = hatlev >= 500 & hatlev < . 

* Dummies for cities, rural, gender, type of work
gen cities = degurba == 1
gen rural = degurba == 3
gen capital = urbrur == 1
gen female = sex == 2
gen pt = ftpt == 2
gen self = stapro == 1
gen temporary = temp == 2
* gen nonnat = countryb != "NAT" // Nonnat is now excluded because missing from 2022 LFS microdata

* Generate dummies for different work locations (for collapse later)
tab work_location, gen(work_l)
rename work_l2 work_other_region
rename work_l3 work_other_country

* generate pseudo-continuous age variable
recode age (1 = 17) (2 = 22) (3 = 27) (4 = 32) (5 = 37) (6 = 42) (7 = 47) (8 = 52) (9 = 57) (10 = 62) (11 = 67) (12 = 72) (13 = 77), gen(agecont)

* Set Germany as the baseline country
labelbook country
fvset base 6 country


* Generate dummy variables for NACE 1-digit sectors
* Maybe change sector names from their labels?
* See https://stackoverflow.com/questions/44343749/tabulate-categorical-variable-into-dummies-with-proper-labels
tabulate nace1d, generate(sector)
* Drop variables for "no response" and "not applicable"
drop sector22 sector23

* Define more aggregete sector groups
gen agric = nace1d == 1
gen manuf = nace1d == 3
gen privserv = nace1d > 9 & nace1d < 15
gen publserv = nace1d > 14 & nace1d < 18


* Collapse at the regional, year and country level
gen n = 1 
collapse (mean) homework_index (mean) homework_any (mean) physical (mean) social /// Dependent vars and teleworkability
	(mean) cities (mean) rural (mean) capital (mean) internet_speed (mean) work_other_region (mean) work_other_country /// Geographic characteristics
	(mean) female (mean) lowed (mean) highed (mean) pt (mean) self (mean) temporary (mean)  agecont /// Demographics, excluding nonnat
	(mean) sector* (mean) agric (mean) manuf (mean) privserv (mean) publserv /// Sector (both as NACE 1-digit and as groups)
	(rawsum) coeffy (rawsum) n [aw=coeffy], by(reglab year country)
	
save "LFS_regression_regional.dta", replace


**# Explore detailed regional-level models ---------------------------------------------------------------------
use "LFS_regression_regional.dta"

* Define variable groups
global twy    c.physicalinteraction // c.socialinteraction
global geo    cities i.capital internet_speed work_other_region work_other_country
global demo   agecont female highed pt self temporary  // nonnat
global sect_g agric manuf privserv publserv


* Estimate pooled regional regressions, for exploration

eststo clear
* R0: just year, no teleworkability indices
qui reg homework_any i.year i.country [pw=coeffy]
eststo R0
estadd local fe_country "Yes"

* R1: teleworkability indices
qui reg homework_any ($twy)##i.year i.country [pw=coeffy]
eststo R1
estadd local fe_country "Yes"

* R2: (R1) + geographic characteristics
qui reg homework_any ($twy)##i.year $geo i.country [pw=coeffy] 
eststo R2
estadd local fe_country "Yes"

* R3: (R2) + work characteristics
qui reg homework_any ($twy)##i.year $geo $demo i.country [pw=coeffy] 
eststo R3
estadd local fe_country "Yes"

* R4: (R3) + sector groups
qui reg homework_any ($twy)##i.year $geo $demo $sect_g i.country [pw=coeffy]
eststo R4
estadd local fe_country "Yes"

* R4b: (R3) + sector at NACE 1-digit level
qui reg homework_any ($twy)##i.year $geo $demo sector* i.country [pw=coeffy]
eststo R4b
estadd local fe_country "Yes"

* Tabulate regression models
#delim ;
esttab R*, nonotes not r2 varwidth(25) label mtitles nonumber compress ///
	nobaselevels obslast order(_cons) drop(*country*)
	stats(fe_country N r2, labels("Country FE" "N" "Adjusted R-squared")) title("Regional share of workers teleworking")

esttab R* using "..\Tables\reg_extended.rtf", nonotes not r2 label mtitles nonumber compress///
	nobaselevels obslast order(_cons) drop(*country*)///
	stats(fe_country N r2, labels("Country FE" "N" "Adjusted R-squared")) title("Regional share of workers teleworking") 
delim cr
/*
Comment:

The regressions specifications always include country controls, which seems a good thing: 
the coefficients for cities is no longer negative (which made little sense), but non-significant.
The specifications without country controls were spuriously capturing country differences through territorial demographics.

The country controls, which by construction are time-invariant, and will be shown and commented separately.

Controlling for NACE-1d (model R4b) nixes the teleworkability coefficient.
In Matteo's view, this is because such a model is over-specified, namely:
1) it may identify some NUTS regions uniquely because of the uneven geographical distributions of some sectors
2) sector correlates highly with occupation, therefore capturing the variance from occupation-level teleworkability

Controlling for more aggregate sectors (model R4) doesn't have the same problem,
but the coefficients (+0.19*** for manufacturing; -0.125* for private services)
are the opposite of what one would expect, and are difficult to justify.
It may result from some spurious residual country-level differences.

Should we control for sector at all?  The reviewers asked for it.

In what follows, we don't, just like in the original draft.

*/


**# Estimate regional-level models for paper ---------------------------------------------------------------------

use "LFS_regression_regional.dta", clear

* Define variable groups
global twy    c.physicalinteraction // c.socialinteraction
global geo    cities i.capital internet_speed work_other_region work_other_country
global demo   agecont female highed pt self temporary  // nonnat

* Estimate pooled regional regressions

eststo clear
* R0: just year, no teleworkability indices
qui reg homework_any i.year i.country [pw=coeffy]
eststo R0
estadd local fe_country "Yes"

* R1: teleworkability indices
qui reg homework_any ($twy)##i.year i.country [pw=coeffy]
eststo R1
estadd local fe_country "Yes"

* R2: (R1) + geographic characteristics
qui reg homework_any ($twy)##i.year $geo $demo i.country [pw=coeffy] 
eststo R2
estadd local fe_country "Yes"


* Show and export regression tables
#delim ;
esttab R*, ///
	nonotes not nomtitles r2 varwidth(30) lab compress nobaselevels obslast order(_cons) drop(*country) ///
	stats(fe_country N r2, labels("Country FE" "N" "Adjusted R-squared")) ///
	title("Proportion of workers teleworking in each NUTS region");

esttab R* using "..\Tables\reg_regional_pooled.tsv", ///
	se not nomtitles r2 lab compress nobaselevels obslast order(_cons) drop(*.country) //
	tab replace;

esttab R* using "..\Tables\reg_regional_pooled_country.tsv", ///
	keep(*.country) wide nonotes se not nostar noobs nomtitle noparentheses ///
	tab replace;

esttab R* using "..\Tables\reg_regional_pooled.rtf", ///
	not r2 lab compress nobaselevels obslast order(_cons) drop(*.country) ///
	title("Proportion of workers teleworking in each NUTS region") nomtitles ///
	replace;
#delim cr


**# Explaining telework at the individual level ***********************************
***********************************************************************************
* We're no longer doing this ******************************************************
***********************************************************************************

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
// gen nonnat = countryb != "NAT"
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
global twy        c.physicalinteraction 
// c.socialinteraction 
global individual yw pw ow ym om lowed highed 
// nonnat i.isco0d
global work       i.pt i.stapro temporary i.work_location
global regional   i.degurba capital internet_speed i.reglab



**# Estimate individual-level models, year by year, incrementally ***************************

eststo clear

forval y = 2018(1)2022 {
	* B1: teleworkability indices
	qui reg homework_any $twy i.reglab [pw=coeffy] if year == `y'
	eststo B1_`y', title("`y'")
 	estadd local fe_region Yes
	
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

esttab B1_*, nonotes not r2 lab compress obslast drop(*reglab*) mtitle(title)
esttab B4_*, nonotes not r2 lab compress obslast drop(*reglab*) mtitle(title)


* Tables for simple and full individual model
* Run as a block: it uses ; as line delimiters
#delim ;
esttab B1_* using "..\Tables\reg_individual_year_simple.tsv", replace tab ///
	nonotes not lab compress obslast mtitle(title) ///
	stats(fe_region N r2, labels("Region FE" "N" "Adjusted R-squared")) ///
	title("Probability of person working from home, at least some of the time");
	
esttab B4_* using "..\Tables\reg_individual_year_full.tsv", replace tab ///
	nonotes not lab compress obslast mtitle(title) ///
	stats(fe_region N r2, labels("Region FE" "N" "Adjusted R-squared")) ///
	title("Probability of person working from home, at least some of the time");
	
esttab B1_* using "..\Tables\reg_individual_year_simple.rtf", replace ///
	not r2 lab compress obslast drop(*reglab*) mtitle(title) ///
	stats(fe_region N r2, labels("Region FE" "N" "Adjusted R-squared")) ///
	title("Probability of person working from home, at least some of the time");
	
esttab B4_* using "..\Tables\reg_individual_year_full.rtf", replace ///
	not r2 lab compress obslast drop(*reglab*) mtitle(title) ///
	stats(fe_region N r2, labels("Region FE" "N" "Adjusted R-squared")) ///
	title("Probability of person working from home, at least some of the time");
#delim cr



* Block 1bis: how does this compare with ISCO at 3 digits?

eststo clear
forval i = 2018(1)2022 {
	quietly: reg homework_any i.isco08_3d [pw=coeffy] if year == `i'
	eststo Occup`i', title("`i'")
}
quietly: reg homework_any i.isco08_3d [pw=coeffy]
eststo All

esttab Occup*, nonotes nocons not r2 lab compress obslast mtitle(title) 
// it used to be able to drop(*), IDK what changed
esttab using "..\Tables\reg_occupation_year.tsv", mtitle nonotes not r2 lab compress obslast tab replace
esttab using "..\Tables\reg_occupation_year.rtf", mtitle         not r2 lab compress obslast replace
eststo clear