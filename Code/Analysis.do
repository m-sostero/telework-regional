* First analysis of teleworkability and telework by region in Europe
* JRC-Eurofound collaborative project
* April 2023

set more off
clear

* Local directory for Enrique
cd "C:\Users\a\Documents\teleworkability with eurofound\Regional 2023 EF_JRC"

* Local directory for Matteo
cd "C:\Users\sostema\repositories\Telework regional\Data"

* Load LFS data, produced by John
use "LFSreg2018_21finalr.dta" 

keep if ilostat == 1 // Keep only employed (excluding unemployed and outside labour force)
drop ilostat

keep if stapro == 0 | stapro == 3 // Keep only employees and self-employed (Drop family workers and "non applicable")

lab define stapro 3 "Employee", modify // Edit label (previously "£mployed")

drop if _merge == 2 // Discard (empty) observations created by previous regional merge (empty)
drop _merge

gen n = 1 // Generate progressive observation number (used for collapse?)


* Initial workplan:
* 1. We will link our teleworkability index at the 3-digit isco level
* 2. We will compare teleworkability vs. actual telework by region*degurba
* 3. We will explore changes region of work vs. region of residence during covid

* Linking teleworkability
* In principle, this should be straightforward having ISCO at 3 digit

bysort country: tab isco08_3d year [aw=coeffy], col nof mis

* All countries seem ok except BG (only available at 2 dig), MT (1 dig), SI (2 dig)
* So we can drop those countries:

drop if country == 3 | country == 20 | country == 26


* Inspect occupation codes again
tab isco08_3d

* Discard occupation codes with < 3 digits:
* those are armed forces occupations (eg, 010, 020),
* the leading 0 was truncated when parsing the ISCO codes as numbers (!)
* We can drop them, because we don't have teleworkability values for them
drop if length(string(isco08_3d)) < 3

* Now, let´s merge the teleworkability data

sort isco08_3d
merge m:1 isco08_3d using "Teleworkability indices.dta"

* Inspect results of merge: which ISCO codes in the LFS have corresponding teleworkability values?
tab isco08_3d _merge, mis

* Values of teleworkability for each ISCO code
version 16: table isco08_3d [aw=coeffy], c(mean physical mean social)
* 'version 16:' is used for backward compatibility, because the table command changed in Stata 17 (on M's laptop)

* There are some categories of isco08_3d in LFS which cannot be matched to our 
* teleworkability data, in most cases because they are higher level codes (2 or 
* even 1 digit ISCO codes: in theory they should not be there, but they are). This
* can be easily fixed by assigning those categories a kind of weighted average value
* of the relevant 3 digit isco codes. Then, there are some 3 digit isco codes which
* are missing in the teleworkability indices, which is a bit trickier but we can 
* fix assigning them values from similar or adjacent occupations. Then, there are
* some residual cases with 999 in isco08_3d (all from DK in 2021, ??), and some
* missing in isco08_3d (mostly from FI, NL and DK). I guess we just have to drop
* those cases? Maybe later, no rush (they are 999 and . in isco08_3d)

drop occupationtitle _merge

preserve
collapse (mean) phys_temp=physical (mean) soc_temp=social [aw=coeffy], by(isco2d)
save "indices_2d.dta", replace
restore, preserve
collapse (mean) phys_temp=physical (mean) soc_temp=social [aw=coeffy], by(isco1d)
save "indices_1d.dta", replace
restore

merge m:1 isco2d using "indices_2d.dta"

replace physical = phys_temp if mod(isco08_3d,10) == 0
replace social = soc_temp if mod(isco08_3d,10) == 0

drop _merge *_temp

merge m:1 isco1d using "indices_1d.dta"

replace physical = phys_temp if mod(isco08_3d,100) == 0
replace social = soc_temp if mod(isco08_3d,100) == 0

drop _merge *_temp

version 16: table isco08_3d [aw=coeffy], c(mean physical mean social)

/*

Ok, so this works well. These are the remaining codes:

223 Traditional and complementary medicine professionals
224 Paramedical practitioners
323 Traditional and complementary medicine associate professionals
630 Subsistence Farmers, Fishers, Hunters and Gatherers
631 Subsistence crop farmers
632 Subsistence livestock farmers
633 Subsistence mixed crop and livestock farmers
634 Subsistence fishers, hunters, trappers and gatherers

The remaining 3 digit codes are weird occupations for which we do not have values
I can give them arbitrary (but reasonable) values: 223 and 224 will get the values
of the 2-digit isco code 220 (335.5 and 813.368), 323 those of 320 (122.823 and 736.206)
630-634 will get the same values as farmers (600): 0 and 390.461
As for the values 11 to 31, I will recode them as two digits and assign them the
correct values (I will implement it above, so you will not see it here)
Finally, the zero is just a wrong code that will go to missing.

*/

replace physical = 335.5 if isco08_3d == 223 | isco08_3d == 224
replace social = 813.368 if isco08_3d == 223 | isco08_3d == 224

replace physical = 122.823 if isco08_3d == 323
replace social = 736.206 if isco08_3d == 323

replace physical = 0 if isco08_3d == 630 | isco08_3d == 631 | isco08_3d == 632 | isco08_3d == 633 | isco08_3d == 634
replace social = 390.461 if isco08_3d == 630 | isco08_3d == 631 | isco08_3d == 632 | isco08_3d == 633 | isco08_3d == 634

version 16: table isco08_3d [aw=coeffy], c(mean physical mean social)

replace physical = physical/1000
replace social = social/1000

recode homework (3 = 0) (2 = .25) (1 = .75), gen(homework_index)
recode homework (3 = 0) (1/2 = 1), gen(homework_any)



* Export data

compress

save temp.dta, replace

* Ok, so we are all set!

********************************************************************************

* Teleworkability vs actual telework: did the pandemic change the link between these?

* Since teleworkability is defined at the three digit ISCO level, this is the most 
* sensible level at which we should do the analysis of the link

use temp.dta, clear

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

* An econometric analysis of the factors explaining telework (including our index)
* before and after covid, at the individual level

use temp.dta, clear

recode age (1/3 = 1 Young) (4/7 = 2 Prime) (8/13 = 3 Old), gen(agecat)
gen yw = agecat == 1 & sex == 2
gen pw = agecat == 2 & sex == 2
gen ow = agecat == 3 & sex == 2
gen ym = agecat == 1 & sex == 1
gen pm = agecat == 2 & sex == 1
gen om = agecat == 3 & sex == 1
gen lowed = hatlev >= 0 & hatlev <= 200
gen mided = hatlev >= 300 & hatlev <= 499
gen highed = hatlev >= 500 & hatlev < .
gen pt = ftpt == 2
gen self = stapro == 0
gen temporary = temp == 2
gen nonnat = countryb != "NAT"

* Block 1: just the two core variables

forval i = 2018(1)2021 {
	reg homework_any physicalinteraction socialinteraction [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_any physicalinteraction socialinteraction [pw=coeffy]
eststo All

esttab using ind_models_any.tsv, nonotes not r2 lab nomti compress obslast tab replace
eststo clear

* Block 1bis: how does this compare with ISCO at 3 digits?

forval i = 2018(1)2021 {
	quietly reg homework_any i.isco08_3d [pw=coeffy] if year == `i'
	eststo Year`i'
}
quietly reg homework_any i.isco08_3d [pw=coeffy]
eststo All

esttab using ind_models_any.tsv, nonotes drop(*) nocons not r2 lab nomti compress obslast tab append
eststo clear

* Block 2: adding individual factors, sex and age, educ and country of birth

forval i = 2018(1)2021 {
	reg homework_any physicalinteraction socialinteraction yw pw ow ym om lowed highed nonnat [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_any physicalinteraction socialinteraction yw pw ow ym om lowed highed nonnat [pw=coeffy]
eststo All

esttab using ind_models_any.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 3: adding work-related factors, part-time and temporary contract, self-empl

forval i = 2018(1)2021 {
	reg homework_any physicalinteraction socialinteraction yw pw ow ym om lowed highed nonnat pt self temporary [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_any physicalinteraction socialinteraction yw pw ow ym om lowed highed nonnat pt self temporary [pw=coeffy]
eststo All

esttab using ind_models_any.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 4: adding degree of urbanisation

forval i = 2018(1)2021 {
	reg homework_any physicalinteraction socialinteraction yw pw ow ym om lowed highed nonnat pt self temporary i.degurba [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_any physicalinteraction socialinteraction yw pw ow ym om lowed highed nonnat pt self temporary i.degurba [pw=coeffy]
eststo All

esttab using ind_models_any.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 5: adding country

forval i = 2018(1)2021 {
	reg homework_any physicalinteraction socialinteraction yw pw ow ym om lowed highed nonnat pt self temporary i.degurba i.country [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_any physicalinteraction socialinteraction yw pw ow ym om lowed highed nonnat pt self temporary i.degurba i.country [pw=coeffy]
eststo All

esttab using ind_models_any.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Same analysis, with the continuous index as dependent variable

* Block 1: just the two core variables

forval i = 2018(1)2021 {
	reg homework_index physicalinteraction socialinteraction [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_index physicalinteraction socialinteraction [pw=coeffy]
eststo All

esttab using ind_models_index.tsv, nonotes not r2 lab nomti compress obslast tab replace
eststo clear

* Block 1bis: how does this compare with ISCO at 3 digits?

forval i = 2018(1)2021 {
	quietly reg homework_index i.isco08_3d [pw=coeffy] if year == `i'
	eststo Year`i'
}
quietly reg homework_index i.isco08_3d [pw=coeffy]
eststo All

esttab using ind_models_index.tsv, nonotes drop(*) nocons not r2 lab nomti compress obslast tab append
eststo clear

* Block 2: adding individual factors, sex and age, educ and country of birth

forval i = 2018(1)2021 {
	reg homework_index physicalinteraction socialinteraction yw pw ow ym om lowed highed nonnat [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_index physicalinteraction socialinteraction yw pw ow ym om lowed highed nonnat [pw=coeffy]
eststo All

esttab using ind_models_index.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 3: adding work-related factors, part-time and temporary contract, self-empl

forval i = 2018(1)2021 {
	reg homework_index physicalinteraction socialinteraction yw pw ow ym om lowed highed nonnat pt self temporary [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_index physicalinteraction socialinteraction yw pw ow ym om lowed highed nonnat pt self temporary [pw=coeffy]
eststo All

esttab using ind_models_index.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 4: adding degree of urbanisation

forval i = 2018(1)2021 {
	reg homework_index physicalinteraction socialinteraction yw pw ow ym om lowed highed nonnat pt self temporary i.degurba [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_index physicalinteraction socialinteraction yw pw ow ym om lowed highed nonnat pt self temporary i.degurba [pw=coeffy]
eststo All

esttab using ind_models_index.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 5: adding country

forval i = 2018(1)2021 {
	reg homework_index physicalinteraction socialinteraction yw pw ow ym om lowed highed nonnat pt self temporary i.degurba i.country [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_index physicalinteraction socialinteraction yw pw ow ym om lowed highed nonnat pt self temporary i.degurba i.country [pw=coeffy]
eststo All

esttab using ind_models_index.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Now, a job-level econometric analysis, using a similar approximation

use temp.dta, clear

gen lowed = hatlev >= 0 & hatlev <= 200
gen mided = hatlev >= 300 & hatlev <= 499
gen highed = hatlev >= 500 & hatlev < .
gen city = degurba == 1
gen female = sex == 2
gen pt = ftpt == 2
gen self = stapro == 0
gen temporary = temp == 2
gen nonnat = countryb != "NAT"

collapse (mean) physical (mean) social (mean) homework_index (mean) homework_any (mean) city (mean) female (mean) lowed (mean) highed (mean) pt (mean) self (mean) temporary (mean) nonnat (rawsum) coeffy (rawsum) n [aw=coeffy], by(isco08_3d country year)

save occup.dta, replace

* Block 1: just the two core variables

forval i = 2018(1)2021 {
	reg homework_any physical social [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_any physical social [pw=coeffy]
eststo All

esttab using job_models_any.tsv, nonotes not r2 lab nomti compress obslast tab replace
eststo clear

* Block 1bis: how does this compare with ISCO at 3 digits?

forval i = 2018(1)2021 {
	quietly reg homework_any i.isco08_3d [pw=coeffy] if year == `i'
	eststo Year`i'
}
quietly reg homework_any i.isco08_3d [pw=coeffy]
eststo All

esttab using job_models_any.tsv, nonotes drop(*) nocons not r2 lab nomti compress obslast tab append
eststo clear

* Block 2: adding individual factors, sex, educ and country of birth

forval i = 2018(1)2021 {
	reg homework_any physical social female lowed highed nonnat [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_any physical social female lowed highed nonnat [pw=coeffy]
eststo All

esttab using job_models_any.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 3: adding work-related factors, part-time and temporary contract, self-empl

forval i = 2018(1)2021 {
	reg homework_any physical social female lowed highed nonnat pt self temporary [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_any physical social female lowed highed nonnat pt self temporary [pw=coeffy]
eststo All

esttab using job_models_any.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 4: adding city

forval i = 2018(1)2021 {
	reg homework_any physical social female lowed highed nonnat pt self temporary city [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_any physical social female lowed highed nonnat pt self temporary city [pw=coeffy]
eststo All

esttab using job_models_any.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 5: adding country

forval i = 2018(1)2021 {
	reg homework_an physical social female lowed highed nonnat pt self temporary city i.country [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_any physical social female lowed highed nonnat pt self temporary city i.country [pw=coeffy]
eststo All

esttab using job_models_any.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Same analysis, with the continuous index as dependent variable

* Block 1: just the two core variables

forval i = 2018(1)2021 {
	reg homework_index physical social [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_index physical social [pw=coeffy]
eststo All

esttab using job_models_index.tsv, nonotes not r2 lab nomti compress obslast tab replace
eststo clear

* Block 1bis: how does this compare with ISCO at 3 digits?

forval i = 2018(1)2021 {
	quietly reg homework_index i.isco08_3d [pw=coeffy] if year == `i'
	eststo Year`i'
}
quietly reg homework_index i.isco08_3d [pw=coeffy]
eststo All

esttab using job_models_index.tsv, nonotes drop(*) nocons not r2 lab nomti compress obslast tab append
eststo clear

* Block 2: adding individual factors, sex, educ and country of birth

forval i = 2018(1)2021 {
	reg homework_index physical social female lowed highed nonnat [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_index physical social female lowed highed nonnat [pw=coeffy]
eststo All

esttab using job_models_index.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 3: adding work-related factors, part-time and temporary contract, self-empl

forval i = 2018(1)2021 {
	reg homework_index physical social female lowed highed nonnat pt self temporary [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_index physical social female lowed highed nonnat pt self temporary [pw=coeffy]
eststo All

esttab using job_models_index.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 4: adding city

forval i = 2018(1)2021 {
	reg homework_index physical social female lowed highed nonnat pt self temporary city [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_index physical social female lowed highed nonnat pt self temporary city [pw=coeffy]
eststo All

esttab using job_models_index.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 5: adding country

forval i = 2018(1)2021 {
	reg homework_an physical social female lowed highed nonnat pt self temporary city i.country [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_index physical social female lowed highed nonnat pt self temporary city i.country [pw=coeffy]
eststo All

esttab using job_models_index.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Now, some charts

egen country_year_id = group(country year)
gen corr = .
levelsof country_year_id, local(ids)
foreach id in `ids' {
    quietly pwcorr homework_index physical if country_year_id == `id' [aw=coeffy], sig
    local rho = round(r(rho),0.01)
    replace corr = `rho' if country_year_id == `id'
}
collapse (mean) corr, by(country year)
reshape wide corr, i(country) j(year)

outsheet using occup_corr_phys_telew.csv, delimiter(";") replace

use occup.dta, clear

tw (scat homework_index physical [aw=coeffy], msymbol(oh)) (lfit homework_index physical) if country < 8, by(country year, cols(4) note("") legend(off)) xtitle("Teleworkability - physical") ytitle("Actual telework")
graph export "occup_corr_1.png", wid(1024) hei(768) replace
tw (scat homework_index physical [aw=coeffy], msymbol(oh)) (lfit homework_index physical) if country > 7 & country < 14, by(country year, cols(4) note("") legend(off)) xtitle("Teleworkability - physical") ytitle("Actual telework")
graph export "occup_corr_2.png", wid(1024) hei(768) replace
tw (scat homework_index physical [aw=coeffy], msymbol(oh)) (lfit homework_index physical) if country > 13 & country < 20, by(country year, cols(4) note("") legend(off)) xtitle("Teleworkability - physical") ytitle("Actual telework")
graph export "occup_corr_3.png", wid(1024) hei(768) replace
tw (scat homework_index physical [aw=coeffy], msymbol(oh)) (lfit homework_index physical) if country > 20 & country < 30, by(country year, cols(4) note("") legend(off)) xtitle("Teleworkability - physical") ytitle("Actual telework")
graph export "occup_corr_4.png", wid(1024) hei(768) replace
tw (scat homework_index physical [aw=coeffy], msymbol(oh)) (lfit homework_index physical) if country == 6 | country == 10 | country == 12 | country == 15 | country == 22 | country == 25, by(country year, cols(4) note("") legend(off)) xtitle("Teleworkability - physical") ytitle("Actual telework")
graph export "occup_corr_big6.png", wid(1024) hei(768) replace

* Clearly, the correlation between physical teleworkability and actual telework
* increased quite significantly, going from .5 to .7 in many cases. So telework
* did not just increase across the board, but it increased more for teleworkable
* occupations

* Another way to show the same:

egen temp=cut(physical), at(0,.1,.9,1.1)
recode temp (0 = 1 "Not teleworkable") (.1 = 2 "Partly teleworkable") (.9 = 3 "Fully teleworkable"), gen(tw_intervals)
drop temp

table tw_intervals year [aw=coeffy], c(mean homework_index) by(country)
table tw_intervals year [aw=coeffy], c(mean homework_index)

* We can check whether the interaction between physical and social matters

egen temp=cut(social), at(0,.4,.6,1.1)
recode temp (0 = 1 "Low social") (.4 = 2 "Mid social") (.6 = 3 "High social"), gen(soc_intervals)
drop temp

table soc_intervals year [aw=coeffy], c(mean homework_index) by(country)
table soc_intervals year [aw=coeffy], c(mean homework_index)

tab tw soc_in [aw=coeffy], cell nof
table tw soc_in [aw=coeffy], by(country year) c(mean homework_index)
table tw soc_in [aw=coeffy], by(year) c(mean homework_index)

* Also very interesting! It seems that, as we predicted, the biggest increases in
* telework took place in teleworkable occupations with low social interaction.
* However, some of these cells have low values and we have to analyse this better.

********************************************************************************

* Now, some preliminary geographic analysis: teleworkability vs. actual telework by country and year

use temp.dta, clear

collapse (mean) physical (mean) social (mean) homework_index (rawsum) coeffy (rawsum) n [aw=coeffy], by(country year)

save country.dta, replace

drop if country == 18

foreach var in physical social {
	forval y=2018(1)2021 {
		pwcorr `var' homework [aw=coeffy] if year == `y'
		local rho = string(round(r(rho),0.01))
		lab def title `y' "`y'  R2:`rho'", add
	}
	lab val year title
	tw (scat homework `var', mlab(country) mlabpos(0) msymbol(i)) (lfit homework `var'), by(year, note("") legend(off)) xtitle("Teleworkability - `var'") ytitle("Actual telework")
	graph export "country_`var'.png", wid(1024) hei(768) replace
	lab drop title
}

* At this very superficial and aggregate level, it seems that we find the expected
* patterns: teleworkability is a reasonably good predictor of homework at the country
* level, and it became a much better predictor with COVID.

********************************************************************************

* Now, let´s look at telework by region

* 1. How much does actual telework vary by region? Does degurba matter? Did this
* change with COVID?

use temp.dta, clear

levelsof country, local(c_local)
levelsof year, local(y_local)
gen tmp = reg + "-" + string(degurba)
encode tmp, gen(regurba)
drop tmp
set matsize 800

gen coeff_reglab = .
gen coeff_degurba = .
gen coeff_regurba = .
foreach y in `y_local' {
	foreach c in reglab degurba regurba {
		reg homework_i i.`c' if year == `y' [pw=coeffy], notable
		replace coeff_`c' = e(r2_a) if year == `y' 
	}
}
collapse (mean) coeff_*, by(year)
ren coeff_* *
xpose, clear varname
ren v1 y2018
ren v2 y2019
ren v3 y2020
ren v4 y2021
drop in 1

graph bar (mean) y*, over(_varname) bar(1, color(gs13)) bar(2, color(gs10)) bar(3, color(gs6)) bar(4, color(gs2)) legend(off) ysize(2)
graph export "anova_nuts_degurba.png", wid(1024) hei(256) replace

* The actual incidence of telework does not vary much by region, nor by degurba,
* nor by the two combined

* Let's do some basic descriptives

use temp.dta, clear

table reglab year [aw=coeffy], c(mean homework_i)
gen agric = nace == 1
table reglab [aw=coeffy] if year == 2021, c(mean agric)

use temp.dta, clear

collapse (mean) physical (mean) social (mean) homework_index (rawsum) coeffy (rawsum) n [aw=coeffy], by(reg year)

save regions.dta, replace

foreach var in physical social {
	forval y=2018(1)2021 {
		pwcorr `var' homework [aw=coeffy] if year == `y'
		local rho = string(round(r(rho),0.01))
		lab def title `y' "`y'  R2:`rho'", add
	}
	lab val year title
	tw (scat homework `var' [aw=coeffy], msymbol(oh)) (lfit homework `var'), by(year, note("") legend(off)) xtitle("Teleworkability - `var'") ytitle("Actual telework")
	graph export "region_`var'.png", wid(1024) hei(768) replace
	lab drop title
}

* Interesting to note that the R2 does not increase when we use region rather than
* country. Using region we add noise, but we also add more granularity which could
* be expected to allow a better prediction based on teleworkability/occupational
* structure. We have to explore this better with a detailed econometric analysis.
* In any case, the associations are strong enough as to justify further analysis,
* for sure.

* Let´s do a regression analysis similar to the earlier one for occupation:

use temp.dta, clear

gen lowed = hatlev >= 0 & hatlev <= 200
gen mided = hatlev >= 300 & hatlev <= 499
gen highed = hatlev >= 500 & hatlev < .
gen city = degurba == 1
gen rural = degurba == 3
gen female = sex == 2
gen pt = ftpt == 2
gen self = stapro == 0
gen temporary = temp == 2
gen nonnat = countryb != "NAT"
recode age (1 = 17) (2 = 22) (3 = 27) (4 = 32) (5 = 37) (6 = 42) (7 = 47) (8 = 52) (9 = 57) (10 = 62) (11 = 67) (12 = 72) (13 = 77), gen(agecont)
gen agric = nace1d == 1
gen manuf = nace1d == 3
gen privservadv = nace > 9 & nace < 15
gen publserv = nace > 14 & nace < 18

collapse (mean) physical (mean) social (mean) homework_index (mean) homework_any (mean) city (mean) rural (mean) female (mean) lowed (mean) highed (mean) pt (mean) self (mean) temporary (mean) nonnat (mean) agecont (mean) agric (mean) manuf (mean) privserv (mean) publserv (rawsum) coeffy (rawsum) n [aw=coeffy], by(reglab country year)

* Block 1: just the two core variables

forval i = 2018(1)2021 {
	reg homework_any physical social [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_any physical social [pw=coeffy]
eststo All

esttab using reg_models_any.tsv, nonotes not r2 lab nomti compress obslast tab replace
eststo clear

* Block 2: adding individual factors, sex, age, educ and country of birth

forval i = 2018(1)2021 {
	reg homework_any physical social female agecont lowed highed nonnat [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_any physical social female agecont lowed highed nonnat [pw=coeffy]
eststo All

esttab using reg_models_any.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 3: adding work-related factors, part-time and temporary contract, self-empl

forval i = 2018(1)2021 {
	reg homework_any physical social female agecont lowed highed nonnat pt self temporary [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_any physical social female agecont lowed highed nonnat pt self temporary [pw=coeffy]
eststo All

esttab using reg_models_any.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 4: adding city and rural

forval i = 2018(1)2021 {
	reg homework_any physical social female agecont lowed highed nonnat pt self temporary city rural [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_any physical social female agecont lowed highed nonnat pt self temporary city rural [pw=coeffy]
eststo All

esttab using reg_models_any.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 5: adding country

forval i = 2018(1)2021 {
	reg homework_an physical social female agecont lowed highed nonnat pt self temporary city rural i.country [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_any physical social female agecont lowed highed nonnat pt self temporary city rural i.country [pw=coeffy]
eststo All

esttab using reg_models_any.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Same analysis, with the continuous index as dependent variable

* Block 1: just the two core variables

forval i = 2018(1)2021 {
	reg homework_index physical social [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_index physical social [pw=coeffy]
eststo All

esttab using reg_models_index.tsv, nonotes not r2 lab nomti compress obslast tab replace
eststo clear

* Block 2: adding individual factors, sex, age, educ and country of birth

forval i = 2018(1)2021 {
	reg homework_index physical social female agecont lowed highed nonnat [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_index physical social female agecont lowed highed nonnat [pw=coeffy]
eststo All

esttab using reg_models_index.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 3: adding work-related factors, part-time and temporary contract, self-empl

forval i = 2018(1)2021 {
	reg homework_index physical social female agecont lowed highed nonnat pt self temporary [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_index physical social female agecont lowed highed nonnat pt self temporary [pw=coeffy]
eststo All

esttab using reg_models_index.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 4: adding city and rural

forval i = 2018(1)2021 {
	reg homework_index physical social female agecont lowed highed nonnat pt self temporary city rural [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_index physical social female agecont lowed highed nonnat pt self temporary city rural [pw=coeffy]
eststo All

esttab using reg_models_index.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

* Block 5: adding country

forval i = 2018(1)2021 {
	reg homework_an physical social female agecont lowed highed nonnat pt self temporary city rural i.country [pw=coeffy] if year == `i'
	eststo Year`i'
}
reg homework_index physical social female agecont lowed highed nonnat pt self temporary city rural i.country [pw=coeffy]
eststo All

esttab using reg_models_index.tsv, nonotes not r2 lab nomti compress obslast tab append
eststo clear

/* Region of residence vs. region of work: did the relationship between these change with COVID?

use temp.dta, clear

gen test = reg == regw

tab reglab test, row nof

drop test

* NUTS of residence is coded at 1 digit in Austria, NUTS of work at 2 digits
* So I will recode the latter into 1 digit to make it consistent

replace regw = substr(regw,1,length(regw)-1) + "0" if country == 1

gen test = reg == regw

tab reglab test, row nof

drop test

* OK, Austria is working now. CY cannot be fixed:

tab regw if country == 4
drop if country == 4

* It´s simply missing, so I just have to drop it. As for DE:

tab regw year if country == 6

* Same problem as AT, same fix:

replace regw = substr(regw,1,length(regw)-1) + "0" if country == 6

gen test = reg == regw

tab reglab test, row nof

drop test

* OK, so I think we are set!

* Let´s create a simple variable for regional commuters (reg != regw)

gen regcom = reg != regw

table country year [aw=coeffy], c(mean regcom)
table reglab year [aw=coeffy], c(mean regcom)

* It does not seem that there were significant changes in the patterns of regional
* commuting because of COVID. In most countries, there is no discernable change
* of trend in 2020, and the countries where there are big changes (LT down,
* FR, HR and PL up) seem suspicious, they may be just reflecting measurement issues.

table regw year [aw=coeffy], c(mean regcom)

* Does regional commuting relate to occupations? And to teleworkability?

table isco08_3d year [aw=coeffy], c(mean regcom)

egen tmp=cut(physical), at(0,.1,.9,1.1)
recode tmp (0 = 1 "Not teleworkable") (.1 = 2 "Partly teleworkable") (.9 = 3 "Fully teleworkable"), gen(tw_intervals)
drop tmp

table tw_intervals year [aw=coeffy], c(mean regcom) by(country)

*/
