* First analysis of teleworkability and telework by region in Europe
* JRC-Eurofound collaborative project
* April 2023

set more off
clear
use "C:\Users\a\Documents\teleworkability with eurofound\Regional 2023 EF_JRC\LFSreg2018_21finalr.dta" 
keep if ilostat == 1
drop ilostat
gen n = 1

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

* Now, let´s merge the teleworkability data

* First we have to drop a _merge variable which was in the file, probably used in 
* the merging of external data to create the variable urbrur (linked via region?)
* I also drop urbrur because in principle I do not need it and it´s not from LFS, check with John

drop _ urbrur

tab isco08_3d
replace isco08_3d = isco08_3d*10 if length(string(isco08_3d)) == 2
replace isco08_3d = . if isco08_3d == 0
sort isco08_3d
merge m:1 isco08_3d using "Teleworkability indices.dta"

tab isco08_3d _merge, mis
table isco08_3d [aw=coeffy], c(mean physical mean social)

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

table isco08_3d [aw=coeffy], c(mean physical mean social)

/*

Ok, so this works well. These are the remaining codes:

0
11 (two digits)
21 (two digits)
30 (one digit)
31 (two digits)
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

table isco08_3d [aw=coeffy], c(mean physical mean social)

replace physical = physical/1000
replace social = social/1000

recode homework (3 = 0) (2 = .5) (1 = 1), gen(homework_index)

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

use temp.dta, clear

gen city = degurba == 1
gen female = sex == 2
gen pt = ftpt == 2
gen self = stapro == 0
gen temporary = temp == 2
gen nonnat = countryb != "NAT"

collapse (mean) physical (mean) social (mean) homework_index (mean) city (mean) female (mean) pt (mean) self (mean) temporary (mean) nonnat (rawsum) coeffy (rawsum) n [aw=coeffy], by(isco08_3d country year)

save occup.dta, replace

bysort year: eststo: reg homew physical social female city pt self temp nonnat i.country [pw=coeff]

esttab,	ar2	label

* Very interesting! Our teleworkability index becomes a better predictor of telework with COVID,
* going from a coefficient of .09 to a coefficient of .24. However, it is interesting
* to see that the most important determinant seems to be the share of people in the
* job living in the city: jobs which are more urban were already more frequently
* teleworked, but it really shoots up with COVID (going from .12 to .42!). It is
* also interesting that social interaction was initially significant, as much or 
* more than physical teleworkability, but becomes non-significant with COVID.
* Other interesting stuff: sex does not seem to matter throughout; part-time work
* becomes stronger with COVID in a negative sense (less chance of telework); self-empl
* which was initially the stronger determinant becomes less important with COVID;
* and finally temporary contract or foreign born are not significant.
* This is just a very simple (although informative!) linear regression, which we
* have to repeat in a more fancy way (since the unit of analysis here are isco3d
* occupations for which we have repeated observations, we can try a panel model?
* Discuss with Matteo.

egen country_year_id = group(country year)
gen corr = .
levelsof country_year_id, local(ids)
foreach id in `ids' {
    quietly pwcorr homework physical if country_year_id == `id' [aw=coeffy], sig
    local rho = round(r(rho),0.01)
    replace corr = `rho' if country_year_id == `id'
}
collapse (mean) corr, by(country year)
reshape wide corr, i(country) j(year)

outsheet using occup_corr_phys_telew.csv, delimiter(";") replace

use occup.dta, clear

tw (scat homework physical [aw=coeffy], msymbol(oh)) (lfit homework physical) if country < 8, by(country year, cols(4) note("") legend(off)) xtitle("Teleworkability - physical") ytitle("Actual telework")
graph export "occup_corr_1.png", wid(1024) hei(768) replace
tw (scat homework physical [aw=coeffy], msymbol(oh)) (lfit homework physical) if country > 7 & country < 14, by(country year, cols(4) note("") legend(off)) xtitle("Teleworkability - physical") ytitle("Actual telework")
graph export "occup_corr_2.png", wid(1024) hei(768) replace
tw (scat homework physical [aw=coeffy], msymbol(oh)) (lfit homework physical) if country > 13 & country < 20, by(country year, cols(4) note("") legend(off)) xtitle("Teleworkability - physical") ytitle("Actual telework")
graph export "occup_corr_3.png", wid(1024) hei(768) replace
tw (scat homework physical [aw=coeffy], msymbol(oh)) (lfit homework physical) if country > 20 & country < 30, by(country year, cols(4) note("") legend(off)) xtitle("Teleworkability - physical") ytitle("Actual telework")
graph export "occup_corr_4.png", wid(1024) hei(768) replace
tw (scat homework physical [aw=coeffy], msymbol(oh)) (lfit homework physical) if country == 6 | country == 10 | country == 12 | country == 15 | country == 22 | country == 25, by(country year, cols(4) note("") legend(off)) xtitle("Teleworkability - physical") ytitle("Actual telework")
graph export "occup_corr_big6.png", wid(1024) hei(768) replace

* Clearly, the correlation between physical teleworkability and actual telework
* increased quite significantly, going from .5 to .7 in many cases. So telework
* did not just increase across the board, but it increased more for teleworkable
* occupations

* Another way to show the same:

egen temp=cut(physical), at(0,.1,.9,1.1)
recode temp (0 = 1 "Not teleworkable") (.1 = 2 "Partly teleworkable") (.9 = 3 "Fully teleworkable"), gen(tw_intervals)
drop temp

table tw_intervals year [aw=coeffy], c(mean homework) by(country)
table tw_intervals year [aw=coeffy], c(mean homework)

* We can check whether the interaction between physical and social matters

egen temp=cut(social), at(0,.4,.6,1.1)
recode temp (0 = 1 "Low social") (.4 = 2 "Mid social") (.6 = 3 "High social"), gen(soc_intervals)
drop temp

table soc_intervals year [aw=coeffy], c(mean homework) by(country)
table soc_intervals year [aw=coeffy], c(mean homework)

tab tw soc_in [aw=coeffy], cell nof
table tw soc_in [aw=coeffy], by(country year) c(mean homew)
table tw soc_in [aw=coeffy], by(year) c(mean homew)

* Also very interesting! It seems that, as we predicted, the biggest increases in
* telework took place in teleworkable occupations with low social interaction.
* However, some of these cells have low values and we have to analyse this better.
* Again, discuss with Matteo

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

collapse (mean) physical (mean) social (mean) homework_index (mean) city (mean) rural (mean) female (mean) pt (mean) self (mean) temporary (mean) nonnat (mean) hatlev (mean) agecont (mean) agric (mean) manuf (mean) privserv (mean) publserv (rawsum) coeffy (rawsum) n [aw=coeffy], by(reglab country year)

bysort year: eststo: reg homew physical social female city rural pt self temp nonnat hatlev agecont agric manuf privserv publserv i.country [pw=coeff]

esttab,	ar2	label not

eststo clear

bysort year: reg homew physical [aw=coeffy], notable
table reglab year [aw=coeff], c(mean physical mean homew) row col

* Now the same, but with region*degurba as dependent variable

use temp.dta, clear

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

collapse (mean) physical (mean) social (mean) homework_index (mean) female (mean) pt (mean) self (mean) temporary (mean) nonnat (mean) hatlev (mean) agecont (mean) agric (mean) manuf (mean) privserv (mean) publserv (rawsum) coeffy (rawsum) n [aw=coeffy], by(reglab degurba country year)

bysort year: eststo: reg homew physical social female pt self temp nonnat hatlev agecont agric manuf privserv publserv i.country [pw=coeff]

esttab,	ar2	label not

eststo clear

bysort year: reg homew physical [aw=coeffy], notable

* Region of residence vs. region of work: did the relationship between these change with COVID?

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
