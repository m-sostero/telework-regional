
///////////////////////////////////
// Generates a simple urban rural typology for our LFS regional data file
// NUTs2 
// Four categories - capital city regions, mainly urban, intermediate and mainly rural
// Note that this is a hack - the OECD and Eurostat urban rural typologies are developed at NUTs 3 level
// Also note that LFS microdata at NUTs 2 level has only one region in five smallest member states, that NL 
// does not provide REGION or REGIONW and so is effectively the same, and that AT and DE only provide data at 1 digit level
// 

set more off
clear

cd "R:\SpssStata\EJM_DO_NOT_DELETE\annualreports\REGIONAL 2023\data\NUTS_urbanrural\"
// first import simple file with urban rural typology 
// based on the 2016 NUTs classification available at 
// https://ec.europa.eu/eurostat/web/rural-development/methodology

import excel using "R:\SpssStata\EJM_DO_NOT_DELETE\annualreports\REGIONAL 2023\data\NUTS_urbanrural\Urban-rural-NUTS-2016.xlsx", first sheet ("Urban-rural typology 2016")

/* for 2021 NUTS file below
keep NUTS_ID URBANRURALCATEGORY

rename NUTS_ID reg3d
rename URBANRURALCATEGORY urbrur
*/

rename Urbanr reg3d
rename B urbrur

keep reg3d urbrur

sort reg3d

//gen countrycode=substr(reg3d, 1,2)

save urbrur.dta, replace

clear

/// now merge with file containing population estimates for each nuts 3 region
/// the following csv file is copied from Eurostat and contains pop ests for 2008, 10, 20 and 21 for regions
/// source: https://ec.europa.eu/eurostat/databrowser/view/DEMO_R_GIND3__custom_4768949/default/table?lang=en
/// 2020 pop data is used for the assignment of NUTs 3 to NUTs 2 regions

insheet using "R:\SpssStata\EJM_DO_NOT_DELETE\annualreports\REGIONAL 2023\data\NUTS_urbanrural\demo_r_gind3.csv"

keep geo time obs_v


ren geo reg // string
ren time year
ren obs_v pop

reshape wide pop, i(reg) j(year)

gen len=strlen(reg) // to identify the NUTs3d regions
tab len
keep if len==5
drop len

ren reg reg3d
sort reg3d

merge reg3d using urbrur.dta

tab _m
drop _m


gen reg2d=substr(reg3d,1,4)

/* code from 2019 EJM regional below

replace reg2d="FR82" if reg2d=="FR83" // these are the regional amalgamations in our main analysis file
replace reg2d="ITH1" if reg2d=="ITH2"
replace reg2d="ES63" if reg2d=="ES64"

*/

gen reg1d=substr(reg3d,1,3)
replace reg1d=reg1d+"0" if substr(reg2d,1,2)=="AT" | substr(reg2d,1,2)=="DE" 

 // reg will be the level of analysis adapted for our LFS data, ie only NUTs 1 in DE and AT
replace reg2d=reg1d if substr(reg2d,1,2)=="AT" | substr(reg2d,1,2)=="DE" 



collapse (sum) pop2020, by(reg2d urbrur)

tab reg2d urbrur [aw=pop2020], row nof


drop if substr(reg2d,1,2)=="UK" 
drop if substr(reg2d,1,2)=="AL" 
drop if substr(reg2d,1,2)=="TR" 
drop if substr(reg2d,1,2)=="NO" 
drop if substr(reg2d,1,2)=="CH" 
drop if substr(reg2d,1,2)=="ME" 
drop if substr(reg2d,1,2)=="MK" 
drop if substr(reg2d,1,2)=="RS" 
drop if substr(reg2d,1,2)=="NU"
drop if substr(reg2d,1,2)=="LI"
drop if substr(reg2d,1,2)=="IS" 

// some missing urbrur values need to be eliminated
drop if urbrur==""
drop if urbrur=="URB_RURAL_CLASS"
// as the HR NUTS classification has changed we may need to use the NUTS 2021 urban rural typology for HR

			
reshape wide pop, i(reg) j(urbrur) string
rename pop20201 urban
rename pop20202 inter
rename pop20203 rural

replace urban=0 if urban==.
replace inter=0 if inter==.
replace rural=0 if rural==.

egen regpop=rowtotal(urban inter rural)
replace urban=100*urban/regpop
replace inter=100*inter/regpop
replace rural=100*rural/regpop

/* DISREGARD blocked out code below

// now we apply the rules from de Beer et al 2014
// New classification of urban and rural NUTs 2 regions in Europe
// NIDI working paper 2014/3

"We classify a NUTS 2 region as predominantly urban if the difference between the
percentages of the population living in urban and rural NUTS 3 regions that are part of that
NUTS 2 region exceeds a certain threshold value. Similarly a NUTS 2 region is classified as
predominantly rural if the differences between the percentages of the population living in
rural and urban NUTS 3 regions exceeds another threshold value. We determine both
threshold values in such a way that in each country the percentages of the population living in
urban and rural regions at NUTS 2 level are as close as possible to those at the NUTS 3 level.
The threshold value for predominantly urban regions turns out to equal 40 percentage points.
Thus if in a given NUTS 2 region 60 per cent of the population is living in urban NUTS 3
regions, whereas 10 per cent is living in rural NUTS 3 regions, the NUTS 2 region is
considered as predominantly urban. In contrast, if 30 per cent is living in rural NUTS 3
regions, the NUTS 2 region is considered as intermediate. The threshold value for rural
regions equals 33 percentage points. Thus if in a NUTS 2 region, 50 per cent of the
population is living in rural NUTS 3 regions and 10 per cent in urban regions, the region is
considered as predominantly rural, whereas if 20 percent is living in urban regions, the region
is considered as intermediate. ""

// first we use the actual thresholds of differences in urb/rur population
//  they use, ie 40% difference between 
// urban and rural pop to denote urban  and 33% difference to denote rural, oherwise
// intermediate


*/


gen diff=urban-rural
gen urbrur=.
replace urbrur=1 if diff>40 & diff!=.
replace urbrur=3 if diff<-33
replace urbrur=2 if urbrur!=1 & urbrur!=3

tab urbrur [aw=regpop]	

/* generating the following emp shares for urbrur


          1 | 82.1754107       38.76       38.76
          2 | 92.1020146       43.44       82.21
          3 | 37.7225747       17.79      100.00

		  
.... and the following for the alternative urbrur2, see below		  

1	93.9295311	44.31	44.31
2	78.72850097	37.14	81.44
3	39.341968	18.56	100.00


*/


		
*/
// a second simpler way of classifying regions
// whichever of the three categories is biggest
// this may be better given that we are using a mix of NUTs 1 and NUTs 2 regions

egen maxval=rowmax(urban inter rur)
gen urbrur2=.
replace urbrur2=1 if round(maxval)==round(urban) 
replace urbrur2=2 if round(maxval)==round(inter) 
replace urbrur2=3 if round(maxval)==round(rural) 

tab urbrur2 [aw=regpop]	

// all capital city regions except Mazwowieckie / Warsaw are classifed as urban
// Warsaw should be urban but the region in which it resides is very big

// we solve this problem by creating a second urban category for the capital zones, a useful tweak in any case

gen capital=.
replace capital=1 if reg2d=="BE10" | reg2d=="BG41" | reg2d=="CZ01" | reg2d=="DE30" | reg2d=="DK01" | reg2d=="EL30" | reg2d=="ES30" | reg2d=="FI1B" 
replace capital=1 if reg2d=="FR10" | reg2d=="HR04" | reg2d=="HU11" | reg2d=="IE06" | reg2d=="ITI4" | reg2d=="LT01" | reg2d=="PL91" 
replace capital=1 if reg2d=="PT17" | reg2d=="RO32" | reg2d=="SE11" | reg2d=="SI04" | reg2d=="SK01" | reg2d=="AT10"
// note: 21 countries have capital regions distinct from the rest in this recoding
// the remaining countries are EE, LV, LU, MT, CY which only have one country region '00' each and NL where no regional level variable is available

// note that HR changed its classification in 2020 
// from HR03 (Dalmatia, intermediate) and HR04 (inc Zagreb) up to 2019
// to HR03 (Dalmatia, intermediate) and HR04 transformed into following three regions
// HR05 (Zagreb, capital region), HR02 and HR06 (Slavonia, both rural regions)
 
replace urbrur2=0 if capital==1
replace urbrur2=4 if substr(reg2d,3,4)=="00"

drop urbrur

rename urbrur2 urbrur

lab define urbrur 0 "Capital region" 1 "Mainly urban" 2 "Intermediate" 3 "Mainly rural" 4 "Regions undifferentiated"
lab values urbrur urbrur

tab urbrur [aw=regpop]

keep reg2d urbrur urban inter rural regpop

rename reg2d reg

sort reg

save urbrurmerge.dta, replace

keep reg urbrur

sort reg

save urbrurmerge2.dta, replace // shorter version

// we should add other regional categorisations
// eg. cohesion fund areas
// something on digital infrastructure, ie broadband penetration
// median age?
// ...