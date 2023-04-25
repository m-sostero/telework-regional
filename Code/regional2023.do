

////////////////////////////////////////////////////////////////////////////////////////////////////////////
// MAKES a combined country file for 2018-21 with relevant variables for 
// EF/JRC project on regional shifts in employment post COVID 
// JOH 260123
///////////////////////////////////////////////////////////////////////////////////////////////////////////


set more off
clear *


local filestublist="BE2018 DE2018 FR2018 IT2018 IE2018 DK2018 EL2018 LU2018 NL2018 ES2018 PT2018 AT2018 SE2018 FI2018 CZ2018 EE2018 HU2018 PL2018 RO2018 SI2018 LT2018 LV2018 SK2018 CY2018 MT2018 BG2018 HR2018 BE2019 DE2019 FR2019 IT2019 IE2019 DK2019 EL2019 LU2019 NL2019 ES2019 PT2019 AT2019 SE2019 FI2019 CZ2019 EE2019 HU2019 PL2019 RO2019 SI2019 LT2019 LV2019 SK2019 CY2019 MT2019 BG2019 HR2019 BE2020 DE2020 FR2020 IT2020 IE2020 DK2020 EL2020 LU2020 NL2020 ES2020 PT2020 AT2020 SE2020 FI2020 CZ2020 EE2020 HU2020 PL2020 RO2020 SI2020 LT2020 LV2020 SK2020 CY2020 MT2020 BG2020 HR2020 BE2021 DE2021 FR2021 IT2021 IE2021 DK2021 EL2021 LU2021 NL2021 ES2021 PT2021 AT2021 SE2021 FI2021 CZ2021 EE2021 HU2021 PL2021 RO2021 SI2021 LT2021 LV2021 SK2021 CY2021 MT2021 BG2021 HR2021"

*/
// NB according to 'datefileinfo' there should be MT data for YEAR 2000 onwards but only MT2009 onwards are available in the JUNE 2019 data release
// TO BE CHECKED WITH EUROSTAT

* Specify path where the dta files are stored
* (e.g. "E:\data\EU-LFS\csv\").
local csv_data_path "R:\SpssStata\EJM_DO_NOT_DELETE\DATA\LFS_For_Researchers_1983to2021\"

* Specify name of final file (e.g. finalfile.dta) for three year combined data -  2018 2021
local finalfile "LFSreg2018_21.dta"



*-----   Loop over all files in stublist   ------.

foreach filestub in `filestublist' {

// "`="`csv_data_path'"+"`filestub'"+"_y.csv"'"

import delimited "`="`csv_data_path'"+"`filestub'"+"_y.csv"'", clear
// "R:\SpssStata\EJM_DO_NOT_DELETE\DATA\LFS_For_Researchers_1983to2021\BE2011_y.csv", varnames(1) 

keep empstat coeffy country region_2d degurba year sex age_grp countryb region_2dw ftpt stapro temp ilostat nace2_1d hatlevel isco08_3d countryw homework hhcomp 
  

	// check first if region vars are numeric as they are in some countries
	// if yes, convert them to string variables
        capture confirm numeric variable region_2d
        if !_rc {
		tostring region_2d, replace
        }
		
		capture confirm numeric variable region_2dw
        if !_rc {
		tostring region_2dw, replace
        }

save `="`csv_data_path'"+"`filestub'"+"_r.dta"', replace

//save `="`stata_data_path'"+"`filestub'"+"_y.dta"', replace

clear
}

clear

/* TO NOTE: a/ some smaller countries have no regional differentiation, eg. MT, EE etc, b/ NL does not have any regional differentiation, c/ DE and AT only provide REGION at NUTs1d level (though strangely they provide REGIONW at 2d)

*/
* Specify path where the dta files are stored
* (e.g. "E:\data\EU-LFS\csv\").
local csv_data_path "R:\SpssStata\EJM_DO_NOT_DELETE\DATA\LFS_For_Researchers_1983to2021\"

* Specify name of final file (e.g. finalfile.dta) for three year combined data -  2018 2021
local finalfile "LFSreg2018_21.dta"

use "R:\SpssStata\EJM_DO_NOT_DELETE\DATA\LFS_For_Researchers_1983to2021\BE2018_r.dta", replace



local filestublist="DE2018 FR2018 IT2018 IE2018 DK2018 EL2018 LU2018 NL2018 ES2018 PT2018 AT2018 SE2018 FI2018 CZ2018 EE2018 HU2018 PL2018 RO2018 SI2018 LT2018 LV2018 SK2018 CY2018 MT2018 BG2018 HR2018 BE2019 DE2019 FR2019 IT2019 IE2019 DK2019 EL2019 LU2019 NL2019 ES2019 PT2019 AT2019 SE2019 FI2019 CZ2019 EE2019 HU2019 PL2019 RO2019 SI2019 LT2019 LV2019 SK2019 CY2019 MT2019 BG2019 HR2019 BE2020 DE2020 FR2020 IT2020 IE2020 DK2020 EL2020 LU2020 NL2020 ES2020 PT2020 AT2020 SE2020 FI2020 CZ2020 EE2020 HU2020 PL2020 RO2020 SI2020 LT2020 LV2020 SK2020 CY2020 MT2020 BG2020 HR2020 BE2021 DE2021 FR2021 IT2021 IE2021 DK2021 EL2021 LU2021 NL2021 ES2021 PT2021 AT2021 SE2021 FI2021 CZ2021 EE2021 HU2021 PL2021 RO2021 SI2021 LT2021 LV2021 SK2021 CY2021 MT2021 BG2021 HR2021"



foreach filestub in `filestublist' {

append using "`="`csv_data_path'"+"`filestub'"+"_r.dta"'"

}



save `="`csv_data_path'"+"`filestub'"+"`finalfile'"', replace
//save "C:\data\LFSreg2018_21.dta", replace

//use "R:\SpssStata\EJM_DO_NOT_DELETE\DATA\LFS_For_Researchers_1983to2021\LFSreg2018_21.dta", clear

encode country, gen(ctry)
tab ctry

drop country
rename ctry country	

// stapro
	
label define stapro 0 "Self-employed"
label define stapro 3 "£mployed", add
label define stapro 4 "Family worker", add
label define stapro 9 "Not applicable", add
label values stapro stapro

// temp
	
label define temp 1 "Permanent"
label define temp 2 "Temporary", add
label define temp 9 "Not applicable", add
label values temp temp


// sex
	
label define sex 1 "Male"
label define sex 2 "Female", add
label values sex sex

tab sex

// ftpt
	
label define ftpt 1 "Full-time"
label define ftpt 2 "Part-time", add
label define ftpt 9 "NA", add
label values ftpt ftpt

tab ftpt

// empstat

label define empstat 1 "Employed"
label define empstat 2 "Not employed", add
label define empstat 9 "Not applicable", add
label values empstat empstat

tab empstat

// ilostat

label define ilostat 1 "Employed"
label define ilostat 2 "Unemployed", add
label define ilostat 3 "Outside the labour force", add
label values ilostat ilostat

tab ilostat

// degurba

label define degurba 1 "Cities"
label define degurba 2 "Towns and suburbs", add
label define degurba 3 "Rural areas", add
label values degurba degurba
tab degurba

// homework

label define homework 1 "Person mainly works at home"
label define homework 2 "Person sometimes works at home", add
label define homework 3 "Person never works at home", add
label define homework 9 "Not applicable", add
label values homework homework
tab homework

// hhcomp

label define hhcomp 10 "One adult without children"
label define hhcomp 11 "One adult with at least an own child <15", add
label define hhcomp 12 "One adult with at least an own child 15-17", add
label define hhcomp 13 "One adult with at least another child", add
label define hhcomp 20 "One couple without children", add
label define hhcomp 21 "One couple with at least an own child <15", add
label define hhcomp 22 "One couple with at least an own child 15-17", add
label define hhcomp 23 "One couple with at least another child <15", add
label define hhcomp 30 "Two adults (not a couple) or more without children", add
label define hhcomp 31 "Two adults (not a couple) or more with at least an own chid <15", add
label define hhcomp 32 "Two adults (not a couple) or more with at least an own child 15-17", add
label define hhcomp 33 "Two adults (not a couple) or more with at least another child <15", add
label values hhcomp hhcomp
tab hhcomp

	  ///////////////////////////////////////////////
	///  Coding of variable isco2d, NEW ISCO-08 ///
	display "--- isco2d ---"

	gen isco2d = floor(isco08_3d/10) if isco08_3d != 999 & isco08_3d !=.
	replace isco2d=isco08_3d if isco08_3d==999
	replace isco2d=99 if isco08_3d==.
	

	tab isco2d
	
label define isco2d ///isco2d	Description
0 	"Armed forces_1d" ///
1	"Commissioned armed forces officers" ///
2	"Non-commissioned armed forces officers" ///
3	"Armed forces occupations, other ranks" ///
10 	"Managers_1d" ///
11	"Chief executives, senior officials and legislators" ///
12	"Administrative and commercial managers" ///
13	"Production and specialised services managers" ///
14  "Hospitality, retail and other services managers" ///
20	"Professionals_1d" ///
21	"Science and engineering professionals" ///
22	"Health professionals" ///
23	"Teaching professionals" ///
24	"Business and administration professionals" ///
25  "ICT professionals" ///
26  "Legal, social and cultural professionals" ///
30	"Technicians and associate professionals_1d" ///
31	"Science and engineering associate professionals" ///
32	"Health associate professionals" ///
33	"Business and administration associate professionals" ///
34	"Legal, social, cultural and related associate professionals" ///
35  "Information and communication technicians" ///
40	"Clerical support workers_1d" ///
41	"General and keyboard clerks" ///
42	"Customer services clerks" ///
43	"Numerical and material recording clerks" ///
44	"Other clerical support workers" ///
50	"Service and sales workers_1d" ///
51	"Personal service workers" ///
52	"Sales workers" ///
53	"Personal care workers" ///
54	"Protective services workers" ///
60	"Skilled agricultural, forestry and fishery workers_1d" ///
61	"Market-oriented skilled agricultural workers" ///
62	"Market-oriented skilled forestry, fishery and hunting workers" ///
63	"Subsistence farmers, fishers, hunters and gatherers" ///
70	"Craft and related trade workers_1d" ///
71	"Building and related trades workers, excluding electricians" ///
72	"Metal, machinery and related trades workers" ///
73	"Handicraft and printing workers" ///
74	"Electrical and electronic trades workers" ///
75	"Food processing, wood working, garment and other craft and related trades workers" ///
80	"Plant and machine operators and assemblers_1d" ///
81	"Stationary plant and machine operators" ///
82	"Assemblers" ///
83	"Drivers and mobile plant operators" ///
90	"Elementary occupations_1d" ///
91	"Cleaners and helpers" ///
92	"Agricultural, forestry and fishery labourers" ///
93	"Labourers in mining, construction, manufacturing and transport" ///
94	"Food preparation assistants" ///
95	"Street and related sales and service workers" ///
96	"Refuse workers and other elementary workers" ///
99 	"Non response" ///
999 "Not applicable" ///

label values isco2d isco2d

tab year isco2d

	/////////////////////////////////////////////////
	///  Recoding of variable isco1d NEW ISCO-08  ///
	display "--- isco1d ---"

//recode isco2d (10/14=1)(20/26=2)(30/35=3)(40/44=4)(50/54=5)(60/63=6)(70/75=7)(80/83=8)(90/96=9)(0/3=10)(*=.), gen(isco1d)
recode isco2d (10/14=10)(20/26=20)(30/35=30)(40/44=40)(50/54=50)(60/63=60)(70/75=70)(80/83=80)(90/96=90)(0/3=0)(99=99), gen(isco1d)
	
label define isco1d ///isco2d	Description
10 	"Managers" ///
20	"Professionals" ///
30	"Technicians and associate professionals" ///
40	"Clerical support workers" ///
50	"Service and sales workers" ///
60	"Skilled agricultural, forestry and fishery workers" ///
70	"Craft and related trade workers" ///
80	"Plant and machine operators and assemblers" ///
90	"Elementary occupations" ///
0	"Armed forces" ///
99 	"Non response"

label values isco1d isco1d

tab year isco1d



	/////////////////////////////////////////////////
	///  NACE2 _1d  ///
	display "--- nace2_1d ---"

replace nace2_1d="ZZ" if nace2_1d=="9"
replace nace2_1d="Z" if nace2_1d==""

encode nace2_1d, gen(nace1d)
tab nace1d nace2_1d

capture label drop nace1d
	
label define nace1d ///
1 	"A - Agriculture, forestry and fishing" ///
2 	"B - Mining and quarrying"  ///
3 	"C - Manufacturing"  ///
4 	"D - Electricity, gas, steam and air conditioning supply"  ///
5 	"E - Water supply; sewerage, waste management and remediation activities"  ///
6 	"F - Construction" ///
7 	"G - Wholesale and retail trade; repair of motor vehicles and motorcycles" ///
8 	"H - Transportation and storage" ///
9 	"I - Accommodation and food service activities" ///
10 	"J - Information and communication" ///
11 	"K - Financial and insurance activities" ///
12 	"L - Real estate activities" ///
13 	"M - Professional, scientific and technical activities" ///
14 	"N - Administrative and support service activities" ///
15 	"O - Public administration and defence; compulsory social security" ///
16 	"P - Education" ///
17 	"Q - Human health and social work activities" ///
18 	"R - Arts, entertainment and recreation" ///
19 	"S - Other service activities" ///
20 	"T - Activities of households as employers" ///
21 	"U - Activities of extraterritorial organizations and bodies" ///
22  "Non response" ///
23  "Not applicable"
label values nace1d nace1d

tab nace1d

/// age grp

gen age=.
replace age=1 if age_grp=="Y15-19"
replace age=2 if age_grp=="Y20-24"
replace age=3 if age_grp=="Y25-29"
replace age=4 if age_grp=="Y30-34"
replace age=5 if age_grp=="Y35-39"
replace age=6 if age_grp=="Y40-44"
replace age=7 if age_grp=="Y45-49"
replace age=8 if age_grp=="Y50-54"
replace age=9 if age_grp=="Y55-59"
replace age=10 if age_grp=="Y60-64"
replace age=11 if age_grp=="Y65-69"
replace age=12 if age_grp=="Y70-74"
replace age=13 if age_grp=="Y75-79"
replace age=13 if age_grp=="Y80-84"
replace age=13 if age_grp=="Y85-89" 
replace age=13 if age_grp=="Y90-94" 
replace age=13 if age_grp=="Y95-99"
replace age=13 if age_grp=="Y_GE100" 
replace age=13 if age_grp=="Y_GE75" 
replace age=13 if age_grp=="Y_GE80" 
replace age=13 if age_grp=="Y_GE85" 
replace age=13 if age_grp=="Y_GE90" 

label define age 1 "15-19"
label define age 2 "20-24", add
label define age 3 "25-29", add
label define age 4 "30-34", add
label define age 5 "35-39", add
label define age 6 "40-44", add
label define age 7 "45-49", add
label define age 8 "50-54", add
label define age 9 "55-59", add
label define age 10 "60-64", add
label define age 11 "65-69", add
label define age 12 "70-74", add
label define age 13 "75+", add
label values age age

tab age age_grp, mis

drop nace2_1d age_grp

//keep if empstat==1
save `="`csv_data_path'"+"`filestub'"+"`finalfile'"', replace
//save "C:\data\LFSreg2018_21r.dta", replace

//use "C:\data\LFSreg2018_21r.dta", clear
decode country, gen(ctry)

tab ctry

gen reg=ctry+region_2d

replace reg = substr(reg,1,2)+"0"+substr(reg,3,1) if country==4 // CY
replace reg = substr(reg,1,2)+"0"+substr(reg,3,1) if country==5 // CZ
replace reg = substr(reg,1,2)+"0"+substr(reg,3,1) if country==7 // DK
replace reg = substr(reg,1,2)+"0"+substr(reg,3,1) if country==8 // EE
replace reg = substr(reg,1,2)+"0"+substr(reg,3,1) if country==13 // HR
replace reg = substr(reg,1,2)+"0"+substr(reg,3,1) if country==15 // IE
replace reg = substr(reg,1,2)+"0"+substr(reg,3,1) if country==17 // LT
replace reg = substr(reg,1,2)+"0"+substr(reg,3,1) if country==18 // LU
replace reg = substr(reg,1,2)+"0"+substr(reg,3,1) if country==19 // LV
replace reg = substr(reg,1,2)+"0"+substr(reg,3,1) if country==20 // MT
replace reg = substr(reg,1,2)+"0"+substr(reg,3,1) if country==21 // NL
replace reg = substr(reg,1,2)+"0"+substr(reg,3,1) if country==26 // SI
replace reg = substr(reg,1,2)+"0"+substr(reg,3,1) if country==27 // SK

gen reglab=""

replace reglab="AT10_Ostösterreich" if reg=="AT10"
replace reglab="AT20_Südösterreich" if reg=="AT20"
replace reglab="AT30_Westösterreich" if reg=="AT30"
replace reglab="BE10_Région de Bruxelles-Capitale/ Brussels Hoofdstedelijk Gewest" if reg=="BE10"
replace reglab="BE21_Prov. Antwerpen" if reg=="BE21"
replace reglab="BE22_Prov. Limburg (BE)" if reg=="BE22"
replace reglab="BE23_Prov. Oost-Vlaanderen" if reg=="BE23"
replace reglab="BE24_Prov. Vlaams-Brabant" if reg=="BE24"
replace reglab="BE25_Prov. West-Vlaanderen" if reg=="BE25"
replace reglab="BE31_Prov. Brabant Wallon" if reg=="BE31"
replace reglab="BE32_Prov. Hainaut" if reg=="BE32"
replace reglab="BE33_Prov. Liège" if reg=="BE33"
replace reglab="BE34_Prov. Luxembourg (BE)" if reg=="BE34"
replace reglab="BE35_Prov. Namur" if reg=="BE35"
replace reglab="BG31_North-West" if reg=="BG31"
replace reglab="BG32_North-Central" if reg=="BG32"
replace reglab="BG33_North-East" if reg=="BG33"
replace reglab="BG34_South-Eastern" if reg=="BG34"
replace reglab="BG41_South-Central" if reg=="BG41"
replace reglab="BG42_South-Western" if reg=="BG42"
replace reglab="CY00_Cyprus" if reg=="CY00"
replace reglab="CZ01_Praha" if reg=="CZ01"
replace reglab="CZ02_Střední Čechy" if reg=="CZ02"
replace reglab="CZ03_Jihozápad" if reg=="CZ03"
replace reglab="CZ04_Severozápad" if reg=="CZ04"
replace reglab="CZ05_Severovýchod" if reg=="CZ05"
replace reglab="CZ06_Jihovýchod" if reg=="CZ06"
replace reglab="CZ07_Střední Morava" if reg=="CZ07"
replace reglab="CZ08_Moravskoslezsko" if reg=="CZ08"
replace reglab="DE10_Baden-Württemberg" if reg=="DE10"
replace reglab="DE20_Bayern" if reg=="DE20"
replace reglab="DE30_Berlin" if reg=="DE30"
replace reglab="DE40_Brandenburg" if reg=="DE40"
replace reglab="DE50_Bremen" if reg=="DE50"
replace reglab="DE60_Hamburg" if reg=="DE60"
replace reglab="DE70_Hessen" if reg=="DE70"
replace reglab="DE80_Mecklenburg-Vorpommern" if reg=="DE80"
replace reglab="DE90_Niedersachsen" if reg=="DE90"
replace reglab="DEA0_Nordrhein-Westfalen" if reg=="DEA0"
replace reglab="DEB0_Rheinland-Pfalz" if reg=="DEB0"
replace reglab="DEC0_Saarland" if reg=="DEC0"
replace reglab="DED0_Sachsen" if reg=="DED0"
replace reglab="DEE0_Sachsen-Anhalt" if reg=="DEE0"
replace reglab="DEF0_Schleswig-Holstein" if reg=="DEF0"
replace reglab="DEG0_Thüringen" if reg=="DEG0"
replace reglab="DK01_Hovedstaden" if reg=="DK01"
replace reglab="DK02_Sjælland" if reg=="DK02"
replace reglab="DK03_Syddanmark" if reg=="DK03"
replace reglab="DK04_Midtjylland" if reg=="DK04"
replace reglab="DK05_Nordjylland" if reg=="DK05"
replace reglab="EE00_Estonia" if reg=="EE00"
replace reglab="EL30_Attica" if reg=="EL30"
replace reglab="EL41_North Aegean" if reg=="EL41"
replace reglab="EL42_South Aegean" if reg=="EL42"
replace reglab="EL43_Crete" if reg=="EL43"
replace reglab="EL51_Eastern Macedonia and Thrace" if reg=="EL51"
replace reglab="EL52_Central Macedonia" if reg=="EL52"
replace reglab="EL53_Western Macedonia" if reg=="EL53"
replace reglab="EL54_Epirus" if reg=="EL54"
replace reglab="EL61_Thessaly" if reg=="EL61"
replace reglab="EL62_Ionian Islands" if reg=="EL62"
replace reglab="EL63_Western Greece" if reg=="EL63"
replace reglab="EL64_Central Greece" if reg=="EL64"
replace reglab="EL65_Peloponnese" if reg=="EL65"
replace reglab="ES11_Galicia" if reg=="ES11"
replace reglab="ES12_Principado de Asturias" if reg=="ES12"
replace reglab="ES13_Cantabria" if reg=="ES13"
replace reglab="ES21_País Vasco" if reg=="ES21"
replace reglab="ES22_Comunidad Foral de Navarra" if reg=="ES22"
replace reglab="ES23_La Rioja" if reg=="ES23"
replace reglab="ES24_Aragón" if reg=="ES24"
replace reglab="ES30_Comunidad de Madrid" if reg=="ES30"
replace reglab="ES41_Castilla y León" if reg=="ES41"
replace reglab="ES42_Castilla-La Mancha" if reg=="ES42"
replace reglab="ES43_Extremadura" if reg=="ES43"
replace reglab="ES51_Cataluña" if reg=="ES51"
replace reglab="ES52_Comunitat Valenciana" if reg=="ES52"
replace reglab="ES53_Illes Balears" if reg=="ES53"
replace reglab="ES61_Andalucía" if reg=="ES61"
replace reglab="ES62_Región de Murcia" if reg=="ES62"
replace reglab="ES63_Ciudad de Ceuta" if reg=="ES63"
replace reglab="ES64_Ciudad de Melilla" if reg=="ES64"
replace reglab="ES70_Canarias" if reg=="ES70"
replace reglab="FI19_Länsi-Suomi" if reg=="FI19"
replace reglab="FI1B_Helsinki-Uusimaa" if reg=="FI1B"
replace reglab="FI1C_Etelä-Suomi" if reg=="FI1C"
replace reglab="FI1D_Pohjois- ja Itä-Suomi" if reg=="FI1D"
replace reglab="FI20_Åland" if reg=="FI20"
replace reglab="FR10_Ile-de-France" if reg=="FR10"
replace reglab="FRB0_Centre — Val de Loire" if reg=="FRB0"
replace reglab="FRC1_Bourgogne" if reg=="FRC1"
replace reglab="FRC2_Franche-Comté" if reg=="FRC2"
replace reglab="FRD1_Basse-Normandie" if reg=="FRD1"
replace reglab="FRD2_Haute-Normandie" if reg=="FRD2"
replace reglab="FRE1_Nord-Pas de Calais" if reg=="FRE1"
replace reglab="FRE2_Picardie" if reg=="FRE2"
replace reglab="FRF1_Alsace" if reg=="FRF1"
replace reglab="FRF2_Champagne-Ardenne" if reg=="FRF2"
replace reglab="FRF3_Lorraine" if reg=="FRF3"
replace reglab="FRG0_Pays de la Loire" if reg=="FRG0"
replace reglab="FRH0_Bretagne" if reg=="FRH0"
replace reglab="FRI1_Aquitaine" if reg=="FRI1"
replace reglab="FRI2_Limousin" if reg=="FRI2"
replace reglab="FRI3_Poitou-Charentes" if reg=="FRI3"
replace reglab="FRJ1_Languedoc-Roussillon" if reg=="FRJ1"
replace reglab="FRJ2_Midi-Pyrénées" if reg=="FRJ2"
replace reglab="FRK1_Auvergne" if reg=="FRK1"
replace reglab="FRK2_Rhône-Alpes" if reg=="FRK2"
replace reglab="FRL0_Provence-Alpes-Côte d'Azur" if reg=="FRL0"
replace reglab="FRM0_Corse" if reg=="FRM0"
replace reglab="FRY1_Guadeloupe" if reg=="FRY1"
replace reglab="FRY2_Martinique" if reg=="FRY2"
replace reglab="FRY3_Guyane" if reg=="FRY3"
replace reglab="FRY4_La Réunion" if reg=="FRY4"
replace reglab="HR02_Panonska Hrvatska" if reg=="HR02"
replace reglab="HR03_Jadranska Hrvatska" if reg=="HR03"
replace reglab="HR04_Kontinentalna Hrvatska (old)" if reg=="HR04"
replace reglab="HR05_Grad Zagreb" if reg=="HR05"
replace reglab="HR06_Sjeverna Hrvatska" if reg=="HR06"
replace reglab="HU11_Budapest" if reg=="HU11"
replace reglab="HU12_Pest" if reg=="HU12"
replace reglab="HU21_Közép-Dunántúl" if reg=="HU21"
replace reglab="HU22_Nyugat-Dunántúl" if reg=="HU22"
replace reglab="HU23_Dél-Dunántúl" if reg=="HU23"
replace reglab="HU31_Észak-Magyarország" if reg=="HU31"
replace reglab="HU32_Észak-Alföld" if reg=="HU32"
replace reglab="HU33_Dél-Alföld" if reg=="HU33"
replace reglab="IE04_Northern and Western" if reg=="IE04"
replace reglab="IE05_Southern" if reg=="IE05"
replace reglab="IE06_Eastern and Midland" if reg=="IE06"
replace reglab="ITC1_Piemonte" if reg=="ITC1"
replace reglab="ITC2_Valle d'Aosta/Vallée d'Aoste" if reg=="ITC2"
replace reglab="ITC3_Liguria" if reg=="ITC3"
replace reglab="ITC4_Lombardia" if reg=="ITC4"
replace reglab="ITF1_Abruzzo" if reg=="ITF1"
replace reglab="ITF2_Molise" if reg=="ITF2"
replace reglab="ITF3_Campania" if reg=="ITF3"
replace reglab="ITF4_Puglia" if reg=="ITF4"
replace reglab="ITF5_Basilicata" if reg=="ITF5"
replace reglab="ITF6_Calabria" if reg=="ITF6"
replace reglab="ITG1_Sicilia" if reg=="ITG1"
replace reglab="ITG2_Sardegna" if reg=="ITG2"
replace reglab="ITH1_Provincia Autonoma di Bolzano/Bozen" if reg=="ITH1"
replace reglab="ITH2_Provincia Autonoma di Trento" if reg=="ITH2"
replace reglab="ITH3_Veneto" if reg=="ITH3"
replace reglab="ITH4_Friuli-Venezia Giulia" if reg=="ITH4"
replace reglab="ITH5_Emilia-Romagna" if reg=="ITH5"
replace reglab="ITI1_Toscana" if reg=="ITI1"
replace reglab="ITI2_Umbria" if reg=="ITI2"
replace reglab="ITI3_Marche" if reg=="ITI3"
replace reglab="ITI4_Lazio" if reg=="ITI4"
replace reglab="LT01_Sostinės regionas" if reg=="LT01"
replace reglab="LT02_Vidurio ir vakarų Lietuvos regionas" if reg=="LT02"
replace reglab="LU00_Luxembourg" if reg=="LU00"
replace reglab="LV00_Latvia" if reg=="LV00"
replace reglab="MT00_Malta" if reg=="MT00"
replace reglab="NL00_The Netherlands" if reg=="NL00"
replace reglab="PL21_Małopolskie" if reg=="PL21"
replace reglab="PL22_Śląskie" if reg=="PL22"
replace reglab="PL41_Wielkopolskie" if reg=="PL41"
replace reglab="PL42_Zachodniopomorskie" if reg=="PL42"
replace reglab="PL43_Lubuskie" if reg=="PL43"
replace reglab="PL51_Dolnośląskie" if reg=="PL51"
replace reglab="PL52_Opolskie" if reg=="PL52"
replace reglab="PL61_Kujawsko-pomorskie" if reg=="PL61"
replace reglab="PL62_Warmińsko-mazurskie" if reg=="PL62"
replace reglab="PL63_Pomorskie" if reg=="PL63"
replace reglab="PL71_Łódzkie" if reg=="PL71"
replace reglab="PL72_Świętokrzyskie" if reg=="PL72"
replace reglab="PL81_Lubelskie" if reg=="PL81"
replace reglab="PL82_Podkarpackie" if reg=="PL82"
replace reglab="PL84_Podlaskie" if reg=="PL84"
replace reglab="PL91_Warszawski stołeczny" if reg=="PL91"
replace reglab="PL92_Mazowiecki regionalny" if reg=="PL92"
replace reglab="PT11_Norte" if reg=="PT11"
replace reglab="PT15_Algarve" if reg=="PT15"
replace reglab="PT16_Centro (PT)" if reg=="PT16"
replace reglab="PT17_Área Metropolitana de Lisboa" if reg=="PT17"
replace reglab="PT18_Alentejo" if reg=="PT18"
replace reglab="PT20_Região Autónoma dos Açores" if reg=="PT20"
replace reglab="PT30_Região Autónoma da Madeira" if reg=="PT30"
replace reglab="RO11_Nord-Vest" if reg=="RO11"
replace reglab="RO12_Centru" if reg=="RO12"
replace reglab="RO21_Nord-Est" if reg=="RO21"
replace reglab="RO22_Sud-Est" if reg=="RO22"
replace reglab="RO31_Sud-Muntenia" if reg=="RO31"
replace reglab="RO32_Bucureşti-Ilfov" if reg=="RO32"
replace reglab="RO41_Sud-Vest Oltenia" if reg=="RO41"
replace reglab="RO42_Vest" if reg=="RO42"
replace reglab="SE11_Stockholm" if reg=="SE11"
replace reglab="SE12_Östra Mellansverige" if reg=="SE12"
replace reglab="SE21_Småland med öarna" if reg=="SE21"
replace reglab="SE22_Sydsverige" if reg=="SE22"
replace reglab="SE23_Västsverige" if reg=="SE23"
replace reglab="SE31_Norra Mellansverige" if reg=="SE31"
replace reglab="SE32_Mellersta Norrland" if reg=="SE32"
replace reglab="SE33_Övre Norrland" if reg=="SE33"
replace reglab="SI03_Vzhodna Slovenija" if reg=="SI03"
replace reglab="SI04_Zahodna Slovenija" if reg=="SI04"
replace reglab="SK01_Bratislavský kraj" if reg=="SK01"
replace reglab="SK02_Západné Slovensko" if reg=="SK02"
replace reglab="SK03_Stredné Slovensko" if reg=="SK03"
replace reglab="SK04_Východné Slovensko" if reg=="SK04"

encode reglab, gen(regl)
drop reglab
rename regl reglab

replace region_2dw="00" if region_2dw=="0"

gen regw=""

replace regw=countryw+region_2dw if countryw!="999" & strlen(countryw)==2

replace regw = substr(regw,1,2)+"0"+substr(regw,3,1) if strlen(regw)==3
// some countries changed from a one digit to a two digit version of region_2dw during the period
// for other countries where there is only one NUTs 2 code, eg. LU or NL, this ensures that they are standardised to a common four character format, eg NL00

replace regw="NS00" if strlen(countryw)>2 & countryw!="999"
// Not stated - these are observations where either EU or nonEU or FOR for foreign are the only indications given

replace regw="NS00" if real(countryw)>0 & real(countryw)<20

tab countryw

gen EU=. // EU27 + UK
replace EU=1 if countryw=="AT" | countryw=="BE" | countryw=="BG" | countryw=="HR" | countryw=="CY" | countryw=="CZ" | countryw=="DK" 
replace EU=1 if countryw=="EE" | countryw=="FI" | countryw=="FR" | countryw=="DE" | countryw=="EL" | countryw=="HU" | countryw=="IE" 
replace EU=1 if countryw=="IT" | countryw=="LT" | countryw=="LU" | countryw=="LV" | countryw=="MT" | countryw=="NL" 
replace EU=1 if countryw=="PL" | countryw=="PT" | countryw=="RO" | countryw=="ES" | countryw=="SE" | countryw=="SK" | countryw=="SI" 
replace EU=1 if countryw=="UK" | countryw=="EU27_2020" | countryw=="EU28"
tab EU

gen EEA=. // LI NO CH and IS
replace EEA=1 if countryw=="LI" | countryw=="NO" | countryw=="CH" | countryw=="IS"

gen rEur=. // other dependencies and UN defined European countries
replace rEur=1 if countryw=="AL" | countryw=="AD" | countryw=="BA" | countryw=="BY" | countryw=="FO" | countryw=="GI" | countryw=="VA" 
replace rEur=1 if countryw=="MK" | countryw=="RU" | countryw=="SM" | countryw=="IM" | countryw=="JE" | countryw=="GG" | countryw=="MD" 
replace rEur=1 if countryw=="MC"  | countryw=="ME" | countryw=="RS" | countryw=="UA" | countryw=="EUR_NEU28" | countryw=="EUR_NEU27_2020"

gen othercountry=.
replace othercountry=1 if real(countryw)>0 & real(countryw)<20 // some entries have numerical codes, we assume 
// these are non identified other countries
replace othercountry=1 if countryw=="FOR"

gen notstated=.
replace notstated=1 if countryw=="" 
// as per LFS manual;  a high share of these in the Nordic countries and (especially) NL where there is no info // anyway about region / regionw

gen notapplic=.
replace notapplic=1 if countryw=="999"

gen ROW=.
replace ROW=1 if countryw!="" & EU!=1 & EEA!=1 & rEur!=1 & notstated!=1 & notapplic!=1

gen ctryw=. // let's create a broad categorisation of country of work with the following categories
// 1. Work in own member state 2. Work in another member state or UK. 3. Work in EEA. 4 Work in greater Europe. 5. Work outside Europe. 6. Country of work not stated. 7999 Not applicable. 

foreach c in AT BE BG HR CY CZ DK EE FI FR DE EL HU  IE IT LT LU LV MT NL PL PT RO ES SE SK SI {
replace ctryw=1 if ctry=="`c'" & countryw=="`c'"
replace ctryw=2 if ctry=="`c'" & countryw!="`c'" & EU==1
replace ctryw=3 if ctry=="`c'" & countryw!="`c'" & EEA==1
replace ctryw=4 if ctry=="`c'" & countryw!="`c'" & rEur==1
replace ctryw=5 if ctry=="`c'" & countryw!="`c'" & ROW==1
replace ctryw=6 if ctry=="`c'" & countryw!="`c'" & othercountry==1
replace ctryw=7 if ctry=="`c'" & countryw!="`c'" & notstated==1
replace ctryw=999 if ctry=="`c'" & countryw!="`c'" & notapplic==1
}

label define ctryw 1 "Work in own MS" 2 "Work in another EU MS or UK" 3 "Work in EEA" 4 "Work in other European country" 5 "Work in ROW" 6 "Work in another country" 7 "Not stated" 999 "Not applicable"
label values ctryw ctryw

tab country ctryw [aw=coeff], row nof

/* we could extend this ctryw to include the differentiation within category 1 between those working in the same REGION and REGIONW within their country and those who work in a different REGION of residence 
... I have not done so as it is somewhat messy a/ some countries such as DE and AT report REGIONW at NUTs2 level but REGION at NUTs1, b/ there are many blanks / missing data for REGIONW
*/

drop EU EEA rEur othercountry notstated notapplic ROW ctry // no longer needed

order year coeffy country ilostat empstat countryw region_2d isco08_3d region_2dw degurba sex countryb homework ftpt stapro temp hatlevel hhcomp age /* and now some derived variables */ isco1d isco2d nace1d reg regw reglab ctryw

label variable reg "Region of residence, NUTs code"
label variable regw "Region of work, NUTs code"
label variable reglab "Region of residence, labelled"
label variable ctryw "Country of work categorisation"

save "R:\SpssStata\EJM_DO_NOT_DELETE\DATA\LFS_For_Researchers_1983to2021\LFSreg2018_21r.dta", replace


//use  "R:\SpssStata\EJM_DO_NOT_DELETE\DATA\LFS_For_Researchers_1983to2021\LFSreg2018_21r.dta", clear
use  "C:\data\LFSreg2018_21r.dta", clear
sort reg

// merge with NUTs_urban rural typology file to include this as a region type in our main dataset

cd "R:\SpssStata\EJM_DO_NOT_DELETE\annualreports\REGIONAL 2023\data\NUTS_urbanrural\"

merge reg using urbrurmerge2.dta

replace urbrur=3 if reg=="HR02" | reg=="HR06"
replace urbrur=0 if reg=="HR05"

save "C:\data\LFSreg2018_21final.dta", replace
save "R:\SpssStata\EJM_DO_NOT_DELETE\DATA\LFS_For_Researchers_1983to2021\LFSreg2018_21final.dta", replace

clear