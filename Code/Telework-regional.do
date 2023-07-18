use "LFS_regressions.dta"

// Includes both employees and self-employed
tab country stapro

// new variable work_location, based on comparing region of residence and region (or country) of work
tab country work_location

// Build regression specification incrementally,

// 1) start from occupation-level indicators (teleworkability)
reg homework_any physicalinteraction socialinteraction [pw=coeffy]
eststo model1

// Compare this to controlling for occupation itself
reg homework_any i.isco08_3d [pw=coeffy]

// 2) Add individual-level characteristics (sex, age, interaction, self-employment)
reg homework_any physicalinteraction socialinteraction i.stapro i.sex##age [pw=coeffy]
eststo model2

// 3) Add country-level fixed effects and territorial typology (degurba, measured at LAU level)
reg homework_any physicalinteraction socialinteraction i.stapro i.sex##age i.country i.degurba i.work_location i.urbrur [pw=coeffy]
eststo model3

// Now, to see how these things have changed over time. 
// Out main contention is that 
// 4) Add country-level fixed effects and territorial typology (degurba, measured at LAU level)
reg homework_any physicalinteraction socialinteraction i.stapro i.sex##age i.country i.degurba  i.work_location i.year [pw=coeffy]
eststo model4

esttab

// Consider homework_index as one possible dependent variable
// (3 values: Never = 0, Sometimes = 0.25, usually = 0.75)
tab homework homework_index

