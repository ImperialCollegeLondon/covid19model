# No need to run this file; it is just provided for sake of giving an idea to people
# what was done in paper in terms of pre-processing
library(tidyr)
library(dplyr)
library(readr)
library(janitor)
library(lubridate)
library(magrittr)
library(ggpubr)
library(tidyr)
library(readxl)
library(tools)
library(ggplot2)
library(flextable)
library(stringr)
library(here)

# Import .rds
DATA <- read.csv(file = here('tiers/data/npis_23Mar_02Dec.csv'))
DATA$date = as.Date(DATA$date, format = '%d/%m/%Y')

Earliest = TRUE # if true, assessing which interventions consitute tier X based on earliest date that Tier X introduced. If false, then based on latest date that Tier X applies.
#Earliest = FALSE # if true, assessing which interventions consitute tier X based on earliest date that Tier X introduced. If false, then based on latest date that Tier X applies.

Interventions = c(
		
		"limited_to_groups_of_6_indoors"                 ,                                              
		"limited_to_groups_of_6_outdoors"                ,                                              
		"curfew_of_10pm_for_hospitality_venues"          ,                                              
		"instruction_to_work_from_home_where_possible"   ,                                              
		"travel_discouraged"                             ,                                              
		"no_indoor_mixing"                               ,                                               
		"overnight_stays_discouraged"                    ,                                               
		"residents_cannot_leave_the_local_area"          ,                                               
		"non-essential_retail_closures"                  ,                                               
		"schools_closed"                                 ,                                               
		"places_of_worship_closed"                       ,                                               
		"weddings_not_allowed"                           ,                                               
		"organised_sport_not_allowed"                    ,                                               
		"tourist_attractions_closed"                     ,                                               
		"gyms_closed"                                    ,                                               
		"public_buildings_closed"                        ,                                               
		"personal_care_contact_services_closed"          ,                                               
		"arts_venues_closed"                             ,                                               
		"sit-down_hospitality_closed_takeaway_only"      ,                                               
		"pubs_and_bars_closed_table_service_only"        ,                                               
		"essential_travel_only"                                                                          
)

# tibbles (apparently) better so re-write below you become familiar 
DATA = as.data.frame(DATA)
str(DATA$date)

# how many ltlas?
N_LTLAs = length(unique(DATA$ltla))

## Define nested tiers
DATA$tier_3_nested 	= DATA$tier_3
DATA$tier_2_nested 	= DATA$tier_2 + DATA$tier_3
DATA$tier_1_nested 	= DATA$tier_1 + DATA$tier_2 + DATA$tier_3
DATA$Tier 			= DATA$tier_1_nested + DATA$tier_2_nested + DATA$tier_3_nested

# Subset to post 11th September
DATA_postSept = DATA[DATA$date >= as.Date("2020-09-11"),]

# check defs
#TierDF = data.frame(date = DATA_postSept$date, TierCat = DATA_postSept$Tier, 
#		tier_1 = DATA_postSept$tier_1, tier_2 = DATA_postSept$tier_2, tier_3 = DATA_postSept$tier_3, 
#		tier_1_nested = DATA_postSept$tier_1_nested, tier_2_nested = DATA_postSept$tier_2_nested, tier_3_nested = DATA_postSept$tier_3_nested)
## Statements below should be false. 
#all(DATA$tier_1[which(DATA$tier_2 == 1)] == 1)
#all(DATA$tier_1[which(DATA$tier_3 == 1)] == 1)
#all(DATA$tier_2[which(DATA$tier_3 == 1)] == 1)
## Statements below should be true
#all(DATA$tier_1_nested[which(DATA$tier_2_nested == 1)] == 1)
#all(DATA$tier_1_nested[which(DATA$tier_3_nested == 1)] == 1)
#all(DATA$tier_2_nested[which(DATA$tier_3_nested == 1)] == 1)


# which interventions are included in Tiers 1, 2, and 3, at first date Tier introduced? i.e. for each ltla, which interventions are on when tier 1 is on, when tier 2 is on etc. 
Nested_Tier = 1
ltla_index 	= 2

Tier_List = list()
Tier_List[[1]] = matrix (0, nrow = length(Interventions), ncol = N_LTLAs)
Tier_List[[2]] = matrix (0, nrow = length(Interventions), ncol = N_LTLAs)
Tier_List[[3]] = matrix (0, nrow = length(Interventions), ncol = N_LTLAs)

for (Nested_Tier in 1:3) colnames(Tier_List[[Nested_Tier]]) = unique(DATA_postSept$ltla)

Interventions_ColNames = gsub("-", ".", Interventions) ## account for hypens in colnames.
#all(Interventions %in% colnames(DATA))
#all(Interventions_ColNames %in% colnames(DATA))

for (ltla_index in 1:N_LTLAs)
{
	# subset to this LTLA
	DATA_ltla = DATA_postSept[DATA_postSept$ltla == unique(DATA_postSept$ltla)[ltla_index], ]
	
	for (Nested_Tier in 1:3)
		if (any(DATA_ltla$Tier >= Nested_Tier)) # if this ltla was ever in this nested tier
		{
			EarliestDate 	= min(DATA_ltla$date[DATA_ltla$Tier >= Nested_Tier])
			LatestDate 		= max(DATA_ltla$date[DATA_ltla$Tier >= Nested_Tier])
			
			if (Earliest) Date_dummy = EarliestDate else Date_dummy = LatestDate
			
			# which interventions turned on at earliest/latest date? Add these to matrix
			Tier_List[[Nested_Tier]][which(DATA_ltla[DATA_ltla$date == Date_dummy, Interventions_ColNames] == 1), ltla_index] = 1
		}
}


#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#### Make plots of interventions by region and tier. 
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

dir.create(here("tiers/figures/"), showWarnings = FALSE)

AllPlotsInSameFigure = TRUE
#AllPlotsInSameFigure = FALSE

if (Earliest) Suffix = "_Earliest" else Suffix = "_Latest"

if (AllPlotsInSameFigure) 
{
	pdf(here(paste0("tiers/figures/IntsByRegion_Tiers_123", Suffix, ".pdf")), height = 10, width = 24)
	par(mfrow = c(1,3), mar = c(10.1, 25.1, 4.1, 2.1))
	CEXLAB = 2; 	CEX_Text = 1.75; 	CEXMAIN = 2.5; 	CEXAXIS = 1.75

} else	{	CEXLAB = 1.25; 	CEX_Text = 1; 	CEXMAIN = 1; 	CEXAXIS = 1	}

if (AllPlotsInSameFigure)  Alpha = 1 else Alpha = 0.6

COLS 	= c("green", "orange", "red")
adjCOLS = adjustcolor(c("green", "orange", "red"), alpha.f = Alpha)

labs <- gsub("_", " ", Interventions)


# Tier 1 only
if (!AllPlotsInSameFigure) 
{
	png(here(paste0("tiers/figures/IntsByRegion_Tier_1", Suffix, ".png")), units = "in", res = 200, height = 10, width = 7.5)
	par(mar = c(8.1, 13.1, 4.1, 2.1))	
	LetterChar = ""
} else LetterChar = "(A) "
x <- barplot(rowSums(Tier_List[[1]]), horiz = TRUE, col = adjCOLS[1], border = NA, cex.main = CEXMAIN, cex.axis = CEXAXIS,
		main = paste0(LetterChar, "Interventions by region (Tier 1)"), xlab = "Num LTLAs where intervention applied", cex.lab = CEXLAB)
text(x = -7, y = x, labs, xpd = TRUE, srt = 45, adj = 1, cex = CEX_Text)
if (!AllPlotsInSameFigure) dev.off()

# Tier 2 only
if (!AllPlotsInSameFigure) 
{
	png(here(paste0("tiers/figures/IntsByRegion_Tier_2", Suffix, ".png")), units = "in", res = 200, height = 10, width = 7.5)
	par(mar = c(8.1, 13.1, 4.1, 2.1))
	LetterChar = ""
} else LetterChar = "(B) "
x <- barplot(rowSums(Tier_List[[2]]), horiz = TRUE, col = adjCOLS[2], border = NA, cex.main = CEXMAIN,cex.axis = CEXAXIS,
		main = paste0(LetterChar, "Interventions by region (Tier 2)"), xlab = "Num LTLAs where intervention applied", cex.lab = CEXLAB)
text(x = -7, y = x, labs, xpd = TRUE, srt = 45, adj = 1, cex = CEX_Text)
if (!AllPlotsInSameFigure) dev.off()

# Tier 3 only
if (!AllPlotsInSameFigure) 
{
	png(here(paste0("tiers/figures/IntsByRegion_Tier_3", Suffix, ".png")), units = "in", res = 200, height = 10, width = 7.5)
	par(mar = c(8.1, 13.1, 4.1, 2.1))
	LetterChar = ""
} else LetterChar = "(C) "
x <- barplot(rowSums(Tier_List[[3]]), horiz = TRUE, col = adjCOLS[3], border = NA, cex.main = CEXMAIN,cex.axis = CEXAXIS,
		main = paste0(LetterChar, "Interventions by region (Tier 3)"), xlab = "Num LTLAs where intervention applied", cex.lab = CEXLAB)
text(x = -1, y = x, labs, xpd = TRUE, srt = 45, adj = 1, cex = CEX_Text)
if (!AllPlotsInSameFigure) dev.off()

if (AllPlotsInSameFigure) dev.off()




#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#### With above plots, define which Ints comprise Tiers 1, 2, and 3. Distinction clearest when using Earliest date. 
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

Tier_1_ints = c(
		"limited_to_groups_of_6_indoors"                ,   
		"limited_to_groups_of_6_outdoors"               ,   
		"curfew_of_10pm_for_hospitality_venues"         ,   
		"instruction_to_work_from_home_where_possible"  )
Tier_2_ints = c(
		"limited_to_groups_of_6_indoors"                ,   
		"limited_to_groups_of_6_outdoors"               ,   
		"curfew_of_10pm_for_hospitality_venues"         ,   
		"instruction_to_work_from_home_where_possible"  ,   
		"travel_discouraged"                            ,   
		"no_indoor_mixing"								,  
		"overnight_stays_discouraged"                   )
Tier_3_ints = c(
		"limited_to_groups_of_6_indoors"                ,   
		"limited_to_groups_of_6_outdoors"               ,   
		"curfew_of_10pm_for_hospitality_venues"         ,   
		"instruction_to_work_from_home_where_possible"  ,   
		"travel_discouraged"                            ,   
		"no_indoor_mixing"								,  
		"overnight_stays_discouraged"                   ,
		"residents_cannot_leave_the_local_area"         ,                                               
		"pubs_and_bars_closed_table_service_only"       )                                               


#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#### back-date tiers in full dataset 
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

DATA$tier_1_BD_nested = rep(0, dim(DATA)[1])
DATA$tier_2_BD_nested = rep(0, dim(DATA)[1])
DATA$tier_3_BD_nested = rep(0, dim(DATA)[1])

for (row in 1:dim(DATA)[1])
{
	if (all(DATA[row, Tier_1_ints] == 1)) DATA$tier_1_BD_nested[row] = 1
	if (all(DATA[row, Tier_2_ints] == 1)) DATA$tier_2_BD_nested[row] = 1
	if (all(DATA[row, Tier_3_ints] == 1)) DATA$tier_3_BD_nested[row] = 1
}
DATA$Tier_BD = DATA$tier_1_BD_nested + DATA$tier_2_BD_nested + DATA$tier_3_BD_nested

# the above are nested defintiions. Make non-nested version for downstream flexibility
DATA$tier_1_BD = DATA$tier_1_BD_nested
DATA$tier_2_BD = DATA$tier_2_BD_nested
DATA$tier_3_BD = DATA$tier_3_BD_nested

DATA$tier_1_BD[which(DATA$tier_2_BD_nested == 1)] = 0 
DATA$tier_1_BD[which(DATA$tier_3_BD_nested == 1)] = 0 
DATA$tier_2_BD[which(DATA$tier_3_BD_nested == 1)] = 0 

DATA$date <- format(DATA$date, "%d/%m/%Y") 
write.csv(DATA, file = here("tiers/data/npis_23Mar_02Dec.csv"),row.names = FALSE)

