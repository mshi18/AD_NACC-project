# Setting Working environment
setwd('\Documents\Script')

# Loading libraries
library(dplyr)
library(ggplot2)
library(readr)
library(rmarkdown)

# Loading NACC data set 
NACC <- read.csv(file = 'NACC.csv',
                 header = TRUE,
                 stringsAsFactors = FALSE)

# New data set from Variable selection
Data_Subset <- select (NACC,NACCADC,NACCID,VISITYR,BIRTHYR,SEX,EDUC,INDEPEND,RESIDENC,MARISTAT,HEIGHT,WEIGHT,NACCBMI,BPSYS,BPDIAS,HRATE,VISION,VISCORR,VISWCORR,HEARING,HEARAID,HEARWAID,TOBAC30,TOBAC100,SMOKYRS,PACKSPER,QUITSMOK,ALCOCCAS,ALCFREQ,CVHATT,HATTMULT,CBSTROKE,STROKMUL,SEIZURES,DIABETES,DIABTYPE,HYPERTEN,HYPERCHO,B12DEF,THYROID,ARTHRIT,INSOMN,ALCOHOL,ABUSOTHR,BIPOLAR,SCHIZ,DEP2YRS,DEPOTHR,ANXIETY,NACCAPOE,NACCNE4S,NPTHAL,NACCUDSD)

# Adding 'AGE' variable calculated from VISITYR and BIRTHYR variables
Data_Subset <- mutate(Data_Subset, AGE = VISITYR - BIRTHYR)

# Change 'SEX' numeric variable to be a factor
Data_Subset$SEX <- factor(Data_Subset$SEX, levels = c(1, 2), labels = c("Male", "Female"))

# Cleaning 'EDUC' variable. 
# Higher educational level: doctorate = 20 to 25. Excludes records with higher values
Data_Subset <- filter(Data_Subset, Data_Subset$EDUC <= 25)

# Change 'EDUC' numeric variable to be a factor
Data_Subset$EDUC <- cut(Data_Subset$EDUC, breaks=c(0, 11, 15, 17, 19, 25), labels=c("Below high school or GRE", "high school or GRE", "bachelor’s degree", "master's degree", "doctorate"))

# Cleaning 'NACCBMI' variable.
Data_Subset <- filter(Data_Subset, Data_Subset$NACCBMI != "888.8", Data_Subset$NACCBMI != "-4" )

# Change 'NACCBMI' numeric variable to be a factor

# In 'TOBAC30' factor variable, replace values "9" and "-4" to "Unknown"
Data_Subset$TOBAC30[Data_Subset$TOBAC30 == 9] <- "Unknown"
Data_Subset$TOBAC30[Data_Subset$TOBAC30 == -4] <- "Unknown"

# Change 'TOBAC30' numeric variable to be a factor
Data_Subset$TOBAC30 <- factor(Data_Subset$TOBAC30, levels = c(0, 1, "Unknown"), labels = c("No", "Yes", "Unknown"))

# In 'TOBAC100' factor variable, replace values "9" and "-4" to "Unknown"
Data_Subset$TOBAC100[Data_Subset$TOBAC100 == 9] <- "Unknown"
Data_Subset$TOBAC100[Data_Subset$TOBAC100 == -4] <- "Unknown"

# Change 'TOBAC100' numeric variable to be a factor
Data_Subset$TOBAC100 <- factor(Data_Subset$TOBAC100, levels = c(0, 1, "Unknown"), labels = c("No", "Yes", "Unknown"))

# In 'SMOKYRS' numeric variable, replace values "88" to "NA", and "99" and "-4" to "Unknown"
Data_Subset$SMOKYRS[Data_Subset$SMOKYRS == 88] <- "NA"
Data_Subset$SMOKYRS[Data_Subset$SMOKYRS == 99] <- "Unknown"
Data_Subset$SMOKYRS[Data_Subset$SMOKYRS == -4] <- "Unknown"

