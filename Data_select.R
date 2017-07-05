# Setting Working environment
setwd('/Users/Melchizedek_Levitz/Desktop/PRACTICUM/AD_NACC-project/')

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

# Change 'PACKSPER' numeric variable to be a factor
Data_Subset$PACKSPER[Data_Subset$PACKSPER == 9] <- "Unknown"
Data_Subset$PACKSPER[Data_Subset$PACKSPER == -4] <- "Unknown"
Data_Subset$PACKSPER <- factor(Data_Subset$PACKSPER, levels = c(0, 1, 2, 3, 4, 5, 8, "Unknown"), labels = c("No reported cigarette use", "1 cigarette to less than 1/2 pack", "½ pack to less than 1 pack", "1 pack to 1½ packs", "1½ packs to 2 packs", "More than two packs", "NA", "Unknown"))

# QUITSMOK
Data_Subset$QUITSMOK[Data_Subset$QUITSMOK == 888] <- "No significant smoking history"
Data_Subset$QUITSMOK[Data_Subset$QUITSMOK == 999] <- "Unknown"
Data_Subset$QUITSMOK[Data_Subset$QUITSMOK == -4] <- "Unknown"


## Anani's Section
# Change 'ALCOHOL' numeric variable to be a factor, and replace invalid entries to 'NA'
Data_Subset_ALCOHOL <- factor(Data_Subset$ALCOHOL,levels=c(0:2),labels=c("Absent","Active","Inactive"))
Data_Subset_ALCOHOL[Data_Subset$ALCOHOL == 9] <- NA
Data_Subset_ALCOHOL[Data_Subset$ALCOHOL == -4] <- NA
stripChart <- stripchart(Data_Subset$ALCOHOL)
summary(Data_Subset_ALCOHOL)
View(Data_Subset_ALCOHOL)
help("stripchart")
hist(stripchart(Data_Subset$ALCOHOL), col = 'turquoise')
plot(stripChart, col = 'turquoise')
stripchart(Data_Subset$ALCOHOL, method = "jitter", add = TRUE, at = 2, col = 'turquoise')
corrplot(cor(Data_Subset[,1:6]), method="ellipse")

# Change 'NACCUDSD' numeric variable to be a factor, and replace invalid entries to 'NA'
Data_Subset_NACCUDSD <- factor(Data_Subset$NACCUDSD,levels=c(1:4),labels=c("Normal","Impaired","MCI","Dementia"))

# Change 'CVHATT' numeric variable to be a factor, and replace invalid entries to 'NA'
Data_Subset_CVHATT <- factor(Data_Subset$CVHATT,levels=c(0:2),labels=c("Absent","Active","Inactive"))
Data_Subset_CVHATT[Data_Subset$CVHATT == 9] <- NA
Data_Subset_CVHATT[Data_Subset$CVHATT == -4] <- NA

# Change 'DIABETES' numeric variable to be a factor, and replace invalid entries to 'NA'
Data_Subset_DIABETES <- factor(Data_Subset$DIABETES,levels=c(0:2),labels=c("Absent","Active","Inactive"))
Data_Subset_DIABETES[Data_Subset$DIABETES == 9] <- NA
Data_Subset_DIABETES[Data_Subset$DIABETES == -4] <- NA

# Change 'CBSTROKE' numeric variable to be a factor, and replace invalid entries to 'NA'
Data_Subset_CBSTROKE <- factor(Data_Subset$CBSTROKE,levels=c(0:2),labels=c("Absent","Active","Inactive"))
Data_Subset_CBSTROKE[Data_Subset$CBSTROKE == 9] <- NA
Data_Subset_CBSTROKE[Data_Subset$CBSTROKE == -4] <- NA
View(Data_Subset_CBSTROKE)
