# Setting Working environment
setwd('\Documents\Script')

# Loading libraries
library(dplyr)
library(ggplot2)
library(readr)
library(rmarkdown)

# Loading NACC data set 
NACC <- read.csv(file = "C:/Users/mingj/Desktop/MSDS/Research Method/Shi05082017-NACC.csv",
                 header = TRUE,
                 stringsAsFactors = FALSE)
# New data set from Variable selection
Data_Subset <- select (NACC,NACCADC,VISITYR,NACCID,BIRTHYR,SEX,EDUC,NACCBMI,TOBAC30,TOBAC100,SMOKYRS,PACKSPER,QUITSMOK,ALCOHOL,NACCUDSD,CVHATT,DIABETES,CBSTROKE,HYPERTEN,DEP2YRS,NACCAPOE,NACCNE4S,NPTHAL)


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

# Change 'ALCOHOL' numeric variable to be a factor, and replace invalid entries to 'NA'
Data_Subset$ALCOHOL <- factor(Data_Subset$ALCOHOL,levels=c(0:2),labels=c("Absent","Active","Inactive"))
Data_Subset$ALCOHOL[Data_Subset$ALCOHOL == 9] <- NA
Data_Subset$ALCOHOL[Data_Subset$ALCOHOL == -4] <- NA

# Change 'NACCUDSD' numeric variable to be a factor, and replace invalid entries to 'NA'
Data_Subset$NACCUDSD <- factor(Data_Subset$NACCUDSD,levels=c(1:4),labels=c("Normal","Impaired","MCI","Dementia"))

# Change 'CVHATT' numeric variable to be a factor, and replace invalid entries to 'NA'
Data_Subset$CVHATT <- factor(Data_Subset$CVHATT,levels=c(0:2),labels=c("Absent","Active","Inactive"))
Data_Subset$CVHATT[Data_Subset$CVHATT == 9] <- NA
Data_Subset$CVHATT[Data_Subset$CVHATT == -4] <- NA

# Change 'DIABETES' numeric variable to be a factor, and replace invalid entries to 'NA'
Data_Subset$DIABETES <- factor(Data_Subset$DIABETES,levels=c(0:2),labels=c("Absent","Active","Inactive"))
Data_Subset$DIABETES[Data_Subset$DIABETES == 9] <- NA
Data_Subset$DIABETES[Data_Subset$DIABETES == -4] <- NA

# Change 'CBSTROKE' numeric variable to be a factor, and replace invalid entries to 'NA'
Data_Subset$CBSTROKE <- factor(Data_Subset$CBSTROKE,levels=c(0:2),labels=c("Absent","Active","Inactive"))
Data_Subset$CBSTROKE[Data_Subset$CBSTROKE == 9] <- NA
Data_Subset$CBSTROKE[Data_Subset$CBSTROKE == -4] <- NA

Data_Subset$HYPERTEN <- factor(Data_Subset$HYPERTEN,levels=c(0,1,2),labels=c("Absent","Recent/Active","Remote/Inactive"))
Data_Subset$HYPERTEN[Data_Subset$HYPERTEN == 9] <- NA
Data_Subset$HYPERTEN[Data_Subset$HYPERTEN == -4] <- NA                                                                  

Data_Subset$DEP2YRS <- factor(Data_Subset$DEP2YRS,levels=c(0,1),labels=c("NO","YES"))
Data_Subset$DEP2YRS[Data_Subset$DEP2YRS == 9] <- NA
Data_Subset$DEP2YRS[Data_Subset$DEP2YRS == -4] <- NA

Data_Subset$NACCAPOE <- factor(Data_Subset$NACCAPOE, levels=c(1,2,3,4,5,6),labels=c("e3e3","e3e4","e3e2","e4e4","e4e2","e2e2"))
Data_Subset$NACCAPOE[Data_Subset$NACCAPOE == 9] <- NA
Data_Subset$NACCAPOE[Data_Subset$NACCAPOE == -4] <- NA

Data_Subset$NACCNE4S <- factor(Data_Subset$NACCNE4S,levels=c(0,1,2),labels=c("No e4 allele","1 copy of e4 allele","2 copies of e4 allele"))
Data_Subset$NACCNE4S[Data_Subset$NACCNE4S == 9] <- NA

Data_Subset$NPTHAL <- factor(Data_Subset$NPTHAL,levels=c(0,1,2,3,4,5),labels=c("Phase0","Phase1","Phase2","Phase3","Phase4","Phase5"))
Data_Subset$NPTHAL[Data_Subset$NPTHAL == 8] <- NA
Data_Subset$NPTHAL[Data_Subset$NPTHAL == 9] <- NA
Data_Subset$NPTHAL[Data_Subset$NPTHAL == -4] <- NA

View(Data_Subset)
write.csv(Data_Subset, file = "Data_Subset.csv")
summary(Data_Subset)

hist(Data_Subset$AGE)
hist(Data_Subset$NACCBMI)
hist(Data_Subset$EDUC)
boxplot(NACCBMI~NACCUDSD,data=Data_Subset, xlab="NACCUDSD", ylab="NACCBMI")
boxplot(AGE~NACCUDSD,data=Data_Subset, xlab="NACCUDSD", ylab="AGE")
boxplot(Data_Subset$SEX~NACCUDSD,data=Data_Subset, xlab="NACCUDSD", ylab="SEX")
boxplot(EDUC~NACCUDSD,data=Data_Subset, xlab="NACCUDSD", ylab="EDUC")
