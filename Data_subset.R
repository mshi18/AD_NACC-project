library(dplyr)
library(ggplot2)
library(readr)
library(xlsx)
Shi05082017_NACC <- read_csv("C:/Users/mingj/Desktop/MSDS/Research Method/Shi05082017-NACC.csv")
View(Shi05082017_NACC)
str(Shi05082017_NACC)
# 47673 obs. of  193 variables:


  
Data_Select <- select(Shi05082017_NACC,NACCADC,NACCID, BIRTHYR,SEX,EDUC,INDEPEND,RESIDENC,MARISTAT,HEIGHT,WEIGHT,NACCBMI,BPSYS,
                      BPDIAS,HRATE,VISION,VISCORR,VISWCORR,HEARING,HEARAID,HEARWAID,TOBAC30,TOBAC100,SMOKYRS,PACKSPER,QUITSMOK,ALCOCCAS,ALCFREQ,CVHATT,HATTMULT,
                      CBSTROKE,STROKMUL,SEIZURES,DIABETES,DIABTYPE,HYPERTEN,HYPERCHO,B12DEF,THYROID,ARTHRIT,INSOMN,ALCOHOL,ABUSOTHR,BIPOLAR,SCHIZ,DEP2YRS,DEPOTHR,
                      ANXIETY,NACCAPOE,NACCNE4S,NPTHAL,NACCUDSD)
View(Data_Select)
write.table(Data_Select, "C:/Users/mingj/Documents/MSDS")

Data_Subset <- select (NACC,NACCADC,NACCID,VISITYR,BIRTHYR,SEX,EDUC,INDEPEND,RESIDENC,MARISTAT,HEIGHT,WEIGHT,NACCBMI,BPSYS,BPDIAS,HRATE,VISION,VISCORR,VISWCORR,HEARING,HEARAID,HEARWAID,TOBAC30,TOBAC100,SMOKYRS,PACKSPER,QUITSMOK,ALCOCCAS,ALCFREQ,CVHATT,HATTMULT,CBSTROKE,STROKMUL,SEIZURES,DIABETES,DIABTYPE,HYPERTEN,HYPERCHO,B12DEF,THYROID,ARTHRIT,INSOMN,ALCOHOL,ABUSOTHR,BIPOLAR,SCHIZ,DEP2YRS,DEPOTHR,ANXIETY,NACCAPOE,NACCNE4S,NPTHAL,NACCUDSD)

# Adding Age variable calculated from VISITYR and BIRTHYR variables
Data_Subset <- mutate(Data_Subset, Age = VISITYR - BIRTHYR)

# Change 'SEX' numeric variable to be a factor
Data_Subset$SEX <- factor(Data_Subset$SEX, levels = c(1, 2), labels = c("Male", "Female"))

# Cleaning EDUC variable. 
# Higher educational level: doctorate = 20 to 25. Excludes records with higher values
Data_Subset <- filter(Data_Subset, Data_Subset$EDUC <= 25)

# Change 'EDUC' numeric variable to be a factor
Data_Subset$EDUC <- cut(Data_Subset$EDUC, breaks=c(0, 11, 15, 17, 19, 25), labels=c("Below high school or GRE", "high school or GRE", "bachelor's degree", "master's degree", "doctorate"))


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

