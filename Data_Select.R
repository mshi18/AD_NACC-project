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

# Adding Age variable calculated from VISITYR and BIRTHYR variables
Data_Subset <- mutate(Data_Subset, Age = VISITYR - BIRTHYR)

# Change 'SEX' numeric variable to be a factor
Data_Subset$SEX <- factor(Data_Subset$SEX, levels = c(0, 1), labels = c("Female", "Male"))