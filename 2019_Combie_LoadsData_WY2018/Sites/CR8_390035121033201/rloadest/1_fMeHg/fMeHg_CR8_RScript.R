#fMeHg CR8 script for retrieving loads models stats (the stats are used to select the best model for this constituent at this site) and loads predictions.

library(akima)
library(dataRetrieval)
library(digest)
library(leaps)
library(lubridate)
library(memoise)
library(rloadest)
library(smwrBase)
library(smwrData)
library(smwrGraphs)
library(smwrQW)
library(smwrStats)
library(boot)
library(KernSmooth)
library(lattice)

fMeHg_CR8<-importRDB("fMeHg_CR8_R.txt")
CR8Q<-importRDB("CR8_QR.txt")

#These data frames are created by the function importRDB. 
#The calls above bring the constituent data and the daily flow data into the script.

fMeHg_CR8m1 <- loadReg(fMeHg ~model(1), data = fMeHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
fMeHg_CR8m1
fMeHg_CR8m2 <- loadReg(fMeHg ~model(2), data = fMeHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
fMeHg_CR8m2
fMeHg_CR8m3 <- loadReg(fMeHg ~model(3), data = fMeHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
fMeHg_CR8m3
fMeHg_CR8m4 <- loadReg(fMeHg ~model(4), data = fMeHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
fMeHg_CR8m4
fMeHg_CR8m5 <- loadReg(fMeHg ~model(5), data = fMeHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
fMeHg_CR8m5
fMeHg_CR8m6 <- loadReg(fMeHg ~model(6), data = fMeHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
fMeHg_CR8m6
fMeHg_CR8m7 <- loadReg(fMeHg ~model(7), data = fMeHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
fMeHg_CR8m7
fMeHg_CR8m8 <- loadReg(fMeHg ~model(8), data = fMeHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
fMeHg_CR8m8
fMeHg_CR8m9 <- loadReg(fMeHg ~model(9), data = fMeHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
fMeHg_CR8m9

#These objects of class "loadReg" are created.
#A list in R allows you to gather a variety of objects under one name (that is, the name of the list) in an ordered way. 
#These objects can be matrices, vectors, data frames, even other lists, etc. It is not even required that these objects are related to each other in any way.
#When the models are run (m1-m9), the output will be in the console. These are the stats used to select the best model. 

#print(fMeHg_CR8m1,brief = FALSE, load.only = FALSE)
#print(fMeHg_CR8m2,brief = FALSE, load.only = FALSE)
#print(fMeHg_CR8m3,brief = FALSE, load.only = FALSE)
#print(fMeHg_CR8m4,brief = FALSE, load.only = FALSE)
#print(fMeHg_CR8m5,brief = FALSE, load.only = FALSE)
#print(fMeHg_CR8m6,brief = FALSE, load.only = FALSE)
#print(fMeHg_CR8m7,brief = FALSE, load.only = FALSE)
#print(fMeHg_CR8m8,brief = FALSE, load.only = FALSE)
#print(fMeHg_CR8m9,brief = FALSE, load.only = FALSE)
#Commenting these out. These provide some explanations of the data in a longer form. Brief results are printed to console (fMeHg_CR8m1-9)

plot(fMeHg_CR8m1,ann=FALSE)
title(main = "390035121033201_m1_fMeHg Response vs Fitted Values",xlab = "Fitted Values",ylab = "Response Values")
plot(fMeHg_CR8m1,which = 2,set.up = F)
title(main = "390035121033201_m1_fMeHg Residuals vs Fitted Values")
plot(fMeHg_CR8m1,which = 3,set.up = F)
title(main = "390035121033201_m1_fMeHg Assessing Heteroscedasticity") 
plot(fMeHg_CR8m1,which = 4,set.up = F)
title(main = "390035121033201_m1_fMeHg Correlogram of Samples")
plot(fMeHg_CR8m1,which = 5,set.up = F)
title(main="390035121033201_m1_fMeHg Normal Discharge")
plot(fMeHg_CR8m1,which = 6,set.up = F)
title(main="390035121033201_m1_fMeHg Box Plot of Loads")

#These functions plot the data using the chosen best model and add a title and labels to the plot.

fMeHg_CR8_load<-predLoad(fMeHg_CR8m1,CR8Q,load.units="kg",by="water year",allow.incomplete = TRUE,conf.int = 0.95,print = TRUE)
write.csv(fMeHg_CR8_load,"1_CR8_m1_fMeHg_Flux_Annual.csv")
fMeHg_CR8_load_day<-predLoad(fMeHg_CR8m1, CR8Q,load.units = "kg",by="day",allow.incomplete = TRUE,conf.int = 0.90,print = TRUE)
write.csv(fMeHg_CR8_load_day,"1_CR8_m1_fMeHg_Flux_Daily.csv")

#Lines 75 and 77 create data frames that use the function predLoad. 
#Description of predLoad: Estimate loads from a rating-curve model from loadReg for a new data frame, aggregating the loads by specified time periods.
#Lines 76 and 78 write the data frames to a .csv file. Important note: The file must be intentionally saved as a CSV file- type ".csv" at the end when prompted.
#file.choose() lets the user select the location for the .csv files.