#wwTHg CR8 script for retrieving loads models stats (the stats are used to select the best model for this constituent at this site) and loads predictions.

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

wwTHg_CR8<-importRDB("wwTHg_CR8_R.txt")
CR8Q<-importRDB("CR8_QR.txt")

#These data frames are created by the function importRDB. 
#The calls above bring the constituent data and the daily flow data into the script.

wwTHg_CR8m1 <- loadReg(wwTHg ~model(1), data = wwTHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
wwTHg_CR8m1
wwTHg_CR8m2 <- loadReg(wwTHg ~model(2), data = wwTHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
wwTHg_CR8m2
wwTHg_CR8m3 <- loadReg(wwTHg ~model(3), data = wwTHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
wwTHg_CR8m3
wwTHg_CR8m4 <- loadReg(wwTHg ~model(4), data = wwTHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
wwTHg_CR8m4
wwTHg_CR8m5 <- loadReg(wwTHg ~model(5), data = wwTHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
wwTHg_CR8m5
wwTHg_CR8m6 <- loadReg(wwTHg ~model(6), data = wwTHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
wwTHg_CR8m6
wwTHg_CR8m7 <- loadReg(wwTHg ~model(7), data = wwTHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
wwTHg_CR8m7
wwTHg_CR8m8 <- loadReg(wwTHg ~model(8), data = wwTHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
wwTHg_CR8m8
wwTHg_CR8m9 <- loadReg(wwTHg ~model(9), data = wwTHg_CR8, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-Out")
wwTHg_CR8m9

#These objects of class "loadReg" are created.
#A list in R allows you to gather a variety of objects under one name (that is, the name of the list) in an ordered way. 
#These objects can be matrices, vectors, data frames, even other lists, etc. It is not even required that these objects are related to each other in any way.
#When the models are run (m1-m9), the output will be in the console. These are the stats used to select the best model. 

#print(wwTHg_CR8m1,brief = FALSE, load.only = FALSE)
#print(wwTHg_CR8m2,brief = FALSE, load.only = FALSE)
#print(wwTHg_CR8m3,brief = FALSE, load.only = FALSE)
#print(wwTHg_CR8m4,brief = FALSE, load.only = FALSE)
#print(wwTHg_CR8m5,brief = FALSE, load.only = FALSE)
#print(wwTHg_CR8m6,brief = FALSE, load.only = FALSE)
#print(wwTHg_CR8m7,brief = FALSE, load.only = FALSE)
#print(wwTHg_CR8m8,brief = FALSE, load.only = FALSE)
#print(wwTHg_CR8m9,brief = FALSE, load.only = FALSE)
#Commenting these out. These provide some explanations of the data in a longer form. Brief results are printed to console (wwTHg_CR8m1-9)

plot(wwTHg_CR8m1,ann=FALSE)
title(main = "390035121033201_m1_wwTHg Response vs Fitted Values",xlab = "Fitted Values",ylab = "Response Values")
plot(wwTHg_CR8m1,which = 2,set.up = F)
title(main = "390035121033201_m1_wwTHg Residuals vs Fitted Values")
plot(wwTHg_CR8m1,which = 3,set.up = F)
title(main = "390035121033201_m1_wwTHg Assessing Heteroscedasticity") 
plot(wwTHg_CR8m1,which = 4,set.up = F)
title(main = "390035121033201_m1_wwTHg Correlogram of Samples")
plot(wwTHg_CR8m1,which = 5,set.up = F)
title(main="390035121033201_m1_wwTHg Normal Discharge")
plot(wwTHg_CR8m1,which = 6,set.up = F)
title(main="390035121033201_m1_wwTHg Box Plot of Loads")

#These functions plot the data using the chosen best model and add a title and labels to the plot.

wwTHg_CR8_load<-predLoad(wwTHg_CR8m1,CR8Q,load.units="kg",by="water year",allow.incomplete = TRUE,conf.int = 0.95,print = TRUE)
write.csv(wwTHg_CR8_load,"1_CR8_m1_wwTHg_Flux_Annual.csv")
wwTHg_CR8_load_day<-predLoad(wwTHg_CR8m1, CR8Q,load.units = "kg",by="day",allow.incomplete = TRUE,conf.int = 0.90,print = TRUE)
write.csv(wwTHg_CR8_load_day,"1_CR8_m1_wwTHg_Flux_Daily.csv")

#Lines 75 and 77 create data frames that use the function predLoad. 
#Description of predLoad: Estimate loads from a rating-curve model from loadReg for a new data frame, aggregating the loads by specified time periods.
#Lines 76 and 78 write the data frames to a .csv file. Important note: The file must be intentionally saved as a CSV file- type ".csv" at the end when prompted.
#file.choose() lets the user select the location for the .csv files.