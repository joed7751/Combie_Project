#wwMeHg CR1A script for retrieving loads models stats (the stats are used to select the best model for this constituent at this site) and loads predictions.

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

wwMeHg_CR1A<-importRDB("wwMeHg_CR1A_R.txt")
CR1AQ<-importRDB("CR1A_QR.txt")

#These data frames are created by the function importRDB. 
#The calls above bring the constituent data and the daily flow data into the script.

wwMeHg_CR1Am1 <- loadReg(wwMeHg ~model(1), data = wwMeHg_CR1A, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-In")
wwMeHg_CR1Am1
wwMeHg_CR1Am2 <- loadReg(wwMeHg ~model(2), data = wwMeHg_CR1A, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-In")
wwMeHg_CR1Am2
wwMeHg_CR1Am3 <- loadReg(wwMeHg ~model(3), data = wwMeHg_CR1A, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-In")
wwMeHg_CR1Am3
wwMeHg_CR1Am4 <- loadReg(wwMeHg ~model(4), data = wwMeHg_CR1A, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-In")
wwMeHg_CR1Am4
wwMeHg_CR1Am5 <- loadReg(wwMeHg ~model(5), data = wwMeHg_CR1A, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-In")
wwMeHg_CR1Am5
wwMeHg_CR1Am6 <- loadReg(wwMeHg ~model(6), data = wwMeHg_CR1A, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-In")
wwMeHg_CR1Am6
wwMeHg_CR1Am7 <- loadReg(wwMeHg ~model(7), data = wwMeHg_CR1A, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-In")
wwMeHg_CR1Am7
wwMeHg_CR1Am8 <- loadReg(wwMeHg ~model(8), data = wwMeHg_CR1A, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-In")
wwMeHg_CR1Am8
wwMeHg_CR1Am9 <- loadReg(wwMeHg ~model(9), data = wwMeHg_CR1A, flow="Flow", dates = "Dates" ,conc.units="ng/L" , station = "Combie-In")
wwMeHg_CR1Am9

#These objects of class "loadReg" are created.
#A list in R allows you to gather a variety of objects under one name (that is, the name of the list) in an ordered way. 
#These objects can be matrices, vectors, data frames, even other lists, etc. It is not even required that these objects are related to each other in any way.
#When the models are run (m1-m9), the output will be in the console. These are the stats used to select the best model. 

#print(wwMeHg_CR1Am1,brief = FALSE, load.only = FALSE)
#print(wwMeHg_CR1Am2,brief = FALSE, load.only = FALSE)
#print(wwMeHg_CR1Am3,brief = FALSE, load.only = FALSE)
#print(wwMeHg_CR1Am4,brief = FALSE, load.only = FALSE)
#print(wwMeHg_CR1Am5,brief = FALSE, load.only = FALSE)
#print(wwMeHg_CR1Am6,brief = FALSE, load.only = FALSE)
#print(wwMeHg_CR1Am7,brief = FALSE, load.only = FALSE)
#print(wwMeHg_CR1Am8,brief = FALSE, load.only = FALSE)
#print(wwMeHg_CR1Am9,brief = FALSE, load.only = FALSE)
#Commenting these out. These provide some explanations of the data in a longer form. Brief results are printed to console (wwMeHg_CR1Am1-9)

plot(wwMeHg_CR1Am1,ann=FALSE)
title(main = "390132121015201_m1_wwMeHg Response vs Fitted Values",xlab = "Fitted Values",ylab = "Response Values")
plot(wwMeHg_CR1Am1,which = 2,set.up = F)
title(main = "390132121015201_m1_wwMeHg Residuals vs Fitted Values")
plot(wwMeHg_CR1Am1,which = 3,set.up = F)
title(main = "390132121015201_m1_wwMeHg Assessing Heteroscedasticity") 
plot(wwMeHg_CR1Am1,which = 4,set.up = F)
title(main = "390132121015201_m1_wwMeHg Correlogram of Samples")
plot(wwMeHg_CR1Am1,which = 5,set.up = F)
title(main="390132121015201_m1_wwMeHg Normal Discharge")
plot(wwMeHg_CR1Am1,which = 6,set.up = F)
title(main="390132121015201_m1_wwMeHg Box Plot of Loads")

#These functions plot the data using the chosen best model and add a title and labels to the plot.

wwMeHg_CR1A_load<-predLoad(wwMeHg_CR1Am1,CR1AQ,load.units="kg",by="water year",allow.incomplete = TRUE,conf.int = 0.95,print = TRUE)
write.csv(wwMeHg_CR1A_load,"1_CR1A_m1_wwMeHg_Flux_Annual.csv")
wwMeHg_CR1A_load_day<-predLoad(wwMeHg_CR1Am1, CR1AQ,load.units = "kg",by="day",allow.incomplete = TRUE,conf.int = 0.90,print = TRUE)
write.csv(wwMeHg_CR1A_load_day,"1_CR1A_m1_wwMeHg_Flux_Daily.csv")

#Lines 75 and 77 create data frames that use the function predLoad. 
#Description of predLoad: Estimate loads from a rating-curve model from loadReg for a new data frame, aggregating the loads by specified time periods.
#Lines 76 and 78 write the data frames to a .csv file. Important note: The file must be intentionally saved as a CSV file- type ".csv" at the end when prompted.
#file.choose() lets the user select the location for the .csv files.