#TSS CR1A script for retrieving loads models stats (the stats are used to select the best model for this constituent at this site) and loads predictions.

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

TSS_CR1A<-importRDB("TSS_CR1A_R.txt")
CR1AQ<-importRDB("CR1A_QR.txt")

#These data frames are created by the function importRDB. 
#The calls above bring the constituent data and the daily flow data into the script.

TSS_CR1Am1 <- loadReg(TSS ~model(1), data = TSS_CR1A, flow="Flow", dates = "Dates" ,conc.units="mg/L" , station = "Combie-In")
TSS_CR1Am1
TSS_CR1Am2 <- loadReg(TSS ~model(2), data = TSS_CR1A, flow="Flow", dates = "Dates" ,conc.units="mg/L" , station = "Combie-In")
TSS_CR1Am2
TSS_CR1Am3 <- loadReg(TSS ~model(3), data = TSS_CR1A, flow="Flow", dates = "Dates" ,conc.units="mg/L" , station = "Combie-In")
TSS_CR1Am3
TSS_CR1Am4 <- loadReg(TSS ~model(4), data = TSS_CR1A, flow="Flow", dates = "Dates" ,conc.units="mg/L" , station = "Combie-In")
TSS_CR1Am4
TSS_CR1Am5 <- loadReg(TSS ~model(5), data = TSS_CR1A, flow="Flow", dates = "Dates" ,conc.units="mg/L" , station = "Combie-In")
TSS_CR1Am5
TSS_CR1Am6 <- loadReg(TSS ~model(6), data = TSS_CR1A, flow="Flow", dates = "Dates" ,conc.units="mg/L" , station = "Combie-In")
TSS_CR1Am6
TSS_CR1Am7 <- loadReg(TSS ~model(7), data = TSS_CR1A, flow="Flow", dates = "Dates" ,conc.units="mg/L" , station = "Combie-In")
TSS_CR1Am7
TSS_CR1Am8 <- loadReg(TSS ~model(8), data = TSS_CR1A, flow="Flow", dates = "Dates" ,conc.units="mg/L" , station = "Combie-In")
TSS_CR1Am8
TSS_CR1Am9 <- loadReg(TSS ~model(9), data = TSS_CR1A, flow="Flow", dates = "Dates" ,conc.units="mg/L" , station = "Combie-In")
TSS_CR1Am9

#These objects of class "loadReg" are created.
#A list in R allows you to gather a variety of objects under one name (that is, the name of the list) in an ordered way. 
#These objects can be matrices, vectors, data frames, even other lists, etc. It is not even required that these objects are related to each other in any way.
#When the models are run (m1-m9), the output will be in the console. These are the stats used to select the best model. 

#print(TSS_CR1Am1,brief = FALSE, load.only = FALSE)
#print(TSS_CR1Am2,brief = FALSE, load.only = FALSE)
#print(TSS_CR1Am3,brief = FALSE, load.only = FALSE)
#print(TSS_CR1Am4,brief = FALSE, load.only = FALSE)
#print(TSS_CR1Am5,brief = FALSE, load.only = FALSE)
#print(TSS_CR1Am6,brief = FALSE, load.only = FALSE)
#print(TSS_CR1Am7,brief = FALSE, load.only = FALSE)
#print(TSS_CR1Am8,brief = FALSE, load.only = FALSE)
#print(TSS_CR1Am9,brief = FALSE, load.only = FALSE)
#Commenting these out. These provide some explanations of the data in a longer form. Brief results are printed to console (TSS_CR1Am1-9)

plot(TSS_CR1Am5,ann=FALSE)
title(main = "390132121015201_m5_TSS Response vs Fitted Values",xlab = "Fitted Values",ylab = "Response Values")
plot(TSS_CR1Am5,which = 2,set.up = F)
title(main = "390132121015201_m5_TSS Residuals vs Fitted Values")
plot(TSS_CR1Am5,which = 3,set.up = F)
title(main = "390132121015201_m5_TSS Assessing Heteroscedasticity") 
plot(TSS_CR1Am5,which = 4,set.up = F)
title(main = "390132121015201_m5_TSS Correlogram of Samples")
plot(TSS_CR1Am5,which = 5,set.up = F)
title(main="390132121015201_m5_TSS Normal Discharge")
plot(TSS_CR1Am5,which = 6,set.up = F)
title(main="390132121015201_m5_TSS Box Plot of Loads")

#These functions plot the data using the chosen best model and add a title and labels to the plot.

TSS_CR1A_load<-predLoad(TSS_CR1Am5,CR1AQ,load.units="kg",by="water year",allow.incomplete = TRUE,conf.int = 0.95,print = TRUE)
write.csv(TSS_CR1A_load,"1_CR1A_m5_TSS_Flux_Annual.csv")
TSS_CR1A_load_day<-predLoad(TSS_CR1Am5, CR1AQ,load.units = "kg",by="day",allow.incomplete = TRUE,conf.int = 0.90,print = TRUE)
write.csv(TSS_CR1A_load_day,"1_CR1A_m5_TSS_Flux_Daily.csv")

#Lines 75 and 77 create data frames that use the function predLoad. 
#Description of predLoad: Estimate loads from a rating-curve model from loadReg for a new data frame, aggregating the loads by specified time periods.
#Lines 76 and 78 write the data frames to a .csv file. Important note: The file must be intentionally saved as a CSV file- type ".csv" at the end when prompted.
#file.choose() lets the user select the location for the .csv files.

