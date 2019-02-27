source('https://raw.githubusercontent.com/laurencelin/Date_analysis/master/LIB_dailytimeseries3.R')
source('https://raw.githubusercontent.com/laurencelin/Date_analysis/master/LIB_misc.r')
source('https://raw.githubusercontent.com/laurencelin/R-coded-scripts-for-RHESSys-calibration/master/LIB_RHESSys_modelBehavior7.R')
source('https://raw.githubusercontent.com/laurencelin/R-coded-scripts-for-RHESSys-calibration/master/LIB_RHESSys_modelFittness7.R')
source('https://raw.githubusercontent.com/laurencelin/R-coded-scripts-for-RHESSys-calibration/master/LIB_RHESSys_modelPlot_7.r')
library(MASS)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------
arg=commandArgs(T)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------

startingDate=as.Date("1990-10-1") #, 
endingDate=as.Date("2011-9-30") #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 

calobs_ = read.csv(paste("/Volumes/LaCie/workArch/University_of_North_Carolina/WS18/cwt18_mcmc","/obs/",'Qobs_18_r.csv',sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,'mmd']) & calobs_[,'mmd']> 0 & sapply(calobs_[,'mmd'],can.be.numeric); 
calobs = calobs_[calobsNonZero,]
calobs.date0 = as.Date(paste(calobs$day, calobs$month, calobs$year,sep="-"),format="%d-%m-%Y")


i=1
rhessys_SingleFile = read.table("/Volumes/LaCie/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output_test/test_basin.daily",header=T,skip=0,sep=' ')
rhessys_SingleFile = read.table("/Volumes/LaCie/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output_test/testg_basin.daily",header=T,skip=0,sep=' ')
	
	rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile$day, rhessys_SingleFile$month, rhessys_SingleFile$year,sep="-"),format="%d-%m-%Y")
	plotTime = intersectDate(list(rhessys_SingleFile.date, calobs.date0, period)) ## "2010-10-01" "2017-09-30"
	rhessys.dtsm = match(plotTime, rhessys_SingleFile.date)
	calobs.dtsm = match(plotTime, calobs.date0)
	DTStable = dailyTimeSeries(plotTime)
	matchYears = range(DTStable$wy)
	
	w = modelFittness( as.numeric(calobs[calobs.dtsm,'mmd']), rhessys_SingleFile[rhessys.dtsm,], DTStable);
	round(w$FittnessList,2)

		
	
	
	
	
	
	
	
	
