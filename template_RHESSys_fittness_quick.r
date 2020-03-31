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

calobs_ = read.csv(paste('path',sep=""),stringsAsFactors=F);
calobsNonZero = sapply(calobs_[,'mmd'],can.be.numeric) & !is.na(calobs_[,'mmd']) & calobs_[,'mmd']> 0 & sapply(calobs_[,'mmd'],can.be.numeric); 
calobs = calobs_[calobsNonZero,]
calobs.date0 = as.Date(paste(calobs$day, calobs$month, calobs$year,sep="-"),format="%d-%m-%Y")


rhessys_SingleFile = read.table("path",header=T,skip=0,sep=' ')

	
	rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile$day, rhessys_SingleFile$month, rhessys_SingleFile$year,sep="-"),format="%d-%m-%Y")
	plotTime = intersectDate(list(rhessys_SingleFile.date, calobs.date0, period)) ## "2010-10-01" "2017-09-30"
	rhessys.dtsm = match(plotTime, rhessys_SingleFile.date)
	calobs.dtsm = match(plotTime, calobs.date0)
	DTStable = dailyTimeSeries(plotTime)
	matchYears = range(DTStable$wy)
	
	w = modelFittness( as.numeric(calobs[calobs.dtsm,'mmd']), rhessys_SingleFile[rhessys.dtsm,], DTStable);
	round(w$FittnessList,2)

	##---------------------------------------- Diagnosis
	dev.new(width=14, height=8)
	layout(matrix(1:4,nrow=4)); 
	par(mar=c(0,3,1,3))
	plot(plotTime, rhessys_SingleFile[rhessys.dtsm,'precip'], col='lightblue', lwd=2,type='l', ylab='',xaxt='n',bty='n', ylim=rev(range(rhessys_SingleFile[rhessys.dtsm,'precip'])))
	abline(h=0)
	par(new = TRUE)
	plot(plotTime, rhessys_SingleFile[rhessys.dtsm,'tmin'], col=t_col('lightblue1'), bty='l',lty=2, type='l', ylim=c(-20,40), yaxt='n', bty='n',xaxt='n')
	lines(plotTime, rhessys_SingleFile[rhessys.dtsm,'tmax'], col= t_col('lightpink'), lty=2)
	lines(plotTime, rhessys_SingleFile[rhessys.dtsm,'tavg'], col= t_col('gray'))
	abline(h=0, lty=2, col='gray')
	axis(4)
	par(mar=c(0,3,0,3))
	upper = max(rhessys_SingleFile[rhessys.dtsm,'streamflow'],calobs[calobs.dtsm,'mmd'])
	lower = min(rhessys_SingleFile[rhessys.dtsm,'streamflow'],calobs[calobs.dtsm,'mmd'])
	plot(plotTime, rhessys_SingleFile[rhessys.dtsm,'streamflow'], col='blue', lwd=2,type='l', ylab='', ylim=c(lower ,upper),bty='n',xaxt='n')
	lines(plotTime, calobs[calobs.dtsm,'mmd'], col='red')
	lines(plotTime, rhessys_SingleFile[rhessys.dtsm,'pet'], col='gray')
	lines(plotTime, rhessys_SingleFile[rhessys.dtsm,'evap']+rhessys_SingleFile[rhessys.dtsm,'trans'], col='green')
	lines(plotTime, rhessys_SingleFile[rhessys.dtsm,'X.sat_area'], lty=2,col=gray(0.8))
	lines(plotTime, rhessys_SingleFile[rhessys.dtsm,'snowpack'],col='lightblue')
	par(mar=c(0,3,0,3))
	plot(plotTime, rhessys_SingleFile[rhessys.dtsm,'sat_def_z'],type='l', ylab='',xaxt='n',bty='n', ylim=rev(range(rhessys_SingleFile[rhessys.dtsm,'sat_def_z'])))
	par(mar=c(3,3,0,3))
	upper = max(rhessys_SingleFile[rhessys.dtsm,'streamflow'],calobs[calobs.dtsm,'mmd'])
	lower = min(rhessys_SingleFile[rhessys.dtsm,'streamflow'],calobs[calobs.dtsm,'mmd'])
	plot(plotTime, calobs[calobs.dtsm,'mmd'], col='red', type='l',log='y',bty='l', ylim=c(lower ,upper))
	lines(plotTime, rhessys_SingleFile[rhessys.dtsm,'streamflow'], col='blue' )
	lines(plotTime, rhessys_SingleFile[rhessys.dtsm,'baseflow'], col='darkblue',lty=2 )
	##---------------------------------------- 
	
	
	
	
	
	
	
	
