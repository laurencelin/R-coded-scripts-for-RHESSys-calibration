
arg=commandArgs(T)
# arg=c(
	# "/Volumes/LaCie/current_work/SEES/PondBranch/",
	# "usgs.csv",
	# "1", "1", "1000", 
	# "2010-10-1","2017-9-30",
	# "rhessys_pondbranch10m",
	# "output_parallelRun18101a",
	# "parallelRun18101a"
# );paste(arg,collapse=" ")

## -------------------------------------------------------------------------------------------------------------------------------------------------------------
source('https://raw.githubusercontent.com/laurencelin/Date_analysis/master/LIB_dailytimeseries3.R')
source('https://raw.githubusercontent.com/laurencelin/Date_analysis/master/LIB_misc.r')
source('https://raw.githubusercontent.com/laurencelin/R-coded-scripts-for-RHESSys-calibration/master/LIB_RHESSys_modelBehavior7.R')
source('https://raw.githubusercontent.com/laurencelin/R-coded-scripts-for-RHESSys-calibration/master/LIB_RHESSys_modelFittness7.R')
source('https://raw.githubusercontent.com/laurencelin/R-coded-scripts-for-RHESSys-calibration/master/LIB_RHESSys_modelPlot_7.r')
library(MASS)

proj = arg[1]
obsfile = arg[2]
sessionID= as.numeric(arg[3])
Itr = as.numeric(arg[4]):as.numeric(arg[5])

startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,'mmd']) & calobs_[,'mmd']> 0 & sapply(calobs_[,'mmd'],can.be.numeric); 
calobs = calobs_[calobsNonZero,]
calobs.date0 = as.Date(paste(calobs$day, calobs$month, calobs$year,sep="-"),format="%d-%m-%Y")


i=1
rhessys_SingleFile = read.table(paste(proj,"/", arg[8],"/", arg[9],"/","rhessys",Itr[i],"_basin.daily" ,sep=''),header=T,skip=0,sep=' ')
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile$day, rhessys_SingleFile$month, rhessys_SingleFile$year,sep="-"),format="%d-%m-%Y")

	plotTime = intersectDate(list(rhessys_SingleFile.date, calobs.date0, period)) ## "2010-10-01" "2017-09-30"
	rhessys.dtsm = match(plotTime, rhessys_SingleFile.date)
	calobs.dtsm = match(plotTime, calobs.date0)
	DTStable = dailyTimeSeries(plotTime)
	matchYears = range(DTStable$wy)

result = sapply(Itr,function(i){
		tryCatch({
			rhessys_SingleFile = read.table(paste(proj,"/", arg[8],"/", arg[9],"/","rhessys",Itr[i],"_basin.daily" ,sep=''),header=T,skip=0,sep=' ')
			
			##......... function calling
			w = modelFittness( as.numeric(calobs[calobs.dtsm,'mmd']), rhessys_SingleFile[rhessys.dtsm,], DTStable);
			
			outputName = paste(proj,"/",arg[8],"/",arg[9],"/","rhessys",Itr[i],"_plot_", matchYears[1],"_",matchYears[2],"_style2.pdf",sep="")
			modelPlotStyle2( 
				calobs_  = as.numeric(calobs[calobs.dtsm,'mmd']) , 
				rhessys_  = rhessys_SingleFile[rhessys.dtsm,] , 
				dailytimeSeries_ = DTStable , 
				output = outputName)
			
			return <- c(i,w$FittnessList)
				
			
			
		}, error = function(e){
			return <- c(i,rep(NA,24))
		})#try blocks
	})# sapply
	
	
	runs = read.table(text = gsub('\'',' ',readLines('parallelRun18101a.sh')) )# UVA format
	hold = cbind(runs[,c(36,37,39,40,42,43)], t(result));
	colnames(hold)[1:7] = c('s1','s2','sv1','sv2','gw1','gw2','itr')

	output = paste(proj,arg[8],'/',arg[10],'_itr',as.numeric(arg[4]),"_",as.numeric(arg[5]),"_fittingEvaluation_", matchYears[1], "_",matchYears[2],".csv",sep="")
	write.csv(hold, output, row.names=F)


