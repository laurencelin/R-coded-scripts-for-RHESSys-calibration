source('https://raw.githubusercontent.com/laurencelin/Date_analysis/master/LIB_dailytimeseries3.R')
source('https://raw.githubusercontent.com/laurencelin/Date_analysis/master/LIB_misc.r')
source('https://raw.githubusercontent.com/laurencelin/R-coded-scripts-for-RHESSys-calibration/master/LIB_RHESSys_modelBehavior7.R')
source('https://raw.githubusercontent.com/laurencelin/R-coded-scripts-for-RHESSys-calibration/master/LIB_RHESSys_modelFittness7.R')
source('https://raw.githubusercontent.com/laurencelin/R-coded-scripts-for-RHESSys-calibration/master/LIB_RHESSys_modelPlot_7.r')
library(MASS)

RHESSysParamBoundaryDefault = data.frame(s1=c(0.001,20))
RHESSysParamBoundaryDefault $s2 = c(0.1,300.0)
RHESSysParamBoundaryDefault $s3 = c(0.1,20)
RHESSysParamBoundaryDefault $sv1 = c(0.001,20)
RHESSysParamBoundaryDefault $sv2 = c(0.1,300.0)
RHESSysParamBoundaryDefault $gw1 = c(0.001,0.2)
RHESSysParamBoundaryDefault $gw2 = c(0.001,0.2)
RHESSysParamBoundaryDefault $snowEs = c(0.5,2)
RHESSysParamBoundaryDefault $snowTs = c(0.5,2)

# for example,
# RHESSys_arg = paste(
		# '-st 1995 1 1 1 -ed 2011 10 1 1',
		# '-b -t tecfiles/tec_daily.txt',
		# '-w worldfiles/combine_oldwayRZ5_soiltest3b',
		# '-whdr worldfiles_ws18/ws18Bolstadcomb.hdr',
		# '-r flow_ws18/ws18Bolstad.flow')

RivannaJobs=function(RHESSys_arg, outputFOLDER, param, outputfile){
	
	
	jobIDnum = floor(as.numeric(Sys.time()))
	
	sbatch_cluster_header = paste('sbatch -o ', outputFOLDER, '/log.txt -J p', jobIDnum, ' --export=v=\'', sep='')
	sbatch_cluster_end = '\' Rivanna_std.sh'
	cmd_header = sbatch_cluster_header;
	cmd_end = sbatch_cluster_end
	
	
	paramNames=colnames(param)
	if('s3'%in% paramNames){
		sParamLine = paste('-s',paste(param[,'s1'],param[,'s2'],param[,'s3']))
	}else{
		sParamLine = paste('-s',paste(param[,'s1'],param[,'s2']))
	}
	svParamLine = paste('-sv',paste(param[,'sv1'],param[,'sv2']))
	gwParamLine = paste('-gw',paste(param[,'gw1'],param[,'gw2']))
	otherParamLine = ''
	
	# check for non default parameter	
	cond = !(paramNames %in% c("itr","s1","s2","s3","sv1","sv2","gw1","gw2"))
	if(sum(cond)>0){
		otherParam = paramNames[cond]
		j=1; 
		otherParamLine = paste(paste('-',otherParam[j],sep=''),param[, otherParam[j]])
		if(sum(cond)>1) for(j in 2:length(otherParam)) otherParamLine = paste(otherParamLine, paste(paste('-',otherParam[j],sep=''),param[, otherParam[j]]))
				
		AllparamLine = paste(sParamLine, svParamLine, gwParamLine, otherParamLine)
	}else{
		AllparamLine = paste(sParamLine, svParamLine, gwParamLine)
	}

	write('#!/bin/bash', outputfile, append=F)
	for(i in 1:dim(param)[1]){

		cmd = paste(
			cmd_header, 
			paste(
				RHESSys_arg, 
				'-pre', paste(outputFOLDER,'/rhessys',param[i,'itr'],sep=''),
				AllparamLine[i]
			), 
			cmd_end, sep='')
		
		write(cmd, outputfile,append=T)
	}#i
	
}#function


argList = list()
argList$projPath		= "/Volumes/LaCie/current_work/SEES/BAISMAN"
argList$orbFile			= "usgs01583580.csv"
argList$startDate		= "2010-10-1"
argList$endDate			= "2017-9-30"
argList$RHESSysModel	= "rhessys_baisman10m"
argList$RHESSysOutput	= "output_parallelRun18101a"
argList$runScript		= "parallelRun18101a.sh"

evaluateModel = function(passedArgList){
	period=seq.Date(from=as.Date(passedArgList$startDate), to=as.Date(passedArgList$endDate) ,by="day") 
	
	path2Obs = ifelse(grepl('/',passedArgList$orbFile), passedArgList$orbFile, paste(passedArgList$projPath,"/obs/", passedArgList$orbFile,sep=""))
	calobs_ = read.csv(path2Obs, stringsAsFactors=F);
	calobsNonZero = !is.na(calobs_[,'mmd']) & calobs_[,'mmd']> 0 & sapply(calobs_[,'mmd'],can.be.numeric); 
	calobs = calobs_[calobsNonZero,]
	calobs.date0 = as.Date(paste(calobs$day, calobs$month, calobs$year,sep="-"),format="%d-%m-%Y")
	
	path2Runscript = ifelse(grepl('/',passedArgList$runScript), passedArgList$orbFile, paste(passedArgList$projPath, passedArgList$RHESSysModel,passedArgList$runScript ,sep='/'))
	runs = read.table(text = gsub('\'',' ',readLines(path2Runscript)), stringsAsFactors=F )
		## if a new parameters, we need to update here
		paramIndex = match(c('-s','-sv','-gw','-snowEs','-snowTs','-rtz'),runs[1,])
		s1Index = ifelse( can.be.numeric(runs[1,paramIndex[1]+1]), paramIndex[1]+1, NA); 
		s2Index = ifelse( can.be.numeric(runs[1,paramIndex[1]+2]), paramIndex[1]+2, NA); 
		s3Index = ifelse( can.be.numeric(runs[1,paramIndex[1]+3]), paramIndex[1]+3, NA); 
		sv1Index = ifelse( can.be.numeric(runs[1,paramIndex[2]+1]), paramIndex[2]+1, NA); 
		sv2Indes = ifelse( can.be.numeric(runs[1,paramIndex[2]+2]), paramIndex[2]+2, NA); 
		gw1Index = ifelse( can.be.numeric(runs[1,paramIndex[3]+1]), paramIndex[3]+1, NA); 
		gw2Index = ifelse( can.be.numeric(runs[1,paramIndex[3]+2]), paramIndex[3]+2, NA); 
		snowEsIndex = ifelse( can.be.numeric(runs[1,paramIndex[4]+1]), paramIndex[4]+1, NA); 
		snowTsIndex = ifelse( can.be.numeric(runs[1,paramIndex[5]+1]), paramIndex[5]+1, NA); 
		rtzindex = ifelse( can.be.numeric(runs[1,paramIndex[6]+1]), paramIndex[6]+1, NA); 
	finalparamIndex = c(s1Index, s2Index, s3Index, sv1Index, sv2Indes, gw1Index, gw2Index, snowEsIndex, snowTsIndex, rtzindex)
	finalparamLabel = c('s1','s2','s3','sv1','sv2','gw1','gw2','snowEs','snowTs','rtz')
	itrIndex = seq_along(runs[1,])[grepl('^output[a-zA-Z0-9_/]+rhessys[0-9]+$',runs[1,])]
	Itr = as.numeric( gsub('/','',gsub("[a-z]", "", runs[, itrIndex])) )
	
	i=1
	rhessys_SingleFile = read.table(paste(passedArgList$projPath, passedArgList$RHESSysModel, passedArgList$RHESSysOutput, paste("rhessys",Itr[i],"_basin.daily",sep='') ,sep='/'),header=T,sep=' ')
	rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile$day, rhessys_SingleFile$month, rhessys_SingleFile$year,sep="-"),format="%d-%m-%Y")
	plotTime = intersectDate(list(rhessys_SingleFile.date, calobs.date0, period)) ## "2010-10-01" "2017-09-30"
	rhessys.dtsm = match(plotTime, rhessys_SingleFile.date)
	calobs.dtsm = match(plotTime, calobs.date0)
	DTStable = dailyTimeSeries(plotTime)
	matchYears = range(DTStable$wy)
	
	result = sapply(Itr,function(i){
		tryCatch({
			rhessys_SingleFile = read.table(paste(passedArgList$projPath, passedArgList$RHESSysModel, passedArgList$RHESSysOutput, paste("rhessys",Itr[i],"_basin.daily",sep='') ,sep='/'),header=T,sep=' ')
			
			##......... function calling
			w = modelFittness( as.numeric(calobs[calobs.dtsm,'mmd']), rhessys_SingleFile[rhessys.dtsm,], DTStable);
			
			outputName = paste(passedArgList$projPath,passedArgList$RHESSysModel,passedArgList$RHESSysOutput,paste("rhessys",Itr[i],"_plot_", matchYears[1],"_",matchYears[2],"_style2.pdf",sep=''),sep="/")
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
	hold = cbind(runs[, finalparamIndex[!is.na(finalparamIndex)]], t(result));
	colnames(hold)[seq_len(sum(!is.na(finalparamIndex))+1)] = c(finalparamLabel[!is.na(finalparamIndex)],'itr')
	
	output = paste(passedArgList$projPath,'/',passedArgList$RHESSysModel,'/', unlist(strsplit(passedArgList$runScript,'\\.'))[1] ,'_itr',min(Itr),"_",max(Itr),"_fittingEvaluation_", matchYears[1], "_",matchYears[2],".csv",sep="")
	write.csv(hold, output, row.names=F)
	
	return <- output
}#function





