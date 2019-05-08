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

argList = list()
searchParam = list()
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
	
    ## AllparamLine is a 2d array
    ## do something about searchParam -- not the best code yet but works for now
    param.name = colnames(param)
    flagname = flagname = paste(substr(param.name,1,nchar(param.name)-1) , gsub('[0-9]','',substr(param.name,nchar(param.name),nchar(param.name)) ), sep='')
    what = tapply(seq_along(param.name),flagname, function(ii){
        if(length(ii)==1 & (param.name[ii][1] == 'itr' | param.name[ii][1] == 'it')){
            return <- NULL
        }else if(length(ii)>1){
            return <- paste(
                paste('-',flagname[ii][1],sep=''),
                sapply(seq_len(dim(param[,ii])[1]),function(jj){ paste(param[jj,ii],collapse=' ') })
                )
        }else{
            retirn <- paste(
                paste('-',flagname[ii][1],sep=''),
                param[,ii]
                )
        }
    })#tapply
    
    ## don;t know how to do
    tmp = names(what);
    what.paramINDEX = match(tmp[tmp!='it'],names(what))
    
    AllparamLine = NULL
    for(ii in seq_along(what.paramINDEX)){
       if(ii==1)  AllparamLine = what[[what.paramINDEX[ii]]]
       else AllparamLine=paste(AllparamLine,what[[what.paramINDEX[ii]]])
    }#for ii
    
    
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


evaluateModel = function(passedArgList, topPrecent=1, bottomPrecent=0){
    
    suffix = '_';
    if(topPrecent<1 | bottomPrecent>0) suffix = paste('',gsub('\\.','', bottomPrecent), gsub('\\.','',topPrecent),'',sep='_')
    
    ## do something about searchParam -- not the best code yet but works for now
    tmp = names(RHESSysParamBoundaryDefault)
    flagname = paste(substr(tmp,1,nchar(tmp)-1) , gsub('[0-9]','',substr(tmp,nchar(tmp),nchar(tmp)) ), sep='')
    hold = tapply(tmp, flagname, function(xx){ seq_along(xx) });
    hold.name = names(hold)
    for( ii in seq_along(hold)){
        findindex = match(hold.name[ii],names(searchParam))
        if(is.na(findindex)){
            searchParam = append(searchParam, hold[ii])
        }else{
            searchParam[[ii]] = hold[[ii]]
        }
    }#for ii
    
    
	period=seq.Date(from=as.Date(passedArgList$startDate), to=as.Date(passedArgList$endDate) ,by="day") 
	
	path2Obs = ifelse(grepl('/',passedArgList$orbFile), passedArgList$orbFile, paste(passedArgList$projPath,"/obs/", passedArgList$orbFile,sep=""))
	calobs_ = read.csv(path2Obs, stringsAsFactors=F);
		## selecting observation
		calobsNonZero = !is.na(calobs_[,'mmd']) & calobs_[,'mmd']> 0 & sapply(calobs_[,'mmd'],can.be.numeric); 
		calobs = calobs_[calobsNonZero,]
		calobs_boundary = quantile(calobs[,'mmd'],prob=c(bottomPrecent, topPrecent))
		calobs_boundary_cond = calobs[,'mmd']>=calobs_boundary[1] & calobs[,'mmd']<=calobs_boundary[2]
		calobs = calobs[calobs_boundary_cond, ]
	calobs.date0 = as.Date(paste(calobs$day, calobs$month, calobs$year,sep="-"),format="%d-%m-%Y")
	
	path2Runscript = ifelse(grepl('/',passedArgList$runScript), passedArgList$runScript, paste(passedArgList$projPath, passedArgList$RHESSysModel,passedArgList$runScript ,sep='/'))
	
	
	runs = read.table(text = gsub('\'',' ',readLines(path2Runscript)), stringsAsFactors=F )
		## if a new parameters, we need to update here
		
		paramNames = names(searchParam)
		paramIndex = match(paste('-', paramNames,sep=''),runs[1,])
		
		finalparamIndex = unlist(lapply(seq_along(searchParam),function(i){
			sapply(searchParam[[i]], function(ii){ ifelse( can.be.numeric(runs[1,paramIndex[i]+ii]), paramIndex[i]+ii, NA); })
		}))# lapply; unlist
		finalparamLabel = unlist(lapply(seq_along(searchParam), function(i){
			if(length(searchParam[[i]])>1) return <- paste(paramNames[i], searchParam[[i]],sep='')
			else return <- paramNames[i]
		}))# lapply; unlist
		
		itrIndex = seq_along(runs[1,])[grepl('^output[a-zA-Z0-9_/]+rhessys[0-9]+$',runs[1,])]
		Itr = as.numeric(sapply(gsub("[a-z]", "", runs[, itrIndex]),function(str){ unlist(strsplit(str,'/'))[2] }))
		
	i=1
	rhessys_SingleFile = read.table(paste(passedArgList$projPath, passedArgList$RHESSysModel, passedArgList$RHESSysOutput, paste("rhessys",Itr[i],"_basin.daily",sep='') ,sep='/'),header=T,sep=' ')
	rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile$day, rhessys_SingleFile$month, rhessys_SingleFile$year,sep="-"),format="%d-%m-%Y")
	plotTime = intersectDate(list(rhessys_SingleFile.date, calobs.date0, period)) ## "2010-10-01" "2017-09-30"
	rhessys.dtsm = match(plotTime, rhessys_SingleFile.date)
	calobs.dtsm = match(plotTime, calobs.date0)
	DTStable = dailyTimeSeries(plotTime)
	matchYears = range(DTStable$wy)
	
	
	pb <- txtProgressBar(min = 0, max = length(Itr), style = 3)
	result = sapply(seq_along(Itr),function(i){
		tryCatch({
			rhessys_SingleFile = read.table(paste(passedArgList$projPath, passedArgList$RHESSysModel, passedArgList$RHESSysOutput, paste("rhessys",Itr[i],"_basin.daily",sep='') ,sep='/'),header=T,sep=' ')
			
			##......... function calling
			w = modelFittness( as.numeric(calobs[calobs.dtsm,'mmd']), rhessys_SingleFile[rhessys.dtsm,], DTStable);
			
			outputName = paste(passedArgList$projPath,passedArgList$RHESSysModel,passedArgList$RHESSysOutput,paste("rhessys",Itr[i],"_plot_", matchYears[1],"_",matchYears[2], suffix,"style2.pdf",sep=''),sep="/")
			modelPlotStyle2( 
				calobs_  = as.numeric(calobs[calobs.dtsm,'mmd']) , 
				rhessys_  = rhessys_SingleFile[rhessys.dtsm,] , 
				dailytimeSeries_ = DTStable , 
				output = outputName)
			
			setTxtProgressBar(pb, i)
			return <- c(Itr[i],w$FittnessList)
		}, error = function(e){
			setTxtProgressBar(pb, i)
			ff = paste(passedArgList$projPath, passedArgList$RHESSysModel, passedArgList$RHESSysOutput, paste("rhessys",Itr[i],"_basin.daily",sep='') ,sep='/')
			print(paste('file', ff, ' is missing/corrupted.'))
			return <- c(i,rep(NA,24))
		})#try blocks
	})# sapply
	hold = cbind(runs[, finalparamIndex[!is.na(finalparamIndex)]], t(result));
	colnames(hold)[seq_len(sum(!is.na(finalparamIndex))+1)] = c(finalparamLabel[!is.na(finalparamIndex)],'itr')
	
	output = paste(passedArgList$projPath,'/',passedArgList$RHESSysModel,'/', unlist(strsplit(tail(unlist(strsplit(passedArgList$runScript,'/')),n=1),'\\.'))[1] ,'_itr',min(Itr),"_",max(Itr),"_fittingEvaluation",suffix, matchYears[1], "_",matchYears[2],".csv",sep="")
	write.csv(hold, output, row.names=F)
	
	return <- output
}#function






