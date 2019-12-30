source('https://raw.githubusercontent.com/laurencelin/Date_analysis/master/LIB_dailytimeseries3.R')
source('https://raw.githubusercontent.com/laurencelin/Date_analysis/master/LIB_misc.r')
source('https://raw.githubusercontent.com/laurencelin/R-coded-scripts-for-RHESSys-calibration/master/LIB_RHESSys_modelBehavior7.R')
source('https://raw.githubusercontent.com/laurencelin/R-coded-scripts-for-RHESSys-calibration/master/LIB_RHESSys_modelFittness7.R')
source('https://raw.githubusercontent.com/laurencelin/R-coded-scripts-for-RHESSys-calibration/master/LIB_RHESSys_modelPlot_7.r')


RHESSysParamBoundaryDefault = data.frame(s1=c(0.001,20))
RHESSysParamBoundaryDefault $s2 = c(0.1,300.0)
RHESSysParamBoundaryDefault $s3 = c(0.1,20)
RHESSysParamBoundaryDefault $sv1 = c(0.001,20)
RHESSysParamBoundaryDefault $sv2 = c(0.1,300.0)
RHESSysParamBoundaryDefault $gw1 = c(0.001,0.2)
RHESSysParamBoundaryDefault $gw2 = c(0.001,0.2)
RHESSysParamBoundaryDefault $snowEs = c(0.5,2)
RHESSysParamBoundaryDefault $snowTs = c(0.5,2)
RHESSysParamBoundaryDefault $svalt1 = c(0.5,2)
RHESSysParamBoundaryDefault $svalt2 = c(0.5,2)
	
argList = list()
searchParam = list()
# for example,
# RHESSys_arg = paste(
		# '-st 1995 1 1 1 -ed 2011 10 1 1',
		# '-b -t tecfiles/tec_daily.txt',
		# '-w worldfiles/combine_oldwayRZ5_soiltest3b',
		# '-whdr worldfiles_ws18/ws18Bolstadcomb.hdr',
		# '-r flow_ws18/ws18Bolstad.flow')

scaler_random = function(itr, param_bounds){
	# itr is a vector of iteration index
	# param_bounds is a data frame
	nn = length(itr)
	outputcolnames = colnames(param_bounds)
	output = data.frame(itr = itr,
	sapply(outputcolnames,function(xx){
		lower = param_bounds[1,xx]
		upper = param_bounds[2,xx]
		if(lower<1 & upper > 1){
	        mm = floor(nn*0.3)
	        normalBounded = rnorm(10*mm,1,(upper-lower)*0.05)
	        cond = normalBounded>=lower & normalBounded<=upper
	        randomlist = c( runif(mm,lower,1), normalBounded[cond][1:mm], runif(nn-2*mm,1,upper) )
	        return <- sample(randomlist,nn)
	    }else{
	        return <- runif(nn,lower, upper)
	    }
	}))# sapply
	#colnames(output) = c('itr',outputcolnames)
	return <- output
}#function

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
            return <- paste(
                paste('-',flagname[ii][1],sep=''),
                param[,ii]
                )
        }
    })#tapply
    
    ## don;t know how to do
    tmp = names(what);
    what.paramINDEX = match(tmp[tmp!='it'& tmp!='itr'],names(what))
    
    AllparamLine = NULL
    for(ii in seq_along(what.paramINDEX)){
       if(ii==1)  AllparamLine = what[[what.paramINDEX[ii] ]]
       else AllparamLine=paste(AllparamLine,what[[what.paramINDEX[ii] ]])
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
		
    path2modelresults = ifelse(grepl('/',passedArgList$RHESSysOutput), passedArgList$RHESSysOutput, paste(passedArgList$projPath, passedArgList$RHESSysModel, passedArgList$RHESSysOutput,sep='/'))
	i=1
	rhessys_SingleFile = read.table(paste(path2modelresults, paste("rhessys",Itr[i],"_basin.daily",sep='') ,sep='/'),header=T,sep=' ')
	rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile$day, rhessys_SingleFile$month, rhessys_SingleFile$year,sep="-"),format="%d-%m-%Y")
	plotTime = intersectDate(list(rhessys_SingleFile.date, calobs.date0, period)) ## "2010-10-01" "2017-09-30"
	rhessys.dtsm = match(plotTime, rhessys_SingleFile.date)
	calobs.dtsm = match(plotTime, calobs.date0)
	DTStable = dailyTimeSeries(plotTime)
	matchYears = range(DTStable$wy)
	
	
	pb <- txtProgressBar(min = 0, max = length(Itr), style = 3)
	result = sapply(seq_along(Itr),function(i){
		tryCatch({
			rhessys_SingleFile = read.table(paste(path2modelresults, paste("rhessys",Itr[i],"_basin.daily",sep='') ,sep='/'),header=T,sep=' ')
			
			##......... function calling
			w = modelFittness( as.numeric(calobs[calobs.dtsm,'mmd']), rhessys_SingleFile[rhessys.dtsm,], DTStable);
			
			outputName = paste(path2modelresults,paste("rhessys",Itr[i],"_plot_", matchYears[1],"_",matchYears[2], suffix,"style2.pdf",sep=''),sep="/")
			modelPlotStyle2( 
				calobs_  = as.numeric(calobs[calobs.dtsm,'mmd']) , 
				rhessys_  = rhessys_SingleFile[rhessys.dtsm,] , 
				dailytimeSeries_ = DTStable , 
				output = outputName)
			
			setTxtProgressBar(pb, i)
			return <- c(Itr[i],w$FittnessList)
		}, error = function(e){
			setTxtProgressBar(pb, i)
			ff = paste(path2modelresults, paste("rhessys",Itr[i],"_basin.daily",sep='') ,sep='/')
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


##-------------------------------------------------------------------------------------------- clusterAnalysis
clusterAnalysis = function(fittnessfile){
	
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
    #print(searchParam)
    
	## read fittness file
	w = read.csv(fittnessfile)
	
	## finding parameters
	w.names = colnames(w)
	paramNames = names(searchParam)
	finalparamLabel = unlist(lapply(seq_along(searchParam), function(i){
			if(length(searchParam[[i]])>1) return <- paste(paramNames[i], searchParam[[i]],sep='')
			else return <- paramNames[i]
		}))# lapply; unlist
	
	finalparamIndex = match(finalparamLabel, w.names)
	print(finalparamLabel)
	print(finalparamIndex)


	
		selectCond = T
		allowance = 6
		while(selectCond){
			threshold = max(w$loglikelihood,na.rm=T) - allowance
			clusterData <- w[!is.na(w$loglikelihood) & w$loglikelihood>threshold,finalparamLabel[!is.na(finalparamIndex)] ]
			fittnessData = w[!is.na(w$loglikelihood) & w$loglikelihood>threshold,c('dailyNSE','dailyLogNSE','weeklyNSE','weeklyLogNSE','weeklyCDFfitr2','monthlyNSE','yearlyNSE','bias')]
			
			#------------------- option 1)
			# Determine number of clusters
			maxNumCluster = floor(dim(clusterData)[1]/3)
			wss <- (dim(clusterData)[1]-1)*sum(apply(clusterData,2,var))
			for (i in 2: maxNumCluster) wss[i] <- sum(kmeans(clusterData, centers=i,iter.max=1000)$withinss)
			#plot(seq_along(wss), wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
			
				AccumImprovement = cumsum(diff(wss))
				#plot( AccumImprovement, type='b')
				numCluster = which( AccumImprovement/min(AccumImprovement) > 0.7)[1]; numCluster #**
			
			# K-Means Cluster Analysis
			fit <- kmeans(clusterData, numCluster,iter.max=1000) # 80 cluster solution
			fit$classification = fit$cluster
			
			#------------------- option 2)
			#fit <- Mclust(clusterData) #plot(fit) #summary(fit) ##<<--- problem
			
			selectCond = !prod(table(fit$classification)>3) | dim(fittnessData)[1]/dim(w)[1] < 0.05 # looping key
			# the second argument make the cluster based on at least 10% of the calibration
			allowance = allowance + 1
		}#while
	
		# selected parameter sets ---- will be used later
		selected = w[!is.na(w$loglikelihood) & w$loglikelihood>threshold,c('itr','loglikelihood')]
		hold = cbind(selected[order(selected[,2]),], fittnessData[order(selected[,2]),])
		# cbind(fit$classification, fittnessData) # for debug
		# c( dim(fittnessData)[1],  dim(fittnessData)[1]/dim(w)[1] )
	
	## parameter space
	param.mean = aggregate(clusterData,by=list(fit$classification),FUN=mean)
	param.sd = aggregate(clusterData,by=list(fit$classification),FUN=sd)
	param.max = aggregate(clusterData,by=list(fit$classification),FUN=max)
	param.min = aggregate(clusterData,by=list(fit$classification),FUN=min)
	param.len = aggregate(clusterData,by=list(fit$classification),FUN=length)
	numCluster = dim(param.mean)[1]
	param.num = seq_len(dim(param.mean)[2]-1)
	
	param.lower = apply(w[,finalparamLabel[!is.na(finalparamIndex)]],2,min)
	param.upper = apply(w[,finalparamLabel[!is.na(finalparamIndex)]],2,max)
	param.upper = ceiling(param.upper/(10^round(log10(param.upper))) )*(10^round(log10(param.upper)))
	param.range = param.upper-param.lower
		
	## fittness space
	fittness.mean = aggregate(fittnessData,by=list(fit$classification),FUN=mean)
	fittness.sd = aggregate(fittnessData,by=list(fit$classification),FUN=sd)
	fittness.max = aggregate(fittnessData,by=list(fit$classification),FUN=max)
	fittness.min = aggregate(fittnessData,by=list(fit$classification),FUN=min)
	fittness.len = aggregate(fittnessData,by=list(fit$classification),FUN=length)
	numCluster = nrow(fittness.mean)

	## plots
	layout(matrix(1:2,nrow=2))
	par(mar=c(4,3,1,1))
	myColor = rainbow(numCluster)
	param.mean.std = as.numeric((param.mean[1,-1]-param.lower)/param.range)
	param.upper.std = as.numeric((param.mean[1,-1]+param.sd[1,-1]-param.lower)/param.range)
	param.lower.std = as.numeric((param.mean[1,-1]-param.sd[1,-1]-param.lower)/param.range)
	plot(param.num, param.mean.std, type='b', ylim=c(0,1),col=myColor[1], ylab='std param space', xlab='', xaxt='n')
	arrows(param.num,y0= param.upper.std, y1= param.lower.std,code=3,length=0.02,angle=90,col=myColor[1])
	for(jj in 2:numCluster){
		param.mean.std = as.numeric((param.mean[jj,-1]-param.lower)/param.range)
		param.upper.std = as.numeric((param.mean[jj,-1]+param.sd[1,-1]-param.lower)/param.range)
		param.lower.std = as.numeric((param.mean[jj,-1]-param.sd[1,-1]-param.lower)/param.range)
	  	lines(param.num, param.mean.std,col=myColor[jj], type='b')
	  	arrows(param.num,y0= param.upper.std,y1=param.lower.std,code=3,length=0.02,angle=90,col=myColor[jj])
	}; 
	axis(1, at= param.num, labels=finalparamLabel[!is.na(finalparamIndex)]); 
	axis(1,line=1, at= param.num, labels=round(param.upper,2),col=NA)

	plot(1:8, fittness.mean[1,-1], type='b', ylim=c(0,1),col=myColor[1], ylab='fittness space', xlab='', xaxt='n')
	arrows(1:8,y0=as.numeric(fittness.mean[1,-1]+ fittness.sd[1,-1]), y1=as.numeric(fittness.mean[1,-1]-fittness.sd[1,-1]),code=3,length=0.02,angle=90,col=myColor[1])
	for(jj in 2:numCluster){
	  lines(1:8, fittness.mean[jj,-1],col=myColor[jj], type='b')
	  arrows(1:8,y0=as.numeric(fittness.mean[jj,-1]+ fittness.sd[jj,-1]),y1=as.numeric(fittness.mean[jj,-1]- fittness.sd[jj,-1]),code=3,length=0.02,angle=90,col=myColor[jj])
	}; 
	axis(1, at=1:8, labels=colnames(fittnessData), cex.axis=0.5,las=2); 
	
	
	
	
	## generate next set of random
	param.num = round(1000/dim(param.mean)[1])
   	paramList = lapply(seq_len(dim(param.mean)[1]), function(i){
		return <- sapply(names(param.mean)[-1], function(nn){
			tmp = rnorm(param.num*10, param.mean[i,nn], param.sd[i,nn])
			cond = (tmp<max(RHESSysParamBoundaryDefault[,nn]) & tmp>min(RHESSysParamBoundaryDefault[,nn]) )
			return <- tmp[cond][1:param.num]
		})
	})# lapply


	param = data.frame( itr = seq_len(param.num*dim(param.mean)[1]) )
	for(nn in names(param.mean)[-1]){
		param[,nn] = as.vector(sapply(seq_len(dim(param.mean)[1]),function(jj){paramList[[jj]][,nn]}))
	}#nn

	return <- list(param=param, selectedParamSet=hold)
}#clusterAnalysis



