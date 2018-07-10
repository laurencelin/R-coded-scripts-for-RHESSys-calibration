
RscriptFolder = '~/LIB_scripts/'
source(paste(RscriptFolder,"LIB_dailytimeseries3.R",sep=''))
source(paste(RscriptFolder,"LIB_RHESSys_modelBehavior7.R",sep=''))
source(paste(RscriptFolder,"LIB_RHESSys_modelFittness7.R",sep=''))
library(MASS)


# assume this script is right next to complied RHESSys execute file
arg=commandArgs(T)
if(length(arg)<1){
	arg=c(
		".", 									# 1 script_to_rhessysection location 
		"../obs/usgs01589290.csv",				# 2 observed file (full/relative) path 
		"2", 								  	# 3 starting iteration
		"20", 									# 4 ending iteration
		"2011-1-1",								# 5 start date of fitting (not simulation start date!)
		"2017-11-30",							# 6 end date of fitting
		"output",								# 7 output folder name
		"SLB3m",								# 8 prefix of the fitting table
		"10",									# 9 number of proposed parameter values in one test
		"1293"									# 10 jobs ID number (use for clusters)
	);paste(arg,collapse=" ")
}


script_to_rhessys = arg[1] 
script_to_obs = arg[2]
Itr = as.numeric(arg[3]):as.numeric(arg[4])
startingDate=as.Date(arg[5])
endingDate=as.Date(arg[6])
period=seq.Date(from=startingDate, to=endingDate ,by="day") 
outputFolder = arg[7]
MCMC_output_Prefix = arg[8]
numCore = as.numeric(arg[9])
psbUniqueID = arg[10]
append = F


#====================================================================================================================#	
	## ---- cluster and RHESSys settings 
RHESSys_model = "./rhessys5.20.0.nighttime-evapFinal_debug2Final2_bioN_growthseason_shadedfraction"
RHESSys_uncalibrated_arguments = paste(
		"-st 2006 1 1 1 -ed 2017 12 1 1",
		"-b",
		paste("-t ", "tecfiles/tec_daily.txt",sep=""),
		paste("-w ", "worldfiles/worldfile",sep=""),
		paste("-whdr ", "worldfiles/world_testing_sorted.hdr",sep=""),
		paste("-r ", "flow/flowtable_sub.txt",sep=""),
		"-grassIrrigation_flag",
		sep=' ')
SLURM_script = rep(NA,6)
SLURM_script[1] = '#!/bin/bash'
SLURM_script[2] = '#SBATCH --nodes=1'
SLURM_script[3] = '#SBATCH --ntasks=1'
SLURM_script[4] = '#SBATCH -t 144:00:00'
SLURM_script[5] = '#SBATCH -p standard'
SLURM_script[6] = paste(RHESSys_model,'$v')
write(SLURM_script, paste(script_to_rhessys,'/SLURM_script.sh',sep=''))

#====================================================================================================================#	
	## ---- listing all RHESSys paramters, included optional / customized ones
RHESSYS_PARAMS = c('s1','s2','s3','sv1','sv2','gw1','gw2','pondz','snowT','RTz','CAPr','snowEs')
iniParam = rep(1, length(RHESSYS_PARAMS)); names(iniParam)= RHESSYS_PARAMS
iniParam['s1'] = 6.164172 #M --> control the water table depth; inital satz=0 and M control the lateral v vertical profile.
iniParam['s2'] = 75.70877 #K
iniParam['s3'] = 0.6413342
iniParam['sv1'] = 0.01162551
iniParam['sv2'] = 31.357
iniParam['gw1'] = 0.006282594
iniParam['gw2'] = 0.106507
iniParam['pondz'] = 1
iniParam['snowT'] = 1
iniParam['snowEs'] = 0.28005
iniParam['RTz'] = 1
iniParam['CAPr'] = 1

	## ---- selecting parameters for calibration
param.fittingNames =c('s1','sv1','snowEs','s3','gw1','gw2')

#====================================================================================================================#	
	## ---- fittness metrics 
FITTNESS_NAMES = c('bias','wbias','sbias','inversedweeklyNSE','weeklyNSE','monthlyNSE','yearlyNSE','weeklyCDFfitr2','weeklyLogNSE','ETbias','dailyNSE','dailyLogNSE','flashCOMP')
	## ---- likelihood probability distribution: exp. / gamma
betaShape1 = rep(NA,length(FITTNESS_NAMES) ); names(betaShape1)= FITTNESS_NAMES
betaShape1['bias'] = 1/5
betaShape1['wbias'] = 1/3
betaShape1['sbias'] = 1/3
betaShape1['inversedweeklyNSE'] = 5
betaShape1['weeklyNSE'] = 5
betaShape1['monthlyNSE'] = 5
betaShape1['yearlyNSE'] = 5
betaShape1['weeklyCDFfitr2'] = 5
betaShape1['weeklyLogNSE'] = 3.5
betaShape1['ETbias'] = 1/2
betaShape1['dailyNSE'] = 5
betaShape1['dailyLogNSE'] = 5
betaShape1['flashCOMP'] = 1/5
	## ---- selecting which fittness metrics to use (set it T; otherwise set it F)
fittnessChoice=rep(T, length(betaShape1)); names(fittnessChoice)= FITTNESS_NAMES
fittnessChoice['flashCOMP']=F

#====================================================================================================================#		
	## ---- parameter boundaries
paramBoundary = matrix(NA,1+length(RHESSYS_PARAMS),2); rownames(paramBoundary)=c('itr', RHESSYS_PARAMS); colnames(paramBoundary)=c('min','max')
	# ...... default
	paramBoundary['s1',]=c(0.01,20.0) 
	paramBoundary['s2',]=c(0.1,300.0) 
	paramBoundary['s3',]=c(0.06,1) 
	paramBoundary['sv1',]=c(0.01,1.0) 
	paramBoundary['sv2',]=c(0.1,300.0) 
	paramBoundary['gw1',]=c(0.001,0.2)
	paramBoundary['gw2',]=c(0.01,0.3) 
	paramBoundary['pondz',]=c(0.01,5) 
	paramBoundary['snowT',]=c(0.001,5) 
	paramBoundary['RTz',]=c(0.001,5)
	paramBoundary['CAPr',]=c(0,1)
	paramBoundary['snowEs',]=c(0.001,5) 

paramRange = paramBoundary[,2]-paramBoundary[,1]
paramBaseSD = paramRange*0.1
paraminSD = paramRange*0.01

#====================================================================================================================#	
	## ---- reading observation file
obsCol = 'sitemmd' ##'mmd'
calobs_ = read.csv(script_to_obs,stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[, obsCol]) & calobs_[, obsCol]>0 #& calobs_[, obsCol]<= quantile(calobs_[, obsCol],0.9) 
calobs= calobs_[calobsNonZero,]
calobs.date0 = as.Date(calobs[,1],format="%m/%d/%y")

#====================================================================================================================#



### ------------------------------------------------------------------------------------------------------
### ---------------------- Do not modified any thing below this line unless you know what you are doing
### ------------------------------------------------------------------------------------------------------


#### ................. initiate calibration [M, K]
if(Itr[1]>1){Itr = c(Itr[1]-1,Itr); append=T} #<<-------------------*** set to previous step ***
param.num=1+length(RHESSYS_PARAMS) #<<----- first col is for itr index
param = matrix(NA, length(Itr), param.num); colnames(param)=c('itr', RHESSYS_PARAMS)
fittness = array(NA,dim=c(length(FITTNESS_NAMES)+1,length(Itr), length(RHESSYS_PARAMS)+2) ); ##<forms of fittness[10]+1>, <itr>, <holding itr-index + param + grand-fittness>
param[1,(1:length(RHESSYS_PARAMS))+1]= iniParam
param.fitting = match(param.fittingNames,colnames(param)) 


##------------------ 
i=1; 
#script_to_output = paste(script_to_rhessys,"/",rhessysFolder,"/", outputFolder,sep="") ## this is wrong if the script is in proj/rhessys/script
script_to_output = outputFolder
if(!append){
	### start up first step
	submitSimulation = '#!/bin/bash'
	submitSimulation = c(submitSimulation, paste(
		'sbatch', 
		'-o', paste(script_to_output,'/log.txt',sep=''),
		paste('-J p', psbUniqueID,sep=''), # "sbatch -J" refers to job name; "-n" means something else!
		'--export=v=\'',
		RHESSys_uncalibrated_arguments,
		paste("-pre ", script_to_output,"/rhessys1",sep=""),
		paste("-s", param[i,'s1'], param[i,'s2'], param[i,'s3']),
		paste("-sv", param[i,'sv1'], param[i,'sv2']),
		paste("-gw",param[i,'gw1'],param[i,'gw2']),	
		ifelse('snowEs'%in%param.fittingNames, paste("-snowEs",param[i,'snowEs']), ''),
		ifelse('snowTs'%in%param.fittingNames, paste("-snowTs",param[i,'snowT']), ''),
		ifelse('RTz'%in%param.fittingNames, paste("-rtz",param[i,'RTz']), ''),
		ifelse('CAPr'%in%param.fittingNames, paste("-capr",param[i,'CAPr']), ''),
		'\'',
		'SLURM_script.sh',
		sep=" "
	))
	write(submitSimulation,paste(script_to_rhessys,"/parallelRun", psbUniqueID,".sh",sep="")) 
	system(paste("sh ",script_to_rhessys,"/parallelRun", psbUniqueID,".sh",sep=""));

	## on BSUB system	
		# job = as.numeric(system(paste("bjobs -J parallelRun", psbUniqueID," | awk 'END{print NR}'",sep=''),intern=T))
		# while(job>0){
			# Sys.sleep(180)#second
			# job = as.numeric(system(paste("bjobs -J parallelRun", psbUniqueID," | awk 'END{print NR}'",sep=''),intern=T)) 
		# }#while
		
	## on SBATCH/SLURM system
		# "squeue -n" refers to job name
		countWaitTime = 0
		job = as.numeric(system(paste("squeue -n p", psbUniqueID,' | awk \'END{print NR}\'',sep=''),intern=T))-1
		while(job>0){
			Sys.sleep(300)#second
			countWaitTime = countWaitTime + 1
			job = as.numeric(system(paste("squeue -n p", psbUniqueID,' | awk \'END{print NR}\'',sep=''),intern=T))-1
		}#while
		WaitTime = 300 * countWaitTime; 
	
	rhessys = read.table(paste(script_to_output,"/rhessys",Itr[1],"_basin.daily",sep=""),header=T) ##<<--------
	system(paste('rm ', script_to_output,'/*.hourly',sep=""))
	system(paste('rm ', script_to_output,'/*.monthly',sep=""))
	system(paste('rm ', script_to_output,'/*.yearly',sep=""))
	system(paste('rm ', script_to_output,'/*.params',sep=""))
}else{
	#.. assuming rhessys_basin.daily is completed in previous step
	rhessys = read.table(paste(script_to_output,"/rhessys",Itr[1],"_basin.daily",sep=""),header=T)
	
	if(file.exists(paste(script_to_output,"/param_list.csv",sep="")) ){
		tmp = read.csv(paste(script_to_output,"/param_list.csv",sep="") )
		tmpCond = tmp[,'takeIt']==1
		if(sum(tmpCond)>0){
		 	param[1,] = as.numeric(tmp[tmpCond,][sum(tmpCond),c('itr', RHESSYS_PARAMS)])
		}
	}#if
	WaitTime = 300
}#if
rhessys.date=as.Date(paste(rhessys[,1], rhessys[,2], rhessys[,3],sep="-"),format="%d-%m-%Y") 

	
tmp = intersectDate(list(rhessys.date, calobs.date0, period)) ## <<--- return daily series
rhessys.dailytimeSeriesMatch = match(tmp, rhessys.date)
calobs.dailytimeSeriesMatch = match(tmp, calobs.date0)
calobs.dailytimeSeries = dailyTimeSeries(tmp)
matchYears=range(calobs.dailytimeSeries $year)



fittness_Overall = function(fittnessValues,choice_){
			
	# exp and gamma based
	fittnessLikilhood = c(
		dgamma(abs(fittnessValues['bias']),shape=betaShape1['bias'], scale=1), # bias
		dgamma(abs(fittnessValues['wbias']),shape=betaShape1['wbias'], scale=1), # wbias
		dgamma(abs(fittnessValues['sbias']),shape=betaShape1['sbias'], scale=1), # sbias
		dexp(1-fittnessValues['inversedweeklyNSE'], rate =betaShape1['inversedweeklyNSE']), # daily NSE => inversed NSE
		dexp(1-fittnessValues['weeklyNSE'], rate =betaShape1['weeklyNSE']), # weekly NSE
		dexp(1-fittnessValues['monthlyNSE'], rate =betaShape1['monthlyNSE']), # monthly NSE
		dexp(1-fittnessValues['yearlyNSE'], rate =betaShape1['yearlyNSE']), # yearly NSE
		dexp(1-fittnessValues['weeklyCDFfitr2'], rate =betaShape1['weeklyCDFfitr2']), # daily log NSE ==> weeklyCDFfit
		dexp(1-fittnessValues['weeklyLogNSE'], rate =betaShape1['weeklyLogNSE']), # weekly log NSE
		dgamma(abs(fittnessValues['ETbias']),shape=betaShape1['ETbias'], scale=1),
		dexp(1-fittnessValues['dailyNSE'], rate =betaShape1['dailyNSE']), # daily NSE
		dexp(1-fittnessValues['dailyLogNSE'], rate =betaShape1['dailyLogNSE']), # daily log NSE
		dgamma(abs(fittnessValues['flashCOMP']),shape=betaShape1['flashCOMP'], scale=1) #	
	); names(fittnessLikilhood) = FITTNESS_NAMES
		
	likilhood = prod(fittnessLikilhood[choice_])
	
	return<-list(
		fittnessLikilhood=fittnessLikilhood, 
		likilhood=likilhood, 
		Loglikilhood=log(likilhood))
	
}#function

	
tmp_fittnessInfo=modelFittness( calobs[calobs.dailytimeSeriesMatch, obsCol], rhessys[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries) 	
tmp_annualInfo=modelBehavior(rhessys[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries) 
write.csv(tmp_annualInfo$AnnualTable, paste(script_to_output,"/rhessys",Itr[i],"_rhessys_annualTable.csv",sep=""),row.names=F ) # the first iteration

fithold = fittness_Overall(tmp_fittnessInfo$FittnessList, fittnessChoice) 
fittness[,i, param.num+1] = c(tmp_fittnessInfo$FittnessList[FITTNESS_NAMES] ,fithold $Loglikilhood) ##<---- initial one: save fittness at the last 
if(!is.finite(fittness[length(FITTNESS_NAMES)+1,i, param.num+1])){fittness[length(FITTNESS_NAMES)+1,i, param.num+1]=-1000000000000}


MCMCoutputFile = paste(script_to_rhessys,'/', MCMC_output_Prefix,'_', outputFolder,"_itr",Itr[1],"_",Itr[length(Itr)],"_fittingEvaluation_", matchYears[1], "_",matchYears[2],".csv",sep="")
titleNames=c(
	"jobID","itrID",
	names(tmp_fittnessInfo$FittnessList),
	RHESSYS_PARAMS,
	names(tmp_annualInfo$AnnualList),
	"Loglikilhood")


write(titleNames, MCMCoutputFile,sep=",",ncolumns=length(titleNames)) 
rhessysTablePrevious=rep(NA,length(titleNames))
rhessysTablePrevious=c(
	i, Itr[i],
	tmp_fittnessInfo$FittnessList,
	param[1, RHESSYS_PARAMS],
	tmp_annualInfo$AnnualList,
	fithold$Loglikilhood);
write( rhessysTablePrevious, MCMCoutputFile,sep=",",ncolumns=length(titleNames),append=T)



# note that
# fittness = array(NA,dim=c(11,length(Itr), param.num+1) ); ##fit+1, itr, #param+final

###------------------------------------------
if(length(Itr)>1){
	NONacceptedCount = 0 ### problems: not parameter specific (should it be?) and 2 no reset even if accepted and 3 not actived!!
	for(i in 2:length(Itr)){
		
		param[i,]=param[i-1,]
		param[i,1]=i
		
		accepted = rep(F,length(param.fitting))
		paramlist_title = c('itr','param','kk', RHESSYS_PARAMS, FITTNESS_NAMES,'Loglikilhood','takeIt')
		write(paramlist_title, paste(script_to_output,"/rhessys",Itr[i],'_param_list.csv',sep=''), ncolumns=length(paramlist_title),sep=","  ) ## write out run list 
		
		RhessysTable = matrix(NA,length(param.fitting),length(titleNames))
		for(j in 1:length(param.fitting) ){
			
			param.core = matrix(NA,numCore,length(RHESSYS_PARAMS)+1); colnames(param.core)=c('itr', RHESSYS_PARAMS)
			for(kk in 1:numCore){
				param.core[kk,] = param[i,]
			}#kk
			
			cond=0;
			if(param.fittingNames[j] %in% c('gw1', 's3', 'gw2','pondz','snowT','RTz','CAPr','snowEs')){ #need to change by names
				## single value propose
				while(sum(cond)<numCore){
					param.tmp = rnorm(10*numCore, param[i-1,param.fitting[j]], paramRange[param.fitting[j]]*0.1 )
					cond = param.tmp<=paramBoundary[param.fitting[j],2] & param.tmp>=paramBoundary[param.fitting[j],1]
				}
				param.core[,param.fitting[j]]=param.tmp[cond][1:numCore]
			}else{
				## multivariant propose
				if(param.fittingNames[j]=='s1'){
					### lateral
					sigma =matrix(c(
						(paramBaseSD[2] + NONacceptedCount*paraminSD[2])^2,
						-0.3*(paramBaseSD[2] + NONacceptedCount*paraminSD[2])*(paramBaseSD[3] + NONacceptedCount*paraminSD[3]),
						-0.3*(paramBaseSD[2] + NONacceptedCount*paraminSD[2])*(paramBaseSD[3] + NONacceptedCount*paraminSD[3]),
						(paramBaseSD[3] + NONacceptedCount*paraminSD[3])^2
						),2,2,byrow=T)
					while(sum(cond)<numCore){
						param.tmp = mvrnorm(10*numCore, param[i-1,2:3], sigma)
						cond = 	param.tmp[,1]<=paramBoundary[2,2] & 
								param.tmp[,1]>=paramBoundary[2,1] & 
								param.tmp[,2]<=paramBoundary[3,2] & 
								param.tmp[,2]>=paramBoundary[3,1] &
								abs(param.tmp[,1]/param[i-1,2])<=10 &
								abs(param.tmp[,1]/param[i-1,2])>=0.1 &
								abs(param.tmp[,2]/param[i-1,3])<=10 &
								abs(param.tmp[,2]/param[i-1,3])>=0.1
					}
					param.core[,2:3]=param.tmp[cond,][1:numCore,]
				}else{
					### vertical
					sigma =matrix(c(
						(paramBaseSD[5] + NONacceptedCount*paraminSD[5])^2,
						-0.3*(paramBaseSD[5] + NONacceptedCount*paraminSD[5])*(paramBaseSD[6] + NONacceptedCount*paraminSD[6]),
						-0.3*(paramBaseSD[5] + NONacceptedCount*paraminSD[5])*(paramBaseSD[6] + NONacceptedCount*paraminSD[6]),
						(paramBaseSD[6] + NONacceptedCount*paraminSD[6])^2
						),2,2,byrow=T) 
					while(sum(cond)<numCore){
						param.tmp = mvrnorm(10*numCore, param[i-1,5:6], sigma)
						cond = 	param.tmp[,1]<=paramBoundary[5,2] & 
								param.tmp[,1]>=paramBoundary[5,1] & 
								param.tmp[,2]<=paramBoundary[6,2] & 
								param.tmp[,2]>=paramBoundary[6,1] &
								abs(param.tmp[,1]/param[i-1,5])<=10 &
								abs(param.tmp[,1]/param[i-1,5])>=0.1 &
								abs(param.tmp[,2]/param[i-1,6])<=10 &
								abs(param.tmp[,2]/param[i-1,6])>=0.1
					}
					param.core[,5:6]=param.tmp[cond,][1:numCore,]
				}
				
			}
			## ------- <<< param.core >>> is the tempory parameter generated
			submitSimulation = '#!/bin/bash'
			for(kk in 1:numCore){
				submitSimulation = c(submitSimulation, paste(
					'sbatch', 
					'-o', paste(script_to_output,'/log.txt',sep=''),
					paste('-J p', psbUniqueID,sep=''), # "sbatch -J" refers to job name; "-n" means something else!
					'--export=v=\'',
					RHESSys_uncalibrated_arguments,
					paste("-pre ",script_to_output,"/rhessys",Itr[i],"_",param.fitting[j],"_",kk,sep=""),
					paste("-s", param.core[kk,'s1'], param.core[kk,'s2'], param.core[kk,'s3']),
					paste("-sv", param.core[kk,'sv1'], param.core[kk,'sv2']),
					paste("-gw", param.core[kk,'gw1'], param.core[kk,'gw2']),
					ifelse('snowEs'%in%param.fittingNames, paste("-snowEs", param.core[kk,'snowEs']), ''),
					ifelse('snowTs'%in%param.fittingNames, paste("-snowTs", param.core[kk,'snowT']), ''),
					ifelse('RTz'%in%param.fittingNames, paste("-rtz", param.core[kk,'RTz']), ''),
					ifelse('CAPr'%in%param.fittingNames, paste("-capr", param.core[kk,'CAPr']), ''),
					'\'',
					'SLURM_script.sh',
					 sep=" "
				))			
			}#kk
			write(submitSimulation,paste(script_to_rhessys,"/parallelRun", psbUniqueID,".sh",sep=""))
			write(submitSimulation,paste(script_to_output,"/rhessys",Itr[i],"_param_",param.fitting[j],"_list.txt",sep='')) ## write out run list 
			system(paste("sh ", script_to_rhessys,"/parallelRun", psbUniqueID,".sh",sep=""));
			
			## on BSUB system	
				# job = as.numeric(system(paste("bjobs -J parallelRun", psbUniqueID," | awk 'END{print NR}'",sep=''),intern=T))
				# while(job>0){
					# Sys.sleep(180)#second
					# job = as.numeric(system(paste("bjobs -J parallelRun", psbUniqueID," | awk 'END{print NR}'",sep=''),intern=T)) 
				# }#while
			
			## on SBATCH/SLURM system
				# "squeue -n" refers to job name
				countWaitTime = 0
				job = as.numeric(system(paste("squeue -n p", psbUniqueID,' | awk \'END{print NR}\'',sep=''),intern=T))-1
				while(job>0){
					Sys.sleep(ifelse(countWaitTime>0,300, WaitTime))#second
					countWaitTime = countWaitTime + 1
					job = as.numeric(system(paste("squeue -n p", psbUniqueID,' | awk \'END{print NR}\'',sep=''),intern=T))-1
				}#while
				WaitTime = ifelse(WaitTime>86400,86400,WaitTime + 300*(countWaitTime-1))
			 	
			
			# -------------------- reading files ....
			tmpRhessysTable = matrix(NA,nrow=numCore,ncol=length(titleNames)); colnames(tmpRhessysTable)= titleNames
			tmpFitTable = matrix(NA,nrow=numCore,ncol=length(FITTNESS_NAMES) )
			tmpAcceptList = rep(0,numCore)
			for(kk in 1:numCore){
				
				tmpRhessys = read.table(paste(script_to_output,"/rhessys",Itr[i],"_",param.fitting[j],"_",kk,"_basin.daily",sep=""),header=T )
				tmp_fittnessInfo=modelFittness( calobs[calobs.dailytimeSeriesMatch, obsCol], tmpRhessys[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)
				tmp_annualInfo=modelBehavior(tmpRhessys[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries)
				write.csv(tmp_annualInfo$AnnualTable, paste(script_to_output,"/rhessys",Itr[i],"_",param.fitting[j],"_",kk,"_rhessys_annualTable.csv",sep=""),row.names=F )
				
				
				fithold = fittness_Overall(tmp_fittnessInfo$FittnessList, fittnessChoice)
				
				tmpRhessysTable[kk,]=c(
					i,sessionID,Itr[i],
					tmp_fittnessInfo$FittnessList,
					param.core[kk, RHESSYS_PARAMS],
					tmp_annualInfo$AnnualList,
					fithold $Loglikilhood)
				
				tmpFitTable[kk,]=tmp_fittnessInfo$FittnessList[FITTNESS_NAMES]
			}#kk
			system(paste('rm ', script_to_output,'/*.hourly',sep=""))
			system(paste('rm ', script_to_output,'/*.monthly',sep=""))
			system(paste('rm ', script_to_output,'/*.yearly',sep=""))
			system(paste('rm ', script_to_output,'/*.params',sep=""))
			
			topSelect = order(tmpRhessysTable[,'Loglikilhood'],decreasing=T)[1]
			fittness[,i,param.fitting[j]] = c( tmpFitTable[topSelect,], tmpRhessysTable[topSelect,'Loglikilhood'])
			tmpAcceptList[topSelect]=1
			
			### ------------------------------------------- ### -- testing (param.tmp[cond])[topSelect]
			if(  fittness[length(FITTNESS_NAMES)+1,i,param.fitting[j]]-fittness[length(FITTNESS_NAMES)+1,i-1,param.num+1]>=log(runif(1))  ){ 
				# ..... purposal acceptance
				#/* May 18, 2016:: note that log(1)=0, as long as the new logliklihood is ≥ previous, the program must take it    */
				#/* however, if current fit = -0.6, then -1.0 may be still >60% possible and -1.3 maybe 50% possible */
				### param.fit[j]<param[i-1,param.num+1] # assume it's 1-r2
				### how to compare fittness
				if(j>1){ #j is parameter
					
					### .... j > 1 (need to compare to other runs within this itr)
					if( fittness[length(FITTNESS_NAMES)+1,i,param.fitting[j]]-fittness[length(FITTNESS_NAMES)+1,i,param.fitting[j-1]]>=log(runif(1))  ){
						### .... accepted
						accepted[j-1]=F
						accepted[j]=T
						RhessysTable[j,] = tmpRhessysTable[topSelect,]
						param[i, RHESSYS_PARAMS] = param.core[topSelect, RHESSYS_PARAMS]##<<------------------- fixed
						system(paste("cp ", script_to_output,"/rhessys",Itr[i],"_",param.fitting[j],"_", topSelect,"_basin.daily ", 
							script_to_output,"/rhessys",Itr[i],"_basin.daily",sep="") ) 
					}else{
						### .... reject (resolve param)
						accepted[j]=F
						RhessysTable[j,] = rhessysTablePrevious
						tmpAcceptList[topSelect]=0
					}
				}else{
					### .... j == 1
					### .... accepted
					accepted[j]=T
					RhessysTable[j,] = tmpRhessysTable[topSelect,]
					param[i, RHESSYS_PARAMS] = param.core[topSelect, RHESSYS_PARAMS]##<<------------------- fixed
					system(paste("cp ", output,"/rhessys",Itr[i],"_",param.fitting[j],"_", topSelect,"_basin.daily ", 
						script_to_output,"/rhessys",Itr[i],"_basin.daily",sep="") ) 
				}
			}else{
				
				### .... reject (resolve param)
				accepted[j]=F
				RhessysTable[j,] = rhessysTablePrevious
				tmpAcceptList[topSelect]=0
				
				if(j==1){
					system(paste("cp ", script_to_output,"/rhessys",Itr[i]-1,'_basin.daily ', script_to_output,"/rhessys",Itr[i],"_basin.daily",sep="") ) 
				}#file exit
				
			}# end of if
			
			print(tmpFitTable)
			write.table(
				cbind(	tmpRhessysTable[,'jobID'],
						rep(param.fitting[j],numCore),
						1:numCore,
						tmpRhessysTable[, RHESSYS_PARAMS],
						tmpFitTable, 
						tmpRhessysTable[,'Loglikilhood'],
						tmpAcceptList ),
				paste(script_to_output,"/rhessys",Itr[i],"_param_list.csv",sep=''), sep="," ,append=T, row.names=F,col.names=F ) ## write out run list 
			
		}#j param.fitting
		
		if(sum(accepted)==0){
			## nothing accepted
			fittness[,i, param.num+1] = fittness[,i-1, param.num+1]
			write( rhessysTablePrevious, MCMCoutputFile,sep=",",ncolumns=length(titleNames),append=T)
		}else{
			tmpWinner = max(which(accepted==T))
			fittness[,i, param.num+1] = fittness[,i, param.fitting[tmpWinner] ]
			rhessysTablePrevious = RhessysTable[tmpWinner,]
			write( RhessysTable[tmpWinner,], MCMCoutputFile,sep=",",ncolumns=length(titleNames),append=T)
		}
		
		
	
		##-------------------- plot ---------------------##
		calobsDayFlow = calobs[calobs.dailytimeSeriesMatch,obsCol]
		calobsWeekFlow = tapply(calobsDayFlow, calobs.dailytimeSeries$woy, sum)
		calobsMonthFlow = tapply(calobsDayFlow, calobs.dailytimeSeries$month, sum)
		calobsYearFlow = tapply(calobsDayFlow, calobs.dailytimeSeries$wy, sum)
	
		rhessys = read.table(paste(script_to_output,"/rhessys",Itr[i],"_basin.daily",sep=""),header=F,skip=1)
		rhessysDayFlow =rhessys[rhessys.dailytimeSeriesMatch,19]#flow
		rhessysWeekFlow = tapply(rhessysDayFlow, calobs.dailytimeSeries$woy, sum)
		rhessysMonthFlow = tapply(rhessysDayFlow, calobs.dailytimeSeries$month, sum)
		rhessysYearFlow = tapply(rhessysDayFlow, calobs.dailytimeSeries$wy, sum)
		rhessysDayResidue = rhessysDayFlow - calobsDayFlow
		weeklyCDF=ecdf(calobsWeekFlow)
		
		## .... plot 1 (color = rhessys; black = obs)
		output = paste(script_to_output,"/rhessys",Itr[i],"_plot_", matchYears[1],"_",matchYears[2],"_style2.pdf",sep="")
			pdf(output,height=9,width=8)
			layout(matrix(1:10,nrow=5,ncol=2,byrow=T))
			par(mar=c(4,4,1,1) )
			
			yy1 = log(rhessysWeekFlow,10)
			yy2 = log(calobsWeekFlow,10)
			ymin = min(yy1,yy2)
			ymax = max(yy1,yy2)
			plot(yy2,type='l',xaxt='n',ylim=c(ymin,ymax) , xlab="",ylab="log weeklyflow",yaxt='n');lines(yy1,col='red'); axis(1, at= calobs.dailytimeSeries$ithweekisbeginWY, labels=calobs.dailytimeSeries $ithweekisbeginWYlbl,las=3);axis(2,at=round(ymin):round(ymax), labels=10^(round(ymin):round(ymax)) )
			
			yy1 = log(rhessysDayFlow,10)
			yy2 = log(calobsDayFlow,10)
			ymin = min(yy1,yy2)
			ymax = max(yy1,yy2)
			plot(yy2,type='l',xaxt='n',ylim=c(ymin,ymax),xlab="",ylab="log dailyflow" ,yaxt='n');lines(yy1,col='red'); axis(1, at= calobs.dailytimeSeries $ithdayisbeginWY, labels=calobs.dailytimeSeries $ithdayisbeginWYlbl);axis(2,at=round(ymin):round(ymax), labels=10^(round(ymin):round(ymax)) )
			
			ymin = min(rhessysYearFlow, calobsYearFlow)
			ymax = max(rhessysYearFlow, calobsYearFlow)
			plot(calobsYearFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");lines(rhessysYearFlow,col='red'); axis(1, at=1:length(calobsYearFlow), labels=calobs.dailytimeSeries $grp_wateryearYYYY )
			ymin = min(rhessysYearFlow, calobsYearFlow)
			ymax = max(rhessysYearFlow, calobsYearFlow)
			plot(rhessysYearFlow ,calobsYearFlow, ylim=c(ymin,ymax));abline(a=0,b=1,lty=2)
			
			ymin = min(rhessysMonthFlow, calobsMonthFlow)
			ymax = max(rhessysMonthFlow, calobsMonthFlow)
			plot(calobsMonthFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");lines(rhessysMonthFlow,col='red'); axis(1, at= calobs.dailytimeSeries$ithmonthisbeginWY, labels=calobs.dailytimeSeries $ithmonthisbeginWYlbl )
			ymin = min(rhessysMonthFlow, calobsMonthFlow)
			ymax = max(rhessysMonthFlow, calobsMonthFlow)
			plot(rhessysMonthFlow ,calobsMonthFlow, ylim=c(ymin,ymax));abline(a=0,b=1,lty=2)
			
			ymin = min(rhessysWeekFlow, calobsWeekFlow)
			ymax = max(rhessysWeekFlow, calobsWeekFlow)
			plot(calobsWeekFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");lines(rhessysWeekFlow,col='red'); axis(1, at= calobs.dailytimeSeries$ithweekisbeginWY, labels=calobs.dailytimeSeries $ithweekisbeginWYlbl )
			###------------------------- log 
			ymin = min(rhessysWeekFlow, calobsWeekFlow)
			ymax = max(rhessysWeekFlow, calobsWeekFlow)
			plot(rhessysWeekFlow ,calobsWeekFlow, ylim=c(ymin,ymax),xlim=c(ymin,ymax),log='xy');abline(a=0,b=1,lty=2)
			
			ymin = min(rhessysDayFlow, calobsDayFlow)
			ymax = max(rhessysDayFlow, calobsDayFlow)
			plot(calobsDayFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");lines(rhessysDayFlow,col='red'); axis(1, at= calobs.dailytimeSeries $ithdayisbeginWY, labels=calobs.dailytimeSeries $ithdayisbeginWYlbl )
			###------------------------- log 
			ymin = min(rhessysDayFlow, calobsDayFlow)
			ymax = max(rhessysDayFlow, calobsDayFlow)
			plot(rhessysDayFlow ,calobsDayFlow, ylim=c(ymin,ymax),xlim=c(ymin,ymax),log='xy' );abline(a=0,b=1,lty=2)
			dev.off()
			
			## .... plot 2
			output = paste(script_to_output,"/rhessys",Itr[i],"_plot_", matchYears[1],"_",matchYears[2],"_style1.pdf",sep="")
			pdf(output,height=6,width=6)
			layout(matrix(1:2,nrow=2))
			par(mar=c(4,4,1,1) )
			
	
			## ----- season plots
			season = seasonalPatterns(calobsMonthFlow,calobs.dailytimeSeries$grp_monthMM,1:12)
			seasonRhessys = seasonalPatterns(rhessysMonthFlow,calobs.dailytimeSeries$grp_monthMM,1:12)
			
			ymax = max(season[,4], seasonRhessys[,4])
			ymin = min(season[,5], seasonRhessys[,5])
			plot(1:12 , season[,1] ,type='n',ylim=c(ymin,ymax),ylab="monthly streamflow (mm)", xlab="months",bty='l')
			polygon(x=c(1:12,rev(1:12)), y=c(seasonRhessys[,2],rev(seasonRhessys[,3])),col=gray(0.8),border=NA )
			points(1:12, season[,1], pch=4,lwd=3)
			arrows(x0=1:12, y0= season[,2], y1= season[,3],code=3,length=0.04,angle=90)
			#arrows(x0=1:12, y0= season[,5], y1= season[,4],code=3,length=0,angle=90,lty=3)
			points(1:12, season[,5],cex=0.6)
			points(1:12, season[,4],cex=0.6)
			lines(1:12, seasonRhessys[,1],col=gray(0.4),lwd=2,lty=1)
			
			## ----- weekly plots
			minWeekCond = calobsWeekFlow>0.008
			flowpt = exp(seq(round(log(min(calobsWeekFlow[minWeekCond]))),round(log(max(calobsWeekFlow[minWeekCond]))), 0.1)) # exp{0.1*log(range)} -- heavy on small tail
			anuualCDF=matrix(NA,length(calobsYearFlow),length(flowpt))
			for(ii in 1:length(calobsYearFlow)){
				hold=ecdf(calobsWeekFlow[calobs.dailytimeSeries$grp_weekWY==calobs.dailytimeSeries $grp_wateryearYYYY[ii]])
				anuualCDF[ii,]=hold(flowpt)
			}#i
			cdfMin = colMins(anuualCDF)
			cdfMax = colMaxs(anuualCDF)
			cdfMean = colMeans(anuualCDF)
			cdfQ1= colQuants(anuualCDF,0.025)
			cdfQ3= colQuants(anuualCDF,0.975)
			
			rhessysweeklyCDF=ecdf(rhessysWeekFlow)
			rhessysanuualCDF=matrix(NA,length(calobsYearFlow),length(flowpt))
			for(ii in 1:length(calobsYearFlow)){
				hold=ecdf(rhessysWeekFlow[calobs.dailytimeSeries$grp_weekWY==calobs.dailytimeSeries $grp_wateryearYYYY[ii]])
				rhessysanuualCDF[ii,]=hold(flowpt)
			}#i
			rhessys_cdfQ1= colQuants(rhessysanuualCDF,0.025)
			rhessys_cdfQ3= colQuants(rhessysanuualCDF,0.975)
			
			plot(  (flowpt), weeklyCDF(flowpt),type='n',lwd=3 , xlab="weekly streamflow (mm)",ylab='freq',bty='l');
			polygon(x=c((flowpt),rev((flowpt))), y=c(rhessys_cdfQ1, rev(rhessys_cdfQ3)),col=gray(0.8),border=NA)
			polygon(x=c((flowpt),rev((flowpt))), y=c(cdfQ1, rev(cdfQ3)),density=20,border=NA,angle=135)
			lines( (flowpt), weeklyCDF(flowpt),lwd=3)
			lines( (flowpt), rhessysweeklyCDF(flowpt),col=gray(0.4),lwd=2) ##<<<__--
			dev.off()
		
			
			## .... plot 3
			output = paste(script_to_output,"/rhessys",Itr[i],"_plot_", matchYears[1],"_",matchYears[2],"_style3.pdf",sep="")
			pdf(output,height=6,width=6)
			par(mar=c(4,4,1,1) )
			plot(rhessysDayFlow, rhessysDayResidue)
			dev.off()
			
	}#i Itr
}## Itr check






















