
arg=commandArgs(T)


#====================================================================================================================#
FITTNESS_NAMES = c('bias','wbias','sbias','inversedweeklyNSE','weeklyNSE','monthlyNSE','yearlyNSE','weeklyCDFfitr2','weeklyLogNSE','ETbias','dailyNSE','dailyLogNSE','flashCOMP')
#exp and gamma based
betaShape1 = rep(NA,length(FITTNESS_NAMES) ); names(betaShape1)= FITTNESS_NAMES
betaShape1['bias'] = 1/10 #1/5
betaShape1['wbias'] = 1/10 #1/3
betaShape1['sbias'] = 1/10 #1/3
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
# xx = seq(0.001,1,0.001); plot(xx, dgamma(xx,shape=1/2,scale=1), type='l' ); lines(xx,dgamma(xx,shape=1/5,scale=1),col='red')
# xx = seq(0.001,1,0.001); plot(xx, dexp(1-xx,rate=6), type='l' )

fittnessChoice=rep(T, length(betaShape1)); names(fittnessChoice)= FITTNESS_NAMES
fittnessChoice['flashCOMP']=F


fittness_Overall = function(fittnessValues,choice_){
			
	# exp and gamma based
	fittnessLikilhood = c(
		#dgamma(abs(fittnessValues['bias']),shape=betaShape1['bias'], scale=1), # bias
		#dgamma(abs(fittnessValues['wbias']),shape=betaShape1['wbias'], scale=1), # wbias
		#dgamma(abs(fittnessValues['sbias']),shape=betaShape1['sbias'], scale=1), # sbias
		dnorm(abs(fittnessValues['bias']),sd=betaShape1['bias'], mean=0), # bias
		dnorm(abs(fittnessValues['wbias']),sd=betaShape1['wbias'], mean=0), # wbias
		dnorm(abs(fittnessValues['sbias']),sd=betaShape1['sbias'], mean=0), # sbias
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
	
	# return<-list(
		# fittnessLikilhood=fittnessLikilhood, 
		# likilhood=likilhood, 
		# Loglikilhood=log(likilhood))
	return <- log(likilhood)
}#function

#====================================================================================================================#
modelFittness = function( calobs_, rhessys_, timeTable_, DailyThreshold_=0){
	#print(DailyThreshold_)
	
	# timeTable_ is from LIB_dailyTimeSeries3 (a data.frame object)
	MonthLen = c(31,28,31,30,31,30,31,31,30,31,30,31)
	
	calobsDayFlow = calobs_
	calobsWeekFlow = tapply(calobsDayFlow, timeTable_$yy_woy, sum)
	calobsMonthFlow = tapply(calobsDayFlow, timeTable_$yy_month, sum)
	calobsYearFlow = tapply(calobsDayFlow, timeTable_$wy, sum)

	dailyDataQuality = (calobsDayFlow> DailyThreshold_) 
	dailyDataQuality.weight = dailyDataQuality/sum(dailyDataQuality)
	dailyobsMean = sum(dailyDataQuality.weight *calobsDayFlow)
	dailyobsSS = sum( dailyDataQuality.weight*(calobsDayFlow - dailyobsMean)^2 )
	dailyobsLogMean = sum( dailyDataQuality.weight *log(calobsDayFlow) ) 
	dailyobsLogSS = sum( dailyDataQuality.weight*(log(calobsDayFlow) - dailyobsLogMean)^2 )
	
	weeklyDataQuality = tapply(1:dim(timeTable_)[1], timeTable_$yy_woy, function(x){
			len = 7;
			return <- length(x)/len * (sum(calobsDayFlow[x]) > length(x)*DailyThreshold_)	
		})
	weeklyDataQuality.weight = weeklyDataQuality/sum(weeklyDataQuality)
	weeklyobsMean = sum(weeklyDataQuality.weight *calobsWeekFlow)
	weeklyobsSS = sum( weeklyDataQuality.weight*(calobsWeekFlow - weeklyobsMean)^2 )
	weeklyobsLogMean = sum( weeklyDataQuality.weight *log(calobsWeekFlow) ) 
	weeklyobsLogSS = sum( weeklyDataQuality.weight*(log(calobsWeekFlow) - weeklyobsLogMean)^2 )

	monthlyDataQuality = tapply(1:dim(timeTable_)[1], timeTable_$yy_month, function(x){
			len = ifelse(timeTable_$month[x[1]]==2 & timeTable_$year[x[1]]%%4==0,MonthLen[timeTable_$month[x[1]]]+1,MonthLen[timeTable_$month[x[1]]]);
			return <- length(x)/len * (sum(calobsDayFlow[x]) > length(x)*DailyThreshold_)	
		})
	monthlyDataQuality.weight = monthlyDataQuality/sum(monthlyDataQuality)
	monthlyobsMean=sum(monthlyDataQuality.weight*calobsMonthFlow)
	monthlyobsSS = sum( monthlyDataQuality.weight*(calobsMonthFlow - monthlyobsMean)^2 )
	
	yearlyDataQuality = tapply(1:dim(timeTable_)[1], timeTable_$wy, function(x){
			len = ifelse((timeTable_$year[x[1]]+1)%%4==0,366,365);
			return <- length(x)/len * (sum(calobsDayFlow[x]) > length(x)*DailyThreshold_)
		}) 
	yearlyDataQuality.weight = yearlyDataQuality/sum(yearlyDataQuality)		
	yearlyobsMean=sum(yearlyDataQuality.weight* calobsYearFlow)
	yearlyobsSS = sum( yearlyDataQuality.weight*(calobsYearFlow - yearlyobsMean)^2 )
	
	minWeekCond = calobsWeekFlow> DailyThreshold_*7
	flowpt = exp(seq(round(log(min(calobsWeekFlow[minWeekCond]))),round(log(max(calobsWeekFlow[minWeekCond]))), 0.1)) 
	weeklyCDF=ecdf(calobsWeekFlow)
	weeklyCDFresult = weeklyCDF(flowpt)
	
	dailyflowpt = exp(seq(round(log(min(calobsDayFlow))),round(log(max(calobsDayFlow))), 0.1)) 
	dailyCDF=ecdf(calobsDayFlow)
	dailyCDFresult = dailyCDF(dailyflowpt)



	#--------------------------------------------
	fittnessList = rep(NA,24)
	names(fittnessList)=c(
		"dailyNSE","dailyLogNSE","meanAnnualFlashObs","meanAnnualFlashRHESSys",
		"weeklyNSE","weeklyLogNSE","inversedweeklyNSE","weeklyCDFfitr2",
		"monthlyNSE","monthlySAE",
		"yearlyNSE","yearlySAE",
		"bias","wbias","sbias",
		"totPrecip","totET","totFlow","totFlowObs",
		"RHESSysRunoffRatio","obsRunoffRatio",'ETbias','flashCOMP','loglikelihood')
	
	rhessysDayFlow = rhessys_[,19]#flow
	rhessysDayRain = rhessys_[,35]#rain
	rhessysDayET = rhessys_[,14]+rhessys_[,16]#et

	# ... / ... daily NSE
			rhessysSS = sum( dailyDataQuality.weight*(calobsDayFlow - rhessysDayFlow)^2 )
			fittnessList['dailyNSE'] = 1 - rhessysSS/dailyobsSS
			
			# ... / ... daily log NSE
			rhessysLogSS = sum( dailyDataQuality.weight*(log(calobsDayFlow) - log(rhessysDayFlow))^2 )
			fittnessList['dailyLogNSE'] = 1 - rhessysLogSS/dailyobsLogSS


			### ---- annual flashing			
			fittnessList['meanAnnualFlashObs'] = mean(tapply(1:dim(timeTable_)[1], timeTable_$wy, function(x){
				len = length(x); return <- sum(abs(calobsDayFlow[x[2:len]] - calobsDayFlow[x[1:(len-1)]])) / sum(calobsDayFlow[x])
			}))
			fittnessList['meanAnnualFlashRHESSys'] = mean(tapply(1:dim(timeTable_)[1], timeTable_$wy, function(x){
				len = length(x); return <- sum(abs(rhessysDayFlow[x[2:len]] - rhessysDayFlow[x[1:(len-1)]])) / sum(rhessysDayFlow[x])
			}))			
					
				
		# ... weekly
			rhessysWeekFlow = tapply(rhessysDayFlow, timeTable_$yy_woy, sum)
		
			# ... / ... NSE
			rhessysSS = sum( weeklyDataQuality.weight*(calobsWeekFlow - rhessysWeekFlow)^2 )
			fittnessList['weeklyNSE'] = 1 - rhessysSS/weeklyobsSS
			
			# ... / ... log NSE
			rhessysLogSS = sum( weeklyDataQuality.weight*(log(calobsWeekFlow) - log(rhessysWeekFlow))^2 )
			fittnessList['weeklyLogNSE'] = 1 - rhessysLogSS/weeklyobsLogSS
			
			# ... / ... inversedNSE 
			fittnessList['inversedweeklyNSE'] = 1 - sum( weeklyDataQuality.weight*(1/calobsWeekFlow - 1/rhessysWeekFlow)^2 ) / 
				sum( weeklyDataQuality.weight*(1/calobsWeekFlow - mean(1/calobsWeekFlow))^2 ) 
			
			# ... / ... weekly CDF fit
			rhessysweeklyCDF=ecdf(rhessysWeekFlow[minWeekCond])
			rhessysweeklyCDFresult = rhessysweeklyCDF(flowpt)
			fittnessList['weeklyCDFfitr2'] = 1-5*sum((weeklyCDFresult-rhessysweeklyCDFresult)^2 ) / sum((weeklyCDFresult- mean(weeklyCDFresult))^2 ) 
		
		
		# ... monthly
			rhessysMonthFlow = tapply(rhessysDayFlow, timeTable_$yy_month, sum)
			rhessysSS = sum( monthlyDataQuality.weight*(calobsMonthFlow - rhessysMonthFlow)^2 )
			fittnessList['monthlyNSE']  = 1 - rhessysSS/monthlyobsSS
			fittnessList['monthlySAE']  = sum(monthlyDataQuality.weight*abs(calobsMonthFlow - rhessysMonthFlow))
			
		
		# ... yearly (water year)
			rhessysYearFlow = tapply(rhessysDayFlow, timeTable_$wy, sum)
			rhessysYearET = tapply(rhessysDayET, timeTable_$wy, sum)
			rhessysYearRain = tapply(rhessysDayRain, timeTable_$wy, sum)
			rhessysSS = sum( yearlyDataQuality.weight*(calobsYearFlow - rhessysYearFlow)^2 )
			fittnessList['yearlyNSE'] = 1 - rhessysSS/yearlyobsSS
			fittnessList['yearlySAE'] = sum(yearlyDataQuality.weight*abs(calobsYearFlow - rhessysYearFlow))
			
		
		### ... bias 
			#fittnessList['bias'] = sum(yearlyDataQuality.weight * (rhessysYearFlow-calobsYearFlow)/calobsYearFlow) ## mean annual bias
			#fittnessList['bias'] = sum(rhessysYearFlow-calobsYearFlow)/sum(calobsYearFlow) ## all years together
			#fittnessList['bias'] = mean( (rhessysYearFlow-calobsYearFlow)/calobsYearFlow ) ## each year
			fittnessList['bias'] = mean( abs(rhessysYearFlow-calobsYearFlow)/calobsYearFlow ) ## each year
	
			monthlybiasMM = tapply(timeTable_$month, timeTable_$yy_month, mean)	
				selectcond = monthlybiasMM%in%c(12,1,2); 
			fittnessList['wbias'] = sum(rhessysMonthFlow[selectcond] - calobsMonthFlow[selectcond])/sum(calobsMonthFlow[selectcond]) # all winter months together
				selectcond = monthlybiasMM%in%c(6,7,8); 
			fittnessList['sbias'] = sum(rhessysMonthFlow[selectcond] - calobsMonthFlow[selectcond])/sum(calobsMonthFlow[selectcond]) # all summer months together

		
		# total flux and ratios "totPrecip","totET","totFlow","totFlowObs","RHESSysRunoffRatio","obsRunoffRatio",'ETbias','flashCOMP')
			fittnessList['totPrecip'] = sum(rhessysYearRain)
			fittnessList['totET'] = sum(rhessysYearET)
			fittnessList['totFlow'] = sum(rhessysYearFlow)
			fittnessList['totFlowObs'] = sum(calobsYearFlow)
			fittnessList['RHESSysRunoffRatio'] = mean(rhessysYearFlow/rhessysYearRain)
			fittnessList['obsRunoffRatio'] = mean(calobsYearFlow/rhessysYearRain) 

			approxET = sum(rhessysDayRain) - sum(calobsYearFlow)
			fittnessList['ETbias'] = (sum(rhessysDayET)-approxET)/approxET
			fittnessList['flashCOMP'] = fittnessList['meanAnnualFlashRHESSys']-fittnessList['meanAnnualFlashObs']
			#fittnessList['wbias'] = fittnessList['flashCOMP']/fittnessList['meanAnnualFlashObs'] ## do it for now
		## 
		fittnessList['loglikelihood'] = fittness_Overall(fittnessList, fittnessChoice)
		
		## these below will go away in the future
		MCMC_fittnessList = rep(NA,10)
		MCMC_fittnessList[1] = fittnessList['bias']
		MCMC_fittnessList[2] = fittnessList['wbias']
		MCMC_fittnessList[3] = fittnessList['sbias']
		MCMC_fittnessList[4] = fittnessList['dailyNSE']
		MCMC_fittnessList[5] = fittnessList['weeklyNSE']
		MCMC_fittnessList[6] = fittnessList['monthlyNSE']
		MCMC_fittnessList[7] = fittnessList['yearlyNSE']
		MCMC_fittnessList[8] = fittnessList['weeklyCDFfitr2']
		MCMC_fittnessList[9] = fittnessList['weeklyLogNSE']
		MCMC_fittnessList[10] = fittnessList['ETbias']

		MCMC_fittnessList2 = rep(NA,13)
		MCMC_fittnessList2[1] = fittnessList['bias']
		MCMC_fittnessList2[2] = fittnessList['wbias']
		MCMC_fittnessList2[3] = fittnessList['sbias']
		MCMC_fittnessList2[4] = fittnessList['inversedweeklyNSE'] 
		MCMC_fittnessList2[5] = fittnessList['weeklyNSE']
		MCMC_fittnessList2[6] = fittnessList['monthlyNSE']
		MCMC_fittnessList2[7] = fittnessList['yearlyNSE']
		MCMC_fittnessList2[8] = fittnessList['weeklyCDFfitr2'] 
		MCMC_fittnessList2[9] = fittnessList['weeklyLogNSE']
		MCMC_fittnessList2[10] = fittnessList['ETbias']
		MCMC_fittnessList2[11] = fittnessList['dailyNSE']
		MCMC_fittnessList2[12] = fittnessList['dailyLogNSE']
		MCMC_fittnessList2[13] = fittnessList['flashCOMP']
		

	return<-list(
		FittnessList=fittnessList,
		MCMC_fittnessList= MCMC_fittnessList,
		MCMC_fittnessList2=MCMC_fittnessList2
	)
	
}#




	
	
	
	
