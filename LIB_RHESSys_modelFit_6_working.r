source("~/Dropbox/LIB_Rscript/LIB_misc.r")
source("~/Dropbox/LIB_Rscript/LIB_dailytimeseries2.r")
source("~/Dropbox/LIB_Rscript/LIB_hydro.r")

arg=commandArgs(T)

modelFittness = function( calobs_, rhessys_, dailytimeSeries_, DailyThreshold_=0){
	print(DailyThreshold_)
	
	calobsDayFlow = calobs_
	calobsWeekFlow =grpSums(calobsDayFlow, dailytimeSeries_$grp_week )
	calobsMonthFlow =grpSums(calobsDayFlow, dailytimeSeries_$grp_month )
	calobsYearFlow =grpSums(calobsDayFlow, dailytimeSeries_$grp_wateryear )


	dailyDataQuality = (calobsDayFlow> DailyThreshold_) 
	dailyDataQuality.weight = dailyDataQuality/sum(dailyDataQuality)
	dailyobsMean = sum(dailyDataQuality.weight *calobsDayFlow)
	dailyobsSS = sum( dailyDataQuality.weight*(calobsDayFlow - dailyobsMean)^2 )
	dailyobsLogMean = sum( dailyDataQuality.weight *log(calobsDayFlow) ) 
	dailyobsLogSS = sum( dailyDataQuality.weight*(log(calobsDayFlow) - dailyobsLogMean)^2 )
	
	weeklyDataQuality = dailytimeSeries_$grp_weekLen/7 * (calobsWeekFlow> DailyThreshold_*7) 
	weeklyDataQuality.weight = weeklyDataQuality/sum(weeklyDataQuality)
	weeklyobsMean = sum(weeklyDataQuality.weight *calobsWeekFlow)
	weeklyobsSS = sum( weeklyDataQuality.weight*(calobsWeekFlow - weeklyobsMean)^2 )
	weeklyobsLogMean = sum( weeklyDataQuality.weight *log(calobsWeekFlow) ) 
	weeklyobsLogSS = sum( weeklyDataQuality.weight*(log(calobsWeekFlow) - weeklyobsLogMean)^2 )

	monthlyDataQuality = dailytimeSeries_$grp_monthLen/dailytimeSeries_$grp_monthDefaultLen * (calobsMonthFlow> dailytimeSeries_$grp_monthDefaultLen* DailyThreshold_)
	monthlyDataQuality.weight = monthlyDataQuality/sum(monthlyDataQuality)
	monthlyobsMean=sum(monthlyDataQuality.weight*calobsMonthFlow)
	monthlyobsSS = sum( monthlyDataQuality.weight*(calobsMonthFlow - monthlyobsMean)^2 )
	
	yearlyDataQuality = dailytimeSeries_$grp_wateryearLen/dailytimeSeries_$grp_wateryearDefaultLen * (calobsYearFlow> dailytimeSeries_$grp_wateryearDefaultLen* DailyThreshold_)
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
	fittnessList = rep(NA,26)
	names(fittnessList)=c('one','two','three',
		"dailyNSE","dailyLogNSE","meanAnnualFlushObs","meanAnnualFlushRHESSys",
		"weeklyNSE","weeklyLogNSE","inversedweeklyNSE","weeklyCDFfitr2",
		"monthlyNSE","monthlySAE",
		"yearlyNSE","yearlySAE",
		"bias","wbias","sbias",
		"totPrecip","totET","totFlow","totFlowObs","RHESSysRunoffRatio","obsRunoffRatio",'ETbias','flashCOMP')
	
	rhessysDayFlow = rhessys_[,19]#flow
	rhessysDayRain = rhessys_[,35]#rain
	rhessysDayET = rhessys_[,14]+rhessys_[,16]#et

	# ... / ... NSE
			rhessysSS = sum( dailyDataQuality.weight*(calobsDayFlow - rhessysDayFlow)^2 )
			rhessysNSE = 1 - rhessysSS/dailyobsSS
			rhessysDayResidue = calobsDayFlow - rhessysDayFlow
			
			# ... / ... log NSE
			rhessysLogSS = sum( dailyDataQuality.weight*(log(calobsDayFlow) - log(rhessysDayFlow))^2 )
			rhessysLogNSE = 1 - rhessysLogSS/dailyobsLogSS
		
			# ... / ... SAE --> change to flushness index (annually) << **** >>
			#rhessysSAE = sum( abs(calobsDayFlow - rhessysDayFlow)) #<<--------------- original
			flushness = matrix(NA, nrow=length(unique(dailytimeSeries_$grp_wateryear)), ncol=2)
			for(fik in unique(dailytimeSeries_$grp_wateryear)){
				tmpCond = dailytimeSeries_$grp_wateryear==fik
				flushness[fik,] = c(
					sum(abs( calobsDayFlow[tmpCond][2:sum(tmpCond)]-calobsDayFlow[tmpCond][1:(sum(tmpCond)-1)] ))/sum(calobsDayFlow[tmpCond]),
					sum(abs( rhessysDayFlow[tmpCond][2:sum(tmpCond)]-rhessysDayFlow[tmpCond][1:(sum(tmpCond)-1)] ))/sum(rhessysDayFlow[tmpCond])
				)
			}#fik
			rhessysSAE = mean(flushness[,1]) #obs
		
			# ... / ... dailyCDFfit --> change to flushness index (annually) << **** >>
			#rhessysdailyCDF=ecdf(rhessysDayFlow)
			#rhessysdailyCDFresult = rhessysdailyCDF(dailyflowpt)
			#rhessysdailyCDFfit = sum(abs(dailyCDFresult-rhessysdailyCDFresult))
			rhessysdailyCDFfit = mean(flushness[,2]) #predicted
			
			# ___________ 1:3
			fittnessList[4:7]=c(rhessysNSE, rhessysLogNSE, rhessysSAE, rhessysdailyCDFfit ) #4<<------------------
				
		# ... weekly
			rhessysWeekFlow =grpSums(rhessysDayFlow, dailytimeSeries_$grp_week )
		
			# ... / ... NSE
			rhessysSS = sum( weeklyDataQuality.weight*(calobsWeekFlow - rhessysWeekFlow)^2 )
			rhessysNSE = 1 - rhessysSS/weeklyobsSS
			rhessysWeekResidue = calobsWeekFlow - rhessysWeekFlow
			
			# ... / ... log NSE
			rhessysLogSS = sum( weeklyDataQuality.weight*(log(calobsWeekFlow) - log(rhessysWeekFlow))^2 )
			rhessysLogNSE = 1 - rhessysLogSS/weeklyobsLogSS
			
			# ... / ... SAE --> change to inversedNSE << **** >>
			# rhessysSAE = sum(weeklyDataQuality.weight*abs(calobsWeekFlow - rhessysWeekFlow)) #<<--------------- original
			rhessysSAE = 1- sum( weeklyDataQuality.weight*(1/calobsWeekFlow - 1/rhessysWeekFlow)^2 ) / sum( weeklyDataQuality.weight*(1/calobsWeekFlow - mean(1/calobsWeekFlow))^2 ) 
			
			
			rhessysweeklyCDF=ecdf(rhessysWeekFlow[minWeekCond])
			rhessysweeklyCDFresult = rhessysweeklyCDF(flowpt)
			rhessysweeklyCDFfit = 1-5*sum((weeklyCDFresult-rhessysweeklyCDFresult)^2 ) / sum((weeklyCDFresult- mean(weeklyCDFresult))^2 ) # --> r2 format << **** >>
			#sum(weeklyCDFresult) #sum(abs(weeklyCDFresult-rhessysweeklyCDFresult))#/sum(weeklyCDFresult)
			
			
			# ___________ 1:3
			fittnessList[8:11]=c(rhessysNSE, rhessysLogNSE, rhessysSAE, rhessysweeklyCDFfit ) #<<-------------------
		
		
		
		
		# ... monthly
			rhessysMonthFlow =grpSums(rhessysDayFlow, dailytimeSeries_$grp_month )
			rhessysSS = sum( monthlyDataQuality.weight*(calobsMonthFlow - rhessysMonthFlow)^2 )
			rhessysNSE = 1 - rhessysSS/monthlyobsSS
			rhessysSAE = sum(monthlyDataQuality.weight*abs(calobsMonthFlow - rhessysMonthFlow))
			fittnessList[12:13]=c(rhessysNSE, rhessysSAE) #<----------------------
		
		
		
		# ... yearly (water year)
			rhessysYearFlow =grpSums(rhessysDayFlow, dailytimeSeries_$grp_wateryear )
			rhessysYearET =grpSums(rhessysDayET, dailytimeSeries_$grp_wateryear )
			rhessysYearRain = grpSums(rhessysDayRain, dailytimeSeries_$grp_wateryear )
			rhessysSS = sum( yearlyDataQuality.weight*(calobsYearFlow - rhessysYearFlow)^2 )
			rhessysNSE = 1 - rhessysSS/yearlyobsSS
			rhessysSAE = sum(yearlyDataQuality.weight*abs(calobsYearFlow - rhessysYearFlow))
			rhessysYearResidue = calobsYearFlow - rhessysYearFlow
			fittnessList[14:15]=c(rhessysNSE, rhessysSAE) #<----------------------
		
		### -----------------------------------------
		# "bias","wbias","sbias", 18
		# "runoffRatio","ETratio","ETcor","annualBalance", 22
		# "s1","s2","s3","sv1","sv2","gw1","gw2")
		# ... / ... hydro
			#condWinter = dailytimeSeries_$grp_monthMM==12|dailytimeSeries_$grp_monthMM==1|dailytimeSeries_$grp_monthMM==2
			
			#holding = allBias(calobsWeekFlow, dailytimeSeries_$grp_weekMM, rhessysWeekFlow)
			holding = allBias(calobsMonthFlow, dailytimeSeries_$grp_monthMM, rhessysMonthFlow)
			fittnessList[16:18] = c(holding$bias,holding$wbias,holding$sbias)
		
		
		# total flux and ratios
		fittnessList[19:24]=c(
			sum(rhessysYearRain),sum(rhessysYearET),sum(rhessysYearFlow),sum(calobsYearFlow), 
			mean(rhessysYearFlow/rhessysYearRain), 
			mean(calobsYearFlow/rhessysYearRain) 
		)


		approxET = sum(rhessysDayRain) - sum(calobsYearFlow)
		fittnessList[25] = (sum(rhessysDayET)-approxET)/approxET
		
		fittnessList[26] = fittnessList['meanAnnualFlushObs']-fittnessList['meanAnnualFlushRHESSys']

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
		MCMC_fittnessList2[4] = fittnessList['inversedweeklyNSE'] #<<----- 4
		MCMC_fittnessList2[5] = fittnessList['weeklyNSE']
		MCMC_fittnessList2[6] = fittnessList['monthlyNSE']
		MCMC_fittnessList2[7] = fittnessList['yearlyNSE']
		MCMC_fittnessList2[8] = fittnessList['weeklyCDFfitr2'] #<<-----8
		MCMC_fittnessList2[9] = fittnessList['weeklyLogNSE']
		MCMC_fittnessList2[10] = fittnessList['ETbias']
		MCMC_fittnessList2[11] = fittnessList['dailyNSE']
		MCMC_fittnessList2[12] = fittnessList['dailyLogNSE']
		MCMC_fittnessList2[13] = fittnessList['flashCOMP']
		

	return<-list(
		FittnessList=fittnessList[4:length(fittnessList)],
		MCMC_fittnessList= MCMC_fittnessList,
		MCMC_fittnessList2=MCMC_fittnessList2
	)
	
}#
