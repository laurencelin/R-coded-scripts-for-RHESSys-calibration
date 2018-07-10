
arg=commandArgs(T)

modelBehavior = function( rhessys_, dailytimeSeries_ ){
	
	mostcol = dim(rhessys_)[2]
	# ... assume rhessys_ is a matrix and already "matched"
	
	# ... rhessys_ balance
	brhessys_DayFlow =rhessys_[,19]#flow
	brhessys_DayRain =rhessys_[,35]#rain
	brhessys_DayET =rhessys_[,14]+rhessys_[,16]#et
	
	brhessys_DayInf = rhessys_[,43] #ifelse(43<=mostcol,rhessys_[,43],rep(0,nrow(rhessys_)))# recharge (infiltration) <------ ifelse
	brhessys_DayUnsatDrain =rhessys_[,12]# unsat drain
	brhessys_DayCap =rhessys_[,13]# cap		

	brhessys_DayRZ =rhessys_[,9]#rz
	brhessys_DayUnsat =rhessys_[,10]#unsat
	brhessys_DaySatdef =rhessys_[,8]#satdef
	brhessys_DaySatz =rhessys_[,7]#satz
	
	brhessys_DayCanopy =rhessys_[,27]# canopy store
	brhessys_DayLitter =rhessys_[,26]# litter store
	brhessys_DayGWS =rhessys_[,23]# groundwater store
	brhessys_DayDets =rhessys_[,24]# detention store
	
	brhessys_DayPSN =rhessys_[,20]# psn
	brhessys_DaySatArea =rhessys_[,25]# sat area
	brhessys_DayReturn =rhessys_[,18]# return
	brhessys_DayBaseflow =rhessys_[,17]# baseflow (sub-surface)
	brhessys_DayGWq =rhessys_[,22]# gwq

	### ----------------------------------------- model behavorial assessment 
	# ... annual flux + balance
	brhessys_YearFlow = tapply(brhessys_DayFlow, dailytimeSeries_$year, sum)
	brhessys_YearRain = tapply(brhessys_DayRain, dailytimeSeries_$year, sum)
	brhessys_YearET = tapply(brhessys_DayET, dailytimeSeries_$year, sum)
	
	wyFirstDate = tapply(1:dim(dailytimeSeries_)[1], dailytimeSeries_$year, function(x){
		cond = dailytimeSeries_$month[x]==10 & dailytimeSeries_$day[x]==1;
		return <- ifelse(sum(cond)>0, x[cond], x[1])
	})
	wyLastDate = tapply(1:dim(dailytimeSeries_)[1], dailytimeSeries_$year, function(x){
		cond = dailytimeSeries_$month[x]==9 & dailytimeSeries_$day[x]==30;
		return <- ifelse(sum(cond)>0, x[cond], x[length(x)])
	})
	
	brhessys_YearRZ = brhessys_DayRZ[wyLastDate] - brhessys_DayRZ[wyFirstDate]
	brhessys_YearUnsat = brhessys_DayUnsat[wyLastDate] - brhessys_DayUnsat[wyFirstDate]
	brhessys_YearSatdef = brhessys_DaySatdef[wyLastDate] - brhessys_DaySatdef[wyFirstDate]
	brhessys_YearCanopy = brhessys_DayCanopy[wyLastDate] - brhessys_DayCanopy[wyFirstDate]
	brhessys_YearLitter = brhessys_DayLitter[wyLastDate] - brhessys_DayLitter[wyFirstDate]
	brhessys_YearGWS = brhessys_DayGWS[wyLastDate] - brhessys_DayGWS[wyFirstDate]
	brhessys_YearDets = brhessys_DayDets[wyLastDate] - brhessys_DayDets[wyFirstDate]
	
	brhessys_YearBalance = brhessys_YearRain-brhessys_YearET-brhessys_YearFlow-brhessys_YearRZ-brhessys_YearUnsat+brhessys_YearSatdef-brhessys_YearCanopy-brhessys_YearLitter-brhessys_YearGWS-brhessys_YearDets
	
	brhessys_YearInf = tapply(brhessys_DayInf, dailytimeSeries_$year, sum) 
	brhessys_YearUnsatDrain = tapply(brhessys_DayUnsatDrain, dailytimeSeries_$year, sum)
	brhessys_YearCap = tapply(brhessys_DayCap, dailytimeSeries_$year, sum)
	brhessys_YearGWq = tapply(brhessys_DayGWq, dailytimeSeries_$year, sum)
	brhessys_YearPSN = tapply(brhessys_DayPSN, dailytimeSeries_$year, sum)
	brhessys_YearSatz = tapply(brhessys_DaySatz, dailytimeSeries_$year, mean)
	
	
	# ... weekly flux
	brhessys_WeekPrecip = tapply(brhessys_DayRain, dailytimeSeries_$yy_woy, sum)
	brhessys_WeekStreamflow = tapply(brhessys_DayFlow, dailytimeSeries_$yy_woy, sum)
	brhessys_WeekReturn = tapply(brhessys_DayReturn, dailytimeSeries_$yy_woy, sum)
	brhessys_WeekGWq = tapply(brhessys_DayGWq, dailytimeSeries_$yy_woy, sum)
	brhessys_WeekBaseflow = brhessys_WeekStreamflow-brhessys_WeekReturn-brhessys_WeekGWq  #subsurface
	brhessys_WeekSatArea = tapply(brhessys_DaySatArea, dailytimeSeries_$yy_woy, mean)
	brhessys_WeekSatz = tapply(brhessys_DaySatz, dailytimeSeries_$yy_woy, mean)
	brhessys_WeekUnsat = tapply(brhessys_DayUnsat, dailytimeSeries_$yy_woy, mean)
	
	hhresult = simplify2array(tapply(1:dim(dailytimeSeries_)[1], dailytimeSeries_$year, function(x){
		brhessys_WeekPrecip = tapply(brhessys_DayRain[x], dailytimeSeries_$yy_woy[x], sum)
		brhessys_WeekStreamflow = tapply(brhessys_DayFlow[x], dailytimeSeries_$yy_woy[x], sum)
		brhessys_WeekReturn = tapply(brhessys_DayReturn[x], dailytimeSeries_$yy_woy[x], sum)
		brhessys_WeekGWq = tapply(brhessys_DayGWq[x], dailytimeSeries_$yy_woy[x], sum)
		brhessys_WeekBaseflow = brhessys_WeekStreamflow-brhessys_WeekReturn-brhessys_WeekGWq  #subsurface
		
		brhessys_WeekSatArea = tapply(brhessys_DaySatArea[x], dailytimeSeries_$yy_woy[x], mean)
		brhessys_WeekSatz = tapply(brhessys_DaySatz[x], dailytimeSeries_$yy_woy[x], mean)
		brhessys_WeekUnsat = tapply(brhessys_DayUnsat[x], dailytimeSeries_$yy_woy[x], mean)
		
		return <- c(
			cor(brhessys_WeekReturn/brhessys_WeekStreamflow, brhessys_WeekPrecip),
			cor(brhessys_WeekBaseflow/brhessys_WeekStreamflow, brhessys_WeekPrecip),
			cor(brhessys_WeekGWq/brhessys_WeekStreamflow, brhessys_WeekPrecip),
			cor(brhessys_WeekUnsat, brhessys_WeekPrecip),
			cor(brhessys_WeekSatz, brhessys_WeekPrecip),
			cor(brhessys_WeekSatArea, brhessys_WeekPrecip),
			min(brhessys_DaySatArea),
			median(brhessys_DaySatArea),
			max(brhessys_DaySatArea),
			sd(brhessys_DaySatArea)/mean(brhessys_DaySatArea)
		)
	})) ## years by columns; variable by rows
	

		
		annualTable=cbind(
			tapply(dailytimeSeries_$year,INDEX=dailytimeSeries_$year,mean),
			brhessys_YearRain,
			brhessys_YearET,
			brhessys_YearFlow,
			brhessys_YearCanopy,
			brhessys_YearLitter,
			brhessys_YearDets,
			brhessys_YearRZ,
			brhessys_YearUnsat,
			brhessys_YearSatdef,
			brhessys_YearGWS,
			(brhessys_YearET+ brhessys_YearFlow)/brhessys_YearRain, # value 1 is good
			##----------------------
			brhessys_YearInf,
			brhessys_YearUnsatDrain, 
			brhessys_YearCap,
			brhessys_YearGWq,
			brhessys_YearPSN,
			brhessys_YearSatz,
			##----------------------
			t(hhresult)
			##----------------------
		);
		colnames(annualTable)=c(
		"year","rain","rhessys_ET","rhessys_Flow","rhessys_Canopy","rhessys_Litter","rhessys_Detention","rhessys_RZ","rhessys_Unsat","rhessys_Satdef","rhessys_GW","rhessys_Balance",
		"annualInf","annualUnsatDrain","annualCap","annualGWq","annualPSN","annualSatz",
		"returnflow_precip","subflow_precip","gwq_precip","unsat_precip","satz_precip","satArea_precip","minSatArea","medianSatArea","maxSatArea","SDSatArea"
		)
		
		annualList=c(
			mean((brhessys_YearET+ brhessys_YearFlow)/brhessys_YearRain),
			mean(brhessys_YearInf),
			mean(brhessys_YearUnsatDrain),
			mean(brhessys_YearCap),
			mean(brhessys_YearSatz),
			mean(brhessys_YearPSN),
			
			mean(brhessys_YearFlow/brhessys_YearRain),
			mean(brhessys_YearET/brhessys_YearRain),
			cor(brhessys_YearET,brhessys_YearRain),
			cor(brhessys_YearFlow,brhessys_YearRain),
			
			mean(brhessys_YearGWq/brhessys_YearFlow),
			
			cor(brhessys_WeekReturn/brhessys_WeekStreamflow, brhessys_WeekPrecip),
			cor(brhessys_WeekBaseflow/brhessys_WeekStreamflow, brhessys_WeekPrecip),
			cor(brhessys_WeekGWq/brhessys_WeekStreamflow, brhessys_WeekPrecip),
			cor(brhessys_WeekUnsat, brhessys_WeekPrecip),
			cor(brhessys_WeekSatz, brhessys_WeekPrecip),
			cor(brhessys_WeekSatArea, brhessys_WeekPrecip),
			mean(brhessys_DaySatArea),
			sd(brhessys_DaySatArea)/mean(brhessys_DaySatArea)
		)
		
		names(annualList)=c('ETFpcp','inf','unsatDrain','cap','satz','PSN','runoffRatio','ETratio','ETpcpCor','FlowpcpCor','GWQratio','returnPcp','subflowPcp','GWpcp','UnsatPcp','satzPcp','satAreaPcp','satArea','satAreaSD')
		
		return<-list(
			AnnualTable=annualTable,
			AnnualList=annualList
		)
		# add summary
	
}#end of function
