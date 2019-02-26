source("~/Dropbox/LIB_Rscript/LIB_misc.r")
source("~/Dropbox/LIB_Rscript/LIB_dailytimeseries2.r")
source("~/Dropbox/LIB_Rscript/LIB_hydro.r")

arg=commandArgs(T)

modelPlotStyle2 = function( calobs_, rhessys_, dailytimeSeries_, output){
	
	
	calobsDayFlow = calobs_
	calobsWeekFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_week )
	calobsMonthFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_month )
	calobsYearFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_wateryear )
	weeklyCDF=ecdf(calobsWeekFlow)

	rhessysDayFlow = rhessys_[,19]#flow
	rhessysWeekFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_week )
	rhessysMonthFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_month )
	rhessysYearFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_wateryear )
	rhessysDayResidue = rhessysDayFlow - calobsDayFlow
	
	
	pdf(output,height=9,width=8)
	layout(matrix(1:10,nrow=5,ncol=2,byrow=T))
	par(mar=c(4,4,1,1) )
	
	yy1 = log(rhessysWeekFlow,10)
	yy2 = log(calobsWeekFlow,10)
	ymin = min(yy1,yy2)
	ymax = max(yy1,yy2)
	plot(yy2,type='l',xaxt='n',ylim=c(ymin,ymax) , xlab="",ylab="log weeklyflow",yaxt='n');lines(yy1,col='red'); axis(1, at= dailytimeSeries_ $ithweekisbeginWY, labels= dailytimeSeries_ $ithweekisbeginWYlbl,las=3);axis(2,at=round(ymin):round(ymax), labels=10^(round(ymin):round(ymax)) )
	
	yy1 = log(rhessysDayFlow,10)
	yy2 = log(calobsDayFlow,10)
	ymin = min(yy1,yy2)
	ymax = max(yy1,yy2)
	plot(yy2,type='l',xaxt='n',ylim=c(ymin,ymax),xlab="",ylab="log dailyflow" ,yaxt='n');lines(yy1,col='red'); axis(1, at= dailytimeSeries_ $ithdayisbeginWY, labels= dailytimeSeries_ $ithdayisbeginWYlbl);axis(2,at=round(ymin):round(ymax), labels=10^(round(ymin):round(ymax)) )
	
	ymin = min(rhessysYearFlow, calobsYearFlow)
	ymax = max(rhessysYearFlow, calobsYearFlow)
	plot(calobsYearFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");lines(rhessysYearFlow,col='red'); axis(1, at=1:length(calobsYearFlow), labels= dailytimeSeries_ $grp_wateryearYYYY )
	ymin = min(rhessysYearFlow, calobsYearFlow)
	ymax = max(rhessysYearFlow, calobsYearFlow)
	plot(rhessysYearFlow ,calobsYearFlow, ylim=c(ymin,ymax));abline(a=0,b=1,lty=2)
	
	ymin = min(rhessysMonthFlow, calobsMonthFlow)
	ymax = max(rhessysMonthFlow, calobsMonthFlow)
	plot(calobsMonthFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");lines(rhessysMonthFlow,col='red'); axis(1, at= dailytimeSeries_ $ithmonthisbeginWY, labels= dailytimeSeries_ $ithmonthisbeginWYlbl )
	ymin = min(rhessysMonthFlow, calobsMonthFlow)
	ymax = max(rhessysMonthFlow, calobsMonthFlow)
	plot(rhessysMonthFlow ,calobsMonthFlow, ylim=c(ymin,ymax));abline(a=0,b=1,lty=2)
	
	ymin = min(rhessysWeekFlow, calobsWeekFlow)
	ymax = max(rhessysWeekFlow, calobsWeekFlow)
	plot(calobsWeekFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");lines(rhessysWeekFlow,col='red'); axis(1, at= dailytimeSeries_ $ithweekisbeginWY, labels= dailytimeSeries_ $ithweekisbeginWYlbl )
	###------------------------- log 
	ymin = min(rhessysWeekFlow, calobsWeekFlow)
	ymax = max(rhessysWeekFlow, calobsWeekFlow)
	plot(rhessysWeekFlow ,calobsWeekFlow, ylim=c(ymin,ymax),xlim=c(ymin,ymax),log='xy');abline(a=0,b=1,lty=2)
	
	ymin = min(rhessysDayFlow, calobsDayFlow)
	ymax = max(rhessysDayFlow, calobsDayFlow)
	plot(calobsDayFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");lines(rhessysDayFlow,col='red'); axis(1, at= dailytimeSeries_ $ithdayisbeginWY, labels= dailytimeSeries_ $ithdayisbeginWYlbl )
	###------------------------- log 
	ymin = min(rhessysDayFlow, calobsDayFlow)
	ymax = max(rhessysDayFlow, calobsDayFlow)
	plot(rhessysDayFlow ,calobsDayFlow, ylim=c(ymin,ymax),xlim=c(ymin,ymax),log='xy' );abline(a=0,b=1,lty=2)
	dev.off()



}#


modelPlotStyle2Multiple = function( calobs_, rhessysFlows_, dailytimeSeries_, output){
	# assuming rhessysFlows_ is a matrix of different flow time series. time is row and difference in column
	
	calobsDayFlow = calobs_
	calobsWeekFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_week )
	calobsMonthFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_month )
	calobsYearFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_wateryear )
	weeklyCDF=ecdf(calobsWeekFlow)

	rhessysDayFlow = rhessysFlows_#flows
	rhessysWeekFlow = grpSums(rhessysDayFlow[,1], dailytimeSeries_ $grp_week )
	rhessysMonthFlow =grpSums(rhessysDayFlow[,1], dailytimeSeries_ $grp_month )
	rhessysYearFlow =grpSums(rhessysDayFlow[,1], dailytimeSeries_ $grp_wateryear )
	rhessysDayResidue = rhessysDayFlow[,1] - calobsDayFlow
	
	for(i in 2:ncol(rhessysDayFlow)){
		rhessysWeekFlow = cbind(rhessysWeekFlow, grpSums(rhessysDayFlow[,i], dailytimeSeries_ $grp_week ))
		rhessysMonthFlow = cbind(rhessysMonthFlow, grpSums(rhessysDayFlow[,i], dailytimeSeries_ $grp_month ))
		rhessysYearFlow = cbind(rhessysYearFlow, grpSums(rhessysDayFlow[,i], dailytimeSeries_ $grp_wateryear ))
		rhessysDayResidue = cbind(rhessysDayResidue, rhessysDayFlow[,i] - calobsDayFlow)
	}#i
	
	rhessysDayFlowStat = colPattern(rhessysDayFlow)
	rhessysWeekFlowStat = colPattern(rhessysWeekFlow)
	rhessysMonthFlowStat = colPattern(rhessysMonthFlow)
	rhessysYearStat = colPattern(rhessysYearFlow)
	
	
	pdf(output,height=6,width=8)
	layout(matrix(1:6,nrow=3,ncol=2,byrow=T))
	par(mar=c(4,4,1,1) )
	
	
	### log weekly
	yy1 = log(rhessysWeekFlow,10)
	yy2 = log(calobsWeekFlow,10)
	ymin = min(yy1,yy2)
	ymax = max(yy1,yy2)
	plot(yy2,type='l',xaxt='n',ylim=c(ymin,ymax) , xlab="",ylab="log weeklyflow",yaxt='n');
	lines(log(rhessysWeekFlowStat[,'mean'],10),col='red'); 
	polygon(
		x=c(1:nrow(rhessysWeekFlowStat), rev(1:nrow(rhessysWeekFlowStat))), 
		y=c(log(rhessysWeekFlowStat[,'q025'],10),rev(log(rhessysWeekFlowStat[,'q975'],10))),
		border=NA,col=makeTransparent('red',50) )
	axis(1, at= dailytimeSeries_ $ithweekisbeginWY, labels= dailytimeSeries_ $ithweekisbeginWYlbl,las=3);
	axis(2,at=round(ymin):round(ymax), labels=10^(round(ymin):round(ymax)) )
	
	## log daily
	yy1 = log(rhessysDayFlow,10)
	yy2 = log(calobsDayFlow,10)
	ymin = min(yy1,yy2)
	ymax = max(yy1,yy2)
	plot(yy2,type='l',xaxt='n',ylim=c(ymin,ymax),xlab="",ylab="log dailyflow" ,yaxt='n');
	lines(log(rhessysDayFlowStat[,'mean'],10),col='red'); 
	polygon(
		x=c(1:nrow(rhessysDayFlowStat), rev(1:nrow(rhessysDayFlowStat))), 
		y=c(log(rhessysDayFlowStat[,'q025'],10),rev(log(rhessysDayFlowStat[,'q975'],10))),
		border=NA,col=makeTransparent('red',50) ) 
	axis(1, at= dailytimeSeries_ $ithdayisbeginWY, labels= dailytimeSeries_ $ithdayisbeginWYlbl);
	axis(2,at=round(ymin):round(ymax), labels=10^(round(ymin):round(ymax)) )
	
	## yearly
	ymin = min(rhessysYearFlow, calobsYearFlow)
	ymax = max(rhessysYearFlow, calobsYearFlow)
	plot(calobsYearFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");
	lines(rhessysYearStat[,'mean'],col='red'); 
	polygon(
		x=c(1:nrow(rhessysYearStat), rev(1:nrow(rhessysYearStat))), 
		y=c(rhessysYearStat[,'q025'],rev(rhessysYearStat[,'q975'])),
		border=NA,col=makeTransparent('red',50) ) 
	axis(1, at=1:length(calobsYearFlow), labels= dailytimeSeries_ $grp_wateryearYYYY )
	print(rhessysYearStat)
	
	### monthly
	ymin = min(rhessysMonthFlow, calobsMonthFlow)
	ymax = max(rhessysMonthFlow, calobsMonthFlow)
	plot(calobsMonthFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");
	lines(rhessysMonthFlowStat[,'mean'],col='red'); 
	polygon(
		x=c(1:nrow(rhessysMonthFlowStat), rev(1:nrow(rhessysMonthFlowStat))), 
		y=c(rhessysMonthFlowStat[,'q025'],rev(rhessysMonthFlowStat[,'q975'])),
		border=NA,col=makeTransparent('red',50) ) 
	axis(1, at= dailytimeSeries_ $ithmonthisbeginWY, labels= dailytimeSeries_ $ithmonthisbeginWYlbl )
	
	### weekly
	ymin = min(rhessysWeekFlow, calobsWeekFlow)
	ymax = max(rhessysWeekFlow, calobsWeekFlow)
	plot(calobsWeekFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");
	lines(rhessysWeekFlowStat[,'mean'],col='red'); 
	polygon(
		x=c(1:nrow(rhessysWeekFlowStat), rev(1:nrow(rhessysWeekFlowStat))), 
		y=c(rhessysWeekFlowStat[,'q025'],rev(rhessysWeekFlowStat[,'q975'])),
		border=NA,col=makeTransparent('red',70) ) 
	axis(1, at= dailytimeSeries_ $ithweekisbeginWY, labels= dailytimeSeries_ $ithweekisbeginWYlbl )
	
	### daily
	ymin = min(rhessysDayFlow, calobsDayFlow)
	ymax = max(rhessysDayFlow, calobsDayFlow)
	plot(calobsDayFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");
	lines(rhessysDayFlowStat[,'mean'],col='red'); 
	polygon(
		x=c(1:nrow(rhessysDayFlowStat), rev(1:nrow(rhessysDayFlowStat))), 
		y=c(rhessysDayFlowStat[,'q025'],rev(rhessysDayFlowStat[,'q975'])),
		border=NA,col=makeTransparent('red',70) ) 
	axis(1, at= dailytimeSeries_ $ithdayisbeginWY, labels= dailytimeSeries_ $ithdayisbeginWYlbl )
	dev.off()

}#



modelPlotMonthlySeason = function( calobs_, rhessys_, dailytimeSeries_, output){
	
	calobsDayFlow = calobs_
	calobsWeekFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_week )
	calobsMonthFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_month )
	calobsSeasonFlow=seasonalPatterns(calobsMonthFlow, dailytimeSeries_$grp_monthMM, 1:12)
	calobsYearFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_wateryear )
	weeklyCDF=ecdf(calobsWeekFlow)

	rhessysDayFlow = rhessys_[,19]#flow
	rhessysWeekFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_week )
	rhessysMonthFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_month )
	rhessysSeasonFlow =seasonalPatterns(rhessysMonthFlow, dailytimeSeries_$grp_monthMM, 1:12)
	rhessysYearFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_wateryear )
	rhessysDayResidue = rhessysDayFlow - calobsDayFlow
	
	
	pdf(output,height=9,width=6)
	layout(matrix(1:3,nrow=3,ncol=1,byrow=T))
	par(mar=c(4,4,1,1) )


	ymin = min(rhessysMonthFlow, calobsMonthFlow)
	ymax = max(rhessysMonthFlow, calobsMonthFlow)
	plot(calobsMonthFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");lines(rhessysMonthFlow,col='red'); axis(1, at= dailytimeSeries_ $ithmonthisbeginWY, labels= dailytimeSeries_ $ithmonthisbeginWYlbl )

	ymin = min(rhessysMonthFlow, calobsMonthFlow)
	ymax = max(rhessysMonthFlow, calobsMonthFlow)
	plot(calobsMonthFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="",log='y');lines(rhessysMonthFlow,col='red'); axis(1, at= dailytimeSeries_ $ithmonthisbeginWY, labels= dailytimeSeries_ $ithmonthisbeginWYlbl )

	ymin = min(rhessysSeasonFlow[,'q025'], calobsSeasonFlow[,'q025'])
	ymax = max(rhessysSeasonFlow[,'q975'], calobsSeasonFlow[,'q975'])
	plot(calobsSeasonFlow[,'mean'],type='l',xaxt='n',ylim=c(ymin,ymax), xlab="", ylab='monthly streamflow');
	lines(rhessysSeasonFlow[,'mean'],col='red'); axis(1, at= 1:12, labels=1:12 )
	arrows(x0=1:12, y0=calobsSeasonFlow[,'q025'], y1=calobsSeasonFlow[,'q975'], code=3, angle=90,length=0.03, lwd=2)
	arrows(x0=1:12, y0= rhessysSeasonFlow[,'q025'], y1= rhessysSeasonFlow[,'q975'], code=3, angle=90,length=0.02,col='red')
	dev.off()

}#



modelPlot_Bolstad_NLCD = function( calobs_, rhessys_, rhessys2_, dailytimeSeries_, output){
	
	calobsDayFlow = calobs_
	calobsWeekFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_week )
	calobsMonthFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_month )
	calobsSeasonFlow=seasonalPatterns(calobsMonthFlow, dailytimeSeries_$grp_monthMM, c(10:12,1:9))
	calobsYearFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_wateryear )
	weeklyCDF=ecdf(calobsWeekFlow)

	## NLCD
	rhessysDayFlow = rhessys_[,19]#flow
	rhessysWeekFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_week )
	rhessysMonthFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_month )
	rhessysSeasonFlow =seasonalPatterns(rhessysMonthFlow, dailytimeSeries_$grp_monthMM, c(10:12,1:9))
	rhessysYearFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_wateryear )
	
	#Bolstad
	rhessys2DayFlow = rhessys2_[,19]#flow
	rhessys2WeekFlow =grpSums(rhessys2DayFlow, dailytimeSeries_ $grp_week )
	rhessys2MonthFlow =grpSums(rhessys2DayFlow, dailytimeSeries_ $grp_month )
	rhessys2SeasonFlow =seasonalPatterns(rhessys2MonthFlow, dailytimeSeries_$grp_monthMM, c(10:12,1:9))
	rhessys2YearFlow =grpSums(rhessys2DayFlow, dailytimeSeries_ $grp_wateryear )
	
	
	rhessysDayPrecip = rhessys_[,35]#precip
	rhessysWeekPrecip =grpSums(rhessysDayPrecip, dailytimeSeries_ $grp_week )
	rhessysMonthPrecip =grpSums(rhessysDayPrecip, dailytimeSeries_ $grp_month )
	rhessysSeasonPrecip =seasonalPatterns(rhessysMonthPrecip, dailytimeSeries_$grp_monthMM, c(10:12,1:9))
	rhessysYearPrecip =grpSums(rhessysDayPrecip, dailytimeSeries_ $grp_wateryear )
	
	
	
	# 1997 - 2009 [13 years]
	# 1990 - 2010 [21 years]
	pdf(output,height=8,width=8.5)
	layout(matrix(1:4,nrow=2,ncol=2,byrow=T))
	par(mar=c(4,4,1,1) )	
	
	#..... (A) monthly flow plot
		ymin = min(rhessysMonthFlow, calobsMonthFlow, rhessys2MonthFlow)
		ymax = max(rhessysMonthFlow, calobsMonthFlow, rhessys2MonthFlow)
		tmaxRain = max(rhessysMonthPrecip)
		plot(calobsMonthFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");
		axis(1, at= dailytimeSeries_ $ithmonthisbeginWY, labels= dailytimeSeries_ $ithmonthisbeginWYlbl )
		lines(rhessysMonthFlow,col='red'); 
		lines(rhessys2MonthFlow,col='green'); 
		#lines(rhessysMonthPrecip, col='blue')
		#par(new=T)
		#plot(rhessysMonthPrecip,col='blue',type='l',ylim=c(ymax+tmaxRain, ymin), yaxt='n',xaxt='n',ylab='');
		
	
	#..... (B) monthly flow plot (log)
		ymin = min(rhessysMonthFlow, calobsMonthFlow, rhessys2MonthFlow)
		ymax = max(rhessysMonthFlow, calobsMonthFlow, rhessys2MonthFlow)
		plot(calobsMonthFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="",log='y');
		lines(rhessysMonthFlow,col='red'); 
		lines(rhessys2MonthFlow,col='green'); 
		axis(1, at= dailytimeSeries_ $ithmonthisbeginWY, labels= dailytimeSeries_ $ithmonthisbeginWYlbl )
	
	#..... (C) seasonal without error
		ymin = min(rhessysSeasonFlow[,'mean'], calobsSeasonFlow[,'mean'], rhessys2SeasonFlow[,'mean'])
		ymax = max(rhessysSeasonFlow[,'mean'], calobsSeasonFlow[,'mean'], rhessys2SeasonFlow[,'mean'])
		plot(calobsSeasonFlow[,'mean'],type='l',xaxt='n',ylim=c(ymin,ymax), xlab="", ylab='monthly streamflow');
		lines(rhessysSeasonFlow[,'mean'],col='red'); 
		lines(rhessys2SeasonFlow[,'mean'],col='green'); 
		#lines(rhessysSeasonPrecip[,'mean'],col='blue')
		axis(1, at= 1:12, labels=c(10:12,1:9) )
		#arrows(x0=1:12, y0=calobsSeasonFlow[,'q025'], y1=calobsSeasonFlow[,'q975'], code=3, angle=90,length=0.03, lwd=2)
		#arrows(x0=1:12, y0= rhessysSeasonFlow[,'q025'], y1= rhessysSeasonFlow[,'q975'], code=3, angle=90,length=0.02,col='red')
	
	#..... (D) weekly Q freq 
		obsCDF = ecdf(calobsWeekFlow)
		rhessysCDF = ecdf(rhessysWeekFlow)
		rhessys2CDF = ecdf(rhessys2WeekFlow)
		flowPoints_ = c(
			exp(quantile( log(calobsWeekFlow), seq(0.1,1,0.1)) ),
			quantile( calobsWeekFlow, seq(0.1,1,0.1))
			)
		flowPoints = flowPoints_[order(flowPoints_)]
		
		plot( 1-obsCDF(flowPoints), flowPoints, type='l', log='y', ylab='Q', xlab='freq(>Q)', xlim=c(0,1))
		lines( 1-rhessysCDF(flowPoints), flowPoints, col='red')
		lines( 1-rhessys2CDF(flowPoints), flowPoints, col='green')
	dev.off()

}#

modelPlot_Bolstad_NLCDII = function( date_, calobs_, rhessys_, rhessys2_, rhessys3_, dailytimeSeries_, output){
	
	calobsDayFlow = calobs_
	calobsWeekFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_week )
	calobsMonthFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_month )
	calobsSeasonFlow=seasonalPatterns(calobsMonthFlow, dailytimeSeries_$grp_monthMM, 1:12)
	calobsYearFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_wateryear )
	weeklyCDF=ecdf(calobsWeekFlow)

	## NLCDII
	rhessysDayFlow = rhessys_[,19]#flow
	rhessysWeekFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_week )
	rhessysMonthFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_month )
	rhessysSeasonFlow =seasonalPatterns(rhessysMonthFlow, dailytimeSeries_$grp_monthMM, 1:12)
	rhessysYearFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_wateryear )
	
	#Bolstad
	rhessys2DayFlow = rhessys2_[,19]#flow
	rhessys2WeekFlow =grpSums(rhessys2DayFlow, dailytimeSeries_ $grp_week )
	rhessys2MonthFlow =grpSums(rhessys2DayFlow, dailytimeSeries_ $grp_month )
	rhessys2SeasonFlow =seasonalPatterns(rhessys2MonthFlow, dailytimeSeries_$grp_monthMM, 1:12)
	rhessys2YearFlow =grpSums(rhessys2DayFlow, dailytimeSeries_ $grp_wateryear )
	
	#NLCD
	rhessys3DayFlow = rhessys3_[,19]#flow
	rhessys3WeekFlow =grpSums(rhessys3DayFlow, dailytimeSeries_ $grp_week )
	rhessys3MonthFlow =grpSums(rhessys3DayFlow, dailytimeSeries_ $grp_month )
	rhessys3SeasonFlow =seasonalPatterns(rhessys3MonthFlow, dailytimeSeries_$grp_monthMM, 1:12)
	rhessys3YearFlow =grpSums(rhessys3DayFlow, dailytimeSeries_ $grp_wateryear )
	
	
	rhessysDayPrecip = rhessys_[,35]#precip
	rhessysWeekPrecip =grpSums(rhessysDayPrecip, dailytimeSeries_ $grp_week )
	rhessysMonthPrecip =grpSums(rhessysDayPrecip, dailytimeSeries_ $grp_month )
	rhessysSeasonPrecip =seasonalPatterns(rhessysMonthPrecip, dailytimeSeries_$grp_monthMM, 1:12)
	rhessysYearPrecip =grpSums(rhessysDayPrecip, dailytimeSeries_ $grp_wateryear )
	
	
	
	# 1997 - 2009 [13 years]
	# 1990 - 2010 [21 years]
	pdf(output,height=8,width=8)
	layout(matrix(1:6,nrow=3,ncol=2,byrow=T))
	par(mar=c(4,4,1,1) )	
	
	#..... (A) monthly flow plot
		ymin = min(rhessysMonthFlow, calobsMonthFlow, rhessys2MonthFlow)
		ymax = max(rhessysMonthFlow, calobsMonthFlow, rhessys2MonthFlow)
		tmaxRain = max(rhessysMonthPrecip)
		plot(calobsMonthFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");
		axis(1, at= dailytimeSeries_ $ithmonthisbeginWY, labels= dailytimeSeries_ $ithmonthisbeginWYlbl )
		lines(rhessysMonthFlow,col='red'); 
		lines(rhessys2MonthFlow,col='green'); 
		lines(rhessys3MonthFlow,col='gray'); 
		#lines(rhessysMonthPrecip, col='blue')
		#par(new=T)
		#plot(rhessysMonthPrecip,col='blue',type='l',ylim=c(ymax+tmaxRain, ymin), yaxt='n',xaxt='n',ylab='');
		
	
	#..... (B) monthly flow plot (log)
		ymin = min(rhessysMonthFlow, calobsMonthFlow, rhessys2MonthFlow)
		ymax = max(rhessysMonthFlow, calobsMonthFlow, rhessys2MonthFlow)
		plot(calobsMonthFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="",log='y');
		lines(rhessysMonthFlow,col='red'); 
		lines(rhessys2MonthFlow,col='green'); 
		lines(rhessys3MonthFlow,col='gray'); 
		axis(1, at= dailytimeSeries_ $ithmonthisbeginWY, labels= dailytimeSeries_ $ithmonthisbeginWYlbl )
	
	#..... (C) seasonal without error
		ymin = min(rhessysSeasonFlow[,'mean'], calobsSeasonFlow[,'mean'], rhessys2SeasonFlow[,'mean'],rhessys3SeasonFlow[,'mean'])
		ymax = max(rhessysSeasonFlow[,'mean'], calobsSeasonFlow[,'mean'], rhessys2SeasonFlow[,'mean'],rhessys3SeasonFlow[,'mean'])
		plot(calobsSeasonFlow[,'mean'],type='l',xaxt='n',ylim=c(ymin,ymax), xlab="", ylab='monthly streamflow');
		lines(rhessysSeasonFlow[,'mean'],col='red'); 
		lines(rhessys2SeasonFlow[,'mean'],col='green'); 
		lines(rhessys3SeasonFlow[,'mean'],col='gray'); 
		#lines(rhessysSeasonPrecip[,'mean'],col='blue')
		axis(1, at= 1:12, labels=1:12 )
		#arrows(x0=1:12, y0=calobsSeasonFlow[,'q025'], y1=calobsSeasonFlow[,'q975'], code=3, angle=90,length=0.03, lwd=2)
		#arrows(x0=1:12, y0= rhessysSeasonFlow[,'q025'], y1= rhessysSeasonFlow[,'q975'], code=3, angle=90,length=0.02,col='red')
	
	#..... (D) weekly Q freq 
		obsCDF = ecdf(calobsWeekFlow)
		rhessysCDF = ecdf(rhessysWeekFlow)
		rhessys2CDF = ecdf(rhessys2WeekFlow)
		rhessys3CDF = ecdf(rhessys3WeekFlow)
		flowPoints_ = c(
			exp(quantile( log(calobsWeekFlow), seq(0.1,1,0.1)) ),
			quantile( calobsWeekFlow, seq(0.1,1,0.1))
			)
		flowPoints = flowPoints_[order(flowPoints_)]
		
		plot( 1-obsCDF(flowPoints), flowPoints, type='l', log='y', ylab='Q', xlab='freq(>Q)', xlim=c(0,1))
		lines( 1-rhessysCDF(flowPoints), flowPoints, col='red')
		lines( 1-rhessys2CDF(flowPoints), flowPoints, col='green')
		lines( 1-rhessys3CDF(flowPoints), flowPoints, col='gray')
		
	#..... (E)	
		period=seq.Date(from=as.Date('2000-10-1'), to=as.Date('2003-9-30') ,by="day") 
		tmp = match2DailyTimeSeries(date_, period)
		target.dailytimeSeriesMatch = tmp$xSelect
		plot(period,calobsDayFlow[target.dailytimeSeriesMatch],lwd=2,xlab='',ylab='dailyflow')
		lines(rhessysDayFlow[target.dailytimeSeriesMatch],col='red'); 
		lines(rhessys2DayFlow[target.dailytimeSeriesMatch],col='green'); 
		lines(rhessys3DayFlow[target.dailytimeSeriesMatch],col='gray'); 
		
	#..... (F)	
		plot(period,calobsDayFlow[target.dailytimeSeriesMatch],lwd=2,xlab='',ylab='dailyflow', log='y')
		lines(rhessysDayFlow[target.dailytimeSeriesMatch],col='red'); 
		lines(rhessys2DayFlow[target.dailytimeSeriesMatch],col='green'); 
		lines(rhessys3DayFlow[target.dailytimeSeriesMatch],col='gray'); 
		
	dev.off()

}#

modelPlot_Bolstad_NLCDIII = function( date_, calobs_, rhessys_, rhessys2_, rhessys3_, dailytimeSeries_, output){
	
	calobsDayFlow = calobs_
	calobsWeekFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_week )
	calobsMonthFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_month )
	calobsSeasonFlow=seasonalPatterns(calobsMonthFlow, dailytimeSeries_$grp_monthMM, 1:12)
	calobsYearFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_wateryear )
	weeklyCDF=ecdf(calobsWeekFlow)
	## NLCDII
	rhessysDayFlow = rhessys_[,19]#flow
	rhessysWeekFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_week )
	rhessysMonthFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_month )
	rhessysSeasonFlow =seasonalPatterns(rhessysMonthFlow, dailytimeSeries_$grp_monthMM, 1:12)
	rhessysYearFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_wateryear )
	
	#Bolstad
	rhessys2DayFlow = rhessys2_[,19]#flow
	rhessys2WeekFlow =grpSums(rhessys2DayFlow, dailytimeSeries_ $grp_week )
	rhessys2MonthFlow =grpSums(rhessys2DayFlow, dailytimeSeries_ $grp_month )
	rhessys2SeasonFlow =seasonalPatterns(rhessys2MonthFlow, dailytimeSeries_$grp_monthMM, 1:12)
	rhessys2YearFlow =grpSums(rhessys2DayFlow, dailytimeSeries_ $grp_wateryear )
	
	#NLCD
	rhessys3DayFlow = rhessys3_[,19]#flow
	rhessys3WeekFlow =grpSums(rhessys3DayFlow, dailytimeSeries_ $grp_week )
	rhessys3MonthFlow =grpSums(rhessys3DayFlow, dailytimeSeries_ $grp_month )
	rhessys3SeasonFlow =seasonalPatterns(rhessys3MonthFlow, dailytimeSeries_$grp_monthMM, 1:12)
	rhessys3YearFlow =grpSums(rhessys3DayFlow, dailytimeSeries_ $grp_wateryear )
	
	
	rhessysDayPrecip = rhessys_[,35]#precip
	rhessysWeekPrecip =grpSums(rhessysDayPrecip, dailytimeSeries_ $grp_week )
	rhessysMonthPrecip =grpSums(rhessysDayPrecip, dailytimeSeries_ $grp_month )
	rhessysSeasonPrecip =seasonalPatterns(rhessysMonthPrecip, dailytimeSeries_$grp_monthMM, 1:12)
	rhessysYearPrecip =grpSums(rhessysDayPrecip, dailytimeSeries_ $grp_wateryear )
	
	
	
	# 1997 - 2009 [13 years]
	# 1990 - 2010 [21 years]
	pdf(output,height=8,width=8)
	layout(matrix(1:6,nrow=3,ncol=2,byrow=T))
	par(mar=c(4,4,1,1) )	
	
	#..... (A) monthly flow plot
		ymin = min(rhessysMonthFlow, calobsMonthFlow)
		ymax = max(rhessysMonthFlow, calobsMonthFlow)
		
		plot(calobsMonthFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");
		axis(1, at= dailytimeSeries_ $ithmonthisbeginWY, labels= dailytimeSeries_ $ithmonthisbeginWYlbl )
		lines(rhessysMonthFlow,col='red'); 
		lines(rhessys2MonthFlow,col='green'); 
		lines(rhessys3MonthFlow,col='gray'); 
		#lines(rhessysMonthPrecip, col='blue')
		#par(new=T)
		#plot(rhessysMonthPrecip,col='blue',type='l',ylim=c(ymax+tmaxRain, ymin), yaxt='n',xaxt='n',ylab='');
		
	
	#..... (B) monthly flow plot (log)
		ymin = min(rhessysMonthFlow, calobsMonthFlow, rhessys2MonthFlow)
		ymax = max(rhessysMonthFlow, calobsMonthFlow, rhessys2MonthFlow)
		plot(calobsMonthFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="",log='y');
		lines(rhessysMonthFlow,col='red'); 
		lines(rhessys2MonthFlow,col='green'); 
		lines(rhessys3MonthFlow,col='gray'); 
		axis(1, at= dailytimeSeries_ $ithmonthisbeginWY, labels= dailytimeSeries_ $ithmonthisbeginWYlbl )
	
	#..... (C) seasonal without error
		ymin = min(rhessysSeasonFlow[,'mean'], calobsSeasonFlow[,'mean'], rhessys2SeasonFlow[,'mean'],rhessys3SeasonFlow[,'mean'])
		ymax = max(rhessysSeasonFlow[,'mean'], calobsSeasonFlow[,'mean'], rhessys2SeasonFlow[,'mean'],rhessys3SeasonFlow[,'mean'])
		plot(calobsSeasonFlow[,'mean'],type='l',xaxt='n',ylim=c(ymin,ymax), xlab="", ylab='monthly streamflow');
		lines(rhessysSeasonFlow[,'mean'],col='red'); 
		lines(rhessys2SeasonFlow[,'mean'],col='green'); 
		lines(rhessys3SeasonFlow[,'mean'],col='gray'); 
		#lines(rhessysSeasonPrecip[,'mean'],col='blue')
		axis(1, at= 1:12, labels=1:12 )
		#arrows(x0=1:12, y0=calobsSeasonFlow[,'q025'], y1=calobsSeasonFlow[,'q975'], code=3, angle=90,length=0.03, lwd=2)
		#arrows(x0=1:12, y0= rhessysSeasonFlow[,'q025'], y1= rhessysSeasonFlow[,'q975'], code=3, angle=90,length=0.02,col='red')
	
		seasonal_NLCD_local = NSE(calobsSeasonFlow[,'mean'],rhessysSeasonFlow[,'mean']) # 0.9646669, 0.9258779
		seasonal_species = NSE(calobsSeasonFlow[,'mean'], rhessys2SeasonFlow[,'mean']) # 0.9837246, 0.9581939
		seasonal_NLCD_std = NSE(calobsSeasonFlow[,'mean'], rhessys3SeasonFlow[,'mean']) # 0.937404, 0.8950738
	
		mean(abs(calobsSeasonFlow[,'mean']-rhessysSeasonFlow[,'mean'])) # 77.15609 -> 6.429674, 6.536236
		mean(abs(calobsSeasonFlow[,'mean']-rhessys2SeasonFlow[,'mean'])) # 55.75789 -> 4.646491, 4.412235
		mean(abs(calobsSeasonFlow[,'mean']-rhessys3SeasonFlow[,'mean'])) # 102.1928 -> 8.516064, 7.340679
		
	#..... (D) weekly Q freq 
		obsCDF = ecdf(calobsWeekFlow)
		rhessysCDF = ecdf(rhessysWeekFlow)
		rhessys2CDF = ecdf(rhessys2WeekFlow)
		rhessys3CDF = ecdf(rhessys3WeekFlow)
		flowPoints_ = c(
			exp(quantile( log(calobsWeekFlow), seq(0.1,1,0.1)) ),
			quantile( calobsWeekFlow, seq(0.1,1,0.1))
			)
		flowPoints = flowPoints_[order(flowPoints_)]
		
		plot( 1-obsCDF(flowPoints), flowPoints, type='l', log='y', ylab='Q', xlab='freq(>Q)', xlim=c(0,1))
		lines( 1-rhessysCDF(flowPoints), flowPoints, col='red')
		lines( 1-rhessys2CDF(flowPoints), flowPoints, col='green')
		lines( 1-rhessys3CDF(flowPoints), flowPoints, col='gray')
		
		seasonal_NLCD_local = NSE(1-obsCDF(flowPoints),1-rhessysCDF(flowPoints)) # 0.9956296
		seasonal_species = NSE(1-obsCDF(flowPoints), 1-rhessys2CDF(flowPoints)) # 0.9921136
		seasonal_NLCD_std = NSE(1-obsCDF(flowPoints), 1-rhessys3CDF(flowPoints)) # 0.990114
		
		sum(abs(1-obsCDF(flowPoints)-(1-rhessysCDF(flowPoints)))) # 0.2643678
		sum(abs(1-obsCDF(flowPoints)-(1-rhessys2CDF(flowPoints)))) # 0.3869732
		sum(abs(1-obsCDF(flowPoints)-(1-rhessys3CDF(flowPoints)))) # 0.4291188
		
		
	#..... (E)	
		period=seq.Date(from=as.Date('2000-10-1'), to=as.Date('2003-9-30') ,by="day") 
		tmp = match2DailyTimeSeries(date_, period)
		target.dailytimeSeriesMatch = tmp$xSelect
		plot(period,calobsDayFlow[target.dailytimeSeriesMatch],lwd=2,xlab='',ylab='dailyflow')
		lines(rhessysDayFlow[target.dailytimeSeriesMatch],col='red'); 
		lines(rhessys2DayFlow[target.dailytimeSeriesMatch],col='green'); 
		lines(rhessys3DayFlow[target.dailytimeSeriesMatch],col='gray'); 
		
	#..... (F)	
		plot(period,calobsDayFlow[target.dailytimeSeriesMatch],lwd=2,xlab='',ylab='dailyflow', log='y')
		lines(rhessysDayFlow[target.dailytimeSeriesMatch],col='red'); 
		lines(rhessys2DayFlow[target.dailytimeSeriesMatch],col='green'); 
		lines(rhessys3DayFlow[target.dailytimeSeriesMatch],col='gray'); 
		
	dev.off()

}#


modelPlot_FIA_NLCD = function( calobs_, rhessys_, rhessys1_, rhessys2_, rhessys3_, dailytimeSeries_, output){
	
	calobsDayFlow = calobs_
	calobsWeekFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_week )
	calobsMonthFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_month )
	calobsSeasonFlow=seasonalPatterns(calobsMonthFlow, dailytimeSeries_$grp_monthMM, 1:12)
	calobsYearFlow =grpSums(calobsDayFlow, dailytimeSeries_ $grp_wateryear )
	weeklyCDF=ecdf(calobsWeekFlow)

	## NLCD
	rhessysDayFlow = rhessys_[,19]#flow
	rhessysWeekFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_week )
	rhessysMonthFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_month )
	rhessysSeasonFlow =seasonalPatterns(rhessysMonthFlow, dailytimeSeries_$grp_monthMM, 1:12)
	rhessysYearFlow =grpSums(rhessysDayFlow, dailytimeSeries_ $grp_wateryear )
	
	rhessys1DayFlow = rhessys1_[,19]#flow
	rhessys1WeekFlow =grpSums(rhessys1DayFlow, dailytimeSeries_ $grp_week )
	rhessys1MonthFlow =grpSums(rhessys1DayFlow, dailytimeSeries_ $grp_month )
	rhessys1SeasonFlow =seasonalPatterns(rhessys1MonthFlow, dailytimeSeries_$grp_monthMM, 1:12)
	rhessys1YearFlow =grpSums(rhessys1DayFlow, dailytimeSeries_ $grp_wateryear )
	
	
	#FIA type
	rhessys2DayFlow = rhessys2_[,19]#flow
	rhessys2WeekFlow =grpSums(rhessys2DayFlow, dailytimeSeries_ $grp_week )
	rhessys2MonthFlow =grpSums(rhessys2DayFlow, dailytimeSeries_ $grp_month )
	rhessys2SeasonFlow =seasonalPatterns(rhessys2MonthFlow, dailytimeSeries_$grp_monthMM, 1:12)
	rhessys2YearFlow =grpSums(rhessys2DayFlow, dailytimeSeries_ $grp_wateryear )
	
	#FIA xylem
	rhessys3DayFlow = rhessys3_[,19]#flow
	rhessys3WeekFlow =grpSums(rhessys3DayFlow, dailytimeSeries_ $grp_week )
	rhessys3MonthFlow =grpSums(rhessys3DayFlow, dailytimeSeries_ $grp_month )
	rhessys3SeasonFlow =seasonalPatterns(rhessys3MonthFlow, dailytimeSeries_$grp_monthMM, 1:12)
	rhessys3YearFlow =grpSums(rhessys3DayFlow, dailytimeSeries_ $grp_wateryear )
	
	
	# rhessysDayPrecip = rhessys_[,35]#precip
	# rhessysWeekPrecip =grpSums(rhessysDayPrecip, dailytimeSeries_ $grp_week )
	# rhessysMonthPrecip =grpSums(rhessysDayPrecip, dailytimeSeries_ $grp_month )
	# rhessysSeasonPrecip =seasonalPatterns(rhessysMonthPrecip, dailytimeSeries_$grp_monthMM, 1:12)
	# rhessysYearPrecip =grpSums(rhessysDayPrecip, dailytimeSeries_ $grp_wateryear )
	
	
	
	# 1997 - 2009 [13 years]
	# 1990 - 2010 [21 years]
	pdf(output,height=6,width=8)
	layout(matrix(1:4,nrow=2,ncol=2,byrow=T))
	par(mar=c(4,4,1,1) )	
	
	#..... (A) weekly flow plot
		ymin = min(rhessysWeekFlow, calobsWeekFlow, rhessys1WeekFlow, rhessys2WeekFlow, rhessys3WeekFlow)
		ymax = max(rhessysWeekFlow, calobsWeekFlow, rhessys1WeekFlow, rhessys2WeekFlow, rhessys3WeekFlow)
		#tmaxRain = max(rhessysWeekPrecip)
		plot(calobsWeekFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");
		axis(1, at= dailytimeSeries_ $ithWeekisbeginWY, labels= dailytimeSeries_ $ithWeekisbeginWYlbl )
		lines(rhessysWeekFlow,col='red'); 
		lines(rhessys1WeekFlow,col='pink'); 
		lines(rhessys2WeekFlow,col='green'); 
		lines(rhessys3WeekFlow,col='blue'); 
		#lines(rhessysWeekPrecip, col='blue')
		#par(new=T)
		#plot(rhessysWeekPrecip,col='blue',type='l',ylim=c(ymax+tmaxRain, ymin), yaxt='n',xaxt='n',ylab='');
		
	
	#..... (B) weekly flow plot (log)
		ymin = min(rhessysWeekFlow, calobsWeekFlow, rhessys1WeekFlow, rhessys2WeekFlow, rhessys3WeekFlow)
		ymax = max(rhessysWeekFlow, calobsWeekFlow, rhessys1WeekFlow, rhessys2WeekFlow, rhessys3WeekFlow)
		plot(calobsWeekFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="",log='y');
		lines(rhessysWeekFlow,col='red'); 
		lines(rhessys1WeekFlow,col='pink'); 
		lines(rhessys2WeekFlow,col='green'); 
		lines(rhessys3WeekFlow,col='blue'); 
		axis(1, at= dailytimeSeries_ $ithWeekisbeginWY, labels= dailytimeSeries_ $ithWeekisbeginWYlbl )
	
	#..... (C) monthly without error
		ymin = min(rhessysMonthFlow, calobsMonthFlow, rhessys1MonthFlow, rhessys2MonthFlow, rhessys3MonthFlow)
		ymax = max(rhessysMonthFlow, calobsMonthFlow, rhessys1MonthFlow, rhessys2MonthFlow, rhessys3MonthFlow)
		#tmaxRain = max(rhessysMonthPrecip)
		plot(calobsMonthFlow,type='l',xaxt='n',ylim=c(ymin,ymax), xlab="");
		axis(1, at= dailytimeSeries_ $ithmonthisbeginWY, labels= dailytimeSeries_ $ithmonthisbeginWYlbl )
		lines(rhessysMonthFlow,col='red'); 
		lines(rhessys1MonthFlow,col='pink'); 
		lines(rhessys2MonthFlow,col='green'); 
		lines(rhessys3MonthFlow,col='blue'); 
		#lines(rhessysMonthPrecip, col='blue')
		#par(new=T)
		#plot(rhessysMonthPrecip,col='blue',type='l',ylim=c(ymax+tmaxRain, ymin), yaxt='n',xaxt='n',ylab='');
	
	#..... (D) weekly Q freq 
		obsCDF = ecdf(calobsWeekFlow)
		rhessysCDF = ecdf(rhessysWeekFlow)
		rhessys2CDF = ecdf(rhessys2WeekFlow)
		rhessys1CDF = ecdf(rhessys1WeekFlow)
		rhessys3CDF = ecdf(rhessys3WeekFlow)
		
		flowPoints_ = c(
			exp(quantile( log(calobsWeekFlow), seq(0.1,1,0.1)) ),
			quantile( calobsWeekFlow, seq(0.1,1,0.1))
			)
		flowPoints = flowPoints_[order(flowPoints_)]
		
		plot( 1-obsCDF(flowPoints), flowPoints, type='l', log='y', ylab='Q', xlab='freq(>Q)', xlim=c(0,1),yaxt='n')
		axis(2,at=c(0.1,1,10,100), labels=c(0.1,1,10,100))
		lines( 1-rhessysCDF(flowPoints), flowPoints, col='red')
		lines( 1-rhessys1CDF(flowPoints), flowPoints, col='pink')
		lines( 1-rhessys2CDF(flowPoints), flowPoints, col='green')
		lines( 1-rhessys3CDF(flowPoints), flowPoints, col='blue')
	dev.off()

}#


if(length(arg)>0){


##--------------------------------------------------------------------------------------------## NY shelter
# arg=c(
	# "~/Dropbox/Myself/UNC/NY_ShelterCreek/workflows10m", 
	# "USGS01434092_19921001_20070930.csv",
	# "rhessys_veg", "output/SESSION_3_world_ITR_1", "rhessys_basin.daily",
	# "1992-10-1","1995-9-30",
	# "~/Dropbox/Myself/UNC/NY_ShelterCreek/workflows10m/rhessys_veg/output/SESSION_3_world_ITR_1/rhessys_itr1_plot_1992_1994_style2.pdf"
# )

# arg=c(
	# "~/Dropbox/Myself/UNC/NY_ShelterCreek/workflows10m", 
	# "USGS01434092_19921001_20070930.csv",
	# "rhessys_veg", "output_soilz1/SESSION_8_world_ITR_137", "rhessys_basin.daily",
	# "1992-10-1","1995-9-30",
	# "~/Dropbox/Myself/UNC/NY_ShelterCreek/workflows10m/rhessys_veg/output_soilz1/SESSION_8_world_ITR_137/rhessys_itr1_plot_1992_1994_style2.pdf"
# )

# arg=c(
	# "/Volumes/storage/RHESSys_watershed/NY_ShelterCreek/workflows10m", 
	# "USGS01434092_19921001_20070930.csv",
	# "rhessys_veg_stream", "output_mcmc_corTime8/SESSION_2_world_ITR_1", "rhessys_basin.daily",
	# "1992-10-1","1995-9-30",
	# "/Volumes/storage/RHESSys_watershed/NY_ShelterCreek/workflows10m/rhessys_veg_stream/output_mcmc_corTime8/SESSION_2_world_ITR_1/rhessys_itr1_plot_1992_1994_style2.pdf"
# )

##--------------------------------------------------------------------------------------------## morgan
# arg=c(
	# "/Users/laurencelin/Desktop/master_FIA", 
	# "morgancreek_19981101_20150817.csv",
	# "rhessys", "output", "morgan_basin.daily",
	# "2000-10-1","2004-9-30",
	# "/Users/laurencelin/Desktop/master_FIA/rhessys/output/morgan_basin_plot_2000_2003_style2.pdf"
# )

##--------------------------------------------------------------------------------------------## flat
# arg=c(
	# "/Users/laurencelin/Desktop/master_FIA", 
	# "flat_19251001_20130930.csv",
	# "rhessys", "output_mcmcIIII", "SESSION_1_world_ITR_3/rhessys_basin.daily",
	# "2000-10-1","2004-9-30",
	# "/Users/laurencelin/Desktop/master_FIA/rhessys/output_mcmcIIII/SESSION_1_world_ITR_3/rhessys_basin_plot_2000_2003_style2New.pdf"
# )

	 arg=c(
		"/Volumes/storage/WSC_storage/WSC_regional_FIA2LAI", 
		"flat_19251001_20130930.csv",
		"rhessys2010_climateB", "output_michieCsiro", "regionalFlatAll",
		"2000-10-1","2004-9-30",
		"~/Downloads/dynamicPhenology_p3.pdf"
	 )

	##---------------------------------- << regular plot >>
	proj = arg[1]
	obsfile = arg[2]
	rhessys_Folder = arg[3]
	outputFolder = arg[4]
	fileName = arg[5]
	startingDate=as.Date(arg[6]) #, 
	endingDate=as.Date(arg[7]) #, 
	period=seq.Date(from=startingDate, to=endingDate ,by="day") 
	outputName = arg[8]
	
	calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
	calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
	calobs= calobs_[calobsNonZero,]
	#calobs.date0 = convertDateExcelMDY(calobs[,1],beginningYY=20) ##<<-----
	calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----
	
		## regular
		rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
		rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")
		
		## special for WSC output format [43 columns]
		rhessys_SingleFile_flowmm = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName,'_streamflowmm.csv' ,sep=''),header=F,skip=1,sep=',') # assume csv
		rhessys_SingleFile_flowet = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName,'_et.csv' ,sep=''),header=F,skip=1,sep=',') # assume csv
		rhessys_SingleFile_flowprecip = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName,'_precip.csv' ,sep=''),header=F,skip=1,sep=',') # assume csv
		rhessys_SingleFile = matrix(NA, nrow(rhessys_SingleFile_flowmm),43)
		rhessys_SingleFile[,19] = rhessys_SingleFile_flowmm[,7]
		rhessys_SingleFile[,16] = rhessys_SingleFile_flowet[,7]
		rhessys_SingleFile[,35] = rhessys_SingleFile_flowprecip[,7]
		rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile_flowmm[,3], rhessys_SingleFile_flowmm[,2], rhessys_SingleFile_flowmm[,1],sep="-"),format="%d-%m-%Y")
		outputName = paste('~/Downloads/dynamicPhenology_p',6,'.pdf')
	
	tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
	calobs.dailytimeSeriesMatch = tmp$xSelect
	rhessys.dailytimeSeriesMatch = tmp$ySelect
	calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])
	
	# plot(calobs[calobs.dailytimeSeriesMatch,2],lwd=2,type='l',xaxt='n', xlim=c(1,365))
	# lines( rhessys_SingleFile[rhessys.dailytimeSeriesMatch,19], col='red')
	# axis(1, at= calobs.dailytimeSeries$ithdayisbeginWY, labels= calobs.dailytimeSeries$ithdayisbeginWYlbl)
	
	modelPlotStyle2( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries, outputName)
	modelPlotMonthlySeason( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries, outputName)
	modelPlot_Bolstad_NLCD( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries, outputName)


##--------------------------------------------------------------------------------------------## cane
# arg=c(
	# "/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012", 
	# "canecreek_19881101_20150826.csv",
	# "rhessys", "output/SESSION_2_world_ITR_1241_patch", "rhessys_basin.daily",
	# "1998-10-1","2012-9-30",
	# "/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output/SESSION_2_world_ITR_1241_patch/rhessys_itr1241_plot_1998_2011_style2New.pdf"
# )

# arg=c(
	# "~/Dropbox/Myself/UNC/WSC/cane_cmip5_test", 
	# "canecreek_19881101_20150826.csv",
	# "rhessys", "output/SESSION_2_world_ITR_1241", "rhessys_basin.daily",
	# "1998-10-1","2012-9-30",
	# "~/Dropbox/Myself/UNC/WSC/cane_cmip5_test/rhessys/output/SESSION_2_world_ITR_1241/rhessys_itr1241_plot_1998_2011_style2.pdf"
# )

# arg=c(
	# "~/Dropbox/Myself/UNC/WSC/cane_FIA_version2_agu", 
	# "canecreek_19881101_20150826.csv",
	# "rhessys", "output", "e_cross_4_h3_basin.daily",
	# "2000-10-1","2004-9-30",
	# "~/Dropbox/Myself/UNC/WSC/cane_FIA_version2_agu/rhessys/output/e_cross_4_h3_basin_plot_1998_2011_style2.pdf"
# )

# arg=c(
	# "/Users/laurence9010/Dropbox/Myself/UNC/WSC/cane_cmip5_test", 
	# "canecreek_19881101_20150826.csv",
	# "rhessys", "needed", "rhessys_basin.daily",
	# "2000-10-1","2004-9-30",
	# "/Users/laurence9010/Dropbox/Myself/UNC/WSC/cane_cmip5_test/rhessys/needed/needed_plot_1998_2011_style2.pdf"
# )

# arg=c(
	# "/Volumes/storage/WSC_storage/cane_link_nov8_2016/calibration_ch2w_ruleMod_1992-2012", 
	# "canecreek_19881101_20150826.csv",
	# "rhessys", "outputII/SESSION_2_world_ITR_1", "rhessys_basin.daily",
	# "2000-10-1","2004-9-30",
	# "/Volumes/storage/WSC_storage/cane_link_nov8_2016/calibration_ch2w_ruleMod_1992-2012/rhessys/outputII/SESSION_2_world_ITR_1/rhessys_plot_1998_2011_style2.pdf"
# )

# arg=c(
	# "/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local", 
	# "canecreek_19881101_20150826.csv",
	# "rhessys", "output", "base_statPhen_basin.daily",
	# "2000-10-1","2004-9-30",
	# "/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/output/base_statPhen_2000_2004_style2.pdf"
# )

# arg=c(
	# "/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local", 
	# "canecreek_19881101_20150826.csv",
	# "rhessys", "output", "base_dynPhen_basin.daily",
	# "2000-10-1","2004-9-30",
	# "/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/output/base_dynPhen_2000_2004_style2.pdf"
# )

# arg=c(
	# "/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local", 
	# "canecreek_19881101_20150826.csv",
	# "rhessys", "output", "hightemp_dynPhen_basin.daily",
	# "2000-10-1","2004-9-30",
	# "/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/output/hightemp_dynPhen_2000_2004_style2.pdf"
# )

# arg=c(
	# "/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local", 
	# "canecreek_19881101_20150826.csv",
	# "rhessys", "output", "hightemp_statPhen_basin.daily",
	# "2000-10-1","2004-9-30",
	# "/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/output/hightemp_statPhen_2000_2004_style2.pdf"
# )


##--------------------------------------------------------------------------------------------## WS14
# arg=c(
	# "/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc", 
	# "ws14dailyflow_19710101_20120703mm.csv",
	# "rhessys", "output", "vegbolstad_bgcNew_basin.daily",
	# "2005-10-1","2011-9-30",
	# "/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc/rhessys/output/vegbolstad_bgcNew_basin_plot_1998_2011_style2.pdf"
# )

# arg=c(
	# "/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc", 
	# "ws14dailyflow_19710101_20120703mm.csv",
	# "rhessys", "output", "vegbolstad_rhd5_bgcNew2_basin.daily",
	# "2005-10-1","2011-9-30",
	# "/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc/rhessys/output/vegbolstad_rhd5_bgcNew2_basin_plot_1998_2011_style2.pdf"
# )

# arg=c(
	# "/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_workflows", 
	# "ws14dailyflow_19710101_20120703mm.csv",
	# "rhessys", "outputKilldevil/SESSION_2_world_ITR_21", "defaultAUG2016_basin.daily",
	# "2005-10-1","2011-9-30",
	# "/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_workflows/rhessys/outputKilldevil/SESSION_2_world_ITR_21/defaultAUG2016_basin_plot_1998_2011_style2.pdf"
# )

# arg=c(
	# "/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_workflows", 
	# "ws14dailyflow_19710101_20120703mm.csv",
	# "rhessys", "outputKilldevil/SESSION_2_world_ITR_21", "defaultAUG2016_basin.daily",
	# "1997-10-1","2011-9-30",
	# "/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_workflows/rhessys/outputKilldevil/SESSION_2_world_ITR_21/defaultAUG2016_basin_plot_1997_2011_style2.pdf"
# )


# arg=c(
	# "/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc", 
	# "ws14dailyflow_19710101_20120703mm.csv",
	# "rhessys", "output/SESSION_2_world_ITR_127", "defaultAUG2016_basin.daily",
	# "1997-10-1","2011-9-30",
	# "/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc/rhessys/output/SESSION_2_world_ITR_127/defaultAUG2016_basin_plot_1997_2011_style2.pdf"
# )

arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt", 
	"ws14dailyflow_19710101_20120703mm.csv",
	"rhessys", "output", "test_it28_basin.daily",
	"1980-10-1","2014-9-30",
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt/rhessys/output/test_it28_basin_plot_1980_2013_style2.pdf"
)

arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_workflows", 
	"ws14dailyflow_19710101_20120703mm.csv",
	"rhessys", "outputKilldevil/SESSION_2_world_ITR_21", "rhessysLONG_basin.daily",
	"1980-10-1","2014-9-30",
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_workflows/rhessys/outputKilldevil/SESSION_2_world_ITR_21/rhessysLONG_it21_basin_plot_1980_2013_style2.pdf"
)

##--------------------------------------------------------------------------------------------## WS18
arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt", 
	"Qobs_18_r.csv",
	"rhessys", "output", "test_it28_ws18_basin.daily",
	"1980-10-1","2014-9-30",
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt/rhessys/output/test_it28_ws18_basin_plot_1980_2013_style2.pdf"
)

arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt", 
	"Qobs_18_r.csv",
	"rhessys", "output", "test_it28_ws18_basin.daily",
	"1991-10-1","2009-9-30",
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt/rhessys/output/test_it28_ws18_basin_plot_1991_2008_style2.pdf"
)

arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS18/cwt18_mcmc", 
	"Qobs_18_r.csv",
	"rhessys_tree", "output/SESSION_3_world_ITR_1", "rhessys_basin.daily",
	"1991-10-1","2009-9-30",
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output/SESSION_3_world_ITR_1/rhessys_itr1_plot_1991_2008_style2.pdf"
)

arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS18/cwt18_mcmc", 
	"Qobs_18_r.csv",
	"rhessys_tree", "output/SESSION_3_world_ITR_100", "rhessysII_100_8_2_basin.daily",
	"1991-10-1","2009-9-30",
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output/SESSION_3_world_ITR_100/rhessysII_itr1_plot_1991_2008_style2.pdf"
)
### ... 18 NLCD
arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS18/cwt18_mcmc", 
	"Qobs_18_r.csv",
	"rhessys_tree", "output_ws18_manuscript_calibration", "manu4sw_basin.daily",
	"1980-10-1","1991-9-30",
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output_ws18_manuscript_calibration/Testws4_ws18_basin_plot_1980_1990_style2.pdf"
)
arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS18/cwt18_mcmc", 
	"Qobs_18_r.csv",
	"rhessys_tree", "output_mcmc/SESSION_3_world_ITR_29", "rhessys_basin.daily",
	"1990-10-1","2011-9-30",
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output_ws18_manuscript_calibration/Test29season_ws18_basin_plot_1980_1990_MonthlySeason.pdf" ###<<<--------- used in manuscript
)

### ... 18 Bolstad v2
arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt", 
	"Qobs_18_r.csv",
	"rhessys", "output_ws18_manuscript_calibration", "manu8sw_basin.daily",
	"1980-10-1","1991-9-30",
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt/rhessys/output_ws18_manuscript_calibration/Testws8_ws18_basin_plot_1980_2013_style2.pdf"
)
arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt", 
	"Qobs_18_r.csv",
	"rhessys", "output_ws18mcmcII/SESSION_1_world_ITR_11", "rhessys_basin.daily",
	"1980-10-1","1991-9-30",
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt/rhessys/output_ws18_manuscript_calibration/Test11season_ws18_basin_plot_1980_2013_style2.pdf" ###<<<--------- used in manuscript
)

arg=c(
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt", 
	"Qobs_18_r.csv",
	"rhessys", "output_ws18_manuscript_oldway_TDR_mcmc", "SESSION_1_world_ITR_2/NCAPc1c1s911m001_basin.daily",
	"1991-10-1","2010-9-30",
	"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt/rhessys/output_ws18_manuscript_oldway_TDR_mcmc/SESSION_1_world_ITR_2/NCAPc1c1s911m001_basin_plot_1991_2010_style2.pdf"
)

##--------------------------------------------------------------------------------------------## swift
# arg=c(
	# "/Volumes/storage/WSC_storage/swift", 
	# "swift2002_2016.csv",
	# "rhessys_climateO_FIA2local", "output/SESSION_7_world_ITR_7", "rhessys_basin.daily",
	# "2002-10-1","2006-9-30",
	# "/Volumes/storage/WSC_storage/swift/rhessys_climateO_FIA2local/output/SESSION_7_world_ITR_7/rhessys_itr7_plot_2002_2005_style2B.pdf"
# )

arg=c(
	"/Volumes/storage/WSC_storage/swift", 
	"swift2002_2016.csv",
	"rhessys_climateO_FIA2local", "output_s10_test/SESSION_10_world_ITR_1", "rhessys_basin.daily",
	"2002-10-1","2006-9-30",
	"/Volumes/storage/WSC_storage/swift/rhessys_climateO_FIA2local/output_s10_test/SESSION_10_world_ITR_1/rhessys_itr1_plot_2002_2005_style2.pdf"
)

arg=c(
	"/Volumes/storage/WSC_storage/swift", 
	"swift2002_2016.csv",
	"rhessys_climateO_FIA2local", "outputLong_upstreamRouting/SESSION_13_world_ITR_1", "rhessys_basin.daily",
	"2002-10-1","2016-9-30",
	"/Volumes/storage/WSC_storage/swift/rhessys_climateO_FIA2local/outputLong_upstreamRouting/SESSION_13_world_ITR_1/rhessys_itr1_plot_2002_2016_style2.pdf"
)

	##---------------------------------- << regular plot >>
	proj = arg[1]
	obsfile = arg[2]
	rhessys_Folder = arg[3]
	outputFolder = arg[4]
	fileName = arg[5]
	startingDate=as.Date(arg[6]) #, 
	endingDate=as.Date(arg[7]) #, 
	period=seq.Date(from=startingDate, to=endingDate ,by="day") 
	outputName = arg[8]
	
	calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
	calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
	calobs= calobs_[calobsNonZero,]
	#calobs.date0 = convertDateExcelMDY(calobs[,1],beginningYY=20) ##<<-----
	calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----
	
	rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", fileName ,sep=''),header=F,skip=1,sep=' ') # assume csv
	rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")
	
	tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
	calobs.dailytimeSeriesMatch = tmp$xSelect
	rhessys.dailytimeSeriesMatch = tmp$ySelect
	calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])
	
	# plot(calobs[calobs.dailytimeSeriesMatch,2],lwd=2,type='l',xaxt='n', xlim=c(1,365))
	# lines( rhessys_SingleFile[rhessys.dailytimeSeriesMatch,19], col='red')
	# axis(1, at= calobs.dailytimeSeries$ithdayisbeginWY, labels= calobs.dailytimeSeries$ithdayisbeginWYlbl)
	
	modelPlotStyle2( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries, outputName)
	modelPlotMonthlySeason( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries, outputName)
	modelPlot_Bolstad_NLCD( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries, outputName)



##--------------------------------------------------------------------------------------------## WS18 + ws14
	###---------- ws18
	arg=c(
		"/Volumes/LaCie/workArch/University_of_North_Carolina/WS18/cwt18_mcmc", 
		"Qobs_18_r.csv",
		"rhessys_tree", "output_ws18_manuscript_oldway_TDR_mcmc10/SESSION_1_world_ITR_3", "rhessys_basin.daily",
		#"2000-10-1","2011-9-30",
		"1990-10-1","2001-9-30",
		"/Volumes/LaCie/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output_ws18_manuscript_oldway_TDR_mcmc10/Test29season_ws18_basin_plot_1980_1990_MonthlySeason.pdf" ###<<<--------- used in manuscript
	)
	
	###---------- ws14
	arg=c(
		"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc", 
		"ws14dailyflow_19710101_20120703mm.csv",
		"rhessys", "output", "test_it28_basin.daily",
		"2000-10-1","2011-9-30",
		"/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc/rhessys/manuscript_fittness_plot.pdf"
	)
	
	proj = arg[1]
	obsfile = arg[2]
	startingDate=as.Date(arg[6]) #, 
	endingDate=as.Date(arg[7]) #, 
	period=seq.Date(from=startingDate, to=endingDate ,by="day") 
	outputName = arg[8]
	
	calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
	calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
	calobs= calobs_[calobsNonZero,]
	#calobs.date0 = convertDateExcelMDY(calobs[,1],beginningYY=20) ##<<-----
	calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----
	
	
	# # nlcd ws14
	# rhessys_SingleFile_1 = read.table(paste('/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_workflows/rhessys/outputKilldevil/SESSION_2_world_ITR_21/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	# rhessys_SingleFile_2 = read.table(paste('/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_workflows/rhessys/outputKilldevil/SESSION_2_world_ITR_445/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	# rhessys_SingleFile_3 = read.table(paste('/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_workflows/rhessys/outputKilldevil/SESSION_2_world_ITR_920/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	# rhessys_SingleFile_4 = read.table(paste('/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_workflows/rhessys/outputKilldevil/SESSION_2_world_ITR_627/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	# rhessys_SingleFile = rhessys_SingleFile_1
	# rhessys_SingleFile[,19] =  (rhessys_SingleFile_1[,19]+rhessys_SingleFile_2[,19]+rhessys_SingleFile_3[,19]+rhessys_SingleFile_4[,19])*0.25
	# rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile_1[,1], rhessys_SingleFile_1[,2], rhessys_SingleFile_1[,3],sep="-"),format="%d-%m-%Y")
	
	# # bolstad ws14
	# rhessys_SingleFile2_1 = read.table(paste('/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc/rhessys/output/SESSION_2_world_ITR_447/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	# rhessys_SingleFile2_2 = read.table(paste('/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc/rhessys/output/SESSION_2_world_ITR_28/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	# rhessys_SingleFile2_3 = read.table(paste('/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc/rhessys/output/SESSION_2_world_ITR_127/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	# rhessys_SingleFile2_4 = read.table(paste('/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc/rhessys/output/SESSION_2_world_ITR_987/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	# rhessys_SingleFile2 = rhessys_SingleFile2_1
	# rhessys_SingleFile2[,19] =  (rhessys_SingleFile2_1[,19]+ rhessys_SingleFile2_2[,19]+ rhessys_SingleFile2_3[,19]+ rhessys_SingleFile2_4[,19])*0.25
	# rhessys_SingleFile2.date=as.Date(paste(rhessys_SingleFile2_1[,1], rhessys_SingleFile2_1[,2], rhessys_SingleFile2_1[,3],sep="-"),format="%d-%m-%Y")
	
	
	
	
	# nlcdII ws18 (3,5,7,12)
	rhessys_SingleFile_1 = read.table(paste('/Volumes/LaCie/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output_mcmc7/SESSION_1_world_ITR_3/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	rhessys_SingleFile_2 = read.table(paste('/Volumes/LaCie/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output_mcmc7/SESSION_1_world_ITR_4/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	rhessys_SingleFile_3 = read.table(paste('/Volumes/LaCie/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output_mcmc7/SESSION_1_world_ITR_5/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume cs
	rhessys_SingleFile_4 = read.table(paste('/Volumes/LaCie/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output_mcmc7/SESSION_1_world_ITR_6/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	rhessys_SingleFile = rhessys_SingleFile_1
	rhessys_SingleFile[,19] =  (rhessys_SingleFile_1[,19]+rhessys_SingleFile_2[,19]+rhessys_SingleFile_3[,19]+rhessys_SingleFile_4[,19])*0.25
	rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile_1[,1], rhessys_SingleFile_1[,2], rhessys_SingleFile_1[,3],sep="-"),format="%d-%m-%Y")

	# bolstad ws18	
	rhessys_SingleFile2_1 = read.table(paste('/Volumes/LaCie/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt/rhessys/output_ws18_manuscript_oldway_TDR_mcmc11/SESSION_1_world_ITR_10/test_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	rhessys_SingleFile2_2 = read.table(paste('/Volumes/LaCie/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt/rhessys/output_ws18_manuscript_oldway_TDR_mcmc11/SESSION_1_world_ITR_12/test_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	rhessys_SingleFile2_3 = read.table(paste('/Volumes/LaCie/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt/rhessys/output_ws18_manuscript_oldway_TDR_mcmc11/SESSION_1_world_ITR_14/test_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	rhessys_SingleFile2_4 = read.table(paste('/Volumes/LaCie/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt/rhessys/output_ws18_manuscript_oldway_TDR_mcmc11/SESSION_1_world_ITR_16/test_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
		#rhessys_SingleFile2_2 = rhessys_SingleFile2_1
		#rhessys_SingleFile2_3 = rhessys_SingleFile2_1
		#rhessys_SingleFile2_4 = rhessys_SingleFile2_1
	rhessys_SingleFile2 = rhessys_SingleFile2_1
	rhessys_SingleFile2[,19] =  (rhessys_SingleFile2_1[,19]+ rhessys_SingleFile2_2[,19]+ rhessys_SingleFile2_3[,19]+ rhessys_SingleFile2_4[,19])*0.25
	rhessys_SingleFile2.date=as.Date(paste(rhessys_SingleFile2_1[,1], rhessys_SingleFile2_1[,2], rhessys_SingleFile2_1[,3],sep="-"),format="%d-%m-%Y")

	# nlcd ws18
	rhessys_SingleFile3_1 = read.table(paste('/Volumes/LaCie/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output_mcmc8/SESSION_1_world_ITR_6/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	rhessys_SingleFile3_2 = read.table(paste('/Volumes/LaCie/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output_mcmc8/SESSION_1_world_ITR_8/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	rhessys_SingleFile3_3 = read.table(paste('/Volumes/LaCie/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output_mcmc8/SESSION_1_world_ITR_9/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	rhessys_SingleFile3_4 = read.table(paste('/Volumes/LaCie/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output_mcmc8/SESSION_1_world_ITR_10/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') # assume csv
	rhessys_SingleFile3 = rhessys_SingleFile3_1
	rhessys_SingleFile3[,19] =  (rhessys_SingleFile3_1[,19]+rhessys_SingleFile3_2[,19]+rhessys_SingleFile3_3[,19]+ rhessys_SingleFile3_4[,19])*0.25
	rhessys_SingleFile3.date=as.Date(paste(rhessys_SingleFile3_1[,1], rhessys_SingleFile3_1[,2], rhessys_SingleFile3_1[,3],sep="-"),format="%d-%m-%Y")
		
		#period = seq.Date(from=as.Date('1990-10-1'), to=as.Date('2000-9-30') ,by="day")
		#period = seq.Date(from=as.Date('1999-10-1'), to=as.Date('2000-9-30') ,by="day") ## dry
		#period = seq.Date(from=as.Date('2002-10-1'), to=as.Date('2003-9-30') ,by="day") ## wet
		period = seq.Date(from=as.Date('2001-10-1'), to=as.Date('2011-9-30') ,by="day") ## validation
	tmp = match4DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, rhessys_SingleFile2.date, period) ### assume period is the most narrow band
	calobs.dailytimeSeriesMatch = tmp$xSelect
	rhessys.dailytimeSeriesMatch = tmp$ySelect
	rhessys2.dailytimeSeriesMatch = tmp$zSelect
	tmp = match2DailyTimeSeries(rhessys_SingleFile3.date, calobs.date0[calobs.dailytimeSeriesMatch])
	rhessys3.dailytimeSeriesMatch = tmp$xSelect
	calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])

	outputName = '/Volumes/LaCie/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt/rhessys/output_ws18_manuscript_oldway_TDR_mcmc11/ws18_basin_plot_2000_2010_MonthlySeasonIII_test_J24.pdf'
	
	modelPlot_Bolstad_NLCD( 
		calobs[calobs.dailytimeSeriesMatch,2], 
		rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], #nlcdII
		rhessys_SingleFile2[rhessys2.dailytimeSeriesMatch,],#bolstad
		calobs.dailytimeSeries, 
		outputName)
	
	modelPlot_Bolstad_NLCDII(
		calobs.date0[calobs.dailytimeSeriesMatch],
		calobs[calobs.dailytimeSeriesMatch,2], 
		rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], #nlcdII
		rhessys_SingleFile2[rhessys2.dailytimeSeriesMatch,],#bolstad
		rhessys_SingleFile3[rhessys3.dailytimeSeriesMatch,],#nlcd
		calobs.dailytimeSeries, 
		outputName)
		
	# date_, calobs_, rhessys_, rhessys2_, rhessys3_, dailytimeSeries_, output
	modelPlot_Bolstad_NLCDIII(
		date_ = calobs.date0 ,
		calobs_ = calobs[calobs.dailytimeSeriesMatch,2] , # vector
		rhessys_  = rhessys_SingleFile[rhessys.dailytimeSeriesMatch,] , #nlcdII
		rhessys2_ = rhessys_SingleFile2[rhessys2.dailytimeSeriesMatch,] ,#bolstad
		rhessys3_ = rhessys_SingleFile3[rhessys3.dailytimeSeriesMatch,] ,#nlcd
		dailytimeSeries_  = calobs.dailytimeSeries , 
		output = outputName
		)
		
	# modelPlot_Bolstad_NLCDIII(
		# date_ = calobs.date0 ,
		# calobs_ = calobs[,2] , # vector
		# rhessys_  = rhessys_SingleFile , #nlcdII
		# rhessys2_ = rhessys_SingleFile2 ,#bolstad
		# rhessys3_ = rhessys_SingleFile3 ,#nlcd
		# dailytimeSeries_  = calobs.dailytimeSeries , 
		# output = outputName
		# )
	
	
	##......................................................................... mulitple plot
	#.......... WS18 NLCD
rhessys_multiFlow = cbind(
	rhessys_SingleFile[,19],
	read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", 'manu7sw_basin.daily' ,sep=''),header=F,skip=1,sep=' ')[,19],
	read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", 'manu5sw_basin.daily' ,sep=''),header=F,skip=1,sep=' ')[,19],
	read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", 'manu5sw_basin.daily' ,sep=''),header=F,skip=1,sep=' ')[,19]
)
modelPlotStyle2Multiple(
	calobs[calobs.dailytimeSeriesMatch,2], 
	rhessys_multiFlow[rhessys.dailytimeSeriesMatch,], 
	calobs.dailytimeSeries, 
	'/Users/laurencelin/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output_ws18_manuscript_calibration/Testws8754_ws18_basin_plot_1980_2013_style2.pdf')
	
rhessys_multiFlow = cbind(
	rhessys_SingleFile[,19],
	read.table(paste(proj,"/", rhessys_Folder,"/", 'output_ws18mcmcII/SESSION_1_world_ITR_7',"/", fileName ,sep=''),header=F,skip=1,sep=' ')[,19],
	read.table(paste(proj,"/", rhessys_Folder,"/", 'output_ws18mcmcII/SESSION_1_world_ITR_20',"/", fileName ,sep=''),header=F,skip=1,sep=' ')[,19],
	read.table(paste(proj,"/", rhessys_Folder,"/", 'output_ws18mcmcII/SESSION_1_world_ITR_16',"/", fileName ,sep=''),header=F,skip=1,sep=' ')[,19]
)
modelPlotStyle2Multiple(
	calobs[calobs.dailytimeSeriesMatch,2], 
	rhessys_multiFlow[rhessys.dailytimeSeriesMatch,], 
	calobs.dailytimeSeries, 
	'/Users/laurencelin/workArch/University_of_North_Carolina/WS18/cwt18_mcmc/rhessys_tree/output_mcmc/Test8152329_ws18_basin_plot_1980_2013_style2.pdf')
	
	
	#.......... WS18 Bolstad v2
rhessys_multiFlow = cbind(
	rhessys_SingleFile[,19],
	read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", 'manu15sw_basin.daily' ,sep=''),header=F,skip=1,sep=' ')[,19],
	read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", 'manu23sw_basin.daily' ,sep=''),header=F,skip=1,sep=' ')[,19],
	read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/", 'manu29sw_basin.daily' ,sep=''),header=F,skip=1,sep=' ')[,19]
)
modelPlotStyle2Multiple(
	calobs[calobs.dailytimeSeriesMatch,2], 
	rhessys_multiFlow[rhessys.dailytimeSeriesMatch,], 
	calobs.dailytimeSeries, 
	'/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt/rhessys/output_ws18_manuscript_calibration/Testws8152329_ws18_basin_plot_1980_2013_style2.pdf')
	
rhessys_multiFlow = cbind(
	rhessys_SingleFile[,19],
	read.table(paste(proj,"/", rhessys_Folder,"/", 'output_ws18mcmcII/SESSION_1_world_ITR_7',"/", fileName ,sep=''),header=F,skip=1,sep=' ')[,19],
	read.table(paste(proj,"/", rhessys_Folder,"/", 'output_ws18mcmcII/SESSION_1_world_ITR_20',"/", fileName ,sep=''),header=F,skip=1,sep=' ')[,19],
	read.table(paste(proj,"/", rhessys_Folder,"/", 'output_ws18mcmcII/SESSION_1_world_ITR_16',"/", fileName ,sep=''),header=F,skip=1,sep=' ')[,19]
)
modelPlotStyle2Multiple(
	calobs[calobs.dailytimeSeriesMatch,2], 
	rhessys_multiFlow[rhessys.dailytimeSeriesMatch,], 
	calobs.dailytimeSeries, 
	'/Users/laurencelin/workArch/University_of_North_Carolina/WS14/Rhessys520_src_Laurence/setup_rhessys_5_20_debug2_bolstad_zone_bgc_cwt/rhessys/output_ws18mcmcII/Test7111620_ws18_basin_plot_1980_2013_style2.pdf')
	
	
	

##--------------------------------------------------------------------------------------------## cane FIA vs NLCD manuscript plot (Aug 2017)
	
		
	period=seq.Date(from=as.Date('2000-10-1'), to=as.Date('2004-9-30') ,by="day") 
	outputName = ''
	
	calobs_ = read.csv('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_FIA_version2/obs/canecreek_19881101_20150826.csv',stringsAsFactors=F)
	calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
	calobs= calobs_[calobsNonZero,]
	calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----
	
	
	# nlcd cane (overall fit)
	rhessys_SingleFile_1 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III/SESSION_2_world_ITR_1/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile_2 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III/SESSION_2_world_ITR_2/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile_3 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III/SESSION_2_world_ITR_3/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile_4 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III/SESSION_2_world_ITR_4/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile_5 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III/SESSION_2_world_ITR_5/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile_6 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III/SESSION_2_world_ITR_6/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile_7 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III/SESSION_2_world_ITR_7/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile_8 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III/SESSION_2_world_ITR_8/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile_9 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III/SESSION_2_world_ITR_9/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile_10 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III/SESSION_2_world_ITR_10/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile = rhessys_SingleFile_1
	rhessys_SingleFile[,19] =  0.1*(rhessys_SingleFile_1[,19]+
									rhessys_SingleFile_2[,19]+
									rhessys_SingleFile_3[,19]+
									rhessys_SingleFile_4[,19]+
									rhessys_SingleFile_5[,19]+
									rhessys_SingleFile_6[,19]+
									rhessys_SingleFile_7[,19]+
									rhessys_SingleFile_8[,19]+
									rhessys_SingleFile_9[,19]+
									rhessys_SingleFile_10[,19])
	rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile_1[,1], rhessys_SingleFile_1[,2], rhessys_SingleFile_1[,3],sep="-"),format="%d-%m-%Y")
	
	# nlcd cane (bias fit)
	rhessys_SingleFile1_1 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III_random/SESSION_2_world_ITR_402/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile1_2 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III_random/SESSION_2_world_ITR_107/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile1_3 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III_random/SESSION_2_world_ITR_489/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile1_4 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III_random/SESSION_2_world_ITR_314/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile1_5 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III_random/SESSION_2_world_ITR_395/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile1_6 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III_random/SESSION_2_world_ITR_488/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile1_7 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III_random/SESSION_2_world_ITR_180/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile1_8 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III_random/SESSION_2_world_ITR_132/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile1_9 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III_random/SESSION_2_world_ITR_465/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile1_10 = read.table(paste('/Volumes/storage/WSC_storage/cane/calibration_ch2w_ruleMod_1992-2012/rhessys/output_d2III_random/SESSION_2_world_ITR_12/rhessys_basin.daily' ,sep=''),header=T,sep=' ')
	rhessys_SingleFile1 = rhessys_SingleFile1_1
	rhessys_SingleFile1[,19] =  0.1*(rhessys_SingleFile1_1[,19]+
									rhessys_SingleFile1_2[,19]+
									rhessys_SingleFile1_3[,19]+
									rhessys_SingleFile1_4[,19]+
									rhessys_SingleFile1_5[,19]+
									rhessys_SingleFile1_6[,19]+
									rhessys_SingleFile1_7[,19]+
									rhessys_SingleFile1_8[,19]+
									rhessys_SingleFile1_9[,19]+
									rhessys_SingleFile1_10[,19])
	rhessys_SingleFile1.date=as.Date(paste(rhessys_SingleFile1_1[,1], rhessys_SingleFile1_1[,2], rhessys_SingleFile1_1[,3],sep="-"),format="%d-%m-%Y")
	
	
	
	# FIA type cane
	rhessys_SingleFile2_1 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/output_h3/SESSION_2_world_ITR_16/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile2_2 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/output_h3/SESSION_2_world_ITR_20/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile2_3 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/output_h3/SESSION_2_world_ITR_42/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile2_4 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/output_h3/SESSION_2_world_ITR_32/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile2_5 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/output_h3/SESSION_2_world_ITR_33/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile2_6 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/output_h3/SESSION_2_world_ITR_41/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile2_7 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/output_h3/SESSION_2_world_ITR_40/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile2_8 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/output_h3/SESSION_2_world_ITR_38/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile2_9 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/output_h3/SESSION_2_world_ITR_39/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile2_10 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_testingMCMC_sept15/cane_FIA_nlcd_local/rhessys/output_h3/SESSION_2_world_ITR_15/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile2 = rhessys_SingleFile2_1
	rhessys_SingleFile2[,19] =  0.1*(rhessys_SingleFile2_1[,19]+ 
									rhessys_SingleFile2_2[,19]+ 
									rhessys_SingleFile2_3[,19]+ 
									rhessys_SingleFile2_4[,19]+ 
									rhessys_SingleFile2_5[,19]+ 
									rhessys_SingleFile2_6[,19]+ 
									rhessys_SingleFile2_7[,19]+ 
									rhessys_SingleFile2_8[,19]+ 
									rhessys_SingleFile2_9[,19]+ 
									rhessys_SingleFile2_10[,19])	
	rhessys_SingleFile2.date=as.Date(paste(rhessys_SingleFile2_1[,1], rhessys_SingleFile2_1[,2], rhessys_SingleFile2_1[,3],sep="-"),format="%d-%m-%Y")
	
	
	
	# FIA xylem cane
	rhessys_SingleFile3_1 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_FIA_version2/rhessys/output/SESSION_2_world_ITR_46/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile3_2 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_FIA_version2/rhessys/output/SESSION_2_world_ITR_45/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile3_3 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_FIA_version2/rhessys/output/SESSION_2_world_ITR_38/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile3_4 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_FIA_version2/rhessys/output/SESSION_2_world_ITR_5/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile3_5 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_FIA_version2/rhessys/output/SESSION_2_world_ITR_47/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile3_6 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_FIA_version2/rhessys/output/SESSION_2_world_ITR_40/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile3_7 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_FIA_version2/rhessys/output/SESSION_2_world_ITR_39/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile3_8 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_FIA_version2/rhessys/output/SESSION_2_world_ITR_41/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile3_9 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_FIA_version2/rhessys/output/SESSION_2_world_ITR_36/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile3_10 = read.table(paste('/Volumes/storage/WSC_storage/cane/cane_FIA/cane_FIA_version2/rhessys/output/SESSION_2_world_ITR_49/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ') 
	rhessys_SingleFile3 = rhessys_SingleFile3_1
	rhessys_SingleFile3[,19] =  0.1*(rhessys_SingleFile3_1[,19]+ 
									rhessys_SingleFile3_2[,19]+ 
									rhessys_SingleFile3_3[,19]+ 
									rhessys_SingleFile3_4[,19]+ 
									rhessys_SingleFile3_5[,19]+ 
									rhessys_SingleFile3_6[,19]+ 
									rhessys_SingleFile3_7[,19]+ 
									rhessys_SingleFile3_8[,19]+ 
									rhessys_SingleFile3_9[,19]+ 
									rhessys_SingleFile3_10[,19])	
	rhessys_SingleFile3.date=as.Date(paste(rhessys_SingleFile3_1[,1], rhessys_SingleFile3_1[,2], rhessys_SingleFile3_1[,3],sep="-"),format="%d-%m-%Y")
	
	
	
	
	
	

	tmp = match4DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, rhessys_SingleFile2.date, period) ### assume period is the most narrow band
	calobs.dailytimeSeriesMatch = tmp$xSelect
	rhessys.dailytimeSeriesMatch = tmp$ySelect
	rhessys2.dailytimeSeriesMatch = tmp$zSelect
	calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])
	
	tmp = match3DailyTimeSeries(rhessys_SingleFile1.date, rhessys_SingleFile3.date, calobs.date0[calobs.dailytimeSeriesMatch]) 
	rhessys1.dailytimeSeriesMatch = tmp$xSelect
	rhessys3.dailytimeSeriesMatch = tmp$ySelect

	modelPlot_FIA_NLCD( 
		calobs[calobs.dailytimeSeriesMatch,2], 
		rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], 
		rhessys_SingleFile1[rhessys1.dailytimeSeriesMatch,], 
		rhessys_SingleFile2[rhessys2.dailytimeSeriesMatch,],
		rhessys_SingleFile3[rhessys3.dailytimeSeriesMatch,], 
		calobs.dailytimeSeries, 
		"~/Downloads/test.pdf")
	
	
	
	
	
	
	
	
	
	
	
	
##--------------------------------------------------------------------------------------------## michie
	

startingDate=as.Date('1990-10-1') #, 
endingDate=as.Date('2010-9-30') #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 

calobs_ = read.csv('/Users/laurencelin/Desktop/master_FIA/obs/flat_19251001_20130930.csv',stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----

rhessys_SingleFile = read.table('/Users/laurencelin/Downloads/WSC_regional_FIA2LAI/rhessys2010_climateB/output_michieTesting/FIAnlcdlocal_2010_param4_basin.daily',header=F,skip=1,sep=' ') 
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")

tmp = match3DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])

outputName='/Users/laurencelin/Downloads/WSC_regional_FIA2LAI/rhessys2010_climateB/output_michieTesting/FIAnlcdlocal_2010_param4_basin_1990_2010.pdf'
modelPlotStyle2( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries, outputName)

##--------------------------------------------------------------------------------------------## Eno
arg=c(
	"/Volumes/storage/WSC_storage/eno/lowerEno", 
	"eno_19630901_20150904.csv",
	".", "output", "rhessys_basin.daily",
	"2000-10-1","2004-9-30",
	"/Volumes/storage/WSC_storage/swift/rhessys_climateO_FIA2local/output/SESSION_7_world_ITR_7/rhessys_itr7_plot_2002_2005_style2B.pdf"
)

proj = arg[1]
obsfile = arg[2]
rhessys_Folder = arg[3]
outputFolder = arg[4]
fileName = arg[5]
startingDate=as.Date(arg[6]) #, 
endingDate=as.Date(arg[7]) #, 
period=seq.Date(from=startingDate, to=endingDate ,by="day") 

calobs_ = read.csv(paste(proj,"/obs/", obsfile,sep=""),stringsAsFactors=F);
calobsNonZero = !is.na(calobs_[,2]) & calobs_[,2]> 0; #0.214285714 #0.00124663 # correct 0 value
calobs= calobs_[calobsNonZero,]
#calobs.date0 = convertDateExcelMDY(calobs[,1],beginningYY=20) ##<<-----
calobs.date0 = convertDateExcelMDY(calobs[,1], beginningYY=19) ##<<-----

upQ_ = read.csv(paste(proj,"/obs/upperEnoQ1986.csv",sep=""),stringsAsFactors=F);
upQNonZero = !is.na(upQ_[,2]) & upQ_[,2]>0 # correct 0 value
upQ = upQ_[upQNonZero,]
upQ.date0 = convertDateExcelMDY(upQ[,1])

rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/",'SESSION_1_world_ITR_1/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ')
rhessys_SingleFile.date=as.Date(paste(rhessys_SingleFile[,1], rhessys_SingleFile[,2], rhessys_SingleFile[,3],sep="-"),format="%d-%m-%Y")

tmp = match4DailyTimeSeries(calobs.date0, rhessys_SingleFile.date, upQ.date0, period) ### assume period is the most narrow band
calobs.dailytimeSeriesMatch = tmp$xSelect
rhessys.dailytimeSeriesMatch = tmp$ySelect
upQ.dailytimeSeriesMatch = tmp$zSelect
calobs.dailytimeSeries = dailyTimeSeries(calobs.date0[calobs.dailytimeSeriesMatch])

for(i in 1:12){
	rhessys_SingleFile = read.table(paste(proj,"/", rhessys_Folder,"/", outputFolder,"/",'SESSION_1_world_ITR_',i,'/rhessys_basin.daily' ,sep=''),header=F,skip=1,sep=' ')
	rhessys_SingleFile[rhessys.dailytimeSeriesMatch,19]= 0.5565124*rhessys_SingleFile[rhessys.dailytimeSeriesMatch,19]+ 0.4434876*upQ[upQ.dailytimeSeriesMatch,2]
	outputName=paste(proj,"/", rhessys_Folder,"/", outputFolder,"/",'SESSION_1_world_ITR_',i,'/rhessys_itr',i,'_plot_2000_2003_style2New.pdf',sep='')
	modelPlotStyle2( calobs[calobs.dailytimeSeriesMatch,2], rhessys_SingleFile[rhessys.dailytimeSeriesMatch,], calobs.dailytimeSeries, outputName)
}#i





##--------------------------------------------------------------------------------------------##	
}#



	
	
	
	