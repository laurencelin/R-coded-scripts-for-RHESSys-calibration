modelPlotStyle2 = function( calobs_, rhessys_, dailytimeSeries_, output){
	
	
	calobsDayFlow = calobs_
	calobsWeekFlow = tapply(calobsDayFlow, dailytimeSeries_ $yy_woy, sum )
	calobsMonthFlow = tapply(calobsDayFlow, dailytimeSeries_ $yy_month, sum )
	calobsYearFlow = tapply(calobsDayFlow, dailytimeSeries_ $wy, sum )
	weeklyCDF=ecdf(calobsWeekFlow)

	rhessysDayFlow = rhessys_[,19]#flow
	rhessysWeekFlow = tapply(rhessysDayFlow, dailytimeSeries_ $yy_woy, sum )
	rhessysMonthFlow = tapply(rhessysDayFlow, dailytimeSeries_ $yy_month, sum )
	rhessysYearFlow = tapply(rhessysDayFlow, dailytimeSeries_ $wy, sum )
	rhessysDayResidue = rhessysDayFlow - calobsDayFlow
	
	day_plottime = as.Date(paste(dailytimeSeries_ $day, dailytimeSeries_ $month, dailytimeSeries_ $year,sep="-"),format="%d-%m-%Y")
	week_plottime = day_plottime[tapply(seq_along(day_plottime), dailytimeSeries_ $yy_woy, function(dd){dd[1]})]
	month_plottime = day_plottime[tapply(seq_along(day_plottime), dailytimeSeries_ $yy_month, function(dd){dd[1]})]
	year_plottime = day_plottime[tapply(seq_along(day_plottime), dailytimeSeries_ $wy, function(dd){dd[1]})]
	
	
	pdf(output,height=9,width=8)
	layout(matrix(1:10,nrow=5,ncol=2,byrow=T))
	par(mar=c(4,4,1,1) )
	
	ymin = min(rhessysYearFlow, calobsYearFlow)
	ymax = max(rhessysYearFlow, calobsYearFlow)
	plot(year_plottime ,calobsYearFlow,type='l',ylim=c(ymin,ymax), xlab="");
	lines(year_plottime ,rhessysYearFlow,col='red'); 
	
	ymin = min(rhessysYearFlow, calobsYearFlow)
	ymax = max(rhessysYearFlow, calobsYearFlow)
	plot(rhessysYearFlow ,calobsYearFlow, ylim=c(ymin,ymax));abline(a=0,b=1,lty=2)
	
	
	
	ymin = min(rhessysMonthFlow, calobsMonthFlow)
	ymax = max(rhessysMonthFlow, calobsMonthFlow)
	plot(month_plottime ,calobsMonthFlow,type='l',ylim=c(ymin,ymax), xlab="");
	lines(month_plottime ,rhessysMonthFlow,col='red'); 
	ymin = min(rhessysMonthFlow, calobsMonthFlow)
	ymax = max(rhessysMonthFlow, calobsMonthFlow)
	plot(rhessysMonthFlow ,calobsMonthFlow, ylim=c(ymin,ymax));abline(a=0,b=1,lty=2)
	
	
	
	ymin = min(rhessysWeekFlow, calobsWeekFlow)
	ymax = max(rhessysWeekFlow, calobsWeekFlow)
	plot(week_plottime ,calobsWeekFlow,type='l',ylim=c(ymin,ymax), xlab="");
	lines(week_plottime ,rhessysWeekFlow,col='red'); 
	###------------------------- log 
	ymin = min(rhessysWeekFlow, calobsWeekFlow)
	ymax = max(rhessysWeekFlow, calobsWeekFlow)
	plot(rhessysWeekFlow ,calobsWeekFlow, ylim=c(ymin,ymax),xlim=c(ymin,ymax),log='xy');abline(a=0,b=1,lty=2)
	
	
	ymin = min(rhessysDayFlow, calobsDayFlow)
	ymax = max(rhessysDayFlow, calobsDayFlow)
	plot(day_plottime ,calobsDayFlow,type='l',ylim=c(ymin,ymax), xlab="");
	lines(day_plottime ,rhessysDayFlow,col='red'); 
	###------------------------- log 
	ymin = min(rhessysDayFlow, calobsDayFlow)
	ymax = max(rhessysDayFlow, calobsDayFlow)
	plot(rhessysDayFlow ,calobsDayFlow, ylim=c(ymin,ymax),xlim=c(ymin,ymax),log='xy' );abline(a=0,b=1,lty=2)
	
	yy1 = log(rhessysWeekFlow,10)
	yy2 = log(calobsWeekFlow,10)
	ymin = min(yy1,yy2)
	ymax = max(yy1,yy2)
	plot(week_plottime ,yy2,type='l',ylim=c(ymin,ymax) , xlab="",ylab="log weeklyflow",yaxt='n');
	lines(week_plottime ,yy1,col='red'); 
	axis(2,at=round(ymin):round(ymax), labels=10^(round(ymin):round(ymax)) )
	
	yy1 = log(rhessysDayFlow,10)
	yy2 = log(calobsDayFlow,10)
	ymin = min(yy1,yy2)
	ymax = max(yy1,yy2)
	plot(day_plottime ,yy2,type='l',ylim=c(ymin,ymax),xlab="",ylab="log dailyflow" ,yaxt='n');
	lines(day_plottime ,yy1,col='red'); 
	axis(2,at=round(ymin):round(ymax), labels=10^(round(ymin):round(ymax)) )
	
	dev.off()
}#
