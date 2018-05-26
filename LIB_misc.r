#----------------------------------------< plotting >-------------------------------------------#
makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
    blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

#----------------------------------------< fittness >-------------------------------------------#
NSE=function(obs,y){
	#assume no zero/NA/Inf, filtered, clean
	obsM = mean(obs,na.rm=T)
	return <- 1 - sum((obs-y)^2,na.rm=T)/sum((obs-obsM)^2,na.rm=T)
}

#----------------------------------------< I/O >-------------------------------------------#
LIBread.csv=function(x,h=T){
	#assume x is a the file name (text)
	return <- read.csv(x,stringsAsFactors=F,header=h)
}

LIBread.table=function(x,h=T){
	#assume x is a the file name (text)
	return <- read.table(x,stringsAsFactors=F,header=h)
}

LIBconvertSingleExcelMDY2RecentDate=function(y){
	# assume x is a just text string, not vector 
	w=as.numeric(unlist(strsplit(y[1],"/")))
	if(w[3]<1000){
		#(year is 2 digit)
		currentYear=as.numeric(format(Sys.Date(),"%y"))
		if(w[3]>currentYear){w[3]=w[3]+1900}
		else{w[3]=w[3]+2000}
	}
	return <- as.Date(paste(w[3],w[1],w[2],sep="-"),format="%Y-%m-%d")
}


convertDateExcelMDY=function(x,beginningYY=19){
	#assume x is a vector of text
	# beginningYY 2 digit year (the front missing part)
	
	w=as.numeric(unlist(strsplit(x[1],"/")))
	if(w[3]>1000){
		return <- as.Date(x,format="%m/%d/%Y")
	}else{
		bYY = beginningYY
		y = rep(as.Date("1900-01-01",format="%Y-%m-%d"),length(x))
		for(i in 1:length(x)){
			
			newYYYY=0
			w=as.numeric(unlist(strsplit(x[i],"/")))
			if(w[3]<1000){
				#2 digit
				newYYYY =w[3]+ bYY*100
			}
			h = as.Date(paste(newYYYY,w[1],w[2],sep="-"),format="%Y-%m-%d")
			if(i>1){
				if(h<y[i-1]){
					bYY= bYY+1
					newYYYY =w[3]+ bYY*100
					h = as.Date(paste(newYYYY,w[1],w[2],sep="-"),format="%Y-%m-%d")
				}
			}#i
			y[i]=h
	
		}#i
		return <-y
	}
}
#----------------------------------------< algorithm >-------------------------------------------#
smoothMean=function(x){
	len = length(x)
	y = rep(NA,len)
	y[1] = mean(x[1:2])
	for(i in 2:(len-1)){
		y[i]=mean(x[(i-1):(i+1)])
	}#i
	y[len] = mean(x[(len-1):len])
	return(y)
}

grpSums=function(x,y){
	grp = unique(y)
	result = tapply(x,INDEX=y, FUN=sum, na.rm=T)
	return(result[match(grp,as.numeric(names(result))])
}#function

grpMeans=function(x,y){
	grp = unique(y)
	result = tapply(x,INDEX=y, FUN=mean, na.rm=T)
	return(result[match(grp,as.numeric(names(result))])
}#function

grpSDs=function(x,y){
	grp = unique(y)
	result = tapply(x,INDEX=y, FUN=sd, na.rm=T)
	return(result[match(grp,as.numeric(names(result))])
}#function
		      
		      
		      
grpStats=function(x,y){
	grp = unique(y)
	result=matrix(NA,length(grp),13); colnames(result)=c('grp','mean','sd','min','max','q0025','q010','q025','q050','q075','q090','q0975','len')
	for(i in 1:length(grp)){
		result[i,]= c(
			grp[i],
			mean(x[y==grp[i]],na.rm=T),
			sd(x[y==grp[i]],na.rm=T),
			min(x[y==grp[i]],na.rm=T),
			max(x[y==grp[i]],na.rm=T),
			quantile(x[y==grp[i]],probs=c(0.025,0.1,0.25,0.5,0.75,0.9,0.975),na.rm=T),
			sum(y==grp[i])
		)
		
	}
	return(result)
}#function




LIBlast=function(x){
	return <- x[length(x)]
}


grpZnorm=function(x,y){
	## return full length not aggregrated length
	
	grp = unique(y)
	result=rep(NA,length(x))
	for(i in 1:length(grp)){
		tmp = x[y==grp[i]]
		mean_ = mean(tmp,na.rm=T)
		sd_ = sd(tmp,na.rm=T)
		
		result[y==grp[i]]=(tmp-mean_)/sd_
	}
	return(result)
}#function
grpProp=function(x,y){
	## return full length not aggregrated length
	
	grp = unique(y)
	result=rep(NA,length(x))
	for(i in 1:length(grp)){
		tmp = x[y==grp[i]]
		sum_ = sum(tmp,na.rm=T)
		if(sum_ > 0){
			result[y==grp[i]]=tmp/sum_
		}else{
			result[y==grp[i]]=0
		}
		
	}
	return(result)
}#function
grpMemberID=function(x,y){
	## return full length not aggregrated length
	
	grp = unique(y)
	result=rep(NA,length(x))
	for(i in 1:length(grp)){
		cond = y==grp[i]
		result[cond]=1:sum(cond)
	}
	return(result)
}#function


rowMaxs=function(x){
	#assume x is a matrix
	return <- apply(x,1,max)
}

grpQuants=function(x,y,q){
	grp = unique(y)
	result=rep(NA,length(grp))
	for(i in 1:length(grp)){
		result[i]= quantile(x[y==grp[i]],probs=c(q),na.rm=T)
	}
	return(result)
}#function

grpMins=function(x,y){
	grp = unique(y)
	result=rep(NA,length(grp))
	for(i in 1:length(grp)){
		result[i]=min(x[y==grp[i]],na.rm=T)
	}
	return(result)
}#function

grpMaxs=function(x,y){
	grp = unique(y)
	result=rep(NA,length(grp))
	for(i in 1:length(grp)){
		result[i]=max(x[y==grp[i]],na.rm=T)
	}
	return(result)
}#function





nonZeroRow=function(x){
	# x is a matrix, no NAs
	return <- rowSums(x>0)==ncol(x)
}

colMaxs=function(x){
	# x is a matrix, no NAs
	return <- apply(x,2,max)
}

colMins=function(x){
	# x is a matrix, no NAs
	return <- apply(x,2,min)
}

colQuants=function(x,q){
	# x is a matrix, no NAs
	result=rep(NA,ncol(x))
	for(i in 1:length(result)){
		result[i]=quantile(x[,i],probs=c(q),na.rm=T)
	}#i
	return <- result
}

colSlopes=function(x){
	return <- (apply(x,2,max)-apply(x,2,min))/nrow(x)
}

normalize=function(x){
	xmean=mean(x)
	xsd=sd(x)
	return <- (x-xmean)/xsd
}

colSDs=function(x){
	return <- apply(x,2, sd)
}


colNorms=function(x){
	return <- apply(x,2, normalize)
}




accumulate=function(x){
	if(length(x)==1){
		return <- x
	}else{
		for(i in 2:length(x)){
			x[i]=x[i-1]+x[i]
		}
		return <- x
	}
}

LIBbinary=function(x){
	# >0 = 1; <=0 = 0
	x[x>0]=1; x[x<0]=0;
	return <- x
}

LIBbinaryRev=function(x){
	# ! LIBbinary
	x[x>0]=-1; x[x==0]=1; x[x<0]=0;
	return <- x
}

LIBrep=function(x,each){
	# rep(x, each) function with fixible each
	result = c()
	for(i in 1:length(x)){
		result=append(result,rep(x[i],each=each[i]))
	}#i
	return <- result
}

LIBfirstEachGrpX=function(x,each){
	#getting x at the beginning of each group
	tmp = c(1)
	for(i in 2:length(each)){
		tmp=append(tmp,tmp[i-1]+each[i-1])
	}#i
	return <- list(at=tmp,lbl=x[tmp])
}

LIBfirstEachGrpAt=function(x){
	#getting x at the beginning of each group
	each = as.numeric(table(x))
	if(length(each)==1){return <- c(1)}
	else{
		return <- accumulate(c(1,each[1:(length(each)-1)]))
	}#else
}

uniqueLen=function(x){
	return <- length(unique(x))
}

combineNumeric=function(x){
	# assume x is numeric vector 
	return <- as.numeric(paste(format(x,scientific=F),collapse=''))
}

combIndex=function(x){
	#assume x is a numeric matrix (maybe random order)
	#there are ncol(x) factors and each factor i has length(unique(x[i])) variation
	#m = as.numeric(table(apply(x,1, combineNumeric)))
	return <- as.numeric(apply(x,1, combineNumeric))
}

combReassignIndex=function(x){
	y = x
	for(j in 1:ncol(y)){
		yj = unique(y[j])
		for(i in 1:length(yj)){
			y[y[,j]==yj[i],j]=i
		}#i
	}#y
	return <- as.numeric(apply(y,1, combineNumeric))
}

LIBmatchOrder=function(x,y){
	##
	hold=(1:length(y))[order(y)]
	#return <- (x[order(x)])[order(hold)] # testing
	return <- order(hold)

}


LIBfirstEach2GrpAt=function(x){
	#getting x at the beginning of each group
	each = as.matrix((table(x)))#<<---------issue
	if(length(each)==1){return <- c(1)}
	else{
		each.name = as.numeric(rownames(each)) ## order got messed up
		sortOrder = unique(x); ## this is the order of the file
		
		#.... slow threashold here
		# tmp=c(1)
		# for(i in 1:(length(sortOrder)-1)){
			# tmp=append(tmp,each[each.name== sortOrder[i]])
		# }#i
		tmp = c(1,each[LIBmatchOrder(each.name, sortOrder)])
		
		return <- accumulate(tmp[1:(length(tmp)-1)])
	}#else
}

LIBnrow=function(x){
	# asssume x is a matrix or dataframe or vector
	if(is.null(nrow(x))){return <- 1}
	else{return <- nrow(x)}
}

LIBas.numeric=function(x){
	#assume x is text
	# in cases like "-9999.000000.1"
	result = suppressWarnings(as.numeric(x))
	if(is.na(result)){
		tmp=unlist(strsplit(x,"\\."))
		if(length(tmp)==1){return <- 0;}else{return <- as.numeric(paste(tmp[1],tmp[2],sep=".")); }
	}else{
		return <- result
	}
	
}

CDFstepseq=function(x){
	# assume x is positive only
	return <- exp(seq(round(log(min(x))),round(log(max(x))), 0.1))
}

monthlyAnomaly=function(x,grp_mom,LTmonthlymean,LTmonthlyMM){
	
	dataMM = unique(grp_mom)
	result = rep(NA,length(x))
	for(i in 1:length(dataMM) ){
		result[grp_mom==dataMM[i]] = x[grp_mom==dataMM[i]] - LTmonthlymean[LTmonthlyMM==dataMM[i]]
	}#i
	
	return <- result
}




permutation = function(y) {
	#assume y is a pattern already
	p <- y
	tmp = NULL
	while(!is.null(p)) {
		#cat(p,"\n")
		tmp=rbind(tmp,p)
		p <- next.perm(p)
	}
	return <- tmp
}

myMin = function(x){
	return <- min(x,na.rm=T)
}

myMax = function(x){
	return <- max(x,na.rm=T)
}

myCl5 = function(x){
	return <- quantile(x,na.rm=T, probs=c(0.5))
}

myCl025 = function(x){
	return <- quantile(x,na.rm=T, probs=c(0.025))
}

myCl975 = function(x){
	return <- quantile(x,na.rm=T, probs=c(0.975))
}


##---- meaning pattern along the col, each row is replication
colPattern = function(x){
	# assume x is a matrix and "col = site"
	result = cbind(
		colmean = rowMeans(x,na.rm=T),
		apply(x,1,myMin),
		apply(x,1,myMax),
		apply(x,1, myCl025),
		apply(x,1, myCl975),
		apply(x,1, myCl5),
		apply(x,1,sd),
		rep(ncol(x),nrow(x))
	)
	colnames(result)=c("mean","min","max","q025","q975","q05","sd",'count')
	return <- result
}

rowPattern = function(x){
	# assume x is a matrix and "row = site"
	result = cbind(
		colmean = colMeans(x,na.rm=T),
		apply(x,2,myMin),
		apply(x,2,myMax),
		apply(x,2, myCl025),
		apply(x,2, myCl975),
		apply(x,2, myCl5),
		apply(x,2,sd),
		rep(nrow(x),ncol(x))
	)
	colnames(result)=c("mean","min","max","q025","q975","q05",'sd','count')
	return <- result
}

LIBrowSums = function(x){
	#assume x is a two dimensional array
	if(is.null(dim(x)) ){
		return <- x
	}else{
		return <- rowSums(x)
	}
}

colWeightedMean = function(x,weight){
	result = rep(NA,ncol(x))
	for(i in 1:ncol(x)){
		result[i] = sum(weight*x[,i])
	}#i
	return <- result
}

LIBstat = function(x){
	#assume x is a numeric vector
	
	return <- list(
		m = mean(x,na.rm=T),
		sd = sd(x,na.rm=T),
		n = sum(!is.na(x)),
		q025 = quantile(x,na.rm=T, probs=c(0.025)),
		q050 = quantile(x,na.rm=T, probs=c(0.5)),
		q975 = quantile(x,na.rm=T, probs=c(0.975))
	)
}















