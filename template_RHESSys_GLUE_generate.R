source("~/Dropbox/LIB_Rscript/LIB_misc.r")
source("~/Dropbox/LIB_Rscript/LIB_dailytimeseries2.r")


paramBoundary = matrix(NA,8,2)
rownames(paramBoundary)=c('s1','s2','s3','sv1','sv2','gw1','gw2', 'snowenergy')
colnames(paramBoundary)=c('min','max')
paramBoundary[1,]=c(2,20.0) #s1
paramBoundary[2,]=c(50,300.0) #s2
paramBoundary[3,]=c(0.1,1) #s3 
paramBoundary[4,]=c(0.0001,0.08) #sv1
paramBoundary[5,]=c(1.0,300.0) #sv2
paramBoundary[6,]=c(0.01,0.1) #gw1 
paramBoundary[7,]=c(0.01,0.99) #gw2 
paramBoundary[8,]=c(0.5,2) #snowenergy
paramRange = paramBoundary[,2]-paramBoundary[,1]
paraminSD = paramRange*0.1


itr = 1:500
param = cbind(
	itr,
	runif(length(itr), paramBoundary[1,1], paramBoundary[1,2]),
	runif(length(itr), paramBoundary[2,1], paramBoundary[2,2]),
	runif(length(itr), paramBoundary[3,1], paramBoundary[3,2]),
	runif(length(itr), paramBoundary[4,1], paramBoundary[4,2]),
	runif(length(itr), paramBoundary[5,1], paramBoundary[5,2]),
	runif(length(itr), paramBoundary[6,1], paramBoundary[6,2]),
	runif(length(itr), paramBoundary[7,1], paramBoundary[7,2]),
	runif(length(itr), paramBoundary[8,1], paramBoundary[8,2])
)			

outputfile = 'parallelRun103.sh'
jobIDnum = 12323
write('#!/bin/bash', outputfile, append=F)
for(i in 1:length(itr)){
	cmd = paste(
	#paste('sbatch -o output/log.txt -J p', jobIDnum, sep=''),  # for cluster
	#'--export=v=\'',  # for cluster
	'./rhesys_bin',
	'-st 1985 1 1 1 -ed 2011 10 1 1',
	'-b -t tecfiles/tec_daily.txt',
	'-w worldfiles_ws18/combine_oldwayRZ5_soiltest3b',
	'-whdr worldfiles_ws18/ws18Bolstadcomb.hdr',
	'-r flow_ws18/ws18Bolstad.flow',
	'-pre', paste('output/rhessys',param[i,1],sep=''),
	paste("-s",param[i,2],param[i,3],param[i,4]),
	paste("-sv",param[i,5],param[i,6]),
	paste("-gw",param[i,7],param[i,8])
	#paste('-snowenergy',param[i,9]), # not calibrating this parameter
	#'\'', # for cluster
	#'Rivanna_std.sh') # for cluster
	)
	write(cmd, outputfile,append=T)
}




