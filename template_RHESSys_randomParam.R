

########## parameter settings

	paramBoundary = matrix(NA,8,2)
	rownames(paramBoundary)=c('s1','s2','s3','sv1','sv2','gw1','gw2', 'snowenergy')
	colnames(paramBoundary)=c('min','max')
	paramBoundary[1,]=c(0.001,20) #s1
	paramBoundary[2,]=c(0.1,300.0) #s2
	paramBoundary[3,]=c(0.1,1) #s3 
	paramBoundary[4,]=c(0.001,20) #sv1
	paramBoundary[5,]=c(0.1,300.0) #sv2
	paramBoundary[6,]=c(0.001,0.2) #gw1 
	paramBoundary[7,]=c(0.001,0.2) #gw2 
	paramBoundary[8,]=c(0.5,2) #snowenergy
	

########## job settings

	outputfile = 'parallelRun103.sh'
	jobIDnum = 12323
	outputFOLDER = 'output'
	
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
	
########## platform settings

	sbatch_cluster_header = paste('sbatch -o ', outputFOLDER, '/log.txt -J p', jobIDnum ' --export=v=\'', sep='')
	sbatch_cluster_end = '\' Rivanna_std.sh'
	
	bsub_cluster_header = paste('bsub -q day -G xxx_pi -M 1 -o ', outputFOLDER, '/log.txt ./rhesys_bin ', sep='') 
	bsub_cluster_end = ''
	
	local_header = './rhessys_bin '
	local_end = ''
		
########## writing the script

	## choose platform
	cmd_header = sbatch_cluster_header
	cmd_end = sbatch_cluster_end
	
write('#!/bin/bash', outputfile, append=F)
for(i in 1:length(itr)){
	 
	RHESSys_arg = paste(
		'-st 1985 1 1 1 -ed 2011 10 1 1',
		'-b -t tecfiles/tec_daily.txt',
		'-w worldfiles_ws18/combine_oldwayRZ5_soiltest3b',
		'-whdr worldfiles_ws18/ws18Bolstadcomb.hdr',
		'-r flow_ws18/ws18Bolstad.flow',
		'-pre', paste(outputFOLDER,'/rhessys',param[i,1],sep=''),
		paste("-s",param[i,2],param[i,3],param[i,4]),
		paste("-sv",param[i,5],param[i,6]),
		paste("-gw",param[i,7],param[i,8])
		#paste('-snowenergy',param[i,9]), # not calibrating this parameter
	)
	
	cmd = paste(cmd_header, RHESSys_arg, cmd_end, sep='')
	
	write(cmd, outputfile,append=T)
}




