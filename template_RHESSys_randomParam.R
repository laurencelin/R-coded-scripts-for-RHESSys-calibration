

RHESSysParamBoundaryDefault = data.frame(s1=c(0.001,20))
RHESSysParamBoundaryDefault $s2 = c(0.1,300.0)
RHESSysParamBoundaryDefault $s3 = c(0.1,20)
RHESSysParamBoundaryDefault $sv1 = c(0.001,20)
RHESSysParamBoundaryDefault $sv2 = c(0.1,300.0)
RHESSysParamBoundaryDefault $gw1 = c(0.001,0.2)
RHESSysParamBoundaryDefault $gw2 = c(0.001,0.2)
RHESSysParamBoundaryDefault $snowEs = c(0.5,2)
RHESSysParamBoundaryDefault $snowTs = c(0.5,2)

# for example,
# RHESSys_arg = paste(
		# '-st 1995 1 1 1 -ed 2011 10 1 1',
		# '-b -t tecfiles/tec_daily.txt',
		# '-w worldfiles/combine_oldwayRZ5_soiltest3b',
		# '-whdr worldfiles_ws18/ws18Bolstadcomb.hdr',
		# '-r flow_ws18/ws18Bolstad.flow')

RivannaJobs=function(RHESSys_arg, outputFOLDER, param, outputfile){
	
	
	jobIDnum = floor(as.numeric(Sys.time()))
	
	sbatch_cluster_header = paste('sbatch -o ', outputFOLDER, '/log.txt -J p', jobIDnum, ' --export=v=\'', sep='')
	sbatch_cluster_end = '\' Rivanna_std.sh'
	cmd_header = sbatch_cluster_header;
	cmd_end = sbatch_cluster_end
	
	
	paramNames=colnames(param)
	if('s3'%in% paramNames){
		sParamLine = paste('-s',paste(param[,'s1'],param[,'s2'],param[,'s3']))
	}else{
		sParamLine = paste('-s',paste(param[,'s1'],param[,'s2']))
	}
	svParamLine = paste('-sv',paste(param[,'sv1'],param[,'sv2']))
	gwParamLine = paste('-gw',paste(param[,'gw1'],param[,'gw2']))
	otherParamLine = ''
	
	# check for non default parameter	
	cond = !(paramNames %in% c("itr","s1","s2","s3","sv1","sv2","gw1","gw2"))
	if(sum(cond)>0){
		otherParam = paramNames[cond]
		j=1; 
		otherParamLine = paste(paste('-',otherParam[j],sep=''),param[, otherParam[j]])
		if(sum(cond)>1) for(j in 2:length(otherParam)) otherParamLine = paste(otherParamLine, paste(paste('-',otherParam[j],sep=''),param[, otherParam[j]]))
				
		AllparamLine = paste(sParamLine, svParamLine, gwParamLine, otherParamLine)
	}else{
		AllparamLine = paste(sParamLine, svParamLine, gwParamLine)
	}

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









