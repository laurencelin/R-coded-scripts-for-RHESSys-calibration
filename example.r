## calling library
source('https://raw.githubusercontent.com/laurencelin/R-coded-scripts-for-RHESSys-calibration/master/LIB_RHESSys_calibration.R')



## -------------------------------------------------------------------------------------------------------------------------------------------------------------

	## generate random parameters for calibraitons and script file for UVA Rivanna (SLURM) job submission

	RHESSysParamBoundaryDefault # contains all RHESSys calibration parameters
	# see https://github.com/RHESSys/RHESSys/wiki for more information

		# add a new parameter / change parameter search boundary
		RHESSysParamBoundaryDefault$gw1 = c(0.001,0.4) # change boundary values
		RHESSysParamBoundaryDefault$gw2 = c(0.001,0.4) # change boundary values
		RHESSysParamBoundaryDefault$rtz = c(0.5,4.0) # e.g., add new parameter and its bounaries 
	
	# use the block below to customize the calibration parameter search
	itr = 1:1000 # R code; mean from 1 to 1000; you can start from 1001 to 2000 too.
	num=length(itr) 
	param = data.frame(itr = itr)
	param$s1 = runif(num, RHESSysParamBoundaryDefault$s1[1],RHESSysParamBoundaryDefault$s1[2])
	param$s2 = runif(num, RHESSysParamBoundaryDefault$s2[1],RHESSysParamBoundaryDefault$s2[2])
	param$sv1 = runif(num, RHESSysParamBoundaryDefault$sv1[1],RHESSysParamBoundaryDefault$sv1[2])
	param$sv2 = runif(num, RHESSysParamBoundaryDefault$sv2[1],RHESSysParamBoundaryDefault$sv2[2])
	param$gw1 = runif(num, RHESSysParamBoundaryDefault$gw1[1],RHESSysParamBoundaryDefault$gw1[2])
	param$gw2 = runif(num, RHESSysParamBoundaryDefault$gw2[1],RHESSysParamBoundaryDefault$gw2[2])
	# some additional parameters may be used for calibration
	param$snowEs = runif(num, RHESSysParamBoundaryDefault$snowEs[1],RHESSysParamBoundaryDefault$snowEs[2])
	param$snowTs = runif(num, RHESSysParamBoundaryDefault$snowTs[1],RHESSysParamBoundaryDefault$snowTs[2])
		
	# use the block below to customize the RHESSys model runs
	RHESSys_arg = paste(
		'path_to_rhessys_binary',  # (relative) path to RHESSys compiled binary code
		'-st 2006 1 1 1 -ed 2017 12 1 1', # start time and end time; typical span-up time is 5-year for a 62 ha catchment
		'-b', # basin scale output
		'-newcaprise -capr 0.001 -gwtoriparian -capMax 0.01', # some customized flags
		'-t tecfiles/tec_daily.txt',
		'-w worldfiles_fc/worldfile', # worldfile
		'-whdr worldfiles_fc/worldfile.hdr', # worldfile header
		'-r flows/subTestfc.txt flows/surfTestfc.txt'  # flowtable
		)
	
	# call a library function to generate a shell/bash file that contains all iterated RHESSys runs for UVA Rivanna
	RivannaJobs(
		RHESSys_arg, # defined above
		'output', # output folder name in the RHESSys model folder
		param, # defined above
		'../parallelRun18101fc.sh') # the path/name of the shell/bash file (keep it for it would be used later)	


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
	## Once all the submitted jobs are done on UVA Rivanna, download all RHESSys outputs and the shell/bash file
	
	# set paths/names of the project folder and RHESSys model, ...
	argList$projPath		= "~/BAISMAN" # project folder
	argList$orbFile			= "usgs01583580.csv" # observed flow time series (certain format is required; see below)
	argList$startDate		= "2010-10-1" # start time for calibration (may not the same as model start time)
	argList$endDate			= "2017-9-30" # end time for calibration (may not the same as model end time)
	argList$RHESSysModel		= "rhessys_baisman10m" # RHESSys model folder
	argList$RHESSysOutput		= "output_parallelRun18101fc" # the output folder 
	argList$runScript		= "parallelRun18101fc.sh" # the shell/bash file
	
	# call a library function to caluate a list of fittness and generate a basic plot for each model run.
	outputfile = evaluateModel(argList)
	
	
	
	
