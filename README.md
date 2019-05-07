# R-coded-scripts-for-RHESSys-calibration
examples below

source('https://raw.githubusercontent.com/laurencelin/R-coded-scripts-for-RHESSys-calibration/master/LIB_RHESSys_calibration.R')


RHESSysParamBoundaryDefault 
// add a new parameter / change parameter search boundary
	RHESSysParamBoundaryDefault$gw1 = c(0.001,0.4)
	RHESSysParamBoundaryDefault$gw2 = c(0.001,0.4)
  
  itr = 1:1000
	num=length(itr) 
	param = data.frame(itr = itr) # construct the parameter set for calibration. 
	param$s1 = runif(num, RHESSysParamBoundaryDefault$s1[1],RHESSysParamBoundaryDefault$s1[2])
	param$s2 = runif(num, RHESSysParamBoundaryDefault$s2[1],RHESSysParamBoundaryDefault$s2[2])
	param$sv1 = runif(num, RHESSysParamBoundaryDefault$sv1[1],RHESSysParamBoundaryDefault$sv1[2])
	param$sv2 = runif(num, RHESSysParamBoundaryDefault$sv2[1],RHESSysParamBoundaryDefault$sv2[2])
	param$gw1 = runif(num, RHESSysParamBoundaryDefault$gw1[1],RHESSysParamBoundaryDefault$gw1[2])
	param$gw2 = runif(num, RHESSysParamBoundaryDefault$gw2[1],RHESSysParamBoundaryDefault$gw2[2])
	param$snowEs = runif(num, RHESSysParamBoundaryDefault$snowEs[1],RHESSysParamBoundaryDefault$snowEs[2])
	param$snowTs = runif(num, RHESSysParamBoundaryDefault$snowTs[1],RHESSysParamBoundaryDefault$snowTs[2])
	
		
	RHESSys_arg = paste(
		'-st 2006 1 1 1 -ed 2017 12 1 1',
		'-b -newcaprise -capr 0.001-capMax 0.01',
		'-t tecfiles/tec_daily.txt',
		'-w worldfiles_fc/worldfile',
		'-whdr worldfiles_fc/worldfile.hdr',
		'-r flows/subTestfc.txt flows/surfTestfc.txt')
			   
	RivannaJobs(RHESSys_arg,'output',param, '../parallelRun18101fc.sh') // generate shell files for job sbatch
  
  
  // ----------------------------------------------------------------- 
	# evaluate the first runs  
	argList$projPath		= "~/BAISMAN"
	argList$orbFile			= "usgs01583580.csv"
	argList$startDate		= "2010-10-1"
	argList$endDate			= "2017-9-30"
	argList$RHESSysModel	= "rhessys_baisman10m"
	argList$RHESSysOutput	= "output_parallelRun18101fc"
	argList$runScript		= "parallelRun18101fc.sh"
	
	outputfile = evaluateModel(argList)



