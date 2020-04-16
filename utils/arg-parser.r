library(optparse)


base_arg_parse <- function (){
	# Commandline options and parsing
	parser <- OptionParser()
	parser <- add_option(parser, c("-D", "--debug"), action="store_true",
	                     help="Perform a debug run of the model")
	parser <- add_option(parser, c("-F", "--full"), action="store_true",
	                     help="Perform a full run of the model")
	parser <- add_option(parser, c("--nosubdir"), action="store_true",
	                     help="Do not create subdirectories for generated data.")
	cmdoptions <- parse_args(parser, args = commandArgs(trailingOnly = TRUE), positional_arguments = TRUE)

	# Default run parameters for the model
	if(is.null(cmdoptions$options$debug)) {
	  DEBUG = Sys.getenv("DEBUG") == "TRUE"
	} else {
	  DEBUG = cmdoptions$options$debug
	}

	if(is.null(cmdoptions$options$full)) {
	  FULL = Sys.getenv("FULL") == "TRUE"
	} else {
	  FULL = cmdoptions$options$full
	}

		
	new_sub_folder = "TRUE"
	if(!is.null(cmdoptions$options$nosubdir)){
		new_sub_folder = !cmdoptions$options$nosubdir
	}

	if(DEBUG && FULL) {
	  stop("Setting both debug and full run modes at once is invalid")
	}

	if(length(cmdoptions$args) == 0) {
	  StanModel = 'base'
	} else {
	  StanModel = cmdoptions$args[1]
	}
	 
	print(sprintf("Running %s",StanModel))
	if(DEBUG) {
	  print("Running in DEBUG mode")
	} else if (FULL) {
	  print("Running in FULL mode")
	}

	parsedargs <- c(
			DEBUG=DEBUG,
			FULL=FULL,
			StanModel=StanModel,
			new_sub_folder=new_sub_folder 
		)
	return(parsedargs)
}

