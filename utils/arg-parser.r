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
	parser <- add_option(parser, c("--maxdate"), default="",
	                     help="Consider only data up to max date 'dd/mm/yy' format.")
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
			new_sub_folder=new_sub_folder ,
			max_date = cmdoptions$options$maxdate
		)
	return(parsedargs)
}

read_country_file <- function (filename){
	countries <- scan(filename, what="", sep="\n")
	for (i in 1:length(countries)){
		countries[i] = trimws(countries[i])
	}
	
	return(countries)
}

trim_data_to_date_range <- function (data, max_date, date_field="DateRep", 
	format_field='%d/%m/%Y', format_max='%d/%m/%y'){
    
	if (max_date == "" || is.null(max_date)){
		return(data)
	} else {

		return (data[
			as.Date(data[[date_field]], format=format_field) 
				<= as.Date(max_date, format=format_max),
			])
	}
}