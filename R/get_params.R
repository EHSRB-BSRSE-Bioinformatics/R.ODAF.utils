construct_params <- function(context = "analysis") {
  message("Reading config file...")
  config <- yaml::read_yaml(here::here("inputs", "config", "config.yaml"), eval.expr = T)
  message("Config file read successfully.")
  message("Setting up parameters...")
  if (context == "analysis") {
    params <- c(config$common, config$DESeq2)
  } else if (context == "QC") {
    params <- c(config$common, config$QC)
  } else {
    stop("Invalid context: pick one of 'analysis' or 'QC'")
  }
  message("Replacing nulls in params with NA...")
  # replace nulls in params with NA
  params <- replace_nulls_in_config(params)

  message("Setting up project directory...")
  # If projectdir is not set, figure out current project root directory
  projectdir <- params$projectdir
  if (is.na(projectdir)) {
    projectdir <- here::here()
    params$projectdir <- projectdir
  }
  message("Setting up paths...")
  paths <- set_up_paths(params) # NOT DONE IN QC REPORT YET, NEED TO HARMONIZE
  browser()
  message("Setting up species data...")
  species_data <- load_species(params$species, params$wikipathways_filename, params$biospyder_manifest_file)
  params$species_data <- species_data
  message("Setting up platform specific parameters...")
  params <- set_up_platform_params(params)
  check_required_params(params)
  return(params)
}

######################################################################################




# # summary report:
# # Combine required params from config
# params <- c(config$common, config$DESeq2)




# params <- set_up_platform_params(params)









# # RENDER DEseq report

# # Combine required params from config
# params <- c(config$common, config$DESeq2)
# # replace nulls in params with NA
# # If projectdir is not set, figure out current project root directory


# params$species_data <- species_data












# # RENDER QC

# # Combine required params from config
# params <- c(config$common, config$QC)

















# # Run DESeq

# # Combine required params from config
# params <- c(config$common, config$DESeq2)


# paths <- set_up_paths(params)
# get_analysis_id <- get_analysis_id(params)
# species_data <- load_species(params$species, params$wikipathways_filename, params$biospyder_manifest_file)
# params$species_data <- species_data
# # ensembl <- useMart("ensembl",
# #                    dataset = species_data$ensembl_species,
# #                    host = "useast.ensembl.org")
# params <- set_up_platform_params(params)
# # Set this variable to be TRUE if you want to have separate plots of top genes as defined in the R-ODAF template
# params$R_ODAF_plots <- FALSE
# check_required_params(params)
















# # SUMMARIZE ACROSS FACETS

# # Combine required params from config
# params <- c(config$common, config$DESeq2)



# params$species_data <- species_data
# params <- set_up_platform_params(params)