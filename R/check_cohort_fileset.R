#' @title Check cohort file set
#' @description This function checks that a cohort has all the files required to be used with the gwascompanion. 
#' @param cohort_label A string representing the cohort to be checked.
#' @param cohort_definition A string with the path to the cohort definition file in xlsx format. The cohort definition is a file with specific information for all our cohorts, that are suitable for the gwascompanion.
#' @return A data.table object containing the results of the check routine.
#' @details This function checks that for a given cohort all data files are available in a predefined data format. This is needed for every cohort that's supposed to be input for the gwascompanion. The checks include the availability of a set of files and that some files have specific columns.  
#' @examples
#' \dontrun{ 
#' if(interactive()){
#'   # Check the cohort for its suitability for the gwascompanion
#'   result <- check_cohort("myCohortData", "path/to/cohort/definition.xlsx")
#' }
#' }
#' @rdname check_cohort_fileset
#' @export
#' @import data.table
#' @import readxl

check_cohort <- function (cohort_label, cohort_definition) {
  # empty vectors to save checks and results
  check = NULL
  check_result = NULL
  
  # load specified cohort_definition
  cohort_def = as.data.table(read_xlsx(path = cohort_definition, sheet = 1))
  
  # start checks
  # 0. check only one cohort at a time
  check0 = (length(cohort_label) == 1)
  if (!check0==TRUE) message("Please check only one cohort at a time!\n")
  
  # 1. check that cohort is in cohort definition
  check1 = is.element(cohort_label, cohort_def[, gwascompanion_label])
  if (!check1==TRUE) message("Given cohort is not listed in cohort definition file!\n")
  check = c(check, "Cohort is listed in cohort definition")
  check_result = c(check_result, check1)
  
  # 2. check snpinfo file (available and columns)
  snpinfo_fn = cohort_def[cohort_label == gwascompanion_label, feature_annotation]
  check2 = file.exists(snpinfo_fn)
  check = c(check, "SNP-Info file is available")
  check_result = c(check_result, check2)
  if (!check2==TRUE) message("SNPinfo file is not available!\n")
  stopifnot(check2 == TRUE)
  
  columns_needed = c("id", "rs", "chr", "position", "strand", "REF0", "ALT1", "type", "info", "freq_ALT_imputed", "maf_imputed", "count_ALT_imputed")
  snpinfo = fread(snpinfo_fn, nrows = 1000)
  dummy = is.element(columns_needed, colnames(snpinfo))
  check3 = sum(dummy==TRUE) == length(columns_needed)
  check = c(check, "SNP-Info file has all columns needed")
  check_result = c(check_result, check3)
  
  # 3. check sample file 
  sample_fn = cohort_def[cohort_label == gwascompanion_label, sample_annotation]
  check4 = file.exists(sample_fn)
  check = c(check, "Sample file is available")
  check_result = c(check_result, check4)
  if (!check4==TRUE) message("Sample file is not available!\n")
  stopifnot(check4 == TRUE)
  
  columns_needed = c("sampleID", "sampleID_2", "sex", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")
  sample_data = fread(sample_fn)
  dummy = is.element(columns_needed, colnames(sample_data))
  check5 = sum(dummy==TRUE) == length(columns_needed)
  check = c(check, "Sample file has all columns needed")
  check_result = c(check_result, check5)
  
  # 4. check pgen file for genetic data
  gen_data_fn = cohort_def[cohort_label == gwascompanion_label, data_file]
  check6 = file.exists(gen_data_fn)
  check = c(check, "PGEN file is available")
  check_result = c(check_result, check6)
  if (!check6==TRUE) message("PGEN file is not available!\n")
  stopifnot(check6 == TRUE)
  
  psam_fn = cohort_def[cohort_label == gwascompanion_label, data_file]
  psam_fn = gsub(pattern = "[.]pgen", replacement = ".psam", psam_fn)
  dummy = file.exists(psam_fn)
  psam_data = fread(psam_fn)
  # rename IID column
  my_id_col = colnames(psam_data)[grep("IID", colnames(psam_data))]
  setnames(psam_data, my_id_col, "IID")
  
  dummy = is.element(psam_data[, IID], sample_data[, sampleID])
  check7 = sum(dummy==TRUE) == nrow(psam_data)
  check = c(check, "Sample identifiers of sample file match genetic data")
  check_result = c(check_result, check7)
  
  # report results object
  if(all(check_result)) message(paste0("The cohort ", cohort_label, " fulfills all criteria to be used with the gwascompanion."))
  result = data.table(check, result = check_result)
  return(result)
}
