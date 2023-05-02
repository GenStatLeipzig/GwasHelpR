#' @title Read SNPTEST Sample File as Data Table
#' @description This function reads a SNPTEST sample file and converts it into a data.table in R.
#' @param snptest_sample_fn A string representing the path to the SNPTEST sample file.
#' @param makeBinaryOutcomeNumeric12 A boolean to specify whether to recode binary outcomes as numeric 1 and 2. Default: T
#' @return A data.table object containing the SNPTEST sample file data with appropriate data types.
#' @details This function reads a SNPTEST sample file and converts it into a data.table. It also
#' converts certain columns to their respective data types and, if specified, recodes binary outcomes
#' as numeric 1 and 2.
#' @examples
#' \dontrun{
#' if(interactive()){
#'   # Read SNPTEST sample file and create a data.table object
#'   snptest_data <- readSNPTESTsamplefileASdata.table("path/to/snptest_sample_file")
#' }
#' }
#' @rdname readSNPTESTsamplefileASdata.table
#' @export

readSNPTESTsamplefileASdata.table <- function (snptest_sample_fn, makeBinaryOutcomeNumeric12 = T) {

  samplefile = data.table::fread(snptest_sample_fn)

  samplefile_types = unlist(samplefile[1])
  samplefile_types
  samplefile = samplefile[-1]

  for(i in names(samplefile_types[samplefile_types %in% c("C","P")])) {
    samplefile[,(i) := as.numeric(get(i))]
  }

  for(i in names(samplefile_types[samplefile_types %in% c("D","B")])) {
    samplefile[,(i) := factor(get(i))]
  }


  ## binary in 1 2 recodieren
  if(makeBinaryOutcomeNumeric12 == T) {for(i in names(samplefile_types[samplefile_types %in% c("B")])) {

    samplefile[,(i) := as.numeric(factor(get(i)))]
    samplefile[,if(any(stats::na.omit(get(i)) %nin% c(1,2))) stop(paste0("Case/control phenotype " ,i, " is not encoded in exactly two levels...")) ]

  }}

  # str(samplefile)
  samplefile
}
