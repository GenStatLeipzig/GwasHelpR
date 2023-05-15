#' Write SNPTEST Sample File
#'
#' @title Write SNPTEST Sample File
#' @description This function writes a SNPTEST sample file with the specified data and variable types.
#' @param filename A string representing the path where the SNPTEST sample file will be saved.
#' @param samplefile A data.table object containing the SNPTEST sample file data. The first three columns
#' should be Family-ID, Individual-ID (the latter should only include unique names!), and missing, which can
#' always be 0, which is typical for imputed data. The column names of the first two columns should be either
#' "ID1","ID2" or "ID_1","ID_2".
#' @param vartypes A character vector specifying the variable types for each column in the sample file. Can be
#' either "0", "C", "D", "B", or "P". The first three should always be "0","0","0" corresponding to the first
#' column of the sample file c("ID1","ID2","missing"). 'C' and 'D' are continuous or dichotomous covariates,
#' 'B' and 'P' are binary and continuous outcomes, respectively.
#' @return None. This function writes the SNPTEST sample file to the specified path.
#' @details This function writes a SNPTEST sample file with the specified data and variable types,
#' ensuring that the file is formatted correctly for SNPTEST and other genetic programs to read.
#' @examples
#' \dontrun{
#' if(interactive()){
#'   # Write the SNPTEST sample file
#'   writeSnptestSamplefile("path/to/output_file", samplefile, vartypes)
#' }
#' }
#' @rdname writeSnptestSamplefile
#' @export


writeSnptestSamplefile <- function (filename, samplefile, vartypes) {
  stopifnot(dim(samplefile)[2] == length(vartypes))
  stopifnot(identical(as.character(vartypes[1:3]),as.character(c(0,0,0))))
  stopifnot(all(vartypes %in% c("0", "C", "D", "B", "P")))
  stopifnot(identical(names(samplefile)[1:3],c("ID1","ID2","missing")) | identical(names(samplefile)[1:3],c("ID_1","ID_2","missing") ))
  f <- file(filename, open="wb")
  write.table(t(names(samplefile)),file=f,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
  write.table(t(as.character(vartypes)),file=f,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
  write.table(samplefile,file=f,col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")
  close(f)
}
