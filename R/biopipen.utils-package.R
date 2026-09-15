#' biopipen.utils: Common Utility Functions for Biopipen
#'
#' Common utility used for biopipen processes to compose bioinformatics
#' pipelines.
#'
#' @keywords internal
#' @importFrom methods formalArgs is
#' @importFrom stats median na.omit sd
#' @importFrom utils head read.csv read.table write.table
"_PACKAGE"

# dplyr's non-standard evaluation column names used from scratch variables
utils::globalVariables(c("cluster", "hitype", "scores"))
