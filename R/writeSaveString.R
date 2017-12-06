#
# writeSaveString <- function(params,type){
#
#   out <- glue::glue("{params$model}_\\
#                     {params$nTRs}TRs_\\
#                     {params$nChannels}channels_\\
#                     {params$nOrientationsTested}OrientationsTested_\\
#                     {params$kappa[1]}_{params$kappa[2]}kappa_\\
#                     {params$a[1]}_{params$a[2]}a_\\
#                     {params$m[1]}_{params$m[2]}m_\\
#                     .{type}")
#   return(out)
# }

writeSaveString <- function(params,type){

  library(magrittr)
  # filename <- writeSaveString(params, type="pdf")


  out <- paste0(params,names(params),collapse = "") %>%
    stringr::str_replace_all(., "[:punct:]", "") %>%
    stringr::str_replace_all(., "[:blank:]", "") %>%
    paste(type,sep=".")
  return(out)
}
