vicmap_get_record <- function(id) {
  
  browser()
  
  if(! curl::has_internet()) stop("No access to internet", call. = FALSE) # nocov
  
  id <- slug_from_url(id)
  
  cli <- vic_catalogue_client("/publicproxy/guest/dv_geoserver/wms/")
  
  r <- cli$get(query = list(id = id, outputFormat = "application/json"))
  
  if (r$status_code == 404){
    stop(paste0("'", id, "' is not a valid record id or name in the B.C. Data Catalogue"), call. = FALSE)
  }
  
  r$raise_for_status()
  
  res <- r$parse()
  stopifnot(res$success)
  
  ret <- res$result
  
  # if (ret$id != id) {
  #   get_record_warn_once(
  #     "It is advised to use the permanent id ('", ret$id, "') ",
  #     "rather than the name of the record ('", id,
  #     "') to guard against future name changes.\n"
  #   )
  # }
  
  as.vicmap_record(ret)
}
