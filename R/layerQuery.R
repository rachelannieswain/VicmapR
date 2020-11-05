VicmapR_layer <- function(layer_id, crs = 4283) {
  if (length(layer_id) != 1) {
    stop("Only one record my be queried at a time.", call. = FALSE)
  }
  
  # Fist catch if a user has passed the name of a warehouse object directly,
  # then can skip all the record parsing and make the API call directly
 # if (is_whse_object_name(record)) {
    ## Parameters for the API call
    query_list <- make_query_list(layer_name = layer_id, crs = crs)
    
    ## Drop any NULLS from the list
    query_list <- purrr::compact(query_list)
    
    ## GET and parse data to sf object
    cli <- vic_wfs_client()
    
    cols_df <- feature_helper(layer_id)
    
    return(
      as.vicmap_promise(list(query_list = query_list, cli = cli, record = NULL,
                           cols_df = cols_df))
    )
 # }
  
  obj <- bcdc_get_record(layer_id)

  bcdc_query_geodata(obj, crs)
}
