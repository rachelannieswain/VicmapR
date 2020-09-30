buildQuery <- function(Client) {
  
  queryUrl <- httr::parse_url(Client$getUrl())
  queryUrl$query <- list(service = "wfs",
                         version = "1.0.0",
                         request = "GetFeature",
                         typename = Client$selectLayer,
                         outputFormat = "application/json",
                         srsName = paste0("EPSG:4283"),
                         CQL_FILTER = Client$filter) %>% purrr::discard(is.null)
  
  return(httr::build_url(queryUrl))
}
