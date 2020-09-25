selectLayer <- function(Client, layer) {
  UseMethod("selectLayer", Client)
}

selectLayer.OWSClient <- function(Client, layer) {
  if(layer %in% listLayers(Client = Client)) {
  Client$selectLayer <- layer
  } else {
    stop(paste("Layer:", layer, "is not available from", deparse(substitute(z))))
  }
}

#### Format Polygon ####
polygonFormat <- function(shape) {
  UseMethod("polygonFormat", shape)
}

polygonFormat.sf <- function(shape) {
  st_union(shape) %>% st_as_text()
}




disjoint <- function(Client, Shape, ...) {
  UseMethod("disjoint", Client)
}

disjoint.OWSClient <- function(Client, ...) {
  
  if(!("selectLayer") %in% names(Client)) {
    stop(paste0("No layer has been selected from ", 
                deparse(substitute(z)), 
                ". Select layer using selectLayer"))
  }
  
  if("queryUrl" %in% names(Client)) {
    
  } else {
    # Name of spatial field
    Client$geomField <- Client$
      getCapabilities()$
      findFeatureTypeByName(layer_name)$
      getDescription(pretty = TRUE) %>%
      dplyr::filter(type == "geometry") %>%
      dplyr::pull(name) 
    
    Client$queryUrl <- httr::parse_url(Client$getUrl())
    Client$queryUrl$query <- list(service = "wfs",
                                  version = "1.0.0",
                                  request = "GetFeature",
                                  typename = Client$selectLayer,
                                  outputFormat = "application/json",
                                  srsName = paste0("EPSG: 4283"),
                                  CQL_FILTER = filter) %>% purrr::discard(is.null)
  }
  
  
}