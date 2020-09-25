selectLayer <- function(Client, layer) {
  UseMethod("selectLayer", Client)
}

selectLayer.OWSClient <- function(Client, layer) {
  if(layer %in% listLayers(Client = Client)$name) {
  Client$selectLayer <- layer
  } else {
    stop(paste("Layer:", layer, "is not available from", deparse(substitute(Client))))
  }
  return(Client)
}

#### Format Polygon ####
polygonFormat <- function(shape) {
  UseMethod("polygonFormat", shape)
}

polygonFormat.sf <- function(shape) {
  st_union(shape) %>% st_as_text()
}
  
filterGeo <- function(Client, method, shape = NULL, add_args = NULL) {

  if(length(add_args) > 0) {
  args <- paste(paste0(", ", add_args), collapse = "")
  } else {
    args <- ""
  }
  
  if(is.null(Client$selectLayer)) {
    stop(paste0("No layer has been selected from ", 
                deparse(substitute(Client)), 
                ". Select layer using selectLayer"))
  }
  
  if(is.null(Client$geomField)) {
    # Name of spatial field
    Client$geomField <- Client$
      getCapabilities()$
      findFeatureTypeByName(Client$selectLayer)$
      getDescription(pretty = TRUE) %>%
      dplyr::filter(type == "geometry") %>%
      dplyr::pull(name) 
  } 
  
  if(method == "bbox") {
    add_filter <- paste0(method, "(", Client$geomField, args, ")")
  } else {
  add_filter <- paste0(method, "(", Client$geomField, ", ", polygonFormat(shape), args, ")")
  }
  
  if(is.null(Client$filter)) {
    Client$filter <- add_filter
  } else{
    Client$filter <- paste(Client$filter, "AND", add_filter)
  }
  return(Client)
}

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


#### Exported functions ####

disjoint <- function(Client, shape, ...) {
  filterGeo(Client, "DISJOINT", shape , ...)
}

intersects <- function(Client, shape, ...) {
  filterGeo(Client, "intersects", shape , ...)
}

equals <- function(Client, shape, ...) {
  filterGeo(Client, "equals", shape , ...)
}

touches <- function(Client, shape, ...) {
  filterGeo(Client, "touches", shape , ...)
}

crosses <- function(Client, shape, ...) {
  filterGeo(Client, "crosses", shape , ...)
}

within <- function(Client, shape, ...) {
  filterGeo(Client, "within", shape , ...)
}

contains <- function(Client, shape, ...) {
  filterGeo(Client, "contains", shape , ...)
}

overlaps <- function(Client, shape, ...) {
  filterGeo(Client, "overlaps", shape , ...)
}

relate <- function(Client, shape, pattern) {
  ... <- pattern
  filterGeo(Client, "overlaps", shape , ...)
}

dwithin <- function(Client, shape, distance, units) { # units is one of feet, meters, statute miles, nautical miles, kilometers
  ... <- c(distance, units)
  filterGeo(Client, "dwithin", shape , ...)
}

beyond <- function(Client, shape, distance, units) { # units is one of feet, meters, statute miles, nautical miles, kilometers
  ... <- c(distance, units)
  filterGeo(Client, "beyond", shape , ...)
}

bbox <- function(Client, xmin, ymin, xmax, ymax) { 
  add_args <- c(xmin, ymin, xmax, ymax)
  filterGeo(Client, "bbox", add_args = add_args)
}

# VicmapClient <- newClient()
# test <- VicmapClient %>% 
#   selectLayer("datavic:VMHYDRO_WATERCOURSE_DRAIN") %>% 
#   #bbox(xmin = 144.25, ymin = -38.44, xmax = 144.50, ymax = -38.25) %>% 
#   intersects(shape = shapey) %>%
#   buildQuery() %>%
#   sf::read_sf(as_tibble = T)

