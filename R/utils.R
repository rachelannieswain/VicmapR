make_query_list <- function(layer_name, crs = 4283) {
  list(
    SERVICE = "WFS",
    VERSION = "2.0.0",
    REQUEST = "GetFeature",
    outputFormat = "application/json",
    typeNames = layer_name,
    SRSNAME = paste0("EPSG:", crs)
  )
}

catch_wfs_error <- function(catalogue_response) {
  msg <- "There was an issue sending this WFS request\n"
  
  if (inherits(catalogue_response, "Paginator")) {
    statuses <- catalogue_response$status_code()
    status_failed <- any(statuses >= 300)
    if (!status_failed) return(invisible(NULL))
    
    msg <- paste0(msg, paste0(length(statuses), " paginated requests issued"))
  } else {
    status_failed <- catalogue_response$status_code >= 300
    if (!status_failed) return(invisible(NULL))
    
    request_res <- catalogue_response$request_headers
    response_res <- catalogue_response$response_headers
    
    msg <- paste0(
      msg,
      cli::rule(line = "bar4", line_col = 'red'),"\n",
      "Request:",
      "\n  URL: ", catalogue_response$request$url$url,
      "\n  POST fields:\n    ", rawToChar(catalogue_response$request$options$postfields),
      "\n"
    )
    
    for (i in seq_along(request_res)) {
      msg <- paste0(
        msg, "  ", names(request_res)[i], ": ",
        request_res[i], "\n"
      )
    }
    msg <- paste0(msg, "Response:\n")
    for (i in seq_along(response_res)) {
      msg <- paste0(
        msg, "  ", names(response_res)[i], ": ",
        response_res[i], "\n"
      )
    }
  }
  
  stop(msg, call. = FALSE)
}

gml_types <- function(x) {
  c(
    "gml:PointPropertyType",
    "gml:CurvePropertyType",
    "gml:SurfacePropertyType",
    "gml:GeometryPropertyType",
    "gml:MultiPointPropertyType",
    "gml:MultiCurvePropertyType",
    "gml:MultiSurfacePropertyType",
    "gml:MultiGeometryPropertyType"
  )
}

wfs_to_r_col_type <- function(col){
  
  dplyr::case_when(
    col == "xsd:string" ~ "character",
    col == "xsd:date" ~ "date",
    col == "xsd:decimal" ~ "numeric",
    col == "xsd:hexBinary" ~ "numeric",
    grepl("^gml:", col) ~ "sfc geometry",
    TRUE ~ as.character(col)
  )
}

parse_raw_feature_tbl <- function(query_list){
  
  ## GET and parse data to sf object
  cli <- vic_wfs_client()
  
  cc <- cli$post(body = query_list, encode = "form")
  
  catch_wfs_error(cc)
  
  xml_res <- xml2::read_xml(cc$parse("UTF-8"))
  xml_res <- xml2::xml_find_all(xml_res, "//xsd:sequence")
  xml_res <- xml2::xml_find_all(xml_res, ".//xsd:element")
  xml_res <- purrr::map(xml_res, xml2::xml_attrs)
  xml_df <- purrr::map_df(xml_res, ~ as.list(.))
  
  
  attr(xml_df, "geom_type") <- intersect(xml_df$type, gml_types())
  
  return(xml_df)
}

feature_helper <- function(whse_name){
  
  query_list <- list(
    SERVICE = "WFS",
    VERSION = "2.0.0",
    REQUEST = "DescribeFeatureType",
    typeNames = whse_name)
  
  ## This is an ugly way of doing this
  ## Manually add id and turn into a row
  id_row <- dplyr::tibble(name = "id",
                          nillable = FALSE,
                          type = "xsd:string")
  
  xml_df <- parse_raw_feature_tbl(query_list)
  geom_type <- attr(xml_df, "geom_type")
  
  ## Identify geometry column and move to last
  # xml_df[xml_df$type == geom_type, "name"] <- "geometry"
  # xml_df <- dplyr::bind_rows(xml_df[xml_df$name != "geometry",],
  #                            xml_df[xml_df$name == "geometry",])
  
  ## Fix logicals
  xml_df$nillable = ifelse(xml_df$nillable == "true", TRUE, FALSE)
  
  xml_df <- xml_df[, c("name", "nillable", "type")]
  ## Add the id_row back into the front
  xml_df <- dplyr::bind_rows(id_row, xml_df)
  colnames(xml_df) <- c("col_name", "sticky", "remote_col_type")
  xml_df$local_col_type <- wfs_to_r_col_type(xml_df$remote_col_type)
  
  xml_df
}