#' VPU Boundaries
#' Vector Processing Unit boundaries
#' @docType data
#' @format An object of class \code{"sf"}
#' @keywords data
"vpu_boundaries"

#' RPU Boundaries
#' Raster Processing Unit boundaries
#' @docType data
#' @format An object of class \code{"sf"}
#' @keywords data
"rpu_boundaries"



#' Return RPU or VPU boundaries
#' @param type character. Either "RPU" or "VPU"
#' @return An object of class \code{"sf"}
#' @export

get_boundaries = function(type = "vpu"){
  if(tolower(type) == "vpu"){
    nhdplusTools::vpu_boundaries
  } else if(tolower(type) == "rpu"){
    nhdplusTools::rpu_boundaries
  } else {
    stop("Type must be either: rpu or vpu", call. = FALSE)
  }
}
