#' Helper function to densify points on the boundary of a polygon
#' @description This function is used to densify the vertices on a linestring. This is used for centerline creation
#' @param geometry Input sf or sfc object whose outline vertices are densified 
#' @param distance Interval at which the new vertices are set
#' @author Florian Betz
#' @return a sfc object with the densified vertices
#' 
densify_sf <- function(geometry, distance) {
  # Ensure geometry is in the correct class
  if (!inherits(geometry, "sf") && !inherits(geometry, "sfc")) {
    stop("Input must be an sf or sfc object.")
  }
  
  # Function to add points to a single line
  add_points_to_line <- function(line, dist) {
    coords <- st_coordinates(line)
    new_coords <- coords[1, , drop = FALSE]
    for (i in 2:nrow(coords)) {
      segment_start = coords[i - 1, ]
      segment_end = coords[i, ]
      segment_length = sqrt(sum((segment_end - segment_start)^2))
      n_points = ceiling(segment_length / dist) - 1
      for (j in 1:n_points) {
        new_point = segment_start + (segment_end - segment_start) * j / (n_points + 1)
        new_coords = rbind(new_coords, new_point)
      }
      new_coords = rbind(new_coords, coords[i, , drop = FALSE])
    }
    return(st_linestring(new_coords))
  }
  
  # Apply the function to each geometry
  densified = lapply(st_geometry(geometry), function(g) {
    if (st_is(g, "LINESTRING")) {
      add_points_to_line(g, distance)
    } else if (st_is(g, "POLYGON")) {
      new_lines = lapply(st_cast(g, "LINESTRING"), add_points_to_line, dist = distance)
      st_polygon(list(do.call(rbind, lapply(new_lines, st_coordinates))))
    } else {
      stop("Geometry type not supported.")
    }
  })
  
  # Return densified geometry
  st_sfc(densified, crs = st_crs(geometry))
}