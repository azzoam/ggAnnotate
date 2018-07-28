#' Half errorbars for ggplot graphs
#'
#' Geometry to draw error bars in ggplot which extend only above
#' and NOT below the data.  Aside from this difference, functions
#' identically to \code{\link[ggplot2]{geom_errorbar()}}.
#'
#' @import ggplot2 grid
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes()}} or
#'   \code{\link[ggplot2]{aes_()}}. If specified and \code{inherit.aes = TRUE} (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply \code{mapping} if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If \code{NULL}, the default, the data is inherited from the plot
#'    data as specified in the call to \code{\link[ggplot2]{ggplot()}}.
#'
#'    A \code{data.frame}, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    \code{\link[ggplot2]{fortify()}} for which variables will be created.
#'
#'    A \code{function} will be called with a single argument,
#'    the plot data. The return value must be a \code{data.frame}, and
#'    will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer()}}. These are often aesthetics,
#'  used to set an aesthetic to a fixed value, like \code{color = "red"} or
#'  \code{size = 3}. They may also be parameters to the paired geom/stat.
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a
#'  warning. If \code{TRUE}, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link[ggplot2]{borders()}}.
#' @section Aesthetics:
#'   \code{geom_halferrorbar} understands the following aesthetics (required
#'   aesthetics are in bold):  
#'  
#'   \itemize{
#'      \item \bold{x}
#'      \item \bold{ymax}
#'      \item alpha
#'      \item colour
#'      \item group
#'      \item linetype
#'      \item size
#'   }
#' @export
geom_halferrorbar <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomHalfErrorbar,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            ...
        )
    )
}


#' Half errorbars for ggplot graphs
#'
#' Geometry to draw error bars in ggplot which extend only above
#' and NOT below the data.  Aside from this difference, functions
#' identically to \code{ggplot2::geom_errorbar()}.
#'
#' @format NULL
#' @usage NULL
#' @export
GeomHalfErrorbar <- ggproto("GeomHalfErrorbar", Geom,
                            
    default_aes = aes(colour = "red", size = 0.5, 
                      linetype = 1, width = 0.5,
                      alpha = NA),
    
    draw_key = draw_key_path,
    
    required_aes = c("x", "y", "ymax"),
    
    setup_data = function(data, params) {

        data$width <- data$width %||% 
            params$width %||% (resolution(data$x, FALSE) * 0.9)

        transform(data,
                  xmin = x - width / 2, xmax = x + width / 2, width = NULL
        )
    },
    
    draw_panel = function(data, panel_params, coord, width = NULL) {
        GeomPath$draw_panel(data.frame(
            x = as.vector(rbind(data$xmin, data$xmax, NA, data$x,    data$x)),
            y = as.vector(rbind(data$ymax, data$ymax, NA, data$ymax, data$y)),
            colour = rep(data$colour, each = 5),
            alpha = rep(data$alpha, each = 5),
            size = rep(data$size, each = 5),
            linetype = rep(data$linetype, each = 5),
            group = rep(1:(nrow(data)), each = 5),
            stringsAsFactors = FALSE,
            row.names = 1:(nrow(data) * 5)
        ), panel_params, coord)
    }
)


# NOTE(alex): The "%||% operator is defined in ggplot2/R/utilities.r
# as the following ---
# 
# "%||%" <- function(a, b) {
#   if (!is.null(a)) a else b
# }
#
# However, it is not exported in NAMESPACE for ggplot2, and thus isn't
# availiable to ggAnnotate.  To maximize portability, I decided to 
# copy/paste the function internally below since it is so short.
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}








