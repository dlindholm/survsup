#' Custom ggplot theme that make Kaplan-Meier curves look nice
#'
#' Custom theme, based upon the theme_bw theme in ggplot2
#' 
#' @param base_size base font size
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#' @examples
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#'     colour = factor(gear))) + facet_wrap(~am)
#' 
#' p + theme_km()

theme_km <- function (base_size = 11, base_family = "", base_line_size = base_size/22, 
    base_rect_size = base_size/22){

    theme_grey(base_size = base_size, base_family = base_family,
        base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
        theme(panel.background = element_rect(fill = "white",
            colour = NA), panel.border = element_rect(fill = NA, colour = "grey70"), 
            strip.background = element_rect(colour = "grey20"), legend.key = element_rect(fill = "white",
                colour = NA), complete = TRUE)
}