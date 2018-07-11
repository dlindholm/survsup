#' Convenience function for skislope color scheme
#'
#' Convenience function to change the color scheme to 
#' four colors: Green, blue, red, and black; i.e. 
#' as ski slopes are classified.
#' 
#' @param g ggplot object containing a Kaplan-Meier plot
#' @param reverse should the order of the colors be reversed?
#' 
#' @return Returns a ggplot object
#' @export
#' @author Daniel Lindholm  
#' @examples
#' veteran %>% 
#'     survfit(Surv(time, status) ~ celltype, data = .) %>% 
#'	   plot_survfit(cuminc = FALSE) %>% 
#'	   skislopes(reverse = TRUE)
skislopes <- function(g, reverse = FALSE){
	stopifnot("ggplot" %in% class(g))
	cols <- c("#008744", "#0057e7", "#d62d20", "black")
	if(reverse) cols <- rev(cols)
	g + scale_color_manual(values = cols)	
}

#' Convenience function for four-category color scheme
#'
#' Convenience function to change the color scheme to 
#' four colors: Green, blue, red, and yellow; i.e. 
#' like the ski slopes but with yellow instead of black.
#' 
#' @param g ggplot object containing a Kaplan-Meier plot
#' @param reverse should the order of the colors be reversed?
#' @return Returns a ggplot object
#' @export
#' @author Daniel Lindholm  
#' @examples
#' veteran %>% 
#'     survfit(Surv(time, status) ~ celltype, data = .) %>% 
#'	   plot_survfit(cuminc = FALSE) %>% 
#'	   cat4(reverse = TRUE)
cat4 <- function(g, reverse = FALSE){
	stopifnot("ggplot" %in% class(g))
	cols <- c("#008744", "#0057e7", "#d62d20", "#ffa700")
	if(reverse) cols <- rev(cols)
	g + scale_color_manual(values = cols)	
}

#' Convenience function for the rainbow_hcl color scheme
#'
#' Convenience function to change the color scheme to 
#' the rainbow_hcl scheme provided by the colorspace package.
#' The function will detect how many strata are available and 
#' will pick that many colors from rainbow_hcl.
#' 
#' @param g ggplot object containing a Kaplan-Meier plot
#' @param reverse should the order of the colors be reversed?
#' @param c chroma
#' @param l luminance
#' @param start the hue at which the rainbow begins
#' @param end the hue at which the rainbow ends
#' 
#' @return Returns a ggplot object
#' @export
#' @author Daniel Lindholm  
#' @examples
#' veteran %>% 
#'     survfit(Surv(time, status) ~ celltype, data = .) %>% 
#'	   plot_survfit(cuminc = FALSE) %>% 
#'	   hcl_rainbow()
hcl_rainbow <- function(g, reverse = FALSE, c = 50, l = 70, start = 0, end = 360*(n-1)/n){
	stopifnot("ggplot" %in% class(g))
	d <- g$data
	stopifnot(exists("strata", d))
	n = length(unique(d$strata))
	cols <- colorspace::rainbow_hcl(n, c = c, l = l, start = start, end = end)
	if(reverse) cols <- rev(cols)
	g + scale_color_manual(values = cols)	
}
