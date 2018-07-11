#' Add a numbers at risk table to a Kaplan-Meier plot
#'
#' Creates a numbers at risk table based on a ggplot object created by
#' the plot_survfit function. It will add a table in the lower part of 
#' the plot, with numbers where the X axis tick marks are situated.
#' 
#' @param g a ggplot object to which a numbers at risk table should be added.
#' @param size a numeric value indicating the font size of the NAR table
#' @param y_offset a constant indicating how much each line in the NAR table
#'     should be offset (as a multiple of both the plotting space size and the row number)
#' @param flip a logical scalar indicating wheter the order in rows of the NAR table
#'     should be flipped or not.
#' @param separator a logical scalar indicating whether a line that separates the plot from
#'     the NAR table should be put in place
#' @param sep_lwd what width should that line have?
#' @param sep_color which color should we use for that line?
#' @return Returns a ggplot object
#' 
#' @examples
#' retinopathy %>% 
#'     survfit(Surv(futime, status) ~ trt, data = .) %>% 
#'	   plot_survfit() %>% 
#' 	   nar()
#' 
#' # Without table flip:
#' #' flchain %>% 
#'     survfit(Surv(futime, death) ~ sex, data = .) %>% 
#'	   plot_survfit(cuminc = FALSE) %>% 
#'	   nar()
#'
#' # With table flip:
#' flchain %>% 
#'     survfit(Surv(futime, death) ~ sex, data = .) %>% 
#'	   plot_survfit(cuminc = FALSE) %>% 
#'	   nar(flip = TRUE)


nar <- function(g, size = NA, y_offset = 0.05, flip = FALSE, separator = TRUE, sep_lwd = 0.20, sep_color = "grey70"){
	stopifnot("ggplot" %in% class(g))

	# Get X axis ticks
	x_values <- ggplot_build(g)$layout$panel_params[[1]]$x.major_source

	# Get Y range
	y_range <- ggplot_build(g)$layout$panel_params[[1]]$y.range
	y_size <- y_range[2]-y_range[1]
	y_values <- ggplot_build(g)$layout$panel_params[[1]]$y.major_source
	
	# Get data from ggplot object
	d <- g$data

	# Get NAR table for the correct x ticks
	nar <- data.frame(n.risk = double(), strata = character(), x = integer(), stringsAsFactors = FALSE)

	for(i in x_values){
		tmp <- d %>% group_by(strata) %>% 
			mutate(max_time = max(time)) %>%
			filter(time <= i) %>% 
			filter(time == max(time) ) %>% 
			arrange(desc(n.risk)) %>% 
			slice(1) %>% 
			mutate(n.risk = ifelse(i > max_time, 0L, n.risk)) %>%
			ungroup() %>% 
			select(n.risk, strata) %>% 
			mutate(x = i)
		nar <- bind_rows(tmp, nar)
	}	

	# Get Y values for the NAR table
	nar$y <- as.double(NA)

	for(i in 1:length(unique(nar$strata))){
		if(!flip) nar[nar$strata == rev(unique(nar$strata))[i],]$y <- y_range[1] - (i * y_offset * y_size) else
		nar[nar$strata == unique(nar$strata)[i],]$y <- y_range[1] - (i * y_offset * y_size)
	}
	
	# Add NAR table, and make sure that Y ticks are unchanged
	
	suppressMessages(
		g <- g + geom_text(data=nar, mapping = aes(x = x, y = y, label = n.risk), show.legend = FALSE, size = size)+ 
		scale_y_continuous(breaks = y_values, limits = c(min(nar$y), y_range[2]))
	)

	# Add separator if desired
	if(separator){
		g <- g + geom_hline(yintercept = y_range[1], lwd = sep_lwd, color = sep_color)
	}

	return(g)
}