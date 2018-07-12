#' Add a numbers at risk table to a Kaplan-Meier plot
#'
#' Creates a numbers at risk table based on a ggplot object created by
#' the plot_survfit function. It will add a table in the lower part of 
#' the plot, with numbers where the X axis tick marks are situated.
#' 
#' @param g a ggplot object to which a numbers at risk table should be added.
#' @param size a numeric value indicating the font size of the NAR table
#' @param x_offset a constant indicating how much padding should be placed to the
#'     left of the NAR table (a multiple of the plot size in the X dimension)
#' @param y_offset a constant indicating how much each line in the NAR table
#'     should be offset (as a multiple of both the plotting space size and the row number)
#' @param forced_height a constant that, if provided, will force the NAR table
#'     to be of a certain height. This represents a multiple of the plot size in the
#'     Y dimension. This overrides the y_offset setting. Useful when you wish to plot
#'     multiple plots along side each other where the number of strata differs, as this
#'     setting will ensure that the sizes of the NAR tables are constant.
#' @param flip a logical scalar indicating wheter the order in rows of the NAR table
#'     should be flipped or not.
#' @param separator a logical scalar indicating whether a line that separates the plot from
#'     the NAR table should be put in place
#' @param sep_lwd what width should that line have?
#' @param sep_color which color should we use for that line?
#' @return Returns a ggplot object
#' 
#' @export
#' @author Daniel Lindholm 

#' @examples
#' library(survsup); library(ggplot2); library(dplyr); library(survival)
#' 
#' retinopathy %>% 
#'     survfit(Surv(futime, status) ~ trt, data = .) %>% 
#'	   plot_survfit() %>% 
#' 	   nar()
#' 
#' # Without table flip:
#' flchain %>% 
#'     survfit(Surv(futime, death) ~ sex, data = .) %>% 
#'	   plot_survfit(cuminc = FALSE) %>% 
#'	   nar()
#'
#' # With table flip:
#' flchain %>% 
#'     survfit(Surv(futime, death) ~ sex, data = .) %>% 
#'	   plot_survfit(cuminc = FALSE) %>% 
#'	   nar(flip = TRUE)


nar <- function(g, size = NA, x_offset = 0.02, y_offset = 0.05, forced_height = NULL, flip = FALSE, separator = TRUE, sep_lwd = 0.20, sep_color = "grey70"){
	stopifnot("ggplot" %in% class(g))

	# Get X axis range and ticks
	x_range <- ggplot_build(g)$layout$panel_params[[1]]$x.range
	x_size <- x_range[2]-x_range[1]
	x_values <- ggplot_build(g)$layout$panel_params[[1]]$x.major_source

	# Get Y axis range and ticks
	y_range <- ggplot_build(g)$layout$panel_params[[1]]$y.range
	y_size <- y_range[2]-y_range[1]
	y_values <- ggplot_build(g)$layout$panel_params[[1]]$y.major_source
	
	# Get data from ggplot object
	d <- g$data

	if(exists("strata", d)){
		#### Create numbers at risk table in case there are strata

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
	
	} else {
		#### Create numbers at risk table in case there are _no_ strata

		# Get NAR table for the correct x ticks (without strata)
		nar <- data.frame(n.risk = double(), x = integer())

		for(i in x_values){
			tmp <- d %>% 
				mutate(max_time = max(time)) %>%
				filter(time <= i) %>% 
				filter(time == max(time) ) %>% 
				arrange(desc(n.risk)) %>% 
				slice(1) %>% 
				mutate(n.risk = ifelse(i > max_time, 0L, n.risk)) %>% 
				select(n.risk) %>% 
				mutate(x = i)
			nar <- bind_rows(tmp, nar)
		}	

		# Get Y values for the NAR table
		nar$y <- as.double(NA)
		nar$y <- y_range[1] - (y_offset * y_size)

	}	
	
	# Implement the forced_height option
	if(!is.null(forced_height[1])){
		stopifnot(is.numeric(forced_height))
		stopifnot(length(forced_height) == 1)
		stopifnot(forced_height > 0)

		# Get range of number at risk table (y)
		nar_size <- y_size * forced_height

		nar_range <- c(y_range[1]-nar_size, y_range[1])
		if(exists("strata", nar)){
			#Divide NAR table space by number of strata
			nar_step <- nar_size / (length(unique(nar$strata)))
			new_y <- rep(nar_range[1], times = length(unique(nar$strata)))
			new_y <- new_y + 0:(length(new_y)-1) * nar_step
			#Update nar Y values
			old_y <- unique(nar$y)
			old_y <- old_y[order(old_y)]
			for(i in 1:length(old_y)){
				nar[nar$y == old_y[i], ]$y <- new_y[i] 
			}
		} else {
			# Update Y to center NAR row (no strata) within this space
			nar$y <- nar_range[1] + nar_size/2.5

		}


	}
	# Add NAR table, and make sure that Y ticks are unchanged
	
	if(!is.null(forced_height[1])){
		suppressMessages(
			g <- g + geom_text(data=nar, mapping = aes(x = x, y = y, label = n.risk), show.legend = FALSE, size = size)+ 
			scale_y_continuous(breaks = y_values, limits = c(nar_range[1], y_range[2]))+
			scale_x_continuous(breaks = x_values, limits = c(x_range[1] - x_offset * x_size, x_range[2]))
		)
	} else {
		suppressMessages(
			g <- g + geom_text(data=nar, mapping = aes(x = x, y = y, label = n.risk), show.legend = FALSE, size = size)+ 
			scale_y_continuous(breaks = y_values, limits = c(min(nar$y), y_range[2]))+
			scale_x_continuous(breaks = x_values, limits = c(x_range[1] - x_offset * x_size, x_range[2]))
		)

	}
	# Add separator if desired
	if(separator){
		g <- g + geom_hline(yintercept = y_range[1], lwd = sep_lwd, color = sep_color)
	}

	return(g)
}