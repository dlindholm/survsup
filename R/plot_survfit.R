#' Plot a survfit object
#'
#' Creates a Kaplan-Meier plot based on a survfit object
#' 
#' @param fit A survfit object that we should plot
#' @param lwd Line width
#' @param xmax What is the upper limit of the X axis? The default will plot all data.
#' @param xbreaks A numeric vector. Where should the breaks in the X axis be? The default will give ggplot's 
#'     default breaks.
#' @param ylim A numeric vector of length 2. Which are the Y axis limits?
#' @param ci A logical scalar. Should confidence intervals be plotted?
#' @param cuminc A logical scalar. Should cumulative incidence be plotted (default), or should be survival
#'     be plotted instead (cuminc = FALSE)?
#' @param y_percent A logical scalar. Should the Y axis display percentages?
#' @param split_legend_labels A logical scalar. Should the legends labels be split, i.e. removing the "<variable_name>="
#'     for each stratum?
#' @param legend.title What title should the legend have? By default it will take the value that was split if split_legend_labels
#'     was used; otherwise it will take the value "strata"
#' @param legend.position Where should the legend be drawn? Possible answers are: "top", "bottom",
#'     "left", "right"; or if you want to skip putting a legend on the plot: "none"
#' 
#' @return Returns a ggplot object
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_step guides scale_x_continuous ylim theme
#' @importFrom dplyr bind_rows %>% mutate 
#' @export

#'  
#' @author Daniel Lindholm 
#' @examples
#' 
#' library(survsup); library(ggplot2); library(dplyr); library(survival)
#' 
#' retinopathy %>% 
#'     survfit(Surv(futime, status) ~ trt, data = .) %>% 
#'	   plot_survfit()
#' 
#' retinopathy %>% 
#'     survfit(Surv(futime, status) ~ trt, data = .) %>% 
#'	   plot_survfit(ci = TRUE, cuminc = FALSE)
#' 
#' # Since a ggplot object is returned,
#' # you can tweak as you wish
#' # (in this case with multiple labels): 
#' mgus2 %>% 
#'      survfit(Surv(futime, death) ~ sex, data = .) %>% 
#'      plot_survfit(cuminc = FALSE) %>%
#'      nar(flip = TRUE) %>% 
#'      hcl_rainbow()+  # <--- Notice + sign here!
#'      labs(title = "This is an excessively long title",
#'      	subtitle = "This is a subtitle",
#'      	x = "Time (days)",
#'      	y = "Survival (%)",
#'      	caption = "(This is a demo)")
 


plot_survfit <- function(fit, lwd = 1, xmax = NULL, xbreaks = NULL, ylim = NULL, ci = FALSE, cuminc = TRUE, y_percent = TRUE,
	split_legend_labels = TRUE, legend.title = NA, legend.position="top"){
	
	#Checks
	stopifnot("survfit" %in% class(fit))
	stopifnot(is.logical(cuminc))
	stopifnot(is.logical(ci))
	stopifnot(is.logical(y_percent))
	stopifnot(is.logical(split_legend_labels))
	stopifnot(legend.position %in% c("top", "bottom", "left", "right", "none"))
	
	#Tidy fit to clean data.frame
	tidy_fit <- broom::tidy(fit)

	#Make sure to start from 1
	if(exists("strata", fit)){
		
		baseline <- data.frame(time = 0,
							   n.risk = fit$n,
							   n.event = 0,
							   n.censor = 0,
							   estimate = 1,
							   std.error = NA,
							   conf.high = 1,
							   conf.low = 1,
							   strata = names(fit$strata),
							   stringsAsFactors = FALSE)
		tidy_fit <- bind_rows(tidy_fit, baseline)
	
	} else {
	
		baseline <- data.frame(time = 0,
							   n.risk = fit$n,
							   n.event = 0,
							   n.censor = 0,
							   estimate = 1,
							   std.error = NA,
							   conf.high = 1,
							   conf.low = 1,
							   stringsAsFactors = FALSE)
		tidy_fit <- bind_rows(tidy_fit, baseline)
	
	}

	#Transform if desired
	if(cuminc){
	
		tidy_fit <- tidy_fit %>% 
			mutate(estimate = 1 - estimate, conf.high = 1 - conf.high, conf.low = 1 - conf.low)
	
	}

	#Multiply y with 100 if desired
	if(y_percent){

		tidy_fit <- tidy_fit  %>% 
			mutate(estimate = 100 * estimate, 
				conf.high = 100 * conf.high,
				conf.low = 100 * conf.low)
	
	}

	# Split labels if desired
	if(split_legend_labels & exists("strata", fit) ){
		tmp_strata <- regmatches(tidy_fit$strata, regexpr("=", tidy_fit$strata), invert = TRUE)
		tmp_label <- sapply(tmp_strata, "[[", 1)
		stopifnot(length(unique(tmp_label)) == 1) #Check that the left hand side is identical across all rows
		tidy_fit$strata <- sapply(tmp_strata, "[[", 2)
		strata_label <- tmp_label[1]

	}


	#Create plot
	if(exists("strata", fit)){
		if(!is.na(legend.title)) strata_label <- legend.title

		if(ci){
			
			res <- ggplot(tidy_fit, aes(x = time, y = estimate, color = strata))+
				geom_ribbon(show.legend = FALSE, fill = "grey60", alpha = 0.2, lty = 0, mapping = aes(ymin = conf.low, ymax = conf.high))+
				geom_step(lwd=lwd)
		
		} else {
		
			res <- ggplot(tidy_fit, aes(x = time, y = estimate, color = strata))+
				geom_step(lwd=lwd)
		
		}

			if(exists("strata_label")) res <- res + guides(color = guide_legend(title = strata_label) )


	} else {

		if(ci){
		
			res <- ggplot(tidy_fit, aes(x = time, y = estimate))+
				geom_ribbon(show.legend = FALSE, fill = "grey60", alpha = 0.2, lty = 0, mapping = aes(ymin = conf.low, ymax = conf.high))+
				geom_step(lwd=lwd)
		
		} else {
		
			res <- ggplot(tidy_fit, aes(x = time, y = estimate)) + geom_step(lwd=lwd)
		
		}	
	}

	# Create custom x limit and x breaks if specified
	if(!is.null(xbreaks[1])){
		if(!is.null(xmax[1])){

			res <- res + scale_x_continuous(breaks = xbreaks, limits = c(0, xmax))
		
		} else res <- res + scale_x_continuous(breaks = xbreaks)
	} 

	else {
		
		if(!is.null(xmax[1])){
			
			res <- res + scale_x_continuous(limits = c(0, xmax))
		
		}
	}

	# Create custom y limits if desired
	if(!is.null(ylim[1])){
		res <- res + ylim(ylim)
	}	


	# Put legend where desired
	res <- res + theme_km() + theme(legend.position = legend.position)

	#Return results
	return(res)

}