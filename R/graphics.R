
#' Plot temperature data as scatter plot with trendline
#'
#' @export
#' @param x tibble of hobotemp data
#' @param main character, title
#' @param alpha numeric, 0.1 default
#' @param xlabel character, title of xaxis
#' @param ylabel character, title of yaxis
#' @param facet character, name of the column to facet upon (like "Site") or NULL to skip
#' @param ... further arguments passed to \code{\link[ggplot2]{theme}}
#' @return ggplot2 object
draw_temp_scatter_plot <- function(x = read_hobotemp(),
                      main = "Temperature at Depth",
                      xlabel = "Date",
                      ylabel = "Temperature (degrees C)",
                      alpha = 0.2,
                      facet = NULL,
                      ...){

  gg <- ggplot2::ggplot(data = x, ggplot2::aes(x = .data$DateTime, y = .data$Temp)) +
    #ggplot2::geom_point(na.rm = TRUE, alpha = alpha, shape = 4, ggplot2::aes(color = Sensor)) +
    ggplot2::geom_point(na.rm = TRUE, alpha = alpha, shape = 4) +
    ggplot2::labs(title = main, x = xlabel, y = ylabel) +
    ggplot2::geom_smooth(na.rm = TRUE, se = FALSE)

  if (!is.null(facet)){
    gg <- gg + ggplot2::facet_wrap(facet)
  }

  gg
}


#' retrieve example type hobotemp csv with multi sites
#'
#' @export
#' @return hobotemp csv
example_ridge_data <- function(){
  x <- readr::read_csv(system.file("exampledata/multisiteHOBO.csv",
                                   package="sensible"))
}

#' Plot temperature data as ridgeline plot
#'
#' @export
#' @param x tibble of hobotemp data with DateTime, Temp, and Site columns
#' @param main character, title
#' @param xlabel character, title of xaxis
#' @param ylabel character, title of yaxis
#' @param ordered vector, vector of site names in order to be displayed bottom to top, must match site names in csv
#' @param ... further arguments passed to \code{\link[ggplot2]{theme}}
#' @return ggplot2 object
draw_ridgeline_plot <- function(x = example_ridge_data(),
                                main = "Temperature at Depth",
                                xlabel = "Date",
                                ylabel = "Site",
                                ordered = NULL,
                                ...) {


  #select only date, temp, site data
  x <- x %>% dplyr::select(.data$DateTime, .data$Temp, .data$Site)

  #create vector with unique site names
  n <- unique(x$Site)

  #create vector with siteNum values (5, then 15 increments after the first)
  ns <- numeric(length = length(n))
  for (xx in 1:length(ns)) {
    ns[xx] = 5 + 15*(xx-1)
  }

  #create new column called SiteNum, default with NA
  x[,"SiteNum"] <- NA

  #loop through the sites, look in df to see if site matches, if so, give it proper sitenum
  for (ii in 1:length(n)) {

    if (!is.null(ordered)) #orders based on order handed to function
      {x$SiteNum <- ifelse(x$Site == ordered[ii], ns[ii], x$SiteNum)}
    else #not ordered
      {x$SiteNum <- ifelse(x$Site == n[ii], ns[ii], x$SiteNum)}
  }

  #create vector with length 10 more than the highest site number
  ylabs <- vector(mode = "character", length = ns[length(ns)] + 10)

  #add names to label vector
  for (yy in 1:length(n)) {

    nl <- ns[yy] + 5 #calculate ylabel position number
    if (!is.null(ordered)) #ordered
    {ylabs[nl] <- ordered[yy]}
    else #not ordered
    {ylabs[nl] <- n[yy]} #assign position name of corresponding site

  }


  #plot

  gg <-  #TempData=dataframe, x=xaxis, y=yaxis, height=line pattern, group=spacing of yaxis
    ggplot2::ggplot(x, ggplot2::aes(x = .data$DateTime, y = .data$SiteNum, height = .data$Temp, group = .data$SiteNum)) +
    #fills underneath the line with temperature values
    ggridges::geom_ridgeline_gradient(ggplot2::aes(fill = .data$Temp)) +
    #color to be used to fill, option is color gradient from viridis package, name is title of legend
    viridis::scale_fill_viridis(option = "A", name = "Temp, C") +
    #black and white theme, removes grey from background of plot
    ggplot2::theme_bw() +
    #sets visual details
    ggplot2::theme(panel.border = ggplot2::element_blank(), #remove border of plot
          panel.grid.major = ggplot2::element_blank(), #remove major gridlines
          panel.grid.minor = ggplot2::element_blank(), #remove minor gridlines
          axis.line = ggplot2::element_line(colour = "black"), #set color of axis line
          axis.ticks = ggplot2::element_blank(), #remove axis ticks, as there would be many on y axis
          plot.title = ggplot2::element_text(hjust = 0.5, size = 16), #center text, set size of title
          axis.title.y = ggplot2::element_text(size = 14), #set font size of yaxis title
          axis.title.x = ggplot2::element_text(size = 14), #set font size of xaxis title
          axis.text = ggplot2::element_text( color = "black", size = 10), #set color and size of labels along axis
          legend.text = ggplot2::element_text(size = 10, colour ="black"), #set color and size of text in legend
          legend.title = ggplot2::element_text(size = 12, colour = "black")) + #set color and size of legent title
    #set title of plot
    ggplot2::labs(title = main) +
    #set title of x axis
    ggplot2::xlab(xlabel) +
    #set title of y axis
    ggplot2::ylab(ylabel) +
    #add outline to colorbar in legend
    ggplot2::guides(fill = ggplot2::guide_colourbar(frame.colour = "black")) +
    #add y axis labels
    ggplot2::scale_y_continuous(breaks = seq(1,length(ylabs)), labels = ylabs)

  gg


}




#' Plot temperature data as line plot, can facet to multiple sites
#'
#' @export
#' @param x tibble of hobotemp data
#' @param main character, title
#' @param xlabel character, title of xaxis
#' @param ylabel character, title of yaxis
#' @param ordered vector, vector of site names in order to be displayed bottom to top in legend and/or when facetted, must match site names in csv, default NULL
#' @param facet character, column to facet upon, default NULL
#' @param ... further arguments passed to \code{\link[ggplot2]{theme}}
#' @return ggplot2 object
draw_temp_line_plot <- function(x = example_ridge_data(),
                              main = "Temperature at Depth",
                              xlabel = "Date",
                              ylabel = "Temperature (degrees C)",
                              ordered = NULL,
                              facet = NULL,
                              ...){

  #define colorblind friendly palette with 15 colors
  pal <- c("#490092","#004949","#009292","#ff6db6","#ffb6db",
           "#000000","#924900","#b66dff","#6db6ff","#b6dbff",
           "#920000","#006ddb","#db6d00","#24ff24","#ffff6d")

  #factorize site column if order is defined (which is opposite of what is asked of user)
  if (!is.null(ordered)){
    x$Site <- as.factor(x$Site)
    x$Site <- factor(x$Site, levels = ordered)
    x$Site <- factor(x$Site, levels = rev(levels(x$Site)))
  }


  gg <- ggplot2::ggplot(data = x, ggplot2::aes(x = .data$DateTime, y = .data$Temp)) +
    #ggplot2::geom_point(na.rm = TRUE, alpha = alpha, shape = 4, ggplot2::aes(color = Sensor)) +
    ggplot2::scale_color_manual(values = pal, name = "Site") +
    ggplot2::geom_line(ggplot2::aes(color = x$Site)) +
    ggplot2::labs(title = main, x = xlabel, y = ylabel) +
    ggplot2::theme_bw()

  if (!is.null(facet)){
    gg <- gg + ggplot2::facet_wrap(ggplot2::vars(x$Site), ncol = 1, strip.position = "right")
  }

  gg
}


#' plot windrose for currents
#'
#' this is a wrapper around \code{\link[clifro]{windrose}}
#'
#' @export
#' @param x tibble of tiltometer data
#' @param legend_title title of legend
#' @param title character, or NA. Title of the plot, or NA to skip
#' @param speed.cuts character, numeric, or NA.
#' \itemize{
#' \item NA - the default, auto compute 5 quantiles
#' \item character - "quantile-3" or "quantile-4", computer 3 or 4 quantiles
#' \item numeric - explicit cut points in a vector
#' }
#' @param flip plotting water, not wind, default TRUE
#' @param ... further arguments passed to \code{\link[ggplot2]{theme}}
#' @return ggplot

#Add change colors, # wedges
tiltometer_rose <- function(x = read_tiltometer(),
                            legend_title = "Current Speed",
                            title = NA,
                            speed.cuts = NA,
                            flip = TRUE,
                            ...){
  if (FALSE){
    legend_title = "Current Speed"
    title = NA
    speed.cuts = NA
  }
  
  if (flip == TRUE) {
    x <- x %>% dplyr::mutate(dir = (.data$dir + 180) %% 360)
  }
  
  #https://math.stackexchange.com/questions/1319615/how-to-calculate-opposite-direction-angle
  
  site <- factor(x$Site)
  
  if (inherits(speed.cuts, "character") || is.na(speed.cuts)){
    
    speed.cuts <- switch(speed.cuts[1],
                         "quantile-3" = quantile(x$speed, probs = c(0, (1/3), (2/3))),
                         "quantile-4" = quantile(x$speed, probs = c(0, 0.25, 0.5, 0.75)),
                         ggplot2::cut_interval(x$speed, 5)
    )
    
    gg = clifro::windrose(x$speed,
                          x$dir,
                          legend_title = legend_title,
                          speed_cuts = speed.cuts,
                          facet = site,
                          ...)
  } else {
    
    gg = clifro::windrose(x$speed,
                          x$dir,
                          legend_title = legend_title,
                          facet = site,
                          ...)
  }
  
  if(!is.na(title[1])){
    gg = gg + ggplot2::labs(title = title[1])
  }
  
  return(gg)
}




#' Plot U and V vectors - possibly with low currents hidden from view
#'
#' @export
#' @param x tibble of tiltometer data
#' @param min_speed numeric, hide currents at or below this speed
#' @param alpha numeric, 0.1 default
#' @param facet character, name of the column to facet upon (like "Site") or NULL to skip
#' @param main character, title
#' @return ggplot2 object
draw_uv <- function(x = read_tiltometer(),
                    min_speed = 10,
                    main = "U-V currents",
                    facet = NULL,
                    alpha = 0.1){
  
  
  normalizeSite <- function(x, key, min_speed = 10) {
    # determine the start of each segment
    m <- abs(min_speed[1])
    x0 <- sqrt(m^2/(1 + x$v^2/x$u^2))
    y0 <- sqrt(m^2 - x0^2)
    
    x %>% dplyr::mutate(
      x0 = ifelse(.data$speed < m, .data$u, x0 * sign(.data$u)),
      y0 = ifelse(.data$speed < m, .data$v, y0 * sign(.data$v))) %>%
      dplyr::filter(.data$speed >= m)
  }
  
  x <- x %>%
    dplyr::group_by(.data$Site) %>%
    dplyr::arrange(.data$DateTime) %>%
    dplyr::group_map(normalizeSite, .keep = TRUE, min_speed = min_speed) %>%
    dplyr::bind_rows()
  
  
  xr <- range(x$u)
  yr <- range(x$v)
  r <- c(min(c(xr[1], yr[1])), max(c(xr[2], yr[2])) )
  gg <- ggplot2::ggplot(data = x, ggplot2::aes(x = .data$u, y = .data$v)) +
    ggplot2::coord_fixed(ratio = 1,
                         xlim = r,
                         ylim = r) +
    ggplot2::labs(title = main) +
    ggplot2::geom_segment(ggplot2::aes(x = x0,
                                       y = y0,
                                       xend = .data$u,
                                       yend = .data$v),
                          alpha = alpha[1])
  
  if (!is.null(facet)){
    gg <- gg + ggplot2::facet_wrap(facet)
  }
  
  gg
}


#' Plot waveheight data
#'
#' @export
#' @param x tibble of wave stats data
#' @param main character, title
#' @param xlabel character, title of xaxis
#' @param ylabel character, title of yaxis
#' @param facet character, name of the column to facet upon (like "Site") or NULL to skip
#' @param ... further arguments passed to \code{\link[ggplot2]{theme}}
#' @return ggplot2 object

wavespec_plot <- function(x = wave_stats(),
                          main = "Significant Wave Height",
                          xlabel = "Date",
                          ylabel = "Significant Wave Height (m)",
                          facet = NULL,
                          ...){
  
  
  gg <- ggplot2::ggplot(data = x, ggplot2::aes(x = .data$DateTime, y = .data$Hm0)) +
    ggplot2::geom_line(na.rm = TRUE, ggplot2::aes(color = .data$Site)) +
    ggplot2::labs(title = main, x = xlabel, y = ylabel)
  
  
  if (!is.null(facet)){
    gg <- gg + ggplot2::facet_wrap(facet)
  }
  
  gg
}



#' Plot PAR data as line plot
#'
#' @export
#' @param x tibble of parXtreem data
#' @param main character, title
#' @param xlabel character, title of xaxis
#' @param ylabel character, title of yaxis
#' @param facet character, name of the column to facet upon (like "Site") or NULL to skip
#' @param ... further arguments passed to \code{\link[ggplot2]{theme}}
#' @return ggplot2 object
draw_par_plot <- function(x = read_parXtreem(),
                      main = "PAR",
                      xlabel = "Date",
                      ylabel = "PAR (umols / m squared / second)",
                      facet = NULL,
                      ...){
  
  gg <- ggplot2::ggplot(data = x, ggplot2::aes(x = .data$DateTime, y = .data$PAR)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = main, x = xlabel, y = ylabel)
  
  if (!is.null(facet)){
    gg <- gg + ggplot2::facet_wrap(facet)
  }
  gg
}



