# Map for a specific year and class ####
map.layout <- function(mesh.data,
                       class = "Deforestation",
                       year.used = "all",
                       col.limits = c(0, 1, 2, 5), #c(0, 30, 60, 100),
                       col.used = c("white", "#E5E200", "#FC780D", "red", "darkred"),
                       save.map.as,
                       ...){

  # Dependencies ####
  require(ggplot2)
  require(ggspatial)
  require(sf)


  # Get the dots (...) information ####
  dots2 <- list(...)

  if(any(!(names(dots2) == "width")) | is.null(names(dots2))) width.int <- 3000
  if(any(!(names(dots2) == "height")) | is.null(names(dots2))) height.int <- 1500
  if(any(!(names(dots2) == "units")) | is.null(names(dots2))) units.int <- "px"
  if(any(!(names(dots2) == "dpi")) | is.null(names(dots2))) dpi.int <- 300


  # Prepare data ####
  mesh.data <- read.geo(mesh.data)

  # Change the column from the class to "value" to use in the function
  num.column.value <- which(colnames(mesh.data) == class)
  colnames(mesh.data)[num.column.value] <- "Value"

  # Change year into numeric if it is "ALL"
  if(any(toupper(year.used) == "ALL")) year.used <- sort(unique(mesh.data$Year))

  # Sum the classes over the years
  mesh.data <-
    mesh.data |>
    dplyr::filter(Year %in% year.used) |>
    dplyr::mutate(Value = ifelse(Value <= 0, 0, Value)) |>
    dplyr::group_by(ID_mesh, geometry) |>
    dplyr::reframe(Value = sum(Value, na.rm = TRUE)) |>
    dplyr::select(ID_mesh, Value, geometry) |>
    as.data.frame() |>
    sf::st_as_sf()


  # Change the data into the pre-defined classes and then as factors
  suppressWarnings({
    if(exists("col.limits")){

      # Create new column
      mesh.data$Value_Class <- NA

      # Loop for every data point in mesh.data
      for(i.mesh.map in 1:nrow(mesh.data)){

        # Acess the Value "i"
        x.var <- mesh.data$Value[i.mesh.map]

        # If it is NA, jump to next level
        if(is.na(x.var)) next

        mesh.data$Value_Class[i.mesh.map] <-
          if(x.var == 0){1} else
            if(findInterval(x.var, sort(c(col.limits[1], col.limits[2]))) == 1L){2} else
              if(findInterval(x.var, sort(c(col.limits[2], col.limits[3]))) == 1L){3} else
                if(findInterval(x.var, sort(c(col.limits[3], col.limits[4]))) == 1L){4} else
                  if(findInterval(x.var, sort(c(col.limits[4], Inf))) == 1L){5}

      }

      # Turn column into factor
      mesh.data$Value_Class <- as.factor(mesh.data$Value_Class)

      # Create label for ggplot2
      col.used.label <-
        c(paste0("No ", tolower(class)),
          bquote("Between " ~ .(col.limits[1]) ~ " and " ~ .(col.limits[2]) ~ km^{2}),
          bquote("Between " ~ .(col.limits[2]) ~ " and " ~ .(col.limits[3]) ~ km^{2}),
          bquote("Between " ~ .(col.limits[3]) ~ " and " ~ .(col.limits[4]) ~ km^{2}),
          bquote("More than " ~ .(col.limits[4]) ~ km^{2}))
    }
  })


  # If there are no values pre defines in col.limits
  if(!exists("col.limits")) mesh.data$Value_Class <- mesh.data$Value


  # Ggplot map ####
  map.internal <-
    ggplot() +

    # Plot the choosen class
    geom_sf(data = mesh.data, aes(fill = Value_Class), color = "black") +

    # Plot the outline as a black line
    geom_sf(data = st_union(mesh.data), fill = "transparent",
            color = "black", lwd = 1) +

    # Class coloration if there are no col.limits or col.used argument
    {if(!all(c(exists("col.limits"), exists("col.used")))){
      scale_fill_continuous(low = "#22B34D", high = "red")}} +

    # Class coloration if there are col.limits & col.used arguments
    {if(all(c(exists("col.limits"), exists("col.used")))){
      scale_fill_manual(values = col.used,
                        labels = col.used.label)}} +

    # Title for map, along with x and y axis and legend
    labs(title = paste0(class, " in ", year.used),
         x = "Longitude", y = "Latitude",
         fill = bquote(.(class) ~ " in " ~ km^{2} ~ ":")) +

    # If there are many years, change the title name
    {if(length(year.used) != 1){
      labs(title = paste0("Acumulated ", class,
                          " between ", min(year.used),
                          " and ", max(year.used)))}} +

    # Geographic north + bar scale
    ggspatial::annotation_scale(location = "br",
                                bar_cols = c("black", "white")) +
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                      pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                                      style = north_arrow_orienteering(fill = c("black", "white"),
                                                                       line_col = "grey20")) +

    # Map theme
    theme_bw() +
    theme(legend.position = "right",
          legend.justification = "top",
          legend.spacing.y = unit(0.5, "cm"),
          legend.text = element_text(color = "black"),
          text = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          title = element_text( color = "black")) +
    guides(fill = guide_legend(byrow = TRUE))


  # Save graph ####
  ggplot2::ggsave(plot = map.internal, filename = save.map.as,
                  width = width.int, height = height.int,
                  units = units.int, dpi = dpi.int)


  # Return ####
  # Returns the map itself if still wants to usee it
  return(map.internal)
}
