# Graph for Deforestation x Other classes growth ####
gg.deforestation.cor <-
  function(proxy.table,
           type.one = "Growth",
           type.two = "Deforestation",
           columns_one = NULL, #"Variation_Water", "Variation_Agriculture", "Variation_Pasture", "Variation_Mining", "Variation_Urban", "Variation_Others"  #"Water", "Agriculture", "Pasture", "Mining", "Urban", "Others" 
           columns_two = NULL,
           color.one = c("purple", "grey50", "#EA9999", "darkorange"),
           color.two = "darkgreen",
           save.as = "Results/Deforestation vs Growth.png",
           title.name = "Analysis for Uaça 1 e 2",
           different.groups = NULL){
    
    # Libraries required ####
    require(ggplot2)
    require(stats)
    
    
    # Function itself ####
    # Prepare data ####
    # Make sure the data is in a data.frame format
    proxy.table <- as.data.frame(proxy.table)
    
    # Make sure the Year column is numeric
    proxy.table$Year <- as.numeric(proxy.table$Year)
    
    # Which columns have "type.one" or "type.two" in their names
    if (!is.null(columns_one)) {
      # Find exact match if "columns_one" exist
      columns.type.one <- intersect(columns_one, colnames(proxy.table))
    } else {
      # find the pattern in "type.one" 
      columns.type.one <- grep(type.one, colnames(proxy.table), value = TRUE)
    }
    
    # Same for columns "type.two"
    if (!is.null(columns_two)) {
      columns.type.two <- intersect(columns_two, colnames(proxy.table))
    } else {
      columns.type.two <- grep(type.two, colnames(proxy.table), value = TRUE)
    }
    
    # Get only these columns for the data.frame
    cols_maintain <- c(1:which(colnames(proxy.table) == "Year"),
                       which(colnames(proxy.table) %in%
                               c(columns.type.one, columns.type.two)))
    proxy.table <- proxy.table[,cols_maintain]
    
    # Change names to make it shorter (ex: "Variation_Water" -> "Water")
    columns.type.one.names <- sub(paste0("^", type.one, "_?"), "", columns.type.one)
    
    # Rename in the data.frame
    colnames(proxy.table)[match(columns.type.one, colnames(proxy.table))] <- columns.type.one.names
    columns.type.one <- columns.type.one.names
    
    # Similar for type.two
    columns.type.two <-
      colnames(proxy.table)[grep(type.two, colnames(proxy.table))]
    
    
    # Correlation analysis ####
    # Only get type.one and type.two columns
    cor.table <- proxy.table[,which(colnames(proxy.table) %in%
                                      c(columns.type.two, columns.type.one))]
    
    # Remove any blank data (NA)
    cor.table <- na.omit(cor.table)
    
    # Correlate the data
    cor.table.num <- which(colnames(cor.table) == columns.type.two)
    suppressWarnings(cor.data <- stats::cor(cor.table)[,cor.table.num])
    
    # Remove the own data to be correlated
    cor.data <- cor.data[-cor.table.num]
    
    # Which class has the higher correlation
    cor.information <-
      round(max(abs(cor.data[1:length(cor.data)]), na.rm = TRUE), digits = 2)
    
    # Write a + or - sign accordingly
    ifelse(max(abs(cor.data[1:length(cor.data)]), na.rm = TRUE) ==
             max(cor.data[1:length(cor.data)], na.rm = TRUE),
           cor.information <- paste0("+", cor.information),
           cor.information <- paste0("-", cor.information))
    
    # Make sure it always has 5 characters
    if(nchar(cor.information) == 4) cor.information <- paste0(cor.information, "0")
    
    # Name the values
    names(cor.information) <-
      names(which.max(abs(cor.data[1:length(cor.data)])))
    #columns.type.one[which.max(abs(cor.data[1:length(cor.data)]))]
    
    
    # Prepare table for ggplot2 ####
    proxy.table <-
      tidyr::pivot_longer(data = proxy.table,
                          cols = c(columns.type.two, columns.type.one),
                          names_to = "Classes",
                          values_to = "NumValue") |>
      dplyr::group_by_at(c("Year", different.groups, "Classes")) |>
      dplyr::reframe(NumValue = sum(NumValue, na.rm = TRUE)) |>
      as.data.frame()
    
    # Alfabetical order of type.one and type.two
    order.types <- order(c(columns.type.one, columns.type.two))
    
    
    # GGPLOT2 graph ####
    gg.internal <-
      
      # Data being plotted
      ggplot2::ggplot(data = proxy.table,
                      aes(x = Year, y = NumValue, color = Classes)) +
      
      # Plot the deforestation only
      ggplot2::geom_line(data = proxy.table[proxy.table$Classes %in%
                                              columns.type.two,],
                         color = color.two, linewidth = 1, alpha = 0.5) +
      
      # Plot other classes
      ggplot2::geom_line(linewidth = 1, alpha = 0.3) +
      ggplot2::geom_point(alpha = 0.5) +
      
      ggplot2::scale_color_manual(values = c(color.one, color.two)[order.types]) +
      
      ggplot2::labs(title = title.name,
                    subtitle = paste0("Greatest correlation with ",
                                      names(cor.information), " (",
                                      cor.information, ")"),
                    x = "Year", y = bquote("[" ~ km^2 ~"]"), color = "Class:") +
      ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                              decimal.mark = ".",
                                                              scientific = F)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "top",
                     legend.text = element_text(size = 12, color = "black"),
                     legend.justification = "left",
                     legend.box.just = "left",
                     title = element_text(size = 12, color = "black", face = "bold"),
                     text = element_text(size = 12, color = "black"),
                     axis.text = element_text(size = 12, color = "black"))
    
    
    # save where the user setted it to
    ggplot2::ggsave(filename = save.as, plot = gg.internal,
                    width = 3000, height = 1500, units = "px", dpi = 300)
    
    
    # Returns (NULL) ####
    return(NULL)
  }



# Mapa para um ano em específico e uma classe específica ####
mesh.map <- function(mesh.data,
                     class = "Deforestation",
                     year.used = "all",
                     col.limits = c(0, 1, 2, 5),
                     col.used = c("white", "#E5E200", "#FC780D", "red", "darkred"),
                     save.map.as = NULL,
                     map.height = 3000, map.width = 2000, map.units = "px",
                     grid.color = "#17212B66",
                     classes.column = NULL,
                     classes.col = c("pink", "purple", "#E1AD01", "darkgreen", "lightgreen"),
                     highlight = NULL){
  
  # Libraries used ####
  require(dplyr)
  require(ggplot2)
  require(ggspatial)
  require(sf)
  require(nngeo)
  
  
  # Prepare data ####
  mesh.data <- read.geo(mesh.data)
  study.area <- mesh.data[match(unique(mesh.data$ID_mesh), mesh.data$ID_mesh),]
  whole.area <- nngeo::st_remove_holes(sf::st_as_sf(st_union(study.area)))
  
  
  # If there are the highlights + classes.column + study.area parameter,
  # create three desired masks
  extra.masks <- all(!is.null(highlight), !is.null(classes.column))
  
  if(extra.masks){
    highlight.mask.col <-
      which(colnames(study.area) == classes.column)
    highlight.mask.row <-
      which(sf::st_drop_geometry(study.area)[,highlight.mask.col] == highlight)
    
    # highlight mask
    highlight.mask <- study.area$geometry[highlight.mask.row]
    highlight.mask <- sf::st_as_sf(sf::st_union(highlight.mask))
    
    # Difference mask
    suppressWarnings({
      sf::sf_use_s2(FALSE)
      difference.mask <- sf::st_difference(x = whole.area, y = highlight.mask)
      sf::sf_use_s2(TRUE)
    })
    
    # Study area mask
    difference.col <- which(colnames(study.area) == classes.column)
    difference.row <- which(sf::st_drop_geometry(study.area)[,difference.col] == highlight)
    study.mask <- study.area #study.area[-difference.row,]
    study.mask$geometry[difference.row] <- st_point()
  }
  
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
  
  
  # GGPLOT2 ####
  map.internal <-
    ggplot() +
    
    # Plot the choosen class
    geom_sf(data = mesh.data, aes(fill = Value_Class), color = grid.color) +
    
    # Plot the study mask if it exists
    {if(extra.masks){
      geom_sf(data = study.mask, fill = classes.col,
              color = "black")}} +
    
    # Plot the difference mask if it exists
    {if(extra.masks){
      geom_sf(data = difference.mask, fill = "white", alpha = 0.7,
              color = "black")}} +
    
    # Plot the study area
    geom_sf(data = study.area, fill = "transparent", color = grid.color) +
    
    # Plot the outline as a black line
    geom_sf(data = whole.area, fill = "transparent", color = "black", lwd = 1) +
    
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
    
    # If there is a highlight, create a subtitle
    {if(!is.null(highlight)){
      labs(subtitle = highlight)}} +
    
    # Geographic north + bar scale
    ggspatial::annotation_scale(location = "br",
                                bar_cols = c("black", "white")) +
    ggspatial::annotation_north_arrow(location = "tl", which_north = "true",
                                      height = unit(1, "cm"),  width = unit(1, "cm"),
                                      pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm"),
                                      style = north_arrow_orienteering(fill = c("black", "white"),
                                                                       line_col = "grey20")) +
    
    # Map theme
    theme_bw() +
    theme(legend.position = "right",
          legend.justification = "top",
          legend.spacing.y = unit(0.5, "cm"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black")) +
    guides(fill = guide_legend(byrow = TRUE))
  
  
  # Save graph (if not NULL) ####
  if(!is.null(save.map.as)){
    ggplot2::ggsave(filename = save.map.as,
                    plot = map.internal,
                    height = map.height, width = map.width,
                    units = map.units, dpi = 300)
  }
  
  
  # Return ####
  # Returns the map itself if still wants to use it
  return(map.internal)
}



# Sankey Diagram for two years ####
SankeyDiagram <- function(tif1 = NULL, tif2 = NULL){
  
  # Libraries used ####
  require(raster)
  
  
  # Function itself ####
  tif1.file <- raster::raster(tif1)
  tif2.file <- raster::raster(tif2)
  
}



# End -------------------------------------------------------------------------
