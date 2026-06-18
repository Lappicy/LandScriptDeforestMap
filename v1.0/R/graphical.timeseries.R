# Graph for Deforestation x Other classes growth ####
graphical.timeseries <-
  function(proxy.table,
           comparison.names = c("Growth_Agriculture", "Growth_Mining",
                                "Growth_Pasture", "Growth_Urban"),
           comparison.color = c("darkorange", "grey50",  "#EA9999", "purple"),
           save.as = "Results/Deforestation vs Growth.png",
           title.name = "Analysis for Caverna do Maroaga",
           different.groups = NULL,
           ...){

    # Dependencies ####
    require(ggplot2)
    require(sf)
    require(stats)


    # Function itself ####
    {
      # Get the dots (...) information ####
      dots2 <- list(...)

      if(any(!(names(dots2) == "width")) | is.null(names(dots2))) width.int <- 3000
      if(any(!(names(dots2) == "height")) | is.null(names(dots2))) height.int <- 1500
      if(any(!(names(dots2) == "units")) | is.null(names(dots2))) units.int <- "px"
      if(any(!(names(dots2) == "dpi")) | is.null(names(dots2))) dpi.int <- 300


      # Prepare data ####
      # Arguments for color and deforestation analysis
      comparison.deforest = "Deforestation"
      comparison.color.deforest = "darkgreen"

      # Make sure the data is in a data.frame format.
      if(any(class(proxy.table) == "sf")){
        proxy.table <- sf::st_drop_geometry(proxy.table)
      }
      proxy.table <- as.data.frame(proxy.table)

      # Make sure the Year column is numeric
      proxy.table$Year <- as.numeric(proxy.table$Year)

      # Columns that have "comparison.name" or "Deforestation" in their names
      columns.comparison <- match(comparison.names, colnames(proxy.table))
      columns.Deforestation <- match(comparison.deforest, colnames(proxy.table))

      # Remove the "Growth_" if it exists
      if(all(substr(comparison.names, 1, 7) == "Growth_")){
        comparison.names.new <- substr(comparison.names, 8, nchar(comparison.names))
      } else comparison.names.new <- comparison.names

      # Correlation analysis ####
      # Only get type.one and type.two columns
      cor.table <- proxy.table[,c(columns.Deforestation, columns.comparison)]
      colnames(cor.table) <- c(comparison.deforest, comparison.names.new)

      # Remove any blank data (NA)
      cor.table <- na.omit(cor.table)

      # Correlate the data
      suppressWarnings(cor.data <- stats::cor(cor.table)[,1])

      # Which class has the higher correlation
      cor.information <-
        round(max(abs(cor.data[2:length(cor.data)]), na.rm = TRUE), digits = 2)

      # Write a + or - sign accordingly
      ifelse(max(abs(cor.data[2:length(cor.data)]), na.rm = TRUE) ==
               max(cor.data[2:length(cor.data)], na.rm = TRUE),
             cor.information <- paste0("+", cor.information),
             cor.information <- paste0("-", cor.information))

      # Make sure it always has 5 characters
      if(nchar(cor.information) == 4) cor.information <- paste0(cor.information, "0")

      # Name the values
      names(cor.information) <- names(which.max(abs(cor.data[2:length(cor.data)])))


      # Prepare table for ggplot2 ####
      proxy.table <-
        tidyr::pivot_longer(data = proxy.table,
                            cols = c(columns.Deforestation, columns.comparison),
                            names_to = "Classes",
                            values_to = "NumValue") |>
        dplyr::group_by_at(c("Year", different.groups, "Classes")) |>
        dplyr::reframe(NumValue = sum(NumValue, na.rm = TRUE)) |>
        as.data.frame()

      # Change the "classes" from comparison.names to comparison.names.new
      proxy.table$Classes[proxy.table$Classes %in% comparison.names] <-
        comparison.names.new[match(proxy.table$Classes[proxy.table$Classes %in%
                                                         comparison.names],
                                   comparison.names)]

      # Transform classes into factors with order
      proxy.table$Classes <- factor(x = proxy.table$Classes,
                                    levels = c(comparison.deforest,
                                               sort(comparison.names.new)))

      # Transformar em Data
      proxy.table$Year <- as.Date(paste0(proxy.table$Year, "-01-01"))

      # GGPLOT2 graph ####
      gg.internal <-

        # Data being plotted
        ggplot2::ggplot(data = proxy.table,
                        aes(x = Year, y = NumValue, color = Classes)) +

        # Plot other classes
        ggplot2::geom_line(linewidth = 1, alpha = 0.3) +
        ggplot2::geom_point(alpha = 0.5) +

        ggplot2::scale_color_manual(values = c(comparison.color.deforest,
                                               comparison.color[order(comparison.names.new)])) +

        ggplot2::labs(title = title.name,
                      subtitle = paste0("Greatest correlation with ",
                                        names(cor.information), " (",
                                        cor.information, ")"),
                      x = "Year", y = bquote("[" ~ km^2 ~"]"), color = "Class:") +
        ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                                decimal.mark = ".",
                                                                scientific = F)) +
        ggplot2::scale_x_date(date_labels = "%Y") + # date_breaks = "1 year"

        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "top",
                       legend.text = element_text(size = 12, color = "black"),
                       legend.justification = "left",
                       legend.box.just = "left",
                       title = element_text(size = 12, color = "black", face = "bold"),
                       text = element_text(size = 12, color = "black"),
                       axis.text = element_text(size = 12, color = "black"))


      # Save the ggplot2 in "save.as" (with 3000 x 1500pixel with 300 dpi) ####
      ggplot2::ggsave(filename = save.as, plot = gg.internal,
                      width = width.int, height = height.int,
                      units = units.int, dpi = dpi.int)


    }

    # Returns ####
    # Returns the graph itself
    return(gg.internal)
  }

