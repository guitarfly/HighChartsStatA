###########################################################################################################
######## Nice Highcharter Functions #######################################################################
################################################################ Benjamin Marti. 14.08.2024 ###############

# load packages:
pacman::p_load(tidyr, dplyr, highcharter, viridis)
# install.packages("C:/dev/StatA_Color_Palette/ColStatA_0.1.0.tar.gz", repos = NULL, type = "source")

### StatA.bar #############################################################################################
# Helper function to insert line breaks in long labels
insert_line_breaks <- function(label, max_length = 20) {
  label <- as.character(label) # Ensure the label is a character string
  words <- strsplit(label, " ")[[1]]
  lines <- character()
  current_line <- ""

  for (word in words) {
    if (nchar(paste(current_line, word, sep = " ")) > max_length) {
      lines <- c(lines, current_line)
      current_line <- word
    } else {
      current_line <- paste(current_line, word, sep = " ")
    }
  }
  lines <- c(lines, current_line)
  return(paste(lines, collapse = "<br/>"))
}

# Define the function
StatA.bar <- function(data_long, group_col = FALSE, measure_col, value_col, title = "Stacked Column Chart", subtitle = NULL,
                      x_axis_title = "Measurements", y_axis_title = "Mean Value", legend_title = NULL,
                      palette = "viridis", stacked = TRUE, horizontal = FALSE, source_text = "Source",
                      background_transparent = FALSE, legend_position = "right", toolbox_sum = FALSE, toolbox_mean = FALSE) {

  # Ensure only one of toolbox_sum or toolbox_mean is TRUE
  if (toolbox_sum && toolbox_mean) {
    stop("Only one of toolbox_sum or toolbox_mean can be TRUE.")
  }

  # Determine if grouping is required
  if (isFALSE(group_col)) {
    unique_groups <- "All Data" # Use a placeholder label for no grouping
    num_colors <- 1
    show_legend <- FALSE # No legend if no grouping
  } else {
    # Get unique groups and measurements
    unique_groups <- unique(data_long[[group_col]])
    num_colors <- length(unique_groups)
    show_legend <- TRUE # Show legend when there is grouping
  }

  unique_measurements <- unique(data_long[[measure_col]])

  # Check if palette is a character vector (custom hex codes)
  if (is.character(palette) && length(palette) > 1) {
    if (length(palette) < num_colors) {
      stop("Not enough colors in the custom palette for the number of groups.")
    }
    palette_colors <- palette
  } else {
    # Get colors from the appropriate viridis palette
    if (num_colors <= 12) {
      palette_colors <- viridis::viridis(num_colors)
    } else {
      stop("This function supports up to 12 unique groups.")
    }
  }

  # Determine stacking option
  stacking_option <- if (stacked) "normal" else NULL

  # Determine chart type and axis swap
  chart_type <- if (horizontal) "bar" else "column"

  # Define xAxis and yAxis based on orientation
  xAxis <- list(categories = unique_measurements, title = list(text = x_axis_title, style = list(marginBottom = 40)), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0)
  yAxis <- list(min = 0, title = list(text = y_axis_title), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0, gridLineColor = "#e0e0e0")

  if (horizontal) {
    xAxis <- list(min = 0, title = list(text = y_axis_title), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0, gridLineColor = "#e0e0e0")
    yAxis <- list(categories = unique_measurements, title = list(text = x_axis_title, style = list(marginBottom = 40)), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0, tickLength = 0)
  }

  # Create the stacked column chart with a minimalist Datawrapper-like design
  hc <- highchart() %>%
    hc_chart(type = chart_type, zoomType = "xy", backgroundColor = if (background_transparent) "transparent" else NULL) %>%
    hc_title(text = title, style = list(fontSize = "16px", fontWeight = "normal", color = "#333333"), align = "center") %>%
    hc_subtitle(text = subtitle, style = list(fontSize = "12px", color = "#666666")) %>%
    hc_xAxis(categories = xAxis$categories, title = xAxis$title, labels = xAxis$labels, gridLineWidth = xAxis$gridLineWidth) %>%
    hc_yAxis(min = yAxis$min, title = yAxis$title, labels = yAxis$labels, gridLineWidth = yAxis$gridLineWidth, gridLineColor = yAxis$gridLineColor) %>%
    hc_plotOptions(series = list(stacking = stacking_option, borderWidth = 0, dataLabels = list(enabled = FALSE),
                                 groupPadding = 0.1, pointPadding = 0.05))  # Adjust padding to control spacing between bars

  # Add series based on grouping
  if (isFALSE(group_col)) {
    # Single group (no grouping)
    hc <- hc %>%
      hc_add_series(
        data = data_long %>%
          arrange(!!sym(measure_col)) %>%
          pull(!!sym(value_col)),
        name = "All Data", # A single group without grouping
        color = palette_colors[1]
      )
  } else {
    # Add series for each group
    for (i in seq_along(unique_groups)) {
      group_name <- unique_groups[i]
      hc <- hc %>%
        hc_add_series(
          data = data_long %>%
            filter(!!sym(group_col) == group_name) %>%
            arrange(!!sym(measure_col)) %>%
            pull(!!sym(value_col)),
          name = insert_line_breaks(group_name),
          stack = "stack",
          color = palette_colors[i]
        )
    }
  }

  # Add legend only if group_col is not FALSE
  if (show_legend) {
    hc <- hc %>%
      hc_legend(title = list(text = legend_title, style = list(fontSize = "14px", fontWeight = "normal", color = "#666666")), layout = if (legend_position == "top") "horizontal" else "vertical",
                align = if (legend_position == "top") "center" else "right",
                verticalAlign = if (legend_position == "top") "top" else "middle",
                symbolHeight = 12,
                symbolWidth = 12,
                symbolRadius = 0,
                itemStyle = list(
                  fontSize = "12px",
                  fontWeight = "normal",
                  color = "#666666"
                ),
                itemMarginTop = if (legend_position == "top") 5 else NULL)
  } else {
    hc <- hc %>%
      hc_legend(enabled = FALSE) # Disable legend if no grouping
  }

  hc <- hc %>%
    hc_tooltip(shared = TRUE, valueDecimals = NA, backgroundColor = "#ffffffE6", borderColor = "#cccccc", borderRadius = 3, borderWidth = 1, padding = 8,
               formatter = if (toolbox_sum) JS("
    function() {
      var points = this.points;
      var sum = 0;
      points.forEach(function(point) {
        sum += point.y;
      });
      var s = '<span style=\"font-size: 14px;\">' + this.x + '</span>';  // Increase font size of the title without bold
      points.forEach(function(point) {
        s += '<br/>' + '<span style=\"color:' + point.color + '\">\u25CF</span> <span style=\"font-weight: normal;\">' + point.series.name + ':</span> <b>' + point.y + '</b>';
      });
      var sumFormatted = (sum % 1 === 0) ? sum : sum.toFixed(2);  // Check if sum is an integer; if not, format to 2 decimals
      s += '<br/><span style=\"font-weight: normal;\"><i>Summe:</i></span> <i><b>' + sumFormatted + '</b></i>';
      return s;
    }")
               else if (toolbox_mean) JS("
    function() {
      var points = this.points;
      var sum = 0;
      var count = points.length;
      points.forEach(function(point) {
        sum += point.y;
      });
      var avg = sum / count;  // Calculate average
      var s = '<span style=\"font-size: 14px;\">' + this.x + '</span>';  // Increase font size of the title without bold
      points.forEach(function(point) {
        s += '<br/>' + '<span style=\"color:' + point.color + '\">\u25CF</span> <span style=\"font-weight: normal;\">' + point.series.name + ':</span> <b>' + point.y + '</b>';
      });
      var avgFormatted = (avg % 1 === 0) ? avg : avg.toFixed(2);  // Check if avg is an integer; if not, format to 2 decimals
      s += '<br/><span style=\"font-weight: normal;\"><i>Durchschnitt:</i></span> <i><b>' + avgFormatted + '</b></i>';
      return s;
    }")
               else JS("
    function() {
      var s = '<span style=\"font-size: 14px;\">' + this.x + '</span>';  // Increase font size of the title without bold
      this.points.forEach(function(point) {
        s += '<br/>' + '<span style=\"color:' + point.color + '\">\u25CF</span> <span style=\"font-weight: normal;\">' + point.series.name + ':</span> <b>' + point.y + '</b>';
      });
      return s;
    }")
    ) %>%
    hc_exporting(
      enabled = TRUE,
      buttons = list(contextButton = list(
        symbolStroke = if (background_transparent) "#666666" else NULL,
        theme = list(
          fill = if (background_transparent) "transparent" else NULL,
          states = list(
            hover = list(
              fill = if (background_transparent) "transparent" else NULL
            ),
            select = list(
              fill = if (background_transparent) "transparent" else NULL
            )
          )
        )
      ))
    ) %>%
    hc_add_theme(
      hc_theme(
        colors = palette_colors,
        chart = list(style = list(fontFamily = "Arial")),
        title = list(style = list(fontSize = "16px", fontWeight = "bold", color = "#333333")),
        subtitle = list(style = list(fontSize = "12px", color = "#666666")),
        xAxis = list(gridLineWidth = 0, labels = list(style = list(fontSize = "12px", color = "#666666"))),
        yAxis = list(gridLineWidth = 0, gridLineColor = "#e0e0e0", labels = list(style = list(fontSize = "12px", color = "#666666"))),
        legend = list(itemStyle = list(fontSize = "12px", color = "#666666")),
        tooltip = list(backgroundColor = "#ffffff", borderColor = "#cccccc", borderRadius = 3, borderWidth = 1, padding = 8)
      )
    )

  # Increase bottom margin to accommodate source text with controlled distance
  hc <- hc %>%
    hc_chart(marginBottom = 80) %>%
    hc_credits(enabled = TRUE, text = source_text,
               position = list(align = "left", x = 15, y = -20),  # Adjusted x to align with y-axis labels and y to position below the x-axis
               style = list(fontSize = "10px", color = "#666666", fontFamily = "Arial"))

  return(hc)
}


# # Example usage with the iris dataset in long format
# mtcars_long <- mtcars %>%
#   pivot_longer(cols = -c(cyl, gear), names_to = "Measurement", values_to = "Value") %>%
#   group_by(cyl, gear, Measurement) %>%
#   summarise(Mean = mean(Value), .groups = 'drop')
#
# hc <- StatA.bar(
#   mtcars_long,
#   group_col = F,
#   measure_col = "gear",
#   value_col = "cyl",
#   title = "Average Measurements of Iris Species",
#   subtitle = "Data from Fisher's Iris dataset",
#   x_axis_title = "Measurement Type",
#   y_axis_title = "Average Value",
#   legend_title = "Species",
#   palette = "Accent",
#   stacked = TRUE,
#   horizontal = FALSE,
#   source_text = "Source: Fisher's Iris dataset, lkdsfjdslkfjdsflkdsjfalkfjölkjsaödlfjsadölkfjdsaölfkjdsafölkdsajföldsakjföldsakjf",
#   background_transparent = TRUE,
#   legend_position = "top",
#   toolbox_sum = TRUE
# )
# hc


### StatA.line ##########################################################################################################################
# Define the function
StatA.line <- function(data_long, group_col, measure_col, value_col, title = "Line Chart", subtitle = NULL,
                       x_axis_title = "Measurements", y_axis_title = "Mean Value", legend_title = NULL,
                       palette = "viridis", horizontal = FALSE, source_text = "Source",
                       background_transparent = FALSE, legend_position = "right", toolbox_sum = FALSE, toolbox_mean = FALSE) {

  # Ensure only one of toolbox_sum or toolbox_mean is TRUE
  if (toolbox_sum && toolbox_mean) {
    stop("Only one of toolbox_sum or toolbox_mean can be TRUE.")
  }

  # Get unique groups and measurements
  unique_groups <- unique(data_long[[group_col]])
  unique_measurements <- unique(data_long[[measure_col]])

  # Determine the number of colors needed
  num_colors <- length(unique_groups)

  # Check if palette is a character vector (custom hex codes)
  if (is.character(palette) && length(palette) > 1) {
    if (length(palette) < num_colors) {
      stop("Not enough colors in the custom palette for the number of groups.")
    }
    palette_colors <- palette
  } else {
    # Get colors from the appropriate viridis palette
    if (num_colors <= 12) {
      palette_colors <- viridis::viridis(num_colors)
    } else {
      stop("This function supports up to 12 unique groups.")
    }
  }

  # Determine chart type and axis swap
  chart_type <- "line"

  # Define xAxis and yAxis based on orientation
  xAxis <- list(categories = unique_measurements, title = list(text = x_axis_title, style = list(marginBottom = 40)), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0)
  yAxis <- list(min = 0, title = list(text = y_axis_title), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0, gridLineColor = "#e0e0e0")

  if (horizontal) {
    xAxis <- list(min = 0, title = list(text = y_axis_title), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0, gridLineColor = "#e0e0e0")
    yAxis <- list(categories = unique_measurements, title = list(text = x_axis_title, style = list(marginBottom = 40)), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0, tickLength = 0)
  }

  # Create the line chart with a minimalist Datawrapper-like design
  hc <- highchart() %>%
    hc_chart(type = chart_type, zoomType = "xy", backgroundColor = if (background_transparent) "transparent" else NULL) %>%
    hc_title(text = title, style = list(fontSize = "16px", fontWeight = "normal", color = "#333333"), align = "center") %>%
    hc_subtitle(text = subtitle, style = list(fontSize = "12px", color = "#666666")) %>%
    hc_xAxis(categories = xAxis$categories, title = xAxis$title, labels = xAxis$labels, gridLineWidth = xAxis$gridLineWidth) %>%
    hc_yAxis(min = yAxis$min, title = yAxis$title, labels = yAxis$labels, gridLineWidth = yAxis$gridLineWidth, gridLineColor = yAxis$gridLineColor) %>%
    hc_plotOptions(line = list(dataLabels = list(enabled = FALSE), enableMouseTracking = TRUE)) # Adjust for line chart options

  # Add series for each group
  for (i in seq_along(unique_groups)) {
    group_name <- unique_groups[i]
    hc <- hc %>%
      hc_add_series(
        data = data_long %>%
          filter(!!sym(group_col) == group_name) %>%
          arrange(!!sym(measure_col)) %>%
          pull(!!sym(value_col)),
        name = insert_line_breaks(group_name),
        color = palette_colors[i],
        type = "line"
      )
  }

  hc <- hc %>%
    hc_legend(title = list(text = legend_title, style = list(fontSize = "14px", fontWeight = "normal", color = "#666666")), layout = if (legend_position == "top") "horizontal" else "vertical",
              align = if (legend_position == "top") "center" else "right",
              verticalAlign = if (legend_position == "top") "top" else "middle",
              symbolHeight = 12,
              symbolWidth = 12,
              symbolRadius = 0,
              itemStyle = list(
                fontSize = "12px",
                fontWeight = "normal",
                color = "#666666"
              ),
              itemMarginTop = if (legend_position == "top") 5 else NULL) %>%
    hc_tooltip(shared = TRUE, valueDecimals = NA, backgroundColor = "#ffffffE6", borderColor = "#cccccc", borderRadius = 3, borderWidth = 1, padding = 8,
               formatter = if (toolbox_sum) JS("
    function() {
      var points = this.points;
      var sum = 0;
      points.forEach(function(point) {
        sum += point.y;
      });
      var s = '<span style=\"font-size: 14px;\">' + this.x + '</span>';  // Increase font size of the title without bold
      points.forEach(function(point) {
        s += '<br/>' + '<span style=\"color:' + point.color + '\">\u25CF</span> <span style=\"font-weight: normal;\">' + point.series.name + ':</span> <b>' + point.y + '</b>';
      });
      var sumFormatted = (sum % 1 === 0) ? sum : sum.toFixed(2);  // Check if sum is an integer; if not, format to 2 decimals
      s += '<br/><span style=\"font-weight: normal;\"><i>Summe:</i></span> <i><b>' + sumFormatted + '</b></i>';
      return s;
    }")
               else if (toolbox_mean) JS("
    function() {
      var points = this.points;
      var sum = 0;
      var count = points.length;
      points.forEach(function(point) {
        sum += point.y;
      });
      var avg = sum / count;  // Calculate average
      var s = '<span style=\"font-size: 14px;\">' + this.x + '</span>';  // Increase font size of the title without bold
      points.forEach(function(point) {
        s += '<br/>' + '<span style=\"color:' + point.color + '\">\u25CF</span> <span style=\"font-weight: normal;\">' + point.series.name + ':</span> <b>' + point.y + '</b>';
      });
      var avgFormatted = (avg % 1 === 0) ? avg : avg.toFixed(2);  // Check if average is an integer; if not, format to 2 decimals
      s += '<br/><span style=\"font-weight: normal;\"><i>Mittelwert:</i></span> <i><b>' + avgFormatted + '</b></i>';
      return s;
    }")
               else JS("
    function() {
      var s = '<span style=\"font-size: 14px;\">' + this.x + '</span>';  // Increase font size of the title without bold
      this.points.forEach(function(point) {
        s += '<br/>' + '<span style=\"color:' + point.color + '\">\u25CF</span> <span style=\"font-weight: normal;\">' + point.series.name + ':</span> <b>' + point.y + '</b>';
      });
      return s;
    }")
    ) %>%
    hc_exporting(
      enabled = TRUE,
      buttons = list(contextButton = list(
        symbolStroke = if (background_transparent) "#666666" else NULL,
        theme = list(
          fill = if (background_transparent) "transparent" else NULL,
          states = list(
            hover = list(
              fill = if (background_transparent) "transparent" else NULL
            ),
            select = list(
              fill = if (background_transparent) "transparent" else NULL
            )
          )
        )
      ))
    ) %>%
    hc_add_theme(
      hc_theme(
        colors = palette_colors,
        chart = list(style = list(fontFamily = "Arial")),
        title = list(style = list(fontSize = "16px", fontWeight = "bold", color = "#333333")),
        subtitle = list(style = list(fontSize = "12px", color = "#666666")),
        xAxis = list(gridLineWidth = 0, labels = list(style = list(fontSize = "12px", color = "#666666"))),
        yAxis = list(gridLineWidth = 0, gridLineColor = "#e0e0e0", labels = list(style = list(fontSize = "12px", color = "#666666"))),
        legend = list(itemStyle = list(fontSize = "12px", color = "#666666")),
        tooltip = list(backgroundColor = "#ffffff", borderColor = "#cccccc", borderRadius = 3, borderWidth = 1, padding = 8)
      )
    )

  # Increase bottom margin to accommodate source text with controlled distance
  hc <- hc %>%
    hc_chart(marginBottom = 80) %>%
    hc_credits(enabled = TRUE, text = source_text,
               position = list(align = "left", x = 15, y = -20),  # Adjusted x to align with y-axis labels and y to position below the x-axis
               style = list(fontSize = "10px", color = "#666666", fontFamily = "Arial"))

  return(hc)
}


# # Example usage with the iris dataset in long format
# mtcars_long <- mtcars %>%
#   pivot_longer(cols = -c(cyl, gear), names_to = "Measurement", values_to = "Value") %>%
#   group_by(cyl, gear, Measurement) %>%
#   summarise(Mean = mean(Value), .groups = 'drop')
#
# hc <- StatA.line(
#   mtcars_long,
#   group_col = "Measurement",
#   measure_col = "gear",
#   value_col = "cyl",
#   title = "Average Measurements of Iris Species",
#   subtitle = "Data from Fisher's Iris dataset",
#   x_axis_title = "Measurement Type",
#   y_axis_title = "Average Value",
#   legend_title = "Species",
#   palette = "Accent",
#   stacked = TRUE,
#   horizontal = FALSE,
#   source_text = "Source: Fisher's Iris dataset, lkdsfjdslkfjdsflkdsjfalkfjölkjsaödlfjsadölkfjdsaölfkjdsafölkdsajföldsakjföldsakjf",
#   background_transparent = TRUE,
#   legend_position = "top",
#   toolbox_sum = TRUE
# )
# hc


### StatA.area ####################################################################################################
# Define the function for area plots with stacking option
StatA.area <- function(data_long, group_col, measure_col, value_col, title = "Area Chart", subtitle = NULL,
                       x_axis_title = "Measurements", y_axis_title = "Mean Value", legend_title = NULL,
                       palette = "viridis", horizontal = FALSE, stacked = FALSE, source_text = "Source",
                       background_transparent = FALSE, legend_position = "right", toolbox_sum = FALSE, toolbox_mean = FALSE) {

  # Ensure only one of toolbox_sum or toolbox_mean is TRUE
  if (toolbox_sum && toolbox_mean) {
    stop("Only one of toolbox_sum or toolbox_mean can be TRUE.")
  }

  # Get unique groups and measurements
  unique_groups <- unique(data_long[[group_col]])
  unique_measurements <- unique(data_long[[measure_col]])

  # Determine the number of colors needed
  num_colors <- length(unique_groups)

  # Check if palette is a character vector (custom hex codes)
  if (is.character(palette) && length(palette) > 1) {
    if (length(palette) < num_colors) {
      stop("Not enough colors in the custom palette for the number of groups.")
    }
    palette_colors <- palette
  } else {
    # Get colors from the appropriate viridis palette
    if (num_colors <= 12) {
      palette_colors <- viridis::viridis(num_colors)
    } else {
      stop("This function supports up to 12 unique groups.")
    }
  }

  # Define xAxis and yAxis based on orientation
  xAxis <- list(categories = unique_measurements, title = list(text = x_axis_title, style = list(marginBottom = 40)), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0)
  yAxis <- list(min = 0, title = list(text = y_axis_title), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0, gridLineColor = "#e0e0e0")

  if (horizontal) {
    xAxis <- list(min = 0, title = list(text = y_axis_title), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0, gridLineColor = "#e0e0e0")
    yAxis <- list(categories = unique_measurements, title = list(text = x_axis_title, style = list(marginBottom = 40)), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0, tickLength = 0)
  }

  # Create the area chart with a minimalist Datawrapper-like design
  hc <- highchart() %>%
    hc_chart(type = "area", zoomType = "xy", backgroundColor = if (background_transparent) "transparent" else NULL) %>%
    hc_title(text = title, style = list(fontSize = "16px", fontWeight = "normal", color = "#333333"), align = "center") %>%
    hc_subtitle(text = subtitle, style = list(fontSize = "12px", color = "#666666")) %>%
    hc_xAxis(categories = xAxis$categories, title = xAxis$title, labels = xAxis$labels, gridLineWidth = xAxis$gridLineWidth) %>%
    hc_yAxis(min = yAxis$min, title = yAxis$title, labels = yAxis$labels, gridLineWidth = yAxis$gridLineWidth, gridLineColor = yAxis$gridLineColor) %>%
    hc_plotOptions(area = list(stacking = if (stacked) "normal" else NULL, dataLabels = list(enabled = FALSE), enableMouseTracking = TRUE, fillOpacity = 0.5))

  # Add series for each group
  for (i in seq_along(unique_groups)) {
    group_name <- unique_groups[i]
    hc <- hc %>%
      hc_add_series(
        data = data_long %>%
          filter(!!sym(group_col) == group_name) %>%
          arrange(!!sym(measure_col)) %>%
          pull(!!sym(value_col)),
        name = insert_line_breaks(group_name),
        color = palette_colors[i],
        type = "area"
      )
  }

  hc <- hc %>%
    hc_legend(title = list(text = legend_title, style = list(fontSize = "14px", fontWeight = "normal", color = "#666666")), layout = if (legend_position == "top") "horizontal" else "vertical",
              align = if (legend_position == "top") "center" else "right",
              verticalAlign = if (legend_position == "top") "top" else "middle",
              symbolHeight = 12,
              symbolWidth = 12,
              symbolRadius = 0,
              itemStyle = list(
                fontSize = "12px",
                fontWeight = "normal",
                color = "#666666"
              ),
              itemMarginTop = if (legend_position == "top") 5 else NULL) %>%
    hc_tooltip(shared = TRUE, valueDecimals = NA, backgroundColor = "#ffffffE6", borderColor = "#cccccc", borderRadius = 3, borderWidth = 1, padding = 8,
               formatter = if (toolbox_sum) JS("
    function() {
      var points = this.points;
      var sum = 0;
      points.forEach(function(point) {
        sum += point.y;
      });
      var s = '<span style=\"font-size: 14px;\">' + this.x + '</span>';  // Increase font size of the title without bold
      points.forEach(function(point) {
        s += '<br/>' + '<span style=\"color:' + point.color + '\">\u25CF</span> <span style=\"font-weight: normal;\">' + point.series.name + ':</span> <b>' + point.y + '</b>';
      });
      var sumFormatted = (sum % 1 === 0) ? sum : sum.toFixed(2);  // Check if sum is an integer; if not, format to 2 decimals
      s += '<br/><span style=\"font-weight: normal;\"><i>Summe:</i></span> <i><b>' + sumFormatted + '</b></i>';
      return s;
    }")
               else if (toolbox_mean) JS("
    function() {
      var points = this.points;
      var sum = 0;
      var count = points.length;
      points.forEach(function(point) {
        sum += point.y;
      });
      var avg = sum / count;  // Calculate average
      var s = '<span style=\"font-size: 14px;\">' + this.x + '</span>';  // Increase font size of the title without bold
      points.forEach(function(point) {
        s += '<br/>' + '<span style=\"color:' + point.color + '\">\u25CF</span> <span style=\"font-weight: normal;\">' + point.series.name + ':</span> <b>' + point.y + '</b>';
      });
      var avgFormatted = (avg % 1 === 0) ? avg : avg.toFixed(2);  // Check if average is an integer; if not, format to 2 decimals
      s += '<br/><span style=\"font-weight: normal;\"><i>Mittelwert:</i></span> <i><b>' + avgFormatted + '</b></i>';
      return s;
    }")
               else JS("
    function() {
      var s = '<span style=\"font-size: 14px;\">' + this.x + '</span>';  // Increase font size of the title without bold
      this.points.forEach(function(point) {
        s += '<br/>' + '<span style=\"color:' + point.color + '\">\u25CF</span> <span style=\"font-weight: normal;\">' + point.series.name + ':</span> <b>' + point.y + '</b>';
      });
      return s;
    }")
    ) %>%
    hc_exporting(
      enabled = TRUE,
      buttons = list(contextButton = list(
        symbolStroke = if (background_transparent) "#666666" else NULL,
        theme = list(
          fill = if (background_transparent) "transparent" else NULL,
          states = list(
            hover = list(
              fill = if (background_transparent) "transparent" else NULL
            ),
            select = list(
              fill = if (background_transparent) "transparent" else NULL
            )
          )
        )
      ))
    ) %>%
    hc_add_theme(
      hc_theme(
        colors = palette_colors,
        chart = list(style = list(fontFamily = "Arial")),
        title = list(style = list(fontSize = "16px", fontWeight = "bold", color = "#333333")),
        subtitle = list(style = list(fontSize = "12px", color = "#666666")),
        xAxis = list(gridLineWidth = 0, labels = list(style = list(fontSize = "12px", color = "#666666"))),
        yAxis = list(gridLineWidth = 0, gridLineColor = "#e0e0e0", labels = list(style = list(fontSize = "12px", color = "#666666"))),
        legend = list(itemStyle = list(fontSize = "12px", color = "#666666")),
        tooltip = list(backgroundColor = "#ffffff", borderColor = "#cccccc", borderRadius = 3, borderWidth = 1, padding = 8)
      )
    ) %>%
    hc_credits(enabled = TRUE, text = source_text, position = list(align = "left", x = 10, y = -5),
               style = list(fontSize = "10px", color = "#666666", fontFamily = "Arial"))

  return(hc)
}


# # Example usage with the iris dataset in long format
# mtcars_long <- mtcars %>%
#   pivot_longer(cols = -c(cyl, gear), names_to = "Measurement", values_to = "Value") %>%
#   group_by(cyl, gear, Measurement) %>%
#   summarise(Mean = mean(Value), .groups = 'drop')
#
# hc <- StatA.area(
#   mtcars_long,
#   group_col = "Measurement",
#   measure_col = "gear",
#   value_col = "cyl",
#   title = "Average Measurements of Iris Species",
#   subtitle = "Data from Fisher's Iris dataset",
#   x_axis_title = "Measurement Type",
#   y_axis_title = "Average Value",
#   legend_title = "Species",
#   palette = "Accent",
#   stacked = TRUE,
#   horizontal = FALSE,
#   source_text = "Source: Fisher's Iris dataset, lkdsfjdslkfjdsflkdsjfalkfjölkjsaödlfjsadölkfjdsaölfkjdsafölkdsajföldsakjföldsakjf",
#   background_transparent = TRUE,
#   legend_position = "top",
#   toolbox_sum = TRUE
# )
# hc


### StatA.dot ####################################################################################################
# Define the function for dot plots
StatA.dot <- function(data_long, group_col, measure_col, value_col, title = "Dot Plot", subtitle = NULL,
                      x_axis_title = "Measurements", y_axis_title = "Mean Value", legend_title = NULL,
                      palette = "viridis", horizontal = FALSE, source_text = "Source",
                      background_transparent = FALSE, legend_position = "right", toolbox_sum = FALSE, toolbox_mean = FALSE) {

  # Ensure only one of toolbox_sum or toolbox_mean is TRUE
  if (toolbox_sum && toolbox_mean) {
    stop("Only one of toolbox_sum or toolbox_mean can be TRUE.")
  }

  # Get unique groups and measurements
  unique_groups <- unique(data_long[[group_col]])
  unique_measurements <- unique(data_long[[measure_col]])

  # Determine the number of colors needed
  num_colors <- length(unique_groups)

  # Check if palette is a character vector (custom hex codes)
  if (is.character(palette) && length(palette) > 1) {
    if (length(palette) < num_colors) {
      stop("Not enough colors in the custom palette for the number of groups.")
    }
    palette_colors <- palette
  } else {
    # Get colors from the appropriate viridis palette
    if (num_colors <= 12) {
      palette_colors <- viridis::viridis(num_colors)
    } else {
      stop("This function supports up to 12 unique groups.")
    }
  }

  # Define xAxis and yAxis based on orientation
  xAxis <- list(categories = unique_measurements, title = list(text = x_axis_title, style = list(marginBottom = 40)), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0)
  yAxis <- list(min = 0, title = list(text = y_axis_title), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0, gridLineColor = "#e0e0e0")

  if (horizontal) {
    xAxis <- list(min = 0, title = list(text = y_axis_title), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0, gridLineColor = "#e0e0e0")
    yAxis <- list(categories = unique_measurements, title = list(text = x_axis_title, style = list(marginBottom = 40)), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0, tickLength = 0)
  }

  # Create the dot plot with a minimalist Datawrapper-like design
  hc <- highchart() %>%
    hc_chart(type = "scatter", zoomType = "xy", backgroundColor = if (background_transparent) "transparent" else NULL) %>%
    hc_title(text = title, style = list(fontSize = "16px", fontWeight = "normal", color = "#333333"), align = "center") %>%
    hc_subtitle(text = subtitle, style = list(fontSize = "12px", color = "#666666")) %>%
    hc_xAxis(categories = xAxis$categories, title = xAxis$title, labels = xAxis$labels, gridLineWidth = xAxis$gridLineWidth) %>%
    hc_yAxis(min = yAxis$min, title = yAxis$title, labels = yAxis$labels, gridLineWidth = yAxis$gridLineWidth, gridLineColor = yAxis$gridLineColor) %>%
    hc_plotOptions(scatter = list(marker = list(radius = 5, symbol = "circle"), dataLabels = list(enabled = FALSE), enableMouseTracking = TRUE))

  # Add series for each group
  for (i in seq_along(unique_groups)) {
    group_name <- unique_groups[i]
    hc <- hc %>%
      hc_add_series(
        data = data_long %>%
          filter(!!sym(group_col) == group_name) %>%
          arrange(!!sym(measure_col)) %>%
          pull(!!sym(value_col)),
        name = insert_line_breaks(group_name),
        color = palette_colors[i],
        type = "scatter"
      )
  }

  # Configure tooltip for each stack
  hc <- hc %>%
    hc_legend(title = list(text = legend_title, style = list(fontSize = "14px", fontWeight = "normal", color = "#666666")), layout = if (legend_position == "top") "horizontal" else "vertical",
              align = if (legend_position == "top") "center" else "right",
              verticalAlign = if (legend_position == "top") "top" else "middle",
              symbolHeight = 12,
              symbolWidth = 12,
              symbolRadius = 0,
              itemStyle = list(
                fontSize = "12px",
                fontWeight = "normal",
                color = "#666666"
              ),
              itemMarginTop = if (legend_position == "top") 5 else NULL) %>%
    hc_tooltip(shared = FALSE, useHTML = TRUE, pointFormatter = JS("
      function() {
        return '<b>' + this.series.name + '</b>: ' + this.y;
      }")) %>%
    hc_exporting(
      enabled = TRUE,
      buttons = list(contextButton = list(
        symbolStroke = if (background_transparent) "#666666" else NULL,
        theme = list(
          fill = if (background_transparent) "transparent" else NULL,
          states = list(
            hover = list(
              fill = if (background_transparent) "transparent" else NULL
            ),
            select = list(
              fill = if (background_transparent) "transparent" else NULL
            )
          )
        )
      ))
    ) %>%
    hc_add_theme(
      hc_theme(
        colors = palette_colors,
        chart = list(style = list(fontFamily = "Arial")),
        title = list(style = list(fontSize = "16px", fontWeight = "bold", color = "#333333")),
        subtitle = list(style = list(fontSize = "12px", color = "#666666")),
        xAxis = list(gridLineWidth = 0, labels = list(style = list(fontSize = "12px", color = "#666666"))),
        yAxis = list(gridLineColor = "#e0e0e0", labels = list(style = list(fontSize = "12px", color = "#666666"))),
        legend = list(itemStyle = list(fontSize = "12px", color = "#666666")),
        tooltip = list(backgroundColor = "#ffffff", borderColor = "#cccccc", borderRadius = 3, borderWidth = 1, padding = 8)
      )
    ) %>%
    hc_credits(enabled = TRUE, text = source_text, position = list(align = "left", x = 10, y = -5),
               style = list(fontSize = "10px", color = "#666666", fontFamily = "Arial"))

  return(hc)
}


# # Example usage with the iris dataset in long format
# mtcars_long <- mtcars %>%
#   pivot_longer(cols = -c(cyl, gear), names_to = "Measurement", values_to = "Value") %>%
#   group_by(cyl, gear, Measurement) %>%
#   summarise(Mean = mean(Value), .groups = 'drop')
#
# hc <- StatA.dot(
#   mtcars_long,
#   group_col = "Measurement",
#   measure_col = "gear",
#   value_col = "cyl",
#   title = "Average Measurements of Iris Species",
#   subtitle = "Data from Fisher's Iris dataset",
#   x_axis_title = "Measurement Type",
#   y_axis_title = "Average Value",
#   legend_title = "Species",
#   palette = "Accent",
#   stacked = TRUE,
#   horizontal = FALSE,
#   source_text = "Source: Fisher's Iris dataset, lkdsfjdslkfjdsflkdsjfalkfjölkjsaödlfjsadölkfjdsaölfkjdsafölkdsajföldsakjföldsakjf",
#   background_transparent = TRUE,
#   legend_position = "top",
#   toolbox_sum = TRUE
# )
# hc


### StatA.population ####################################################################################################

# Helper function to insert line breaks in long labels
insert_line_breaks <- function(label, max_length = 20) {
  label <- as.character(label) # Ensure the label is a character string
  words <- strsplit(label, " ")[[1]]
  lines <- character()
  current_line <- ""

  for (word in words) {
    if (nchar(paste(current_line, word, sep = " ")) > max_length) {
      lines <- c(lines, current_line)
      current_line <- word
    } else {
      current_line <- paste(current_line, word, sep = " ")
    }
  }
  lines <- c(lines, current_line)
  return(paste(lines, collapse = "<br/>"))
}

# Define the StatA.population function
StatA.population <- function(data_long, group_col, subgroup_col, x_Axis, y_Axis, title = "Population Pyramid", subtitle = NULL,
                             x_axis_title = "Population", y_axis_title = "Age Group", legend_title = NULL,
                             palette = "viridis", background_transparent = FALSE, legend_position = "right", toolbox_sum = FALSE, toolbox_mean = FALSE,
                             source_text = "Source") {

  # Ensure only one of toolbox_sum or toolbox_mean is TRUE
  if (toolbox_sum && toolbox_mean) {
    stop("Only one of toolbox_sum or toolbox_mean can be TRUE.")
  }

  # Prepare data for the pyramid plot
  data_long <- data_long %>%
    mutate(Values = ifelse(!!sym(group_col) == "Male", -!!sym(x_Axis), !!sym(x_Axis)))  # Make male values negative for left-side bars

  # Get unique age groups and subcategories
  unique_age_groups <- unique(data_long[[y_Axis]])
  unique_subcategories <- unique(data_long[[subgroup_col]])

  # Determine the number of colors needed
  num_colors <- length(unique_subcategories)

  # Check if palette is a character vector (custom hex codes)
  if (is.character(palette) && length(palette) > 1) {
    if (length(palette) < num_colors) {
      stop("Not enough colors in the custom palette for the number of subcategories.")
    }
    palette_colors <- palette
  } else {
    # Get colors from the appropriate viridis palette
    if (num_colors <= 12) {
      palette_colors <- viridis::viridis(num_colors)
    } else {
      stop("This function supports up to 12 unique subcategories.")
    }
  }

  # Create the population pyramid with a minimalist Datawrapper-like design
  hc <- highchart() %>%
    hc_chart(type = "bar", zoomType = "xy", backgroundColor = if (background_transparent) "transparent" else NULL) %>%
    hc_title(text = title, style = list(fontSize = "16px", fontWeight = "normal", color = "#333333"), align = "center") %>%
    hc_subtitle(text = subtitle, style = list(fontSize = "12px", color = "#666666")) %>%
    hc_xAxis(title = list(text = x_axis_title), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0) %>%
    hc_yAxis(categories = unique_age_groups, title = list(text = y_axis_title, style = list(marginBottom = 40)), labels = list(style = list(fontSize = "12px", color = "#666666")), gridLineWidth = 0, gridLineColor = "#e0e0e0") %>%
    hc_plotOptions(series = list(stacking = "normal", borderWidth = 0, dataLabels = list(enabled = FALSE),
                                 groupPadding = 0.1, pointPadding = 0.05))  # Adjust padding to control spacing between bars

  # Add series for each subcategory (e.g., work and no work)
  for (i in seq_along(unique_subcategories)) {
    subcategory_name <- unique_subcategories[i]
    hc <- hc %>%
      hc_add_series(
        data = data_long %>%
          filter(!!sym(subgroup_col) == subcategory_name) %>%
          arrange(!!sym(y_Axis)) %>%
          pull(!!sym(x_Axis)),
        name = insert_line_breaks(subcategory_name),
        stack = "stack",
        color = palette_colors[i]
      )
  }

  hc <- hc %>%
    hc_legend(title = list(text = legend_title, style = list(fontSize = "14px", fontWeight = "normal", color = "#666666")), layout = if (legend_position == "top") "horizontal" else "vertical",
              align = if (legend_position == "top") "center" else "right",
              verticalAlign = if (legend_position == "top") "top" else "middle",
              symbolHeight = 12,
              symbolWidth = 12,
              symbolRadius = 0,
              itemStyle = list(
                fontSize = "12px",
                fontWeight = "normal",
                color = "#666666"
              ),
              itemMarginTop = if (legend_position == "top") 5 else NULL) %>%
    hc_tooltip(shared = TRUE, valueDecimals = NA, backgroundColor = "#ffffffE6", borderColor = "#cccccc", borderRadius = 3, borderWidth = 1, padding = 8,
               formatter = if (toolbox_sum) JS("
    function() {
      var points = this.points;
      var sum = 0;
      points.forEach(function(point) {
        sum += Math.abs(point.y);
      });
      var s = '<span style=\"font-size: 14px;\">' + this.x + '</span>';  // Increase font size of the title without bold
      points.forEach(function(point) {
        s += '<br/>' + '<span style=\"color:' + point.color + '\">\u25CF</span> <span style=\"font-weight: normal;\">' + point.series.name + ':</span> <b>' + Math.abs(point.y) + '</b>';
      });
      var sumFormatted = (sum % 1 === 0) ? sum : sum.toFixed(2);  // Check if sum is an integer; if not, format to 2 decimals
      s += '<br/><span style=\"font-weight: normal;\"><i>Summe:</i></span> <i><b>' + sumFormatted + '</b></i>';
      return s;
    }")
               else if (toolbox_mean) JS("
    function() {
      var points = this.points;
      var sum = 0;
      var count = points.length;
      points.forEach(function(point) {
        sum += Math.abs(point.y);
      });
      var avg = sum / count;  // Calculate average
      var s = '<span style=\"font-size: 14px;\">' + this.x + '</span>';  // Increase font size of the title without bold
      points.forEach(function(point) {
        s += '<br/>' + '<span style=\"color:' + point.color + '\">\u25CF</span> <span style=\"font-weight: normal;\">' + point.series.name + ':</span> <b>' + Math.abs(point.y) + '</b>';
      });
      var avgFormatted = (avg % 1 === 0) ? avg : avg.toFixed(2);  // Check if average is an integer; if not, format to 2 decimals
      s += '<br/><span style=\"font-weight: normal;\"><i>Mittelwert:</i></span> <i><b>' + avgFormatted + '</b></i>';
      return s;
    }")
               else JS("
    function() {
      var s = '<span style=\"font-size: 14px;\">' + this.x + '</span>';  // Increase font size of the title without bold
      this.points.forEach(function(point) {
        s += '<br/>' + '<span style=\"color:' + point.color + '\">\u25CF</span> <span style=\"font-weight: normal;\">' + point.series.name + ':</span> <b>' + Math.abs(point.y) + '</b>';
      });
      return s;
    }")
    ) %>%
    hc_exporting(
      enabled = TRUE,
      buttons = list(contextButton = list(
        symbolStroke = if (background_transparent) "#666666" else NULL,
        theme = list(
          fill = if (background_transparent) "transparent" else NULL,
          states = list(
            hover = list(
              fill = if (background_transparent) "transparent" else NULL
            ),
            select = list(
              fill = if (background_transparent) "transparent" else NULL
            )
          )
        )
      ))
    ) %>%
    hc_add_theme(
      hc_theme(
        colors = palette_colors,
        chart = list(style = list(fontFamily = "Arial")),
        title = list(style = list(fontSize = "16px", fontWeight = "bold", color = "#333333")),
        subtitle = list(style = list(fontSize = "12px", color = "#666666")),
        xAxis = list(gridLineWidth = 0, labels = list(style = list(fontSize = "12px", color = "#666666"))),
        yAxis = list(gridLineWidth = 0, gridLineColor = "#e0e0e0", labels = list(style = list(fontSize = "12px", color = "#666666"))),
        legend = list(itemStyle = list(fontSize = "12px", color = "#666666")),
        tooltip = list(backgroundColor = "#ffffff", borderColor = "#cccccc", borderRadius = 3, borderWidth = 1, padding = 8)
      )
    ) %>%
    hc_credits(enabled = TRUE, text = source_text, position = list(align = "left", x = 10, y = -5),
               style = list(fontSize = "10px", color = "#666666", fontFamily = "Arial"))  # Adjusted style to match axis labels

  return(hc)
}


# # read_data:
# population_table_above <- openxlsx::read.xlsx("//filebsuser.bs.ch/PD-Privat/u2trba/Benjamin_Marti_Dokumente/Testdaten/Pop.xlsx")
#
#
#
# # Example usage of StatA.population function
# hc <- StatA.population(
#   population_table_above,
#   group_col = "Gender",
#   subgroup_col = "Subcategory",
#   x_Axis = "Values",
#   y_Axis = "AgeGroup",
#   title = "Population Pyramid",
#   subtitle = "Hypothetical Population Data",
#   x_axis_title = "Population",
#   y_axis_title = "Age Group",
#   legend_title = "Sub category",
#   palette = "Accent",
#   background_transparent = TRUE,
#   legend_position = "top",
#   toolbox_sum = TRUE,
#   toolbox_mean = FALSE,
#   source_text = "Source: Hypothetical Data"
# )
# hc


### StatA.area_with_line #########################################################################################
# Helper function to insert line breaks in long labels
insert_line_breaks <- function(label, max_length = 20) {
  words <- strsplit(as.character(label), " ")[[1]]
  lines <- character()
  current_line <- ""

  for (word in words) {
    if (nchar(paste(current_line, word)) > max_length) {
      lines <- c(lines, current_line)
      current_line <- word
    } else {
      current_line <- paste(current_line, word)
    }
  }
  lines <- c(lines, current_line)
  return(paste(lines, collapse = "<br/>"))
}

# Function for creating the customized highcharter area and line plot
StatA.area_with_line <- function(data, date_col,
                                 value_col_area, group_col_area,
                                 value_col_line, group_col_line,
                                 title = "Area and Line Chart", subtitle = NULL,
                                 x_axis_title = NULL, y_axis_title = NULL, legend_title = NULL,
                                 palette = "viridis", stacked = FALSE, source_text = "Source",
                                 background_transparent = FALSE, legend_position = "right") {

  # Get unique groups for area and line plots, and determine the number of colors needed
  unique_groups_area <- unique(data[[group_col_area]])
  unique_groups_line <- unique(data[[group_col_line]])
  num_colors_area <- length(unique_groups_area)
  num_colors_line <- length(unique_groups_line)

  # Handle color palette
  palette_colors_area <- if (is.character(palette) && length(palette) >= num_colors_area) {
    palette[1:num_colors_area]
  } else {
    viridis::viridis(num_colors_area)
  }

  palette_colors_line <- if (is.character(palette) && length(palette) >= num_colors_line) {
    palette[(num_colors_area + 1):(num_colors_area + num_colors_line)]
  } else {
    viridis::viridis(num_colors_line)
  }

  # Create the chart
  hc <- highchart() %>%
    hc_chart(type = "area", zoomType = "xy", backgroundColor = if (background_transparent) "transparent" else NULL) %>%
    hc_title(text = title, style = list(fontSize = "16px", fontWeight = "normal", color = "#333333"), align = "center") %>%
    hc_subtitle(text = subtitle, style = list(fontSize = "12px", color = "#666666")) %>%
    hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = '%b %Y'),
             title = list(text = x_axis_title, style = list(marginBottom = 40)),
             labels = list(style = list(fontSize = "12px", color = "#666666"))) %>%
    hc_yAxis(title = list(text = y_axis_title), labels = list(style = list(fontSize = "12px", color = "#666666")),
             gridLineWidth = 0, gridLineColor = "#e0e0e0") %>%
    hc_plotOptions(series = list(stacking = if (stacked) "normal" else NULL, borderWidth = 0,
                                 dataLabels = list(enabled = FALSE), groupPadding = 0.1, pointPadding = 0.05)) %>%
    hc_legend(title = list(text = legend_title, style = list(fontSize = "14px", fontWeight = "normal", color = "#666666")),
              layout = if (legend_position == "top") "horizontal" else "vertical",
              align = if (legend_position == "top") "center" else "right",
              verticalAlign = if (legend_position == "top") "top" else "middle",
              itemStyle = list(fontSize = "12px", fontWeight = "normal", color = "#666666"),
              itemMarginTop = if (legend_position == "top") 5 else NULL) %>%
    hc_tooltip(shared = TRUE, valueDecimals = NA, backgroundColor = "#ffffffE6",
               borderColor = "#cccccc", borderRadius = 3, borderWidth = 1, padding = 8) %>%
    hc_add_theme(hc_theme(colors = palette_colors_area, chart = list(style = list(fontFamily = "Arial")),
                          title = list(style = list(fontSize = "16px", fontWeight = "bold", color = "#333333")),
                          subtitle = list(style = list(fontSize = "12px", color = "#666666")),
                          xAxis = list(gridLineWidth = 0, labels = list(style = list(fontSize = "12px", color = "#666666"))),
                          yAxis = list(gridLineWidth = 0, gridLineColor = "#e0e0e0",
                                       labels = list(style = list(fontSize = "12px", color = "#666666"))),
                          legend = list(itemStyle = list(fontSize = "12px", color = "#666666")),
                          tooltip = list(backgroundColor = "#ffffff", borderColor = "#cccccc",
                                         borderRadius = 3, borderWidth = 1, padding = 8))) %>%
    hc_credits(enabled = TRUE, text = source_text, position = list(align = "left", x = 10, y = -5),
               style = list(fontSize = "10px", color = "#666666", fontFamily = "Arial"))

  # Add area series for each group in category1
  for (i in seq_along(unique_groups_area)) {
    group_name <- unique_groups_area[i]
    group_data <- data %>% filter(!!sym(group_col_area) == group_name)

    hc <- hc %>%
      hc_add_series(data = group_data, type = "area",
                    hcaes(x = !!sym(date_col), y = !!sym(value_col_area)),
                    name = insert_line_breaks(group_name), color = palette_colors_area[i])
  }

  # Add line series for each group in category2
  for (i in seq_along(unique_groups_line)) {
    group_name <- unique_groups_line[i]
    group_data <- data %>% filter(!!sym(group_col_line) == group_name)

    hc <- hc %>%
      hc_add_series(data = group_data, type = "line",
                    hcaes(x = !!sym(date_col), y = !!sym(value_col_line)),
                    name = insert_line_breaks(group_name), color = palette_colors_line[i])
  }

  return(hc)
}


# # Example usage with the iris dataset in long format
# mtcars_long <- mtcars %>%
#   pivot_longer(cols = -c(cyl, gear), names_to = "Measurement", values_to = "Value") %>%
#   group_by(cyl, gear, Measurement) %>%
#   summarise(Mean = mean(Value), .groups = 'drop')
#
# hc <- StatA.bar(
#   mtcars_long,
#   group_col = "Measurement",
#   measure_col = "gear",
#   value_col = "cyl",
#   title = "Average Measurements of Iris Species",
#   subtitle = "Data from Fisher's Iris dataset",
#   x_axis_title = "Measurement Type",
#   y_axis_title = "Average Value",
#   legend_title = "Species",
#   palette = "Accent",
#   stacked = TRUE,
#   horizontal = FALSE,
#   source_text = "Source: Fisher's Iris dataset, lkdsfjdslkfjdsflkdsjfalkfjölkjsaödlfjsadölkfjdsaölfkjdsafölkdsajföldsakjföldsakjf",
#   background_transparent = TRUE,
#   legend_position = "top",
#   toolbox_sum = TRUE
# )
# hc
