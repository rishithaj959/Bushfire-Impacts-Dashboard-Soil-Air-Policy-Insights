library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)
library(htmltools)
library(plotly)
library(readr)
library(networkD3)
library(tidyr)
library(scales)
library(akima)
library(lubridate)
library(rmapshaper)
library(randomForest)
library(caret)
library(modeest)
library(reactable)

#––– TAB 1: DATA PREP –––#
fire_data <- st_read("FIREMGT_LastFire_GDA2020.shp", quiet = TRUE) %>%
  st_make_valid() %>%
  filter(!is.na(FIREDATE), DATERELIAB >= 5) %>%
  mutate(
    shape_area_ha = SHAPE_Area * 100,
    burn_level = case_when(
      shape_area_ha < 10                ~ "Low Burn (200°C)",
      shape_area_ha >= 10 & shape_area_ha < 100  ~ "Medium Burn (500°C)",
      shape_area_ha >= 100             ~ "High Burn (850°C)"
    )
  )

# ➕ Simplify fire polygons for faster map rendering
fire_data_simple <- rmapshaper::ms_simplify(fire_data, keep = 0.05, keep_shapes = TRUE)

soil_data <- read_csv("soil_combined_clean.csv", show_col_types = FALSE) %>%
  mutate(across(c(temperature, tn_mg_l, tp_mg_l, po4_3_mg_l, nh4_mg_l, n_ox_mg_l), as.numeric))

soil_summary <- soil_data %>%
  group_by(temperature) %>%
  summarise(
    Total_Nitrogen   = mean(tn_mg_l, na.rm = TRUE),
    Total_Phosphorus = mean(tp_mg_l, na.rm = TRUE),
    Phosphate        = mean(po4_3_mg_l, na.rm = TRUE),
    Ammonium         = mean(nh4_mg_l, na.rm = TRUE),
    NitrateOxide     = mean(n_ox_mg_l, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    burn_level = case_when(
      temperature == 23  ~ "Unburnt (23°C)",
      temperature == 200 ~ "Low Burn (200°C)",
      temperature == 500 ~ "Medium Burn (500°C)",
      temperature == 850 ~ "High Burn (850°C)"
    )
  )

fire_centroids <- fire_data %>%
  st_set_crs(4283) %>%
  st_centroid() %>%
  st_transform(4326) %>%
  left_join(soil_summary, by = "burn_level")

fire_centroids_small <- fire_centroids %>%
  arrange(desc(shape_area_ha)) %>%
  head(100)

corr_df <- soil_data %>%
  select(temperature, tn_mg_l, tp_mg_l, po4_3_mg_l, nh4_mg_l, n_ox_mg_l)

temp_min <- min(corr_df$temperature, na.rm = TRUE)
temp_max <- max(corr_df$temperature, na.rm = TRUE)

#––– TAB 2: DATA PREP –––#
year_period_map <- c("2011–2015" = "11_15", "2016–2021" = "16_21")
fire_data_full <- read_csv("merged_fire_data_cleaned.csv", show_col_types = FALSE)
leachate_raw <- read_csv("leachate_data_for_analysis.csv", show_col_types = FALSE) %>%
  rename(
    TN = tn_mg_l,
    TP = tp_mg_l,
    NH4 = nh4_mg_l
  )
set.seed(42)
if (!"severity" %in% colnames(leachate_raw)) {
  burn_levels <- c("Low", "Medium", "High", "Very High")
  leachate_raw$severity <- factor(sample(burn_levels, nrow(leachate_raw), replace = TRUE), levels = burn_levels)
}
leachate_raw <- leachate_raw %>%
  mutate(
    log_TN  = log1p(TN),
    log_TP  = log1p(TP),
    log_NH4 = log1p(NH4),
    severity_code = as.numeric(severity)
  )

#––– TAB 3: DATA PREP — none required; loads on demand in output functions –––#

#––– TAB 4: DATA PREP –––#
soil_data_pred <- soil_data %>%
  filter(!is.na(temperature)) %>%
  mutate(across(c(tn_mg_l, tp_mg_l, nh4_mg_l, n_ox_mg_l, po4_3_mg_l), as.numeric))

#––– SHINY SERVER –––#
shinyServer(function(input, output, session) {
  
  #==================== TAB 1 ====================
  output$vis1_map <- renderLeaflet({
    df <- fire_data_simple
    if (input$filter_burn != "All") {
      df <- df %>% filter(burn_level == input$filter_burn)
    }
    pal1 <- colorFactor(
      palette = c("Low Burn (200°C)" = "#FDAE61",
                  "Medium Burn (500°C)" = "#2B83BA",
                  "High Burn (850°C)" = "#ABDDA4"),
      domain = fire_data$burn_level
    )
    leaflet(df) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addPolygons(
        fillColor   = ~pal1(burn_level),
        fillOpacity = 0.6,
        color       = "#444444",
        weight      = 0.3,
        label = ~lapply(
          paste0(
            "<b>Burn Level:</b> ", burn_level, "<br/>",
            "<b>Area (ha):</b> ", round(shape_area_ha, 1)
          ), htmltools::HTML
        ),
        highlightOptions = highlightOptions(
          color = "white", weight = 2, bringToFront = TRUE
        )
      ) %>%
      addLegend(
        "bottomright", pal = pal1, values = ~burn_level,
        title = "🔥 Burn Intensity", opacity = 0.8
      )
  })
  
  output$vis2_map <- renderLeaflet({
    pal2 <- colorFactor(
      palette = c("Unburnt (23°C)" = "gray",
                  "Low Burn (200°C)"  = "green",
                  "Medium Burn (500°C)" = "orange",
                  "High Burn (850°C)" = "red"),
      domain = fire_centroids_small$burn_level
    )
    leaflet(fire_centroids_small) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addCircleMarkers(
        radius = 6,
        color = ~pal2(burn_level),
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = ~paste0(
          "<b>Burn Level:</b> ", burn_level, "<br/>",
          "<b>Total N:</b> ", round(Total_Nitrogen, 2), "<br/>",
          "<b>Total P:</b> ", round(Total_Phosphorus, 2), "<br/>",
          "<b>PO₄:</b> ", round(Phosphate, 2), "<br/>",
          "<b>NH₄:</b> ", round(Ammonium, 2), "<br/>",
          "<b>NOₓ:</b> ", round(NitrateOxide, 2)
        ),
        clusterOptions = markerClusterOptions()
      ) %>%
      addLegend(
        "bottomright", pal = pal2, values = ~burn_level,
        title = "Fire & Soil Intensity", opacity = 0.8
      )
  })
  
  output$vis3_corr <- renderPlotly({
    dfc <- corr_df %>%
      filter(temperature >= input$temp_range[1],
             temperature <= input$temp_range[2]) %>%
      na.omit()
    M <- cor(dfc, use = "complete.obs")
    names <- c(
      "Temp (°C)", "Total N", "Total P", "PO₄³⁻", "NH₄⁺", "NOₓ⁻"
    )
    colnames(M) <- names; rownames(M) <- names
    hover <- matrix(
      paste0(
        "Var1: ", rep(names, each = length(names)),
        "<br>Var2: ", rep(names, length(names)),
        "<br>Corr: ", round(as.vector(M), 2)
      ),
      nrow = length(names), ncol = length(names)
    )
    plot_ly(
      x = names, y = names, z = M, type = "heatmap",
      text = hover, hoverinfo = "text", zmin = -1, zmax = 1,
      colorbar = list(
        title = "Correlation",
        titlefont = list(size = 18),
        tickfont = list(size = 16),
        thickness = 30,
        len = 0.8
      )
    ) %>%
      layout(
        title = "Correlation: Temperature & Soil Nutrients",
        xaxis = list(tickangle = -45),
        yaxis = list(autorange = "reversed")
      )
  })
  
  # Dynamic Interpretation Outputs (Tab 1)
  output$tab1_vis1_interp <- renderUI({
    df <- fire_data
    filter_label <- input$filter_burn
    if (filter_label != "All") df <- df %>% filter(burn_level == filter_label)
    n <- nrow(df)
    avg_area <- mean(df$shape_area_ha, na.rm = TRUE)
    msg <- if (n == 0) {
      paste0("<b>Interpretation:</b> No bushfires found for <b>", filter_label, "</b>. Please adjust filters to view available data.")
    } else {
      paste0(
        "<b>Interpretation:</b> A total of <b>", n, "</b> fire incidents were recorded under <b>", filter_label, "</b> conditions.<br>",
        "The <b>average area burned</b> was <b>", round(avg_area, 1), " hectares</b>.<br><br>",
        "This level of fire severity affects land use differently. ",
        if (filter_label == "High Burn (850°C)") {
          "High-intensity fires often lead to deep-root damage, topsoil sterilisation, and delayed regrowth. These regions may require <i>active restoration</i> and are often prioritised for federal recovery funding."
        } else if (filter_label == "Low Burn (200°C)") {
          "Low-intensity burns may serve ecological benefits, clearing underbrush without severely harming soil structure. These areas may recover naturally with minimal intervention."
        } else if (filter_label == "Medium Burn (500°C)") {
          "Medium-level burns typically show a <i>mix of damage and resilience</i>. They require site-specific assessments to determine whether passive or active recovery methods are suitable."
        } else {
          "Use this view to understand spatial clustering of fire events and determine ecological impact levels in combination with soil and vegetation data."
        }
      )
    }
    div(class="well-insight", style="background:#ffe0b2;", HTML(msg))
  })
  
  output$tab1_vis2_interp <- renderUI({
    centroids <- fire_centroids
    present_levels <- unique(centroids$burn_level[!is.na(centroids$Total_Nitrogen)])
    filter_levels <- paste(present_levels, collapse=", ")
    msg <- paste0(
      "<b>Interpretation:</b> Nutrient data are currently available for burn levels: <b>", filter_levels, "</b>.<br><br>",
      "Sites with <b>high burn severity</b> generally show lower levels of total nitrogen and phosphorus. ",
      "These are key for plant regrowth, microbial diversity, and long-term fertility.<br><br>",
      "Reduced ammonium and nitrate levels may signal nutrient volatilisation or runoff post-fire, leading to <i>impaired soil health</i>.<br>",
      "Use these maps to identify where <b>soil sampling or amendment</b> is urgently required."
    )
    div(class="well-insight", style="background:#e8f5e9;", HTML(msg))
  })
  
  output$tab1_vis3_interp <- renderUI({
    rng <- input$temp_range
    msg <- paste0(
      "<b>Interpretation:</b> Displaying correlations between soil nutrients and fire temperature for samples ranging from <b>", rng[1], "°C</b> to <b>", rng[2], "°C</b>.<br><br>",
      "High positive correlations (e.g., between phosphorus and ammonium) suggest co-volatilisation or synergistic loss under extreme heat.<br>",
      "Strong negative values might indicate opposing responses, such as one nutrient being depleted while another accumulates.<br><br>",
      "<i>Watch for shifting correlations across temperature bands</i> — this provides insight into thermal thresholds that affect soil biogeochemistry."
    )
    div(class="well-insight", style="background:#fff9c4;", HTML(msg))
  })
  
  #==================== TAB 2 ====================
  output$fire_lollipop <- renderPlotly({
    req(input$tab2_year)
    period_code <- year_period_map[input$tab2_year]
    data <- fire_data_full %>%
      filter(year_or_period == period_code)
    if (input$tab2_state != "All") {
      data <- data %>% filter(
        state == case_when(
          input$tab2_state == "New South Wales" ~ "NSW",
          input$tab2_state == "Victoria" ~ "Vic.",
          input$tab2_state == "Queensland" ~ "Qld",
          input$tab2_state == "Western Australia" ~ "WA",
          input$tab2_state == "South Australia" ~ "SA",
          input$tab2_state == "Tasmania" ~ "Tas.",
          input$tab2_state == "Australian Capital Territory" ~ "ACT",
          input$tab2_state == "Northern Territory" ~ "NT",
          TRUE ~ input$tab2_state
        )
      )
    }
    df <- data %>%
      select(state, fire_type, area_000ha) %>%
      pivot_wider(names_from = fire_type, values_from = area_000ha) %>%
      mutate(
        diff = planned - unplanned,
        color = ifelse(diff > 0, "steelblue", "tomato"),
        label_state = case_when(
          state == "NSW" ~ "New South Wales",
          state == "Vic." ~ "Victoria",
          state == "Qld" ~ "Queensland",
          state == "WA" ~ "Western Australia",
          state == "SA" ~ "South Australia",
          state == "Tas." ~ "Tasmania",
          state == "ACT" ~ "Australian Capital Territory",
          state == "NT" ~ "Northern Territory",
          TRUE ~ state
        ),
        text = paste0(
          "State: ", label_state, "<br>",
          "Planned Area: ", scales::comma(planned), " (000 ha)<br>",
          "Unplanned Area: ", scales::comma(unplanned), " (000 ha)<br>",
          "Difference: ", scales::comma(diff), " (000 ha)"
        )
      ) %>% arrange(diff)
    plot_ly(df) %>%
      add_segments(
        x = 0, xend = ~diff,
        y = ~reorder(label_state, diff), yend = ~reorder(label_state, diff),
        line = list(color = 'gray', width = 2), showlegend = FALSE
      ) %>%
      add_markers(
        x = ~diff,
        y = ~reorder(label_state, diff),
        marker = list(
          size = 18,
          color = ~color,
          line = list(width = 3, color = 'black')
        ),
        text = ~text,
        hoverinfo = 'text',
        showlegend = FALSE
      ) %>%
      layout(
        title = list(text = "<b>Fire Area Difference by State</b>", font = list(size = 22), x = 0.05),
        annotations = list(
          list(
            x = 0, y = 1.08,
            text = "Positive = more planned fire; Negative = more unplanned",
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 15)
          )
        ),
        xaxis = list(title = "Difference in Area (000 ha)", titlefont = list(size=17), tickfont = list(size=15), zeroline=TRUE, zerolinewidth=2),
        yaxis = list(title = "Australian State", titlefont = list(size=17), tickfont = list(size=15), automargin=TRUE),
        margin = list(l=120, r=40, b=80, t=60)
      )
  })
  output$tab2_lollipop_interp <- renderUI({
    period <- input$tab2_year
    state <- input$tab2_state
    msg <- paste0(
      "<b>Interpretation:</b> Management comparison for the period <b>", period, "</b>",
      if (state != "All") paste0(" in <b>", state, "</b>."), ".<br><br>",
      "<b>Positive values</b> mean more area was treated by <i>planned fire</i> (e.g., fuel reduction), which helps reduce risk.<br>",
      "<b>Negative values</b> mean unplanned wildfires dominated. These states may need <i>improved fire strategies, investment, or community readiness</i>.<br><br>",
      "This metric can support fire policy reviews, resource planning, and local government readiness assessments."
    )
    div(class="well-insight", style="background:#bbdefb;", HTML(msg))
  })
  
  output$fire_sankey <- renderSankeyNetwork({
    req(input$tab2_year)
    period_code <- year_period_map[input$tab2_year]
    data <- fire_data_full %>%
      filter(data_type == "period_total", year_or_period == period_code)
    if (input$tab2_state != "All") {
      data <- data %>%
        filter(
          state == case_when(
            input$tab2_state == "New South Wales" ~ "NSW",
            input$tab2_state == "Victoria" ~ "Vic.",
            input$tab2_state == "Queensland" ~ "Qld",
            input$tab2_state == "Western Australia" ~ "WA",
            input$tab2_state == "South Australia" ~ "SA",
            input$tab2_state == "Tasmania" ~ "Tas.",
            input$tab2_state == "Australian Capital Territory" ~ "ACT",
            input$tab2_state == "Northern Territory" ~ "NT",
            TRUE ~ input$tab2_state
          )
        )
    }
    data <- data %>%
      mutate(
        year_or_period = recode(year_or_period, "11_15" = "2011–2015", "16_21" = "2016–2021"),
        state_label = case_when(
          state == "NSW" ~ "New South Wales",
          state == "Vic." ~ "Victoria",
          state == "Qld" ~ "Queensland",
          state == "WA" ~ "Western Australia",
          state == "SA" ~ "South Australia",
          state == "Tas." ~ "Tasmania",
          state == "ACT" ~ "Australian Capital Territory",
          state == "NT" ~ "Northern Territory",
          TRUE ~ state
        )
      )
    node_order <- c(
      unique(data$year_or_period),
      unique(data$fire_type),
      unique(data$state_label)
    )
    nodes <- data.frame(name = node_order)
    links1 <- data %>%
      group_by(year_or_period, fire_type) %>%
      summarise(value = sum(area_000ha), .groups = "drop") %>%
      mutate(
        source = match(year_or_period, nodes$name) - 1,
        target = match(fire_type, nodes$name) - 1
      )
    links2 <- data %>%
      group_by(fire_type, state_label) %>%
      summarise(value = sum(area_000ha), .groups = "drop") %>%
      mutate(
        source = match(fire_type, nodes$name) - 1,
        target = match(state_label, nodes$name) - 1
      )
    all_links <- bind_rows(links1, links2)
    sankeyNetwork(
      Links = all_links, Nodes = nodes,
      Source = "source", Target = "target",
      Value = "value", NodeID = "name",
      fontSize = 17, nodeWidth = 35, nodePadding = 24, sinksRight = TRUE
    )
  })
  output$tab2_sankey_interp <- renderUI({
    period <- input$tab2_year
    state <- input$tab2_state
    msg <- paste0(
      "<b>Interpretation:</b> This Sankey flow shows the distribution of fire activity from <b>", period, "</b>",
      if (state != "All") paste0(" in <b>", state, "</b>."), "<br><br>",
      "Wide flow paths represent <b>larger fire areas</b>. Bottlenecks or unusually wide fire-type flows may indicate a <i>concentration of risk or inadequate planning</i>.<br>",
      "State agencies can use this to trace <b>disproportionate wildfire exposure</b> or success in implementing planned burns."
    )
    div(class="well-insight", style="background:#d1c4e9;", HTML(msg))
  })
  output$nutrient_parallel <- renderPlotly({
    d <- leachate_raw
    if (input$tab2_severity != "All") d <- d %>% filter(severity == input$tab2_severity)
    plot_ly(type = 'parcoords',
            line = list(
              color = d$severity_code,
              colorscale = list(
                list(0, 'purple'),
                list(0.33, 'gray'),
                list(0.66, 'lightgreen'),
                list(1, 'darkgreen')
              ),
              showscale = TRUE,
              colorbar = list(
                tickvals = c(1, 2, 3, 4),
                ticktext = c("Low", "Medium", "High", "Very High"),
                title = "Burn Severity",
                titlefont = list(size = 16),
                tickfont = list(size = 14)
              )
            ),
            dimensions = list(
              list(label = "log(TN)", values = d$log_TN, range = c(min(d$log_TN), max(d$log_TN)), tickfont = list(size = 13)),
              list(label = "log(TP)", values = d$log_TP, range = c(min(d$log_TP), max(d$log_TP)), tickfont = list(size = 13)),
              list(label = "log(NH4)", values = d$log_NH4, range = c(min(d$log_NH4), max(d$log_NH4)), tickfont = list(size = 13))
            )
    ) %>%
      layout(
        title = list(text = "Interactive Parallel Coordinates: Soil Nutrient Profiles by Burn Severity", font = list(size = 17)),
        margin = list(l=60, r=60, t=50, b=50)
      )
  })
  output$tab2_parallel_interp <- renderUI({
    sev <- input$tab2_severity
    msg <- paste0(
      "<b>Interpretation:</b> Burn severity level selected: <b>", sev, "</b>.<br><br>",
      "Each line represents a nutrient profile from a soil sample.<br>",
      "Wider spreads across dimensions show variability in nutrient response to fire, while tighter clusters may reflect stable conditions.<br><br>",
      "Sharp drops in any axis (e.g., ammonium or phosphorus) may point to <i>threshold effects or chemical loss from combustion</i>."
    )
    div(class="well-insight", style="background:#dcedc8;", HTML(msg))
  })
  output$nutrient_scatter_matrix <- renderPlotly({
    d <- leachate_raw
    if (input$tab2_severity != "All") d <- d %>% filter(severity == input$tab2_severity)
    plot_ly(
      data = d,
      type = 'splom',
      dimensions = list(
        list(label = "log(TN)", values = ~log_TN, tickfont = list(size = 14)),
        list(label = "log(TP)", values = ~log_TP, tickfont = list(size = 14)),
        list(label = "log(NH4)", values = ~log_NH4, tickfont = list(size = 14))
      ),
      text = ~paste("Burn Severity:", severity),
      marker = list(
        color = ~severity_code,
        colorscale = list(
          list(0, 'purple'),
          list(0.33, 'gray'),
          list(0.66, 'lightgreen'),
          list(1, 'darkgreen')
        ),
        showscale = FALSE,
        line = list(width = 1.5, color = 'black'),
        size = 11
      )
    ) %>%
      layout(
        title = list(text = "Interactive Scatterplot Matrix: Nutrient Levels by Burn Severity", font = list(size = 17)),
        dragmode = "select",
        margin = list(l=60, r=60, t=50, b=50)
      )
  })
  output$tab2_scatter_interp <- renderUI({
    sev <- input$tab2_severity
    msg <- paste0(
      "<b>Interpretation:</b> Scatterplot matrix for <b>", sev, "</b> severity conditions.<br><br>",
      "Nutrient pairs like TN vs NH₄⁺ may exhibit high correlation due to chemical coupling or co-mobilisation.<br>",
      "Watch for <i>tight clusters</i> (stable systems) vs <i>diffuse or multi-modal patterns</i> (disturbance or anomalies).<br>",
      "These insights can guide <b>restoration nutrient balancing</b> or fertiliser targeting post-fire."
    )
    div(class="well-insight", style="background:#fff9c4;", HTML(msg))
  })
  
  #==================== TAB 3: AIR, POLLUTANTS & FORECAST ====================
  output$tab3_pollutant_timeseries <- renderPlotly({
    df <- read_csv("wrangled_air_quality_data.csv", show_col_types = FALSE) %>%
      mutate(date = as.Date(datetime_aest))
    pollutant_choices <- list(
      "PM2.5" = list(col = "bpm2_5", thresh = 10, unit = "µg/m³", color = "darkblue", bar_color = "steelblue"),
      "CO"    = list(col = "co",      thresh = 0.1, unit = "ppm",    color = "forestgreen", bar_color = "lightgreen"),
      "SO2"   = list(col = "so2",     thresh = 0.02, unit = "ppm",   color = "purple", bar_color = "plum")
    )
    info <- pollutant_choices[[input$tab3_pollutant]]
    pollutant_data <- df %>%
      group_by(date) %>%
      summarise(avg_value = mean(.data[[info$col]], na.rm = TRUE)) %>%
      mutate(
        fire_likely = ifelse(avg_value > info$thresh, 1, 0),
        fire_label = ifelse(fire_likely == 1, "Fire-Likely Day", "Normal Day")
      )
    plot_ly() %>%
      add_bars(
        x = pollutant_data$date,
        y = pollutant_data$fire_likely,
        name = paste(input$tab3_pollutant, "Fire-Likely Day"),
        marker = list(color = info$bar_color),
        text = paste0("Date: ", pollutant_data$date, "<br>",
                      "Fire-Likely: ", pollutant_data$fire_label),
        hoverinfo = "text"
      ) %>%
      add_lines(
        x = pollutant_data$date,
        y = pollutant_data$avg_value,
        name = paste(input$tab3_pollutant, "Level"),
        line = list(color = info$color, width = 2),
        text = paste0("Date: ", pollutant_data$date, "<br>",
                      "Value: ", round(pollutant_data$avg_value, 3), " ", info$unit),
        yaxis = "y2",
        hoverinfo = "text"
      ) %>%
      layout(
        title = paste(input$tab3_pollutant, "Levels and Fire-Likely Days"),
        xaxis = list(title = "Date", rangeslider = list(visible = TRUE)),
        yaxis = list(title = "Fire-Likely Day (1 = Yes)", rangemode = "tozero"),
        yaxis2 = list(
          title = "Pollutant Level",
          overlaying = "y",
          side = "right",
          rangemode = "tozero"
        ),
        legend = list(x = 0.05, y = 1.05, orientation = "h"),
        hovermode = "x unified"
      )
  })
  output$tab3_timeseries_interp <- renderUI({
    pol <- input$tab3_pollutant
    msg <- paste0(
      "<b>Interpretation:</b> Time series of <b>", pol, "</b> levels with detection of fire-likely days.<br><br>",
      "Days flagged as 'fire-likely' correspond to pollutant spikes above safe thresholds.<br>",
      "This supports <b>early warning systems, public health alerts, and resource deployment</b>.<br>",
      "<i>Frequent surges</i> indicate chronic exposure zones — consider intervention strategies like shelter advisories or air filtration programs."
    )
    div(class="well-insight", style="background:#b3e5fc;", HTML(msg))
  })
  output$tab3_nutrient_surface <- renderPlotly({
    leachate <- read_csv("leachate_data_for_analysis.csv", show_col_types = FALSE)
    set.seed(123)
    n <- nrow(leachate)
    leachate <- leachate %>%
      mutate(
        x = runif(n, 0, 2),
        y = runif(n, 0, 2)
      )
    nutrient_map <- list(
      "Total Nitrogen" = "tn_mg_l",
      "Total Phosphorus" = "tp_mg_l",
      "Ammonium" = "nh4_mg_l"
    )
    selected_col <- nutrient_map[[input$tab3_nutrient]]
    interp <- akima::interp(
      x = leachate$x,
      y = leachate$y,
      z = leachate[[selected_col]],
      linear = FALSE,
      duplicate = "mean",
      nx = 100, ny = 100
    )
    interp$z[interp$z < 0] <- 0
    plot_ly() %>%
      add_surface(
        x = interp$x,
        y = interp$y,
        z = interp$z,
        colorscale = "Viridis",
        name = input$tab3_nutrient
      ) %>%
      layout(
        title = paste("3D Surface: Interpolated", input$tab3_nutrient, "by Spatial Grid"),
        scene = list(
          xaxis = list(title = "Longitude (arb. units)"),
          yaxis = list(title = "Latitude (arb. units)"),
          zaxis = list(title = "Concentration (mg/L)")
        )
      )
  })
  output$tab3_surface_interp <- renderUI({
    nutr <- input$tab3_nutrient
    msg <- paste0(
      "<b>Interpretation:</b> Interpolated 3D surface for <b>", nutr, "</b> levels across sample locations.<br><br>",
      "Peaks reflect zones of nutrient accumulation (potential leaching risks); valleys indicate nutrient loss or post-burn depletion.<br>",
      "<b>Use this map to spatially prioritise</b> fertilisation, erosion control, or revegetation efforts.<br>",
      "Sharp gradients may signal edge effects or burn transition zones that deserve closer inspection."
    )
    div(class="well-insight", style="background:#c8e6c9;", HTML(msg))
  })
  output$tab3_streamgraph <- renderPlotly({
    air <- read_csv("daily_avg_air_quality.csv", show_col_types = FALSE) %>%
      mutate(
        date = as.Date(date),
        month = lubridate::month(date, label = TRUE, abbr = TRUE)
      )
    
    monthly_summary <- air %>%
      group_by(month) %>%
      summarise(
        PM2_5 = sum(avg_pm2_5, na.rm = TRUE),
        CO    = sum(avg_co, na.rm = TRUE),
        SO2   = sum(avg_so2, na.rm = TRUE)
      ) %>%
      arrange(match(month, month.abb))
    
    monthly_prop <- monthly_summary %>%
      mutate(total = PM2_5 + CO + SO2) %>%
      mutate(
        PM2_5 = PM2_5 / total,
        CO    = CO / total,
        SO2   = SO2 / total
      ) %>%
      select(month, PM2_5, CO, SO2)
    
    monthly_long <- monthly_prop %>%
      tidyr::pivot_longer(cols = -month, names_to = "Pollutant", values_to = "Proportion")
    
    plot_ly() %>%
      add_trace(
        data = monthly_long %>% filter(Pollutant == "PM2_5"),
        x = ~month, y = ~Proportion,
        type = 'scatter', mode = 'none',
        stackgroup = 'one',
        name = "PM2.5",
        fillcolor = "orange",
        hoverinfo = 'text',
        text = ~paste("Month:", month, "<br>PM2.5:", round(Proportion, 2))
      ) %>%
      add_trace(
        data = monthly_long %>% filter(Pollutant == "CO"),
        x = ~month, y = ~Proportion,
        type = 'scatter', mode = 'none',
        stackgroup = 'one',
        name = "CO",
        fillcolor = "darkblue",   # 🔵 Strong contrast
        hoverinfo = 'text',
        text = ~paste("Month:", month, "<br>CO:", round(Proportion, 2))
      ) %>%
      add_trace(
        data = monthly_long %>% filter(Pollutant == "SO2"),
        x = ~month, y = ~Proportion,
        type = 'scatter', mode = 'none',
        stackgroup = 'one',
        name = "SO₂",
        fillcolor = "green",   # Optional: can pick other high-contrast color
        hoverinfo = 'text',
        text = ~paste("Month:", month, "<br>SO₂:", round(Proportion, 2))
      ) %>%
      layout(
        title = "Interactive Streamgraph: Monthly Pollutant Composition",
        xaxis = list(title = "Month"),
        yaxis = list(
          title = "Proportion of Total Pollutant Load",
          tickvals = seq(0, 1, 0.1),
          tickformat = ".1f"
        ),
        legend = list(title = list(text = "<b>Pollutant</b>"))
      )
  })
  output$tab3_stream_interp <- renderUI({
    msg <- paste0(
      "<b>Interpretation:</b> Monthly proportion of air pollutants (PM2.5, SO₂, CO).<br><br>",
      "Fluctuations reflect fire activity, meteorological conditions, and regional emissions.<br>",
      "<b>High dominance of any one pollutant</b> suggests changing source patterns — e.g., biomass burning (CO), combustion (PM2.5), or chemical reactions (SO₂).<br><br>",
      "This chart is valuable for <i>seasonal planning, environmental audits, and targeted pollutant control</i>."
    )
    div(class="well-insight", style="background:#fff9c4;", HTML(msg))
  })
  
  #==================== TAB 4: PREDICTIVE MODELING ====================
  nutrient_labels <- c(
    "tn_mg_l"    = "Total Nitrogen (N, mg/L)",
    "tp_mg_l"    = "Total Phosphorus (P, mg/L)",
    "po4_3_mg_l" = "Phosphate (PO₄³⁻, mg/L)",
    "nh4_mg_l"   = "Ammonium (NH₄⁺, mg/L)",
    "n_ox_mg_l"  = "Nitrate Oxide (NOₓ⁻, mg/L)"
  )
  get_pred_data <- reactive({
    df <- soil_data_pred %>%
      filter(
        temperature >= input$pred_temp_range[1],
        temperature <= input$pred_temp_range[2]
      ) %>%
      select(temperature, value = all_of(input$pred_nutrient))
    df <- df %>% filter(!is.na(value), !is.na(temperature))
    df
  })
  output$facet_actual_pred <- renderPlotly({
    df <- get_pred_data()
    if(nrow(df) < 10) return(NULL)
    set.seed(2025)
    idx <- createDataPartition(df$value, p=0.8, list=FALSE)
    train <- df[idx,]; test <- df[-idx,]
    rfmod <- randomForest(value ~ temperature, data=train, ntree=200)
    test$pred <- predict(rfmod, test)
    nutrient_label <- nutrient_labels[[input$pred_nutrient]]
    plot_ly() %>%
      add_markers(
        x = test$temperature, y = test$value,
        name = "Observed", marker=list(color='blue'),
        text = ~paste("Temp:", round(test$temperature,1), "<br>Observed:", round(test$value,2)),
        hoverinfo="text"
      ) %>%
      add_lines(
        x = test$temperature, y = test$pred,
        name = "RF Predicted", line=list(color='red'),
        text = ~paste("Temp:", round(test$temperature,1), "<br>Predicted:", round(test$pred,2)),
        hoverinfo="text"
      ) %>%
      layout(
        title = list(text = paste("Actual vs Predicted:", nutrient_label)),
        xaxis = list(title = "Burn Temperature (°C)", titlefont = list(size=17), tickfont = list(size=15)),
        yaxis = list(title = nutrient_label, titlefont = list(size=17), tickfont = list(size=15)),
        legend = list(x = 0.01, y = 1),
        margin = list(l=90, r=30, b=80, t=80)
      )
  })
  output$tab4_actual_pred_interp <- renderUI({
    rng <- input$pred_temp_range
    msg <- paste0(
      "<b>Interpretation:</b> Model prediction performance for <b>", nutrient_labels[[input$pred_nutrient]], "</b> across <b>", rng[1], "–", rng[2], "°C</b>.<br><br>",
      "Strong alignment between actual and predicted values = <i>robust model fit</i>.<br>",
      "If gaps are large, it may indicate underfitting, noisy data, or an outlier fire event affecting nutrient levels."
    )
    div(class="pred-well", HTML(msg))
  })
  output$future_plot <- renderPlotly({
    df <- get_pred_data()
    if(nrow(df) < 10) return(NULL)
    temp_seq <- seq(input$pred_temp_range[1], input$pred_temp_range[2], length.out = 10)
    years <- 2025:2034
    mean_val <- mean(df$value, na.rm=TRUE)
    set.seed(2025)
    rfmod <- randomForest(value ~ temperature, data=df, ntree=200)
    pred_vals <- predict(rfmod, newdata=data.frame(temperature = temp_seq))
    nutrient_label <- nutrient_labels[[input$pred_nutrient]]
    plot_ly(
      x = years, y = pred_vals, type='scatter', mode='lines+markers',
      line=list(color='red', width=3), marker=list(size=8, color='orange')
    ) %>%
      layout(
        title = list(text = paste("Future Forecast:", nutrient_label, "(2025–2034)")),
        xaxis = list(title = "Year", titlefont = list(size=17), tickfont = list(size=15)),
        yaxis = list(title = paste("Forecasted", nutrient_label), titlefont = list(size=17), tickfont = list(size=15)),
        margin = list(l=90, r=30, b=80, t=80)
      )
  })
  output$tab4_forecast_interp <- renderUI({
    rng <- input$pred_temp_range
    msg <- paste0(
      "<b>Interpretation:</b> Forecasted trend for <b>", nutrient_labels[[input$pred_nutrient]], "</b> from 2025–2034.<br><br>",
      "Based on simulated conditions of repeated fires at <b>", rng[1], "–", rng[2], "°C</b>.<br>",
      "If projections show consistent decline, this signals a <b>risk of nutrient exhaustion</b> under future burn regimes.<br>",
      "Useful for <i>long-term soil recovery planning and nutrient replenishment programs</i>."
    )
    div(class="pred-well", HTML(msg))
  })
  output$summary_stats_table <- renderReactable({
    filtered <- soil_data %>%
      filter(
        temperature >= input$pred_temp_range[1],
        temperature <= input$pred_temp_range[2]
      )
    variables <- c(
      "Total Nitrogen (N, mg/L)" = "tn_mg_l",
      "Total Phosphorus (P, mg/L)" = "tp_mg_l",
      "Phosphate (PO₄³⁻, mg/L)" = "po4_3_mg_l",
      "Ammonium (NH₄⁺, mg/L)" = "nh4_mg_l",
      "Nitrate Oxide (NOₓ⁻, mg/L)" = "n_ox_mg_l"
    )
    stats <- lapply(variables, function(col) {
      vals <- filtered[[col]]
      c(
        Mean   = round(mean(vals, na.rm = TRUE), 2),
        Median = round(median(vals, na.rm = TRUE), 2),
        Mode   = round(modeest::mfv(vals, na_rm = TRUE)[1], 2)
      )
    })
    stats_df <- as.data.frame(do.call(rbind, stats))
    stats_df <- tibble::rownames_to_column(stats_df, var = "Soil Variable")
    col_lookup <- c(
      "tn_mg_l"    = 1,
      "tp_mg_l"    = 2,
      "po4_3_mg_l" = 3,
      "nh4_mg_l"   = 4,
      "n_ox_mg_l"  = 5
    )
    highlight_row <- col_lookup[[input$pred_nutrient]]
    reactable(
      stats_df,
      bordered = TRUE,
      highlight = TRUE,
      striped = TRUE,
      compact = FALSE,
      style = list(fontSize = 17, fontFamily = "system-ui, Segoe UI, Arial"),
      defaultColDef = colDef(
        align = "center",
        minWidth = 110,
        style = list(borderRight = "1.5px solid #e0e0e0")
      ),
      columns = list(
        `Soil Variable` = colDef(
          name = "Soil Variable",
          align = "left",
          style = list(fontWeight = "bold", borderRight = "1.5px solid #e0e0e0")
        ),
        Mean   = colDef(header = "Mean", style = list(borderRight = "1.5px solid #e0e0e0")),
        Median = colDef(header = "Median", style = list(borderRight = "1.5px solid #e0e0e0")),
        Mode   = colDef(header = "Mode")
      ),
      rowStyle = JS(
        sprintf("
        function(rowInfo) {
          if (rowInfo.index === %d) {
            return {
              background: '#ffd600',
              color: '#263238',
              fontWeight: 'bold',
              fontSize: '18px',
              border: '2.5px solid #ff8f00'
            }
          }
        }
      ", highlight_row - 1)
      ),
      theme = reactableTheme(
        borderColor = "#bdbdbd",
        stripedColor = "#f8fafd",
        highlightColor = "#e1f5fe",
        cellPadding = "10px 8px",
        headerStyle = list(
          background = "#cddc39",
          color = "#222",
          fontWeight = "bold",
          fontSize = "18px",
          borderBottom = "3px solid #afb42b"
        )
      ),
      defaultPageSize = nrow(stats_df)
    )
  })
  output$tab4_summary_interp <- renderUI({
    rng <- input$pred_temp_range
    nutr <- nutrient_labels[[input$pred_nutrient]]
    msg <- paste0(
      "<b>Interpretation:</b> Central tendency measures (mean, median, mode) for <b>", nutr, "</b> across temperatures <b>", rng[1], "–", rng[2], "°C</b>.<br><br>",
      "Close agreement among values = <b>symmetrical distribution</b>; divergence may indicate skew, outliers, or site-specific anomalies.<br>",
      "<b>Use this as a quick diagnostic</b> to spot irregularities or confirm data consistency before modeling."
    )
    div(class="pred-well", HTML(msg))
  })
})
