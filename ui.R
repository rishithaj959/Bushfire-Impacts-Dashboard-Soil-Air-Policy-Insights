library(shiny)
library(leaflet)
library(plotly)
library(bslib)
library(networkD3)
library(shinyBS)
library(reactable)

shinyUI(
  navbarPage(
    "Bushfire Impacts Dashboard",
    
    # ==== INTRODUCTION TAB ====
    tabPanel(
      "Introduction",
      fluidPage(
        theme = bs_theme(version = 5, bootswatch = "flatly"),
        tags$style(HTML(".intro-title { font-size: 2.2em; font-weight: bold; color: #1a2634; margin-top: 12px; }
                         .intro-sub { font-size: 1.12em; color: #343a40; line-height: 1.5; }
                         .feature-card { background-color: #f8f9fa; border-left: 6px solid #1976d2; border-radius: 8px; padding: 15px; margin-bottom: 15px; box-shadow: 0 2px 6px rgba(0,0,0,0.05); }
                         .feature-title { font-weight: bold; color: #1976d2; }
                         .about-section, .meta-panel { background-color: #f1f1f1; padding: 20px; border-radius: 8px; margin-top: 30px; box-shadow: 0 2px 6px rgba(0,0,0,0.05); }
                         .img-banner { width: 100%; height: auto; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.2); }
                         .section-header { font-size: 1.5em; font-weight: bold; margin-top: 20px; }")),
        
        fluidRow(
          column(5,
                 tags$img(src = "bushfire.jpeg", class = "img-banner", alt = "Bushfire Image")
          ),
          column(7,
                 div(class="intro-title", "Welcome to the Bushfire Impacts Dashboard"),
                 div(class="intro-sub", "This dashboard integrates detailed bushfire incident mapping, soil nutrient analysis, air quality monitoring, and predictive modeling to deliver a comprehensive view of bushfire impacts in Australia. By combining geospatial data, laboratory results, and statistical forecasts, users can explore fire patterns, assess soil and air changes, and anticipate future risks and recovery needs—supporting decision-making for policy, land management, health, and research."),
                 br(),
                 fluidRow(
                   column(6, div(class="feature-card", span(class="feature-title", "Soil & Fire Analysis: "),
                                 " Explore where bushfires have occurred, compare fire intensity across the landscape, and assess impacts on key soil nutrients vital for ecosystem recovery.")),
                   column(6, div(class="feature-card", span(class="feature-title", "Fire Flow & Soil Nutrients: "),
                                 " Track changes in fire management (planned vs unplanned burns), visualize fire activity over time and space, and investigate how soil chemistry responds to different burn severities.")),
                   column(6, div(class="feature-card", span(class="feature-title", "Air, Pollutants & Forecast: "),
                                 " Analyze the links between bushfire events and air quality, discover pollutant surges, and monitor health risks—plus view dynamic 3D soil maps and pollutant trends.")),
                   column(6, div(class="feature-card", span(class="feature-title", "Prediction Analysis: "),
                                 " Apply predictive modeling to anticipate nutrient loss and forecast future scenarios. See statistical trends for long-term environmental planning."))
                 )
          )
        ),
        
        fluidRow(
          column(8,
                 div(class="about-section",
                     tags$h4("About the Project"),
                     p("This dashboard integrates bushfire incident data, lab and field soil chemistry, air quality monitoring, and machine learning forecasts. By merging geospatial, chemical, and time-series datasets, it helps users:"),
                     tags$ul(
                       tags$li("Pinpoint risk zones and explore changes in soil and air quality post-fire"),
                       tags$li("Visualize trends and interactions between environmental variables"),
                       tags$li("Model future scenarios for better planning and recovery")
                     ),
                     tags$h4("How It Works"),
                     tags$ul(
                       tags$li("Select a tab to explore maps, charts, and results."),
                       tags$li("Adjust filters for region, year, fire intensity, or pollutant."),
                       tags$li("Interpretation panels and summary tables update instantly to reflect your choices.")
                     )
                 )
          ),
          column(4,
                 div(class="meta-panel",
                     tags$h4("Key Data Sources"),
                     tags$ul(
                       tags$li(tags$b("Fire Incidents:"), " National bushfire shapefiles (FIREMGT_LastFire_GDA2020.shp)"),
                       tags$li(tags$b("Soil Chemistry:"), " Lab and field measures (soil_combined_clean.csv, leachate_data_for_analysis.csv)"),
                       tags$li(tags$b("Air Quality:"), " Regional air monitoring (wrangled_air_quality_data.csv)"),
                       tags$li(tags$b("Forecasts:"), " Merged data and model results")
                     ),
                     tags$h4("Who Should Use This?"),
                     tags$ul(
                       tags$li("Environmental policy makers & land agencies"),
                       tags$li("Public health and research groups"),
                       tags$li("Community stakeholders in recovery/resilience")
                     )
                 )
          )
        ),
        
        hr(),
        div(style = "font-size: 1.05em; color: #626a70; margin-top:10px; margin-bottom:20px; text-align:center;",
            tags$b("How to use this dashboard:"),
            " Use the tabs above. Each section provides interactive filters and actionable insights to help interpret bushfire effects at a local and national scale."
        )
      )
    ),
    
    # ==== TAB 1 ====
    tabPanel(
      "Tab 1 — Soil & Fire",
      fluidPage(
        theme = bs_theme(version = 5, bootswatch = "flatly"),
        tags$style(HTML("
          .card { margin-bottom: 88px !important; }
          .bslib-card { padding: 42px 50px 38px 80px !important; }
          .well-insight { font-size: 1.07em; margin-top: 14px; margin-bottom: 10px; padding:7px 12px; border-radius:7px; width:99%; }
          .card-header { margin-bottom: 76px !important; }
          .dashboard-section { margin-bottom: 26px; }
          .lead-desc { font-size: 1.11em; margin-bottom: 20px; margin-top: 5px; }
          .content-box {
            background: #f8fafd;
            border-left: 5px solid #bdbdbd;
            border-radius: 8px;
            padding: 16px 22px 12px 22px;
            margin-bottom: 20px;
            font-size: 1.09em;
            box-shadow: 0 2px 6px rgba(190,190,190,0.04);
          }
        ")),
        fluidRow(
          column(
            12,
            h2("Soil & Fire: Spatial and Analytical Insights"),
            div(class="content-box",
                p("This section provides an in-depth analysis of bushfire impacts on Australian landscapes, focusing on spatial fire patterns and their effects on soil health. Through interactive maps and analytics, users can:"),
                tags$ul(
                  tags$li("Pinpoint regions most affected by severe or frequent bushfires."),
                  tags$li("Examine how fire intensity alters key soil nutrients essential for ecosystem recovery."),
                  tags$li("Support science-based post-fire response, remediation, and hazard mitigation strategies.")
                ),
                p(tags$b("Why it matters:"), "Fire-driven changes in soil chemistry can affect vegetation regrowth, water quality, and agricultural productivity. Understanding these dynamics is essential for sustainable land and fire management.")
            ),
            hr(style="margin-bottom:24px;")
          )
        ),
        fluidRow(
          column(
            6,
            bslib::card(
              style = "min-height:1050px; background:#fff3e0; border-left: 8px solid #fb8c00;",
              card_header(
                tags$div(
                  style="display:flex; align-items:center; gap:32px;",
                  tags$span("Fire Polygons by Burn Intensity", style="font-size:1.23em; font-weight:bold;")
                )
              ),
              div(class="content-box",
                  p("This map visualises the spatial distribution of all recorded bushfire events across the region, color-coded by estimated burn intensity. Each polygon shows a distinct fire, whether prescribed or unplanned."),
                  tags$ul(
                    tags$li(tags$b("Purpose:"), "Identify local fire hotspots and observe patterns of fire severity at a landscape scale."),
                    tags$li(tags$b("How to use:"), "Adjust the burn intensity filter to highlight low, moderate, or severe fires. Hover over polygons for detailed area and severity information.")
                  )
              ),
              selectInput("filter_burn", "Burn Intensity:",
                          choices = c("All", "Low Burn (200°C)", "Medium Burn (500°C)", "High Burn (850°C)"),
                          selected = "All",
                          width = "96%"),
              leafletOutput("vis1_map", height = 290),
              uiOutput("tab1_vis1_interp")
            )
          ),
          column(
            6,
            bslib::card(
              style = "min-height:1050px; background:#f1f8e9; border-left: 8px solid #388e3c;",
              card_header(
                tags$div(
                  style="display:flex; align-items:center; gap:92px;",
                  tags$span("Soil Nutrient Summaries at Fire Centroids", style="font-size:1.23em; font-weight:bold;")
                )
              ),
              div(class="content-box",
                  p("This panel shows the average concentrations of key soil nutrients—including total nitrogen, phosphorus, ammonium, and nitrate—at the centroid of each mapped fire. Nutrient levels are visualized by the fire’s intensity for easy side-by-side comparison."),
                  tags$ul(
                    tags$li(tags$b("Purpose:"), "Assess how different burn severities impact soil fertility and post-fire recovery potential."),
                    tags$li(tags$b("How to use:"), "Click any centroid marker to see detailed nutrient values for that event. Compare patterns across burn levels to identify areas most at risk of soil degradation or vegetation failure.")
                  )
              ),
              leafletOutput("vis2_map", height = 290),
              uiOutput("tab1_vis2_interp")
            )
          )
        ),
        fluidRow(
          column(
            12,
            bslib::card(
              style = "margin-top:22px; background:#fffde7; border-left: 8px solid #fbc02d;",
              card_header(
                tags$div(
                  style="display:flex; align-items:center; gap:12px;",
                  tags$span("Correlation Heatmap: Temperature & Soil Nutrients", style="font-size:1.23em; font-weight:bold;")
                )
              ),
              div(class="content-box",
                  p("This interactive heatmap quantifies the relationship between burn temperature and the abundance of major soil nutrients. Correlations are shown for all available soil samples across the range of fire intensities."),
                  tags$ul(
                    tags$li(tags$b("Purpose:"), "Reveal which nutrients are most vulnerable to loss or transformation under increasing burn temperatures."),
                    tags$li(tags$b("How to use:"), "Use the temperature slider to focus on specific burn ranges and observe how nutrient correlations shift.")
                  )
              ),
              sliderInput("temp_range", "Burn Temperature (°C):",
                          min = 23, max = 850, value = c(23, 850), step = 1, width = "75%"),
              plotlyOutput("vis3_corr", height = 340),
              uiOutput("tab1_vis3_interp")
            )
          )
        ),
        fluidRow(
          column(
            12,
            hr(style="margin-bottom:0; margin-top:28px;"),
            tags$div(
              style = "font-size: 13.5px; color: #555; padding-top: 8px; margin-bottom:2px;",
              tags$b("About:"), 
              "All dashboard data are aggregated and anonymised to support responsible environmental decision-making and protect sensitive sites. Use these insights to inform sustainable land management and bushfire recovery planning across Australia."
            )
          )
        )
      )
    ),
    
    # ==== TAB 2 ====
    tabPanel(
      "Tab 2 — Fire Flow & Soil Nutrients",
      fluidPage(
        theme = bs_theme(version = 5, bootswatch = "flatly"),
        tags$style(HTML("
          .card { margin-bottom: 24px !important; }
          .bslib-card { padding: 22px 30px 18px 30px !important; }
          .well-insight { font-size: 1.07em; margin-top: 14px; margin-bottom: 10px; padding:7px 12px; border-radius:7px; width:99%; }
          .card-header { margin-bottom: 12px !important; }
          .dashboard-section { margin-bottom: 16px; }
          .lead-desc { font-size: 1.11em; margin-bottom: 18px; margin-top: 5px; }
          .content-box {
            background: #f8fafd;
            border-left: 5px solid #bdbdbd;
            border-radius: 8px;
            padding: 16px 22px 12px 22px;
            margin-bottom: 20px;
            font-size: 1.09em;
            box-shadow: 0 2px 6px rgba(190,190,190,0.04);
          }
          @media (max-width: 1050px) {
            .bslib-card { padding: 12px 8px 12px 8px !important; }
          }
        ")),
        fluidRow(
          column(
            12,
            h2("Fire Management Flow & Soil Nutrient Patterns"),
            div(class="content-box",
                p("This section provides an integrated view of bushfire management and its environmental impacts across Australia, bringing together fire regime patterns and post-fire soil chemistry. The suite of interactive visualisations below allows stakeholders to:"),
                tags$ul(
                  tags$li("Track shifts in planned and unplanned fire management at a state and national level."),
                  tags$li("Visualise the flow of fire activity over time, from management type to geographic distribution."),
                  tags$li("Assess the cascading effects of fire severity on critical soil nutrients, which underpin ecosystem resilience and recovery."),
                  tags$li("Support evidence-based decision making for bushfire prevention, emergency response, and sustainable land management.")
                ),
                p(tags$b("Why it matters:"), "Understanding how fire type, intensity, and landscape context interact helps policy makers and land managers allocate resources efficiently, mitigate negative outcomes, and promote long-term ecosystem health after major fire events.")
            ),
            hr(style="margin-bottom:22px;")
          )
        ),
        fluidRow(
          column(3, selectInput("tab2_year", "Select Year/Period:",
                                choices = c("2011–2015", "2016–2021"), selected = "2016–2021")
          ),
          column(3, selectInput("tab2_state", "Select State:",
                                choices = c("All", "New South Wales", "Victoria", "Queensland", "Western Australia", "South Australia", "Tasmania", "Australian Capital Territory", "Northern Territory"),
                                selected = "All"
          )),
          column(6)
        ),
        fluidRow(
          column(
            6,
            bslib::card(
              style = "min-height:1100px; background:#e3f2fd; border-left: 8px solid #1976d2;",
              card_header(
                tags$div(
                  style="display:flex; align-items:center; gap:24px;",
                  tags$span("Lollipop Chart: Difference in Planned vs Unplanned Fire Area", style="font-size:1.18em; font-weight:bold;")
                )
              ),
              div(class="content-box",
                  p("This chart highlights the balance of fire management strategies at the state level, comparing areas subjected to planned (prescribed) burns and unplanned (wildfire) incidents."),
                  tags$ul(
                    tags$li(tags$b("Purpose:"), "Identify which states have successfully shifted toward proactive fire management, and which remain dominated by unplanned fire."),
                    tags$li(tags$b("How to use:"), "Select a time period and state to view the net difference in fire area. Hover to view exact values for each state.")
                  )
              ),
              plotlyOutput("fire_lollipop", height = 400),
              uiOutput("tab2_lollipop_interp")
            )
          ),
          column(
            6,
            bslib::card(
              style = "min-height:1100px; background:#f3e5f5; border-left: 8px solid #8e24aa;",
              card_header(
                tags$div(
                  style="display:flex; align-items:center; gap:34px;",
                  tags$span("Fire Flow Sankey: Period → Type → State", style="font-size:1.18em; font-weight:bold;")
                )
              ),
              div(class="content-box",
                  p("This Sankey diagram reveals the pathway of fire area across the country, tracing fire flows from management period, to fire type, to final state outcomes."),
                  tags$ul(
                    tags$li(tags$b("Purpose:"), "Map the complex distribution of fire activity and spot bottlenecks, major contributors, or at-risk states."),
                    tags$li(tags$b("How to use:"), "Select time period or state to dynamically update the flows. Node width represents the scale of area impacted.")
                  )
              ),
              sankeyNetworkOutput("fire_sankey", height = "400px"),
              uiOutput("tab2_sankey_interp")
            )
          )
        ),
        fluidRow(
          column(3, selectInput("tab2_severity", "Burn Severity:",
                                choices = c("All", "Low", "Medium", "High", "Very High"),
                                selected = "All"
          )),
          column(9)
        ),
        fluidRow(
          column(
            6,
            bslib::card(
              style = "min-height:1050px; background:#f1f8e9; border-left: 8px solid #43a047;",
              card_header(
                tags$div(
                  style="display:flex; align-items:center; gap:24px;",
                  tags$span("Parallel Coordinates: Soil Nutrient Profiles by Severity", style="font-size:1.18em; font-weight:bold;")
                )
              ),
              div(class="content-box",
                  p("Severe fires can disrupt soil nutrient cycles, impacting plant regrowth, water quality, and long-term land productivity. This visual explores how concentrations of total nitrogen (TN), total phosphorus (TP), and ammonium (NH₄) shift as burn severity increases."),
                  tags$ul(
                    tags$li(tags$b("Purpose:"), "Reveal multi-nutrient patterns and shifts caused by different burn intensities."),
                    tags$li(tags$b("How to use:"), "Use the burn severity filter to highlight specific fire impact classes. Each line represents a soil sample profile.")
                  )
              ),
              plotlyOutput("nutrient_parallel", height = 320),
              uiOutput("tab2_parallel_interp")
            )
          ),
          column(
            6,
            bslib::card(
              style = "min-height:1050px; background:#fffde7; border-left: 8px solid #fbc02d;",
              card_header(
                tags$div(
                  style="display:flex; align-items:center; gap:14px;",
                  tags$span("Scatterplot Matrix: Nutrient Intercorrelations by Severity", style="font-size:1.18em; font-weight:bold;")
                )
              ),
              div(class="content-box",
                  p("This scatterplot matrix uncovers relationships between key soil nutrients following fire. Use it to investigate whether nutrient losses or gains are linked, and whether these links depend on fire severity."),
                  tags$ul(
                    tags$li(tags$b("Purpose:"), "Detect interdependencies and tradeoffs between nutrients that can guide targeted recovery efforts."),
                    tags$li(tags$b("How to use:"), "Adjust burn severity and look for clusters, trends, or outliers by color.")
                  )
              ),
              plotlyOutput("nutrient_scatter_matrix", height = 320),
              uiOutput("tab2_scatter_interp")
            )
          )
        )
      )
    ),
    
    # ==== TAB 3 ====
    tabPanel(
      "Tab 3 — Air, Pollutants & Forecast",
      fluidPage(
        theme = bs_theme(version = 5, bootswatch = "flatly"),
        tags$style(HTML("
          .tab3-card { margin-bottom: 28px !important; }
          .tab3-blue { background: #e1f5fe !important; border-left: 8px solid #0277bd !important; }
          .tab3-green { background: #e8f5e9 !important; border-left: 8px solid #388e3c !important; }
          .tab3-yellow { background: #fffde7 !important; border-left: 8px solid #fbc02d !important; }
          .content-box {
            background: #f8fafd;
            border-left: 5px solid #bdbdbd;
            border-radius: 8px;
            padding: 16px 22px 12px 22px;
            margin-bottom: 20px;
            font-size: 1.09em;
            box-shadow: 0 2px 6px rgba(190,190,190,0.04);
          }
        ")),
        fluidRow(
          column(
            12,
            h2("Air Quality, Pollutant Dynamics & Post-Fire Risks"),
            div(class="content-box",
                p("This dashboard tab explores the links between bushfire activity and air quality in Australia, focusing on real-world health, environment, and policy implications."),
                tags$ul(
                  tags$li(tags$b("Fire-likely days:"), " Identify days with elevated pollutant levels following bushfires. This is vital for public health warnings, especially for vulnerable groups like children, elderly, and those with respiratory conditions."),
                  tags$li(tags$b("3D Nutrient Surfaces:"), " Visualise the spatial distribution and intensity of nutrient loss or accumulation in fire-affected soils—critical for understanding soil recovery and targeted remediation after major fire events."),
                  tags$li(tags$b("Pollutant composition over time:"), " Track monthly and seasonal changes in air pollutant makeup, revealing how fire regimes drive air quality patterns and risks."),
                  tags$li(tags$b("Forecast and policy relevance:"), " By visualising these dynamics, the dashboard supports evidence-based air quality forecasts and informs land and health policy decisions.")
                ),
                tags$div(style="margin-top:9px;",
                         tags$b("Why it matters:"),
                         tags$ul(
                           tags$li("Bushfire smoke and post-fire pollution are major health risks for vulnerable groups, including children, the elderly, and people with respiratory conditions."),
                           tags$li("Rapid identification of pollution surges enables public health warnings, reduces risk of exposure, and informs targeted intervention."),
                           tags$li("Visualising changes in soil nutrients after fire supports ecosystem recovery, land restoration, and agricultural planning."),
                           tags$li("Understanding pollutant dynamics over time informs evidence-based policy and resource allocation for environmental management.")
                         )
                ),
                
            ),
            hr(style="margin-bottom:22px;")
          )
        ),
        # ---- Pollutant Time Series Controls ----
        fluidRow(
          column(4,
                 selectInput("tab3_pollutant", "Select Pollutant:",
                             choices = c("PM2.5", "CO", "SO2"),
                             selected = "PM2.5", width = "100%")
          ),
          column(8)
        ),
        fluidRow(
          column(
            12,
            bslib::card(
              class = "tab3-card tab3-blue",
              card_header(
                tags$div(
                  style="display:flex; align-items:center; gap:30px;",
                  tags$span("Pollutant Levels and Fire-Likely Days", style="font-size:1.22em; font-weight:bold;")
                )
              ),
              div(class="content-box",
                  p("This interactive time series combines daily pollutant levels with fire-likely day detection based on air quality thresholds. Peaks often follow major bushfire events and indicate periods of greatest exposure risk."),
                  tags$ul(
                    tags$li(tags$b("Purpose:"), "Rapidly identify pollution surges post-fire to inform public warnings or air quality alerts."),
                    tags$li(tags$b("How to use:"), "Switch between pollutants to compare their trends and the frequency of fire-likely days across the observation period. Hover over bars/lines for specific day values and risk labels.")
                  )
              ),
              plotlyOutput("tab3_pollutant_timeseries", height = 370),
              uiOutput("tab3_timeseries_interp")
            )
          )
        ),
        # ---- 3D Surface Controls ----
        fluidRow(
          column(4,
                 selectInput("tab3_nutrient", "Select Nutrient:",
                             choices = c("Total Nitrogen", "Total Phosphorus", "Ammonium"),
                             selected = "Total Nitrogen", width = "100%")
          ),
          column(8)
        ),
        fluidRow(
          column(
            12,
            bslib::card(
              class = "tab3-card tab3-green",
              card_header(
                tags$div(
                  style="display:flex; align-items:center; gap:34px;",
                  tags$span("3D Surface: Nutrient Interpolation Across Sites", style="font-size:1.22em; font-weight:bold;")
                )
              ),
              div(class="content-box",
                  p("The 3D surface plot below reveals spatial patterns of soil nutrient levels after bushfire. By selecting a nutrient, users can see areas of depletion or concentration—key to assessing fire impact and guiding remediation."),
                  tags$ul(
                    tags$li(
                      tags$b("What does this show?"),
                      " After bushfires, nutrients like nitrogen and phosphorus can be lost (e.g., by volatilisation or erosion) or concentrated in soil patches. This plot visualises those patterns using interpolated sample data."
                    ),
                    tags$li(
                      tags$b("How do to use this?"),
                      tags$ul(
                        tags$li("Select a nutrient (Total Nitrogen, Total Phosphorus, or Ammonium) to display its surface."),
                        tags$li("Rotate, zoom, and hover for detail—look for high or low peaks that indicate nutrient 'hotspots' or loss."),
                        tags$li("If most of the surface is low, it may indicate widespread nutrient depletion post-fire."),
                        tags$li("Use as evidence to target soil testing, restoration, or fertilisation efforts.")
                      )
                    ),
                    tags$li(
                      tags$b("Why does this matter?"),
                      " Soil nutrient status determines ecosystem recovery, water quality, and agricultural productivity after fire. Early detection of depleted zones helps prioritise land management actions."
                    )
                  ),
                  tags$div(style="margin-top:7px; font-size:0.97em; color:#444;",
                           tags$b("Tip:"),
                           " Rotate the 3D plot with your mouse, zoom for a closer look, and hover to read exact nutrient concentrations at different points. Peaks show 'hotspots', valleys show nutrient loss."
                  )
              ),
              div(
                style = "display: flex; justify-content: center; padding-top: 45px;",
                plotlyOutput("tab3_nutrient_surface", height = "520px", width = "100%")
              ),
              uiOutput("tab3_surface_interp")
            )
          )
        ),
        # ---- Streamgraph: Monthly Pollutant Composition ----
        fluidRow(
          column(
            12,
            bslib::card(
              class = "tab3-card tab3-yellow",
              card_header(
                tags$div(
                  style="display:flex; align-items:center; gap:20px;",
                  tags$span("Interactive Streamgraph: Monthly Pollutant Load", style="font-size:1.22em; font-weight:bold;")
                )
              ),
              div(class="content-box",
                  p("The streamgraph below illustrates the changing composition of key air pollutants each month, reflecting both fire seasonality and recovery. Look for periods where a particular pollutant dominates—these may correspond to high fire activity or specific environmental conditions."),
                  tags$ul(
                    tags$li(tags$b("Purpose:"), "Track how the proportion of each pollutant (SO2, PM2.5, CO) changes through the year."),
                    tags$li(tags$b("How to use:"), "Observe seasonal spikes or dips—hover for exact values. Use in conjunction with fire data to understand timing and drivers of air quality risks.")
                  )
              ),
              plotlyOutput("tab3_streamgraph", height = 340),
              uiOutput("tab3_stream_interp")
            )
          )
        )
      )
    ),
    
    # ==== TAB 4 — Prediction Analysis ====
    tabPanel(
      "Tab 4 — Prediction Analysis",
      fluidPage(
        theme = bs_theme(version = 5, bootswatch = "flatly"),
        tags$style(HTML("
          .pred-card { margin-bottom: 26px !important; background: #f9fbe7 !important; border-left: 8px solid #cddc39 !important; }
          .pred-well { font-size: 1.08em; background: #f1f8e9; margin-top: 14px; margin-bottom: 8px; padding:7px 12px; border-radius:7px; width:99%; }
        ")),
        fluidRow(
          column(
            12,
            h2("Statistical Testing & Predictive Modeling: Nutrient Leachability"),
            div(class="content-box",
                p("This tab applies Random Forest regression to model how burn temperature affects nutrient leachability, and forecasts key soil nutrient losses from 2025–2035 under simulated high-burn scenarios."),
                tags$ul(
                  tags$li("Dual visualizations show model fit (actual vs predicted) and 10-year nutrient projections."),
                  tags$li("Interactive controls allow you to select the nutrient of interest and filter by temperature."),
                  tags$li("Forecasts help anticipate soil health risks under future fire regimes and guide intervention planning.")
                ),
                p(tags$b("Why it matters:"), "Sustained nutrient depletion from repeated high-intensity burns threatens post-fire recovery, soil fertility, and long-term land productivity.")
            ),
            hr(style="margin-bottom:24px;")
          )
        ),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectInput("pred_nutrient", "Choose Nutrient for Forecast:",
                        choices = c(
                          "Total Nitrogen (tn_mg_l)" = "tn_mg_l",
                          "Total Phosphorus (tp_mg_l)" = "tp_mg_l",
                          "Ammonium (nh4_mg_l)" = "nh4_mg_l",
                          "Nitrate (n_ox_mg_l)" = "n_ox_mg_l",
                          "Phosphate (po4_3_mg_l)" = "po4_3_mg_l"
                        ),
                        selected = "tn_mg_l"
            ),
            sliderInput("pred_temp_range", "Burn Temperature (°C):",
                        min = 23, max = 850, value = c(23, 850), step = 1)
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              tabPanel(
                "Actual vs Predicted",
                bslib::card(
                  class = "pred-card",
                  card_header(tags$span("Model Fit: Actual vs. Predicted by Temperature", style="font-size:1.13em; font-weight:bold;")),
                  plotlyOutput("facet_actual_pred", height = 350),
                  uiOutput("tab4_actual_pred_interp")
                )
              ),
              tabPanel(
                "Future Forecast (2025–2034)",
                bslib::card(
                  class = "pred-card",
                  card_header(tags$span("Forecasted Nutrient Leachability (10 Years)", style="font-size:1.13em; font-weight:bold;")),
                  plotlyOutput("future_plot", height = 350),
                  uiOutput("tab4_forecast_interp")
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            12,
            bslib::card(
              class = "pred-card",
              card_header(tags$span("Summary Table: Mean, Median, Mode Across All Datasets", style="font-size:1.13em; font-weight:bold;")),
              reactableOutput("summary_stats_table", height = 340),
              uiOutput("tab4_summary_interp")
            )
          )
        )
      )
    )
  )
)