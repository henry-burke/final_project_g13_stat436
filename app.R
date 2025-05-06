library(shiny)
library(shinyjs)
library(tidyverse)
library(plotly)
library(leaflet)
library(rnaturalearth)
library(sf)
library(scales)
library(htmltools)
library(htmlwidgets)
library(reshape2)
library(ggnewscale)
library(reactable)

#── GLOBAL DATA PREPARATION ────────────────────────────────────────────────────

# 1) Load & clean the Alzheimer’s survey
alz_df = read_csv(
  "https://uwmadison.box.com/shared/static/a2a74zejbdt5ywk7pxsvsw06gsypi8to.csv",
  show_col_types = FALSE
) |>
  mutate(
    Country = str_replace(Country, "USA", "United States"),
    Country = str_replace(Country, "UK",  "United Kingdom")
  ) |>
  rename_at(vars(25), ~ "Alzheimer's Diagnosis")

# 2) Load World Happiness
happy_df = read_csv(
  "https://uwmadison.box.com/shared/static/z79r9iafg3tzvm6cxagvft9auvcu868t.csv",
  show_col_types = FALSE
) |>
  rename(Country = `Country name`)

#── DATA FOR HAPPINESS SCATTER (Henry’s) ───────────────────────────────────────


alzheimers = alz_df |>
  filter(!if_any(everything(), ~ . == "Medium")) |>
  filter(`Sleep Quality` != "Average") |>
  filter(`Dietary Habits` != "Average")

country_choices = c("All Countries", sort(unique(alzheimers$Country)))

#── DATA FOR HAPPINESS SCATTER (Henry’s) ───────────────────────────────────────

alz_by_country = left_join(alz_df, happy_df, by = "Country") |>
  filter(`Alzheimer's Diagnosis` == "Yes") |>
  group_by(Country, `Ladder score`) |>
  summarize(yes_count = n(), .groups = "drop")

#── DATA FOR LEAFLET MAP (Sanjay’s) ────────────────────────────────────────────

# numeric flag for AD
map_alz_df = alz_df |>
  mutate(AD_numeric = if_else(`Alzheimer's Diagnosis` == "Yes", 1, 0))

countries_geo = ne_countries(scale = "medium", returnclass = "sf") |>
  st_transform(crs = 4326) |>
  mutate(
    Country = name,
    Country = if_else(Country == "United States of America",
                      "United States", Country)
  )

country_metrics = map_alz_df |>
  group_by(Country) |>
  summarize(
    sample_size     = n(),
    alzheimers_rate = mean(AD_numeric, na.rm = TRUE),
    diabetes_rate   = mean(Diabetes   == "Yes", na.rm = TRUE),
    avg_age         = mean(Age        , na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(sample_size >= 5)

suppressWarnings({
  countries_centroid = st_centroid(countries_geo)
})
countries_coords = st_coordinates(countries_centroid) |>
  as_tibble() |>
  rename(X = X, Y = Y) |>
  bind_cols(Country = countries_geo$Country)

map_data     = inner_join(countries_geo,     country_metrics, by = "Country")
circles_data = inner_join(country_metrics, countries_coords, by = "Country")

diabetes_palette = colorNumeric("YlOrBr", domain = country_metrics$diabetes_rate)
age_palette      = colorNumeric("YlOrBr", domain = country_metrics$avg_age)
alz_color        = "darkorchid"
alz_radius       = function(rate) sqrt(rate) * 12

# Legend helper from Sanjay
addAlzheimersLegend = function(map) {
  addLegendCustom = function(map, colors, labels, sizes, opacity = 1) {
    legendHTML = paste0(
      "<div style='line-height:1.3em;font-size:11px;'>",
      paste(
        "<div style='display:flex;align-items:center;'>",
        "<svg width='", sizes*2, "' height='", sizes*2, "'>",
        "<circle cx='", sizes, "' cy='", sizes, "' r='", sizes,
        "' fill='", colors, "' fill-opacity='", opacity,
        "' stroke='black' stroke-width='1'/>",
        "</svg>",
        "<span style='margin-left:6px;'>", labels, "</span>",
        "</div>",
        collapse = ""
      ),
      "</div>"
    )
    addControl(map, html = legendHTML, position = "bottomleft")
  }
  sizes  = c(4,8,12)
  labels = c("Low Alzheimer's Rate", "Medium", "High")
  colors = rep(alz_color, length(sizes))
  addLegendCustom(map, colors, labels, sizes, opacity = 0.8)
}

instructions_html = HTML(
  "<div style='padding:4px 6px;background:white;font-size:11px;
       border:1px solid gray;border-radius:4px;'>
     <b>How to use:</b><br>
     Hover for details<br>
     Switch layers top-right<br>
     Circle = Alzheimer's rate
   </div>"
)

diabetes_legend = HTML(
  "<div id='diabetes-legend' style='display:none;background:white;
        padding:6px;font-size:12px;border:1px solid #999;
        border-radius:4px;'>
     <strong>Diabetes Rate</strong><br>
     <svg height='80' width='20'><defs>
       <linearGradient id='grad1' x1='0%' y1='100%' x2='0%' y2='0%'>
         <stop offset='0%' style='stop-color:#ffffcc;stop-opacity:1'/>
         <stop offset='100%'style='stop-color:#d95f0e;stop-opacity:1'/>
       </linearGradient>
     </defs>
     <rect width='20' height='80' fill='url(#grad1)'/>
     </svg><br><small>Low → High</small>
   </div>"
)

age_legend = HTML(
  "<div id='age-legend' style='display:none;background:white;
        padding:6px;font-size:12px;border:1px solid #999;
        border-radius:4px;'>
     <strong>Average Age</strong><br>
     <svg height='80' width='20'><defs>
       <linearGradient id='grad2' x1='0%' y1='100%' x2='0%' y2='0%'>
         <stop offset='0%' style='stop-color:#ffffcc;stop-opacity:1'/>
         <stop offset='100%'style='stop-color:#d95f0e;stop-opacity:1'/>
       </linearGradient>
     </defs>
     <rect width='20' height='80' fill='url(#grad2)'/>
     </svg><br><small>Low → High</small>
   </div>"
)

#── DATA FOR NESTED PIE CHARTS  (Beomseong’s) ──────────────────────────────────

# Create binned variables for education, BMI, and cognitive score
alz_df_binned <- alz_df %>%
  mutate(
    `Education Level (Binned)` = cut(
      `Education Level`,
      breaks = c(-Inf, 4, 9, 14, Inf),
      labels = c("Low", "Medium", "High", "Very High"),
      right = TRUE
    ),
    `BMI (Binned)` = cut(
      BMI,
      breaks = c(-Inf, 24.9, 29.9, Inf),
      labels = c("Normal", "Overweight", "Obese"),
      right = TRUE
    ),
    `Cognitive Score (Binned)` = cut(
      `Cognitive Test Score`,
      breaks = c(-Inf, 46, 64, 82, Inf),
      labels = c("Low", "Medium", "High", "Very High"),
      right = TRUE
    )
  )

# Calculate the overall Alzheimer's rate for reference
overall_rate_static <- round(
  sum(alz_df$`Alzheimer's Diagnosis` == "Yes", na.rm = TRUE) / nrow(alz_df) * 100, 1)

#── UI ─────────────────────────────────────────────────────────────────────────

ui = fluidPage(
  useShinyjs(),
  
  # custom CSS for grey background + white cards
  tags$head(tags$style(HTML("
    body { background-color: #f0f0f0; }
    .navbar { background-color: #f0f0f0; border: none; }
    .tab-content, .tab-pane {
      background-color: #ffffff;
      padding: 20px;
      margin: 20px;
      border-radius: 8px;
      box-shadow: 0 2px 5px rgba(0,0,0,0.1);
    }
  "))),
  
  sidebarLayout(
    # persistent textbox on the left
    sidebarPanel(
      wellPanel(
        tags$h4("Instructions"),
        tags$p("Welcome to your interactive Alzheimer's Dashboard!
               There are five visualizations for you to explore via
               the tabs along the top of this page. Investigate each
               visualization and see what correlations you can discover.")
      ),
      wellPanel(
        tags$h4("Authors"),
        tags$p("Henry Burke, John Derr, Beomseong Kim, Sanjay Nagarimadugu, Jack Vaughn")
      ),
      width = 3
    ),
    
    # all visualizations on the right
    mainPanel(
      width = 9,
      navbarPage(
        title = "Alzheimer's Dashboard",
        id    = "nav",
        
        # 1) Global Map (Sanjay)
        tabPanel("Global Map",
                 leafletOutput("global_map", height = "600px")
        ),
        
        # 2) Happiness vs AD (Henry)
        tabPanel("Happiness vs Alzheimer's Diagnosis",
                 wellPanel(
                   "This scatterplot shows how the World Happiness Index (score from 1-10 denoting how happy
             a country's general population is) for each country relates to the number of 
             Alzheimer's diagnoses. Hover over a point to see the exact
             country and values, and brush to select multiple countries below."
                 ),
                 
                 fluidRow(
                   column(
                     width = 8,
                     div(
                       style = "position: relative; overflow: visible;",
                       plotOutput(
                         "scatter",
                         hover = hoverOpts("plot_hover", delay = 50, delayType = "throttle"),
                         brush = brushOpts("plot_brush", resetOnNew = TRUE)
                       ),
                       uiOutput("tooltip")
                     )
                   ),
                   column(
                     width = 4,
                     h4("Selected countries"),
                     reactableOutput("brush_info")
                   )
                 )
        ),
        
        # 3) Correlation Explorer (John)
        tabPanel("Correlation Explorer",
                 sidebarLayout(
                   sidebarPanel(
                     tabsetPanel(
                       id = "sidebar_tabs",
                       
                       tabPanel("Explorer",
                                selectInput("corr_xvar", "X Variable:", choices = NULL),
                                selectInput("corr_yvar", "Y Variable:", choices = NULL),
                                radioButtons("corr_summary_metric", "Summary Metric:",
                                             choices = c("Alzheimer's Percentage" = "alz_pct",
                                                         "Average Age"            = "avg_age",
                                                         "Count"                  = "count"),
                                             selected = "alz_pct"
                                ),
                                sliderInput("corr_bin_count", "Number of Groups:",
                                            min = 2, max = 20, value = 10
                                ),
                                checkboxInput("corr_show_other", "Show 'Other' Category for Country",
                                              value = FALSE, width = "100%"
                                )
                       ),
                       
                       tabPanel("Filters",
                                sliderInput("corr_age_range", "Age Range:",
                                            min = 20, max = 100, value = c(20, 100)
                                ),
                                selectInput("corr_gender", "Gender:",
                                            choices = c("All", "Male", "Female")
                                ),
                                selectInput("corr_diagnosis", "Alzheimer's Diagnosis:",
                                            choices = c("All", "Yes", "No")
                                )
                       )
                     ),
                     
                     actionButton("corr_apply_filters", "Apply Filters",
                                  class = "btn-primary",
                                  style = "margin-top:10px; width:100%;"
                     )
                   ),
                   
                   mainPanel(
                     plotlyOutput("corr_plot",       height = "500px"),
                     htmlOutput  ("corr_explanation")
                   )
                 )
        ),
        
        # 4) Health Conditions Heatmap (Jack)
        tabPanel("Health Heatmap",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("country", "Select Country:", 
                                 choices = country_choices,
                                 selected = "All Countries"),
                     
                     div(
                       h4("Select Health Factors:"),
                       div(
                         style = "background-color: #f9f9f9; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                         h5("Physical Health Factors", style = "margin-top: 0;"),
                         checkboxInput("selectAllPhysical", "Select All Physical Factors", FALSE),
                         checkboxGroupInput("physicalFactors", NULL,
                                            choices = c(
                                              "Diabetes",
                                              "Hypertension"
                                            ),
                                            selected = c("Diabetes"))
                       ),
                       
                       div(
                         style = "background-color: #f9f9f9; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                         h5("Mental Health Factors", style = "margin-top: 0;"),
                         checkboxInput("selectAllMental", "Select All Mental Factors", FALSE),
                         checkboxGroupInput("mentalFactors", NULL,
                                            choices = c(
                                              "Depression Level",
                                              "Stress Level"
                                            ),
                                            selected = c("Depression Level"))
                       ),
                       
                       div(
                         style = "background-color: #f9f9f9; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                         h5("Lifestyle Factors", style = "margin-top: 0;"),
                         checkboxInput("selectAllLifestyle", "Select All Lifestyle Factors", FALSE),
                         checkboxGroupInput("lifestyleFactors", NULL,
                                            choices = c(
                                              "Activity Level",
                                              "Sleep Quality",
                                              "Social Engagement"
                                            ),
                                            selected = c())
                       ),
                       
                       div(
                         style = "background-color: #f9f9f9; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
                         h5("Genetic Factors", style = "margin-top: 0;"),
                         checkboxGroupInput("geneticFactors", NULL,
                                            choices = c(
                                              "Family History of Alzheimer's"
                                            ),
                                            selected = c())
                       )
                     ),
                     
                     div(
                       style = "border-top: 1px solid #eee; padding-top: 10px; margin-top: 10px;",
                       h4("Advanced Options:"),
                       selectInput("colorScheme", "Color Scheme:",
                                   choices = c("Blue/Red (Colorblind Friendly)" = "colorblind",
                                               "Sequential Blues/Purples" = "sequential",
                                               "Diverging" = "diverging"),
                                   selected = "colorblind"),
                       checkboxInput("showLabels", "Show Value Labels", TRUE),
                       checkboxInput("normalizeCounts", "Normalize Counts (%)", FALSE)
                     )
                   ),
                   
                   mainPanel(
                     br(),
                     plotOutput("heatmap", height = "600px"),
                     br(),
                     div(id = "legend-key",
                         style = "background-color: #f9f9f9; padding: 15px; border-radius: 5px;",
                         h4("How to Interpret:"),
                         p("Each tile represents the number of Alzheimer's cases associated with a specific factor."),
                         p("• Y-axis: Selected health factors"),
                         p("• X-axis: Status of each condition (Yes/No or High/Low)"),
                         p("• Color intensity: Darker colors indicate higher number of cases")
                     )
                   )
                 )
        ),
        
        # 5) Diagnosis Pie Charts (Beomseong)
        tabPanel("Pie Charts",
                 sidebarLayout(
                   sidebarPanel(
                     width = 3,
                     tags$div(
                       class = "filter-section",
                       tags$h4("Population Filters",
                               style = "color: #2c3e50; border-bottom: 1px solid #ddd; padding-bottom: 10px;"),
                       sliderInput(
                         inputId = "pie_ageRange",
                         label = "Age Range:",
                         min = floor(min(alz_df$Age, na.rm = TRUE)),
                         max = ceiling(max(alz_df$Age, na.rm = TRUE)),
                         value = c(50, 90)
                       ),
                       selectInput(
                         inputId = "pie_genderFilter",
                         label = "Gender:",
                         choices = c("All", unique(alz_df$Gender)),
                         selected = "All"
                       ),
                       selectInput(
                         inputId = "pie_countryFilter",
                         label = "Country:",
                         choices = c("All", unique(alz_df$Country)),
                         selected = "All"
                       )
                     ),
                     tags$hr(),
                     tags$div(
                       class = "analysis-section",
                       tags$h4("Analysis Variables",
                               style = "color: #2c3e50; border-bottom: 1px solid #ddd; padding-bottom: 10px;"),
                       selectInput(
                         inputId = "pie_varChoice",
                         label = "Primary Risk Factor:",
                         choices = c(
                           "Physical Activity Level", "Smoking Status", "Alcohol Consumption",
                           "Diabetes", "Hypertension", "Cholesterol Level",
                           "Family History of Alzheimer’s", "Genetic Risk Factor (APOE-ε4 allele)",
                           "Stress Levels", "Depression Level", "Sleep Quality", "Dietary Habits",
                           "Air Pollution Exposure", "Employment Status", "Marital Status",
                           "Social Engagement Level", "Income Level", "Urban vs Rural Living",
                           "Education Level", "BMI", "Cognitive Test Score"
                         ),
                         selected = "Physical Activity Level"
                       )
                     )
                   ),
                   
                   mainPanel(
                     width = 9,
                     tags$div(style = "display: flex; flex-direction: column;",
                              
                              tags$div(
                                class = "insights-panel",
                                style = "padding: 15px; background-color: #f5f5f5; border-radius: 5px; margin-bottom: 20px;",
                                tags$h3("Key Insights", style = "margin-top: 0; color: #2c3e50;"),
                                uiOutput("keyInsights")
                              ),
                              
                              tags$div(
                                style = "display: flex; flex-wrap: wrap;",
                                tags$div(
                                  style = "flex: 1; min-width: 300px; margin-right: 20px;",
                                  tags$div(style = "background-color: white; border-radius: 5px; padding: 15px; min-height: 520px;",
                                           tags$h4("Distribution in Alzheimer's Patients", style = "color: #2c3e50; text-align: center;"),
                                           plotlyOutput("pie_chart", height = "550px")
                                  )
                                )
                              )
                     )
                   )
                 )
                 
        )
      )  # end navbarPage
    )    # end mainPanel
  )      # end sidebarLayout
)        # end fluidPage



#── SERVER ───────────────────────────────────────────────────────────────────

server = function(input, output, session) {
  
  #—— 1) Render Leaflet Map —————————————————————————————————————————————
  output$global_map = renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = 0, lat = 20, zoom = 2) |>
      
      # Diabetes fill
      addPolygons(
        data       = map_data,
        fillColor  = ~diabetes_palette(diabetes_rate),
        fillOpacity= 0.7,
        weight     = 1, color = "#333",
        highlightOptions = highlightOptions(weight = 2, color = "#666"),
        label      = ~paste0(Country, ": Diabetes = ",
                             percent(diabetes_rate)),
        popup      = ~paste0(
          "<strong>", Country, "</strong><br>",
          "Diabetes Rate: ", percent(diabetes_rate), "<br>",
          "Average Age: ", round(avg_age,1), "<br>",
          "Alzheimer's Rate: ", percent(alzheimers_rate), "<br>",
          "Sample Size: ", sample_size
        ),
        group = "Diabetes Rate"
      ) |>
      
      # Age fill
      addPolygons(
        data       = map_data,
        fillColor  = ~age_palette(avg_age),
        fillOpacity= 0.7,
        weight     = 1, color = "#333",
        highlightOptions = highlightOptions(weight = 2, color = "#666"),
        label      = ~paste0(Country, ": Avg Age = ", round(avg_age,1)),
        popup      = ~paste0(
          "<strong>", Country, "</strong><br>",
          "Average Age: ", round(avg_age,1), "<br>",
          "Diabetes Rate: ", percent(diabetes_rate), "<br>",
          "Alzheimer's Rate: ", percent(alzheimers_rate), "<br>",
          "Sample Size: ", sample_size
        ),
        group = "Average Age"
      ) |>
      
      # Alzheimer's circles
      addCircleMarkers(
        data       = circles_data,
        lng        = ~X, lat = ~Y,
        radius     = ~alz_radius(alzheimers_rate),
        stroke     = TRUE, color = "black", weight = 1,
        fillColor  = alz_color, fillOpacity = 0.8,
        popup      = ~paste0(
          "<strong>", Country, "</strong><br>",
          "Alzheimer's Rate: ", percent(alzheimers_rate)
        ),
        group = c("Diabetes Rate","Average Age")
      ) |>
      
      addLayersControl(
        baseGroups = c("Diabetes Rate","Average Age"),
        options    = layersControlOptions(collapsed = FALSE)
      ) |>
      
      addAlzheimersLegend() |>
      addControl(instructions_html, position = "topleft") |>
      addControl(diabetes_legend,  position = "bottomright") |>
      addControl(age_legend,       position = "bottomright") |>
      
      onRender("
        function(el,x) {
          var dL = document.getElementById('diabetes-legend'),
              aL = document.getElementById('age-legend'),
              radios = el.querySelectorAll('input[type=radio]');
          radios.forEach(function(r){
            r.addEventListener('change',function(){
              if(r.value.includes('Diabetes')) {
                dL.style.display='block'; aL.style.display='none';
              } else {
                dL.style.display='none';  aL.style.display='block';
              }
            });
          });
          dL.style.display = 'block';
        }
      ")
  })
  
  
  #—— 2) Happiness vs AD Scatter —————————————————————————————————————
  # 1) render the scatterplot
  output$scatter <- renderPlot({
    ggplot(alz_by_country, aes(x = `Ladder score`, y = yes_count)) +
      geom_point(size = 3) +
      labs(
        x = "World Happiness Index (Ladder score)",
        y = "Number of Alzheimer's Diagnoses",
        subtitle = "Hover for details; brush to select"
      ) +
      theme_minimal()
  })
  
  # 2) floating tooltip on hover
  output$tooltip <- renderUI({
    hover <- input$plot_hover
    if (is.null(hover)) return(NULL)
    
    pt <- nearPoints(
      alz_by_country,
      hover,
      xvar = "Ladder score",
      yvar = "yes_count",
      maxpoints = 1,
      threshold = 10
    )
    if (nrow(pt) == 0) return(NULL)
    
    style <- sprintf(
      "position:absolute; 
       left:%fpx; 
       top:%fpx;
       z-index:1000;
       background: rgba(255,255,255,0.9);
       padding: 5px; 
       border:1px solid #666;
       border-radius:4px; 
       pointer-events:none;",
      hover$coords_css$x + 10,
      hover$coords_css$y + 10
    )
    
    wellPanel(
      style = style,
      p(strong("Country: "), pt$Country),
      p(strong("Ladder score: "), pt$`Ladder score`),
      p(strong("Diagnoses: "), pt$yes_count)
    )
  })
  
  # 3) show brushed points (or all if no brush) in a reactable table
  output$brush_info <- renderReactable({
    df <- if (is.null(input$plot_brush)) {
      alz_by_country
    } else {
      brushedPoints(
        alz_by_country,
        input$plot_brush,
        xvar = "Ladder score",
        yvar = "yes_count"
      )
    }
    
    reactable(
      df,
      columns = list(
        Country       = colDef(name = "Country"),
        `Ladder score` = colDef(name = "Ladder Score"),
        yes_count     = colDef(name = "Diagnoses")
      ),
      defaultPageSize = 10,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(5, 10, 20),
      highlight = TRUE,
      striped = TRUE
    )
  })
  
  
  #—— 3) Correlation Explorer ————————————————————————————————————————
  
  # helper for binning (John’s function)
  create_bins = function(var, var_name, n_bins, show_other) {
    if (is.numeric(var)) {
      breaks = quantile(var, probs = seq(0,1,length.out=n_bins+1),
                        na.rm = TRUE)
      breaks = unique(breaks)
      if (length(breaks) <= 1) {
        return(factor(rep(paste0("Value: ",
                                 format(unique(na.omit(var))[1],digits=3)),
                          length(var))))
      }
      if (length(breaks)==2 && length(unique(na.omit(var)))>1) {
        breaks = seq(min(var,na.rm=TRUE), max(var,na.rm=TRUE),
                     length.out = n_bins+1) |> unique()
        if (length(breaks)<=1) {
          return(factor(rep(paste0("Value: ",
                                   format(unique(na.omit(var))[1],digits=3)),
                            length(var))))
        }
      }
      cut(var, breaks=breaks, include.lowest=TRUE, dig.lab=3)
    } else {
      var_factor = factor(var)
      num_levels = nlevels(var_factor)
      if (var_name=="Country") {
        country_counts = sort(table(var_factor), decreasing=TRUE)
        top_n = min(n_bins, length(country_counts))
        top_c  = names(country_counts)[1:top_n]
        if (!show_other && length(country_counts)>top_n) {
          factor(ifelse(var %in% top_c, as.character(var), NA),
                 levels=top_c)
        } else if (show_other && length(country_counts)>top_n) {
          factor(ifelse(var %in% top_c, as.character(var), "Other"),
                 levels=c(top_c,"Other"))
        } else {
          factor(var, levels=names(country_counts))
        }
      } else {
        if (num_levels > n_bins) {
          cat_counts = sort(table(var_factor), decreasing=TRUE)
          top_n = min(n_bins, length(cat_counts))
          top_c = names(cat_counts)[1:top_n]
          factor(ifelse(var %in% top_c, as.character(var), "Other"),
                 levels=c(top_c,"Other"))
        } else {
          var_factor
        }
      }
    }
  }
  
  # initial variable choices
  observe({
    cols = names(alz_df)
    sel_x = if ("Age" %in% cols) "Age" else cols[1]
    sel_y = if ("Cognitive Test Score" %in% cols)
      "Cognitive Test Score" else cols[min(2,length(cols))]
    updateSelectInput(session, "corr_xvar",
                      choices = cols, selected = sel_x)
    updateSelectInput(session, "corr_yvar",
                      choices = cols, selected = sel_y)
  })
  
  # age slider
  observe({
    rng = range(alz_df$Age, na.rm=TRUE)
    updateSliderInput(session, "corr_age_range",
                      min = floor(rng[1]), max = ceiling(rng[2]),
                      value = c(floor(rng[1]), ceiling(rng[2])))
  })
  
  # toggle show_other
  observe({
    needs = input$corr_xvar=="Country" ||
      input$corr_yvar=="Country"
    toggleState("corr_show_other", needs)
    if (!needs) {
      updateCheckboxInput(session, "corr_show_other", value = FALSE)
    }
  })
  
  # filtered data event
  filtered_corr = eventReactive(input$corr_apply_filters, {
    df = alz_df
    df = df |> filter(
      Age >= input$corr_age_range[1],
      Age <= input$corr_age_range[2]
    )
    if (input$corr_gender != "All")
      df = df |> filter(Gender == input$corr_gender)
    if (input$corr_diagnosis != "All")
      df = df |> filter(`Alzheimer's Diagnosis` == input$corr_diagnosis)
    df
  }, ignoreNULL = FALSE)
  
  # aggregation
  summary_corr = reactive({
    df = filtered_corr()
    req(nrow(df) > 0)
    
    x_var = input$corr_xvar
    y_var = input$corr_yvar
    
    x_bins = create_bins(df[[x_var]], x_var,
                         input$corr_bin_count,
                         input$corr_show_other)
    y_bins = create_bins(df[[y_var]], y_var,
                         input$corr_bin_count,
                         input$corr_show_other)
    
    bdf = df |> mutate(
      x_binned = x_bins,
      y_binned = y_bins
    ) |> drop_na(x_binned, y_binned)
    
    bdf |> group_by(x_binned, y_binned, .drop=FALSE) |>
      summarize(
        count     = n(),
        alz_count = sum(`Alzheimer's Diagnosis`=="Yes", na.rm=TRUE),
        alz_pct   = mean(`Alzheimer's Diagnosis`=="Yes", na.rm=TRUE)*100,
        avg_age   = mean(Age, na.rm=TRUE),
        .groups = "drop"
      ) |> filter(count>0)
  })
  
  output$corr_plot = renderPlotly({
    df = summary_corr()
    var = input$corr_summary_metric
    
    if (var=="alz_pct") {
      title = paste("Alzheimer's % by", input$corr_xvar, "vs", input$corr_yvar)
      scale_fill = scale_fill_gradient(
        low="#00b300", high="#e10000", name="Alz %"
      )
    } else if (var=="avg_age") {
      title = paste("Avg Age by", input$corr_xvar, "vs", input$corr_yvar)
      scale_fill = scale_fill_viridis_c(option="viridis",
                                        name="Avg Age")
    } else {
      title = paste("Count by", input$corr_xvar, "vs", input$corr_yvar)
      scale_fill = scale_fill_viridis_c(option="plasma",
                                        name="Count", trans="sqrt")
    }
    
    p = ggplot(df, aes(
      x = x_binned, y = y_binned,
      fill = .data[[var]],
      text = paste0(
        input$corr_xvar, ": ", x_binned,
        "<br>", input$corr_yvar, ": ", y_binned,
        "<br>",
        if(var=="alz_pct") paste0("Alz %: ", round(alz_pct,1),"%")
        else if(var=="avg_age") paste0("Avg Age: ", round(avg_age,1))
        else paste0("Count: ", count)
      )
    )) +
      geom_point(shape=21, size=7, alpha=0.8) +
      labs(title=title, x=input$corr_xvar, y=input$corr_yvar) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle=45, hjust=1, size=9),
        axis.text.y = element_text(size=9),
        plot.title  = element_text(hjust=0.5)
      ) +
      scale_fill + scale_size(range=c(2,10)) +
      guides(size="none")
    
    ggplotly(p, tooltip="text") |>
      layout(margin=list(b=100,l=100))
  })
  
  output$corr_explanation = renderUI({
    metric = switch(input$corr_summary_metric,
                    alz_pct = "the % with Alzheimer's",
                    avg_age = "the average age",
                    count   = "the number of people (sqrt scale)"
    )
    HTML(paste0(
      "<p>This bubble chart groups by ", input$corr_xvar,
      " and ", input$corr_yvar, ". Each bubble’s color shows ",
      metric, ".</p><p>Hover for details.</p>"
    ))
  })
  
  
  #—— 4) Health Conditions Heatmap —————————————————————————————————————
  
  selected_predictors <- reactive({
    c(input$physicalFactors, input$mentalFactors, input$lifestyleFactors, input$geneticFactors)
  })
  
  observeEvent(input$selectAllPhysical, {
    if(input$selectAllPhysical) {
      updateCheckboxGroupInput(session, "physicalFactors", 
                               selected = c("Diabetes", "Hypertension"))
    } else {
      updateCheckboxGroupInput(session, "physicalFactors", selected = c())
    }
  })
  
  observeEvent(input$selectAllMental, {
    if(input$selectAllMental) {
      updateCheckboxGroupInput(session, "mentalFactors", 
                               selected = c("Depression Level", "Stress Level"))
    } else {
      updateCheckboxGroupInput(session, "mentalFactors", selected = c())
    }
  })
  
  observeEvent(input$selectAllLifestyle, {
    if(input$selectAllLifestyle) {
      updateCheckboxGroupInput(session, "lifestyleFactors", 
                               selected = c("Activity Level", "Sleep Quality", "Social Engagement"))
    } else {
      updateCheckboxGroupInput(session, "lifestyleFactors", selected = c())
    }
  })
  
  filtered_data <- reactive({
    if(input$country == "All Countries") {
      data <- alzheimers
    } else {
      data <- alzheimers %>% filter(Country == input$country)
    }
    return(data)
  })
  
  heatmap_data <- reactive({
    alzheimers <- filtered_data() %>% 
      filter(`Alzheimer's Diagnosis` == "Yes")
    if(nrow(alzheimers) == 0) {
      return(NULL)
    }
    
    conditions <- selected_predictors()
    if(length(conditions) == 0) return(NULL)
    
    combined_data <- list()
    
    condition_map <- list(
      "Diabetes" = "Diabetes",
      "Hypertension" = "Hypertension",
      "Depression Level" = "Depression Level",
      "Activity Level" = "Physical Activity Level",
      "Sleep Quality" = "Sleep Quality",
      "Social Engagement" = "Social Engagement Level",
      "Family History of Alzheimer's" = "Family History of Alzheimer's",
      "Stress Level" = "Stress Levels"
    )
    
    for(condition in conditions) {
      if(condition %in% names(condition_map)) {
        column_name <- condition_map[[condition]]
        temp <- melt(table(alzheimers$Gender, alzheimers[[column_name]]))
        colnames(temp) <- c("Gender", "Condition", "Count")
        temp$HealthFactor <- condition
        
        if(input$normalizeCounts) {
          total_by_gender <- temp %>%
            group_by(Gender, HealthFactor) %>%
            summarize(Total = sum(Count), .groups = "drop")
          
          temp <- temp %>%
            left_join(total_by_gender, by = c("Gender", "HealthFactor")) %>%
            mutate(Count = (Count / Total) * 100) %>%
            select(-Total)
        }
        
        combined_data[[length(combined_data)+1]] <- temp
      }
    }
    
    if (length(combined_data) == 0) return(NULL)
    
    heatmap_data <- do.call(rbind, combined_data)
    
    heatmap_data$HealthFactor <- factor(heatmap_data$HealthFactor, levels = conditions)
    
    heatmap_data$Condition <- as.character(heatmap_data$Condition)
    
    heatmap_data$Condition <- ifelse(
      heatmap_data$Condition %in% c("Yes", "High", "Good"), "+", "-")
    
    heatmap_data$Condition <- factor(heatmap_data$Condition, levels = c("-", "+"))
    
    return(heatmap_data)
  })
  
  output$heatmap <- renderPlot({
    data <- heatmap_data()
    if (is.null(data) || nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "Please select at least one health condition to display") +
               theme_void() +
               xlim(0, 1) + 
               ylim(0, 1))
    }
    
    color_scheme <- input$colorScheme
    if(color_scheme == "colorblind") {
      male_colors <- scale_fill_gradient(low = "#E6F0FF", high = "#0072B2", 
                                         name = if(input$normalizeCounts) "% of Male Cases" else "Male Cases")
      female_colors <- scale_fill_gradient(low = "#FFE6E6", high = "#D55E00", 
                                           name = if(input$normalizeCounts) "% of Female Cases" else "Female Cases")
    } else if(color_scheme == "sequential") {
      male_colors <- scale_fill_gradient(low = "#E6E6FF", high = "#3333CC", 
                                         name = if(input$normalizeCounts) "% of Male Cases" else "Male Cases")
      female_colors <- scale_fill_gradient(low = "#F2E6FF", high = "#8B00CC", 
                                           name = if(input$normalizeCounts) "% of Female Cases" else "Female Cases")
    } else {
      male_colors <- scale_fill_gradient2(low = "#4575B4", mid = "#FFFFBF", high = "#D73027", 
                                          midpoint = median(data$Count[data$Gender == "Male"]), 
                                          name = if(input$normalizeCounts) "% of Male Cases" else "Male Cases")
      female_colors <- scale_fill_gradient2(low = "#4575B4", mid = "#FFFFBF", high = "#D73027", 
                                            midpoint = median(data$Count[data$Gender == "Female"]), 
                                            name = if(input$normalizeCounts) "% of Female Cases" else "Female Cases")
    }
    
    male_data <- data %>% filter(Gender == "Male")
    female_data <- data %>% filter(Gender == "Female")
    
    if(input$showLabels) {
      male_labels <- geom_text(data = male_data, 
                               aes(x = Condition, y = HealthFactor, 
                                   label = if(input$normalizeCounts) 
                                     sprintf("%.1f%%", Count) else 
                                       as.character(Count)), 
                               color = "black", size = 3.5)
      
      female_labels <- geom_text(data = female_data, 
                                 aes(x = Condition, y = HealthFactor, 
                                     label = if(input$normalizeCounts) 
                                       sprintf("%.1f%%", Count) else 
                                         as.character(Count)), 
                                 color = "black", size = 3.5)
    } else {
      male_labels <- NULL
      female_labels <- NULL
    }
    
    title <- paste("Alzheimer's Cases by Gender and Health Conditions", 
                   if (input$country != "All Countries") paste("in", input$country) else "")
    
    subtitle <- if(input$normalizeCounts) {
      "Values shown as percentages within each gender group"
    } else {
      "Values show actual case counts"
    }
    
    ggplot() +
      geom_tile(data = male_data, 
                aes(x = Condition, y = HealthFactor, fill = Count), 
                color = "black") +
      geom_hline(yintercept = seq(1.5, length(unique(data$HealthFactor))-0.5, 1), 
                 color = "darkgrey", 
                 linewidth = .5) +
      male_colors +
      male_labels +
      new_scale_fill() +
      
      geom_tile(data = female_data, 
                aes(x = Condition, y = HealthFactor, fill = Count), 
                color = "black") +
      female_colors +
      female_labels +
      
      facet_grid(. ~ Gender, scales = "free_x", space = "free_x") +
      
      labs(title = title,
           subtitle = subtitle,
           x = "Condition Status", 
           y = "Health Factor") +
      
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.text.x = element_text(size = 28),
        strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#666666"),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.8, "cm"),
        panel.spacing.x = unit(0.5, "lines"),
        panel.border = element_rect(color = "white", fill = NA, size = .5)
      )
  })
  
  
  #—— 5) Pie Charts ———————————————————————————————————————————————————
  
  filtered_pie = reactive({
    df = alz_df |> 
      mutate(
        Diagnosis = `Alzheimer's Diagnosis`,
        `Education Level (Binned)` = cut(`Education Level`,
                                         breaks = c(-Inf, 4, 9, 14, Inf),
                                         labels = c("Low", "Medium", "High", "Very High"),
                                         right  = TRUE
        ),
        `BMI (Binned)` = cut(BMI,
                             breaks = c(-Inf, 24.9, 29.9, Inf),
                             labels = c("Normal", "Overweight", "Obese"),
                             right = TRUE
        ),
        `Cognitive Score (Binned)` = cut(`Cognitive Test Score`,
                                         breaks = c(-Inf, 46, 64, 82, Inf),
                                         labels = c("Low", "Medium", "High", "Very High"),
                                         right = TRUE
        )
      ) |> 
      filter(
        Age >= input$pie_ageRange[1],
        Age <= input$pie_ageRange[2],
        if (input$pie_genderFilter != "All") Gender == input$pie_genderFilter else TRUE,
        if (input$pie_countryFilter != "All") Country == input$pie_countryFilter else TRUE
      )
    return(df)
  })
  
  summary_data <- reactive({
    df  <- filtered_pie()
    req(input$pie_varChoice)
    var <- input$pie_varChoice
    
    var_mapped <- dplyr::case_when(
      var == "Education Level" ~ "Education Level (Binned)",
      var == "BMI" ~ "BMI (Binned)",
      var == "Cognitive Test Score" ~ "Cognitive Score (Binned)",
      TRUE ~ as.character(var) 
    )
    
    result <- df %>%
      group_by(Diagnosis, .data[[var_mapped]]) %>%
      summarise(Count = n(), .groups = "drop") %>%
      filter(!is.na(.data[[var_mapped]])) %>%
      group_by(Diagnosis) %>%
      mutate(
        Total = sum(Count),
        Percentage = round(Count / Total * 100, 1)
      ) %>%
      ungroup()
    
    comparison <- result %>%
      select(Diagnosis, !!as.name(var_mapped), Percentage) %>%
      tidyr::pivot_wider(names_from = Diagnosis, values_from = Percentage) %>%
      mutate(Difference = Yes - No) %>%
      arrange(desc(abs(Difference)))
    
    yes_total <- sum(df$Diagnosis == "Yes", na.rm = TRUE)
    no_total  <- sum(df$Diagnosis == "No",  na.rm = TRUE)
    
    list(
      detail = result,
      comparison = comparison,
      yes_total = yes_total,
      no_total = no_total,
      yes_percent = round(yes_total / (yes_total + no_total) * 100, 1)
    )
  })
  
  # Key Insights panels
  output$keyInsights <- renderUI({
    data <- summary_data()
    var  <- input$pie_varChoice 
    overall_rate <- overall_rate_static 
    
    # Check if data is available for insights
    if (nrow(data$comparison) == 0 || is.null(data$comparison) || all(is.na(data$comparison))) {
      # Display counts and percentage if available, otherwise message
      total_filtered = data$yes_total + data$no_total
      content <- if (total_filtered > 0) {
        tags$div(
          tags$div( 
            style = "display: flex; justify-content: space-between; margin-bottom: 15px;",
            tags$div(
              style = "flex: 1; text-align: center; background-color: #e8f4f8; padding: 10px; border-radius: 5px; margin-right: 10px;",
              tags$h4(data$yes_total, style = "margin: 0; color: #2980b9;"),
              tags$p("Alzheimer's Patients", style = "margin: 0;")
            ),
            tags$div(
              style = "flex: 1; text-align: center; background-color: #f8f4e8; padding: 10px; border-radius: 5px;",
              tags$h4(paste0(data$yes_percent, "%"), style = "margin: 0; color: #d35400;"),
              tags$p("of Selected Population", style = "margin: 0;")
            )
          ),
          tags$p("Insufficient data for the selected variable and filters to generate detailed insights.", style="text-align: center; font-style: italic; margin-top: 15px;")
        )
      } else {
        tags$p("No individuals found matching the selected filters.", style="text-align: center; font-style: italic; margin-top: 15px;")
      }
      return(content)
    }
    
    top_diff <- data$comparison[1, ]
    
    # Map variable name again for indexing
    var_mapped <- dplyr::case_when(
      var == "Education Level"       & "Education Level (Binned)" %in% names(data$comparison) ~ "Education Level (Binned)",
      var == "BMI"                   & "BMI (Binned)" %in% names(data$comparison) ~ "BMI (Binned)",
      var == "Cognitive Test Score"  & "Cognitive Score (Binned)" %in% names(data$comparison) ~ "Cognitive Score (Binned)",
      TRUE ~ var
    )
    
    raw_value <- top_diff[[var_mapped]]
    
    factor_value <- if (is.numeric(raw_value)) {paste0("level ", round(raw_value, 1)) # Use level for numeric/binned continuous
    } else {
      as.character(raw_value) # Use value for categorical
    }
    
    yes_percent <- ifelse(is.na(top_diff[["Yes"]]) | !is.finite(top_diff[["Yes"]]), 0, top_diff[["Yes"]])
    no_percent  <- ifelse(is.na(top_diff[["No"]]) | !is.finite(top_diff[["No"]]), 0, top_diff[["No"]])
    difference  <- ifelse(is.na(top_diff[["Difference"]]) | !is.finite(top_diff[["Difference"]]), 0, top_diff[["Difference"]])
    
    direction   <- ifelse(difference > 0, "higher", "lower")
    rel_diff    <- round(abs(difference), 1)
    
    # Format percentages for display
    yes_percent_fmt <- paste0(round(yes_percent, 1), "%")
    no_percent_fmt  <- paste0(round(no_percent, 1), "%")
    
    # Construct Key Finding sentence dynamically
    key_finding_text <- if(rel_diff > 0) {
      paste0("For the variable '", var, "', the group '", factor_value,
             "' shows the largest percentage point difference. This group makes up ",
             yes_percent_fmt, " of Alzheimer's patients compared to ", no_percent_fmt,
             " of non-Alzheimer's patients in this selection (a ", rel_diff, " point difference).")
    } else {
      paste0("For the variable '", var, "', no significant percentage point difference was observed ",
             "between Alzheimer's and non-Alzheimer's patients for any category within the selected population.")
    }
    
    
    # Construct Demographic label based on filters
    demo_components <- c()
    if (input$pie_genderFilter != "All") demo_components <- c(demo_components, input$genderFilter)
    if (input$pie_genderFilter != "All") demo_components <- c(demo_components, input$countryFilter)
    
    # Always include age range
    age_range_str <- paste0("age ", input$pie_ageRange[1], "–", input$pie_ageRange[2])
    demo_components <- c(demo_components, age_range_str)
    demo_label <- paste(demo_components, collapse = ", ")
    
    # Calculate comparison to overall rate
    current_rate   <- data$yes_percent 
    demo_change    <- round(current_rate - overall_rate, 1)
    demo_direction <- ifelse(demo_change > 0, "higher than", ifelse(demo_change < 0, "lower than", "the same as"))
    demo_abs       <- abs(demo_change)
    
    # Construct Demographic Insight sentence dynamically
    demographic_insight_text <- paste0(
      "The selected group (", demo_label, ") has an Alzheimer's diagnosis rate of ",
      current_rate, "%. This is ", demo_abs, "% points ", demo_direction,
      " the overall population's rate of ", overall_rate, "%."
    )
    
    
    # Render the UI 
    tags$div(
      # Top row: Counts and Percentage
      tags$div(
        style = "display: flex; justify-content: space-between; margin-bottom: 15px;",
        tags$div(style = "flex: 1; text-align: center; background-color: #e8f4f8; padding: 10px; border-radius: 5px; margin-right: 10px;",
                 tags$h4(data$yes_total, style = "margin: 0; color: #2980b9;"),
                 tags$p("Alzheimer's Patients", style = "margin: 0;")
        ),
        tags$div(style = "flex: 1; text-align: center; background-color: #f8f4e8; padding: 10px; border-radius: 5px;",
                 tags$h4(paste0(current_rate, "%"), style = "margin: 0; color: #d35400;"),
                 tags$p("of Selected Population", style = "margin: 0;")
        )
      ),
      # Bottom row: Insight Boxes
      tags$div(style = "display: flex; gap: 10px; margin-bottom: 10px;",
               tags$div(style = "flex: 1; background-color: white; padding: 15px; border-left: 4px solid #3498db;",
                        tags$p(
                          tags$strong("Key Finding:"), br(), 
                          key_finding_text)
               ),
               tags$div(
                 style = "flex: 1; background-color: white; padding: 15px; border-left: 4px solid #2ecc71;",
                 tags$p(
                   tags$strong("Demographic Insight:"), br(), 
                   demographic_insight_text 
                 )
               )
      )
    ) 
  }) 
  output$pie_chart <- renderPlotly({
    req(input$pie_varChoice)
    var <- input$pie_varChoice
    
    var_mapped <- dplyr::case_when(
      var == "Education Level" ~ "Education Level (Binned)",
      var == "BMI" ~ "BMI (Binned)",
      var == "Cognitive Test Score" ~ "Cognitive Score (Binned)",
      TRUE ~ as.character(var) 
    )
    
    df <- filtered_pie() %>% filter(!is.na(.data[[var_mapped]]))
    
    yes_df <- df %>%
      filter(Diagnosis == "Yes") %>%
      group_by(level = .data[[var_mapped]]) %>%   
      summarise(n = n(), .groups = "drop")
    no_df <- df %>%
      filter(Diagnosis == "No") %>%
      group_by(level = .data[[var_mapped]]) %>% 
      summarise(n = n(), .groups = "drop")
    
    yes_cols <- colorRampPalette(c("#1f77b4", "#aec7e8"))(nrow(yes_df))
    no_cols  <- colorRampPalette(c("#e74c3c", "#f5b7b1"))(nrow(no_df))
    
    plot_ly() %>%
      add_pie(
        data       = yes_df,
        labels     = ~level,
        values     = ~n,
        name       = "Alzheimer's Yes",
        marker     = list(colors = yes_cols,
                          line   = list(color = "#FFFFFF", width = 1)),
        textinfo     = "label+percent+value",
        texttemplate = "%{label} (%{value})<br>%{percent}",
        textposition = "auto",
        hoverinfo  = "label+percent+value",
        domain     = list(x = c(0, 1), y = c(0, 1)),
        hole       = 0.6,
        sort       = FALSE,
        direction  = "clockwise",
        textfont   = list(size = 18, color = 'black', family = 'Arial', weight = 'bold'),
        outsidetextfont = list(size = 18, color = 'black', family = 'Arial', weight = 'bold')
      ) %>%
      add_pie(
        data       = no_df,
        labels     = ~level,
        values     = ~n,
        name       = "Alzheimer's No",
        marker     = list(colors = no_cols,
                          line   = list(color = "#FFFFFF", width = 1)),
        textinfo     = "label+percent+value",
        texttemplate = "%{label} (%{value})<br>%{percent}",
        textposition   = "inside",
        insidetextfont = list(size = 16, color = 'black', family = 'Arial', weight = 'bold'),
        hoverinfo  = "label+percent+value",
        domain     = list(x = c(0.23,0.77), y = c(0.23,0.77)),
        hole       = 0.3,    
        sort       = FALSE,
        showlegend = FALSE
      ) %>%
      layout(
        title = list(
          text = paste(var, "Distribution (Outer: Alzheimer's, Inner: Non-Alzheimer's)"),
          font = list(size = 16, family = 'Arial', weight = 'bold')
        ),
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0.5, y = -0.05, xanchor = "center",
                      font = list(size = 15, family = 'Arial', weight = 'bold')),
        margin = list(t = 50, b = 50, l = 0, r = 0),
        uniformtext = list(minsize = 16, mode = 'show')
      )
  })}

#── RUN APP ─────────────────────────────────────────────────────────────────

shinyApp(ui=ui, server=server)
