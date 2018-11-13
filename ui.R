source("globals.R")

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    tags$head(includeScript("google-analytics.js")),
    leaflet::leafletOutput("map", width = "100%", height = "100%"),
    shiny::titlePanel("under development",
                     windowTitle = 'Debris Encounter Forecast'),
    shiny::absolutePanel(top = 10, right = 10,

        # dev warning
        shiny::tags$textarea(id="message", rows=1, cols=30,
                             "** Under Development **"),

        #basemap - select one
        shiny::selectInput("basemap_name", "Base Map",
                           choices = c(
                               "None",
                               "OpenStreetMap.Mapnik",
                               "Esri.WorldImagery",
                               "Esri.WorldStreetMap",
                               "OpenTopoMap"),
                           selected = "OpenStreetMap.Mapnik"),
        #overlay - date, opacity and color
        shiny::selectInput("overlay_version", "Overlay Version",
            choices = c("v1.00", "v1.01"),
            selected = DEFAULTS$version),
        shiny::sliderInput("overlay_date", "Overlay Date",
                           min = DEFAULTS$date_range[1],
                           max = DEFAULTS$date_range[2],
                           value = DEFAULTS$date_range[1]),
        shiny::sliderInput("overlay_opacity", "Overlay Opacity",
                           min = 0,
                           max = 1,
                           step = 0.1,
                           value = DEFAULTS$ovrly_opacity),
        shiny::selectInput("overlay_color", "Overlay Color",
            choices = c("None", "Oranges", "Purples", "Reds", "Blues", "Greens"),
            selected = "Oranges"),
        shiny::sliderInput("points_date", "Points Date",
                           min = DEFAULTS$date_range[1],
                           max = DEFAULTS$date_range[2],
                           value = DEFAULTS$date_range),
        shiny::selectInput("points_color", "Points Color",
                           choices = names(COLOR_LUT),
                           selected = DEFAULTS$obs_color)
    ) #absolutePanel
)
