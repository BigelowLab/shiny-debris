source("globals.R")

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    tags$head(includeScript("google-analytics.js")),
    tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}"),
    leaflet::leafletOutput("map", width = "100%", height = "100%"),
    shiny::titlePanel("under development",
                     windowTitle = 'Debris Encounter Likelihood'),
    shiny::absolutePanel(top = 10, right = 10,

        # dev warning
        shiny::tags$textarea(id="message", rows=1, cols=30,
                             "** Under Development **"),

        if(DEVMODE) shiny::selectInput("xcast_color", "Colors",
                           choices = PALNAMES,
                           selected = PALNAME) else NULL,
        shiny::selectInput("xcast_version", "Debris Type",
                          choices = unname(GLOBALS$vchoices),
                          selected = unname(GLOBALS$vchoices[1])),
        shiny::sliderInput("xcast_date", "Date",
                           min = GLOBALS$date_range[1],
                           max = GLOBALS$date_range[2],
                           value = GLOBALS$date_range[2]),

        shiny::actionButton("update_button", "Up to date",
                            icon = icon("thumbs-up")),

        shiny::tags$p(""),

        shiny::checkboxInput(inputId='obs_show',
                             #label = "Show observations near date",
                             label = sprintf("Show sightings within %i/+%i days of date",
                                             GLOBALS$obs_window[1],
                                             GLOBALS$obs_window[2]),
                             value = GLOBALS$show_obs),
        shiny::tags$p(""),

        #shiny::downloadButton("save_png"),

        shiny::actionButton(inputId='report',
                            label = "Report debris",
                            icon = shiny::icon("edit"),
                            onclick = GLOBALS$report_links["report"]),

        shiny::tags$p(""),

        shiny::actionButton(inputId='home',
                            label = "Return to Ecocast home",
                            icon = shiny::icon("home"),
                            onclick = GLOBALS$report_links["home"])
    ) #absolutePanel
)
