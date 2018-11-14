
server <- function(input, output, session){

    rx_xcast_date <- reactive({
        input$xcast_date
    })
    rx_xcast_version <- reactive({
        input$xcast_version
    })
    rx_show_obs <- reactive({
        input$obs_show
    })
    rx_update_button <- reactive({
        input$update_button
    })

    add_raster <- function(map,
                           dt = GLOBALS$date,
                           version = GLOBALS$version){
        if (DEVMODE) cat("add_raster date:", format(dt, "%Y-%m-%d"), " version:", version, "\n")
        x <- read_xcast(dt = dt, version = version)
        map %>%
            leaflet::addRasterImage(x,
                                    group = "xcast",
                                    project = FALSE,
                                    opacity = GLOBALS$xcast_opacity,
                                    colors = GLOBALS$xcast_pal)
    }
    add_legend <- function(map){
        if (DEVMODE) cat("add_legend\n")
        lgnd <- legend_from_pal()
        map %>%
            leaflet::addLegend("bottomleft",
                                colors = lgnd$colors,
                                values = lgnd$values,
                                labels = lgnd$labels,
                                opacity = 1)

    }

    add_circles <- function(map, obs = filter_obs()){
        if (DEVMODE) cat("add_circles n = ", nrow(obs), "\n")
        if (nrow(obs) > 0){
            map %>%
                leaflet::addCircles(lng = obs$lon, lat = obs$lat,
                                group = 'obs',
                                radius = GLOBALS$obs_radius,
                                color = GLOBALS$obs_color)
        }
        map
    }

    output$map <- renderLeaflet({
        obs <- filter_obs()
        bb <- get_bb()

        leaflet() %>%
            leaflet::fitBounds(bb[1], bb[3], bb[2], bb[4]) %>%
            leaflet::mapOptions(zoomToLimits = "never")  %>%
            leaflet::addProviderTiles(provider="OpenStreetMap.Mapnik",
                                      group = 'tiles') %>%
            add_raster(dt = GLOBALS$date, version = GLOBALS$version) %>%
            add_legend()
    })

    # output$save_png <- downloadHandler(
    #
    #     filename = format(isolate(rx_xcast_date()), "%Y-%m-%d_debrismap.png"),
    #
    #     content = function(filename) {
    #         mapshot( x = leafletProxy("map"),
    #                   file = file.path("~", file),
    #                   cliprect = "viewport", # the clipping rectangle matches the height & width from the viewing port
    #                   selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
    #         )
    #     } # end of content() function
    # )

    observeEvent(c(input$xcast_date, input$xcast_version),{
        updateActionButton(session, "update_button",
                           label = "   ** Click To Update ** ",
                           icon = icon("refresh"))
    },
    ignoreInit = TRUE)

    observeEvent(input$update_button,{
        proxy <- leafletProxy("map") %>%
            clearControls() %>%
            clearGroup("xcast") %>%
            clearGroup("points")   %>%
            add_legend() %>%
            add_raster(dt = isolate(input$xcast_date),
                       version = version_choice(isolate(input$xcast_version)) )
        if (isolate(rx_show_obs())) {
            obs <- filter_obs(date = isolate(input$xcast_date),
                              version = version_choice(isolate(input$xcast_version)) )
            proxy <- proxy %>% add_circles(obs = obs)
        }
        updateActionButton(session, "update_button",
                           label = "Up to date",
                           icon = icon("thumbs-up"))
    },
    ignoreInit = TRUE)

    observeEvent(input$obs_show,{
        # draw for points
        proxy <- leafletProxy("map") %>%
            clearGroup("obs")

        if (input$obs_show) {
            obs <- filter_obs(date = isolate(input$xcast_date),
                              version = version_choice(isolate(input$xcast_version)))
            proxy <- proxy %>%
                add_circles(obs = obs)
        }
    },
    ignoreInit = TRUE)


}
