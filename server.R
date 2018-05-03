
server <- function(input, output){

    # reactive when user selects basemap by name
    rx_basemap_select <- reactive({
        input$basemap_name
    })

    # reactive when the overlay_date changes - flips overlay so we can tell it changed
    rx_ovrlay <- reactive({
        read_xcast(input$overlay_date, version = input$overlay_version)
    })
    # reactive when user selects overlay palette
    rx_ovrlay_pal <- reactive({
       if (tolower(input$overlay_color) != "none"){
            pal <- colorNumeric(
                RColorBrewer::brewer.pal(DEFAULTS$npal,input$overlay_color),
                domain = c(0,1),
                na.color = "transparent")
        } else {
            pal <- NULL
        }
       pal
    })

    # reactive when user selects overlay opacity
    rx_ovrlay_opacity <- shiny::reactive({
        input$overlay_opacity
    })

    rx_filtered_obs <- shiny::reactive({
        filter_obs(OBS, dates = input$points_date, version = input$overlay_version)
    })

    rx_obs_color <- shiny::reactive({
        #unname to avoid the dreaded "Input to asJSON(keep_vec_names=TRUE) is a
        # named vector. In a future version of jsonlite..."
        unname(COLOR_LUT[input$points_color])
    })

    output$map <- renderLeaflet({
        pal <- DEFAULTS$pal
        lgnd <- legend_from_pal(pal)
        obs <- filter_obs()
        bb <- get_bb()
        leaflet() %>%
            leaflet::fitBounds(bb[1], bb[3], bb[2], bb[4]) %>%
            leaflet::mapOptions(zoomToLimits = "never")  %>%
            leaflet::addProviderTiles(provider="OpenStreetMap.Mapnik",
                                      group = 'tiles') %>%
            leaflet::addRasterImage(read_xcast(DEFAULTS$date_range[1],
                                               version = DEFAULTS$version),
                                    group = "overlay",
                                    project = FALSE,
                                    opacity = DEFAULTS$ovrly_opacity,
                                    colors = DEFAULTS$ovrly_pal) %>%
            leaflet::addCircles(lng = obs$lon, lat = obs$lat,
                                group = 'obs',
                                radius = DEFAULTS$obs_radis,
                                color = DEFAULTS$obs_color) %>%
            leaflet::addLegend("bottomleft",
                               colors = lgnd$colors,
                               values = lgnd$values,
                               labels = lgnd$labels,
                               opacity = 1)
    })

    # draw for new basemap - then add overlay (it seems to be hidden)
    observe({
        pal <- rx_ovrlay_pal()
        lgnd <- legend_from_pal(pal)
        basemap <- rx_basemap_select()
        proxy <- leafletProxy("map") %>%
            clearGroup("tiles")         # for a fresh basemap
        if (tolower(basemap) != 'none')
            proxy <- proxy %>%
                leaflet::addProviderTiles(provider=basemap,
                    group = 'tiles')
        if (!is.null(pal)){
            proxy <- proxy %>%
                clearControls() %>%        # for a fresh legend
                clearGroup("overlay") %>%  # for a fresh overlay
                leaflet::addLegend("bottomleft",
                               colors = lgnd$colors,
                               values = lgnd$values,
                               labels = lgnd$labels,
                               opacity = 1) %>%
                leaflet::addRasterImage(rx_ovrlay(),
                                    group = 'overlay',
                                    project=FALSE,
                                    opacity = rx_ovrlay_opacity(),
                                    colors = pal)

        }
    })

    # draw for overlay
    observe({
        pal <- rx_ovrlay_pal()
        lgnd <- legend_from_pal(pal)

        proxy <- leafletProxy("map") %>%
            clearControls() %>%        # for a fresh legend
            clearGroup("overlay")       # for a fresh overlay
        if (!is.null(pal)) proxy <- proxy %>%
            leaflet::addLegend("bottomleft",
                               colors = lgnd$colors,
                               values = lgnd$values,
                               labels = lgnd$labels,
                               opacity = 1) %>%
            leaflet::addRasterImage(rx_ovrlay(),
                                   group = 'overlay',
                                   project=FALSE,
                                   opacity = rx_ovrlay_opacity(),
                                   colors = pal)
    })

    observe({
        # draw for points
        color <- rx_obs_color()
        obs <- rx_filtered_obs()
        proxy <- leafletProxy("map") %>%
            clearGroup("obs")
        if (!is.na(color))  proxy <- proxy %>%
            leaflet::addCircles(lng = obs$lon, lat = obs$lat,
                                group = 'obs',
                                radius = DEFAULTS$obs_radius,
                                color = color)
    })

}
