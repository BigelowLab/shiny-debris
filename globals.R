library(leaflet)
library(shiny)
library(raster)
library(RColorBrewer)
library(mapview)
library(debrismaptools)
suppressPackageStartupMessages(library(dplyr))
options(shiny.sanitize.errors = FALSE)

# BASEMAPS see https://github.com/leaflet-extras/leaflet-providers#providers
# ESRI provides map data gratis with ESRI user registered (btupper@bigelow.org)
# OPENSTREETMAPS provides map data gratis

DATAPATH = if(system("echo $HOSTNAME", intern = TRUE) == 'gale.local')
    '/Users/Shared/code/R/sites/_data/debrismap/versions/v1' else
        '/mnt/ecocast/projectdata/debrismap/versions/v1'
OBS = debrismaptools::read_obs("daily-current")
OBS$Date <- as.Date(OBS$sighted)
TOPO <- debrismaptools::read_predictors("topo")


#' List the xcast files
#'
#' @param version character version stamp (default is 'v1.00')
#' @param form either 'filename' (default) or 'date'
#' @param path the directory to the data
#' @return character or Date vector
list_xcast <- function(version = 'v1.00',
                       form = c("filename", "date")[1],
                       path = DATAPATH){

    ff <- list.files(file.path(path, version, 'xcast'),
                     pattern = glob2rx("*.grd"),
                     full.names = TRUE)
    if (tolower(form[1]) == 'date'){
        ff <- as.Date(basename(ff), format = '%Y-%m-%d.grd')
    }
    ff
}

xcast_template <- function(value = 0,
                           bb = c(-83.4, -78.5, 32, 35.2),
                           #dim = c(192, 294),
                           dim = c(2,2),
                           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"){
    r <- raster::raster(
        matrix(value[1], ncol = dim[2], nrow = dim[1]),
        xmn = bb[1], xmx = bb[2], ymn = bb[3], ymx = bb[4],
        crs = crs)
    #r[1] <- 1
    r
}

#' Read one or more xcast by date
#'
#'
#' @param dt the Date to read, if missing then read all
#' @param version the character version stamp to read (default 'v1.00')
#' @param path path to the data
#' @returns raster::stack with Z values set to date
read_xcast <- function(dt = as.Date("2018-11-13"),
    version = 'v1.00',
    path = DATAPATH,
    threshold = c(NA, -10)[1]){

    if (FALSE){
        dt = as.Date("2018-11-13")
        version = 'v1.00'
        path = DATAPATH
        threshold = c(NA, -10)[1]
    }

    if (DEVMODE){
        cat("read_xcast date", format(dt, "%Y-%m-%d"), "\n")
        cat("           version", version, "\n")
        ff = file.path(path, version, 'xcast', format(dt[1], "%Y-%m-%d.grd"))
        cat("           file", ff, "\n")
        cat("           exists", file.exists(ff), "\n")
    }

    if (!is.na(dt)){
        ff = file.path(path, version, 'xcast', format(dt[1], "%Y-%m-%d.grd"))
        if (file.exists(ff[1])){
            x = raster::raster(ff[1])
        } else {
            x = xcast_template()
        }
    } else {
        ff = list_xcast(version = version, form = 'filename', path = path)
        x <- raster::stack(ff)
        dt <- as.Date(basename(ff), format = '%Y-%m-%d.grd')
        x <- raster::setZ(x, dt)
    }
    if (!is.null(x) && !is.na(threshold)) x[TOPO <= threshold] <- NA
    if (!is.null(x)) x <- leaflet::projectRasterForLeaflet(x)
    x
}

#' Get a predefined bounding box
#'
#' @param where character, see celmap::get_bb()
#' @return a 4 element bounding box
get_bb <- function(input) {
    # see http://rstudio.github.io/leaflet/shiny.html
    if (!missing(input) && !is.null(input$map_bounds)){
        b <- input$map_bounds
        bb <- c(b$west, b$east, b$south, b$north)
    } else {
        bb <- debrismaptools::debrismap_bb()
    }
    bb
}

DEVMODE    = interactive()
NPAL       = 9
PALNAME    = 'Oranges'
COLOR_LUT  = c("None" = NA, Yellow = "#FFFF00", Orange = "#FFA500",
             Blue = "#0000FF", White = "#FFFFFF")
date_range <- range(list_xcast(form = 'date'))
GLOBALS = list(
    date_range = date_range,
    date       = date_range[2],
    version    = 'v1.00',
    vchoices   = c(
                   'v1.00' = 'All debris',
                   'v1.01' = 'Just styrofoam'),
    npal       = NPAL,
    xcast_pal  =  leaflet::colorNumeric(RColorBrewer::brewer.pal(NPAL,PALNAME),
                                        domain = c(0,1),
                                        na.color = "transparent"),
    xcast_opacity    = 0.7,
    show_obs         = FALSE,
    obs_window       = c(-30, 30),
    obs_radius       = 20,
    obs_color        = 'Yellow',
    report_links     = c(
        'report' = "window.open('https://www.anecdata.org/projects/view/122', '_blank')",
        'home'      = "window.open('https://www.bigelow.org/science/lab/computational-science/','_blank')"
    ))

#' A function to filter points between two dates
#' @param x the PTS tibble or a subset thereof
#' @param date a date shown on xcast
#' @param win the date window -/+ days
#' @param version the xcast version
#' @return a tibble with zero or more rows
filter_obs <- function(x = OBS,
                       date = GLOBALS$date,
                       win = GLOBALS$obs_window,
                       version = GLOBALS$version){
    dates <- date + GLOBALS$obs_window
    x <- x %>% dplyr::filter(
        dplyr::between(`Date`, dates[1], dates[2]))
    if (version == 'v1.01') x <- x %>%
        dplyr::filter(`Debris Type` == 'Styrofoam')
    x
}

#' Given a palette function, generate the colors, values and labels for a
#' custom legend
#' @param pal colorNumeric palette function
#' @return a three element named list of colors, values, and labels
legend_from_pal <- function(pal = GLOBALS$xcast_pal){
    if (is.null(pal)) pal <- GLOBALS$xcast_pal
    lgnd_colors <- pal(seq(from = 0, to = 1, length = GLOBALS$npal))
    lgnd_labels <- rep("", GLOBALS$npal)
    lgnd_labels[1] <- 'high' ; lgnd_labels[GLOBALS$npal] <- 'low'
    lgnd_values <- seq(from = 0, to = 1, len = GLOBALS$npal)
    list(colors = rev(lgnd_colors),
         labels = lgnd_labels,
         values = lgnd_values)
}

version_choice <- function(x = "All debris", choices = GLOBALS$vchoices){
    names( choices[tolower(choices) %in% tolower(x)] )[1]
}


