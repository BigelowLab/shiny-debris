library(leaflet)
library(shiny)
library(raster)
library(RColorBrewer)
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

#' Read one or more xcast by date
#'
#'
#' @param dt the Date to read, if missing then read all
#' @param version the character version stamp to read (default 'v1.00')
#' @param path path to the data
#' @returns raster::stack with Z values set to date
read_xcast <- function(dt,
    version = 'v1.00',
    path = DATAPATH,
    threshold = c(NA, -10)[2]){
    if (!missing(dt)){
        ff = file.path(path, version,
                      'xcast', format(dt[1], "%Y-%m-%d.grd"))
        if (file.exists(ff[1])){
            x = raster::raster(ff[1])
        } else {
            x = NULL
        }
    } else {
        ff = list_xcast(version = version, form = 'filename', path = path)
        x <- raster::brick(ff)
        dt <- as.Date(basename(ff), format = '%Y-%m-%d.grd')
        x <- raster::setZ(x, dt)
    }
    if (!is.na(threshold)) x[TOPO <= threshold] <- NA
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



COLOR_LUT = c("None" = NA, Yellow = "#FFFF00", Orange = "#FFA500",
             Blue = "#0000FF", White = "#FFFFFF")
DEFAULTS = list(
    date_range = range(list_xcast(form = 'date')),
    version = 'v1.00',
    npal = 7,
    ovrly_pal =  leaflet::colorNumeric(RColorBrewer::brewer.pal(7,'Oranges'),
        domain = c(0,1),
        na.color = "transparent"),
    ovrly_opacity = 0.8,
    obs_radius = 20,
    obs_color = 'Yellow')

#' A function to filter points between two dates
#' @param x the PTS tibble or a subset thereof
#' @param dates a two element start/stop vector of Date class
#' @return a tibble with zero or more rows
filter_obs <- function(x = OBS, dates = DEFAULTS$date_range,
        version = DEFAULTS$version){
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
legend_from_pal <- function(pal = DEFAULTS$ovrly_pal){
    if (is.null(pal)) pal <- DEFAULTS$ovrly_pal
    lgnd_colors <- pal(seq(from = 0, to = 1, length = DEFAULTS$npal))
    lgnd_labels <- rep("", DEFAULTS$npal)
    lgnd_labels[1] <- 'high' ; lgnd_labels[DEFAULTS$npal] <- 'low'
    lgnd_values <- seq(from = 0, to = 1, len = DEFAULTS$npal)
    list(colors = rev(lgnd_colors),
         labels = lgnd_labels,
         values = lgnd_values)
}

