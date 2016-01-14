library(dplyr)
library(RSQLite)
library(oce)
library(geosphere)
library(adehabitatLT)
library(foreach)
library(iterators)
library(ggplot2)

POSIXct.from.db <- function(x) as.POSIXct(x, tz = 'UTC', origin = '1970-01-01 00:00.00 UTC')
MHIdb <- dbConnect(dbDriver('SQLite'), 'Hawaii_data/MHI_GPS_TDR.sqlite')

trips <- dbGetQuery(MHIdb,
"SELECT T.DeployID, T.TripID, T.Begin, T.End, T.Duration,
  BM.FieldID, BM.Species
FROM Trip T
  JOIN DeploymentMEtadata DM
    ON DM.DeployID = T.DeployID
  JOIN BirdMetadata BM
    ON BM.DeployID = T.DeployID
WHERE DM.TDRRecovered = 1
  AND DM.GPSRecovered = 1
  AND T.BeginComplete = 1
  AND T.EndComplete = 1
  AND T.End - T.Begin > 3600 
  AND BM.Species IN ('BRBO', 'RFBO')
  AND BM.SubColonyCode = 'LEH'")

foreach(trip = iter(trips[1:20,], by = 'row'), .combine = rbind) %do% {
  track <- dbGetPreparedQuery(MHIdb,
    "SELECT T.*, W.DeployID IS NOT NULL AS 'Wet'
    FROM RediscretizedTrack T
      LEFT JOIN WetDry W
        ON T.DeployID = W.DeployID
          AND T.UTC >= W.Begin
          AND T.UTC < W.End
    WHERE T.DeployID = ?
      AND T.TripID = ?",
    dplyr::select(trip, DeployID, TripID))
  
  x0 <- mean(track$Longitude)
  y0 <- mean(track$Latitude)
  origin = c(x0, y0)
  
  trackxy <- mutate(track,
                    UTC = POSIXct.from.db(UTC),
                    d = distGeo(origin, cbind(Longitude, Latitude)),
                    b = bearing(cbind(Longitude, Latitude), origin),
                    x = d * -sin(b * pi / 180),
                    y = d * -cos(b * pi / 180),
                    step = sqrt((lead(x) - x)^2 + (lead(y) - y)^2))

  tracklt <- with(trackxy, as.ltraj(cbind(x, y), 
                                    date = UTC, 
                                    id = paste(trip$DeployID, trip$TripID, sep = '_')))
  fptresults <- fpt(tracklt, radii = seq(from = 1e3, to = 500e3, by = 1e3), units = 'seconds')
  
  get.fpt.at.r <- function(fpt, r) {
    fpt[[1]][, which(attr(fpt, 'radii') == r)]
  }
  
  smoothed <- function(x, y) loess(y ~ x, span = .1)$fitted
  
  rv_data <- data.frame(radius = attr(fptresults, 'radii'),
                        variance = apply(fptresults[[1]], 2, function(f) var(log(f), na.rm = TRUE))) %>%
    na.omit %>%
    mutate(variance_smoothed = smoothed(radius, variance),
           dv_dr = (lead(variance_smoothed) - variance_smoothed) / (lead(radius) - radius),
           dv_2dr = (lead(dv_dr) - dv_dr) / (lead(radius) - radius))
  
  ars_radius <- rv_data$radius[which.min(rv_data$dv_2dr)]
  
  trackxy$fpt <- get.fpt.at.r(fptresults, ars_radius)
  
  png(sprintf('Hawaii_data/Lehua/ars/plots/%i_%i.png', trip$DeployID, trip$TripID),
      width = 1200,
      height = 600)

  map_plot <- ggplot(trackxy,
         aes(x = x,
             y = y)) +
    geom_point(aes(color = fpt,
                   size = 4)) +
    geom_path(aes(alpha = .5)) +
    coord_fixed() +
    scale_size_continuous(guide = FALSE) +
    scale_alpha_continuous(guide = FALSE) +
    theme(legend.position = 'bottom')
  
  varfpt_plot <- rv_data %>%
    ggplot(aes(radius, 
               variance)) + 
    geom_line() +
    stat_smooth(method = 'loess', span = .1) +
    geom_vline(xintercept = ars_radius, linetype = 'dashed') +
    ggtitle(sprintf('ARS scale = %.0fm', ars_radius))
  
  # from http://stackoverflow.com/questions/9490482/combined-plot-of-ggplot2-not-in-a-single-plot-using-par-or-layout-functio
  lay_out = function(...) {    
    x <- list(...)
    n <- max(sapply(x, function(x) max(x[[2]])))
    p <- max(sapply(x, function(x) max(x[[3]])))
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))    
    
    for (i in seq_len(length(x))) {
      print(x[[i]][[1]], vp = grid::viewport(layout.pos.row = x[[i]][[2]], 
                                             layout.pos.col = x[[i]][[3]]))
    }
  } 

  lay_out(list(map_plot, 1:2, 1),
          list(varfpt_plot, 1:2, 2))
  
  dev.off()
  
  trip %>% transmute(DeployID, TripID, Species, ars_radius = ars_radius)
} 
