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

foreach(trip = iter(trips[13,], by = 'row'), .combine = rbind) %do% {
  track <- dbGetPreparedQuery(MHIdb,
    "SELECT *
    FROM RediscretizedTrack T
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
  fptresults <- fpt(tracklt, radii = seq(from = 100, to = 5000, by = 5), units = 'seconds')
  
  find.ars.scale <- function(fptresults) {
    vlfr <- varlogfpt(fptresults, graph = FALSE)
    attr(vlfr, 'radii')[vlfr %>%
                          as.matrix %>%
                          t %>%
                          which.max]
  }
  
  get.fpt.at.r <- function(fpt, r) {
    fpt[[1]][, which(attr(fpt, 'radii') == r)]
  }
  
  find.behavior.changepoint <- function(fpt, r) {
    FPTatR <- get.fpt.at.r(fpt, r)
    density(FPTatR, na.rm = TRUE)[1:2] %>%
      as.data.frame %>%
      filter(x >= min(FPTatR, na.rm = TRUE),
             x < max(FPTatR, na.rm = TRUE)) %>%
      summarize(fpt = x[which.min(y)]) %>%
      as.numeric
  }
  
  ars_radius <- find.ars.scale(fptresults)
  behavior_changepoint <- find.behavior.changepoint(fptresults, ars_radius)
  
  trackxy <- mutate(trackxy,
                    fpt = get.fpt.at.r(fptresults, ars_radius),
                    behavior = ifelse(fpt < behavior_changepoint, 'transit', 'forage'))
  
#   png(sprintf('Hawaii_data/Lehua/ars/plots/%i_%i.png', trip$DeployID, trip$TripID),
#       width = 1200,
#       height = 900)
# 
#   map_plot <- ggplot(trackxy,
#          aes(x = x,
#              y = y)) +
#     geom_point(aes(color = fpt,
#                    size = 4)) +
#     geom_path(aes(alpha = .5)) +
#     coord_fixed() +
#     scale_size_continuous(guide = FALSE) +
#     scale_alpha_continuous(guide = FALSE) +
#     theme(legend.position = 'bottom')
#   
#   varfpt_plot <- data.frame(radius = attr(fptresults, 'radii'),
#                             variance = apply(fptresults[[1]], 2, function(f) var(log(f), na.rm = TRUE))) %>%
#     ggplot(aes(radius, 
#                variance)) + 
#     geom_line() +
#     geom_vline(xintercept = ars_radius, linetype = 'dashed') +
#     ggtitle(sprintf('ARS scale = %.0fm', ars_radius))
#   
#   behavior_plot <- ggplot(trackxy,
#                           aes(fpt)) +
#     geom_density() +
#     geom_vline(xintercept = behavior_changepoint, linetype = 'dashed') +
#     ggtitle(sprintf('Behavior changepoint = %.0fs', behavior_changepoint))
#   
#   # from http://stackoverflow.com/questions/9490482/combined-plot-of-ggplot2-not-in-a-single-plot-using-par-or-layout-functio
#   lay_out = function(...) {    
#     x <- list(...)
#     n <- max(sapply(x, function(x) max(x[[2]])))
#     p <- max(sapply(x, function(x) max(x[[3]])))
#     grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))    
#     
#     for (i in seq_len(length(x))) {
#       print(x[[i]][[1]], vp = grid::viewport(layout.pos.row = x[[i]][[2]], 
#                                              layout.pos.col = x[[i]][[3]]))
#     }
#   } 
# 
#   lay_out(list(map_plot, 1:2, 1:2),
#          list(varfpt_plot, 1, 3:4),
#          list(behavior_plot, 2, 3:4))
#   
#   dev.off()
  
  # Tortuosity analysis
  distance <- function(x1, y1, x2, y2) sqrt((x2 - x1)^2 + (y2 - y1)^2)
  trackxy <- mutate(trackxy,
                    tortuosity = (lag(step, 4) +
                                    lag(step, 3) +
                                    lag(step, 2) +
                                    lag(step, 1) +
                                    step +
                                    lead(step, 1) +
                                    lead(step, 2) +
                                    lead(step, 3))/
                      distance(lag(x, 4), lag(y, 4), lead(x, 4), lead(y, 4)))
browser()

ggplot(trackxy,
       aes(log(tortuosity))) +
  geom_density()
png('test.png', 800, 800)
ggplot(trackxy,
       aes(x,
           y)) +
  geom_point(aes(color = log(tortuosity)),
             size = 3) +
  geom_path(alpha = .5)
dev.off()
} 
