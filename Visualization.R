## Network visualzation 

coord <- gplot(g[[1]], mode = "springrepulse")

vertex.col <- lapply(g, function(x) {
  vertex.col <- x %v% "candidate.preference"
  vertex.col[vertex.col == '0'] <- "tomato"
  vertex.col[vertex.col == '1'] <- "steel blue"
  vertex.col
})

vertex.cex <- lapply(g, function(x) {
  vertex.cex <- log(degree(x, cmode = "outdegree") + 1)
  vertex.cex
})

vertex.cex <- (Reduce("+", vertex.cex)/3 + 0.2)/4.8618760 + 0.5

plot.network(g[[1]], coord = coord, 
             vertex.col = vertex.col[[1]], vertex.cex = vertex.cex, edge.col = "gray80", vertex.border = "white")
plot.network(g[[2]], coord = coord, 
             vertex.col = vertex.col[[2]], vertex.cex = vertex.cex, edge.col = "gray80", vertex.border = "white")
plot.network(g[[3]], coord = coord, 
             vertex.col = vertex.col[[2]], vertex.cex = vertex.cex, edge.col = "gray80", vertex.border = "white")


## timeline
data <- data.frame(
  id      = 1:6,
  content = c("Start of log data"  , "W1"  ,"W2", "W3", "Election Day", "End of log data"),
  start   = c("2012-11-23", "2012-11-27", "2012-12-11", "2012-12-21", "2012-12-19", "2012-12-19"),
  end     = c(NA          , "2012-11-29", "2012-12-13", "2012-12-23", NA, NA)
)

timevis(data, showZoom = FALSE, options = list(editable = TRUE))
