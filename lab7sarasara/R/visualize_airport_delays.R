#' @title Visualize airport arrival delays
#' @description A function to visualize the mean of airport arrival delays. Each airport is represented 
#' by their longitude and latitude. 
#' @return A scatterplot where the size of the points indicates the arrival delay 
#' @importFrom stats na.omit
#' @export visualize_airport_delays

visualize_airport_delays <- function(){
  airports <- nycflights13::airports[,c(1,3:4)]
  flights <- nycflights13::flights[,c(14,9)]
  flights <- na.omit(flights)
  flights <- dplyr::summarise(dplyr::group_by(flights, flights$dest), delay = mean(flights$arr_delay))
  airport_delays <- dplyr::inner_join(airports, flights, by=c("faa" = "dest"))
  
  ggplot2::ggplot(airport_delays, ggplot2::aes(x=airport_delays$lat, y=airport_delays$lon)) +
    ggplot2::geom_point(ggplot2::aes(size=airport_delays$delay), alpha=0.5, color="steelblue4") +
    ggplot2::theme_bw() +
    ggplot2::labs(title="Average arrival delays",
                  subtitle="Longitud vs. Latitude",
                  x="Latitude", y="Longitude",
                  size="Arrival delays") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5, size=16),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5, size=14, face="italic"),
                   axis.text = ggplot2::element_text(size=12))
}

