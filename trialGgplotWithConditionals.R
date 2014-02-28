create_plot <- function (my.test.data) {
  my.test.gplot <- ggplot(my.test.data, environment = environment(), aes(x = x, y = y)) + geom_point(shape = 20)
  if (TRUE) {
    index.1 <- c(5, 6, 7)
    my.test.gplot <- my.test.gplot +
      geom_point(aes(x = x[index.1], y = y[index.1]), col = "blue", shape = 19)
  }
  if (TRUE) {
    index.2 <- c(1, 8, 9)
    my.test.gplot <- my.test.gplot +
      geom_point(aes(x = x[index.2], y = y[index.2]), col = "red", shape = 19)
  }
  my.test.gplot
}

my.test.data <- data.frame(x = seq(1:10), y = runif(10, 200, 500))
create_plot(my.test.data)
