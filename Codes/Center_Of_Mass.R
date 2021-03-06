library(UsingR)
library(manipulate)

myHistFn <- function(mu){
      mse <- mean((galton$child - mu)^2)
      g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour = "black", binwidth = 1) + geom_vline(xintercept = mu, size = 3)
      g <- g + ggtitle(paste("mu - ", mu, ", MSE - ", round(mse,2), sep = ""))
      g
}

manipulate(myHistFn(mu), mu = slider(62, 74, step = 0.5))
