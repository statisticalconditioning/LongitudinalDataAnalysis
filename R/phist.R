# Pretty histogram
phist <- function(x, dens = FALSE, ...) {
  hist(x, col = "grey", breaks = "fd", prob = T)
  if(dens) curve(dnorm(x, 
                            mean(x, na.rm = T), sd(x, na.rm = T)),
                 add = TRUE, col = "blue")
}