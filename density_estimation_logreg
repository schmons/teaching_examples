library("mvtnorm")
library("calibrateBinary")

set.seed(123)
n = 40

# Generate Target Data
data = rbind(rmvnorm(n, c(-1.75, 1), matrix(c(1, 0.7, 0.7, 1), 2, 2)),
             rmvnorm(n, c(1.75, 1), matrix(c(1, -0.7, -0.7, 1), 2, 2)))
plot(
  data, xlim=c(-4.5, 4.5), ylim=c(-2, 5), 
  lwd=7, col="dodgerblue4", 
  xlab="", ylab="", pch=16
)    

# Generate Reference data p_0 
# Note that I am not dividing by p_0 later since the value is constant
# and therefore can be ignored for visualization purposes
comparison = cbind(runif(2*n, -4.5, 4.5), runif(2*n, -2, 5))

points(comparison, col="firebrick", lwd=7, pch=17)

# Create data for classification task
data = data.frame(
  cbind(rbind(data, comparison), 
  rep(c(1, 0), each=2*n))
)

# Coordinates to evaluate the classification probabilities d(x)
x <- seq(-4.5, 4.5, 0.1)
y <- seq(-2, 5, 0.1)

coords = expand.grid(x, y)
probs = KLR(data[c(1, 2)], data$X3, coords, 
                 kernel = "exponential", rho=1.7, lambda = 80.5)
contour(x, y, matrix(probs/(1 - probs), length(x), length(y)), 
        nlevels = 6, lty=2,
        col="grey80", add=TRUE, lwd = 2, drawlabels=FALSE)

