### R utility functions
### Xu Fan
### Sep 17, 2010

# To save a copy for in-session call: 
# save(sharpe, Decompose, locate, f2, rmax, rmin, dplot.fun, file="your_directory/Utility.RData")
# attach("your_directory/Utility.RData", pos=2)

#sharpe: computing the Sharpe ratio
#x: the return stream
sharpe <- function (x, ...)
 ifelse(rep(sum(x*x)==0,3), rep(0,3), c(mean(x, ...)/sqrt(var(x, ...)), mean(x, ...), sqrt(var(x, ...))))

# Decompose() and locate(), used in matrix index locating.
# To avoide name conflict: decompose() is a R system function on time series.
Decompose <- function(number, DIMS){
    if (length(DIMS) == 1)
        RESULT <- number
    else{
	RESULT <- rep(0, length(DIMS))
	RESULT[1] <- number %% DIMS[1]
	if (RESULT[1] == 0)
	    RESULT[1] <- DIMS[1]
	newnumber <- (number - RESULT[1]) / DIMS[1] + 1
	RESULT[-1] <- decompose(newnumber, DIMS[-1])
    }
    RESULT
}

locate <- function(array, conds, V=F){
    index <- (1:length(array))[as.logical(c(conds))]
    dimlength <- length(dim(array))
    located<-matrix(0,length(index),dimlength)
    for(i in 1:length(index)){
        located[i,]<-decompose(index[i], dim(array))
        if(V) cat(located[i,], "value = ", array[index[i]], "\n")
    }
    invisible(located)
}

f2 <- function (x) 
{
    val <- seq(along = x)
    val[x] <- 0
    val[!x][-1] <- diff(val[!x])
    out <- cumsum(val)
    out[out == 0] <- (out[out > 0])[1]
    out
}


#running max and min.
rmax <- function(x, span=6){
     options(warn=-1)
     apply(matrix(x, length(x)+1, span)[1:(length(x)-span+1),], 1, max)
}

rmin <- function(x, span=6){
     options(warn=-1)
     apply(matrix(x, length(x)+1, span)[1:(length(x)-span+1),], 1, min)
}


dplot.fun <- function(psi, normal = F, const = 1.06, xlab="",...)
{
# 
# Produces a plot of a density based on observed draws from the 
# corresponding distribution. Also includes marks for .025 and .975 
# quantiles.
#
# ARGUMENTS
#
# psi --  vector of observed draws
#
# If second argument is T then a Normal approximation is included.
#
# const - smoothness constant; default is rule on p. 131 of 
# Scott (1992); increasing const makes plot smoother.
#
# ADDITIONAL FUNCTION
# 
# must have flushplot.fun defined previously
#
#
        n <- length(psi)
        psi <- sort(psi)        # sorted for calculation of quantiles
        s <- sqrt(var(psi))#
#
n_length(psi)
psi_sort(psi) # sorted for calculation of quantiles
s_sqrt(var(psi))
iqr_quantile(psi,.75)-quantile(psi,.25)
a_min(c(s,iqr/1.34))
h_const*a/n^(1/5)
from_mean(psi)-5*s;  to_mean(psi)+5*s
dens_density(psi,width=4*h,from=from,to=to,n=1000)
maxplot_dnorm(mean(psi),mean(psi),sqrt(var(psi)))
maxplot_max(c(maxplot,dens$y))
#
# plot density, then add quantile lines
#
flushplot.fun(dens$x,dens$y,xlab=xlab,ylim=c(0,maxplot),...)
left_round(.025*n);  right_round(.975*n)
if(left==0)left_1
left_psi[left];   right_psi[right]
h1_density(psi,width=4*h,from=left,n=1)$y
h2_density(psi,width=4*h,from=right,n=1)$y
lines(c(left,left),c(0,h1),lty=2)
lines(c(right,right),c(0,h2),lty=2)
#
# Normal density approximation#
#
#
     if(normal==T){
          grid_seq(from,to,length=1000)
          lines(grid,dnorm(grid,mean(psi),sqrt(var(psi))),lty=4)
          }

        list(left=left, right=right)
     }

flushplot.fun<-function(x,y,xlab="",ylab="",type="l",...){
#
# plot with x axis positioned at y=0 (appropriate for density plot)
#
plot(x,y,xlab=xlab,ylab=ylab,type=type,axes=FALSE,bty="n",...)
axis(1,pos=0)
}


### isotonic regression estimation
ISO <- function(x, y, n=1000, increase = F, plot=F){
  # Isotonic regression: Slope of GCM of accumulated observations
  # Purpose: Fit a monotonic step function from the observations (x, y)

  # The chull() function usage needs a revision if used in R
  #  In Splus: The order of the output hull starts from 1, and runs 
  #  through the upper hull first.
  #  In R: The order of the output hull is different.

  # n is set to be large number to reduce rounding error.

  # Interpolation such that new observations are evenly spaced
  new <- approx(x, y, n=n)
  xx <- new$x
  yy <- new$y

  # Integration of y*dx (CSD)
  y.cs <- c(0, cumsum(yy - yy[1])[-1] * diff(xx))

  index <- chull(xx, y.cs)
  first <- which(index==min(index))

  # Make sure that the first element of convex hull is left-most (start) of CSD     
  # (cumulative sum diagram). Then index[first:last]
  if(first > 1) 
	index <- c(index[first:length(index)], index[1:(first-1)])

  last <- which(index==max(index))

  # If an increasing function: The GCM includes the left-most point - add it.
  if(!increase) 
    index <- index[1:last]
  else
    index <- sort(c(index[last:length(index)], index[1])) 

  # Take the slope of the upper hull
  yfit <- rep((y.cs[index][-1] - y.cs[index][-length(index)]) / (xx[index][-1] - xx[index][-length(index)]), diff(index))
  yfit <- yy[1] + c(yfit, yfit[length(yfit)])

  # Interpolation back to the original grid
  yfit <- approx(xx, yfit, xout=x)$y

  if(plot){
    plot(x, y); lines(x, yfit)
  }

  list(x=x, y=yfit, yin=y)
}

rollVar <- function(x, n = 9, trim = TRUE, unbiased = TRUE, na.rm = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute rolling variance

    # FUNCTION:
   
    # Transform:
    x = as.vector(x)
   
    # Roll Var:
    if (na.rm) x = as.vector(na.omit(x))
    rvar = rollFun(x = x, n = n, FUN = var)
    if (!unbiased) rvar = (rvar * (n-1))/n
    if (!trim) rvar = c(rep(NA, (n-1)), rvar)
   
    # Return Value:
    rvar
}

rollFun <- function(x, n, FUN, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute rolling function value

    # FUNCTION:
   
    # Transform:
    x = as.vector(x)
   
    # Roll FUN:
    start = 1
    end = length(x)-n+1
    m = x[start:end]
    for (i in 2:n) {
        start = start + 1
        end = end + 1
        m = cbind(m, x[start:end])}
   
    # Result:
    ans = apply(m, MARGIN = 1, FUN = FUN, ...)
   
    # Return value:
    ans
}