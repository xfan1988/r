#Simulate: Target 60 minutes, POV, Delta=5 minutes
#r=0.1
#N=6000 shares order size

#Generate Order Sequence (Time, Size)

#Share Band=1000 (no tighter to target)
#Percent Band=0.05

#Assume Passive Order send has a 0.5 prob of being filled

#Re-evaluate interval: POV... on each new volume arrival
#Approximate as every 1-minute

#Simulate volume arrival as 1000 shares per minute with a random change
#(500-1500)

#Project market volume as 1000 shares per minute
set.seed(200)
V_M = cumsum(c(0,100*(5+round(runif(65)*10))))

r = 0.1

u_pct = 0.05
u_sh  = 1000

Delta = 5       #5-minute bin look-forward; target = V_P[i+Delta]
n = 60          #1-minute bin: re-evaluate;
N = 100*n
lmda = 0.4 #0.2   #factor to translate q1 to child order size

v_m = 1000          # Projected per-minute volume
V_P = N * cumsum(c(0, rep(v_m, 60)))/sum(c(0, rep(v_m, 60))) #Target-schedule (VWAP/TWAP)

genChildOrder = function(strategy="POV", n=10){
  a = l = filled = K = rep(0,n)
  q1 = 0
  for (i in 1:n){
   v = v_m * Delta      #Market volume in bin
   V = V_M[i]
   if ((strategy=="VWAP") || (strategy=="TWAP"))
   {
    q1 = V_P[min(i+Delta,n)] - K[i]
    target = V_P[i+Delta]
   }
   if (strategy=="POV")
   {
    q1 = ((r*(V+v))-K[i])/(1-r)
   }
   
   # q1: maximum quantity to fill during the bin by strategy definition
   # r: target participation
   # K: cumulative filled quantity
   
   U_pct = (((r+u_pct)*(V))-K[i])/(1-(r+u_pct)) #percent-upper-band
   U_sh  = (((r)*(V))-K[i])/(1-r) + U_sh        #share-upper-band
   
   l[i] = min(q1, U_pct, U_sh, N-K[i])    #l: distance to band, as maximum child order size
   a[i] = lmda * l[i]
   filled[i] = 100*round(a[i] * runif(1) * 0.01)   #simulate fill probability during the interval
   K[i] = K[i] + filled[i]    # Cumulative fill quantity
   rpov = K[i]/(V_M[i+1])     # cumulative POV
   #cat(paste(i, l[i], a[i], filled[i], K[i], rpov, "\n"))
   K[i+1] = K[i]
  }
 
 list(l=l, a = a, filled=filled, K=K)
}

#Question: How to set re-evaluation trigger for VWAP algo? Always set as 1-minute?
#How often are child order re-sent? On trigger?

# Graph
# Target
# Actual
# With band

o = genChildOrder(strategy="VWAP", n=60)
plot(o$K, type="l", xlab="Time")
lines(V_P, lty=2)
lines(pmax(0, V_P - u_sh), lty=3)
lines(pmax(0, V_P + u_sh), lty=3)
#lines(V_P*(1+0.05), lty=4)
#lines(V_P*(1-0.05), lty=4)

plot(cumsum(o$filled)/cumsum(o$a), main="Fill Ratio")

#Add:
# Modify band: Tighter to target (adjust share band)
# Add dark fills
