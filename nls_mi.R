
sample_order = read.table("C:/temp/sample_order.csv", sep=",", header=T)
# fm2DNase1 <- nls(MI ~ 1/(1 + exp((xmid - log(conc))/scal)),
#                  data = DNase1,
#                  start = list(xmid = 0, scal = 1),
#                  algorithm = "plinear")
# 

res <- nls(MI ~ a1*(size/adv)^a2*sigma^a3*(1-b+b*(pov/(0.5+0.5*pov))),
                 data = sample_order,
                 start = list(a1 = 200, a2 = 0.5, a3=1, b=0.9))
                 


summary(res)

sample_order
