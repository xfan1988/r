plot(c(0,55), c(0,55), type="n", xlab = "", ylab="", axes=F)

lines(c(0,28+2/12), c(0,0), lwd=2)
lines(c(0,0), c(0,55+1/12), lwd=2)

lines(c(10+9/12,10+9/12), c(47+1/12,55+1/12), lwd=2) #Storage
lines(c(0,10+9/12), c(55+1/12,55+1/12), lwd=2) #Storage
lines(c(0,10+9/12), c(47+1/12,47+1/12))

lines(c(10+9/12,10+9/12), c(32+7/12,47+1/12), lwd=2)
lines(c(10+9/12,28+2/12), c(32+7/12,32+7/12), lwd=2)

lines(c(0,13+6/12), c(8+1/12,8+1/12)) #utility room
lines(c(13+6/12,13+6/12), c(0,8+1/12)) #utility room

lines(c(13+6/12,13+6/12), c(8+1/12,16+8/12)) #bedroom
lines(c(3,10+7/12), c(24+3/12, 16+8/12))
lines(c(10+7/12, 13+6/12), c(16+8/12, 16+8/12)) 

lines(c(13+6/12,24+5/12), c(12, 12)) #bathroom wall to staircase
lines(c(24+5/12,24+5/12), c(7+7/12, 12))	#bathroom

lines(c(24+5/12, 28+2/12), c(7+7/12,7+7/12)) #electricity room
lines(c(24+5/12, 24+5/12), c(0,7+7/12)) #electricity room
lines(c(28+2/12, 28+2/12), c(0,7+7/12), lwd =2) #electricity room

lines(c(0, 3), c(24+3/12,24+3/12)) 

lines(c(13+6/12, 16+6/12), c(15+8/12, 15+8/12))	#closet under staircase
lines(c(16+6/12, 24+5/12), c(14+8/12, 14+8/12))
lines(c(16+6/12, 16+6/12), c(15+8/12, 14+8/12))

lines(c(28+2/12, 28+2/12), c(7+7/12, 12+7/12), lwd=2)
lines(c(28+2/12, 30+8/12), c(12+7/12, 12+7/12), lwd=2)
lines(c(28+2/12, 30+8/12), c(19+3/12, 19+3/12), lwd=2)
lines(c(30+8/12, 30+8/12), c(12+7/12, 19+3/12), lwd=2)
lines(c(28+2/12, 28+2/12), c(19+3/12, 32+7/12), lwd=2)

abline(v=0);abline(h=0)

#Total sq footage
#39 * 28 - 14.5 * 17.5 
#minus staircase and bathroom: 7*11=77
#plus a small den under bookcase: 17
#plus the luggage den: 3 * 7 = 20 
#plus the closet: 5*2=10 
#total:808

#Length of 1/4 round
