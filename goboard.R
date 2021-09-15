goboard <- function(n){
	
	plot(c(0,n), c(n,0),type="n", axes=FALSE, xlab="", ylab="",  xlim=c(0,n), ylim=c(0,n))
	for (i in 0:n)
	{
		lines( 0:n, rep(i, n+1) )
		lines( rep(i, n+1), 0:n )
	}

	points(c(3,n-3,3,n-3), c(3,3,n-3,n-3), pch=1)

	n
}

pdf("c:/temp/go9.pdf", width=8, height=8.8)
goboard(9)
dev.off()

