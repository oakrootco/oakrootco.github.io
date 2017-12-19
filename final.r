# P(row, column)
# columns are the future, rows are the current(past)
# P(x1=3 | x0=1) = P_1,3

a <- cbind(.2, .3, .5) #cbind!

m <- matrix( c(
    .1, .3, .6,
     0, .4, .6,
    .3, .2, .5), ncol= 3)

m_power <- for(i in range(1,power-1)){ mPower <- m %*% mpower}

rI<-1 # a=alpha
# 1: P(x7=3|X6=2)=p_3,2
m[3,2]
# 2: P(x0=3|x1=1) = P(joint)/P(marginal)
# P(joint) = p(x1=1|x0=3)*P(x0=3) = P(x)1,3 * a3
top <- m[1,3] %*% a[3]
# P(marginal) = (a*P)1
bot <- a %*% m[1,]
# answer:
ans <- top/bot


# Conditioning:
# E[x] = E[x|a]*P(a) + also for b, c
# = (aTime + E[x])*P(a) + (bTime+E[x])*P(b) + cTime*P(c) =, then solve

# Simulate rats running maze
mazeRun <- function(){
    time <- 0
    door <- 0
    while(door!=3){
        door <- sample( c(1,2,3), 1, prob= c( 1/3, 1/3, 1/3), replace= T)
        if(door==1){time <- time+2}
        if(door==2){time <- time+3}
        if(door==3){time <- time+1}
    }
    return(time)
}
#mean( replicate( 1e4, mazeRun()))
