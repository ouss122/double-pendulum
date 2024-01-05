m1 <- 1
m2 <- 1
L1 <- 1.5
L2 <- 1
g <- 9.8
x <- c(0,0,pi,0.05)
t <- 0

runge_kutta<- function (f,h,n=1){


    while (n>0){
        k1<-h*f(x,t)
        k2<-h*f(x+(k1/2),t+(h/2))
        k3<-h*f(x+(k2/2),t+(h/2))
        k4<-h*f(x+k3,t+h)

        x <<- x + (1/6)*(k1+2*k2+2*k3+k4)
        t<<- t + h
        n <- n-1
    }
    
}



double_pendulum_o1 <- function (o1,o2,t1,t2) {
    dtet <- t1-t2
    return ((m2*L1*(o1^2)*sin(2*dtet)+2*m2*L2*(o2^2)*sin(dtet)+2*g*m2*cos(t2)*sin(dtet)+2*g*m1*sin(t1))/(-2*L1*(m1+m2*(sin(dtet)^2))))
}

double_pendulum_o2 <- function (o1,o2,t1,t2){
    dtet <- t1-t2
    return ((m2*L2*(o2^2)*sin(2*dtet)+2*(m1+m2)*L1*(o1^2)*sin(dtet)+2*g*(m1+m2)*cos(t1)*sin(dtet))/(2*L2*(m1+m2*(sin(dtet)^2))))
}


double_pendulum_t1 <- function (o1){
    return (o1)

}

double_pendulum_t2 <- function (o2){
    return (o2)
}

f<- function (x,t){
    return(c(double_pendulum_o1(x[1],x[2],x[3],x[4]),double_pendulum_o2(x[1],x[2],x[3],x[4]),double_pendulum_t1(x[1]),double_pendulum_t2(x[2])))
}


get_cords <- function (delta){
    runge_kutta(f,delta)
    return(c(x[3],x[4]))
}


