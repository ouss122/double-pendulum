m1 <- 1
m2 <- 1
L1 <- 1.5
L2 <- 1
a1<-3.14
a2<-0.05
g <- 9.8
x <- c(0,0,a1,a2)
# x <- 1
t <- 0

runge_kutta<- function (f,h,n=1){

    while (n>0){
        k1<-h*f(x,t)
        k2<-h*f(x+(k1/2),t+(h/2))
        k3<-h*f(x+(k2/2),t+(h/2))
        k4<-h*f(x+k3,t+h)

        x <<- x + (1/6)*(k1+2*k2+2*k3+k4)
        t <<- t + h
        n <- n-1
    }
    
}

euler <- function (f,h,n=1){
    while (n>0){
        x <<- x + h*f(x,t)
        t <<- t + h
        n <- n-1
    } 
}

euler_modifie<- function(f,h,n=1){
    while (n>0){
        k<-f(x,t)
        k2<-f(x+h*k,t+h)
        x <<- x +(h/2)*(k+k2)
        t <<- t + h
        n <- n-1
    }
}

euler_ameliore<-function(f,h,n=1){
    while(n>0){
        k <- f(x,t)
        k2 <- f(x+(h/2)*k,t+(h/2))
        x <<- x + h*k2
        t <<- t+h
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


get_cords_rk4 <- function (delta){
    runge_kutta(f,delta)
    return(c(x[3],x[4]))
}

get_cords_euler <- function (delta){
    euler(f,delta);
    return(c(x[3],x[4]))
}

get_cords_eulerM <- function (delta){
    euler_modifie(f,delta);
    return(c(x[3],x[4]))
}

get_cords_eulerA <- function (delta){
    euler_ameliore(f,delta);
    return(c(x[3],x[4]))
}

# g <- function(x,t){
#     return(-x+t+1)
# }
# g <- function(x,t){
#     return(x+t)
# }



# euler(g,0.1,11)
# euler_ameliore(g,0.1,11)
# euler_modifie(g,0.1,11)


#   dtet <- t1-t2
#    e<- expression((m2*L1*(o1^2)*sin(2*dtet)+2*m2*L2*(o2^2)*sin(dtet)+2*g*m2*cos(t2)*sin(dtet)+2*g*m1*sin(t1))/(-2*L1*(m1+m2*(sin(dtet)^2))))

deriv(f,"t")