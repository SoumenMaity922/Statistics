set.seed(123)
x = seq(-1,2,by = 0.01)
plot(x, punif(x), col = "red", lwd = 2, type = "l",
     ylab = expression(F(x)))


### 29-09-2022 
## controlled variate method


m = 1000
x = runif(m)
print(x)
typeof(x)
theta_hat = mean(exp(x))
print(theta_hat)
se_theta_hat = sd(exp(x))/sqrt(m)
print(se_theta_hat)


## consider psi(X) as uniform (0,1)
c_star = -6*(3-exp(1))
print(c_star)

m = 10000
U = runif(m)
T_1 = exp(U)
T_2 = exp(U) + c_star*(U - mean(U))
print(mean(T_1))
print(mean(T_2))
print(mean(U))

variance_g = sd(T_1)/sqrt(m)
print(variance_g)
print(sd(T_2)/sqrt(m))

percentage_reduction = ((sd(T_1) - sd(T_2))/sd(T_1))*100
print(percentage_reduction)

print(sd(T_1))
print(sd(T_2))


### important sampling

g = function(x){
  exp(-x)/(1 + x^2)*(x>0)*(x<1)
}
g(2)
curve(g(x),-1,2,col = "red",lwd = 2)

f0 = function(x){
  (x>0)*(x<1)
}

curve(f0(x),-1,2,col = "red",lwd = 2)

f1 = function(x){
  exp(-x)*(x>0)
}
curve(f1(x),-1,2,col = "red",lwd = 2)

f2 = function(x){
  1/(pi*(1+x^2))
}

curve(f2(x),-1,2,col = "red",lwd =2)

f3 = function(x){
  exp(-x)/(1 - exp(-1))*(x>0)*(x<1)
}
curve(f3(x),-1,2,col = "red",lwd = 2)

f4 = function(x){
  (4/pi)*(1/(1 +x^2))*(x>0)*(x<1)
}

curve(f4(x),-1,2,col = "red",lwd = 2)


m = 10000
theta_hat = numeric(5)
se_theta_hat=  numeric(5)


## f0
x =u =  runif(m)
gf = g(x)/f0(x)
theta_hat[1] = mean(g(x)/f0(x))
print(theta_hat)
se_theta_hat[1] = sd(gf)/sqrt(m)
se_theta_hat



## for f1

x = - log(1- u)
gf = g(x) /f1(x)
se_theta_hat[2] = sd(gf)/sqrt(m)
theta_hat[2] = mean(g(x)/f1(x))


## for f2
x = tan(pi*(u - 1/2))
gf = g(x)/f2(x)
gf = gf[!is.na(gf)]
theta_hat[3] = mean(gf)
se_theta_hat[3] = sd(gf)/sqrt(m)
print(theta_hat)


## for f3
x = - log(1 - u*(1 - exp(-1)))
gf = g(x)/f3(x)
theta_hat[4] = mean(g(x)/f3(x))
se_theta_hat[4] = sd(gf)/sqrt(m)
print(theta_hat)


## for f4

x = tan((u*pi)/4)
gf = g(x)/f4(x)
theta_hat[5] = mean(g(x)/f4(x))
se_theta_hat[5] = sd(gf)/sqrt(m)
print(theta_hat)
print(se_theta_hat)
print(rbind(theta_hat,se_theta_hat))

par(mfrow = c(1,1))
## plotting g
curve(g(x),0,1,col = 2,lty = 1,lwd =2,ylim = c(0,1.5),ylab = "")
curve(f0(x),col = 3,lty = 2,lwd = 2,add = TRUE)
curve(f1(x),col= 4,lty = 3,lwd = 2,add = TRUE)
curve(f2(x),col = 5,lty = 4,lwd = 2,add = TRUE)
curve(f3(x),col = 6,lty = 5,lwd = 2,add = TRUE)
curve(f4(x),col = 7,lty = 6,lwd = 2,add = TRUE)
legend = c(expression(g),expression(f[0]),expression(f[1]),expression(f[2]),expression(f[3]),
           expression(f[4]))
legend("topright",bty ="n",legend =legend,col = 2:7,lty =1:6,lwd = rep(3,6) )

par(mfrow = c(1,1))
## plotting g
# curve(g(x),0.0001,0.999,col = 2,lty = 1,lwd =2,ylim = c(0,3),ylab = "")


curve(g(x)/f0(x),0.0001,0.999,col = 3,lty = 2,lwd = 3,ylab = "g/f0",ylim = c(0,3))
curve(g(x)/f1(x),col= 4,lty = 3,lwd = 3,add = TRUE)
curve(g(x)/f2(x),col = 5,lty = 4,lwd = 3,add = TRUE)
curve(g(x)/f3(x),col = 6,lty = 5,lwd = 3,add = TRUE)
curve(g(x)/f4(x),col = 7,lty = 6,lwd = 3,add = TRUE)
legend = c(expression(0),expression(1),expression(2),expression(3),
           expression(4))
legend("topright",bty ="n",legend =legend,col = 2:7,lty =1:6,lwd = rep(3,5) )
g(x)/f3(x)

typeof(gf)


hist(f2(x),probability = TRUE)

sum(is.na(x))

hist(g(x)/f2(x),probability = TRUE)
