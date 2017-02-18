n=100; a = 0; b = 10;
t = seq(from = a,to = b,length.out = n)
dt = (b-a)/n; 
dw = rnorm(n,sd=dt);

theta = 1; 
mu = 1; 
sigma = 1;
aX<-function(mu,x) {
  A = theta*(mu-x)
}
bX<-function(sigma,x) {
  B = sigma
}

x = rep(0,n)
x0 = 0; x[1] = x0

for (i in seq(1,n-1,dt)) {
  A = aX(mu,x[i])
  B = bX(sigma,x[i])
  x[i+1] = x[i]+ A * dt + B * dw[i]
}

plot(t,x,'l')