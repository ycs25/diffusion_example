T = 10
dt = 0.01
t = seq(0,T,dt)
##Wiener process
w = rnorm(n = length(t) - 1, sd = sqrt(dt))
##diffusion process
N =T/dt
x = 0
X = rep(0,N)
##coefficients
alpha = 3
beta = 5
theta = 1
##Euler-Maruyama
for (i in 1:N) {
  mu = theta*(alpha/(alpha+beta)-x)
  sigma = sqrt(2*theta/(alpha+beta)*x*(1-x))
  x = x + mu*dt + sigma*w[i]
  X[i]=x
}
plot(type="l",X)