
library(ggplot2)
library(gridExtra)

# Code for Density plot

r_base <- rnorm(1000, mean = 0.02, sd = 0.0001)
r_volatile <- rnorm(1000, mean = 0.02, sd = 0.001)
r_low <- rnorm(1000, mean = 0.02, sd = 0.00095)

dat1 <- data.frame(dens = c(rnorm(1000, 0.02, 0.001), rnorm(1000, mean = 0.02, sd = 0.002), rnorm(1000, mean = 0.02, sd = 0.00075))
                   , lines = rep(c("Base Case", "High Volatility", "Low Volatility"), each = 1000))

ggplot(dat1, aes(x = dens, fill = lines)) + 
  geom_density(alpha = 0.5) +
  xlab("Interest Rate") + ylab("Density") + ggtitle("Figure 1: Probability Density Plot - Base/High/Low Volatility")

# Define a function to calculate bond price 


bond_price_function <- function(p, cr, y, fv, ir, cf = 1) {
  n_coupons <- y * cf
  semi_annual_interest_rate <- ir / cf
  coupon_payment <- cr * fv / cf
  present_value_coupons <- sum(coupon_payment / (1 + semi_annual_interest_rate)^(1:n_coupons))
  present_value_face_value <- fv / (1 + semi_annual_interest_rate)^n_coupons
  bond_price <- present_value_coupons + present_value_face_value
}


calculate_bond_prices <- function(p, cr, y, fv, ir, cf = 1) {
    bond_prices <- numeric(length(ir))
  for (i in seq_along(ir)) {
    bond_prices[i] <- bond_price_function(p, cr, y, fv, ir[i], cf)
  }
  print(bond_prices)
}

p <- 1000  
cr <- 0.05  
y <- 5  
fv <- 1000  
cf <- 1  

# Calculate bond prices for each interest rate
bond_prices_low <- calculate_bond_prices(p, cr, y, fv, rnorm(1000, mean = 0.02, sd = 0.00075), cf)
bond_prices_high <- calculate_bond_prices(p, cr, y, fv, rnorm(1000, mean = 0.02, sd = 0.002), cf)
bond_prices_base <- calculate_bond_prices(p, cr, y, fv, rnorm(1000, mean = 0.02, sd = 0.001), cf)




## MCMC Generation

library(ggplot2)
library(gridExtra)


# Define the transition matrix (example with 3 states: low, medium, high)
n_steps <- 1000
initial_state <- "Base"

transition_matrix <- matrix(c(0.2, 0.5, 0.3,
                              0.4, 0.5, 0.1,
                              0.1, 0.6, 0.3),
                            nrow = 3, byrow = TRUE,
                            dimnames = list(c("High", "Base", "Low"),
                                            c("High", "Base", "Low")))


# Create Markov chain object
mc <- new("markovchain", states = c("Low", "Base", "High"), transitionMatrix = transition_matrix)

# Specify initial state (choose one of the states: "Low", "Medium", "High")

interest_rate_sequence <- markovchainSequence(n = n_steps, markovchain = mc, t0 = initial_state)

mc1 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc2 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc3 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc4 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc5 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc6 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc7 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc8 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc9 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc10 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")

mc21 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc22 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc23 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc24 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc25 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc26 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc27 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc28 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc29 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")
mc210 <- markovchainSequence(n = 1000, markovchain = mc, t0 = "Base")


mcdf <- as.data.frame(cbind(mc1,mc2,mc3,mc4,mc5,mc6,mc7,mc8,mc9,mc10,
                            mc21,mc22,mc23,mc24,mc25,mc26,mc27,mc28,mc29,mc210
))


result <- numeric(length(mc1))
for(i in seq_along(mc1)) {
  if(mc1[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc1[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } 
  else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r1 <- result 


result <- numeric(length(mc2))
for(i in seq_along(mc2)) {
  if(mc2[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc2[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r2 <- result 

result <- numeric(length(mc3))
for(i in seq_along(mc3)) {
  if(mc3[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc3[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r3 <- result 


result <- numeric(length(mc4))
for(i in seq_along(mc4)) {
  if(mc4[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc4[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r4 <- result 


result <- numeric(length(mc5))
for(i in seq_along(mc5)) {
  if(mc5[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc5[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r5 <- result 


result <- numeric(length(mc6))
for(i in seq_along(mc6)) {
  if(mc6[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc6[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r6 <- result 


result <- numeric(length(mc7))
for(i in seq_along(mc7)) {
  if(mc7[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc7[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r7 <- result 


result <- numeric(length(mc8))
for(i in seq_along(mc8)) {
  if(mc8[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc8[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r8 <- result 


result <- numeric(length(mc9))
for(i in seq_along(mc9)) {
  if(mc9[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc9[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r9 <- result 


result <- numeric(length(mc10))
for(i in seq_along(mc10)) {
  if(mc10[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc10[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r10 <- result 


result <- numeric(length(mc21))
for(i in seq_along(mc21)) {
  if(mc21[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc21[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r21 <- result 


result <- numeric(length(mc22))
for(i in seq_along(mc22)) {
  if(mc22[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc22[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r22 <- result 


result <- numeric(length(mc23))
for(i in seq_along(mc23)) {
  if(mc23[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc23[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r23 <- result 


result <- numeric(length(mc24))
for(i in seq_along(mc24)) {
  if(mc24[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc24[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r24 <- result 


result <- numeric(length(mc25))
for(i in seq_along(mc25)) {
  if(mc25[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc25[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r25 <- result 


result <- numeric(length(mc26))
for(i in seq_along(mc26)) {
  if(mc26[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc26[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r26 <- result 


result <- numeric(length(mc27))
for(i in seq_along(mc27)) {
  if(mc27[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc27[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r27 <- result 


result <- numeric(length(mc28))
for(i in seq_along(mc28)) {
  if(mc28[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc28[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r28 <- result 


result <- numeric(length(mc29))
for(i in seq_along(mc29)) {
  if(mc29[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc29[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r29 <- result 


result <- numeric(length(mc210))
for(i in seq_along(mc210)) {
  if(mc210[i] == "Base"){
    result[i] <- rnorm(1000, 0.02, 0.001)
  }
  if(mc210[i] == "Low"){
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.00075)
  } else {
    result[i] <- rnorm(1000, mean = 0.02, sd = 0.002)
  }
}

r210 <- result 

rate_df <- as.data.frame(cbind(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,
                               r21,r22,r23,r24,r25,r26,r27,r28,r29,r10))

t <- seq(from = 1, to = 1000, by = 1)

ggplot(data = NULL, aes(x = t)) + 
  geom_line(aes(y = rate_df$r1*100)) +
  geom_line(aes(y = rate_df$r2*100)) +
  geom_line(aes(y = rate_df$r3*100)) +
  geom_line(aes(y = rate_df$r4*100)) +
  geom_line(aes(y = rate_df$r5*100)) +
  geom_line(aes(y = rate_df$r6*100)) +
  geom_line(aes(y = rate_df$r7*100)) +
  geom_line(aes(y = rate_df$r8*100)) +
  geom_line(aes(y = rate_df$r9*100)) +
  geom_line(aes(y = rate_df$r10*100)) +
  ylim(0.015*100,0.025*100) + 
  xlab("System Step") + ylab("Interest Rate") + ggtitle("Figure 3: Rate Produced at Markov Chain Step")


terminal_rates <- rate_df[1000,]


price_final <- as.data.frame(cbind(
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,1], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,2], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,3], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,4], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,5], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,6], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,7], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,8], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,9], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,10], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,11], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,12], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,13], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,14], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,15], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,16], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,17], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,18], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,19], coupon_frequency),
  calculate_bond_prices(principal, coupon_rate, years, face_value, terminal_rates[,20], coupon_frequency)
  
))

hist(t(price_final), breaks = 5, freq = FALSE )


y1 <- ggplot(data = NULL, (aes(x = t(terminal_rates*100)))) + 
  geom_density(color="black", fill="lightblue") + 
  xlab("Interest Rate") + ylab("Density") +
  ggtitle("Figure 4: Terminal Interest Rate for MCMC Iterations")

y2 <- ggplot(data = NULL, (aes(x = t(price_final)))) + 
  geom_density(color="black", fill="pink")+ 
  xlab("Bond Price") + ylab("Density") +
  ggtitle("Figure 5: Terminal Bond Price for MCMC Iterations")

grid.arrange(y1, y2, ncol=1)






