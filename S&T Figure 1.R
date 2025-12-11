#Simple Excitatory Conditioning (X+) UNACQUIRED SALIENCE = 0.99

#Constant associated to outcome intensity that determines the 
#...asymptote of learning 

lambda_X <- 1

#Initial value of the target cue's excitatory associative 
#...strength (cue-outcome pairings) assuming subjects are naïve regarding 
#...that cue

V_X <- 0

#Initial value of the target cue's inhibitory (cue-no outcome experiences) 
#...associative strength

V.bar_X <- 0

#Constant influencing the learning rate to the excitatory associative 
#...strength, parametrized within 0 and 0.1, as per Esber and 
#...Haselgrove recommendation (see Supplemental Materials) 

beta_delta.V <- 0.07

#Constant influencing the learning rate to the inhibitory associative 
#...strength which is slightly inferior to its excitatory homologue; this
#...can be supported by the intuition that excitatory learning is 
#...faster than inhibitory learning

beta_delta.Vbar <- 0.01

#Initial value of the predictive value of the static context with regard to
#...the occurrence of the cue, which can be understood as the inverse of the
#...current novelty status of the cue

pre <- 0

#Constant determining the total reduction of the "pre" term in salience is 
#...set to one  

k <- 1

#Constant representing the unacquired or static salience of the 
#...target cue, which is related to its intensity 

phi_X <- .99

#Initial value of the target cue's acquired salience, assuming 
#...naïvity regarding the cue

epsilon_X <- 0

#Creating empty vectors to store variables of interest in a 
#...trial-by-trial fashion

trials_pre <- c()
trials_epsilon <- c()
trials_alpha <- c()
trials_V <- c()
trials_V.bar <- c()
trials_CR99 <- c()

#Iterative routine implementing Esber and Haselgrove's model

for (i in 1:150){
  
  #The expression of the conditioned response in the current trial is given
  #...by the antagonistic relationship of the excitatory and inhibitory
  #...associative strengths; negative values would represent
  #...behaviors anticipating the absence of an expected outcome
  
  CR_X <-  V_X - V.bar_X
  
  #Determining the value of effective salience according to Equation 5.9
  
  alpha_X <- phi_X + epsilon_X - k*pre 
  
  #Recording all the variables of interest in the current trial
  
  trials_pre <- c(trials_pre, pre)
  trials_epsilon <- c(trials_epsilon, epsilon_X)
  trials_alpha <- c(trials_alpha, alpha_X)
  trials_V <- c(trials_V, V_X)
  trials_V.bar <- c(trials_V.bar, V.bar_X)
  trials_CR99 <- c(trials_CR99, CR_X)
  
  #Determining the change in the "pre" term by a simple 
  #...error prediction mechanism in which the asymptote is the 
  #...unacquired salience and the learning rate was arbitrarily set to 0.08
  
  #Restricted to operate only if the outcome is not 
  #...presented (i.e., lambda_X equals zero)
  
  if(lambda_X == 0){
    delta_pre <- 0.08 * (lambda_X - pre)
  }else{delta_pre <- 0}
  
  #Setting the value of the "pre" term for the next trial
  
  pre <- pre + delta_pre
  
  #Determining the value of the acquired salience as the aggregation of
  #...cue-outcome and cue-no outcome associative strengths
  
  epsilon_X <- V_X + V.bar_X
  
  #Determining the change in cue-outcome associative strength as per
  #...the Pearce-Hall model (Equation 5.6). Restricted to positive gains
  
  if (beta_delta.V * alpha_X * (lambda_X - (V_X - V.bar_X)) > 0){
    delta_V <- beta_delta.V * alpha_X * (lambda_X - (V_X - V.bar_X))
  }else{delta_V <- 0}
  
  #Setting the excitatory associative strength for the next trial
  
  V_X <- V_X + delta_V
  
  #Determining the change in cue-no outcome associative strength as per
  #...Equation 5.7. Restricted to positive gains
  
  if(((V_X - V.bar_X) - lambda_X) > 0){
    delta_V.bar <- beta_delta.Vbar * alpha_X * ((V_X - V.bar_X) - lambda_X)
  }else{delta_V.bar <- 0}
  
  #Setting the inhibitory associative strength for the next trial
  
  V.bar_X <- V.bar_X + delta_V.bar 
} 

#Simple Excitatory Conditioning (X+) UNACQUIRED SALIENCE = 0.099

#Constant associated to outcome intensity that determines the 
#...asymptote of learning 

lambda_X <- 1

#Initial value of the target cue's excitatory associative 
#...strength (cue-outcome pairings) assuming subjects are naïve regarding 
#...that cue

V_X <- 0

#Initial value of the target cue's inhibitory (cue-no outcome experiences) 
#...associative strength

V.bar_X <- 0

#Constant influencing the learning rate to the excitatory associative 
#...strength, parametrized within 0 and 0.1, as per Esber and 
#...Haselgrove recommendation (see Supplemental Materials) 

beta_delta.V <- 0.07

#Constant influencing the learning rate to the inhibitory associative 
#...strength which is slightly inferior to its excitatory homologue; this
#...can be supported by the intuition that excitatory learning is 
#...faster than inhibitory learning

beta_delta.Vbar <- 0.01

#Initial value of the predictive value of the static context with regard to
#...the occurrence of the cue, which can be understood as the inverse of the
#...current novelty status of the cue

pre <- 0

#Constant determining the total reduction of the "pre" term in salience is 
#...set to one  

k <- 1

#Constant representing the unacquired or static salience of the 
#...target cue, which is related to its intensity 

phi_X <- .099

#Initial value of the target cue's acquired salience, assuming 
#...naïvity regarding the cue

epsilon_X <- 0

#Creating empty vectors to store variables of interest in a 
#...trial-by-trial fashion

trials_pre <- c()
trials_epsilon <- c()
trials_alpha <- c()
trials_V <- c()
trials_V.bar <- c()
trials_CR099 <- c()

#Iterative routine implementing Esber and Haselgrove's model

for (i in 1:150){
  
  #The expression of the conditioned response in the current trial is given
  #...by the antagonistic relationship of the excitatory and inhibitory
  #...associative strengths; negative values would represent
  #...behaviors anticipating the absence of an expected outcome
  
  CR_X <-  V_X - V.bar_X
  
  #Determining the value of effective salience according to Equation 5.9
  
  alpha_X <- phi_X + epsilon_X - k*pre 
  
  #Recording all the variables of interest in the current trial
  
  trials_pre <- c(trials_pre, pre)
  trials_epsilon <- c(trials_epsilon, epsilon_X)
  trials_alpha <- c(trials_alpha, alpha_X)
  trials_V <- c(trials_V, V_X)
  trials_V.bar <- c(trials_V.bar, V.bar_X)
  trials_CR099 <- c(trials_CR099, CR_X)
  
  #Determining the change in the "pre" term by a simple 
  #...error prediction mechanism in which the asymptote is the 
  #...unacquired salience and the learning rate was arbitrarily set to 0.08
  
  #Restricted to operate only if the outcome is not 
  #...presented (i.e., lambda_X equals zero)
  
  if(lambda_X == 0){
    delta_pre <- 0.08 * (lambda_X - pre)
  }else{delta_pre <- 0}
  
  #Setting the value of the "pre" term for the next trial
  
  pre <- pre + delta_pre
  
  #Determining the value of the acquired salience as the aggregation of
  #...cue-outcome and cue-no outcome associative strengths
  
  epsilon_X <- V_X + V.bar_X
  
  #Determining the change in cue-outcome associative strength as per
  #...the Pearce-Hall model (Equation 5.6). Restricted to positive gains
  
  if (beta_delta.V * alpha_X * (lambda_X - (V_X - V.bar_X)) > 0){
    delta_V <- beta_delta.V * alpha_X * (lambda_X - (V_X - V.bar_X))
  }else{delta_V <- 0}
  
  #Setting the excitatory associative strength for the next trial
  
  V_X <- V_X + delta_V
  
  #Determining the change in cue-no outcome associative strength as per
  #...Equation 5.7. Restricted to positive gains
  
  if(((V_X - V.bar_X) - lambda_X) > 0){
    delta_V.bar <- beta_delta.Vbar * alpha_X * ((V_X - V.bar_X) - lambda_X)
  }else{delta_V.bar <- 0}
  
  #Setting the inhibitory associative strength for the next trial
  
  V.bar_X <- V.bar_X + delta_V.bar 
} 

#Simple Excitatory Conditioning (X+) UNACQUIRED SALIENCE = 0.0099

#Constant associated to outcome intensity that determines the 
#...asymptote of learning 

lambda_X <- 1

#Initial value of the target cue's excitatory associative 
#...strength (cue-outcome pairings) assuming subjects are naïve regarding 
#...that cue

V_X <- 0

#Initial value of the target cue's inhibitory (cue-no outcome experiences) 
#...associative strength

V.bar_X <- 0

#Constant influencing the learning rate to the excitatory associative 
#...strength, parametrized within 0 and 0.1, as per Esber and 
#...Haselgrove recommendation (see Supplemental Materials) 

beta_delta.V <- 0.07

#Constant influencing the learning rate to the inhibitory associative 
#...strength which is slightly inferior to its excitatory homologue; this
#...can be supported by the intuition that excitatory learning is 
#...faster than inhibitory learning

beta_delta.Vbar <- 0.01

#Initial value of the predictive value of the static context with regard to
#...the occurrence of the cue, which can be understood as the inverse of the
#...current novelty status of the cue

pre <- 0

#Constant determining the total reduction of the "pre" term in salience is 
#...set to one  

k <- 1

#Constant representing the unacquired or static salience of the 
#...target cue, which is related to its intensity 

phi_X <- .0099

#Initial value of the target cue's acquired salience, assuming 
#...naïvity regarding the cue

epsilon_X <- 0

#Creating empty vectors to store variables of interest in a 
#...trial-by-trial fashion

trials_pre <- c()
trials_epsilon <- c()
trials_alpha <- c()
trials_V <- c()
trials_V.bar <- c()
trials_CR0099 <- c()

#Iterative routine implementing Esber and Haselgrove's model

for (i in 1:150){
  
  #The expression of the conditioned response in the current trial is given
  #...by the antagonistic relationship of the excitatory and inhibitory
  #...associative strengths; negative values would represent
  #...behaviors anticipating the absence of an expected outcome
  
  CR_X <-  V_X - V.bar_X
  
  #Determining the value of effective salience according to Equation 5.9
  
  alpha_X <- phi_X + epsilon_X - k*pre 
  
  #Recording all the variables of interest in the current trial
  
  trials_pre <- c(trials_pre, pre)
  trials_epsilon <- c(trials_epsilon, epsilon_X)
  trials_alpha <- c(trials_alpha, alpha_X)
  trials_V <- c(trials_V, V_X)
  trials_V.bar <- c(trials_V.bar, V.bar_X)
  trials_CR0099 <- c(trials_CR0099, CR_X)
  
  #Determining the change in the "pre" term by a simple 
  #...error prediction mechanism in which the asymptote is the 
  #...unacquired salience and the learning rate was arbitrarily set to 0.08
  
  #Restricted to operate only if the outcome is not 
  #...presented (i.e., lambda_X equals zero)
  
  if(lambda_X == 0){
    delta_pre <- 0.08 * (lambda_X - pre)
  }else{delta_pre <- 0}
  
  #Setting the value of the "pre" term for the next trial
  
  pre <- pre + delta_pre
  
  #Determining the value of the acquired salience as the aggregation of
  #...cue-outcome and cue-no outcome associative strengths
  
  epsilon_X <- V_X + V.bar_X
  
  #Determining the change in cue-outcome associative strength as per
  #...the Pearce-Hall model (Equation 5.6). Restricted to positive gains
  
  if (beta_delta.V * alpha_X * (lambda_X - (V_X - V.bar_X)) > 0){
    delta_V <- beta_delta.V * alpha_X * (lambda_X - (V_X - V.bar_X))
  }else{delta_V <- 0}
  
  #Setting the excitatory associative strength for the next trial
  
  V_X <- V_X + delta_V
  
  #Determining the change in cue-no outcome associative strength as per
  #...Equation 5.7. Restricted to positive gains
  
  if(((V_X - V.bar_X) - lambda_X) > 0){
    delta_V.bar <- beta_delta.Vbar * alpha_X * ((V_X - V.bar_X) - lambda_X)
  }else{delta_V.bar <- 0}
  
  #Setting the inhibitory associative strength for the next trial
  
  V.bar_X <- V.bar_X + delta_V.bar 
} 


#Load necessary library
library(ggplot2)

#Create the data frame
data <- data.frame(
  Trial = rep(1:150, times = 3),
  Performance = c(trials_CR0099, trials_CR099, trials_CR99),
  Salience = rep(c("weak (ϕ = 0.0099)", "medium (ϕ = 0.099)", "strong (ϕ = 0.99)"), each = 150)
)

#Ensure the Salience factor levels are in the desired order
data$Salience <- factor(data$Salience, levels = c("weak (ϕ = 0.0099)", "medium (ϕ = 0.099)", "strong (ϕ = 0.99)"))

#Plot the data with italicized phi
p <- ggplot(data, aes(x = Trial, y = Performance, color = Salience)) +
  geom_point(size = 4, alpha = 0.25) +
  labs(
    x = "Trials",
    y = "Associative Strength"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title = element_blank(), 
    legend.position = c(0.8, 0.4), 
    legend.background = element_rect(fill = alpha('white', 0.6)),
    legend.text = element_text(size = 12)
  ) +
  scale_color_manual(
    values = c("weak (ϕ = 0.0099)" = "gray", "medium (ϕ = 0.099)" = "black", "strong (ϕ = 0.99)" = "turquoise"),
    labels = c(
      expression(paste("weak (", italic(phi), " = 0.0099)")),
      expression(paste("medium (", italic(phi), " = 0.099)")),
      expression(paste("strong (", italic(phi), " = 0.99)"))
    )
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

#Print the plot
print(p)
#Save in 800x400 pixel dimension

