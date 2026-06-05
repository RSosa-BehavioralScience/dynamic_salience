####Esber & Haselgrove simulation

####Latent Inhibition

#Preexposure phase for the target cue

#Constant associated to outcome intensity is set to zero, representing 
#...non-occurrence

lambda_X <- 0

#Initial value of the target cue's excitatory associative 
#...strength (cue-outcome pairings) assuming subjects are naïve regarding 
#...that cue

V_X <- 0

#Initial value of the target cue's 
#...inhibitory (cue-no [expected] outcome experiences) associative strength

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

#Constant representing the unacquired or static salience of the 
#...target cue, which is related to its intensity 

phi_X <- 0.1

#Initial value of the target cue's acquired salience, assuming 
#...naïvity regarding the cue

epsilon_X <- 0

#Initial value of the predictive value of the static context with regard to
#...the occurrence of the cue, which can be understood as the inverse of the
#...current novelty status of the cue

pre_X <- 0

#Constant determining the total reduction of the "pre_X" term in salience is 
#...set to one  

k <- .99

#Creating empty vectors to store variables of interest in a 
#...trial-by-trial fashion

trials_pre_X <- c()
trials_epsilon <- c()
trials_alpha <- c()
trials_V <- c()
trials_V.bar <- c()
trials_CR <- c()

#Iterative routine implementing Esber and Haselgrove's model

for (i in 1:200){
  
  #The expression of the conditioned response in the current trial is given
  #...by the antagonistic relationship of the excitatory and inhibitory
  #...associative strengths; negative values would represent
  #...behaviors anticipating the absence of an expected outcome
  
  CR_X <-  V_X - V.bar_X
  
  #Determining the value of effective salience according to Equation 5.9
  
  alpha_X <- phi_X + epsilon_X - k*pre_X 
  
  #Recording all the variables of interest in the current trial
  
  trials_pre_X <- c(trials_pre_X, pre_X)
  trials_epsilon <- c(trials_epsilon, epsilon_X)
  trials_alpha <- c(trials_alpha, alpha_X)
  trials_V <- c(trials_V, V_X)
  trials_V.bar <- c(trials_V.bar, V.bar_X)
  trials_CR <- c(trials_CR, CR_X)
  
  #Determining the change in the "pre_X" term by a simple 
  #...error prediction mechanism in which the asymptote is the 
  #...unacquired salience and the learning rate was arbitrarily set to 0.08
  
  #Restricted to operate only if the outcome is not 
  #...presented (i.e., lambda_X equals zero)
  
  if(lambda_X == 0){
    delta_pre_X <- 0.01 * (phi_X - pre_X)
  }else{delta_pre_X <- 0}
  
  #Setting the value of the "pre_X" term for the next trial
  
  pre_X <- pre_X + delta_pre_X
  
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

#The "pre_X" term increases toward its asymptote, meaning that enough training
#...in the same context will detract all the novelty of the 
#...target cue at the expense of losing all the salience attributable
#...to the unacquired salience

plot(trials_pre_X)

#The acquired salience term increases toward a minor asymptote, due to
#...positive and negative values of cue-outcome and cue-no outcome 
#...associative strengths nearly cancelling out each other; a total
#...collapse is prevented presumably because of differing rate parameters

plot(trials_epsilon)

#Effective salience dramatically decreases early because the context is
#...a good predictor of the target cue, then it slightly increases because 
#...the cue becomes a good predictor of the outcome asymptoting at 
#...the value of the acquired salience term

plot(trials_alpha)

#The excitatory associative strength grows because the cue is 
#...a good predictor of the outcome, and asymptotes at a value below lambda

plot(trials_V)

#The inhibitory associative strength grows negative because the
#...cue-no outcome representation is antagonized by the 
#...strength of the cue-outcome component 

plot(trials_V.bar)

#As expected, behaviors representing excitatory conditioning
#...increase approaching the value of lambda

plot(trials_CR)

#Test Phase

######Acquisition of excitatory associative strength by the 
#...preexposed cue, X, and by a neutral intensity-equated cue, Y, for reference

####Esber & Haselgrove simulation

#Simple Excitatory Conditioning (X+)

#Constant associated to outcome intensity that determines the 
#...asymptote of learning 

lambda_X <- 1
lambda_Y <- 1

#Initial value of the target cue's excitatory associative 
#...strength (cue-outcome pairings) assuming subjects are naïve regarding 
#...that cue

print(V_X)
V_Y <- 0

#Initial value of the target cue's inhibitory (cue-no outcome experiences) 
#...associative strength

print(V.bar_X)
V.bar_Y <- 0

#Constant influencing the learning rate to the excitatory associative 
#...strength, parametrized within 0 and 0.1, as per Esber and 
#...Haselgrove recommendation (see Supplemental Materials) 

print(beta_delta.V)

#Constant influencing the learning rate to the inhibitory associative 
#...strength which is slightly inferior to its excitatory homologue; this
#...can be supported by the intuition that excitatory learning is 
#...faster than inhibitory learning

print(beta_delta.Vbar)

#Constant representing the unacquired or static salience of the 
#...target cue, which is related to its intensity 

print(phi_X)
phi_Y <- 0.1

#Initial value of the target cue's acquired salience, assuming 
#...naïvity regarding the cue

print(epsilon_X)
epsilon_Y <- 0 

#Initial value of the predictive value of the static context with regard to
#...the occurrence of the cue, which can be understood as the inverse of the
#...current novelty status of the cue

print(pre_X)
pre_Y <- 0

#Constant determining the total reduction of the "pre_X" term in salience is 
#...set to one  

print(k)

#Creating empty vectors to store variables of interest in a 
#...trial-by-trial fashion

#Pre-exposed cue

trials_pre_X <- c()
trials_epsilon <- c()
trials_alpha <- c()
trials_V <- c()
trials_V.bar <- c()
trials_CR <- c()

#Control cue

trials_pre_ctrl <- c()
trials_epsilon_ctrl <- c()
trials_alpha_ctrl <- c()
trials_V_ctrl <- c()
trials_V.bar_ctrl <- c()
trials_CR_ctrl <- c()

#Iterative routine implementing excitatory conditioning to X and Y cues

for (i in 1:150){
  
  #The expression of the conditioned response in the current trial is given
  #...by the antagonistic relationship of the excitatory and inhibitory
  #...associative strengths; negative values would represent
  #...behaviors anticipating the absence of an expected outcome
  
  CR_X <-  V_X - V.bar_X
  CR_Y <-  V_Y - V.bar_Y
  
  #Determining the value of effective salience according to Equation 5.9
  
  alpha_X <- phi_X + epsilon_X - k*pre_X 
  alpha_Y <- phi_Y + epsilon_Y - k*pre_Y
  
  #Recording all the variables of interest in the current trial
  
  trials_pre_X <- c(trials_pre_X, pre_X)
  trials_epsilon <- c(trials_epsilon, epsilon_X)
  trials_alpha <- c(trials_alpha, alpha_X)
  trials_V <- c(trials_V, V_X)
  trials_V.bar <- c(trials_V.bar, V.bar_X)
  trials_CR <- c(trials_CR, CR_X)
  
  trials_pre_ctrl <- c(trials_pre_ctrl, pre_Y)
  trials_epsilon_ctrl <- c(trials_epsilon_ctrl, epsilon_Y)
  trials_alpha_ctrl <- c(trials_alpha_ctrl, alpha_Y)
  trials_V_ctrl <- c(trials_V_ctrl, V_Y)
  trials_V.bar_ctrl <- c(trials_V.bar_ctrl, V.bar_Y)
  trials_CR_ctrl <- c(trials_CR_ctrl, CR_Y)
  
  #Determining the change in the "pre_X" term by a simple 
  #...error prediction mechanism in which the asymptote is the 
  #...unacquired salience and the learning rate was arbitrarily set to 0.08
  
  #Restricted to operate only if the outcome is not 
  #...presented (i.e., lambda_X equals zero)
  
  if(lambda_X == 0){
    delta_pre_X <- 0.01 * (phi_X - pre_X)
  }else{delta_pre_X <- 0}
  
  if(lambda_Y == 0){
    delta_pre_Y <- 0.01 * (phi_Y - pre_Y)
  }else{delta_pre_Y <- 0}
  
  #Setting the value of the "pre_X" term for the next trial
  
  pre_X <- pre_X + delta_pre_X
  pre_Y <- pre_Y + delta_pre_Y
  
  #Determining the value of the acquired salience as the aggregation of
  #...cue-outcome and cue-no outcome associative strengths
  
  epsilon_X <- V_X + V.bar_X
  epsilon_Y <- V_Y + V.bar_Y
  
  #Determining the change in cue-outcome associative strength as per
  #...the Pearce-Hall model (Equation 5.6). Restricted to positive gains
  
  if (beta_delta.V * alpha_X * (lambda_X - (V_X - V.bar_X)) > 0){
    delta_V <- beta_delta.V * alpha_X * (lambda_X - (V_X - V.bar_X))
  }else{delta_V <- 0}
  
  if (beta_delta.V * alpha_Y * (lambda_Y - (V_Y - V.bar_Y)) > 0){
    delta_V_Y <- beta_delta.V * alpha_Y * (lambda_Y - (V_Y - V.bar_Y))
  }else{delta_V_Y <- 0}
  
  #Setting the excitatory associative strength for the next trial
  
  V_X <- V_X + delta_V
  V_Y <- V_Y + delta_V_Y
  
  #Determining the change in cue-no outcome associative strength as per
  #...Equation 5.7. Restricted to positive gains
  
  if(((V_X - V.bar_X) - lambda_X) > 0){
    delta_V.bar <- beta_delta.Vbar * alpha_X * ((V_X - V.bar_X) - lambda_X)
  }else{delta_V.bar <- 0}
  
  if(((V_Y - V.bar_Y) - lambda_Y) > 0){
    delta_V.bar_Y <- beta_delta.Vbar * alpha_Y * ((V_Y - V.bar_Y) - lambda_Y)
  }else{delta_V.bar_Y <- 0}
  
  #Setting the inhibitory associative strength for the next trial
  
  V.bar_X <- V.bar_X + delta_V.bar 
  V.bar_Y <- V.bar_Y + delta_V.bar_Y 
} 

#The "pre_X" term increases toward its asymptote, meaning that enough training
#...in the same context will detract all the novelty of the 
#...target cue at the expense of losing all the salience attributable
#...to the unacquired salience

plot(trials_pre_X)
plot(trials_pre_ctrl)

#The acquired salience term increases toward a minor asymptote, due to
#...positive and negative values of cue-outcome and cue-no outcome 
#...associative strengths nearly cancelling out each other; a total
#...collapse is prevented presumably because of differing rate parameters

plot(trials_epsilon)
plot(trials_epsilon_ctrl)

#Effective salience dramatically decreases early because the context is
#...a good predictor of the target cue, then it slightly increases because 
#...the cue becomes a good predictor of the outcome asymptoting at 
#...the value of the acquired salience term

plot(trials_alpha)
plot(trials_alpha_ctrl)

#The excitatory associative strength grows because the cue is 
#...a good predictor of the outcome, and asymptotes at a value below lambda

plot(trials_V)
plot(trials_V_ctrl)

#The inhibitory associative strength grows negative because the
#...cue-no outcome representation is antagonized by the 
#...strength of the cue-outcome component 

plot(trials_V.bar)
plot(trials_V.bar_ctrl)

#As expected, behaviors representing excitatory conditioning
#...increase approaching the value of lambda

plot(trials_CR)
plot(trials_CR_ctrl)

# Load necessary library
library(ggplot2)

# Create the data frame
data <- data.frame(
  Trial = rep(1:150, times = 2),
  Performance = c(trials_CR, trials_CR_ctrl),
  Condition = rep(c("Preexposure", "Control"), each = 150)
)

# Ensure the Salience factor levels are in the desired order
#data$Salience <- factor(data$Salience, levels = c("weak (ϕ = 0.0099)", "medium (ϕ = 0.099)", "strong (ϕ = 0.99)"))

# Plot the data
p <- ggplot(data, aes(x = Trial, y = Performance, color = Condition)) +
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
    legend.title = element_blank(), # Remove the legend title
    legend.position = c(0.8, 0.4), # Move legend inside the plot
    legend.background = element_rect(fill = alpha('white', 0.6)),
    legend.text = element_text(size = 12)
  ) +
  scale_color_manual(values = c("Preexposure" = "antiquewhite4", "Control" = "green")) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

# Print the plot
print(p)
#Save in 800x400 pixel dimension
