####Esber & Haselgrove scaffolding to produce 
#...motivational*associability-attenuation interactions 

# Namely, latent inhibition depends on the motivational state at the
# cue-preexposure phase

#### IMPLEMENTATION THROUGH THE RATE PARAMETER OF ASSOCIABILITY ATTENUATION
#...see Equation 10

# Preexposure phase for the target cue

# Constant associated to outcome intensity is set to zero, representing 
# non-occurrence. Neither water nor food pairings occur in the cue-preexposure stage

# WATER
lambda_X1 <- 0
# FOOD
lambda_X2 <- 0

# Initial value of the target cue's excitatory associative 
# strength (cue-outcome pairings) assuming subjects are naïve regarding that cue

# CUE-WATER ASSOCIATIVE STRENGTH
V_X1 <- 0
# CUE-FOOD ASSOCIATIVE STRENGTH
V_X2 <- 0

# Initial value of the target cue's 
# inhibitory (cue-no [expected] outcome experiences) associative strength

# CUE-NO WATER ASSOCIATIVE STRENGTH
V.bar_X1 <- 0
# CUE-NO FOOD ASSOCIATIVE STRENGTH
V.bar_X2 <- 0

# Constant influencing the learning rate to the excitatory associative 
# strength, parametrized within 0 and 0.1, as per Esber and 
# Haselgrove recommendation (see Supplemental Materials)

beta_delta.V <- 0.07

# Constant influencing the learning rate to the inhibitory associative 
# strength which is slightly inferior to its excitatory homologue; this
# can be supported by the intuition that excitatory learning is 
# faster than inhibitory learning

beta_delta.Vbar <- 0.01

# Constant representing the unacquired or static salience of the 
# target cue, which is related to its intensity

phi_X <- 0.1

# Initial value of the target cue's acquired salience, assuming 
# naïvity regarding the cue

epsilon_X1 <- 0
epsilon_X2 <- 0

# Initial value of the predictive value of the static context with regard to
# the occurrence of the cue, which can be understood as the inverse of the
# current novelty status of the cue

# We have different pre_X components linked to different
# outcomes to be associated with the target cue

# This represents preexposure attenuation of cue-water associability
pre_X1 <- 0

# This represents preexposure attenuation of cue-food associability
pre_X2 <- 0

# Constant determining the total reduction of the "pre_X" term in salience is 
# set to one
k <- .99

#### MOTIVATIONAL STATES

# Set a high degree of thirst
d_lambda1 <- 0.9

# Set a low degree of hunger, for comparison
d_lambda2 <- 0.1

# Creating empty vectors to store variables of interest in a 
# trial-by-trial fashion

trials_pre_X1 <- c()
trials_pre_X2 <- c()
trials_epsilon1 <- c()
trials_epsilon2 <- c()
trials_alpha1 <- c()
trials_alpha2 <- c()
trials_V1 <- c()
trials_V2 <- c()
trials_V.bar1 <- c()
trials_V.bar2 <- c()
trials_CR1 <- c()
trials_CR2 <- c()

# Iterative routine implementing Esber and Haselgrove's model

for (i in 1:100){
  
  # The expression of the conditioned response in the current trial is given
  # by the antagonistic relationship of the excitatory and inhibitory
  # associative strengths; negative values would represent
  # behaviors anticipating the absence of an expected outcome
  
  CR_X1 <- V_X1 - V.bar_X1
  CR_X2 <- V_X2 - V.bar_X2
  
  # Determining the value of effective salience according to Equation 5.9
  
  alpha_X1 <- phi_X + epsilon_X1 - k*pre_X1 
  alpha_X2 <- phi_X + epsilon_X2 - k*pre_X2 
  
  # Recording all the variables of interest in the current trial
  
  trials_pre_X1 <- c(trials_pre_X1, pre_X1)
  trials_pre_X2 <- c(trials_pre_X2, pre_X2)
  trials_epsilon1 <- c(trials_epsilon1, epsilon_X1)
  trials_epsilon2 <- c(trials_epsilon2, epsilon_X2)
  trials_alpha1 <- c(trials_alpha1, alpha_X1)
  trials_alpha2 <- c(trials_alpha2, alpha_X2)
  trials_V1 <- c(trials_V1, V_X1)
  trials_V2 <- c(trials_V2, V_X2)
  trials_V.bar1 <- c(trials_V.bar1, V.bar_X1)
  trials_V.bar2 <- c(trials_V.bar2, V.bar_X2)
  trials_CR1 <- c(trials_CR1, CR_X1)
  trials_CR2 <- c(trials_CR2, CR_X2)
  
  # Determining the change in the "pre_X" term by a simple 
  # error prediction mechanism in which the asymptote is the 
  # unacquired salience and the learning rate was arbitrarily set to 0.4
  
  # Restricted to operate only if the outcome is not 
  # presented (i.e., lambda_X equals zero)
  
  if(lambda_X1 == 0 & lambda_X2 == 0){
    delta_pre_X1 <- 0.4 * d_lambda1 * (phi_X - pre_X1)
  }else{delta_pre_X1 <- 0}
  
  if(lambda_X1 == 0 & lambda_X2 == 0){
    delta_pre_X2 <- 0.4 * d_lambda2 * (phi_X - pre_X2)
  }else{delta_pre_X2 <- 0}
  
  # Setting the value of the "pre_X" term for the next trial
  
  pre_X1 <- pre_X1 + delta_pre_X1
  pre_X2 <- pre_X2 + delta_pre_X2
  
  # Determining the value of the acquired salience as the aggregation of
  # cue-outcome and cue-no outcome associative strengths
  
  epsilon_X1 <- V_X1 + V.bar_X1
  epsilon_X2 <- V_X2 + V.bar_X2
  
  # Determining the change in cue-outcome associative strength as per
  # the Pearce-Hall model (Equation 5.6). Restricted to positive gains
  
  if (lambda_X1 > 0){
    if (beta_delta.V * alpha_X1 * (lambda_X1 - (V_X1 - V.bar_X1)) > 0){
      delta_V1 <- beta_delta.V * alpha_X1 * (lambda_X1 - (V_X1 - V.bar_X1))
    }else{delta_V1 <- 0}
  }else{delta_V1 <- 0}
  
  if (lambda_X2 > 0){
    if (beta_delta.V * alpha_X2 * (lambda_X2 - (V_X2 - V.bar_X2)) > 0){
      delta_V2 <- beta_delta.V * alpha_X2 * (lambda_X2 - (V_X2 - V.bar_X2))
    }else{delta_V2 <- 0}
  }else{delta_V2 <- 0}
  
  # Setting the excitatory associative strength for the next trial
  
  V_X1 <- V_X1 + delta_V1
  V_X2 <- V_X2 + delta_V2
  
  # Determining the change in cue-no outcome associative strength as per
  # Equation 5.7. Restricted to positive gains
  
  if(((V_X1 - V.bar_X1) - lambda_X1) > 0){
    delta_V.bar1 <- beta_delta.Vbar * alpha_X1 * ((V_X1 - V.bar_X1) - lambda_X1)
  }else{delta_V.bar1 <- 0}
  
  if(((V_X2 - V.bar_X2) - lambda_X2) > 0){
    delta_V.bar2 <- beta_delta.Vbar * alpha_X2 * ((V_X2 - V.bar_X2) - lambda_X2)
  }else{delta_V.bar2 <- 0}
  
  # Setting the inhibitory associative strength for the next trial
  
  V.bar_X1 <- V.bar_X1 + delta_V.bar1 
  V.bar_X2 <- V.bar_X2 + delta_V.bar2
} 

rate_implementation <- cbind(trials_alpha1, trials_alpha2)


#### IMPLEMENTATION THROUGH THE ASYMPTOTE OF ASSOCIABILITY ATTENUATION
#...see Equation 11

# Preexposure phase for the target cue

# Constant associated to outcome intensity is set to zero, representing 
# non-occurrence. Neither water nor food pairings occur in the cue-preexposure stage

# WATER
lambda_X1 <- 0
# FOOD
lambda_X2 <- 0

# Initial value of the target cue's excitatory associative 
# strength (cue-outcome pairings) assuming subjects are naïve regarding that cue

# CUE-WATER ASSOCIATIVE STRENGTH
V_X1 <- 0
# CUE-FOOD ASSOCIATIVE STRENGTH
V_X2 <- 0

# Initial value of the target cue's 
# inhibitory (cue-no [expected] outcome experiences) associative strength

# CUE-NO WATER ASSOCIATIVE STRENGTH
V.bar_X1 <- 0
# CUE-NO FOOD ASSOCIATIVE STRENGTH
V.bar_X2 <- 0

# Constant influencing the learning rate to the excitatory associative 
# strength, parametrized within 0 and 0.1, as per Esber and 
# Haselgrove recommendation (see Supplemental Materials)

beta_delta.V <- 0.07

# Constant influencing the learning rate to the inhibitory associative 
# strength which is slightly inferior to its excitatory homologue; this
# can be supported by the intuition that excitatory learning is 
# faster than inhibitory learning

beta_delta.Vbar <- 0.01

# Constant representing the unacquired or static salience of the 
# target cue, which is related to its intensity

phi_X <- 0.1

# Initial value of the target cue's acquired salience, assuming 
# naïvity regarding the cue

epsilon_X1 <- 0
epsilon_X2 <- 0

# Initial value of the predictive value of the static context with regard to
# the occurrence of the cue, which can be understood as the inverse of the
# current novelty status of the cue

# We have different pre_X components linked to different
# outcomes to be associated with the target cue

# This represents preexposure attenuation of cue-water associability
pre_X1 <- 0

# This represents preexposure attenuation of cue-food associability
pre_X2 <- 0

# Constant determining the total reduction of the "pre_X" term in salience is 
# set to one
k <- .99

#### MOTIVATIONAL STATES

# Set a high degree of thirst
d_lambda1 <- 0.9

# Set a low degree of hunger, for comparison
d_lambda2 <- 0.1

# Creating empty vectors to store variables of interest in a 
# trial-by-trial fashion

trials_pre_X1 <- c()
trials_pre_X2 <- c()
trials_epsilon1 <- c()
trials_epsilon2 <- c()
trials_alpha1 <- c()
trials_alpha2 <- c()
trials_V1 <- c()
trials_V2 <- c()
trials_V.bar1 <- c()
trials_V.bar2 <- c()
trials_CR1 <- c()
trials_CR2 <- c()

# Iterative routine implementing Esber and Haselgrove's model

for (i in 1:100){
  
  # The expression of the conditioned response in the current trial is given
  # by the antagonistic relationship of the excitatory and inhibitory
  # associative strengths; negative values would represent
  # behaviors anticipating the absence of an expected outcome
  
  CR_X1 <- V_X1 - V.bar_X1
  CR_X2 <- V_X2 - V.bar_X2
  
  # Determining the value of effective salience according to Equation 5.9
  
  alpha_X1 <- phi_X + epsilon_X1 - k*pre_X1 
  alpha_X2 <- phi_X + epsilon_X2 - k*pre_X2 
  
  # Recording all the variables of interest in the current trial
  
  trials_pre_X1 <- c(trials_pre_X1, pre_X1)
  trials_pre_X2 <- c(trials_pre_X2, pre_X2)
  trials_epsilon1 <- c(trials_epsilon1, epsilon_X1)
  trials_epsilon2 <- c(trials_epsilon2, epsilon_X2)
  trials_alpha1 <- c(trials_alpha1, alpha_X1)
  trials_alpha2 <- c(trials_alpha2, alpha_X2)
  trials_V1 <- c(trials_V1, V_X1)
  trials_V2 <- c(trials_V2, V_X2)
  trials_V.bar1 <- c(trials_V.bar1, V.bar_X1)
  trials_V.bar2 <- c(trials_V.bar2, V.bar_X2)
  trials_CR1 <- c(trials_CR1, CR_X1)
  trials_CR2 <- c(trials_CR2, CR_X2)
  
  # Determining the change in the "pre_X" term by a simple 
  # error prediction mechanism in which the asymptote is the 
  # unacquired salience and the learning rate was arbitrarily set to 0.4
  
  # Restricted to operate only if the outcome is not 
  # presented (i.e., lambda_X equals zero)
  
  if(lambda_X1 == 0 & lambda_X2 == 0){
    delta_pre_X1 <- 0.4 * ((phi_X * d_lambda1) - pre_X1)
  }else{delta_pre_X1 <- 0}
  
  if(lambda_X1 == 0 & lambda_X2 == 0){
    delta_pre_X2 <- 0.4 * ((phi_X * d_lambda2) - pre_X2)
  }else{delta_pre_X2 <- 0}
  
  # Setting the value of the "pre_X" term for the next trial
  
  pre_X1 <- pre_X1 + delta_pre_X1
  pre_X2 <- pre_X2 + delta_pre_X2
  
  # Determining the value of the acquired salience as the aggregation of
  # cue-outcome and cue-no outcome associative strengths
  
  epsilon_X1 <- V_X1 + V.bar_X1
  epsilon_X2 <- V_X2 + V.bar_X2
  
  # Determining the change in cue-outcome associative strength as per
  # the Pearce-Hall model (Equation 5.6). Restricted to positive gains
  
  if (lambda_X1 > 0){
    if (beta_delta.V * alpha_X1 * (lambda_X1 - (V_X1 - V.bar_X1)) > 0){
      delta_V1 <- beta_delta.V * alpha_X1 * (lambda_X1 - (V_X1 - V.bar_X1))
    }else{delta_V1 <- 0}
  }else{delta_V1 <- 0}
  
  if (lambda_X2 > 0){
    if (beta_delta.V * alpha_X2 * (lambda_X2 - (V_X2 - V.bar_X2)) > 0){
      delta_V2 <- beta_delta.V * alpha_X2 * (lambda_X2 - (V_X2 - V.bar_X2))
    }else{delta_V2 <- 0}
  }else{delta_V2 <- 0}
  
  # Setting the excitatory associative strength for the next trial
  
  V_X1 <- V_X1 + delta_V1
  V_X2 <- V_X2 + delta_V2
  
  # Determining the change in cue-no outcome associative strength as per
  # Equation 5.7. Restricted to positive gains
  
  if(((V_X1 - V.bar_X1) - lambda_X1) > 0){
    delta_V.bar1 <- beta_delta.Vbar * alpha_X1 * ((V_X1 - V.bar_X1) - lambda_X1)
  }else{delta_V.bar1 <- 0}
  
  if(((V_X2 - V.bar_X2) - lambda_X2) > 0){
    delta_V.bar2 <- beta_delta.Vbar * alpha_X2 * ((V_X2 - V.bar_X2) - lambda_X2)
  }else{delta_V.bar2 <- 0}
  
  # Setting the inhibitory associative strength for the next trial
  
  V.bar_X1 <- V.bar_X1 + delta_V.bar1 
  V.bar_X2 <- V.bar_X2 + delta_V.bar2
} 

assmpt_implementation <- cbind(trials_alpha1, trials_alpha2)


#### Create Figure 4

rate_implementation <- data.frame(
  Trial = rep(1:100, times = 2),
  Performance = c(rate_implementation[, 1], rate_implementation[, 2]),
  Condition = rep(c("Large_Motiv", "Slight_Motiv"), each = 100)
)

library(ggplot2)
p1 <- ggplot(rate_implementation, aes(x = Trial, y = Performance, color = Condition)) +
  geom_point(size = 4, alpha = 0.5) +
  labs(
    x = "Trials",
    y = "Salience"
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
  scale_color_manual(values = c("Large_Motiv" = "purple", "Slight_Motiv" = "#69b3a2"),
                     labels = c("Large_Motiv" = "Substantial Deprivation", "Slight_Motiv" = "Slight Deprivation")) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

print(p1)

assmpt_implementation <- data.frame(
  Trial = rep(1:100, times = 2),
  Performance = c(assmpt_implementation[, 1], assmpt_implementation[, 2]),
  Condition = rep(c("Large_Motiv", "Slight_Motiv"), each = 100)
)

p2 <- ggplot(assmpt_implementation, aes(x = Trial, y = Performance, color = Condition)) +
  geom_point(size = 4, alpha = 0.5) +
  labs(
    x = "Trials",
    y = "Salience"
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
  scale_color_manual(values = c("Large_Motiv" = "purple", "Slight_Motiv" = "#69b3a2"),
                     labels = c("Large_Motiv" = "Substantial Deprivation", "Slight_Motiv" = "Slight Deprivation")) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  scale_y_continuous(limits = c(0, 0.1))

print(p2)

if (!require('gridExtra')) install.packages('gridExtra'); library(gridExtra)
grid.arrange(p1, p2, ncol=2)
#Save in 800x400 pixel dimension
