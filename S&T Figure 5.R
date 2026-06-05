#### Killcross & Balleine (1996) simulation:
#### outcome-specific preexposure (motivational modulation) -> acquisition
####
#### One cue X carries two outcome-specific associabilities:
####   subscript 1 = WATER, subscript 2 = FOOD
####
#### CURVES represent acquisition where the deprivation state at pre-exposure
#### matched the outcome used in the conditioning pairings.
####
#### A dashed vertical line marks the max-separation trial
#### (the "lucky window"); bar plots are sampled there.
####
#### BARS (are redundant, for fidelity to Killcross & Balleine): both deprivation
#### states are run. Bars are grouped by TEST OUTCOME (water, food); within
#### each group, two bars = thirsty vs hungry. The latent-inhibition crossover
#### shows as the within-group bar order flipping between the two groups.
####
#### Two implementations (from Sosa & Treviño):
####   RATE      (Equation 10): d_lambda scales the RATE of attenuation
####   ASYMPTOTE (Equation 11): d_lambda scales the ASYMPTOTE of attenuation


# WHAT EACH SYMBOL STANDS FOR
# ---------------------------
# A single cue X is tracked separately for two outcomes, subscript 1 = water and
# subscript 2 = food. For each outcome the model carries:
#   V_X       excitatory associative strength of X
#   V.bar_X   inhibitory associative strength of X 
#   epsilon_X expectancy term, the running sum V_X + V.bar_X used to update
#             salience on the next step
#   pre_X     the quantity that builds up across preexpusure trials and
#             suppresses associability
#   lambda_X  the asymptote the association is driven toward (0 with no US
#             present during preexposure, 1 once the US is introduced at
#             acquisition)
#   alpha_X   the associability (salience) of X on a given trial
#   phi_X     the baseline unacquired salience of X
#   k         the rate at which accumulated preexposure subtracts from
#             associability
#   d1, d2    the deprivation drives for the two outcomes; d_high marks the
#             state that matches the cue's outcome, d_low the mismatched state;
#             akin to beta in the Rescorla-Wagner model
#
# PREEXPOSURE PHASE  (run_preexposure)
# ------------------------------------
# With no US present, lambda is 0 for both outcomes, so no associative learning
# occurs. The only quantity that moves is pre_X, which approaches a target by a
# fixed proportion r_pre each trial. The two implementations differ only in how
# drive enters that approaching trajectory:
#   RATE      delta_pre = r_pre * d * (phi_X - pre_X)
#             drive multiplies the whole step, so a higher drive reaches the 
#             target faster. Drive scales the RATE of accumulation.
#   ASYMPTOTE delta_pre = r_pre * ((phi_X * d) - pre_X)
#             drive scales the target itself, so a higher drive accumulates to a
#             higher ceiling. Drive scales the ASYMPTOTE of accumulation.
#
# ACQUISITION PHASE  (run_acquisition)
# ------------------------------------
# Acquisition seeds V_X, V.bar_X, epsilon_X and pre_X from one preexposure row,
# then sets lambda_X to 1 because the US is now present. On each of n_acq trials:
#   1. the current conditioned response CR = V_X - V.bar_X is recorded first,
#      so trial 1 reflects the seeded starting point;
#   2. associability is recomputed as alpha_X = phi_X + epsilon_X - k * pre_X,
#      which is where accumulated preexposure impacts. A larger pre_X
#      lowers alpha_X and slows the rate of learning, known as latent inhibition;
#   3. epsilon_X is refreshed to the current V_X + V.bar_X (Esberg-Haselgrove rule);
#   4. excitatory strength grows toward lambda by a delta gated through
#      alpha_X;
#   5. inhibition V.bar_X grows only in inhibitory conditioning (not used here).
# The returned vector is the per-trial CR, i.e. one acquisition curve.
#

# =====================================================================
# Set all parameters and labels 
# =====================================================================
beta_delta.V    <- 0.07 #excitatory learning rate
beta_delta.Vbar <- 0.01 #inhibitory learning rate
phi_X           <- 0.1 #unacquired salience
k               <- 0.99 #how much accrued pre-exposure influences conditioning
r_pre           <- 0.4 #how much each pre-exposure trial influences salience

#deprivation drive values 
d_high <- 0.9
d_low  <- 0.1

#number of total pre-exposure trials
n_pre_total <- 100
pre_samples <- c(1, 5, 15, 100) #sampling different number of pre-exposure trials
n_acq       <- 150 #numer of conditioning trials

#labels for plots ahead
LAB_curve_x      <- "Acquisition trial"
LAB_curve_y      <- "Associative strength"
LAB_bar_y        <- "Responding at sampled trial"
LAB_bar_group_x  <- "Outcome at acquisition"  
LAB_drive_title  <- "Drive"        
LAB_bar_title    <- "Deprivation"    
LAB_matched      <- "Matched"
LAB_mismatched   <- "Mismatched"
LAB_outcome1     <- "Water"
LAB_outcome2     <- "Food"
LAB_state_hi     <- "Thirsty"
LAB_state_lo     <- "Hungry"
LAB_impl_rate    <- "Rate"
LAB_impl_asym    <- "Asymptote"
LAB_pre_unit     <- "preexp."

#colors for plots ahead
COL_matched    <- "purple"
COL_mismatch   <- "#69b3a2"
COL_bar_hi     <- "grey30"
COL_bar_lo     <- "white"
COL_bar_border <- "black"
COL_dashed     <- "grey40"

#legend config for plots ahead

LEG_curve_pos <- "bottomright"
LEG_curve_x   <- 60 #overrides "bottomright"
LEG_curve_y   <- .45      
LEG_bar_pos   <- "top"
LEG_bar_x     <- NA
LEG_bar_y     <- NA
LEG_cex       <- 0.95    #legend text and symbol size
LEG_lwd       <- 3.2     #legend line/symbol thickness (curve legend)
CEX_acq_xlab  <- 1.15    #size of the "Acquisition trial" x-label
CEX_acq_ylab  <- 1.15    #size of the curve-panel y-label (associative strength)
CEX_test_ylab <- 1.15    #size of the bar-panel y-label (CR at sampled trial)
CEX_panel_main<- 1.15    #size of the per-panel headings

OPEN_DEVICE <- TRUE
DEV_WIDTH   <- 14
DEV_HEIGHT  <- 12

#-------------------EXPERIMENT STARTS__________________________________

# =====================================================================
# PREEXPOSURE PHASE
# =====================================================================
run_preexposure <- function(implementation, d1, d2){
  V_X1<-0; V_X2<-0; V.bar_X1<-0; V.bar_X2<-0
  epsilon_X1<-0; epsilon_X2<-0; pre_X1<-0; pre_X2<-0
  lambda_X1<-0; lambda_X2<-0

  out <- data.frame(trial=1:n_pre_total,
                    pre_X1=NA, pre_X2=NA, V_X1=NA, V_X2=NA,
                    V.bar_X1=NA, V.bar_X2=NA, epsilon_X1=NA, epsilon_X2=NA)

  for (i in 1:n_pre_total){
    if (lambda_X1==0 & lambda_X2==0){
      if (implementation=="rate"){
        delta_pre_X1 <- r_pre * d1 * (phi_X - pre_X1)
        delta_pre_X2 <- r_pre * d2 * (phi_X - pre_X2)
      } else {
        delta_pre_X1 <- r_pre * ((phi_X * d1) - pre_X1)
        delta_pre_X2 <- r_pre * ((phi_X * d2) - pre_X2)
      }
    } else {delta_pre_X1<-0; delta_pre_X2<-0}
    pre_X1 <- pre_X1 + delta_pre_X1
    pre_X2 <- pre_X2 + delta_pre_X2

    epsilon_X1 <- V_X1 + V.bar_X1
    epsilon_X2 <- V_X2 + V.bar_X2

    out$pre_X1[i]<-pre_X1; out$pre_X2[i]<-pre_X2
    out$V_X1[i]<-V_X1; out$V_X2[i]<-V_X2
    out$V.bar_X1[i]<-V.bar_X1; out$V.bar_X2[i]<-V.bar_X2
    out$epsilon_X1[i]<-epsilon_X1; out$epsilon_X2[i]<-epsilon_X2
  }
  out
}

# =====================================================================
# ACQUISITION PHASE
# =====================================================================
run_acquisition <- function(seed_pre, seed_V, seed_Vbar, seed_epsilon){
  V_X<-seed_V; V.bar_X<-seed_Vbar; epsilon_X<-seed_epsilon
  pre_X<-seed_pre; lambda_X<-1
  CR <- numeric(n_acq)
  for (i in 1:n_acq){
    CR[i] <- V_X - V.bar_X
    alpha_X <- phi_X + epsilon_X - k*pre_X
    epsilon_X <- V_X + V.bar_X
    d <- beta_delta.V * alpha_X * (lambda_X - (V_X - V.bar_X))
    V_X <- V_X + max(d, 0)
    if (((V_X - V.bar_X) - lambda_X) > 0){
      V.bar_X <- V.bar_X + beta_delta.Vbar * alpha_X * ((V_X - V.bar_X) - lambda_X)
    }
  }
  CR
}

acq_from <- function(pre_row, o){
  if (o==1) run_acquisition(pre_row$pre_X1, pre_row$V_X1, pre_row$V.bar_X1, pre_row$epsilon_X1)
  else      run_acquisition(pre_row$pre_X2, pre_row$V_X2, pre_row$V.bar_X2, pre_row$epsilon_X2)
}

# =====================================================================
# DRIVER
# =====================================================================
build_all <- function(implementation){
  pre_thirsty <- run_preexposure(implementation, d_high, d_low)
  pre_hungry  <- run_preexposure(implementation, d_low,  d_high)

  res <- list()
  for (n in pre_samples){
    st <- pre_thirsty[n, ]; sh <- pre_hungry[n, ]

    cr_matched    <- acq_from(st, 1)
    cr_mismatched <- acq_from(st, 2)

    sep <- abs(cr_mismatched - cr_matched)
    t_star <- which.max(sep)

    bar_thirsty_water <- cr_matched[t_star]
    bar_thirsty_food  <- cr_mismatched[t_star]
    bar_hungry_water  <- acq_from(sh, 1)[t_star]
    bar_hungry_food   <- acq_from(sh, 2)[t_star]

    res[[as.character(n)]] <- list(
      curve_matched = cr_matched,
      curve_mismatched = cr_mismatched,
      t_star = t_star,
      bars = matrix(c(bar_thirsty_water, bar_hungry_water,
                      bar_thirsty_food,  bar_hungry_food),
                    nrow = 2, byrow = FALSE,
                    dimnames = list(c(LAB_state_hi, LAB_state_lo),
                                    c(LAB_outcome1, LAB_outcome2)))
    )
  }
  res
}

results <- list(rate = build_all("rate"),
                asymptote = build_all("asymptote"))

# =====================================================================
# PLOT  (standard multipanel; legends live inside the first panels)
# =====================================================================
impl_order <- c("rate","asymptote")
impl_labs  <- c(LAB_impl_rate, LAB_impl_asym)

#places legend
place_legend <- function(pos, x, y, ...){
  if (!is.na(x) && !is.na(y)) legend(x = x, y = y, ...)
  else                        legend(pos, ...)
}

#IMPORTANT: this may open a pop-up viewer that interrupts execution; minimize it
#...and continue running the script.
if (OPEN_DEVICE){
  if (.Platform$OS.type == "windows") {
    windows(width = DEV_WIDTH, height = DEV_HEIGHT)
  } else if (Sys.info()[["sysname"]] == "Darwin") {
    quartz(width = DEV_WIDTH, height = DEV_HEIGHT)
  } else {
    tryCatch(x11(width = DEV_WIDTH, height = DEV_HEIGHT),
             error = function(e) message("Could not open X11; drawing in current device."))
  }
}

op <- par(no.readonly = TRUE)
par(mfrow = c(length(pre_samples), 4),
    mar = c(3.4, 3.6, 2.6, 0.8),
    mgp = c(2, 0.6, 0),
    oma = c(0, 0, 0, 0))

for (ni in seq_along(pre_samples)){
  n <- pre_samples[ni]
  for (ii in seq_along(impl_order)){
    im <- impl_order[ii]
    cell <- results[[im]][[as.character(n)]]
    ttl_pre <- sprintf("%d %s trial%s", n, LAB_pre_unit, ifelse(n==1,"","s"))

    #curve panel
    plot(1:n_acq, cell$curve_matched, type="l", lwd=2.5, col=COL_matched,
         ylim=c(0,1), xlab=NA, ylab=NA,
         main=sprintf("%s  -  %s", impl_labs[ii], ttl_pre), cex.main=CEX_panel_main)
    title(xlab=LAB_curve_x, cex.lab=CEX_acq_xlab)
    title(ylab=LAB_curve_y, cex.lab=CEX_acq_ylab)
    lines(1:n_acq, cell$curve_mismatched, lwd=2.5, col=COL_mismatch)
    abline(v=cell$t_star, lty=2, col=COL_dashed)
    if (ni==1 && ii==1){
      place_legend(LEG_curve_pos, LEG_curve_x, LEG_curve_y,
                   title=LAB_drive_title,
                   legend=c(LAB_matched, LAB_mismatched),
                   col=c(COL_matched, COL_mismatch), lwd=LEG_lwd,
                   bty="n", cex=LEG_cex, xpd=NA)
    }

    ##bar panel
    bp <- barplot(cell$bars, beside=TRUE, ylim=c(0,1),
            col=c(COL_bar_hi, COL_bar_lo), border=COL_bar_border,
            ylab=NA,
            main=sprintf("%s  -  trial = %d", impl_labs[ii], cell$t_star),
            cex.main=CEX_panel_main)
    title(ylab=LAB_bar_y, cex.lab=CEX_test_ylab)
    box(bty="l")                  #replicates Killcross & Balleine configuration
    #x-axis configuration
    mtext(LAB_bar_group_x, side=1, line=2, at=mean(bp), cex=0.75)
    if (ni==1 && ii==1){
      place_legend(LEG_bar_pos, LEG_bar_x, LEG_bar_y,
                   title=LAB_bar_title,
                   legend=c(LAB_state_hi, LAB_state_lo),
                   fill=c(COL_bar_hi, COL_bar_lo), border=COL_bar_border,
                   bty="n", cex=LEG_cex, xpd=NA)
    }
  }
}
par(op)

#numeric readout of the experiment
cat("\n--- sampled trials (t_star) and bar values ---\n")
for (im in impl_order){
  for (n in pre_samples){
    cl <- results[[im]][[as.character(n)]]
    cat(sprintf("\n%s, %d preexposure: t_star=%d\n", im, n, cl$t_star))
    print(round(cl$bars, 4))
  }
}

