#------------------------------------------------------------------------------#
#- !! PLEASE CREATE THESE FOLDERS BEFORE running this script : ----------------#
#- 'figures' and 'data' !! ----------------------------------------------------#
#------------------------------------------------------------------------------#
#- This script generates figures to calibrate the constants (beta = bl and ----#
#- gamma = ql) of the multiscale penalty : ------------------------------------#
#- (1) It repeatedly simulates iid gaussian signals with no changepoint and ---#
#- varying size n. ------------------------------------------------------------#
#- (2) It estimates the number of changepoints with Ms.Fpop for varying beta --#
#- and gamma. -----------------------------------------------------------------#
#- (3) It calculates and plots the proportions of replicates with > 0 ---------#
#- changepoints. --------------------------------------------------------------#
#------------------------------------------------------------------------------#

source("R/load.R")

#------------------------------------------------------------------------------#
#- simulations ----------------------------------------------------------------#
#------------------------------------------------------------------------------#

logical_threads <- 70
#- grid of parameter (beta and gammma) ----------------------------------------#
bl   <- seq(1,3,0.25)
ql   <- seq(1,20, length=10)
#- tested profile sizes -------------------------------------------------------#
n    <- c(10^c(2:5), 250000) 
#- number of replicates for each combination of previous paramters ------------#
reps <- 1:300
params           <- expand.grid(ql, bl, n, reps)
colnames(params) <- c("ql","bl","n","rep")

#- steps (1) and (2) ----------------------------------------------------------#
res_msfpop <- do.call(rbind, mclapply(
  1:nrow(params), 
  function(i) {
    
    y <- rnorm(params$n[[i]])
    
    cp <- length(MsFPOP(
      y     = y, 
      beta  = params$bl[[i]], 
      alpha = params$ql[[i]]+params$bl[[i]]*log(params$n[[i]])
    )$changepoints)-1
    
    data.frame(
      ql  = params$ql[[i]],
      bl  = params$bl[[i]],
      n   = params$n[[i]],
      rep = params$rep[[i]],
      cp  = cp
    )
  },
  mc.cores = logical_threads
))
saveRDS(res_msfpop, "data/calibrate_MsFPOP.rds")


#- step (3) -------------------------------------------------------------------#

options(scipen = 999)
res_msfpop          <- readRDS("data/calibrate_MsFPOP.rds")
res_msfpop$decision <- res_msfpop$cp == 0

#- calculate FPR for each combination of parameters (beta, gamma, n) ----------#
nb_rep <- nrow(split(
  res_msfpop, 
  paste0(res_msfpop$bl,res_msfpop$ql,res_msfpop$n)
)[[1]])

res_msfpop_2 <- do.call(rbind, lapply(
  split(
    res_msfpop, 
    paste0(res_msfpop$bl,res_msfpop$ql,res_msfpop$n)
  ),
  function(x) {
    data.frame(
      FP = sum(!x$decision), 
      ql = x$ql[[1]], 
      bl = x$bl[[1]], 
      n  = x$n[[1]]
    )
  }
)) 
res_msfpop_2$fp_rate <- res_msfpop_2$FP/nb_rep


#------------------------------------------------------------------------------#
#- figures --------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#- proportions of replicates with > 0 changepoints, beta >= 2, supp figure S1 -#
g1 <- ggplot(
  res_msfpop_2[res_msfpop_2$bl >= 2,],
  aes(
    x     = ql, 
    y     = 1-fp_rate,
    color = as.factor(n)
  )
)+
facet_wrap(
  bl~., 
  ncol     = 2, 
  nrow     = 3, 
  scales   = "free", 
  labeller = label_bquote(beta == .(bl))
)+
geom_line(size = 1)+
geom_hline(
  yintercept = 0.95, 
  color      = "red", 
  linetype   = "dashed", 
  size       = 1
)+
xlab(expression(gamma))+
ylab("1 - propotion of designs with > 0 changepoints")+
geom_text(
  aes(
    x = x, 
    y = y
  ), 
  label = expression(1-alpha ~"level (0.95)"),
  color = "red", 
  size  = 6.5,  
  data  = data.frame(
    x = 16, 
    y = 0.90, 
    bl = 2
  )
)+
labs(color = "n : ") +
theme_bw()+
theme(
  text             = element_text(size=25),
  legend.text      = element_text(size=17),
  legend.position  = "bottom",
  strip.background = element_rect(fill="grey95")
)

pdf(widt=10, height=14, "figures/figure_S1.pdf")
g1
dev.off()

#- figure 2 -------------------------------------------------------------------#
g2 <- ggplot(
  res_msfpop_2[res_msfpop_2$bl==2.25,],
  aes(
    x     = ql, 
    y     = 1-fp_rate,
    color = as.factor(n)
  )
)+
  geom_line(size = 1)+
  geom_hline(
    yintercept = 0.95, 
    color      = "red", 
    linetype   = "dashed", 
    size       = 1
  )+
  xlab(expression(gamma))+
  ylab("1 - propotion of designs with > 0 changepoints")+
  geom_text(
    aes(
      x = x, 
      y = y
    ), 
    label = expression(1-alpha ~"level (0.95)"),
    color = "red", 
    size  = 6.5,  
    data  = data.frame(
      x = 16, 
      y = 0.90, 
      bl = 2
    )
  )+
  labs(color = "n : ") +
  theme_bw()+
  theme(
    text             = element_text(size=25),
    legend.text      = element_text(size=17),
    legend.position  = "bottom",
    strip.background = element_rect(fill="grey95")
  )

pdf(widt=7, height=7, "figures/figure_2.pdf")
g2
dev.off()