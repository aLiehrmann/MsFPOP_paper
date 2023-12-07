#------------------------------------------------------------------------------#
#- !! PLEASE CREATE THESE FOLDERS BEFORE running this script : ----------------#
#- 'figures' and 'data' !! ----------------------------------------------------#
#------------------------------------------------------------------------------#
#- This script saves and plots the runtime of Ms.PELT and Ms.FPOP on iid ------#
#- Gaussian signals with : ----------------------------------------------------#
#- (1) no changepoint or ------------------------------------------------------#
#- (2) a number of changepoints that increases linearly with the profile size -#
#- For both scenarios we increase the profile size (n) ------------------------#
#------------------------------------------------------------------------------#

source("R/load.R")

#------------------------------------------------------------------------------#
#- simulations ----------------------------------------------------------------#
#------------------------------------------------------------------------------#

#- simulations on scenario (1) ------------------------------------------------#
sim.time.all <- function(n, max=2^7*10^3){
  y   <- rnorm(n)
  tps <- c(NA, NA)
  if(n <= max){
    tps[1] <- system.time(res <- MsPELT(
      y, 
      beta  = 2.25, 
      alpha = 9+2.25*log(length(y))
    ))[3]
  }
  tps[2] <- system.time(res <- MsFPOP(
    y, 
    beta  = 2.25, 
    alpha = 9+2.25*log(length(y))
  ))[3]
  data.frame(
    n       = n, 
    runtime = tps, 
    met    = c("Ms.PELT", "Ms.FPOP"))
}

ns  <- rep(1000* 2^(0:12), each=1)
res <- do.call(rbind, lapply(ns, FUN=sim.time.all))
saveRDS(res,"data/simple_runtime.rds")

#- simulations on scenario (2) ------------------------------------------------#
sim.time.all2 <- function(n, max=2^13*10^3){
  y   <- rnorm(n)+ rep(c(0, 1), each=1000)
  tps <- c(NA, NA)
  if(n <= max){ 
    tps[1] <- system.time(res <- MsPELT(
      y, 
      beta  = 2.25, 
      alpha = 9+2.25*log(length(y))
    ))[3]
  }
  tps[2] <- system.time(res <- MsFPOP(
    y, 
    beta  = 2.25, 
    alpha = 9+2.25*log(length(y))
  ))[3]
  data.frame(
    n       = n, 
    runtime = tps, 
    met     = c("Ms.PELT", "Ms.FPOP")
  )
}

ns   <- rep(1000* 2^(0:12), each=1)
res2 <- do.call(rbind, lapply(ns, FUN=sim.time.all2))
saveRDS(res2,"data/simple_runtime_2.rds")

#------------------------------------------------------------------------------#
#- figures --------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#- plot runtime for scenario (1) ----------------------------------------------#
res <- readRDS("data/simple_runtime.rds")

p1 <- ggplot(
  res, 
  aes(
    x     = n, 
    y     = runtime, 
    color = met)
) + 
geom_point() + 
stat_smooth()+
scale_x_continuous(
  trans  = 'log', 
  labels = scales::number_format(accuracy = 1)
) + 
scale_y_continuous(
  "runtime (s)", 
  trans  = 'log', 
  labels = scales::number_format(accuracy = 0.01)
) +
theme_bw()+
labs(
  color = "method :"
)+
theme(
  text            = element_text(size=20),
  legend.position = "bottom"
)

#- plot runtime for scenario (2) ----------------------------------------------#
res2 <- readRDS("data/simple_runtime_2.rds")

p2 <- ggplot(
  res2, 
  aes(
    x     = n, 
    y     = runtime, 
    color = met
  )
) + 
geom_point() + 
stat_smooth()+
scale_x_continuous(
  trans  = 'log', 
  labels = scales::number_format(accuracy = 1)
) + 
scale_y_continuous(
  "", 
  trans  = 'log', 
  labels = scales::number_format(accuracy = 0.01)
) +
theme_bw()+
labs(
  color = "method :"
)+
theme(
  text            = element_text(size=20),
  legend.position = "bottom"
)

#- plot both scenarios --------------------------------------------------------#
pdf.options(reset = TRUE, onefile = FALSE)
pdf("figures/figure_1.pdf",height=6.7, width=14.3)
ggpubr::ggarrange(
  p1, 
  p2, 
  labels = c("A", "B"),
  font.label = list(size = 35),
  common.legend = TRUE, legend = "bottom"
)
dev.off()