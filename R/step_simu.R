#------------------------------------------------------------------------------#
#- !! PLEASE CREATE THESE FOLDERS BEFORE running this script : ----------------#
#- 'figures' and 'data' !! ----------------------------------------------------#
#------------------------------------------------------------------------------#
#- This script compares the accuracy of Ms.FPOP and FPOP step-like iid --------#
#- Gaussian signals (1 changepoint). We make vary the profile size and the ----#
#- position of the changepoint. -----------------------------------------------#
#------------------------------------------------------------------------------#

source("R/load.R")

#------------------------------------------------------------------------------#
#- simulations ----------------------------------------------------------------#
#------------------------------------------------------------------------------#

#- generate one iid gaussian signal with cp as position of changepoints -------# 
one_signal <- function(
  n, 
  cp,
  difficulty = sqrt(70/n)
) {
  S_length <- diff(cp)
  difficulty*rep(
    rep(c(0,1), length(S_length))[1:length(S_length)], 
    S_length
  ) + rnorm(n, sd=1)
}

logical_threads <- 85
#- size of tested profiles ----------------------------------------------------#
n               <- c(10^3, 10^4, 10^5)
#- number of replicates -------------------------------------------------------#
reps            <- 1:5000
#- number of positions tested -------------------------------------------------#
nb_cp           <- 40 

params <- do.call(rbind, lapply(
  n,
  FUN = function(n) {
    #- tested positions -------------------------------------------------------#
    cp               <- unique(as.integer(exp(seq(log(1), 
        log(as.integer(n*1/2)), length=nb_cp))))
    params           <- expand.grid(cp, reps, n)
    colnames(params) <- c("cp", "rep", "n")
    params
  }
))

#- simulations ----------------------------------------------------------------#
res <- mclapply(
  1:nrow(params),
  function(i) {
    #- generate one step-like iid gaussian signal (1 changepoint) -------------# 
    y <- one_signal(
      n          = params$n[[i]], 
      difficulty = sqrt(70/params$n[[i]]),
      cp         = c(0, params$cp[[i]], params$n[[i]])
    )
    cp_msfpop <- MsFPOP(
      y     = y, 
      beta  = 2.25, 
      alpha = 9+2.25*log(params$n[[i]])
    )$changepoints
    cp_fpop <- Fpop(
      x      = y, 
      lambda = 2*log(params$n[[i]])
    )$t.est
    list(
      n         = params$n[[i]],
      cp        = params$cp[[i]],
      cp_msfpop = cp_msfpop,
      cp_fpop   = cp_fpop
    )
  }, mc.cores = logical_threads
)

saveRDS(res, "data/step_simu.rds")


#------------------------------------------------------------------------------#
#- figures --------------------------------------------------------------------#
#------------------------------------------------------------------------------#

options(scipen=10000)
res <- readRDS("data/step_simu.rds")

#- count replicates with 0, 1 or >1 changepoints ------------------------------#
cp_msfpop <- sapply(res, function(x) length(x$cp_msfpop)-1)
cp_fpop <- sapply(res, function(x) length(x$cp_fpop)-1)
score <- c(
  cp_msfpop>1, cp_msfpop<1, cp_msfpop==1, 
  cp_fpop>1, cp_fpop<1, cp_fpop==1
)
type <- factor(rep(rep(
    c("more than one", "less than one", "exactly one"), 
    each = length(cp_msfpop)),
    2
  ), 
  levels= c("exactly one", "less than one", "more than one")
)
method <- rep(c("Ms.FPOP", "FPOP"), each=3*length(cp_msfpop))
cp <- rep(sapply(res, function(x) x$cp),6)
n <- rep(sapply(res, function(x) x$n), 6)
res_2 <- data.frame(type, method, res = score, cp, n)

#- proportion of replicates with 0, 1 or >1 changepoints ----------------------#
res_3 <- do.call(rbind, lapply(
  split(res_2, paste0(res_2$type, res_2$method, res_2$cp, res_2$n)),
  function(x) {
    data.frame(
      cp=x$cp[[1]], 
      type=x$type[[1]], 
      method=x$method[[1]], 
      rate = sum(x$res)/nrow(x),
      n = x$n[[1]]
    )
  }
))
res_3$n <- factor(
  paste0("n = ", res_3$n), 
  levels=c("n = 1000", "n = 10000", "n = 100000")
)

#- plot proportion of replicates with 0, 1 or >1 changepoints -----------------#
ggplot(
  data = res_3,
  aes(
    x=cp, 
    y=rate, 
    color=method
  )
)+
geom_line()+
facet_grid(type~n, scales = "free")+
theme_bw()+
scale_x_continuous(tr="log10")+
ylab("propotions")+
scale_color_discrete("method : ",
  labels=c(
  "FPOP",
  "Ms.FPOP"
))+
xlab(TeX(r'($\tau_1$)'))+
theme(
  text = element_text(size=25),
  legend.text = element_text(size=17),
  legend.position = "bottom",
  strip.background = element_rect(fill="grey95")
)
ggsave("figures/figure_S5.jpeg", width=12, height=13, dpi=350)
