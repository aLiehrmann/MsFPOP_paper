#------------------------------------------------------------------------------#
#- !! PLEASE CREATE THESE FOLDERS BEFORE running this script : ----------------#
#- 'figures' and 'data' !! ----------------------------------------------------#
#------------------------------------------------------------------------------#
#- This file compares the accuracy of Ms.FPOP and FPOP het-like iid -----------#
#- Gaussian signals (2 changepoint). We make vary the profile size and the ----#
#- position of the first changepoint. The position of the second changepoint --#
#- is fixed at 2/3 of the profile size ----------------------------------------#
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
#- number of positions tested between 1 to 1/3 --------------------------------#
nb_cp <- 30 


params <- do.call(rbind, lapply(
  n,
  FUN = function(n) {
    cp1_left         <- unique(as.integer(exp(seq(log(1),
        log(as.integer(n*1/3)), length=nb_cp))))
    cp1_right        <- as.integer(n*2/3) - cp1_left
    params           <- expand.grid(c(cp1_left, cp1_right), reps, n)
    colnames(params) <- c("cp1", "rep", "n")
    params$cp2       <- as.integer(params$n * 2/3)
    params$x         <- rep(cp1_left, length(reps)*2)
    params$side      <- rep(rep(c("left", "right"), each=length(cp1_left)), 
        length(reps))
    params
  }
))

#- simulations ----------------------------------------------------------------#
res <- mclapply(
  1:nrow(params),
  function(i) {
    #- generate one hat-like iid gaussian signal (2 changepoint) --------------# 
    y <- one_signal(
      n           = params$n[[i]], 
      difficulty  = sqrt(100/params$n[[i]]),
      cp          = c(0, params$cp1[[i]], params$cp2[[i]], params$n[[i]])
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
      cp1       = params$cp1[[i]],
      cp2       = params$cp2[[i]],
      side      = params$side[[i]],
      x         = params$x[[i]],
      cp_msfpop = cp_msfpop,
      cp_fpop   = cp_fpop
    )
  }, mc.cores=logical_threads
)

saveRDS(res, "data/hat_simu.rds")

#------------------------------------------------------------------------------#
#- figures --------------------------------------------------------------------#
#------------------------------------------------------------------------------#

options(scipen=10000)
res <- readRDS("data/hat_simu.rds")

#- count replicates with 0, 1, 2 or >2 changepoints ---------------------------#
cp_msfpop <- sapply(res, function(x) length(x$cp_msfpop)-1)
cp_fpop   <- sapply(res, function(x) length(x$cp_fpop)-1)
score     <- c(
  cp_msfpop>2, cp_msfpop<1, cp_msfpop==1, cp_msfpop==2, 
  cp_fpop>2, cp_fpop<1, cp_fpop==1, cp_fpop==2
)
type      <- factor(rep(rep(
  c("more than two", "less than one", "exactly one","exactly two"), 
  each = length(cp_msfpop)),
  2), levels=c("exactly two", "exactly one", "less than one", "more than two"))
method    <- rep(c("Ms.FPOP", "FPOP"), each=4*length(cp_msfpop))
cp1       <- rep(sapply(res, function(x) x$cp1), 8)
side      <- rep(sapply(res, function(x) x$side), 8)
x         <- rep(sapply(res, function(x) x$x), 8)
n         <- rep(sapply(res, function(x) x$n), 8)
res_2 <- data.frame(type, method, res = score, cp1, side, x, n)

#- proportion replicates with 0, 1, 2 or >2 changepoints ----------------------#
res_3 <- do.call(rbind, lapply(
  split(res_2, paste0(res_2$type, res_2$method, res_2$cp1, res_2$n, res_2$side)),
  function(x) {
    data.frame(
      cp1    = x$cp1[[1]], 
      type   = x$type[[1]], 
      method = x$method[[1]], 
      rate   = sum(x$res)/nrow(x),
      n      = x$n[[1]],
      side   = x$side[[1]],
      x      = x$x[[1]]
    )
  }
))
res_3$n <- paste0("n = ", res_3$n)

#- figure S5 ------------------------------------------------------------------#
ggplot(
  data = res_3[res_3$n %in% c("n = 1000", "n = 10000", "n = 100000"),],
  aes(
    x        = x, 
    y        = rate, 
    color    = method,
    linetype = side
  )
)+
geom_line(size=0.75)+
facet_grid(type~n, scales = "free")+
theme_bw()+
scale_x_continuous(tr="log10")+
xlab("x")+
ylab("proportions")+
scale_linetype_manual(
  TeX(r'($\tau_1$ : )'), 
  values = c("dashed", "dotted"), 
  labels = c(
    TeX(r'($x$)'),
    TeX(r'($\frac{2}{3}n - x$)')
  )
)+
scale_color_discrete("method : ",
  labels=c(
  "FPOP",
  "Ms.FPOP"
))+
theme(
  text             = element_text(size=25),
  legend.text      = element_text(size=17),
  legend.position  = "bottom",
  strip.background = element_rect(fill="grey95")
)
ggsave("figures/figure_S5.jpeg", width=13.5, height=14, dpi=350)

#- figure 4 -------------------------------------------------------------------#
g1 <- ggplot(
  data = res_3[
    res_3$n %in% c("n = 1000", "n = 10000", "n = 100000") & 
    res_3$type=="exactly two" &
    res_3$side == "left",],
  aes(
    x     = x, 
    y     = rate, 
    color = method
  )
)+
geom_line(size=0.75)+
facet_wrap(n~., scales = "free", ncol=1)+
theme_bw()+
scale_x_continuous(tr="log10")+
xlab(TeX(r'($\tau_1$)'))+
ylab(TeX(r'($R_{2}$)'))+
scale_linetype_manual(
  "x : ", 
  values= c("dashed", "dotted"), 
  labels=c(
    TeX(r'($\tau_1$)'),
    TeX(r'($\frac{2}{3}n - \tau_1$)')
  )
)+
scale_color_discrete("method : ",
  labels=c(
  "FPOP",
  "Ms.FPOP"
))+
theme(
  text             = element_text(size=25),
  legend.text      = element_text(size=17),
  legend.position  = "bottom",
  strip.background = element_rect(fill="grey95")
)
print(g1)

#- log-ratio ------------------------------------------------------------------#
res_4 <- res_3[res_3$method == "Ms.FPOP",]
res_4$method <- NULL
res_4$rate <- log2((res_3[res_3$method == "Ms.FPOP",]$rate+1)/
    (res_3[res_3$method == "Ms.FPOP",]$rate+1))
res_4$rate <- res_3[res_3$method == "Ms.FPOP",]$rate/
    res_3[res_3$method == "FPOP",]$rate
res_4$rate <- log2((res_3[res_3$method == "Ms.FPOP",]$rate)/
    (res_3[res_3$method == "FPOP",]$rate))

g2 <- ggplot(
  data = res_4[res_4$n %in% c("n = 1000", "n = 10000", "n = 100000") & 
    res_4$type=="exactly two" & res_4$side=="left",
  ],
  aes(
    x = x, 
    y = rate
  )
)+
geom_line(size=0.75)+
facet_wrap(~n, scales = "free", ncol=1)+
theme_bw()+
scale_x_continuous(tr="log10")+
xlab(TeX(r'($\tau_1$)'))+
ylab(TeX(r'($\Delta_{R_2}$)'))+
scale_linetype_manual("x : ", values=c("dashed", "dotted"), labels=c(
  TeX(r'($\tau_1$)'),
  TeX(r'($\frac{2}{3}n - \tau_1$)')
))+
scale_color_discrete("method : ",
  labels=c(
  "FPOP",
  "Ms.FPOP"
))+
theme(
  text             = element_text(size=25),
  legend.text      = element_text(size=17),
  legend.position  = "bottom",
  strip.background = element_rect(fill="grey95")
)

ggarrange(g1, g2, common.legend=TRUE, legend="bottom")
ggsave("figures/figure_4.jpeg", width=12.5, height=15, dpi=350)
