#------------------------------------------------------------------------------#
#- !! PLEASE CREATE THESE FOLDERS BEFORE running this script : ----------------#
#- 'figures' and 'data' !! ----------------------------------------------------#
#------------------------------------------------------------------------------#
#- This script saves and plots the runtime of Ms.PELT, Ms.FPOP, FPOP and PELT -#
#- on iid Gaussian signals of fixed size and an increasing number of segments -#
#------------------------------------------------------------------------------#

source("R/load.R")

#------------------------------------------------------------------------------#
#- simulations ----------------------------------------------------------------#
#------------------------------------------------------------------------------#

#- create a signal of size n with varying number of segments (K) --------------#
one_signal <- function(K, n, seed){
  set.seed(seed)
  if(K > 1){
    bkp <- sort(sample(1:(n-1), K-1))
    lg <- diff(c(0, bkp, n))
    signal <- rep(rep(c(0, 1), length(lg))[1:length(lg)], lg)[1:n]
  } else {
    signal <- rep(0, n)		
  }
  signal+ rnorm(n,sd=1)
}

#- all compared changepoint estimation procedures -----------------------------#
all_methods <- list(
  msFPOP = function(y) {
    MsFPOP(
      y     = y,
      beta  = 2.25,
      alpha = 9+2.25*log(length(y))
    )
  },
  msFPOP_all = function(y) {
    MsFPOP(
      y     = y,
      beta  = 2.25,
      alpha = 9+2.25*log(length(y)),
      sampling_method = "all"
    )
  },
  msFPOP_s2 = function(y) {
    MsFPOP(
      y     = y,
      beta  = 2.25,
      alpha = 9+2.25*log(length(y)),
      sampling_method = "rand",
      sampling_method_parameter = 2
    )
  },
  msFPOP_s3 = function(y) {
    MsFPOP(
      y     = y,
      beta  = 2.25,
      alpha = 9+2.25*log(length(y)),
      sampling_method = "rand",
      sampling_method_parameter = 3
    )
  },
  msFPOP_min2 = function(y) {
    MsFPOP(
      y     = y,
      beta  = 2.25,
      alpha = 9+2.25*log(length(y)),
      min_segment = 1
    )
  },
  msFPOP_min3 = function(y) {
    MsFPOP(
      y     = y,
      beta  = 2.25,
      alpha = 9+2.25*log(length(y)),
      min_segment = 2
    )
  },
  msFPOP_min4 = function(y) {
    MsFPOP(
      y     = y,
      beta  = 2.25,
      alpha = 9+2.25*log(length(y)),
      min_segment = 3
    )
  },
  msPELT = function(y) {
    MsPELT(
      y     = y,
      beta  = 2.25,
      alpha = 9+2.25*log(length(y)),
    )
  },
  FPOP = function(y) {
    fpopw::Fpop(
      x      = y,
      lambda = 2*log(length(y))
    )
  },
  PELT = function(y) {
    cpt.mean(
      y, 
      method    = "PELT", 
      penalty   = "Manual",
      pen.value = 2*log(length(y))
    )
  }
)

#- tested profile size --------------------------------------------------------#
n        <- 10^5

#- number of replicates by scenario -------------------------------------------#
rep      <- 1:30

#- number of segments ---------------------------------------------------------#
K        <- c(1, seq(5, 50, by =5), seq(100, 1000, by =50))
nb_seeds <- length(rep)*length(K)
method   <- c(
  "msFPOP",
  "msFPOP_all",
  "msFPOP_s2",
  "msFPOP_s3",
  "msPELT",
  "FPOP", 
  "PELT"
)
params   <- expand.grid(
  n, 
  rep, 
  K, 
  method
)
colnames(params) <- c("n", "rep", "K", "method")
params$seeds <- rep(1:nb_seeds+2021, length(method))

#- same for n = 10^6 ----------------------------------------------------------#
n        <- 10^6
rep      <- 1:5
K        <- c(1, seq(500, 10000, by =500))
nb_seeds <- length(rep)*length(K)
method   <- c(
  "msFPOP",
  "PELT",
  "FPOP",
  "msFPOP_min2",
  "msFPOP_min3",
  "msFPOP_min4",
)
params_2   <- expand.grid(
  n, 
  rep, 
  K, 
  method
)
colnames(params_2) <- c("n", "rep", "K", "method")
params_2$seeds <- rep(1:nb_seeds+2021, length(method))
params <- rbind(params, params_2)
all_res <- do.call(rbind,lapply(
  1:nrow(params),
  function(i) {
    y <- one_signal(
      K    = params[i,]$K,
      n    = params[i,]$n,
      seed = params[i,]$seed
    )
    runtime <- system.time(
      all_methods[[as.character(params[i,]$method)]](y) 
    )[["user.self"]]
    data.frame(
      runtime = runtime,  
      K       = params[i,]$K,
      n       = params[i,]$n,
      seed    = params[i,]$seed,
      method  = params[i,]$method
    )
  }
))
saveRDS(all_res, "data/runtime_cmp_al.rds")

#------------------------------------------------------------------------------#
#- figures --------------------------------------------------------------------#
#------------------------------------------------------------------------------#

all_res <- readRDS("data/runtime_cmp_al.rds")

#- choose the order of levels -------------------------------------------------#
all_res$method <- factor(
  all_res$method, 
  levels=c(
    "msFPOP", 
    "msFPOP_s2", 
    "msFPOP_s3", 
    "msFPOP_all", 
    "PELT", 
    "FPOP", 
    "msPELT",
    "msFPOP_min2",
    "msFPOP_min3",
    "msFPOP_min4",
  )
)

#- verbose --------------------------------------------------------------------#
all_res$criterion<-NA
all_res$criterion[grep(x=all_res$method, pattern="ms")] <- "multiscale penalized\nlikelihood"
all_res$criterion[grep(x=all_res$method, pattern="ms", invert=TRUE)] <- "penalized likelihood"
all_res$criterion <- factor(all_res$criterion)

#- figure S3 ------------------------------------------------------------------#
g1 <- ggplot(
  data = all_res[all_res$n==10^6,],
  aes(
    x     = K, 
    y     = runtime, 
    color = method
  )
)+
geom_point(alpha = 0.05)+
geom_smooth(
 se = TRUE, 
 aes(linetype=criterion)
)+
scale_y_continuous(
  tr = "log10", 
  "runtime (s)"
)+
scale_x_continuous(
  tr = "log10"
)+
xlab(
  "true number of changepoints"
)+
labs(
  color    = "method : ", 
  linetype = "criterion : "
) +
scale_color_discrete(
  labels=c(
    "Ms.FPOP",
    "PELT",
    "FPOP"
  )
) +
theme_bw()+
theme(
  text = element_text(size=25),
  legend.text = element_text(size=15),
  legend.title = element_text(size=20),
  legend.position="bottom"
)+
guides(linetype = guide_legend(nrow = 2), col=guide_legend(nrow = 3))

pdf("figures/figure_S3.pdf",width=10, height=9.5)
g1
dev.off()

#- figure 3 -------------------------------------------------------------------#
g2 <- ggplot(
  data = all_res[all_res$n==10^5 & all_res$method %in% c("msFPOP","msPELT","FPOP","PELT"),],
  aes(
    x     = K, 
    y     = runtime, 
    color = method
  )
)+
geom_point(alpha = 0.05)+
geom_smooth(
 se = TRUE, 
 aes(linetype=criterion)
)+
scale_y_continuous(
  tr = "log10", 
  "runtime (s)"
)+
scale_x_continuous(
  tr = "log10"
)+
xlab(
  "true number of changepoints"
)+
labs(
  color    = "method : ", 
  linetype = "criterion : "
) +
scale_color_discrete(
  labels=c(
    "Ms.FPOP",
    "PELT",
    "FPOP",
    "Ms.PELT"
  )
) +
theme_bw()+
theme(
  text = element_text(size=25),
  legend.text = element_text(size=15),
  legend.title = element_text(size=20),
  legend.position="bottom"
)+
guides(linetype = guide_legend(nrow = 2), col=guide_legend(nrow = 4))

pdf("figures/figure_3.pdf",width=10, height=9.5)
g2
dev.off()

#- figure S2 ------------------------------------------------------------------#
g3 <- ggplot(
  data = all_res[all_res$n==10^5 & all_res$method %in% c("msFPOP","msFPOP_s2","msFPOP_s3", "msFPOP_all"),],
  aes(
    x     = K, 
    y     = runtime, 
    color = method
  )
)+
geom_point(alpha = 0.05)+
geom_smooth(
 se = TRUE
)+
scale_y_continuous(
  tr = "log10", 
  "runtime (s)"
)+
scale_x_continuous(
  tr = "log10"
)+
xlab(
  "true number of changepoints"
)+
labs(
  color    = "method : "
) +
scale_color_discrete(
  labels=c(
    "Ms.FPOP (rand 1)",
    "Ms.FPOP (rand 2)",
    "Ms.FPOP (rand 3)",
    "Ms.FPOP (all)"
  )
) +
theme_bw()+
theme(
  text = element_text(size=25),
  legend.text = element_text(size=15),
  legend.title = element_text(size=20),
  legend.position="bottom"
)+
guides(col=guide_legend(nrow = 4))

pdf("figures/figure_S2.pdf",width=10, height=9.5)
g3
dev.off()

#- figure S4 ------------------------------------------------------------------#
g4 <- ggplot(
  data = all_res[all_res$n==10^6 & all_res$method %in% c("msFPOP","msFPOP_min2","msFPOP_min3", "msFPOP_min4"),],
  aes(
    x     = K, 
    y     = runtime, 
    color = method
  )
)+
geom_point(alpha = 0.05)+
geom_smooth(
 se = TRUE
)+
scale_y_continuous(
  tr = "log10", 
  "runtime (s)"
)+
scale_x_continuous(
  tr = "log10"
)+
xlab(
  "true number of changepoints"
)+
labs(
  color    = "minimum segment length : "
) +
scale_color_discrete(
  labels=c(
    "none"
    "2",
    "3",
    "4"
  )
) +
theme_bw()+
theme(
  text = element_text(size=25),
  legend.text = element_text(size=15),
  legend.title = element_text(size=20),
  legend.position="bottom"
)+
guides(col=guide_legend(nrow = 4))

pdf("figures/figure_S4.pdf",width=10, height=9.5)
g4
dev.off()
