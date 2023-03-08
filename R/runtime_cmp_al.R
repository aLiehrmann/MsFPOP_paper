#------------------------------------------------------------------------------#
#- This file saves and plots the runtime of Ms.PELT, Ms.FPOP, FPOP and PELT ---#
#- on iid Gaussian signals of fixed size and an increasing number of segments -#
#------------------------------------------------------------------------------#

source("R/load.R")

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
  msFPOP_s4 = function(y) {
    MsFPOP(
      y     = y,
      beta  = 2.25,
      alpha = 9+2.25*log(length(y)),
      sampling_method = "rand",
      sampling_method_parameter = 4
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
  "msFPOP_s4", 
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
  "FPOP"
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


#- simulations ----------------------------------------------------------------#
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

#- figure ---------------------------------------------------------------------#

all_res <- readRDS("data/runtime_cmp_al.rds")

all_res$method <- factor(
  all_res$method, 
  levels=c(
    "msFPOP", 
    "msFPOP_s2", 
    "msFPOP_s3", 
    "msFPOP_s4", 
    "msFPOP_all", 
    "PELT", 
    "FPOP", 
    "msPELT"
  )
)


pdf("figures/runtime_cmp_al.pdf",width=10, height=9.5)
ggplot(
  data = all_res[all_res$n==10^5,],
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
    "Ms.FPOP (rand 1)",
    "Ms.FPOP (rand 2)",
    "Ms.FPOP (rand 3)",
    "Ms.FPOP (rand 4)",
    "Ms.FPOP (all)",
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
)
dev.off()