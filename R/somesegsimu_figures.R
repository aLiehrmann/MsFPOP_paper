#------------------------------------------------------------------------------#
#- This file plots the the average number of times a method (among MOSUM, -----#
#- Ms.FPOP, FPOP) is at least as good as other methods (AE%) in terms of: -----#
#- (1) absolute difference between the true number of changes and the ---------#
#----- estimated number of changes (K), ---------------------------------------#
#- (2) mean squared error (mse), ----------------------------------------------#
#- (3) adjusted rand index (ari). ---------------------------------------------#
#------------------------------------------------------------------------------# 
#- The results are precomputed from a modified version of ---------------------#
#- Simulation_PerfStat.Rmd (see https://github.com/guillemr/SomeSegSimu). -----#
#- This modified version (m_Simulation_PerfStat.Rmd) needs to be executed from #
#- SomeSegSimu directory. -----------------------------------------------------#
#------------------------------------------------------------------------------#


source("R/load.R")

all <- readRDS("data/all.rds")
all <- lapply(
  all,
  function(x) {
   x$dataset2 <- sapply(strsplit(as.character(x$dataset), "_"), function(x) x[[1]])
   x$difficulty <- as.numeric(sapply(strsplit(as.character(x$dataset), "_"), function(x) x[[3]]))
   x$Method <- as.character(x$Method)
   #x[x$Method == "PsiFPOP",]$Method <- "Ms.FPOP"
   x
})

#- plot AE% based on (3) criterion --------------------------------------------# 
ggplot(
  all[[1]][all[[1]]$difficulty<3,],
  aes(
    x= difficulty,
    y = scoreN,
    color = Method
  )
) +
geom_line()+
facet_wrap(dataset2~., ncol=3, scale = "free")+
theme_bw()+
ylab("AE%")+
xlab("scaling factor for variance (difficulty)")+
scale_color_discrete("method : ", labels=c(
  TeX(r'(FPOP)'),
  TeX(r'(MOSUM)'),
  TeX(r'(Ms.FPOP)')
))+
theme(
  text = element_text(size=25),
  legend.text = element_text(size=17),
  legend.position = "bottom",
  strip.background = element_rect(fill="grey95")
)
ggsave("figures/somesegsimu_ari.jpeg", width=14, height=17, dpi=350)

#- plot AE% based on (2) criterion --------------------------------------------# 
ggplot(
  all[[2]][all[[2]]$difficulty<3,],
  aes(
    x= difficulty,
    y = scoreN,
    color = Method
  )
) +
geom_line()+
facet_wrap(dataset2~., ncol=3, scale = "free")+
theme_bw()+
ylab("AE%")+
xlab("scaling factor for variance (difficulty)")+
scale_color_discrete("method : ", labels=c(
  TeX(r'(FPOP)'),
  TeX(r'(MOSUM)'),
  TeX(r'(Ms.FPOP)')
))+
theme(
  text = element_text(size=25),
  legend.text = element_text(size=17),
  legend.position = "bottom",
  strip.background = element_rect(fill="grey95")
)
ggsave("figures/somesegsimu_mse.jpeg", width=14, height=17, dpi=350)

#- plot AE% based on (1) criterion --------------------------------------------# 
ggplot(
  all[[3]][all[[3]]$difficulty<3,],
  aes(
    x= difficulty,
    y = scoreN,
    color = Method
  )
) +
geom_line()+
facet_wrap(dataset2~., ncol=3, scale = "free")+
theme_bw()+
ylab("AE%")+
xlab("scaling factor for variance (difficulty)")+
scale_color_discrete("method : ", labels=c(
  TeX(r'(FPOP)'),
  TeX(r'(MOSUM)'),
  TeX(r'(Ms.FPOP)')
))+
theme(
  text = element_text(size=25),
  legend.text = element_text(size=17),
  legend.position = "bottom",
  strip.background = element_rect(fill="grey95")
)
ggsave("figures/somesegsimu_K.jpeg", width=14, height=17, dpi=350)
