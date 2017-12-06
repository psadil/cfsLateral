#!/usr/bin/Rscript
suppressMessages(require(docopt))
'Usage:
plot-tex.R [-d <data_type>  -f <full_or_trim> -p <pattern> -e <extras> -b <parameter>]

Options:
  -d data type [default: data]
  -f whether full dataset or not [default: full]
  -p name pattern for model, output, stan_dat files [default: mp_simple_fixed_cC_cS_cI_mC_mS_mI]
  -e extra note added to output [default: intercept]
  -b parameter to plot [default: beta]
]' -> doc

opts <- docopt(doc)

source(file.path(getwd(), 'R', 'dirs.R'))
source(file.path(getwd(), 'R', 'make_figures', 'params_helper.R'))
source(file.path(getwd(), 'R', 'fns.R'))
suppressMessages(require(ggplot2))
suppressMessages(require(Hmisc))
suppressMessages(require(rstan))
suppressMessages(require(gridExtra))
suppressMessages(require(ggmcmc))
suppressMessages(require(readr))
suppressMessages(require(rio))
library(tikzDevice)
library(ggpubr)

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

fmt_dcimals <- function(decimals=0){
    function(x) format(x,nsmall = decimals,scientific = FALSE)
}

colmap <- c('Not Studied'=cbPalette[7]
            ,'Word'=cbPalette[2]
            ,'CFS'=cbPalette[3]
            ,'Binocular'=cbPalette[4]
)

dir.out <- file.path(dir.figures, paste(opts$p, opts$d, opts$f, opts$e, sep="_"))
if (!dir.exists(dir.out)){
    dir.create(dir.out)
}

if(opts$d == 'sim'){
    dir.d <- file.path(dir.sim, opts$p)
}else if(opts$d == 'data'){
    dir.d <- file.path(dir.data)
}    
d <- readr::read_csv(file.path(dir.d, paste(opts$d, 'updated', opts$f, '.csv', sep = '_')))%>%
    mutate(obs = 1:n())


## Load samples
fit <- read_stan_samples(dir.samples, opts$p, opts$d, opts$f, n_distinct(d$subject), opts$e)
post<-rstan::extract(fit)


S_afc <- tibble(no_study = post$beta[, 1,1]
                , Word = post$beta[, 1,2] + post$beta[, 1,1]
                , CFS = post$beta[, 1,3] + post$beta[, 1,1]
                , Binocular = post$beta[, 1,4] + post$beta[, 1,1]) %>%
    gather(Condition, afc, no_study:Binocular) %>%
    mutate(Condition = plyr::mapvalues(Condition, from = c('no_study'), to = c('Not Studied'))) %>%
    mutate(., Condition=factor(Condition,levels=c('Not Studied','Word','CFS','Binocular'))) %>%
    mutate(id = 1:n())

S_name <- tibble(no_study = post$beta[, 2,1]
                 , Word = post$beta[, 2,2] + post$beta[, 2,1]
                 , CFS = post$beta[, 2,3] + post$beta[, 2,1]
                 , Binocular = post$beta[, 2,4] + post$beta[, 2,1]) %>%
    gather(Condition, named, no_study:Binocular) %>%
    mutate(Condition = plyr::mapvalues(Condition, from = c('no_study'), to = c('Not Studied'))) %>%
    mutate(., Condition=factor(Condition,levels=c('Not Studied','Word','CFS','Binocular')))%>%
    mutate(id = 1:n())


S2 <- full_join(S_afc, S_name, by=c('id','Condition')) 


p1 <- S2 %>%
  ggplot(aes( x = afc, y = named, fill = Condition) ) +
  # geom_point() +
  stat_ellipse(geom = "polygon",  type='norm') +
  scale_fill_manual(values=colmap) +
  # scale_color_manual(values=colmap) +
  scale_y_continuous(breaks=seq(from= -1, to=1, by=1), name ="Naming (Identity Knowledge)") +
  scale_x_continuous(breaks=seq(from= -.5, to=1.5, by=1), name ="2AFC (Visual Knowledge)") +
  # coord_cartesian(xlim=c(-1,1.5), ylim=c(-1,1.5)) +
  # geom_hline(yintercept = 0) +
  # geom_vline(xintercept = 0) +
  coord_fixed(xlim=c(-.5, 1.5), ylim=c(-1, 1)) +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(        
    axis.ticks = element_blank()
    # , axis.title = element_text(margin = margin(rep(0,4)))
    # , axis.text.x = element_text(margin = margin(rep(0,4)))
    # , axis.text.y = element_text(margin = margin(rep(0,4)))
    , legend.position = "none"
    , axis.line = element_line(size=1)
    , plot.margin=margin(rep(0,4))
  ) +
    annotate("text", x=0.2, y=.75, label = "Estimated Effect") 
# +
#   ggsave(filename = file.path(dir.out, 'betas.png'))


# tikz(file.path(dir.out, 'sta-betas.tikz'), width = 3, height = 2)
# plot(p1)
# dev.off()


conds <- c('Not Studied','Word','CFS','Binocular')
df <- read_csv('forR.csv') %>%
    mutate(Condition = factor(conds,levels=conds))
p2 <- df %>%
    ggplot(aes(x=AFC, y=Name, color=Condition)) +
    geom_errorbar(aes(ymin=Name-errY, ymax=Name+errY), width=.01, size=1.5) +
    geom_errorbarh(aes(xmin=AFC-errX, xmax=AFC+errX), height=.01, size=1.5) +
    scale_y_continuous(labels = fmt_dcimals(), breaks=c(.3, .45, .6), name = "Naming (Identity Knowledge)") +
    scale_x_continuous(labels = fmt_dcimals(), breaks=c(.5, .65, .8), name = "2AFC (Visual Knowledge)") +
    scale_colour_manual(values=colmap) +
    coord_fixed(xlim=c(.45, .85), ylim=c(.25, .65)) +
    theme_classic(base_size = 12, base_family = "Arial") +
    theme(        
        axis.ticks = element_blank()
        , legend.position = "none"
        , axis.line = element_line(size=1)
        , plot.margin=margin(rep(0,4))
    ) +
    annotate("text", x=.65, y=.5625, label = "Proportion Correct") +
    geom_path(aes(x=xPred,y=yPred),color='black', linetype='longdash', size=1) 

# +
#     ggsave(filename = file.path(dir.out, 'sta-data.png'))

# plot(p2)


# grid.arrange(p2, p1, nrow=2)
figure <- ggpubr::ggarrange(p1, p2, labels = c("(a)", "(b)"),
                            ncol = 2, nrow = 1,
                            common.legend = TRUE, legend = "bottom") 
tikz(file.path(dir.out,'sta-results.tikz'), width = 6, height = 4)
plot(figure)
dev.off()



tikz(file.path(dir.out,'predictions-overview.tikz'), width = 3, height = 2)

tibble(x = c(.1, .1, .1),
       y = c(.1, .1, .1),
       xend = c(.25, .75, .75), 
       yend = c(.75, .25, .75), 
       condition = factor(c('Word', 'CFS', 'Binocular'), levels = c('Not Studied','Word', 'CFS', 'Binocular'))
) %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = condition)) +
    geom_segment(size = 2, arrow = arrow(length = unit(0.03, "npc"))) +
    geom_point(x=.1,y=.1, size=5, color = colmap[1]) +
    scale_y_continuous(labels = NULL, name = "Identity Knowledge") +
    scale_x_continuous(labels = NULL, name = "Visual Knowledge") +
    scale_colour_manual(values=colmap) +
    coord_fixed(xlim=c(0, 1), ylim=c(0, 1)) +
    theme_classic(base_size = 12) +
    theme(        
        axis.ticks = element_blank()
        , legend.position = "none"
        , axis.line = element_line(size=1)
        , plot.margin=margin(rep(0,4))
    )

dev.off()
