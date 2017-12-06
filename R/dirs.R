# bioler plate code for creation of directory groups

wd <- 'D:/gitlab/reproducible-stan'

dir.stan_dat <- file.path('output', 'stan_dat')
dir.sim <- file.path('output', 'sim')
dir.samples <- file.path('output', 'stan_samples')
dir.stan <- file.path('stan')
dir.data <- file.path('output', 'data')
dir.figures <- file.path('output', 'figures')
dir.ic <- file.path('output', 'ic')
dir.report <- file.path('output', 'report')


if (!dir.exists(dir.stan_dat)){
    dir.create(dir.stan_dat)
}
if (!dir.exists(dir.sim)){
    dir.create(dir.sim)
}
if (!dir.exists(dir.data)){
    dir.create(dir.data)
}
if (!dir.exists(dir.stan)){
    dir.create(dir.stan)
}
if (!dir.exists(dir.samples)){
    dir.create(dir.samples)
}
if (!dir.exists(dir.figures)){
    dir.create(dir.figures)
}
if (!dir.exists(dir.ic)){
    dir.create(dir.ic)
}
if (!dir.exists(dir.report)){
    dir.create(dir.report)
}


