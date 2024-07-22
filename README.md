# Example code from “Bayesian views of generalized additive modelling”

This repo contains the code and data to reproduce the examples in the
paper [*Bayesian views of generalized additive
modelling*](https://arxiv.org/abs/1902.01330).

We begin by loading some useful packages and the data available here
contained in `fish.RData`. Processing steps can be found at [this
repo](https://github.com/dill/RACE_bering_sea). We also clean-up the
column names for easier printing later.

    library(mgcv)

    ## Loading required package: nlme

    ## This is mgcv 1.9-1. For overview type 'help("mgcv-package")'.

    library(ggplot2)
    library(viridis)

    ## Loading required package: viridisLite

    library(gratia)
    library(patchwork)
    library(tidyr)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:nlme':
    ## 
    ##     collapse

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    # load some fishy data
    load("fish.RData")

    # pretty printer
    fish$Bottom <- fish$BOT_TEMP
    fish$Surface <- fish$SURF_TEMP
    fish$Depth <- fish$BOT_DEPTH
    fish$Year <- fish$YEAR
    fish$BOT_TEMP <- NULL
    fish$SURF_TEMP <- NULL
    fish$BOT_DEPTH <- NULL
    fish$YEAR <- NULL

# Example 1 - 3.1 Term selection

For this example we only use the data from 2010, so let’s select that
first:

    fish2010 <- subset(fish, Year==2010)

Now let’s fit our three models, using all the available covariates:

    # fit using "normal" tprs basis
    b_norm <- gam(NUMCPUE ~ s(x, y, k=40, bs="tp") +
                            s(Depth, k=15, bs="tp") +
                            s(Bottom, k=15, bs="tp") +
                            s(Surface, k=15, bs="tp"),
                  data=fish2010, family=tw(), method="REML")

    # fit using shrinkage tprs basis (note use of "ts" basis)
    b_term <- gam(NUMCPUE ~ s(x, y, k=40, bs="ts") +
                            s(Depth, k=15, bs="ts") +
                            s(Bottom, k=15, bs="ts") +
                            s(Surface, k=15, bs="ts"),
                  data=fish2010, family=tw(), method="REML")

    # fit using double penalty (note use of "select=TRUE")
    b_term_sel <- gam(NUMCPUE ~ s(x, y, k=40) +
                                s(Depth, k=15) +
                                s(Bottom, k=15) +
                                s(Surface, k=15),
                  data=fish2010, family=tw(), method="REML", select=TRUE)

We can then duplicate the plot from the paper, showing that the surface
temperature smooth is estimated as zero by both the double penalty and
shrinkage approaches:

    # bit of fiddling to put these on single plots
    model_list <- list(b_norm, b_term, b_term_sel)
    names(model_list) <- c("No selection", "Shrinkage smoother", "Extra penalty")
    term_list <- c("s(Depth)", "s(Bottom)", "s(Surface)")

    plot_dat <- c()

    # grab the per-smooth effects for each model, using gratias handy
    # smooth_estimates function to help us
    for(this_term in term_list){
      for(i in seq_along(model_list)){
        this_smoo <- gratia::smooth_estimates(model_list[[i]], this_term)
        this_smoo$model <- names(model_list)[i]
        this_smoo$term <- this_term

        # some annoying processing to get things to plot nicely
        this_smoo[["covar"]] <- this_smoo[[sub("s\\((.+)\\)", "\\1", this_term)]]
        this_smoo[["covname"]] <- sub("s\\((.+)\\)", "\\1", this_term)
        this_smoo[[sub("s\\((.+)\\)", "\\1", this_term)]] <- NULL

        plot_dat <- rbind.data.frame(plot_dat, this_smoo)
      }
    }

    ggplot(plot_dat, aes(x=covar, group=model, fill=model)) +
      # uncertainty band using Nychka's result
      geom_ribbon(aes(ymin=.estimate-2*.se, ymax=.estimate+2*.se), alpha=0.4) +
      # mean effect line
      geom_line(aes(y=.estimate, colour=model)) +
      labs(y="Effect", fill="Model", colour="Model", x="") +
      facet_wrap(~covname, scale="free", strip.position="bottom") +
      theme_minimal() +
      theme(strip.placement = "outside", legend.title=element_text(size=8), legend.text=element_text(size=6))

![](README_files/figure-markdown_strict/selectcompare-plot1-1.png)

We can also see that the `summary` shows that the effective degrees of
freedom (EDFs) for the surface temperature smooth is almost zero for
those models:

    summary(b_norm)

    ## 
    ## Family: Tweedie(p=1.745) 
    ## Link function: log 
    ## 
    ## Formula:
    ## NUMCPUE ~ s(x, y, k = 40, bs = "tp") + s(Depth, k = 15, bs = "tp") + 
    ##     s(Bottom, k = 15, bs = "tp") + s(Surface, k = 15, bs = "tp")
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.09022    0.06844   45.15   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##               edf Ref.df     F  p-value    
    ## s(x,y)     31.651 36.432 5.420  < 2e-16 ***
    ## s(Depth)    8.241 10.069 5.121 9.82e-07 ***
    ## s(Bottom)  11.115 12.626 4.140 3.81e-06 ***
    ## s(Surface)  1.004  1.008 0.035     0.86    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.277   Deviance explained = 68.7%
    ## -REML = 1706.9  Scale est. = 3.4144    n = 376

    summary(b_term)

    ## 
    ## Family: Tweedie(p=1.745) 
    ## Link function: log 
    ## 
    ## Formula:
    ## NUMCPUE ~ s(x, y, k = 40, bs = "ts") + s(Depth, k = 15, bs = "ts") + 
    ##     s(Bottom, k = 15, bs = "ts") + s(Surface, k = 15, bs = "ts")
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.10869    0.06832    45.5   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                  edf Ref.df     F p-value    
    ## s(x,y)     31.017226     39 5.278  <2e-16 ***
    ## s(Depth)    7.868991     14 3.765  <2e-16 ***
    ## s(Bottom)  10.757123     14 3.605  <2e-16 ***
    ## s(Surface)  0.002427     14 0.000    0.94    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.281   Deviance explained = 68.3%
    ## -REML = 1728.9  Scale est. = 3.4342    n = 376

    summary(b_term_sel)

    ## 
    ## Family: Tweedie(p=1.745) 
    ## Link function: log 
    ## 
    ## Formula:
    ## NUMCPUE ~ s(x, y, k = 40) + s(Depth, k = 15) + s(Bottom, k = 15) + 
    ##     s(Surface, k = 15)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.09795    0.06824    45.4   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                  edf Ref.df     F p-value    
    ## s(x,y)     3.048e+01     39 5.461  <2e-16 ***
    ## s(Depth)   7.273e+00     14 3.486  <2e-16 ***
    ## s(Bottom)  1.028e+01     14 3.841  <2e-16 ***
    ## s(Surface) 1.863e-04     14 0.000   0.868    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.284   Deviance explained = 68.6%
    ## -REML = 1712.4  Scale est. = 3.4121    n = 376

# Example 2: 3.2 Uncertainty around smooth terms

Now to illustrate Nychka’s point, that the intervals generated using
mean +/- se\*Z\_alpha have good across the function properties (but can
under/over cover at the peaks/troughs).

    # First do the simulation approach
    set.seed(3141)

    # generate coefficients using the mean and covariance from the model
    n <- 1000
    betas <- rmvn(n, coef(b_term), vcov(b_term))

    # setup a prediction grid, since we only care about the Depth smooth
    # we can set everything else to zero.
    xx <- data.frame(Surface = 0,
                     Bottom  = 0,
                     Depth   = seq(min(fish2010$Depth), max(fish2010$Depth), length=400),
                     x       = 0,
                     y       = 0)

    # build the prediction matrix ("L_p" in the paper)
    Xp <- predict(b_term, xx, type="lpmatrix")

    # here we only care about the Depth effect, so zero the rest of
    # the coefficients
    betas[, !grepl("Depth", colnames(betas))] <- 0

    # make predictions
    preds <- Xp %*% t(betas)


    # Nychka intervals can be obtained using se.fit to get the per-prediction
    # standard errors
    nych <- predict(b_term, xx, se.fit=TRUE, type="terms")

    # built the plot data, taking the quantiles for the simulation method and
    # using the formula for the Nychka method
    plot_dat <- data.frame(Depth=rep(xx$Depth, 2))
    plot_dat$upper <- c(apply(preds, 1, quantile, 0.975), # quantile
                        nych$fit[, 2] + qnorm(0.975, 0, 1) * nych$se.fit[, 2]) # nychka
    plot_dat$lower <- c(apply(preds, 1, quantile, 0.025), # quantile
                        nych$fit[, 2] - qnorm(0.975, 0, 1) * nych$se.fit[, 2]) # nychka

    # create labels for plotting
    plot_dat$type <- c(rep("Quantile", 400), rep("Nychka", 400))

    # mean
    plot_mean <- data.frame(Depth=xx$Depth, pred = nych$fit[, 2])

    # put it together
    preds <- as.data.frame(preds)
    preds$Depth <- xx$Depth
    preds <- pivot_longer(preds, cols=-c(Depth))

    # make the plot
    ggplot(plot_dat) +
      geom_line(aes(y=value, group=name, x=Depth), lwd=0.15, data=preds, alpha=0.1) +
      geom_ribbon(aes(x=Depth, ymin=lower, ymax=upper, group=type, fill=type), alpha=0.5) +
      geom_line(aes(y=pred, x=Depth), lwd=0.3, lty=2, data=plot_mean) +
      labs(x="Depth", y="s(Depth)", fill="Method") +
      theme_minimal() +
      coord_cartesian(expand=FALSE)

![](README_files/figure-markdown_strict/nychka-1.png)

We can see there’s not much between these methods here!

# Example 3: 3.3 Posterior simulation/parametric bootstrap

Finally we use the general posterior sampling approach to make summaries
of a spatio-temporal model at given time-points.

First we fit the model, where we use the tensor product (`te`) to
construct a 2D smooth of space (`x`, `y`) and a 1D smooth of time
(`Year`). The model knows the dimensions due to the grouping dimension
argument (`d=c(2,1)`).

    b_t2 <- gam(NUMCPUE ~ te(x, y, Year, k=10, bs="ts", d=c(2,1)),
                data=fish, family=tw(), method="REML")

We can then run the Metropolis-Hastings sampler to get posterior samples
for the coefficients. Note that some fiddling with `t.df` (degrees of
freedom of the proposal *t*-distribution) and `rw.scale` (scale of
random walk) is needed to get reasonable acceptance.

    set.seed(1971)
    bs <- gam.mh(b_t2, ns=10000, burn=5000, thin=10, t.df=30, rw.scale=0.01)

    ##   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%
    ## fixed acceptance =  0.1653   RW acceptance =  0.6195

Once we have our samples (stored in `bs$bs`) we can construct our
predictions, using a similar procedure to the above.

    # make the grid of stations for each year
    xx <- grid %>%
      arrange(YEAR, STATION)
    xx$Year <- xx$YEAR

    # generate the prediction matrix from the prediction data
    Xp <- predict(b_t2, xx, type="lpmatrix")

    # storage
    res <- matrix(NA, nrow(bs$bs), length(unique(xx$Year)))

    # now run through the algorithm presented in the paper
    for(i in 1:nrow(bs$bs)){
      # make predictions
      preds <- exp(Xp %*% bs$bs[i, ])

      # get our summary: sum predictions per time period
      aa <- aggregate(preds, list(xx$Year), sum)

      # store the per-year predictions
      res[i, ] <- aa[, 2]
    }

Now we have our summaries, we can do a little processing to get the
figure

    # storage for plot data
    plot_dat <- data.frame(Year=sort(unique(xx$Year)))

    # quantile uncertainty band
    plot_dat$lower <- apply(res, 2, quantile, 0.025)
    plot_dat$upper <- apply(res, 2, quantile, 0.975)
    plot_dat$med <- apply(res, 2, quantile, 0.5)
    plot_dat$mean <- apply(res, 2, mean)


    # get the data summaries to overplot as points
    fdat <- fish %>%
      group_by(Year) %>%
      mutate(total = sum(NUMCPUE)) %>%
      select(Year, total) %>%
      distinct()


    # get the mean prediction using predict (we could use colMeans on res too)
    xx$pp <- predict(b_t2, xx, type="response")


    # plot all
    ggplot() +
      geom_ribbon(aes(x=Year, ymin=lower, ymax=upper),
                  alpha=0.5, data=plot_dat, fill="#A1E3B4") +
      geom_point(aes(x=Year, y=total), data=fdat) +
      geom_line(aes(y=med, x=Year), lwd=0.5, data=plot_dat, lty=2) +
      labs(x="Year", y="Abundance") +
      theme_minimal() +
      coord_cartesian(ylim=range(fdat$total)+c(-700, 700),
                      xlim=range(xx$Year)+c(-1,1), expand=FALSE) +
      theme(legend.position="bottom")

![](README_files/figure-markdown_strict/plotit-1.png)
