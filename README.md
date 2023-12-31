
# Analysis of Historical Togolese Temperatures

This repository serves as a home for an analysis of historical monthly
minimum and maximum temperatures for 10 cities in the west African
nation of [Togo](https://en.wikipedia.org/wiki/Togo). The data were
obtained from Togolese meteorologists, and the cities include: Lomé,
Sokodé, Kara, Atakpamé, Mango, Dapaong, Niamtougou, Sotouboua, Tabligbo,
and Kouma Konda. The project project is organized as follows:

- `R`: collection of `R` scripts responsible for cleaning and imputing
  the raw data, applying Singular Spectrum Analysis (SSA) to each city
  individually, and development of global models.
- `Figures`: all of the figures generated by the R code. Mostly
  diagnostic plots from the SSA phase where seasonal components were
  manually selected.
- `Presentations`: a collection of presentations given at different
  conferences by myself and/or Professor [Andy
  Long](https://www.nku.edu/~longa/)

<div class="figure" style="text-align: center">

<img src="Figures/togo.jpg" alt="Map of Togo" width="30%" />
<p class="caption">
Map of Togo
</p>

</div>

The main component of the analysis is the identification of important
seasonal trends in both sets of temperature data. This is done manually
using SSA, and the resulting periods are then incorporated into linear
mixed models which also include a random intercept for location and
geographical parameters (elevation, longitude, and latitude).

For this project I use the `Rssa` package, though I encourage you to
visit my [website](https://jacobenglert.rbind.io/post/ssa-from-scratch/)
for a walkthrough of how one could conduct such an analysis from
scratch.

## Global Models

Here is quick summary of the findings for the global models:

<div class="figure" style="text-align: center">

<img src="Figures/min_model.png" alt="Global Minimum Model Fit" width="80%" />
<p class="caption">
Global Minimum Model Fit
</p>

</div>

As far as minimum monthly average temperatures go, we see an overall
average increasing trend of approximately .032 degrees Celsius per year.
Some cities, such as Mango, have much greater variability in
temperatures, but the trend persists. In other cities like Niamtougou,
observed temperatures do not appear to be rising as dramatically, but we
still expect an increase due to the overall global trend.

<div class="figure" style="text-align: center">

<img src="Figures/max_model.png" alt="Global Maximum Model Fit" width="80%" />
<p class="caption">
Global Maximum Model Fit
</p>

</div>

In the average monthly maximum temperatures, the increasing trend of
0.019 degrees Celsius per year is not quite as alarming as the trend in
the minima, but worrisome nonetheless. Once again, there is
heterogeneity across cities, but the global trend persist throughout
most and the singular spectrum analysis appears to have adequately
captured seasonal trends.

## 3D Visualization of the impact of Geography

Here I’ve included an interactive 3D visualization of the impact of
latitude and longitude on both models. While I cannot share the data,
feel free to download the models from the `Output` folder and run the
following code to create your own versions of these to play around with.

``` r
max_model <- readr::read_rds(here::here('Output','max_model.rds'))
min_model <- readr::read_rds(here::here('Output','min_model.rds'))
```

``` r
# Minimum temperature model
min_geo_coef <- coef(summary(min_model))[c('Longitude','Latitude', 
                                           'I(Latitude^2)','I(Longitude^2)',
                                           'I(Latitude * Longitude)'),
                                         'Estimate']
min_geo <- function(long, lat){
  cbind(long, lat, lat^2, long^2, long*lat) %*% min_geo_coef
}
rgl::open3d()
```

    ## null 
    ##    6

``` r
rgl::plot3d(min_geo, 
            col = colorRampPalette(c("blue", "white", "red")), 
            xlab = "longitude", ylab = "latitude", zlab = "GeoMinTrend", 
            xlim = c(0,2), ylim = c(6,12),
            aspect = c(1, 1, 0.5))
```

![](README_files/figure-gfm/unnamed-chunk-5-1-rgl.png)<!-- -->

``` r
# Maximum temperature model
max_geo_coef <- coef(summary(max_model))[c('Longitude','Latitude', 
                                           'I(Latitude^2)','I(Longitude^2)',
                                           'I(Latitude * Longitude)'),
                                         'Estimate']

max_geo <- function(long, lat){
  cbind(long, lat, lat^2, long^2, long*lat) %*% max_geo_coef
}

rgl::open3d()
```

    ## null 
    ##   10

``` r
rgl::plot3d(max_geo, 
            col = colorRampPalette(c("blue", "white", "red")), 
            xlab = "Longitude", ylab = "Latitude", zlab = "GeoMaxTrend", 
            xlim = c(0,2), ylim = c(6,12),
            aspect = c(1, 1, 0.5))
```

![](README_files/figure-gfm/unnamed-chunk-6-2-rgl.png)<!-- -->
