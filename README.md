# What is visualizeR?

`visualizeR` is an R package for climate data visualization, with special focus on ensemble forecasting and uncertainty communication. It includes functions for visualizing climatological, forecast and evaluation products, and combinations of them. Find out more about this package at the [visualizeR wiki](https://github.com/SantanderMetGroup/visualizeR/wiki). 

This package is part of the [climate4R bundle](http://www.meteo.unican.es/climate4r), formed by `loadeR`, `transformeR`, `downscaleR` and `visualizeR`.

The recommended installation procedure is to use the `install_github` command from the devtools R package. See the [installation info](https://github.com/SantanderMetGroup/visualizeR/wiki/installation) instructions in the GitHub [wiki](https://github.com/SantanderMetGroup/visualizeR/wiki).

```r
devtools::install_github(c("SantanderMetGroup/transformeR", "SantanderMetGroup/visualizeR"))
```
**IMPORTANT:** Note that `transformeR` must be previously installed on your system 

---
Reference and further information: 

Frías M.D., Iturbide M., Manzanas R., Bedia J., Fernández J., Herrera S., Cofiño A.S., Gutiérrez J.M. (2018) An R package to visualize and communicate uncertainty in seasonal climate prediction. **Environmental Modelling and Software**, 99, 101-110, http://doi.org/10.1016/j.envsoft.2017.09.008

Cofiño et al. (2018) The ECOMS User Data Gateway: Towards seasonal forecast data provision and research reproducibility in the era of Climate Services. **Climate Services**, http://dx.doi.org/10.1016/j.cliser.2017.07.001.
