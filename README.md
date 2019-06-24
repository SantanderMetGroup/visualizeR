# What is visualizeR?

`visualizeR` is an R package for climate data visualization, with special focus on ensemble forecasting and uncertainty communication. It includes functions for visualizing climatological, forecast and evaluation products, and combinations of them. Find out more about this package at the [visualizeR wiki](https://github.com/SantanderMetGroup/visualizeR/wiki). 

This package is part of the [climate4R bundle](http://www.meteo.unican.es/climate4r), formed by `loadeR`, `transformeR`, `downscaleR` and `visualizeR`. The recommended installation procedure is to use the `install_github` command from the devtools R package:

```r
devtools::install_github(c("SantanderMetGroup/transformeR", "SantanderMetGroup/visualizeR"))
```
**NOTE:** Note that `transformeR` is a dependency for `visualizeR`. Note that `transformeR` also includes illustrative datasets for the `climate4r`framework.

---
Reference and further information: 

Frías M.D., Iturbide M., Manzanas R., Bedia J., Fernández J., Herrera S., Cofiño A.S., Gutiérrez J.M. (2018). An R package to visualize and communicate uncertainty in seasonal climate prediction. **Environmental Modelling and Software**, 99, 101-110, http://doi.org/10.1016/j.envsoft.2017.09.008

**[General description of the climate4R framework]** Iturbide et al. (2019). The R-based climate4R open framework for reproducible climate data access and post-processing. **Environmental Modelling and Software**, 111, 42-54. https://doi.org/10.1016/j.envsoft.2018.09.009
Check out the companion notebooks for the two examples [GitHub](https://github.com/SantanderMetGroup/notebooks).

**[Seasonal forecasting applications]** Cofiño A.S. et al. (2018). The ECOMS User Data Gateway: Towards seasonal forecast data provision and research reproducibility in the era of Climate Services. **Climate Services**, 9, 33-43. http://doi.org/10.1016/j.cliser.2017.07.001

**[Climate change applications]** Fernández et al. (2019). Consistency of climate change projections from multiple global and regional model intercomparison projects. **Climate Dynamics**, 52, 1139-1156. https://doi.org/10.1007/s00382-018-4181-8
