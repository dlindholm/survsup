# survsup <img src="icons/survsup_icon.png" align="right" width="15%" height="15%" />
<br><br>
This is a package for R that provides functionality to plot beautiful survival curves (with numbers at risk table).
Based upon the ggplot2 package, it provides large flexibility, yet a straightforward and easy syntax. 

An example: 

![](example.png)<!-- -->


The package is now available [on CRAN](https://cran.r-project.org/package=survsup)! Easiest way to install is just to use _install.packages_: 
```r
install.packages("survsup")
```


If you want the very latest development version of the package, use _devtools_: 

```r
install.packages("devtools") # Can be omitted if already installed
devtools::install_github("dlindholm/survsup", build_vignettes = TRUE)
```

For detailed instructions on how to use the package, please refer to the [Vignette](https://cran.r-project.org/web/packages/survsup/vignettes/survsup_intro.html).
```r
library(survsup)
vignette("survsup_intro")
```

Please submit any bugs under _issues_. Collaborative efforts for further development of this package are of course very welcome! 

Enjoy! 
