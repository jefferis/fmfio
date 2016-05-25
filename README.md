# fmfio
R package to support the **FlyMovieFormat** video originally developed by
Andrew Straw and colleagues, initially at Caltech. Presently this provides
functions to read and parse the intial header of a fmf file and then to
read individual images frames and timestamp information. fmf files are
frequently used as intermediates for analysis of fly behaviour.

## Quick Start
```r
# install
if (!require("devtools")) install.packages("devtools") 
devtools::install_github("jefferis/elmr")

# use
library(elmr)
```
