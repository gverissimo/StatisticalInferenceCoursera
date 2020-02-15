## ******************************** 
library(R.utils)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

# if only the dataset is known.
ggplot(diamonds)

# if only X-axis is known. The Y-axis can be specified in respective geoms.
ggplot(diamonds, aes(x=carat))

# if both X and Y axes are fixed for all layers.
ggplot(diamonds, aes(x=carat, y=price))

# Each category of the 'cut' variable will now have a distinct  color, once a geom is added.
ggplot(diamonds, aes(x=carat, color=cut))

# Same as above but specifying the aesthetics inside the geoms.
ggplot(diamonds) + 
        geom_point(aes(x=carat, y=price, color=cut)) + 
        geom_smooth(aes(x=carat, y=price, color=cut)) 
