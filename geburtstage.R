library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)

geburtstage <- read.csv("geburtstage.csv")
geburtstage <- geburtstage %>% gather(monat, wert, jaenner:dezember)

gebquant <- geburtstage$wert
decile <- quantile(gebquant, prob = seq(0, 1, length = 11), na.rm=TRUE, type = 5)

geburtstage$gebkat[geburtstage$wert< 100000] <- "a"
geburtstage$gebkat[geburtstage$wert< 21038] <- "b"
geburtstage$gebkat[geburtstage$wert< 20303] <- "c"
geburtstage$gebkat[geburtstage$wert< 20013] <- "d"
geburtstage$gebkat[geburtstage$wert< 19794] <- "e"
geburtstage$gebkat[geburtstage$wert< 19643] <- "f"
geburtstage$gebkat[geburtstage$wert< 19518] <- "g"
geburtstage$gebkat[geburtstage$wert< 19384] <- "h"
geburtstage$gebkat[geburtstage$wert< 19173] <- "i"
geburtstage$gebkat[geburtstage$wert< 18857] <- "j"
geburtstage$gebkat[geburtstage$wert< 18482] <- "k"
geburtstage$gebkat[geburtstage$wert< 4887] <- "l"

# Convert the column to a factor
geburtstage$gebkat <- factor(geburtstage$gebkat)

color <- c("a" = "#ffffcc","b" = "#a1dab4","c" = "#41b6c4", "d" = "#2c7fb8", "e" ="#253494", "f"="#000000")
['#fcfbfd','#efedf5','#dadaeb','#bcbddc','#9e9ac8','#807dba','#6a51a3','#54278f','#3f007d']

g <- ggplot(data = geburtstage, aes(x = date, y = monat)) +
  geom_tile(aes(fill = gebkat))+
  scale_y_discrete(limits= rev(c("jaenner","februar","maerz","april","mai","juni","juli","august", "september","oktober","november","dezember"))) +
  print(g)

#v2
g <- ggplot(data = geburtstage, aes(x = date, y = monat)) +
  geom_tile(aes(fill = wert))+
  scale_y_discrete(limits= rev(c("jaenner","februar","maerz","april","mai","juni","juli","august", "september","oktober","november","dezember")))+
  scale_fill_gradient(breaks=quantile_range, values=quantile_range. colour=color_palette, space="Lab")
print(g)


