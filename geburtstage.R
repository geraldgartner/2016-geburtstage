library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(ggthemes)
source("style.R")

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

color <- c("a" = "#49006a",
           "b" = "#7a0177",
           "c" = "#ae017e", 
           "d" = "#dd3497", 
           "e" = "#f768a1", 
           "f"= "#fa9fb5",
           "g"= "#fcc5c0",
           "h"= "#fde0dd",
           "i"= "#fff7f3",
           "j"= "#fff7f3",
           "k"= "#fff7f3",
           "NA"= "#000000"
           
           )
['#','#','#','#','#',
  '#','#','#','#']



g <- ggplot(data = geburtstage, aes(x = date, y = monat)) +
  geom_tile(aes(fill = gebkat), na.rm = FALSE)+
  scale_y_discrete(limits= rev(c("jaenner","februar","maerz","april","mai","juni","juli","august", "september","oktober","november","dezember")),
                   labels=rev(c("Jänner",
                            "Februar", 
                            "März", 
                            "April", 
                            "Mai", 
                            "Juni",
                            "Juli",
                            "August",
                            "September",
                            "Oktober",
                            "November",
                            "Dezember")))+
  scale_fill_manual(values = color)+
  theme_dstd()
  print(g)

#v2
  g2 <- ggplot(data = geburtstage, aes(x = date, y = monat)) +
    geom_tile(aes(fill = wert), color ="#969696", na.rm = FALSE)+
    scale_y_discrete(limits= rev(c("jaenner","februar","maerz","april","mai","juni","juli","august", "september","oktober","november","dezember")),
                     labels=rev(c("Jänner",
                                  "Februar", 
                                  "März", 
                                  "April", 
                                  "Mai", 
                                  "Juni",
                                  "Juli",
                                  "August",
                                  "September",
                                  "Oktober",
                                  "November",
                                  "Dezember")))+
    scale_fill_gradient(limits=c(16965, 21040), low="#fff7f3", high="#49006a", space="Lab")+
    theme_dstd()
    
    
  print(g2)
ggsave("heatmap.pdf", width=10, height=5, units = "in")  
  
# 
# + scale_colour_gradient(limits=c(16965, 21040), low="white", high="red", space="Lab")


