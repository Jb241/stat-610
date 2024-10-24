source("llr_functions.R")
library(reshape2)
data(french_fries)
french_fries <- french_fries[complete.cases(french_fries),]
z <- seq(0, 15, length.out = 100)
fits <- llr(z = z, x = french_fries$potato, y = french_fries$buttery, omega = 2)
plot(z, fits)

for(i in 1:10){
  fits <- llr(z = z, x = french_fries$potato, y = french_fries$buttery, omega=i)
  plot(z, fits)
}

library(bench)
llr_mark <- mark(llr)
llr_mark