library(tidyverse)



#delete walking fast and cycling
for (i in seq_along(gpsl)){
   gpsl[[i]] <- gpsl[[i]] %>%
     filter(gpsl[[i]][13] != "Walking fast")
}

for (i in seq_along(gpsl)){
  gpsl[[i]] <- gpsl[[i]] %>%
    filter(gpsl[[i]][13] != "Cycling")
}

for (i in seq_along(gpsl)){
  gpsl[[i]] <- gpsl[[i]] %>%
    filter(gpsl[[i]][13] != "Cycling-leisure")
}

for (i in seq_along(gpsl)){
  gpsl[[i]] <- gpsl[[i]] %>%
    filter(gpsl[[i]][13] != "Cycling-urban")
}



#delete all NA rows of GPS data in gpsl list (row 101, 102, 108, 109, 110)
gpsl[[101]] <- gpsl[[101]][-(133:144),]
gpsl[[101]] <- gpsl[[101]][-(113:115),]

gpsl[[102]] <- gpsl[[102]][-(which(is.na(gpsl[[102]]$Longitude))),]

gpsl[[108]] <- gpsl[[108]][-(which(is.na(gpsl[[108]]$Longitude))),]

gpsl[[109]] <- gpsl[[109]][-(which(is.na(gpsl[[109]]$Longitude))),]

gpsl[[110]] <- gpsl[[110]][-(which(is.na(gpsl[[110]]$Longitude))),]



