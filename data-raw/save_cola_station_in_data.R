library(gmwmx)
cola=PBO_get_station("COLA", "dE")
plot(cola$t, cola$y, type="l")
save(cola, file = "data/cola.RData")
