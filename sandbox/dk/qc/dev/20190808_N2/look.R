library(oceanglider)
options(oceEOS="gsw") # A *lot* of code is hard-wired for this, so we don't let user choose.
file <- "../../sea021m51_unedited.rda"
if (!exists("g"))
    load(file)
y <- subset(g, yoNumber==97)
worstPressure <- which.max(y[["pressure"]])
print(y[["payload1"]][worstPressure+seq(-3, 3), ])

