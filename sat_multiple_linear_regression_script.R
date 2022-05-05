# Shruti Alavala
# Final Project

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Read in data
sat_data_orig<-read.csv("2012_SAT_Results_Original.csv")
head(sat_data_orig)
nrow(sat_data_orig)

colnames(sat_data_orig) <- c("dnb", "name", "num.testers", "reading", "writing", "math")
head(sat_data_orig)

# DATA CLEANING

# remove rows with s as value

sat_data <- subset(sat_data_orig, 
                   (num.testers != "s" & reading != "s" & writing != "s" & math != "s"), 
                   select = c("dnb", "name", "num.testers", "reading", "writing", "math"))

nrow(sat_data)

# convert to numerical value column

sapply(sat_data, class)

i <- c("num.testers", "reading", "writing", "math")
sat_data[, i] <- apply(sat_data[, i], 2, function(x) as.numeric(x))

sapply(sat_data, class)

# check that avg score ranges from 200 to 800

summary(sat_data)

# save cleaned data to csv
write.csv(sat_data, "cleaned_sat_data.csv", row.names = FALSE)

# multiple linear regression analysis

# scatterplot and correlation

sat_data_numerical <- sat_data[, c("reading","writing","math")]

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="complete.obs"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * (1 + r) / 2)
}

panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}

panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                      cex = 1, col.smooth = "black", ...) {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  abline(stats::lm(y ~ x), col = col.smooth, ...)
}

pairs(sat_data_numerical,upper.panel=panel.cor, diag.panel=panel.hist, lower.panel=panel.lm)

# linear regression

attach(sat_data)

m <-lm(math~reading+writing)
m
summary(m)

confint(m, level =.95) 

#Assessing fit/Regression Diagnostics
#Residual plots - checking constant variance and linearity
plot(fitted(m),resid(m), axes=TRUE, frame.plot=TRUE,
     main = "Residual Plot of Predicted SAT Math Score (Multiple Linear Regression)",
     xlab = "fitted values", ylab="residuals")
abline(h=0, col = 'deeppink')

#Checking Normality of residuals
hist(resid(m),
     main = "Histogram of Residuals (Multiple Regression for SAT Math Score)",
     xlab = "Residual",
     breaks = seq(-60,40,10),
     xaxt = "n",
     col = "plum3")
axis(side = 1, seq(-60,40,10), labels = TRUE)