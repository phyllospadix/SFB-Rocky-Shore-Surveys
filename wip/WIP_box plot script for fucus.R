setwd("C:/Users/913868311/Box/Rstats_WD")
#read in an excel file from the sheet called "data"
library(readxl)
reu_fucus_ptchauncey_weights <- read_excel("C:/Users/913868311/Box/EOS Center Director/Rockweed SF Bay Project/reu_fucus_ptchauncey_weights.xlsx", sheet = "data")

fucus_pcw <- reu_fucus_ptchauncey_weights
# Boxplots of fucus by position 
# observations (points) are overlayed and jittered
library(ggplot2)
qplot(position, fucus, data=fucus_pcw, geom=c("boxplot", "jitter"), 
      fill=position, main="Fucus Density by Position",
      xlab="", ylab="# thalli per 0.25 m2")

# Boxplots of fucus by zone 
# observations (points) are overlayed and jittered
library(ggplot2)
qplot(zone, fucus, data=fucus_pcw, geom=c("boxplot", "jitter"), 
      fill=zone, main="Fucus Density by Position",
      xlab="", ylab="# thalli per 0.25 m2")


# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)


# Plot
fucus_pcw %>%
  ggplot( aes(x=position, y=fucus, fill=position)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=1.0, alpha=0.5) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Distribution of Fucus on Concrete Blocks") +
  xlab("")


# grouped boxplot
bp <-ggplot(fucus_pcw, aes(x=position, y=fucus, fill=zone)) + 
  geom_boxplot(alpha=1.0)
bp + scale_fill_brewer(palette="Dark2")+
ylab("# Fucus thalli per 0.25 m2")
# adjust lightness and chromoa: bp + scale_fill_hue(l=40, c=35)
# adjust colors manually: bp + scale_fill_manual(values=c("blue", "red"))

  

ggplot(mpg, aes(x=class, y=hwy, fill=class)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")

# One box per zone
ggplot(fucus_pcw, aes(x=position, y=fucus, fill=zone)) + 
  geom_boxplot() +
  facet_wrap(~zone)

# scatter plot w. groups
sp <-ggplot(fucus_pcw, aes(x=angle, y=fucus)) + 
  geom_point(size=2) +
  ylab("# Fucus thalli per 0.25 m2")+
  geom_point(aes(color = position)) +
  scale_fill_brewer(palette="Dark2")+
   geom_point(aes(color = position)) + geom_smooth(method = lm)+  # Add regression line
scale_fill_brewer(palette="Dark2")
sp



# Initiate a ggplot
b <- ggplot(df, aes(x = wt, y = mpg))

# Basic scatter plot
b + geom_point()


# Multiple Linear Regression Example 
fit <- lm(fucus ~ zone + position, data=fucus_pcw)
summary(fit) # show results
anova(fit) # anova table

# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
anova(fit) # anova table 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

warnings()

install.packages("extrafont")

library(extrafont)
font_import()
y



# Multiple Linear Regression Example 
fit <- lm(sqrt_fucus ~ angle, data=fucus_pcw)
summary(fit) # show results

# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
anova(fit) # anova table 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)







