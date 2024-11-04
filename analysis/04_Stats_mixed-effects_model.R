# Linear mixed models for each visual features
# Produces:
#     - GLMM and LMM result in Table S8 in Supplementary Materials


library(lme4)
library(multcomp)
library(readxl)

#### Set directory

df <- read_excel("FINAL_ALL FEATURES_883.xlsx")
head(df)

#### LMM for aesthetic value ####

# Build model using a family function that results in normally distributed residuals
Elomodel <-lmer(aesval~habitat+(1|site)-1, data=df)

# Test the residuals by plotting histograms and qqnorms
qqnorm(residuals(Elomodel))
hist(residuals(Elomodel))

# Gives you summary statistics from the model
summary(Elomodel)

# Run a null model to test statistical significance – everything the same except no fixed term
nullelomodel<-lmer(aesval~(1|site)-1, data=df)

# Test model vs null using anova, and Tukey test for different groups
anova(Elomodel,nullelomodel)
summary(glht(Elomodel,mcp(habitat="Tukey")))

#### GLMM for Simpson diversity ####

# Build model using a family function that results in normally distributed residuals
simpsonmodel1 <- glmer(simpson ~ habitat + (1|site)-1, 
                       data=df, family=Gamma(link="log"))

# Test the residuals by plotting histograms and qqnorm
qqnorm(residuals(simpsonmodel1))
hist(residuals(simpsonmodel1))

# Gives you summary statistics from the model
summary(simpsonmodel1)

# Run a null model to test statistical significance - everything the same except no fixed term
nullsimpson <- glmer(simpson ~ 1 + (1|site)-1, 
                     data=df, family=Gamma(link="log"))

# Test model vs null using anova, and Tukey test for different groups
anova(simpsonmodel1, nullsimpson)
summary(glht(simpsonmodel1, mcp(habitat="Tukey")))

#### GLMM for number of colour categories present ####
colourmodel <- glmer(colour_count ~ habitat + (1|site)-1, 
                     data=df, family=poisson)

# Test the residuals by plotting histograms and qqnorm
qqnorm(residuals(colourmodel))
hist(residuals(colourmodel))

# Gives you summary statistics from the model
summary(colourmodel)    

# Run a null model to test statistical significance - everything the same except no fixed term
colournull <- glmer(colour_count ~ 1 + (1|site)-1, 
                    data=df, family=poisson)

# Test model vs null using anova, and Tukey test for different groups
anova(colourmodel, colournull)
summary(glht(colourmodel, mcp(habitat="Tukey")))

#### GLMM for percent of image in blue colour category ####

# Build model using a model
bluemodel <- glmer(blue ~ habitat + (1|site)-1, 
                   data=df, family=Gamma(link="log"))

# Test the residuals by plotting histograms and qqnorm
qqnorm(residuals(bluemodel))
hist(residuals(bluemodel))

# Gives you summary statistics from the model
summary(bluemodel)    

# Run a null model to test statistical significance - everything the same except no fixed term
nullblue <- glmer(blue ~ 1 + (1|site)-1, 
                  data=df, family=Gamma(link="log"))

# Test model vs null using anova, and Tukey test for different groups
anova(bluemodel, nullblue)
summary(glht(bluemodel, mcp(habitat="Tukey")))

#### LMM for live hard coral cover ####

# Build model using a family function that results in normally distributed residuals
lc_LMM <-lmer(live_coral~habitat+(1|site)-1, data=df)

# Test the residuals by plotting histograms and qqnorms
qqnorm(residuals(lc_LMM))
hist(residuals(lc_LMM))

# Gives you summary statistics from the model
summary(lc_LMM)

# Run a null model to test statistical significance – everything the same except no fixed term
nullLCmodel<-lmer(live_coral~(1|site)-1, data=df)

# Test model vs null using anova, and Tukey test for different groups
anova(lc_LMM,nullLCmodel)
summary(glht(lc_LMM,mcp(habitat="Tukey")))

#### LMM for percent of image in shadow ####

# Build model using a family function that results in normally distributed residuals
lmm_colour <-lmer(black~habitat+(1|site)-1, data=df)

# Test the residuals by plotting histograms and qqnorms
qqnorm(residuals(lmm_colour))
hist(residuals(lmm_colour))

# Gives you summary statistics from the model][]
summary(lmm_colour)

# Run a null model to test statistical significance – everything the same except no fixed term
nulllmm <-lmer(black~(1|site)-1, data=df)

# Test model vs null using anova, and Tukey test for different groups
anova(lmm_colour,nulllmm)
summary(glht(lmm_colour,mcp(habitat="Tukey")))

#### GLMM for number of coral morphologies present ####
# Build model using a model
morphmodel <- glmer(morph ~ habitat + (1|site)-1, 
                    data=df, family=poisson)

# Test the residuals by plotting histograms and qqnorm
qqnorm(residuals(morphmodel))
hist(residuals(morphmodel))

# Gives you summary statistics from the model
summary(morphmodel)    

# Run a null model to test statistical significance - everything the same except no fixed term
nullmorph <- glmer(morph ~ 1 + (1|site)-1, 
                   data=df, family=poisson)

# Test model vs null using anova, and Tukey test for different groups
anova(morphmodel, nullmorph)
summary(glht(morphmodel, mcp(habitat="Tukey")))
