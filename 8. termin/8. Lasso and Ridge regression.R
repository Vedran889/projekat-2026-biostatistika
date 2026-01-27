############################################################
#
# 8. Lasso and Ridge regression
# 
############################################################


############################################################
# Installing packages (Linear regression)
############################################################

# Install the packages needed to conduct linear regression
install.packages("foreign")
install.packages("ggplot2")

# Activate the packages needed to conduct linear regression
library(foreign)
library(ggplot2)

# Import the specific file from the folder where the file is located
WEF <- read.spss(file.choose(), to.data.frame = TRUE)

# Get to know your data
str(WEF)



############################################################
# Linear regression in R
############################################################

# Create the LRM
regmodel1 <- lm(Macroeconomic_environment_Score_2013 ~ 
                  Market_size_Score_2013 + Goods_market_efficiency_Score_2013, 
                data = WEF)

# List the results of the created LRM
summary(regmodel1)



############################################################
# Ridge regression in R
############################################################

# Install the needed package 
install.packages("readr")
install.packages("glmnet")

# Activate the needed packages
library(readr) 
library(glmnet)


# Create a subset with the independent variables
x1 <- as.matrix(data.frame(WEF$Market_size_Score_2013,
                           WEF$Goods_market_efficiency_Score_2013))

# Create and conduct a ridge regression model
ridge1 <- glmnet(x1, WEF$Macroeconomic_environment_Score_2013, 
                 family = "gaussian", alpha = 0)

# Print some results
summary(ridge1) 
print(ridge1)
# Are these results useful?

# Provide insights on a model for a particular Lambda
coef(ridge1, s = 0.01)
# Is this the best lambda? NO

# Change of coefficients as Lambda changes
plot(ridge1, xvar = "lambda") 
grid() 
legend(6, 0.75, legend = c("Goods_market_efficiency_Score_2013",
                           "Market_size_Score_2013"), 
       col = c("red", "black"), lty = 1:1, cex = 0.6, 
       title = "Coefficient", text.font = 4)

# Set seed of random generator
set.seed(123)

# Conduct ridge regression with cross validation
cv.ridge1 <- cv.glmnet(x1, WEF$Macroeconomic_environment_Score_2013, 
                       family = "gaussian", type.measure = "mse", alpha = 0)

# Print the results
print(cv.ridge1)

# How MSE changes as lambda change?
# MSE change plot
plot(cv.ridge1) 
grid()

# Show and save best lambda
# Save best lambda as an object
bestlambda1 <- cv.ridge1$lambda.min 

# Print best lambda
bestlambda1 

# Create the final ridge regression model
ridge1best <- glmnet(x1, WEF$Macroeconomic_environment_Score_2013, 
                     family = "gaussian", alpha = 0, lambda = bestlambda1)

# Print model results
coef(ridge1best)



############################################################
# Lasso regression in R
############################################################

# Create and conduct a lasso regression model
lasso1 <- glmnet(x1, WEF$Macroeconomic_environment_Score_2013, 
                 family = "gaussian", alpha = 1)

# Print some results
summary(lasso1) 
print(lasso1)
# Are these results useful?

# Provide insights on a model for a particular Lambda
coef(lasso1, s = 0.01)
# Is this the best lambda? NO

# Change of coefficients as Lambda changes
plot(lasso1, xvar = "lambda") 
grid() 
legend(6, 0.75, legend = c("Goods_market_efficiency_Score_2013",
                           "Market_size_Score_2013"), 
       col = c("red", "black"), lty = 1:1, cex = 0.6, 
       title = "Coefficient", text.font = 4)

# Set seed of random generator
set.seed(123)

# Conduct lasso regression with cross validation
cv.lasso1 <- cv.glmnet(x1, WEF$Macroeconomic_environment_Score_2013, 
                       family = "gaussian", type.measure = "mse", alpha = 1)

# Print the results
print(cv.lasso1)

# How MSE changes as lambda change?
# MSE change plot
plot(cv.lasso1) 
grid()

# Show and save best lambda
# Save best lambda as an object
bestlambda1 <- cv.lasso1$lambda.min 

# Print best lambda
bestlambda1 

# Create the final ridge regression model
lasso1best <- glmnet(x1, WEF$Macroeconomic_environment_Score_2013, 
                     family = "gaussian", alpha = 1, lambda = bestlambda1)

# Print model results
coef(lasso1best)



############################################################
# Elastic net model in R
############################################################

# Install the needed package 
install.packages("caret") 

# Activate the needed packages
library(caret)

# Seet seed
set.seed(42)

# Define the cross-validation which will be conducted
cv_5 = trainControl(method = "cv", number = 5)

# Create the models
hit_elnet = train(
  Macroeconomic_environment_Score_2013 ~ Market_size_Score_2013 +
    Goods_market_efficiency_Score_2013, data = WEF,
  method = "glmnet",
  trControl = cv_5
)

# Print the results
hit_elnet


# Create the models and expand the search space
hit_elnet_int = train(
  Macroeconomic_environment_Score_2013 ~ Market_size_Score_2013 +
    Goods_market_efficiency_Score_2013, data = WEF,
  method = "glmnet",
  trControl = cv_5,
  tuneLength = 10
)

# Print the results
hit_elnet_int


# Find the best combination of alpha and lambda
get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

# Print the best combination model evaluation
get_best_result(hit_elnet_int)



############################################################
# PITANJA / VEŽBE (10)
# (Studenti rešavaju koristeći kod iznad)
############################################################


# ==========================================================
# PITANJE 1
# Pokreni linearni regresioni model regmodel1 i napiši procenjene koeficijente
# (Intercept, Market_size_Score_2013, Goods_market_efficiency_Score_2013).
#
#
#
#
# --- KOD ZA REŠENJE ---
coef(regmodel1)
# # REŠENJE (iz slajda): 
# # (Intercept) = 1.60421
# # Market_size_Score_2013 = 0.13472
# # Goods_market_efficiency_Score_2013 = 0.60651



# ==========================================================
# PITANJE 2  (dopuna / komentar)
# Dopuni sledeću rečenicu u komentaru:
# "Konstanta (Intercept) u LR modelu je statistički značajna na nivou 0.05: ____"
# (Upiši: DA ili NE, na osnovu p-vrednosti iz summary(regmodel1)).
#
#
#
#
# --- KOD ZA REŠENJE ---
summary(regmodel1)$coefficients["(Intercept)", "Pr(>|t|)"]
# # REŠENJE: DA (p = 0.00692 < 0.05)



# ==========================================================
# PITANJE 3
# Iz summary(regmodel1) izvuci:
# (a) Multiple R-squared i (b) p-vrednost F-testa celog modela.
#
#
#
#
# --- KOD ZA REŠENJE ---
summary(regmodel1)$r.squared
summary(regmodel1)$fstatistic
pf(summary(regmodel1)$fstatistic[1],
   summary(regmodel1)$fstatistic[2],
   summary(regmodel1)$fstatistic[3],
   lower.tail = FALSE)
# # REŠENJE (iz slajda): R^2 = 0.1943; p(F) = 5.18e-07



# ==========================================================
# PITANJE 4  (dopuna koda)
# Dopuni sledeću liniju tako da iz cv.ridge1 izvučeš najbolju lambda vrednost:
# bestlambda_ridge <- cv.ridge1$__________
#
#
#
#
# --- KOD ZA REŠENJE ---
bestlambda_ridge <- cv.ridge1$lambda.min
bestlambda_ridge




# ==========================================================
# PITANJE 5
# Koristeći najbolju ridge lambdu (lambda.min), izračunaj koeficijente ridge modela
# i uporedi ih sa OLS koeficijentima (Pitanje 1).
#
#
#
#
# --- KOD ZA REŠENJE ---
coef(ridge1best)
# # REŠENJE (iz slajda, ridge konačni model):
# # (Intercept) = 
# # Market_size_Score_2013 = 
# # Goods_market_efficiency_Score_2013 = 



# ==========================================================
# PITANJE 6  (dopuna / komentar)
# Dopuni sledeće:
# "U glmnet(), LASSO regresiju dobijamo kada je alpha = ____."
#
#
#
#
# --- KOD ZA REŠENJE ---
# alpha_lasso <- 1
# alpha_lasso
# # REŠENJE: alpha = 1



# ==========================================================
# PITANJE 7
# Iz cv.lasso1 izvuci najbolju lambda vrednost (lambda.min).
#
#
#
#
# --- KOD ZA REŠENJE ---
bestlambda_lasso <- cv.lasso1$lambda.min
bestlambda_lasso
# # REŠENJE (iz slajda): best lambda (lasso) = 0.00228



# ==========================================================
# PITANJE 8
# Na osnovu najbolje lambda vrednosti za lasso (lambda.min), izvuci koeficijente
# finalnog lasso modela i proveri da li je neki koeficijent postao tačno 0.
#
#
#
#
# --- KOD ZA REŠENJE ---
coef(lasso1best)
# # REŠENJE (iz slajda, lasso konačni model):
# # (Intercept) = 1.6231402
# # Market_size_Score_2013 = 0.1333062
# # Goods_market_efficiency_Score_2013 = 0.6033447
# # Nijedan koeficijent nije tačno 0 (Nonzero = 2 prediktora).



# ==========================================================
# PITANJE 9
# Elastic net: Iz modela hit_elnet (prvi train bez proširenja prostora),
# napiši koje (alpha, lambda) vrednosti su izabrane kao najbolje.
#
#
#
#
# --- KOD ZA REŠENJE ---
hit_elnet$bestTune
# # REŠENJE (iz slajda): alpha = 0.1; lambda = 0.07598729



# ==========================================================
# PITANJE 10
# Elastic net (prošireni prostor): pokreni get_best_result(hit_elnet_int) i napiši:
# (a) najbolju kombinaciju (alpha, lambda) i (b) RMSE.
#
#
#
#
# --- KOD ZA REŠENJE ---
get_best_result(hit_elnet_int)
# # REŠENJE (iz slajda):
# # alpha = 0.1
# # lambda = 0.06163564
# # RMSE = 0.8397613