############################################################
# Introduction to Data Analysis 2 
# Sources: "Data Analysis 2.pdf" and "Linear regression Extended presentation"

############################################################

## ---- 0) Install and load packages (as used in the slides) -----------------
# car        : Levene's test for homogeneity of variance (independent t-test precheck)
# coin       : Wilcoxon/Mann–Whitney (nonparametric independent samples)
# dunn.test  : Dunn’s post-hoc test after Kruskal–Wallis
# gmodels    : CrossTable (contingency tables + chi-square)
# foreign    : Import SPSS .sav files (lottery dataset)

install.packages("car")
install.packages("coin")
install.packages("dunn.test")
install.packages("gmodels")
install.packages("foreign")

library(car)
library(coin)
library(dunn.test)
library(gmodels)
library(foreign)

## ---- 1) Load data ----------------------------------------------------------
# Select the CSV with the IDI variables (e.g., IDI.csv)
idi <- read.csv(file.choose(), header = TRUE)

# Select the SPSS .sav with the lottery variables (e.g., lottery.sav)
lottery <- read.spss(file.choose(), to.data.frame = TRUE)

## ---- 2) Normality checks: Shapiro–Wilk ------------------------------------
# Slide code: run Shapiro on selected variables
shapiro.test(idi$Fixed2012)
# Output note: If p < .05 ⇒ NOT normal; if p ≥ .05 ⇒ normal. Base for choosing parametric vs nonparametric.  

shapiro.test(idi$Mobile2012)
# Output note: Interpret as above (decision threshold .05).  

## ---- 3) One-sample t-test --------------------------------------------------
# H0: mean(Mobile2011) = 95; H1: mean ≠ 95
t.test(idi$Mobile2011, mu = 95)
# Output note: If p < .05 ⇒ reject H0 (mean differs from 95); else do not reject H0.  

## ---- 4) Independent samples t-test (with Levene pretest) -------------------
# Subset: high vs medium HDI (between-subjects design in slides)
hmHDI <- subset(idi, (HDI_level == "high" | HDI_level == "medium"))

# Levene’s test for equal variances
leveneTest(hmHDI$Mobile2012, hmHDI$HDI_level, center = "mean")
# Output note: If p ≥ .05 ⇒ assume equal variances and use var.equal = TRUE in t.test; 
#              If p < .05 ⇒ use Welch t-test (var.equal = FALSE). Slides proceed with equal vars.  

# Independent two-sample t-test (as in slides)
t.test(hmHDI$Mobile2012 ~ hmHDI$HDI_level, var.equal = TRUE)
# Output note: If two-sided p < .05 ⇒ significant mean difference; else “no significant difference”.  #

# ---------- Embedded bullets: independent vs paired t-test (application) ----------
# Independent samples t-test — when/how (slides 15–19)
# - Design: two SEPARATE groups (between-subjects). Var check via Levene. 
# - Variables: numeric outcome (e.g., Mobile2012) + 2-level group (e.g., HDI_level).
# - Decide by p: p < .05 ⇒ groups differ; p ≥ .05 ⇒ no evidence of a difference.  #
# Paired samples t-test — when/how (slides 27–28)
# - Design: two measures on the SAME units (within-subjects; e.g., Mobile2011 vs Mobile2012).
# - Test on the mean of paired DIFFERENCES; Levene not applicable here.
# - Decide by p: p < .05 ⇒ mean changed; sign of mean(diff) gives direction.  

## ---- 5) One-way ANOVA (3 groups) ------------------------------------------
# Subset: high/medium/low HDI (between-subjects, k=3)
hmlHDI <- subset(idi, (HDI_level == "high" | HDI_level == "medium" | HDI_level == "low"))

# ANOVA omnibus test
fit <- aov(hmlHDI$Mobile2012 ~ hmlHDI$HDI_level)
summary(fit)
# Output note: If p < .05 ⇒ at least one mean differs → proceed to post-hoc; else no evidence of differences.  

# Post-hoc (no p-value adjustment)
pairwise.t.test(hmlHDI$Mobile2012, hmlHDI$HDI_level, p.adj = "none")
# Output note: Unadjusted pairwise p-values (liberal).  

# Post-hoc (Bonferroni adjustment)
pairwise.t.test(hmlHDI$Mobile2012, hmlHDI$HDI_level, p.adj = "bonf")
# Output note: Bonferroni is more conservative; p < .05 after adjustment ⇒ pair differs.  

# Tukey HSD post-hoc
TukeyHSD(fit)
# Output note: Check “p adj”; p adj < .05 ⇒ those two groups differ.


#################### Something more about post-hoc tests #######

# Bonferroni: per-test alpha = alpha_global / m (or p_adj = p * m); very simple, general, conservative

# Holm (Holm–Bonferroni): sort p's; compare p_(1) to alpha/m, p_(2) to alpha/(m-1), ... ; step-down,
#                         controls FWER, always >= power than plain Bonferroni

# Tukey HSD: post-hoc for one-way ANOVA, all pairwise mean comparisons; uses studentized range (q),
#            FWER control, usually more powerful than Bonferroni when ANOVA assumptions hold


################################################################

## ---- 6) Paired samples t-test ----------------------------------------------
# Same countries in 2011 vs 2012 (within-subjects)
t.test(idi$Mobile2011, idi$Mobile2012, paired = TRUE, alternative = "two.sided")
# Output note: If p < .05 ⇒ mean changed between years; mean(M2012 - M2011) sign shows direction.  

## ---- 7) Mann–Whitney (Wilcoxon rank-sum) for two independent samples -------
# Nonparametric alternative when normality/equal-variance assumptions fail
wilcox_test(hmHDI$Fixed2012 ~ hmHDI$HDI_level)

### Important note ###

str(hmHDI$HDI_level)
hmHDI$HDI_level  <- factor(hmHDI$HDI_level)

# Convert to factor with custom numeric order
#idi$HDI_level <- factor(idi$HDI_level, 
#                        levels = c("low", "medium", "high"), 
#                        ordered = TRUE)


wilcox_test(hmHDI$Fixed2012 ~ hmHDI$HDI_level)
# Output note: If p < .05 ⇒ distributions differ between groups.  

## ---- 8) Kruskal–Wallis (k independent samples) + post-hoc ------------------
kruskal.test(Fixed2012 ~ HDI_level, data = hmlHDI)
# Output note: If p < .05 ⇒ at least one group differs; proceed to nonparametric post-hoc.  

# Post-hoc approach I: pairwise Wilcoxon with Bonferroni adjustment
pairwise.wilcox.test(hmlHDI$Fixed2012, hmlHDI$HDI_level, p.adjust.method = "bonf")
# Output note: p < .05 in adjusted matrix ⇒ that pair differs; note: exact p may be unavailable with ties.  

# Post-hoc approach II: Dunn’s test (Bonferroni)
dunn.test(hmlHDI$Fixed2012, hmlHDI$HDI_level, method = "bonferroni", kw = TRUE)
# Output note: Check adjusted p-values; p < .05 ⇒ that pair differs.  

## ---- 9) Wilcoxon signed-rank (paired, nonparametric) -----------------------

wilcox.test(hmlHDI$Fixed2011, hmlHDI$Fixed2012, paired = TRUE)
# Output note: If p < .05 ⇒ paired medians differ (nonparametric analogue of paired t-test).  


#################### Something more about post-hoc tests #######

# Dunn test: nonparametric post-hoc for Kruskal–Wallis; uses global ranks and mean-rank differences
#            between groups; z-stat per pair + p-value; then adjust (Bonferroni/Holm/…)

# Pairwise Wilcoxon: run Wilcoxon rank-sum (Mann–Whitney) for each pair separately (pairwise data only),
#                    then adjust p-values (Holm/Bonferroni/FDR, etc.); similar goal, different ranking/variance basis

################################################################



## ---- 10) Contingency tables (CrossTabs) + chi-square -----------------------
# Different percentage displays as in slides (always request chisq = TRUE)
CrossTable(lottery$LotterySpending, lottery$LotteryPlaying,
           prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE, chisq = TRUE)
# Output note: If chi-square p ≥ .05 ⇒ variables independent; if p < .05 ⇒ dependent.  

CrossTable(lottery$LotterySpending, lottery$LotteryPlaying,
           prop.r = TRUE,  prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE, chisq = TRUE)

CrossTable(lottery$LotterySpending, lottery$LotteryPlaying,
           prop.r = FALSE, prop.c = TRUE,  prop.t = FALSE, prop.chisq = FALSE, chisq = TRUE)

CrossTable(lottery$LotterySpending, lottery$LotteryPlaying,
           prop.r = FALSE, prop.c = FALSE, prop.t = TRUE,  prop.chisq = FALSE, chisq = TRUE)

CrossTable(lottery$LotterySpending, lottery$LotteryPlaying,
           prop.r = TRUE,  prop.c = TRUE,  prop.t = TRUE,  prop.chisq = FALSE, chisq = TRUE)

## ---- 11) Correlation -------------------------------------------------------
# Pearson r (slides use Mobile2011 vs Mobile2012)
cor(idi$Mobile2011, idi$Mobile2012, method = "pearson")
# Output note: r ∈ [-1,1]; sign = direction; |r| = strength.  

# Test r against 0
cor.test(idi$Mobile2011, idi$Mobile2012, method = "pearson")
# Output note: If p < .05 ⇒ r ≠ 0 (linear association present).  

## ---- 12) Scatter plots (visual check) --------------------------------------
plot(idi$Mobile2011, idi$Mobile2012)
plot(idi$Mobile2011, idi$Mobile2012,
     main = "Scatterplot",
     xlab = "Mobile-cellular subscriptions per 100 inhabitants (2011)",
     ylab = "Mobile-cellular subscriptions per 100 inhabitants (2012)",
     pch  = 19)
# Output note: Slides describe this relation as strong, direct, linear.  

## ---- 13) Simple linear regression -----------------------------------------
fit_s <- lm(idi$Mobile2012 ~ idi$Mobile2011, data = idi)
summary(fit_s)
# Output note:
# - Coefficient (slope) test: p < .05 ⇒ slope significant (X predicts Y).
# - Model F-test: p < .05 ⇒ model significant overall.
# - R-squared: proportion of variance in Y explained by X.  

## ---- 14) Multiple linear regression ---------------------------------------
fit_m <- lm(idi$Mobile2012 ~ idi$Fixed2011 + idi$Mobile2011)
summary(fit_m)
# Output note:
# - Check each slope’s p-value (individual effects) and the F-test (joint effect).
# - Interpret Adjusted R-squared as overall explained variance (slides discuss this concept).  

# (Slides also show an example equation Mobile2012 = -0.162*Fixed2011 + 1.037*Mobile2011 + 4.946
