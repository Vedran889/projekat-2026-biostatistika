###############################################################################
# DIMENSION REDUCTION — PCA & FA 
# Packages:
#   foreign  – read SPSS (.sav) files (slide “PCA in R: load Food.sav”) 
#   REdaS    – Bartlett’s test & KMO pretests (factorability checks)
#   car      – quick scatterplot matrices for screening
#   psych    – FA/PCA helper (principal(), VSS.scree, scores)
###############################################################################

# ---- Install (if needed) & load ------------------------------------------------
pkgs <- c("foreign", "REdaS", "car", "psych")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(foreign)  # .sav import              # slides list: foreign, REdaS, car, psych 
library(REdaS)    # bart_spher(), KMO()      # pretests per slides 
library(car)      # scatterplotMatrix()      # quick EDA 
library(psych)    # principal(), VSS.scree   # FA/PCA helpers 

# NOTE (outdated bits): foreign::read.spss() is kept to mirror slides; 
# haven::read_sav() is a modern alternative if foreign is unavailable.

# ---- Load data exactly as on slides -------------------------------------------
# Choose the .sav (e.g., Transport.sav, Food.sav). Removes "Country" if present.
dataset   <- read.spss(file.choose(), to.data.frame = TRUE) # umjesto kovariacione matrice radim korelacionu kad su mi podaci skalirani
data.use  <- subset(dataset, select = -c(Country))            
matrixdata <- data.matrix(data.use)                           

# ---- PRETESTS: Bartlett’s test & KMO ------------------------------------------
# Bartlett’s test: H0 = correlation matrix is identity (variables orthogonal). (izmedju kolona)
# Decision: if p < .05 → correlations exist → factor/PCA plausible; if large p,
# reconsider FA/PCA 
bartlett_out <- bart_spher(matrixdata)   
print(bartlett_out)
# Output note: look at 'p.value' – small p means matrix ≠ identity ⇒ proceed.

# KMO measure: ≥ .8 great, ~.7 acceptable, ~.6 mediocre, < .5 unacceptable. (Keiser-Meyer-Olkin) koliko je matrica pogodna
# (slides thresholds) veci stepen parcijalne korelacije -- manja mogucnost za zdruzenje (sazimanje)
kmo_out <- KMO(matrixdata)  # slide shows KMOS(); correct is KMO() koeficijent pokazuje da li je ta varijabla pogodna u odnosu na overrall score
kmo_out

print(kmo_out$MSA)
# Output note: check MSA overall and by variable; low MSA flags variables to drop.




# ---- QUICK VISUAL SCREENING  -------------------------------------------------
scatterplotMatrix(data.use[1:3])         # first 3 variables matrix plot 
plot(data.use$V1, data.use$V2)           # 2‑var scatter; adjust names if needed

# Неке битне напомене:

# 1) линеарна комбинација -- нелинеарна комбинација

# 2) Зашто се користи линеарна комбинација код АГК?
# nelinearna kombinacija se moze koristiti ali je rezultat neobjasnjiv (pojave blackbox-a nekog sistema, npr. neuronske mreze)

# 3) Да ли имамо неко ограничење? Зашто баш то?
# suma kvadrata lin. koeficienata = 1, zato sto ce varijansa da podivlja (da raste). zato ovo ogranicenje

# 4) Коваријациона и корелациона матрица
# korelaciona -- standardizovana kovarijaciona matrica

# 5) Нормализација, стандардизација, аутлајери, недостајуће вредности?

# moraju se autlajeri i NaNs odstraniti, jer utice na racunanje varijanse, onda kovarijacione i korelacione matrice, pa onda se siti kao virus

# 6) Хммм, а шта ћемо са претпоставком нормалности?
# nema, jer je algebarska operacija

###############################################################################
# PART A — PRINCIPAL COMPONENT ANALYSIS (PCA) 
###############################################################################

# Base PCA (scaled) — matches slide example with 'scale. = TRUE' 
food.pca <- prcomp(data.use,
                   center = TRUE, #одузимам средину колоне свакој ћелиији (центрирам), ово је дефовлт ТРУЕ
                   scale. = TRUE) #делим са сопственим ст. дев.

# Шта треба урадити да би се рачунало на коваријационој матрици?


summary(food.pca)                         # variance explained etc. 
# Output note: use 'Proportion of Variance'/'Cumulative' to judge retained PCs
# (plus rules below: eigen>1 and scree elbow). 

# Eigenvalues (λ = sdev^2) — per slides 
food.pca$sdev
(food.pca$sdev)^2
# Var(Y1)=lambda_1 broj jedinicnih varijansi koje ja objasnjavam
# veci varijabilitet -- vece odstupanje. Jurimo sve koji su >=1 po Keiseru (heuristicka metoda)

################# Ako zelim da se stvari lepo i pregledno vide, mora truda malkice

# Eigen-vrednosti (varijanse komponenti)
eigenvalues <- food.pca$sdev^2

# Udeo objašnjene varijanse po komponenti
prop_var <- eigenvalues / sum(eigenvalues)

# Kumulativni udeo
cum_prop <- cumsum(prop_var)

# Lep pregled u tabeli
pca_var_table <- data.frame(
  PC       = 1:length(eigenvalues),
  Eigenval = round(eigenvalues, 3),
  PropVar  = round(prop_var, 3),
  CumProp  = round(cum_prop, 3)
)

print(pca_var_table)

#################################################################################


# Component (loading) matrix access — per slides 
food.pca$rotation           # all components
food.pca$rotation[, 1]      # first component
food.pca$rotation[, 1:2]    # first two components

# Scree plot (visual elbow) — per slides 
screeplot(food.pca, main = "Scree plot", type = "lines")

#beware of Kaiser's rule (lambda>1). Pogledati liniju 90

# Output note: look for the “bend” where eigenvalues level off; keep PCs before elbow.

# Scatter of PC1 vs PC2 — per slides 
plot(food.pca$x[,1], food.pca$x[,2],
     xlab = "First component", ylab = "Second component",
     main = "Scatter of first two principal components")

# siri opseg jer je veca varijansa. Ovo je metod bottom-up, top-bottom radi detalja. Da bih mogao top-bottom, moram faktorsku analizu izvrsiti

# Output note: clusters/patterns here guide interpretation of component scores.

######################################## igranje oko stabilnosti za broj GK (dodatni dio)

####### Split-sample tehnika za proveru stabilnosti podprostora prvih k komponenti

# set.seed(123)  # radi reproduktivnosti
# 
# n    <- nrow(data.use)
# idx  <- sample.int(n)
# half <- floor(n / 2)
# 
# data1 <- data.use[idx[1:half], ]
# data2 <- data.use[idx[(half + 1):n], ]
# 
# pca1 <- prcomp(data1, scale. = TRUE)
# pca2 <- prcomp(data2, scale. = TRUE)
# 
# #ranije smo odlucili da na osnovu scree-plot-a uzmemo 2
# k <- 2  # broj komponenti čiju stabilnost proveravaš
# 
# L1 <- pca1$rotation[, 1:k, drop = FALSE]  # loadings 1. polovina
# L2 <- pca2$rotation[, 1:k, drop = FALSE]  # loadings 2. polovina
# 
# # Singularne vrednosti T(L1) %*% L2 = cosinusi prin. uglova između podprostora
# sv <- svd(t(L1) %*% L2)$d
# 
# cat("Cosinusi principalnih uglova između podprostora prvih", k, "PC:\n")
# print(round(sv, 4))

# Dakle, ako su svi kosinusi > 0,95 to znaci da podprostor jeste vrlo stabilan (slicne ose razdvajanja u obe polovine podataka)


###############################################################################
# PART B — FACTOR ANALYSIS  -- zelim Y, na osnovu vektora X, da nije linearna kombinacija nego skup odredjenih promenljivih koji nuzno mora biti manji od ukupnog broja promenljivih. Oni ce biti faktori
# faktor je nesto sto objedinjuje slicne promenljive (znaci, ne sve) (sto ukljucuje vektor X, strucno faktor loadings)
# sto veci loadings, to mogu gurati takve promenljive koje imaju takve faktore, zato sto homogenizuje za te promenljive
#cilj: odredjeni skup varijabli na osnovu koga vrsim mapiranje promenljivih na osnovu faktora

# ideja: X-mu=matrica faktorskih opterecenja (loadings) * matrica zajednickih faktora+ eps (slucajna greska, specif. dio varijanse)

# matrica faktorskih opterecenja * matrica zajednickih faktora = komunaliti

# efa, siefe
# efa -- polazi od podataka i koristim kriterijum odlucivanja
# siefe -- unaprijed znam sta su faktori
###############################################################################

# Load psych (slides) and fit “basic” FA/PCA via principal() 
food.factor <- principal(data.use) # default:  faktor
food.factor
# h2=suma svih kvadrata loadinga
# u2= uniqueness, odnosno ono sto nije objasnjeno
# com -- complexity
# h2+u2=1 (h-squared+u-squared)
# Cov(F)=Fi gdje Fi=jedinicna matrica --> nezavisni faktori


# RMSR and test for adequate number of components/factors 
# > 0.05 -> retain Ho that number is adequate

# Output note: this run retains 1 component by default on slides; 
# use rules below (eigen>1 & scree) to decide real number.

# Retain several factors, no rotation (nfactors=5, rotation="none") — slides 
food.factor <- principal(data.use, nfactors = 5, rotate = "none")
food.factor
# Output note: inspect 'PC1..PC5' loadings, h2 (communality), u2 (uniqueness).

# Scree based on loadings (slides show VSS.scree on loadings) 
VSS.scree(food.factor$loadings)

# Fit with Varimax rotation and 4 factors — slides sequence 
food.factor <- principal(data.use, nfactors = 4, rotate = "varimax")
food.factor
food.factor$loadings                                # 
print(food.factor$loadings, cutoff = 0.3)           # show loadings ≥ .30 
food.factor$values                                  # eigenvalues of factors 
# Output note: after rotation, interpret factors by high loadings (≥.30/.40) 
# with simple structure (few cross‑loadings).

# Factor scores (retain & display) — slides 
food.factor <- principal(data.use, nfactors = 4, scores = TRUE, rotate = "varimax")
head(food.factor$scores)
# Output note: scores are per‑row positions on each factor; use in regressions/plots.

#### Bitne razlike

food.factor <- principal(data.use, nfactors = 3, rotate = "varimax")
food.factor

food.factor$loadings
# food.factor$loadings

print(food.factor$loadings, cutoff = 0)
# no hiding :)))))))

food.factor$values
# food.factor$values
# eigen values for non rotated values of correlation matrix (classical PCA)

food.factor$scores
# food.factor$scores
# values of each component/factor for each entity in dataset


# Additional: correlation matrix & rounded print — slides 
cor(data.use)
round(cor(data.use), 3)

food.factor <- principal(data.use, nfactors = 3, rotate = "varimax")
food.factor
food.factor$loadings

#####################################################################
# Uporedjivanje rezultata razlicitih izbora broja faktora (EFA)
#####################################################################


compare_pca_models <- function(data, nf_list = c(3, 4),
                               rotations = c("none", "varimax", "oblimin"),
                               cutoff_cross = 0.30) {
  # Korelaciona matrica – ista za sve modele
  R <- cor(data, use = "pairwise.complete.obs")
  
  models  <- list()
  summary <- list()
  
  for (m in nf_list) {
    for (rot in rotations) {
      
      # 1) PCA sa zadatim brojem faktora i rotacijom
      mod <- principal(data, nfactors = m, rotate = rot)
      name <- paste0("f", m, "_", rot)
      models[[name]] <- mod
      
      # 2) Matrica opterecenja (loadings) kao obicna matrica
      L <- as.matrix(mod$loadings)
      
      # 3) Komunaliteti (h2) – suma kvadrata loadinga po varijabli
      h2 <- rowSums(L^2)
      
      # 4) Rekonstruisana R (na osnovu loadinga), pa reziduali
      R_hat <- L %*% t(L)
      diag(R_hat) <- 1  # na dijagonali korelacije = 1
      resid <- R - R_hat
      
      # RMSR = koren proseka kvadrata off-diagonal reziduala
      r_off  <- resid[lower.tri(resid)]
      RMSR   <- sqrt(mean(r_off^2, na.rm = TRUE))
      
      # 5) Kumulativni udeo ukupne varijanse objasnjen prve m komponente
      #    (isto sto gledas u Vaccounted, ali eksplicitno)
      eigvals       <- mod$values        # eigen-vrednosti (za sve komponente)
      cum_prop_var  <- sum(eigvals[1:m]) / ncol(data)
      
      # 6) Prosecna kompleksnost stavki (manje = jednostavnija struktura)
      mean_complexity <- mean(mod$complexity)
      
      # 7) Broj stavki sa cross-loadingom (>= 2 opterecenja >= cutoff_cross)
      absL <- abs(L)
      big  <- absL >= cutoff_cross
      n_cross_items <- sum(rowSums(big) >= 2)
      
      # 8) Cuvamo jedan red u tabeli
      summary[[length(summary) + 1]] <- data.frame(
        nfactors          = m,
        rotation          = rot,
        cum_prop_var      = round(cum_prop_var, 3),   # ukupna objasnjena varijansa
        RMSR              = round(RMSR, 3),           # manji = bolji fit
        mean_complexity   = round(mean_complexity, 2),# manji = jednostavnija struktura
        n_crossload_items = n_cross_items,            # manje = bolje (simple structure) crossload --> npr. ne moze varijabla da bude u 2 faktora
        
        # rotacione matrice, dopustaju razlicita hvatanja varijansi
        stringsAsFactors  = FALSE
      )
    }
  }
  
  summary_tab <- do.call(rbind, summary)
  rownames(summary_tab) <- NULL
  
  list(models = models, summary = summary_tab)
}

# ---- Pokretanje poređenja -----------------------------------------------------

cmp <- compare_pca_models(data.use)

# Sažetak svih modela (ovo gledas prvo!)
cmp$summary







##### Играње са ротацијама , показни пример разлика

# library(psych)
# 
# # 1) Nerotirano rešenje (2 faktora / komponente)
# fa_none    <- principal(data.use, nfactors = 2, rotate = "none")
# fa_varimax <- principal(data.use, nfactors = 2, rotate = "varimax")
# fa_oblimin <- principal(data.use, nfactors = 2, rotate = "oblimin")
# 
# plot_loadings <- function(loadings, main = "") {
#   L <- as.matrix(loadings[, 1:2])
#   plot(L,
#        xlim = c(-1, 1), ylim = c(-1, 1),
#        xlab = "F1", ylab = "F2",
#        main = main)
#   abline(h = 0, v = 0, lty = 3)
#   # krug jedinice radi lepšeg efekta
#   symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, lty = 3)
#   text(L, labels = rownames(L), pos = 3)
# }
# 
# par(mfrow = c(1, 3))
# plot_loadings(fa_none$loadings,    "Nerotirano rešenje")
# plot_loadings(fa_varimax$loadings, "Varimax (ortogonalna rotacija)")
# plot_loadings(fa_oblimin$loadings, "Oblimin (oblique rotacija)")
# par(mfrow = c(1, 1))



###############################################################################
# DECISION RULES REMINDER 
# - Bartlett p < .05 ⇒ correlations present ⇒ factor/PCA reasonable. 
# - KMO: ≥.8 great; ≥.7 acceptable; ~.6 mediocre; <.5 unacceptable ⇒ reconsider FA.
# - Number of components/factors: Eigenvalue>1 (Kaiser) + Scree elbow; keep where 
#   the curve bends/levels off. 
###############################################################################
