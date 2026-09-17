# -------- Odbacivanje svih postojecih prikacenih paketa -------------
invisible(
  lapply(names(sessionInfo()$otherPkgs), function(package) {
    detach(paste0("package:", package), character.only = T, unload = T, force = T)
  })
)
options(prompt = "R> ")

setwd("./data")

# -------- Instaliranje i pozivanje paketa ------------
packages <- c(
  "glmnet", "readr",
  "coin", "car", "readxl", "ggplot2",
  "cluster",
  "REdaS", "psych", "foreign",
  "survival", "survminer",
  "caret", "pROC"
)
for (package in packages) {
  if (!requireNamespace(package, quietly = T)) {
    install.packages(package)
  }
  if (!(paste0("package:", package) %in% search())) {
    library(package, character.only = T)
  }
}
if (!requireNamespace("biclust", quietly = T)) {
  if (!requireNamespace("remotes", quietly = T)) install.packages("remotes")
  remotes::install_version("biclust", upgrade = "never", version = "2.0.3", INSTALL_opts = "--no-lock")
}
if (!(paste0("package:biclust") %in% search())) {
  library(biclust)
}


# ------ 1) RIDGE regresija --------
arcadis <- read_excel(file.choose())
print(names(arcadis))
cat("\n")
str(arcadis)
cat("\n")

## Da li ima NA vrijednosti? (0 - Nema)
print(arcadis[!(complete.cases(arcadis)), ])
cat("\n")

## Da li su opservacije jedinstvene?
print(nrow(arcadis) == length(unique(arcadis$City)))
cat("\n")

prediktori <- c("education", "crime", "health", "affordability", "Energy", "Greenspace")
X <- as.matrix(arcadis[, prediktori])
y <- arcadis$Tourism

set.seed(123)
cv.ridge <- cv.glmnet(X, y,
  alpha = 0,
  family = "gaussian",
  type.measure = "mse",
  nfolds = 10,
  standardize = T
)

min.lambda <- cv.ridge$lambda.min
lambda_1se <- cv.ridge$lambda.1se
cat(sprintf("Minimalno lambda: %.4f.\n", min.lambda))
cat(sprintf("Lambda 1se iznosi: %.4f.\n", lambda_1se))

coef.min <- coef(cv.ridge, s = "lambda.min")

nenule_koeficijenti <- rownames(coef.min)[as.numeric(coef.min) != 0]
zastupljene_varijable <- setdiff(nenule_koeficijenti, "(Intercept)")
cat(sprintf("Broj promijenljivih u modelu je %d, a to su %s.\n", length(zastupljene_varijable), paste(zastupljene_varijable, collapse = ", ")), "\n")

## -------------- Predvidjanje -------------
pred_Tourism <- predict(cv.ridge, newx = X, s = "lambda.min")
df.rezultati <- data.frame(
  City = arcadis$City,
  Tourism = arcadis$Tourism,
  pred_Tourism = as.vector(pred_Tourism),
  abs_err = abs(arcadis$Tourism - as.vector(pred_Tourism))
)

## --------- Top 5 gradova sa najvecom greskom ----------------
top5.gradovi <- head(df.rezultati[order(-df.rezultati$abs_err), ], 5)
print("Top 5 gradova sa najvecom greskom:")
print(top5.gradovi)
cat("\n")

## --------- Top 3 grada po zadatim kriterijumima ----------------
medijana.crime <- median(arcadis$crime)
posmatrani.skup <- subset(arcadis, Continent %in% c(1, 3) & crime > medijana.crime)

X.skup <- as.matrix(posmatrani.skup[, prediktori])
y.skup <- posmatrani.skup$Tourism

set.seed(123)
cv.ridge.podskup <- cv.glmnet(X.skup, y.skup,
  alpha = 0,
  family = "gaussian",
  type.measure = "mse",
  standardize = T,
  nfolds = 10
)

pred_podskup <- predict(cv.ridge.podskup, newx = X.skup, s = "lambda.min")
posmatrani.skup$pred_Tourism <- as.vector(pred_podskup)
posmatrani.skup$abs_err <- abs(posmatrani.skup$Tourism - posmatrani.skup$pred_Tourism)

top3.kontinenti <- data.frame()
for (kontinent in c(1, 3)) {
  podskup.kontinent <- posmatrani.skup[posmatrani.skup$Continent == kontinent, ]
  podskup.kontinent.top3 <- head(podskup.kontinent[order(-podskup.kontinent$abs_err), c("Continent", "City", "Tourism", "pred_Tourism", "abs_err")], 3)
  top3.kontinenti <- rbind(top3.kontinenti, podskup.kontinent.top3)
}

top3.kontinenti$Continent <- factor(top3.kontinenti$Continent, labels = c("Europe", "Asia"), ordered = T, levels = c(1, 3))
print("Top 3 grada sa najvecom greskom po kontinentima")
print(as.data.frame(top3.kontinenti))


# ------ 2) Statisticki testovi --------
## --------- a) Da li postoji razlika u vrijednostima varijable Easeofdoingbusiness ako posmatramo gradove u Evropi i Aziji? -----------
cities.europe.asia <- subset(arcadis, (Continent == 1 | Continent == 3))
cities.europe.asia$Continent <- factor(cities.europe.asia$Continent,
  levels = c(1, 3),
  labels = c("Europe", "Asia"),
  ordered = T
)

print(ggplot(data = cities.europe.asia, mapping = aes(x = Continent, y = Easeofdoingbusiness)) +
  geom_boxplot() +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )) # Nema autlajera za evropske i azijske gradove

## Shapiro-Wilk test za ispitivanje normalnosti
print(
  shapiro.test(
    cities.europe.asia$Easeofdoingbusiness[
      cities.europe.asia$Continent == "Europe"
    ]
  ) # p > 0.05 -> normalna raspodijela
)
print(
  shapiro.test(
    cities.europe.asia$Easeofdoingbusiness[
      cities.europe.asia$Continent == "Asia"
    ]
  ) # p < 0.05 -> nije normalna raspodijela
)

## Posto posmatrana varijabla za azijske gradove nije ispunjen kriterijum normalnosti, koristi se Mann-Whitney U test
print(wilcox_test(Easeofdoingbusiness ~ Continent, data = cities.europe.asia)) # p < 0.05 -> Oba uzorka nisu iz iste populacije

## ------------- b) Da li postoji razlika u vrijednostima varijabli crime, health, Energy ako posmatramo gradove iz Evrope i Azije? --------------------------
## cija varijabla Greenspace se nalazi iznad medijane (medijana je izracunata na citavom skupu)
medijana <- median(arcadis$Greenspace)
high.Greenspace <- cities.europe.asia[cities.europe.asia$Greenspace > medijana, ]

print(ggplot(data = high.Greenspace, mapping = aes(x = Continent, y = crime)) +
  geom_boxplot() +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )) # Postoje dva autlajera u Evropi i jedan u Aziji
print(ggplot(data = high.Greenspace, mapping = aes(x = Continent, y = health)) +
  geom_boxplot() +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )) # Postoje jedan autlajer u Evropi
print(ggplot(data = high.Greenspace, mapping = aes(x = Continent, y = Energy)) +
  geom_boxplot() +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )) # Postoje jedan autlajer u Aziji

## Shapiro-Wilk test za ispitivanje normalnosti
kolone <- c("crime", "health", "Energy")
kontinenti <- unique(high.Greenspace$Continent)

safe_shapiro <- function(x) {
  if (length(x) >= 3) {
    return(shapiro.test(x)$p.value)
  } else {
    return(NA)
  }
}

for (kolona in kolone) {
  cat("\n=====================================")
  cat("\nShapiro-Wilk test za varijablu", kolona)
  cat("\n=====================================\n")

  for (kontinent in kontinenti) {
    kontinent.podaci <- high.Greenspace[[kolona]][high.Greenspace$Continent == kontinent]
    p.value <- safe_shapiro(kontinent.podaci)

    if (!is.na(p.value)) {
      status <- if (p.value > 0.05) "normalna raspodijela" else "nije normalna raspodijela"
      cat(sprintf("%-15s -> p = %g (%s)\n", kontinent, p.value, status))
    } else {
      cat(sprintf("%-15s -> n < 3 (nedovoljno podataka za Shapiro test)\n", kontinent))
    }
  }
}

## Posto u sve tri grupe (crime, health, Energy) u azijskim gradovima nije ispunjen kriterijum normalnosti, koristi se Mann-Whitney U test
print(wilcox_test(crime ~ Continent, data = high.Greenspace)) # p > 0.05 -> Oba uzorka su iz iste populacije
print(wilcox_test(health ~ Continent, data = high.Greenspace)) # p > 0.05 -> Oba uzorka nisu iz iste populacije
print(wilcox_test(Energy ~ Continent, data = high.Greenspace)) # p < 0.05 -> Oba uzorka nisu iz iste populacije

## --------------- c) Da li postoji razlika u vrijednostima varijable Tourism ako posmatramo gradove u Evropi i Sjevernoj Americi? ------------------------------------
first.quantile <- as.numeric(quantile(arcadis$affordability, 0.25))
cities.europe.north_america <- subset(arcadis, (Continent == 1 | Continent == 2) & affordability <= first.quantile)
cities.europe.north_america$Continent <- factor(cities.europe.north_america$Continent,
  levels = c(1, 2),
  labels = c("Europe", "North America"),
  ordered = T
)
str(cities.europe.north_america)

print(ggplot(data = cities.europe.north_america, mapping = aes(x = Continent, y = Tourism)) +
  geom_boxplot() +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )) # Postoje dva autlajera u Sjevernoj Americi

## Crime pragovi
pragovi <- c(0.8, 0.9, 0.95)

for (prag in pragovi) {
  do_praga <- cities.europe.north_america[cities.europe.north_america$crime < prag, ]
  do_praga$Continent <- droplevels(do_praga$Continent)

  n.europe <- sum(do_praga$Continent == "Europe")
  n.north_america <- sum(do_praga$Continent == "North America")

  ## Shapiro-Wilk test za ispitivanje normalnosti
  shapiro.europe <- safe_shapiro(do_praga$Tourism[do_praga$Continent == "Europe"])
  shapiro.north_america <- safe_shapiro(do_praga$Tourism[do_praga$Continent == "North America"])

  if (n.europe > 0 && n.north_america > 0) {
    if (!is.na(shapiro.europe) && !is.na(shapiro.north_america) && shapiro.europe > 0.05 && shapiro.north_america > 0.05) {
      ## Levene test za homogenost varijansi
      levene.result <- leveneTest(Tourism ~ Continent, center = "mean", data = do_praga)
      levene.p_value <- levene.result$`Pr(>F)`[1]

      if (levene.p_value > 0.05) {
        test.result <- t.test(Tourism ~ Continent, data = do_praga, var.equal = T)
        test.name <- "Student t-test"
      } else {
        test.result <- t.test(Tourism ~ Continent, data = do_praga, var.equal = F)
        test.name <- "Welch t-test"
      }
      test.p_value <- test.result$p.value
    } else {
      test.result <- wilcox_test(Tourism ~ Continent, data = do_praga)
      test.name <- "Mann-Whitney U test"
      test.p_value <- pvalue(test.result)
    }
  } else {
    test.name <- "Nije moguce sprovesti (jedna grupa nema opservacija)"
    test.p_value <- NA
  }

  cat("\n=========== PRAG crime <", prag, "==============")
  cat("\nBroj gradova -> Europe:", n.europe, "| North America:", n.north_america)
  cat("\nShapiro p-value -> Europe:", shapiro.europe, "| North America:", shapiro.north_america)
  cat("\nKorisceni test:", test.name)
  cat("\nTest p-value:", test.p_value, "\n")
}


# ------ 3) Da li postoji razlika u vrijednostima varijable Connectivity u zavisnosti od kontinenta na kome se grad nalazi? --------
arcadis.use <- as.data.frame(arcadis)
arcadis.use$Continent <- factor(arcadis$Continent,
  levels = c(1, 2, 3),
  labels = c("Europe", "North America", "Asia"),
  ordered = T
)

print(ggplot(data = arcadis.use, mapping = aes(x = Continent, y = Connectivity)) +
  geom_boxplot() +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )) # Postoje dva autlajera u Evropi

# Shapiro-Wilk test za ispitivanje normalnosti
print(shapiro.test(
  arcadis.use$Connectivity[
    arcadis.use$Continent == "Europe"
  ]
)) # p > 0.05 -> normalna raspodijela
print(shapiro.test(
  arcadis.use$Connectivity[
    arcadis.use$Continent == "North America"
  ]
)) # p > 0.05 -> normalna raspodijela
print(shapiro.test(
  arcadis.use$Connectivity[
    arcadis.use$Continent == "Asia"
  ]
)) # p < 0.05 -> nije normalna raspodijela

# Posto posmatrana varijabla za azijske gradove nije ispunjen kriterijum normalnosti, koristi se Kruskal-Wallis test
print(kruskal.test(Connectivity ~ Continent, data = arcadis.use)) # p < 0.05 -> postoji znacajna razlika u medijani izmedju najmanje 2 uzorka


high.Greenspace.arcadis <- arcadis.use[arcadis.use$Greenspace > medijana, ]

print(ggplot(data = high.Greenspace.arcadis, mapping = aes(x = Continent, y = Tourism)) +
  geom_boxplot() +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )) # Postoje jedan autlajer u Sjevernoj Americi

# Shapiro-Wilk test za ispitivanje normalnosti
cat("\n======================================")
cat("\nShapiro-Wilk test za varijablu Tourism")
cat("\n======================================\n")

kontinenti <- unique(high.Greenspace.arcadis$Continent)
for (kontinent in kontinenti) {
  kontinent.podaci <- high.Greenspace.arcadis$Tourism[high.Greenspace.arcadis$Continent == kontinent]
  p.value <- safe_shapiro(kontinent.podaci)

  if (!is.na(p.value)) {
    status <- if (p.value > 0.05) "normalna raspodijela" else "nije normalna raspodijela"
    cat(sprintf("%-15s -> p = %g (%s)\n", kontinent, p.value, status))
  } else {
    cat(sprintf("%-15s -> n < 3 (nedovoljno podataka za Shapiro test)\n", kontinent))
  }
}
cat("\n")

# Posto svi uzorci ispunjavaju kriterijum normalnosti, koristice se ANOVA test
# Preduslov: Levenov test - kriterijum o homogenosti varijansi. Ako test padne, tada se koristi Welch-test u sklopu ANOVA
print(leveneTest(Tourism ~ Continent, data = high.Greenspace.arcadis, center = "mean")) # p > 0.05 -> ispunjen kriterijum o homogenosti varijansi
cat("\n")

fit <- aov(Tourism ~ Continent, data = high.Greenspace.arcadis)
print(summary(fit)) # p > 0.05 -> nema znacajnih razlika izmedju posmatranih uzoraka


# ------------ 4) Klasterizovati gradove prema sljedecim varijablama: health, affordability, Energy, Easeofdoingbusiness i Connectivity ----------------------------------
# Koristiti PAM metodu, broj klastera: 3

varijable <- c("health", "affordability", "Energy", "Easeofdoingbusiness", "Connectivity")
numericki_podaci <- arcadis.use[, varijable]
rownames(numericki_podaci) <- arcadis.use$City
pamcluster <- pam(numericki_podaci, k = 3, metric = "euclidean")

## ----------- Broj gradova po klasterima ----------------------
cat("\nBroj gradova po klasterima:\n")
print(table(pamcluster$clustering))

## ----------- Medoidi ----------------------
medoidi <- rownames(pamcluster$medoids)
cat("\nMedoidi:", paste(medoidi, collapse = ", "))

## ----------- Prosijecni siluetni indeks ----------------------
cat("\nAverage silhouette width:", pamcluster$silinfo$avg.width)


## =============== Profilisanje klastera ==============
cat("\n\n=============== Profilisanje klastera ==============\n")
profili <- aggregate(numericki_podaci, by = list(Cluster = pamcluster$clustering), FUN = mean)

min.health_cluster <- which.min(profili$health)
cat(sprintf("Medoid grad: %s | vrednost: %g\n", medoidi[min.health_cluster], profili$health[min.health_cluster]))

max.Energy_cluster <- which.max(profili$Energy)
cat(sprintf("Medoid grad: %s | vrednost: %g\n", medoidi[max.Energy_cluster], profili$Energy[max.Energy_cluster]))

min.Connectivity_cluster <- which.min(profili$Connectivity)
cat(sprintf("Medoid grad: %s | vrednost: %g\n\n", medoidi[min.Connectivity_cluster], profili$Connectivity[min.Connectivity_cluster]))


### ------------ Raspored medoida i najudaljenijih gradova -----------------
medijana.Tourism <- median(arcadis.use$Tourism)
medijana.crime <- median(arcadis.use$crime)
filtrirani_podaci <- subset(arcadis.use, Tourism > medijana.Tourism & crime < medijana.crime)
filtrirani_podaci.use <- filtrirani_podaci[, varijable]

rownames(filtrirani_podaci.use) <- filtrirani_podaci$City
pamcluster.filter <- pam(filtrirani_podaci.use, k = 3, metric = "euclidean")

distance.matrix <- as.matrix(dist(filtrirani_podaci.use, method = "euclidean"))


for (i in 1:3) {
  id.clanovi <- which(pamcluster.filter$clustering == i)
  id.medoid <- pamcluster.filter$id.med[i]

  rastojanja <- distance.matrix[id.clanovi, id.medoid]

  max.distance <- which.max(rastojanja)
  najudaljeniji_grad <- names(rastojanja)[max.distance]
  max.distance.value <- rastojanja[max.distance]

  medoid.grad <- filtrirani_podaci$City[id.medoid]
  broj_gradova <- length(id.clanovi)

  cat(sprintf(
    "Klaster %d | Medoid: %-12s | Najudaljeniji grad: %-12s | Distanca: %g | Broj gradova: %d\n",
    i, medoid.grad, najudaljeniji_grad, max.distance.value, broj_gradova
  ))
}


### ------------ Raspodijela gradova po klasterima u odnosu na pripadajuce kontinente -----------------
medijana.health <- median(arcadis.use$health)
high.health <- subset(arcadis.use, health > medijana.health)
high.health.use <- high.health[, varijable]

rownames(high.health.use) <- high.health$City
pamcluster.high.health.use <- pam(high.health.use, k = 3, metric = "euclidean")

high.health$Cluster <- pamcluster.high.health.use$clustering


kontinenti.klasteri <- table(high.health$Continent, high.health$Cluster)
kontinenti.target <- c("Europe", "North America", "Asia")
cat("\n=========== Raspodijela gradova po klasterima u odnosu na pripadajuce kontinente =============\n")
print(kontinenti.klasteri[rownames(kontinenti.klasteri) %in% kontinenti.target, ])


### -------------- Pregled dominantnih klastera po kontinentima ---------------
cat("\n=========== Pregled dominantnih klastera po kontinentima =============\n")
for (kontinent in kontinenti.target) {
  kontinent.podskup <- subset(high.health, high.health$Continent == kontinent)
  kontinent.broj_gradova <- nrow(kontinent.podskup)

  if (kontinent.broj_gradova > 0) {
    klasteri <- table(kontinent.podskup$Cluster)

    dominantni.klaster <- as.numeric(names(which.max(klasteri)))

    dominantni.broj <- max(klasteri)
    udio <- dominantni.broj / kontinent.broj_gradova

    cat(sprintf(
      "Kontinent: %-15s | Dominantni klaster: %d | Udio: %g (%.2f%%) | Ukupno: %d\n",
      kontinent, dominantni.klaster, udio, udio * 100, kontinent.broj_gradova
    ))
  }
}
cat("\n\n")


## ------------ Metrike: Euclidean i Manhattan - koja je bolja? -------------------
metrike <- c("Euclidean", "Manhattan")
rezultati <- data.frame(
  avg_sill_score = numeric(2),
  Objective_score = numeric(2),
  row.names = metrike
)

modeli <- list()
for (metrika in metrike) {
  if (metrika == "Euclidean") {
    model <- pam(numericki_podaci, k = 3, metric = "euclidean")
  } else {
    matrica <- dist(numericki_podaci, method = "manhattan")
    model <- pam(matrica, k = 3, diss = T)
  }

  modeli[[metrika]] <- model

  rezultati[metrika, "avg_sill_score"] <- model$silinfo$avg.width
  rezultati[metrika, "Objective_score"] <- model$objective["swap"]
  # build - vrijednost funkcije cilja nakon inicijalne faze gradjenja klastera
  # swap - konacna vrijednost funkcije cilja nakon sto je algoritam zavrsio optimizaciju medoida
}

print(rezultati)

bolja_metrika <- rownames(rezultati)[which.max(rezultati$avg_sill_score)]
bolji_model <- modeli[[bolja_metrika]]
imena_medoida <- arcadis.use$City[bolji_model$id.med]

cat("\n\n")
cat(sprintf("Bolja metrika je: %s.\n", bolja_metrika))
cat(sprintf("Zato sto: ima vecu vrijednost prosijecnog siluetnog indeksa (avg_sill_score = %g vs %g).\n", max(rezultati$avg_sill_score), min(rezultati$avg_sill_score)))
cat(sprintf("Medoidi dobijeni pomocu bolje metrike su: %s.\n", paste(imena_medoida, collapse = ", ")))


# ------------ 5) Metodom faktorske analize smanjiti dimenzionalnost date matrice ----------------------------------
## --------------- Bartlett i KMO pretests -------------------
matrica.podataka <- as.matrix(subset(arcadis.use, select = -c(City, Continent)))
bartlett_out <- bart_spher(matrica.podataka)
print(bartlett_out) # p < 0.05 -> varijable nisu ortogonalne, tj. korelacije postoje

kmo_out <- KMO(matrica.podataka)
print(kmo_out) # overall MSA = 0.84 -> odlicna, za semplovanje

## --------------- Neki zahtijevi ----------------------
arcadis.factor <- principal(matrica.podataka, nfactors = ncol(matrica.podataka), rotate = "none")
# print(arcadis.factor)
cumulative.var <- arcadis.factor$Vaccounted["Cumulative Var", ]
n.factors_70 <- which(cumulative.var >= 0.70)[1]
cat(sprintf("\n\nPotrebno je izabrati %d faktora za objasnjavanje bar 70%% varijabiliteta podataka.\n\n", n.factors_70))

arcadis.factor.2 <- principal(matrica.podataka, nfactors = 4, rotate = "varimax")
# print(arcadis.factor.2)

loadings.RC1 <- arcadis.factor.2$loadings[, "RC1"]
max.RC1 <- names(which.max(abs(loadings.RC1)))
cat(sprintf("\n\nNa RC1 najvece apsolutno opterecenje je: %s (%.2f).\n", max.RC1, loadings.RC1[max.RC1]))

communality <- arcadis.factor.2$communality
min.communality <- names(which.min(communality))
cat(sprintf("Najmanji komunalitet: %s (%.2f).\n", min.communality, communality[min.communality]))

cat(sprintf("Komunalitet za health: %.2f (%.0f%%).\n", communality["health"], communality["health"] * 100))

## ----------- Faktor sa maksimalnim prosijecnim komunalitetom i top 2 promijenljive na tom faktoru ---------------
loadings.arcadis <- abs(unclass(arcadis.factor.2$loadings))

pripadnost <- max.col(loadings.arcadis)
faktori <- colnames(loadings.arcadis)
avg.communality <- sapply(1:length(faktori), function(i) {
  mean(communality[pripadnost == i])
})
names(avg.communality) <- faktori

max.faktor <- names(which.max(avg.communality))
max.avg.communality.value <- max(avg.communality)

top2_promenljive <- names(sort(loadings.arcadis[, max.faktor], decreasing = T)[1:2])

cat(sprintf("Faktor: %s (%.2f) | Top 2 promijenljive: %s, %s.\n\n", max.faktor, round(max.avg.communality.value, 2), top2_promenljive[1], top2_promenljive[2]))


## ------------- Gradovi sa najvecim skorom -------------------
city.scores <- principal(matrica.podataka, nfactors = 4, scores = T, rotate = "varimax")
skorovi <- city.scores$scores

for (f in colnames(skorovi)) {
  idx <- which.max(skorovi[, f])
  cat("Faktor: ", f, " | Grad: ", arcadis.use$City[idx], " | Skor: ", round(skorovi[idx, f], 2), "\n")
}
cat("\n")

## ------------ Evropski gradovi koji pripadaju gornjem kvartilu ----------------
evropa <- subset(arcadis.use, Continent == "Europe")
third.quantile <- as.numeric(quantile(evropa$Tourism, 0.75))
cat("Gornji kvartil: ", third.quantile, "\n")
top.evropa <- subset(evropa, Tourism >= third.quantile)

top.skorovi <- skorovi[rownames(top.evropa), ]
korelacije <- numeric(4)
names(korelacije) <- c("MR1", "MR2", "MR3", "MR4")

for (i in 1:4) {
  korelacije[i] <- cor(top.evropa$Tourism, top.skorovi[, i])
}
cat("Vrijednosti korelacija: ", round(korelacije, 2), "\n")
max.idx <- which.max((abs(korelacije)))
cat("Faktor sa najvecim koeficijentom korelacije: ", names(korelacije)[max.idx], " | Vrijednost: ", round(korelacije[max.idx], 2), "\n\n")


## --------------- Trazenje top communality varijable na osnovu 5 indikatora: health, affordability, Energy, Easeofdoingbusiness, Connectivity ------------------------
for (kontinent in kontinenti) {
  podskup.kontinent <- as.matrix(subset(arcadis.use, Continent == kontinent)[, varijable])

  n <- nrow(podskup.kontinent)
  bart <- bart_spher(podskup.kontinent)
  kmo <- KMO(podskup.kontinent)
  eiv <- eigen(cor(podskup.kontinent))$values
  n_factors <- sum(eiv > 1)
  fa_rezultat <- principal(podskup.kontinent, nfactors = n_factors, rotate = "varimax")
  komunalitet <- fa_rezultat$communality
  max.varijabla <- names(which.max(komunalitet))
  max.vrijednost <- komunalitet[max.varijabla]

  cat("\n=====================================\n")
  cat("Kontinent: ", kontinent)
  cat("\nn: ", n)
  cat("\nBartlett_p: ", round(bart$p.value, 5))
  cat("\nKMO MSA value: ", round(kmo$MSA, 5))
  cat("\nn_factors: ", n_factors)
  cat("\ntop_communality_variable: ", max.varijabla)
  cat("\nComm_value: ", round(max.vrijednost, 5))
}
cat("\n")

## -----------------  Podskup gradova oivicen gornjim kvartilom promijenljive Connectivity i donjim kvartilom promijenljive affordability --------------------------
fa.sve <- principal(matrica.podataka, nfactors = 4, rotate = "varimax", scores = T, method = "regression")
svi.podaci <- cbind(arcadis.use, fa.sve$scores)
q75.connectivity <- quantile(svi.podaci$Connectivity, 0.75)
q25.affordability <- quantile(svi.podaci$affordability, 0.25)

trazeni.podskup <- subset(svi.podaci, Connectivity >= q75.connectivity & affordability <= q25.affordability)
prosijeci <- numeric(4)
names(prosijeci) <- c("MR1", "MR3", "MR4", "MR2")

for (i in 1:4) {
  prosijeci[i] <- mean(trazeni.podskup[[faktori[i]]])
}
max.abs.idx <- which.max(abs(prosijeci))
odabrani_faktor <- names(prosijeci[max.abs.idx])
odabrani_faktor_kolona <- faktori[max.abs.idx]
prosijek <- prosijeci[max.abs.idx]

if (prosijek > 0) {
  rangirani <- trazeni.podskup[order(-trazeni.podskup[[odabrani_faktor_kolona]]), ]
  smjer <- "prosijek je pozitivan."
} else {
  rangirani <- trazeni.podskup[order(trazeni.podskup[[odabrani_faktor_kolona]]), ]
  smjer <- "prosijek je negativan."
}

top5 <- head(rangirani, 5)

cat("\nProsijecne vrijednosti svakog od faktora (MR1, MR3, MR4, MR2) su: ", paste(round(prosijeci, 2), collapse = ", "), ".\n\n")
cat("Od svih faktora bira se ", odabrani_faktor, " jer ima najvecu apsolutnu vrijednost (", round(prosijek, 2), ") i ", smjer, "\n\n")
cat("TABELA TOP 5 GRADOVA:\n")
gradovi.tabela <- data.frame(
  Rang = 1:5,
  Grad = top5$City,
  Kontinent = top5$Continent,
  Tourism = round(top5$Tourism, 2),
  Odabrani_faktor = odabrani_faktor,
  Score = round(top5[[odabrani_faktor_kolona]], 2)
)
print(gradovi.tabela)


# ------------ 6) Survival analysis ----------------------------------
pluca <- read_excel(file.choose())

pluca$event <- ifelse(pluca$status == 2, 1, 0)
pluca$sex <- factor(pluca$sex, levels = c(1, 2), labels = c("Male", "Female"))

## Da li ima NA vrijednosti? (0 - Nema)
print(pluca[!(complete.cases(pluca)), ], n = 100)
cat("\n\n")

na_counts <- colSums(is.na(pluca))
print(na_counts[na_counts > 0])

fit <- survfit(Surv(time, event) ~ 1, data = pluca)

cat(sprintf("\nBroj opservacija: %d.\n", summary(fit)$table["records"]))
cat(sprintf("Broj hazardnih dogadjaja: %d.\n", summary(fit)$table["events"]))
cat(sprintf("Medijalno vrijeme prezivljavanja: %f.\n", summary(fit)$table["median"]))

## ------------ Log-Rank test ----------------
log.rank_test <- survdiff(Surv(time, event) ~ sex, data = pluca)
chisq_value <- log.rank_test$chisq
df_value <- length(log.rank_test$n) - 1
p_log.rank <- 1 - pchisq(chisq_value, df = df_value)

cat(
  "\nChi-square =", round(chisq_value, 4),
  "\ndf =", df_value,
  "\np-value =", signif(p_log.rank, 5), "\n"
) # p > 0.05 -> zivotne krive izmedju polova su iste

## ------------- Cox PH model --------------
cox <- coxph(Surv(time, event) ~ sex, data = pluca)
print(cox) # HR < 1 -> smanjen rizik zena u odnosu na muskarce; pol nije statisticki znacajan kao prediktor (p > 0.05)

## ---------- Multivariant Cox PH model ---------------
cox_multiv <- coxph(Surv(time, event) ~ sex + age + ph.ecog, data = pluca)
print(cox_multiv)

## ------- Rezime rezultata prema grupama starosti AgeGroup ------------
tercili <- quantile(pluca$age, probs = c(0, 1 / 3, 2 / 3, 1), na.rm = T)
pluca$AgeGroup <- cut(pluca$age, breaks = tercili, labels = c("Low", "Medium", "High"), include.lowest = T)

log.rank_age <- survdiff(Surv(time, event) ~ AgeGroup, data = pluca)
p_value.age <- 1 - pchisq(log.rank_age$chisq, length(log.rank_age$n) - 1)
cat("\n\nLog-Rank p-value za AgeGroup:", p_value.age, ".\n\n")

fit_age <- survfit(Surv(time, event) ~ AgeGroup, data = pluca)
summary_age_365 <- summary(fit_age, times = 365)

## n, events, median, S_365
print(summary(fit_age)$table[, c("records", "events", "median")])
cat("\n\n")
print(summary_age_365$surv)
cat("\n\n")

## ---------- Rezime rezultata analize prezivljavanja izmedju najzilavije grupe i ostalih grupa -------------
medijane <- summary(fit_age)$table[, "median"]

best <- max(medijane)
best.name <- names(which.max(medijane))

ostale.medijane <- medijane[names(medijane) != best.name]

delta_days <- best - ostale.medijane
extra_days_per_month <- delta_days / (ostale.medijane / 30)

rezime <- data.frame(
  Najzilavija = gsub("AgeGroup=", "", best.name),
  Preostala_grupa = gsub("AgeGroup=", "", names(ostale.medijane)),
  Medijana_Najzilavija = round(best),
  Medijana_Preostala_grupa = round(ostale.medijane),
  Delta_days = round(delta_days),
  Extra_days_per_month = round(extra_days_per_month, 4)
)
print(rezime)

## ----------------- Pacijentkinje koji imaju ph.ecog <= 1 ----------------------
podskup <- subset(pluca, (sex == "Female") & (ph.ecog <= 1))
kvantili <- seq(from = 0.20, to = 0.80, by = 0.10)
cutoffs <- quantile(podskup$age, probs = kvantili, na.rm = T)

rezultati.cutoffs <- data.frame()
for (cutoff in cutoffs) {
  podskup$AgeGroup_temp <- ifelse(podskup$age < cutoff, "< cutoff", ">= cutoff")

  donji <- sum(podskup$AgeGroup_temp == "< cutoff")
  gornji <- sum(podskup$AgeGroup_temp == ">= cutoff")

  events_donji <- sum(podskup$event[podskup$AgeGroup_temp == "< cutoff"])
  events_gornji <- sum(podskup$event[podskup$AgeGroup_temp == ">= cutoff"])

  if (donji >= 5 && gornji >= 5 && events_donji >= 5 && events_gornji >= 5) {
    logrank.test <- survdiff(Surv(time, event) ~ AgeGroup_temp, data = podskup)
    chisq <- logrank.test$chisq
    p_value <- 1 - pchisq(chisq, length(logrank.test$n) - 1)

    rezultati.cutoffs <- rbind(rezultati.cutoffs, data.frame(
      cutoff = cutoff,
      chisq = chisq,
      p_value = p_value
    ))
  }
}

best.index <- which.max(rezultati.cutoffs$chisq)
best.chisq <- rezultati.cutoffs$chisq[best.index]
best.cutoff <- rezultati.cutoffs$cutoff[best.index]
best.pvalue <- rezultati.cutoffs$p_value[best.index]

cat(sprintf("\n\nVrijednost statistike za izabrani cutoff je %.4f.\np-value: %.4f.\nOptimalni cutoff: %g.\n\n", best.chisq, best.pvalue, best.cutoff))

podskup$AgeBin <- ifelse(podskup$age < best.cutoff, paste0("AgeBin < ", round(best.cutoff, 2)), paste0("AgeBin >= ", round(best.cutoff, 2)))

fit.optimal <- survfit(Surv(time, event) ~ AgeBin, data = podskup)
summary_365 <- summary(fit.optimal, times = 365)

tabela.optimal <- data.frame(
  AgeBin = names(fit.optimal$strata),
  n = summary(fit.optimal)$table[, "records"],
  events = summary(fit.optimal)$table[, "events"],
  meadian_survival = summary(fit.optimal)$table[, "median"],
  S_365 = summary_365$surv
)

print(tabela.optimal)

# ------------ 7) Biklasterovanje ----------------------------------
food <- read.spss(file.choose(), to.data.frame = T)
cat("\n")
print(names(food))
cat("\n")
str(food)
cat("\n")

## Da li su opservacije jedinstvene?
print(nrow(food) == length(unique(food$Country)))
cat("\n")

food.use <- food
food.use$Country <- trimws(food$Country)
str(food.use)
cat("\n")
rownames(food.use) <- food.use$Country
food.use <- subset(food.use, select = -c(Country))

## Da li ima NA vrijednosti? (0 - Nema)
print(food.use[!(complete.cases(food.use)), ])
cat("\n")

## Nule po kolonama
zero_counts <- sapply(food.use, function(x) {
  sum(x == 0, na.rm = T)
})
zero_prop <- zero_counts / nrow(food.use)
zero_report <- data.frame(
  variable = names(zero_counts),
  zeros = zero_counts,
  zero_pct = round(100 * zero_prop, 1)
)
zero_report <- zero_report[order(-zero_report$zero_pct), ]

cat("Tabela:\n")
print(zero_report, row.names = F)

## Skaliranje (nema potrebe jer su podaci skalirani)
# food.use <- as.data.frame(scale(food.use))

## Cuvanje kao matrice
food.use.matrix <- as.matrix(food.use)

## ------------ Biklasterovanje metodom Cheng-Church --------------
set.seed(123)

resBCCC <- biclust(food.use.matrix,
  method = BCCC(),
  delta = 1.5,
  alpha = 1,
  number = 25
)
summary(resBCCC)


X <- food.use.matrix
B <- resBCCC
bc_vals <- function(B, X, k, variable, require_in = TRUE) {
  if (!inherits(B, "Biclust")) {
    stop("B mora biti 'Biclust' objekat.")
  }

  if (is.null(colnames(X))) {
    stop("X mora imati imena kolona.")
  }

  if (!is.character(variable) || length(variable) != 1L) {
    stop("Mora biti zadana kolona.")
  }

  if (k < 1L || k > B@Number) {
    stop(sprintf("k mora biti u opsegu [1, %d]", B@Number))
  }

  j <- match(variable, colnames(X))
  if (is.na(j)) {
    stop(sprintf("Kolona '%s' ne postoji u X.", variable))
  }

  row_indices <- which(B@RowxNumber[, k, drop = T])
  if (!length(row_indices)) {
    stop(sprintf("Biklaster %d nema redova.", k))
  }

  if (require_in && !isTRUE(B@NumberxCol[k, j])) {
    stop(sprintf("Kolona '%s' ne pripada biklasteru %d.", variable, k))
  }

  X[row_indices, j, drop = T]
}

cat(sprintf("Srednja vrijednost V17 u 2. biklasteru: %g.", mean(bc_vals(B, X, 2, "V17"))), "\n")
cat(sprintf("Najveca vrijednost V13 u 4. biklasteru: %g.", max(bc_vals(B, X, 4, "V13"))), "\n")
cat(sprintf("Medijana V7 u 5. biklasteru: %g.", median(bc_vals(B, X, 5, "V7"))), "\n")
cat(sprintf("Minimalna vrijednost V2 u 3. biklasteru: %g.", min(bc_vals(B, X, 3, "V2"))), "\n")


## ---------- Bicluster membership ----------------
biclustmember(B, X, main = "Bicluster membership graph UNORDERED")

ord <- bicorder(B, cols = T, rev = T)
biclustmember(B, X, which = ord, mid = T, main = "Bicluster membership graph ORDERED")

## ------------ Varijabla koja se najvise pojavljuje ---------------------
variable_counts <- colSums(B@NumberxCol)
names(variable_counts) <- colnames(X)
total_counts.variable <- max(variable_counts)
total_counts.variable_name <- names(which.max(variable_counts))

cat(sprintf(
  "\nVarijabla koja se pojavljuje u najvise biklastera je %s i pojavljuje se %d puta.",
  total_counts.variable_name, total_counts.variable
), "\n")

## --------- Zajednicka pojavljivanja varijabli u razlicitim biklasterima --------------
varijabla.matrica <- B@NumberxCol
varijable.matrica <- t(varijabla.matrica) %*% varijabla.matrica

colnames(varijable.matrica) <- colnames(X)
rownames(varijable.matrica) <- colnames(X)

## Uklanjanje dijagonale
diag(varijable.matrica) <- 0

## Najveci broj zajednickog pojavljivanja
max.varijable.matrica <- max(varijable.matrica)

pair_indices <- which(varijable.matrica == max.varijable.matrica, arr.ind = T)

parovi <- unique(t(apply(pair_indices, 1, sort)))
parovi.imema <- apply(parovi, 1, function(index) {
  paste(colnames(X)[index[1]], "i", colnames(X)[index[2]])
})

cat(sprintf("\nMax broj zajednickog pojavljivanja dve varijable sa razlicitim biklasterima: %d.\n", max.varijable.matrica))
cat(sprintf("Varijable koje se zajedno pojavljuju najveci broj puta: %s.\n", paste(parovi.imema, collapse = ", ")))

## -------------- Afinitet biklastera -----------------
broj_klastera <- B@Number
afiniteti <- numeric(broj_klastera)

for (k in 1:broj_klastera) {
  row_idx <- which(B@RowxNumber[, k, drop = T])
  col_idx <- which(B@NumberxCol[k, , drop = T])

  srednji.biklaster <- mean(X[row_idx, col_idx])
  srednji.matrica <- mean(X[, col_idx])

  afiniteti[k] <- srednji.biklaster / srednji.matrica
}
najbolji_afinitet.biklaster <- which.max(afiniteti)
najbolji_afinitet <- max(afiniteti)

cat(sprintf("\nBiklaster sa najvecim afinitetom je klaster broj %d, a vrijednost afiniteta je %g.\n", najbolji_afinitet.biklaster, najbolji_afinitet))

## ------------- Biklaster sa najmanjim within_sd() --------------

validni_biklasteri <- c()
within_sds <- c()
n_row_vector <- c()
n_col_vector <- c()

for (k in 1:broj_klastera) {
  row_idx <- which(B@RowxNumber[, k, drop = T])
  col_idx <- which(B@NumberxCol[k, , drop = T])

  n_row_idx <- length(row_idx)
  n_col_idx <- length(col_idx)

  if (n_row_idx >= 6 & n_col_idx >= 5) {
    podmatrica <- X[row_idx, col_idx]
    sd.trenutni <- sd(podmatrica)

    validni_biklasteri <- c(validni_biklasteri, k)
    within_sds <- c(within_sds, sd.trenutni)
    n_row_vector <- c(n_row_vector, n_row_idx)
    n_col_vector <- c(n_col_vector, n_col_idx)
  }
}
najmanji.biklaster.idx <- which.min(within_sds)

najmanji.biklaster <- validni_biklasteri[najmanji.biklaster.idx]
najmanji.sd <- within_sds[najmanji.biklaster.idx]
najmanji.red <- n_row_vector[najmanji.biklaster.idx]
najmanji.kolona <- n_col_vector[najmanji.biklaster.idx]

cat(sprintf(
  "\nPrema zadatim kriterijumima, najbolji biklaster je %d. On ima %d redova i %d kolona. Vrijednost za njegov parametar within_sd iznosi %g.\n",
  najmanji.biklaster, najmanji.red, najmanji.kolona, najmanji.sd
))


# ------------ 8) Logisticka regresija ----------------------------------
arcadis$EmploymentCategory <- cut(arcadis$Employment,
  breaks = c(-Inf, 0.3, 0.6, Inf),
  labels = c("low", "medium", "high")
)
arcadis$EducationCategory <- cut(arcadis$education,
  breaks = c(-Inf, 0.4, 0.6, Inf),
  labels = c("low", "medium", "high")
)


cat("\nEmploymentCategory:\n")
print(table(arcadis$EmploymentCategory))
cat("\nEducationCategory:\n")
print(table(arcadis$EducationCategory))

arcadis$HighEmployment <- ifelse(arcadis$EmploymentCategory == "high", 1, 0)
cat("\nHighEmployment counts:\n")
print(table(arcadis$HighEmployment))

lr.model <- glm(HighEmployment ~
  crime + Drinkingwaterandsanitation +
  Airpollution + Energy + Greenspace +
  EducationCategory, data = arcadis, family = binomial)
cat("\n------------ Rezultati modela ------------\n")
print(summary(lr.model))

## --------- Koeficijent i logit ------------
coef.medium <- coef(lr.model)["EducationCategorymedium"]
odd.ratio <- exp(coef.medium)
cat("\nKoeficijent EducationCategorymedium:", coef.medium, "\nOdds Ratio: ", odd.ratio, "\n")

## ------------- Predikcije ------------
predikcije <- predict(lr.model, type = "response")
predikcije.citav_skup <- ifelse(predikcije > 0.5, 1, 0)

## ------------- Konfuziona matrica i metrike ----------
cm.full <- confusionMatrix(factor(predikcije.citav_skup),
  factor(arcadis$HighEmployment),
  positive = "1"
)

accuracy <- cm.full$overall["Accuracy"]
precision <- cm.full$byClass["Precision"]
recall <- cm.full$byClass["Sensitivity"]
f1_score <- cm.full$byClass["F1"]

cat(sprintf("\nAccuracy: %.4f.\nPrecision: %4f.\nRecall: %.4f.\nF1: %.4f.\n", accuracy, precision, recall, f1_score))

## ----------- 10 seed-ova ----------------
seedovi <- data.frame(
  Seed = integer(),
  Accuracy = numeric(),
  Precision = numeric(),
  Recall = numeric(),
  F1 = numeric()
)

for (seed in 123:132) {
  set.seed(seed)

  train.idx <- createDataPartition(arcadis$HighEmployment, p = 0.70, list = F)
  train.set <- arcadis[train.idx, ]
  test.set <- arcadis[-train.idx, ]

  fit.seeds <- glm(
    HighEmployment ~ crime + Drinkingwaterandsanitation +
      Airpollution + Energy + Greenspace +
      EducationCategory,
    family = binomial,
    data = train.set
  )

  test.seeds <- predict(fit.seeds, newdata = test.set, type = "response")
  pred.test <- ifelse(test.seeds > 0.5, 1, 0)

  cm <- confusionMatrix(factor(pred.test, levels = c(0, 1)),
    factor(test.set$HighEmployment, levels = c(0, 1)),
    positive = "1"
  )
  acc <- cm$overall["Accuracy"]
  prec <- cm$byClass["Precision"]
  rec <- cm$byClass["Sensitivity"]
  f1.score <- cm$byClass["F1"]

  seedovi <- rbind(seedovi, data.frame(
    Seed = seed,
    Accuracy = round(acc, 4),
    Precision = round(prec, 4),
    Recall = round(rec, 4),
    F1 = round(f1.score, 4)
  ))
}

cat("\n========== Tabela sa 10 seed-ova ============\n")
print(seedovi)
