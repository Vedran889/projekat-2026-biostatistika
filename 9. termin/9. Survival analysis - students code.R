################################################################################
#
# 9. SURVIVAL ANALYSIS (Kaplan–Meier, Log-rank, Cox PH) 
#
################################################################################

# ================================================================
# 0) Paketi 
# ================================================================

install.packages("survival", dependencies = TRUE)
library(survival)

library(survival)
library(survminer)
library(car)
data(package = "survival")



# ================================================================
# *** Takticko-pokazni primer:
# ================================================================

df5 <- data.frame(
  id    = 1:5,
  sex   = factor(c("M","M","Z","Z","M")),
  time  = c(2, 3, 4, 5, 6),
  event = c(1, 0, 1, 1, 0)   # 1=event, 0=censored
)

# 1) KM za celu grupu
fit_all <- survfit(Surv(time, event) ~ 1, data = df5)
fit_all
summary(fit_all)

# 2) Medijana (KM)
summary(fit_all)$table["median"]

# 3) “Ručni” KM iz n.risk i n.event (provera)
step_prob <- 1 - fit_all$n.event / fit_all$n.risk
S_manual  <- cumprod(step_prob)
cbind(time = fit_all$time, S_fit = fit_all$surv, S_manual = S_manual)



# ================================================================
# 1) Učitavanje primera iz prezentacije: lung (survival paket)
# ================================================================
lung <- survival::lung    # učitava data.frame lung iz paketa survival

data(lung, package = "survival")
head(lung)

# Kratak opis kolona (kao na slajdu sa opisom baze):
# time    : vreme praćenja (dani)
# status  : 1=censored, 2=dead
# age     : godine
# sex     : 1=Male, 2=Female
# ph.ecog : ECOG (0=good ... 5=dead), itd.

# ================================================================
# 2) Ključni koncepti
# ================================================================

# Neka je T = slučajna promenljiva "vreme do događaja" (npr. smrt, kvar, churn).
#
# (A) Funkcija preživljavanja:
#   S(t) = P(T > t)
# "Kolika je verovatnoća da osoba/mašina/korisnik PREŽIVI duže od vremena t?"

# (B) Funkcija raspodele:
#   F(t) = P(T <= t) = 1 - S(t)

# ================================================================
# 3) Priprema podataka: event indikator + faktori
# ================================================================

# U lung bazi status je 1=cenzurisano, 2=događaj (smrt).
# Da bi bilo potpuno jasno, napravićemo "event" = 1 ako je smrt, inače 0:
lung$event <- ifelse(lung$status == 2, 1, 0)

# Pol kao faktor sa labelama (radi interpretacije u ispisima i grafiku)
lung$sex <- factor(lung$sex, levels = c(1, 2), labels = c("Male", "Female"))

# Osnovni brojevi:
N_total  <- nrow(lung)
N_events <- sum(lung$event == 1)
N_cens   <- sum(lung$event == 0)

cat("============================================================\n")
cat("Osnovne informacije o uzorku (lung)\n")
cat("Ukupno opservacija:", N_total, "\n")
cat("Broj događaja (event=1):", N_events, "\n")
cat("Broj cenzurisanih (event=0):", N_cens, "\n")
cat("============================================================\n\n")

# ================================================================
# 4) Surv() objekat — kako je povezan sa modelima
# ================================================================

# U survival paketu, sve "survival" metode počinju od Surv objekta.
# Surv(time, event) kod desnog-cenzurisanja pravi objekt koji "pamti":
#  - vreme posmatranja
#  - indikator da li se događaj desio (1) ili je cenzurisano (0)
surv_obj <- Surv(time = lung$time, event = lung$event)

# Napomena
# U prezentaciji se često piše Surv(time, status) direktno.
# To radi zato što Surv() ume da prepozna kodiranje 1/2 i mapira ga na 0/1.
# Ipak, u nastavi je pedagoški jasnije eksplicitno napraviti event (sto je i uradjeno!)

# ================================================================
# 5) Kaplan–Meier (KM) kriva za CELOKUPAN uzorak
# ================================================================

# KM estimator je neparametarski estimator S(t).
# KM formula (slajd sa proizvodom):
#
#   Ŝ(t) = Π_{t_i <= t} (1 - d_i / n_i)
#
# gde je:
#   t_i  : i-ti DISTINCT trenutak događaja (vreme kada se desila smrt/kvar/...)
#   d_i  : broj događaja koji se desio baš u t_i
#   n_i  : broj "na riziku" neposredno pre t_i (risk set)
#
# Ovo je direktna primena lančanog pravila verovatnoće:
#   P(T > t_k) = Π P(T > t_i | T >= t_i)
# a procena uslovne verovatnoće "preživi preko t_i, ako je doživeo t_i"
# je približno (n_i - d_i)/n_i = 1 - d_i/n_i.

fit_all <- survfit(surv_obj ~ 1, data = lung)
fit_all

# Median survival time:
# Medijana preživljavanja je t_med takvo da Ŝ(t_med) = 0.5.
# U R-u to dobijamo iz summary(fit)$table.
tab_all <- summary(fit_all)$table
tab_all

med_all <- unname(tab_all["median"])
lcl_all <- unname(tab_all["0.95LCL"])
ucl_all <- unname(tab_all["0.95UCL"])

cat("============================================================\n")
cat("Kaplan–Meier za ceo uzorak:\n")
cat("Medijana preživljavanja =", med_all, "dana\n")
cat("95% CI za medijanu = [", lcl_all, ",", ucl_all, "]\n")
cat("============================================================\n\n")

# ================================================================
# 5a) Ručna KM verifikacija: proizvod (1 - d_i/n_i)
# ================================================================

# fit_all$n.risk  = n_i
# fit_all$n.event = d_i
# KM "step" uslovne verovatnoće preživljavanja preko t_i je:
step_prob <- 1 - (fit_all$n.event / fit_all$n.risk)

# Ručna KM procena do svakog event time:
km_manual <- cumprod(step_prob)

# Uporedimo prvih nekoliko redova:
km_check <- data.frame(
  time      = fit_all$time[1:10],
  n_risk    = fit_all$n.risk[1:10],
  d_event   = fit_all$n.event[1:10],
  step_prob = round(step_prob[1:10], 6),
  surv_fit  = round(fit_all$surv[1:10], 6),
  surv_man  = round(km_manual[1:10], 6)
)
cat("Prvih 10 KM koraka (fit vs ručno):\n")
print(km_check)
cat("\nMaks. apsolutna razlika (fit_all$surv - km_manual): ",
    max(abs(fit_all$surv - km_manual)), "\n\n", sep="")

# (Teorijski komentar)
# Zašto je ova ručna formula "statistički prirodna"?
# Jer u svakom trenutku događaja t_i procenjujemo:
#   P(preživeti preko t_i | preživeti do t_i) ≈ (n_i - d_i)/n_i
# i onda množimo uslovne verovatnoće (lančano pravilo).

# ================================================================
# 6) KM krive po POLU (sex)
# ================================================================
fit_sex <- survfit(surv_obj ~ sex, data = lung)
print(fit_sex)

# Tabela sa medijanama po grupama:
tab_sex <- summary(fit_sex)$table
tab_sex

# Izvlačenje korisnih stvari:
med_by_sex    <- tab_sex[, "median"]
events_by_sex <- tab_sex[, "events"]
records_by_sex <- tab_sex[, "records"]
cens_by_sex <- records_by_sex - events_by_sex

cat("============================================================\n")
cat("Kaplan–Meier po polu:\n")
cat("Male  : median =", med_by_sex["sex=Male"],
    "| events =", events_by_sex["sex=Male"],
    "| censored =", cens_by_sex["sex=Male"], "\n")
cat("Female: median =", med_by_sex["sex=Female"],
    "| events =", events_by_sex["sex=Female"],
    "| censored =", cens_by_sex["sex=Female"], "\n")
cat("============================================================\n\n")

# ================================================================
# 7) Pristup komponentama survfit() objekta
# ================================================================
# survfit vraća objekat koji ima komponente:
#   $n, $time, $n.risk, $n.event, $n.censor, $surv, $upper, $lower, $strata, ...

cat("Komponente survfit objekta (skraćeno):\n")
str(fit_sex)

# Kao u prezentaciji: pravimo data.frame iz komponenti
d <- data.frame(
  time     = fit_sex$time,
  n.risk   = fit_sex$n.risk,
  n.event  = fit_sex$n.event,
  n.censor = fit_sex$n.censor,
  surv     = fit_sex$surv,
  upper    = fit_sex$upper,
  lower    = fit_sex$lower
)
cat("\nPrvih nekoliko redova tabele iz survfit (kombinovano za obe grupe):\n")
head(d)

# ================================================================
# 8) Vizuelizacija KM krivih (ggsurvplot)
# ================================================================

# Interpretacija (bitno):
# - x-osa: vreme (dani)
# - y-osa: Ŝ(t) = procenjena verovatnoća preživljavanja
# - vertikalni pad: dogodio se event (smrt)
# - "tick" na liniji: cenzurisanje (izašao iz praćenja bez event-a)
# - risk table: koliko je još "na riziku" u različitim trenucima

p1 <- ggsurvplot(
  fit_sex,
  data = lung,
  pval = TRUE,          # prikazuje p iz log-rank testa
  conf.int = TRUE,      # intervali poverenja
  risk.table = TRUE,    # tabela "Number at risk"
  risk.table.col = "strata",
  linetype = "strata",
  surv.median.line = "hv",
  ggtheme = theme_bw(),
  palette = c("#E7B800", "#2E9FDF")
)
print(p1)

# Skraćivanje x-ose (kao u prezentaciji primer xlim):
p2 <- ggsurvplot(
  fit_sex,
  data = lung,
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col = "strata",
  ggtheme = theme_bw(),
  palette = c("#E7B800", "#2E9FDF"),
  xlim = c(0, 600)
)
print(p2)

# Kumulativni događaji (fun="event") — kao u prezentaciji:
p3 <- ggsurvplot(
  fit_sex,
  data = lung,
  conf.int = TRUE,
  risk.table = TRUE,
  ggtheme = theme_bw(),
  palette = c("#E7B800", "#2E9FDF"),
  fun = "event"
)
print(p3)

# ================================================================
# 9) KM "life table" / summary survival curves (surv_summary)
# ================================================================
# surv_summary() vraća data.frame sa kolonama:
# time, n.risk, n.event, n.censor, surv, std.err, upper, lower, strata
res_sum <- surv_summary(fit_sex)
cat("Prvih 6 redova surv_summary(fit_sex):\n")
head(res_sum)

# ================================================================
# 10) Log-rank test: survdiff() — kao u prezentaciji
# ================================================================
# Log-rank test proverava:
# H0: S1(t) = S2(t) = ... (nema razlike u survival krivama)
# Ideja: upoređuje posmatrani broj događaja (O) u grupi sa očekivanim (E)
# pod H0.
#
# Test statistika je "približno" hi-kvadrat:
#   χ² ≈ Σ (O_g - E_g)^2 / Var(O_g - E_g)
# za 2 grupe df = 1.

surv_diff <- survdiff(surv_obj ~ sex, data = lung)
surv_diff

chisq_val <- surv_diff$chisq
df_val <- length(surv_diff$n) - 1
p_logrank <- 1 - pchisq(chisq_val, df = df_val)

cat("============================================================\n")
cat("Log-rank test (survdiff):\n")
cat("Chi-square =", round(chisq_val, 4), " | df =", df_val,
    " | p-value =", signif(p_logrank, 5), "\n")
cat("============================================================\n\n")

# ================================================================
# 11) Cox Proportional Hazards model (Cox PH)
# ================================================================
# Cox model:
#   h(t | x) = h0(t) * exp(β1 x1 + ... + βp xp)
#
# h0(t) je "baseline hazard" (kad su varijable 0 ili na referentnom nivou).
# exp(βj) je Hazard Ratio (HR) za porast xj za 1 jedinicu (ili prelazak na nivo kod faktora),
# uz ostale kovarijate fiksne.
#
# Važno:
#   HR = 1  -> nema efekta
#   HR > 1  -> povećava hazard (veći rizik događaja)
#   HR < 1  -> smanjuje hazard (zaštitni efekat)

cox_sex <- coxph(surv_obj ~ sex, data = lung)
summary(cox_sex)

# Izvlačenje HR i p-vrednosti:
cox_sex_sum <- summary(cox_sex)
hr_sex <- cox_sex_sum$coefficients[,"exp(coef)"]
p_sex  <- cox_sex_sum$coefficients[,"Pr(>|z|)"]
ci_l   <- cox_sex_sum$conf.int[,"lower .95"]
ci_u   <- cox_sex_sum$conf.int[,"upper .95"]

cat("============================================================\n")
cat("Cox model (samo pol):\n")
cat("HR (Female vs Male) =", round(hr_sex, 3),
    " | 95% CI [", round(ci_l,3), ",", round(ci_u,3), "]",
    " | p =", signif(p_sex, 5), "\n")
cat("============================================================\n\n")

# Interpretacija na procenat:
# Ako je HR=0.588, to znači da je hazard kod žena 0.588 puta hazard kod muškaraca.
# Procentualno smanjenje hazarda:
risk_reduction_pct <- (1 - hr_sex) * 100
cat("Tumačenje: Female ima oko", round(risk_reduction_pct, 1),
    "% manji hazard (rizik po jedinici vremena) od Male.\n\n")

# Često korisno: obrnuto poređenje (Male vs Female) = 1/HR
hr_male_vs_female <- 1 / hr_sex
cat("Ekvivalentno: Male ima", round(hr_male_vs_female, 2),
    "puta veći hazard od Female.\n\n")

# ================================================================
# 12) Multivarijantni Cox model (sex + age + ph.ecog) — sporedno, ali vazno!
# ================================================================
cox_multi <- coxph(surv_obj ~ sex + age + ph.ecog, data = lung)
cox_multi_sum <- summary(cox_multi)
cox_multi_sum

# Koliko je opservacija ušlo u model (zbog NA može biti manje od N_total):
cat("Broj opservacija korišćenih u cox_multi:", cox_multi$n, "\n")
cat("Izbačeno zbog NA:", N_total - cox_multi$n, "\n\n")

# HR i p-vrednosti:
hr_vals <- exp(coef(cox_multi))
p_vals  <- cox_multi_sum$coefficients[,"Pr(>|z|)"]
ci_lower <- cox_multi_sum$conf.int[,"lower .95"]
ci_upper <- cox_multi_sum$conf.int[,"upper .95"]

cat("HR (exp(coef)) u multivarijantnom modelu:\n")
print(round(hr_vals, 4))
cat("\np-vrednosti u multivarijantnom modelu:\n")
print(signif(p_vals, 5))
cat("\n")

# Koji prediktori su statistički značajni na 0.05?
sig_pred <- names(p_vals)[p_vals < 0.05]
cat("Prediktori sa p < 0.05:", paste(sig_pred, collapse = ", "), "\n\n")

# Primer tumačenja kontinuirane varijable (age):
# HR_age = exp(β_age) je multiplikativni faktor na hazard za porast godina za 1.
# Za porast od 10 godina:
beta_age <- coef(cox_multi)["age"]
hr_10y <- exp(10 * beta_age)
cat("Ako se age poveća za 10 godina, hazard se množi sa:", round(hr_10y, 3), "\n\n")

# ================================================================
# 13) Provera PH pretpostavke (Schoenfeld residuals): cox.zph()
# ================================================================
# Cox PH pretpostavlja da je HR konstanta kroz vreme (proportional hazards).
# cox.zph testira korelaciju Schoenfeld reziduala sa vremenom:
# - p < 0.05 može ukazivati na kršenje PH pretpostavke.
ph_test <- cox.zph(cox_multi)
print(ph_test)
# plot(ph_test)  # otkomentariši da vidiš grafike po varijablama

cat("\nNapomena: Ako neka varijabla ima p < 0.05 u cox.zph, to je signal da PH možda ne važi.\n\n")

# ================================================================
# 14) (Opcija iz prezentacije) Cox model sa više promenljivih
# ================================================================
# Kao na slajdu gde se koristi "sve promenljive".
# Upozorenje: zbog missing vrednosti biće izbačene neke opservacije!!!
cox_all <- coxph(surv_obj ~ sex + age + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss,
                 data = lung)
summary(cox_all)

cat("\nBroj opservacija korišćenih u cox_all:", cox_all$n, "\n")
cat("Izbačeno zbog NA:", N_total - cox_all$n, "\n\n")

# ================================================================
# 15) (OPCIONO) Survival analiza kao churn
# ================================================================
# Ideja: "događaj" = churn (prestanak aktivnosti), T = vreme do churn-a.
# Ovde pravimo mali SIMULIRANI primer sa 3 grupe:
#  - Non-paying (najbrže churn)
#  - Paying (srednje)
#  - Whales (najsporije churn)
#
# Ovo je samo da se intuitivno vidi kako KM izgleda u churn kontekstu.

set.seed(123)

n_non    <- 250
n_pay    <- 250
n_whale  <- 250

# Simulacija vremena do churn-a (dani) — različite skale:
T_non   <- rweibull(n_non,   shape = 0.7, scale = 10)   # brzo
T_pay   <- rweibull(n_pay,   shape = 0.7, scale = 40)   # srednje
T_whale <- rweibull(n_whale, shape = 0.7, scale = 120)  # sporo

# Administrativno desno-cenzurisanje (studija traje npr. 200 dana ili neki drugi zadati broj):
C <- 200
time_obs <- c(pmin(T_non, C), pmin(T_pay, C), pmin(T_whale, C))
event_obs <- c(as.numeric(T_non <= C), as.numeric(T_pay <= C), as.numeric(T_whale <= C))
group <- factor(c(rep("Non-paying", n_non), rep("Paying", n_pay), rep("Whales", n_whale)),
                levels = c("Non-paying","Paying","Whales"))

churn_df <- data.frame(time = time_obs, event = event_obs, group = group)

fit_churn <- survfit(Surv(time, event) ~ group, data = churn_df)

p_churn <- ggsurvplot(
  fit_churn, data = churn_df,
  conf.int = TRUE, risk.table = TRUE, pval = TRUE,
  ggtheme = theme_bw()
)
print(p_churn)

# ================================================================
# Bitno:
# ================================================================
# - Kaplan–Meier daje neparametarsku procenu S(t)=P(T>t) kao proizvod uslovnih verovatnoća.
# - Log-rank test formalno poredi KM krive (O vs E događaji), statistika ~ χ².
# - Cox PH modeluje hazard: h(t|x)=h0(t)exp(β'x); exp(β) je hazard ratio (multiplikativni efekat).
################################################################################



