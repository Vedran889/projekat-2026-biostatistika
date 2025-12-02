############################################################
# Application of biclustering algorithms 
# Source: Application of biclustering algorithms.pdf
#
# Packages used (per slides):
#   readr     : CSV import helper
#   biclust   : CC/ChengâChurch (BCCC), xMotif/Quest, plots
#   BiocManager: helper to install Bioconductor packages
#   BicARE    : FLOC (Flexible Overlapped biClustering)
############################################################

install.packages("readr")       # CSV I/O
install.packages("biclust")     # biclustering + plots
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
# Bioconductor package for FLOC:
if (!requireNamespace("BicARE", quietly = TRUE)) BiocManager::install("BicARE")

library(readr)
library(biclust)
library(BicARE)

################################################################################
# PART A â Delta biclustering on GFSI2021 (food security)
################################################################################

# 1. ciscenje podataka , ovdje ne moze Country varijabla
# 2. skaliranje (neka standardizacija)
# 3. diskretizacija
# 4. set.seed(broj)
# 5. mora biti matrix
# @ --  pristupanje objekta posebne klase (Biclust) (S4)

GFSI2021 <- read.csv(file.choose())

View(GFSI2021)  # nema undo u R

## Remove string column (Country) before biclustering !!!!!!
GFSI2021.use <- subset(GFSI2021, select = -c(ï..Country))


################################################################################
## ---- OPTIONAL preproc for zeros: place this BEFORE creating resBCCC ----#####

# 1) Audit zeros per column
zero_counts <- sapply(GFSI2021.use, function(x) sum(x == 0, na.rm = TRUE))
zero_prop   <- zero_counts / nrow(GFSI2021.use)
zero_report <- data.frame(variable = names(zero_counts),
                          zeros = zero_counts,
                          zero_pct = round(100 * zero_prop, 1))
zero_report <- zero_report[order(-zero_report$zero_pct), ]
print(zero_report, row.names = FALSE)

# # 2) Drop pathologically uninformative columns:
# #    - zero variance (all identical values)
# #    - >95% zeros (adjust threshold if needed)
# nzv_cols       <- which(sapply(GFSI2021.use, function(x) var(x, na.rm = TRUE) == 0))
# hi_zero_cols   <- which(zero_prop > 0.95)
# drop_cols_idx  <- union(nzv_cols, hi_zero_cols)
# if (length(drop_cols_idx)) {
#   message("Dropping columns (zero variance or >95% zeros): ",
#           paste(names(GFSI2021.use)[drop_cols_idx], collapse = ", "))
#   GFSI2021.use <- GFSI2021.use[, -drop_cols_idx, drop = FALSE]
# }

# # 3) If (and ONLY if) zeros mean 'missing' in specific variables,
# #    list them here to convert 0 -> NA. Otherwise leave list empty.
# cols_where_zero_is_missing <- c(
#   # "VarName1", "VarName2"  # <-- put column names here if applicable
# )
# if (length(cols_where_zero_is_missing)) {
#   for (v in cols_where_zero_is_missing) {
#     GFSI2021.use[[v]][GFSI2021.use[[v]] == 0] <- NA
#   }
# }

# # 4) Optional but recommended: standardize columns (z-score) to balance scales
# GFSI2021.use <- as.data.frame(scale(GFSI2021.use))

# 5) Rebuild matrix for biclustering
GFSI2021.use.matrix <- as.matrix(GFSI2021.use)

##----------------------------------------------------------------------------##
################################################################################

set.seed(123)

## ChengâChurch (delta) biclustering â initial run (number = 25)  
resBCCC <- biclust(GFSI2021.use.matrix, method = BCCC(), 
                   delta = 80, 
                   alpha = 1.5, 
                   number = 25) # number -- maksimalan broj klastera (gornja granica)
# u praksi -- igranje sa parametrima
summary(resBCCC)
# Output note: inspect âNumber of Clusters foundâ, cluster sizes, and
# rows/columns selected per bicluster; this guides segmentation breadth.

## Reârun (number = 7) â slides demonstrate same structure retained  
set.seed(123)
resBCCC <- biclust(GFSI2021.use.matrix, method = BCCC(), delta = 80, alpha = 1.5, number = 7)
summary(resBCCC)
# Output note: with same seed, core biclusters match; object metadata differs when number is reduced. 

## Reârun (number = 4) â again for comparison in slides 
set.seed(123)
resBCCC <- biclust(GFSI2021.use.matrix, method = BCCC(), delta = 80, alpha = 1.5, number = 4)
summary(resBCCC)
# Output note: structure of retained biclusters remains; fewer biclusters are stored in the object. 

# =================== Are there some unassigned rows/columns???? ===================================

X <- GFSI2021.use

# redovi koji nisu ni u jednom BC (0 Älanstava)
rows_unassigned <- which(rowSums(resBCCC@RowxNumber) == 0)
length(rows_unassigned); head(rownames(X)[rows_unassigned], 10)

# kolone (varijable) koje nisu ni u jednom BC
cols_unassigned <- which(colSums(resBCCC@NumberxCol) == 0)
length(cols_unassigned); head(colnames(X)[cols_unassigned], 10)




############## Handling bicluster properties ###################################

## List values inside a specific bicluster (e.g., #3)  
bicluster(GFSI2021.use, resBCCC,number=3) # error, unused argument number=3

# Sometimes, due to multiple bicluster functions across different libraries having the same labelling
# an error "Error in bicluster(GFSI2021.use, resBCCC, number = 3) : unused argument (number = 3)"
# may occur. The remedy is just to call the function "by force" from the root library, as follows:
biclust::bicluster(GFSI2021.use, resBCCC, number = 1)

### biclust vs. bicluster

# biclust::biclust() is a function that uses df or matrix input to creat an S4 object of class Biclust -> resBCCC herein

# biclust::bicluster() is a function that extracts/shows one given bicluster from the bicluster object

# Note: there is also BicARE::bicluster(resFLOC, k, graph=TRUE) function that has the same label
# that may cause miscalling the right function which results with an error "unused argument ()"

# mean_in -- prosek entiteta u datom biklasteru
# mean_out -- prosek entiteta u datom biklasteru koji se ne nalaze u datom biklasteru
# mean_all -- prosek svih

# mean_in == mean_out -- nije nam od interesa jer su ujednaceni


# Anatomy of Biclust object ----------------------------------------------
k_found <- resBCCC@Number        # how many biclusters were found
cat("Biclusters found:", k_found, "\n")

resBCCC@Parameters
resBCCC@RowxNumber
resBCCC@NumberxCol
resBCCC@Number # number of biclusters
resBCCC@info

# resBCCC@RowxNumber is a logical matrix with dimensions (n_rows x k): row membership per bicluster
# resBCCC@NumberxCol is a logical matrix with dimensions (k x n_cols): column membership per bicluster

dim(resBCCC@RowxNumber) # 113 x 7 -> 113 rows were used to create 7 biclusters
dim(resBCCC@NumberxCol) # 7 x 25  -> 25 columns were used to create 7 biclusters


which(resBCCC@RowxNumber[, 1]) #indexes of rows that are part of bicluster number 1
which(resBCCC@NumberxCol[1,])  #indexes of columns that are part of bicluster number 1

# How many rows in each bicluster?
rows_per_bc <- colSums(resBCCC@RowxNumber)
rows_per_bc

# How many variables in each bicluster?
cols_per_bc <- rowSums(resBCCC@NumberxCol)
cols_per_bc

summary(resBCCC)


### Accessing specific rows/columns in given bicluster (by index) ##############

## Assume:
## X        = your data (data.frame or matrix) used in biclust()
## resBCCC  = Biclust object from biclust(..., method=BCCC(), ...)
stopifnot(inherits(resBCCC, "Biclust"))
X <- GFSI2021.use

k <- 1  # <-- choose bicluster index (1..resBCCC@Number)

# (1) Row indices that belong to bicluster k
rows_k <- which(resBCCC@RowxNumber[, k])
rows_k                      # print indices
length(rows_k)              # how many rows


# (2) Column indices that define bicluster k
cols_k <- which(resBCCC@NumberxCol[k, ])
cols_k                      # print indices
length(cols_k)              # how many columns

# (3) Submatrix (rows and columns of bicluster k)
X_k <- X[rows_k, cols_k, drop = FALSE]
X_k

# (4) One specific column by numeric index j (and check membership first)
j <- 8  # e.g., take 5th column of X
if (resBCCC@NumberxCol[k, j]) {
  vals_kj <- X[rows_k, j]
  head(vals_kj)
} else {
  message(sprintf("Column #%d is NOT part of bicluster %d.", j, k))
}

# (5) One specific row by numeric index i (and restrict to bicluster columns)
rows_k
rows_k[2]
i <- rows_k[2]
X[i, cols_k, drop = FALSE]


### Accessing specific rows/columns in given bicluster (by column name) ########

## This assumes your X has colnames(X)
stopifnot(!is.null(colnames(X)))

k <- 1                # bicluster index
rows_k <- which(resBCCC@RowxNumber[, k])     # as above

# (1) Names of variables included in bicluster k
vars_in_k <- colnames(X)[which(resBCCC@NumberxCol[k, ])]
vars_in_k

# (2) Values for ONE named variable v inside bicluster k, with membership check
v <- "x1.4"  # <-- put the exact column name
j <- match(v, colnames(X))
j
if (is.na(j)) stop(sprintf("Column '%s' not found in X.", v))

if (resBCCC@NumberxCol[k, j]) {
  vals_v_in_k <- X[rows_k, j]
  head(vals_v_in_k)
} else {
  message(sprintf("Variable '%s' is NOT part of bicluster %d.", v, k))
}

# (3) Values for MULTIPLE named variables only if they are included in k
vars <- c("X1.4", "X2.3", "x4.1")             # your chosen names
js    <- match(vars, colnames(X))             # numeric positions (may include NA)
ok    <- !is.na(js)                            # keep those present in X
keep  <- rep(FALSE, length(js))
keep[ok] <- resBCCC@NumberxCol[k, js[ok]]
# keep == TRUE means the variable is part of bicluster k

if (!any(keep)) {
  message(sprintf("None of the requested variables are part of bicluster %d.", k))
} else {
  X_subset <- X[rows_k, js[keep], drop = FALSE]
  X_subset                              # submatrix: rows in k Ã requested vars in k
}

##### Little practice #####

# Which rows belong to k?
which(resBCCC@RowxNumber[, k])

# Which variable NAMES belong to k (reliable even if @NumberxCol has no dimnames):
colnames(X)[which(resBCCC@NumberxCol[k, ])]

# and the bicluster k looks like:
rows_k <- which(resBCCC@RowxNumber[, k])
cols_k <- which(resBCCC@NumberxCol[k, ])
X_k <- X[rows_k, cols_k, drop = FALSE]
X_k
k <- 7

# Is a given variable name 'v' part of bicluster k? (onliner :))))) )
v <- "x1.4"; j <- match(v, colnames(X)); !is.na(j) && resBCCC@NumberxCol[k, j]

v <- "X1.3"
j <- match(v, colnames(X))
!is.na(j) && resBCCC@NumberxCol[k, j]


####################################################################################################

################################################################################

### Mini tasks ####

# 0) Broj biklastera koji sadrze varijablu k
k<-"X1.1"
length(which(resBCCC@NumberxCol[, match(k, colnames(GFSI2021.use))])) # match(k, colnames(GFSI2021.use)) uparuje 2 niza

# 1) Prosek varijable x1.1 u 1. biklasteru
mean(GFSI2021.use[resBCCC@RowxNumber[, 1], k], na.rm=TRUE)

# 2) Maksimum varijable x2.3 u 3. biklasteru
max()

# 3) Broj redova u 7. biklasteru
counts()

# 4) Medijana varijable x4.1 u 5. biklasteru
median()

# 5) U koliko biklastera se pojavljuje svaka promenljiva
# for petlja provijeri sljedece




### Advanced approach - scalable, robust & reproducable ###########

# Minimal helper: get values of a NAMED column inside bicluster k
X <- GFSI2021.use
B <- resBCCC
k 

bc_vals <- function(B, X, k, var, require_in = TRUE) {
  if (!inherits(B, "Biclust")) stop("B must be a 'Biclust' object.")
  if (is.null(colnames(X)))   stop("X must have column names.")
  if (!is.character(var) || length(var) != 1L) stop("Provide one column name (character).")
  if (k < 1L || k > B@Number) stop(sprintf("k must be in 1..%d", B@Number))
  j <- match(var, colnames(X)); if (is.na(j)) stop(sprintf("Column '%s' not found in X.", var))
  r <- which(B@RowxNumber[, k, drop = TRUE]); if (!length(r)) stop(sprintf("Bicluster %d has no rows.", k))
  if (require_in && !isTRUE(B@NumberxCol[k, j])) stop(sprintf("Column '%s' is not part of bicluster %d.", var, k))
  X[r, j, drop = TRUE]
}



# 1) prosek varijable x1.1 u 1. biklasteru

mean(bc_vals(resBCCC, X, 1, "X1.1"), na.rm = TRUE)



# 2) maksimum varijable x2.3 u 3. biklasteru

max(bc_vals(resBCCC, X, 3, "X2.3"), na.rm = TRUE)



# 3) broj redova u 7. biklasteru

sum(resBCCC@RowxNumber[, 7])



# 4) medijana varijable x4.1 u 5. biklasteru

median(bc_vals(resBCCC, X, 5, "X4.1"), na.rm = TRUE)



# 5) medijana varijable x4.1 u svim biklasterima u kojima postoji

j <- match("X4.1", colnames(X)); if (is.na(j)) stop("x4.1 not found in X.")
ks <- which(resBCCC@NumberxCol[, j, drop = TRUE])
setNames(sapply(ks, function(k) median(bc_vals(resBCCC, X, k, "X4.1"), na.rm = TRUE)),
         paste0("bc", ks))



# 6) abs razlika izmedju svih proseka varijabli x2.2 i x1.1 izracunatih
#     u svim biclasterima gde su obe varijable prisutne

# U kojim biclasterima su pojedinaÄno?
which(resBCCC@NumberxCol[, match("X1.3", colnames(X)), drop = TRUE])
which(resBCCC@NumberxCol[, match("X1.1", colnames(X)), drop = TRUE])

# names must match EXACTLY what's in colnames(X)
jA <- match("X1.3", colnames(X))
jB <- match("X1.1", colnames(X))

ksA <- which(resBCCC@NumberxCol[, jA, drop = TRUE])
ksB <- which(resBCCC@NumberxCol[, jB, drop = TRUE])
ks  <- intersect(ksA, ksB)

if (length(ks) == 0L) {
  # nothing to compute; return an empty named vector (won't error)
  setNames(numeric(0), character(0))
} else {
  setNames(
    sapply(ks, function(k) {
      r <- which(resBCCC@RowxNumber[, k, drop = TRUE])
      abs(mean(X[r, jA], na.rm = TRUE) - mean(X[r, jB], na.rm = TRUE))
    }),
    paste0("bc", ks)
  )
}
################################################################################


############## Interpreting various bicluster representations ##############

## ============================================================================
##  BICLUSTER VISUALS â BAR CHARTS, MEMBERSHIP CHARTS, PARALLEL COORDINATES
## ============================================================================

## ============================================================================
## 1) BICLUSTER BAR CHARTS (GLOBAL MEAN vs SEGMENT MEANS)
##    â Unordered: raw variable order
##    â Ordered: columns reordered to reveal patterns
## ============================================================================

# (A) Unordered bar charts
#    What it shows: For each bicluster, blue bars ~ overall means; orange bars ~ means in that bicluster.
#    Why it's useful: Quick âsignature viewâ to spot which variables differ strongly inside the segment.
biclust::biclustbarchart(GFSI2021.use, resBCCC,
                         main = "Bicluster bar charts (global mean vs. bicluster mean)")

# (B) Ordered bar charts (reorder columns to sharpen the pattern)
#    What it shows: Same as above, but columns are reordered to group coâmoving variables.
#    Why it's useful: Patterns often âpop outâ once indicators are ordered.
ord <- biclust::bicorder(resBCCC, cols = TRUE, rev = FALSE)
biclust::biclustbarchart(GFSI2021.use, resBCCC, which = ord,
                         main = "Ordered bicluster bar charts (columns reordered for clarity)")

#### Little help for the graph interpretation
k <- 1  # panel A (bicluster 1)
rows_in <- which(resBCCC@RowxNumber[, k])
cols_in <- which(resBCCC@NumberxCol[k, ])
tab <- data.frame(
  variable  = colnames(GFSI2021.use)[cols_in],
  mean_in   = colMeans(GFSI2021.use[rows_in,  cols_in, drop = FALSE], na.rm = TRUE),
  mean_out  = colMeans(GFSI2021.use[-rows_in, cols_in, drop = FALSE], na.rm = TRUE)
)
tab$delta    <- tab$mean_in - tab$mean_out
tab$abs_diff <- abs(tab$delta)
tab[order(-tab$abs_diff), ][1:5, ]   # Topâ5 promenljivih koje najviÅ¡e razlikuju segment
####


## ============================================================================
## 2) MEMBERSHIP CHARTS
##    â Variableâbyâbicluster (which variables define each bicluster)
##    â Rowâbyâbicluster (which rows/entities fall into each bicluster)
##    Tip: We draw simple 0/1 heatmaps using base 'image' so students see the structure.
## ============================================================================

## (A) VARIABLES â BICLUSTER heatmap (K Ã p)
M <- resBCCC@NumberxCol * 1    # logical -> numeric (0/1)
rownames(M) <- paste0("BC", seq_len(resBCCC@Number))
colnames(M) <- colnames(GFSI2021.use)

op <- par(mar = c(8, 6, 2, 1)) # more space for long axis labels
# image() uses [0,1] scale on axes; create label positions:
ax_x <- seq(0, 1, length.out = ncol(M))
ax_y <- seq(0, 1, length.out = nrow(M))
image(t(M[nrow(M):1, , drop = FALSE]), axes = FALSE, col = c("grey90", "steelblue"))
axis(1, at = ax_x, labels = colnames(M), las = 2, cex.axis = 0.7)               # variables (rotated)
axis(2, at = ax_y, labels = rev(rownames(M)), las = 2, cex.axis = 0.9)          # BC names
box()
mtext("Variable membership per bicluster (1=steelblue, 0=grey)", side = 3, line = 0.5, cex = 0.9)
par(op)

## (B) ROWS â BICLUSTER heatmap (n Ã K)
R <- resBCCC@RowxNumber * 1
rownames(R) <- if (!is.null(rownames(GFSI2021.use))) rownames(GFSI2021.use) else paste0("row", seq_len(nrow(R)))
colnames(R) <- paste0("BC", seq_len(resBCCC@Number))

op <- par(mar = c(4, 6, 2, 1))
ax_x <- seq(0, 1, length.out = ncol(R))
ax_y <- seq(0, 1, length.out = nrow(R))
# Note: large n â suppress y labels to keep it readable
image(t(R[nrow(R):1, , drop = FALSE]), axes = FALSE, col = c("grey90", "steelblue"))
axis(1, at = ax_x, labels = colnames(R), las = 2, cex.axis = 0.9)                # BC names
# Optionally: show only a few row labels if n is small; otherwise skip
if (nrow(R) <= 30) axis(2, at = ax_y, labels = rev(rownames(R)), las = 2, cex.axis = 0.7)
box()
mtext("Row membership per bicluster (1=steelblue, 0=grey)", side = 3, line = 0.5, cex = 0.9)
par(op)

########################################################
# Bicluster membership chart
########################################################

# #######################################################################
# # Raspodela pripadnosti svake promenljive ukupnom broju klastera
# setNames(colSums(resBCCC@NumberxCol), colnames(GFSI2021.use))
# 
# # 0) Broj biklastera koji sadrze varijablu k
# k <- "X1.2"
# length(which(resBCCC@NumberxCol[, match(k, colnames(GFSI2021.use)), drop = TRUE]))
# 
# which(resBCCC@NumberxCol[, match(k, colnames(GFSI2021.use)), drop = TRUE])
# #######################################################################


biclustmember(resBCCC, GFSI2021.use)

ord <- bicorder(resBCCC, cols = TRUE, rev = TRUE)
#cols = TRUE -> orders the columns by appearance in the bicluster, else order the rows
biclustmember(resBCCC,
              GFSI2021.use,
              which = ord,
              mid=TRUE, # if TRUE shows all 3 mean values
              main = "Bicluster membership graph ORDERED")

# quick check
k  <- 2
j  <- match("X1.2", colnames(GFSI2021.use))
rin <- which(resBCCC@RowxNumber[, k])
c(mean_in  = mean(GFSI2021.use[ rin,  j], na.rm = TRUE),
  mean_all = mean(GFSI2021.use[     ,  j], na.rm = TRUE),
  mean_out = mean(GFSI2021.use[-rin,  j], na.rm = TRUE))


########################################################
# Heatmap of biclustering
########################################################

op <- par(lwd = 5)  # npr. 3 ili 4

heatmapBC(x = GFSI2021.use,
          bicResult = resBCCC,
          number = c(4),
          order    = FALSE, #if FALSE there is no bicluster overlapping
          outside  = TRUE)

par(op)  


## ============================================================================
## 3) PARALLEL COORDINATE PLOTS
##    â Each line = one row/entity; 
        #xâaxis = variables;
        #yâaxis = values
##    â compare = FALSE â only members of the bicluster (clean pattern)
##    â compare = TRUE  â show both inâcluster and outâofâcluster lines
## ============================================================================

k <- 5
rows_in <- which(resBCCC@RowxNumber[, k])
cols_in <- which(resBCCC@NumberxCol[k, ])

Xk <- as.matrix(GFSI2021.use.matrix[rows_in, cols_in, drop = FALSE])

# Ensure we have labels
if (is.null(rownames(Xk))) rownames(Xk) <- as.character(rows_in)

# Colors (distinct per row) + a little transparency
cols <- grDevices::adjustcolor(rainbow(nrow(Xk), s = 0.8, v = 0.9), alpha.f = 0.85)

op <- par(mar = c(7, 4, 3, 10))  # extra room for axis & right-side labels
matplot(
  x = t(Xk), type = "l", lwd = 2, lty = 1, col = cols,
  xaxt = "n", xlab = "Variables in bicluster", ylab = "Value",
  main = sprintf("Bicluster %d: rows = %d, cols = %d", k, nrow(Xk), ncol(Xk))
)
axis(1, at = seq_len(ncol(Xk)), labels = colnames(Xk), las = 2, cex.axis = 0.8)

# Label each line at the right edge (last variable)
x_last <- ncol(Xk)
for (i in seq_len(nrow(Xk))) {
  text(x = x_last + 0.1, y = Xk[i, x_last], labels = rownames(Xk)[i],
       cex = 0.7, col = cols[i], pos = 4, xpd = NA)
}

# Optional: also label on the left edge
for (i in seq_len(nrow(Xk))) {
  text(x = 1 - 0.1, y = Xk[i, 1], labels = rownames(Xk)[i],
       cex = 0.6, col = cols[i], pos = 2, xpd = NA)
}

# Legend (scrollable in RStudio plotting window)
legend("right", inset = c(-0.1, 0), xpd = NA, bty = "n",
       legend = rownames(Xk), col = cols, lwd = 2, cex = 0.7)
par(op)

# # Click near a line to print the label (base R graphics)
# identify(x = rep(ncol(resBCCC), nrow(resBCCC)), y = resBCCC[, ncol(resBCCC)], labels = rownames(resBCCC))


##### Or maybe like this
biclust::parallelCoordinates(GFSI2021.use.matrix, 
                             bicResult = resBCCC, 
                             number = 5,
                             compare = FALSE, 
                             info = TRUE, 
                             plotBoth = TRUE)


# (B) Compare inâcluster vs outâofâcluster:
biclust::parallelCoordinates(GFSI2021.use.matrix, 
                             bicResult = resBCCC, 
                             number = 5,
                             compare = TRUE, 
                             info = TRUE, 
                             plotBoth = TRUE)
# Read: With compare=TRUE, inâcluster lines vs outâofâcluster lines highlight which variables separate the segment.




################################################################################
# PART B â FLOC (Flexible Overlapped biClustering)
# Finds K possibly-overlapping biclusters by iteratively sampling/optimizing
# row/column membership; use when units can belong to multiple patterns (numeric data).
################################################################################

getAnywhere("FLOC")    # treba da pokaÅ¾e BicARE::FLOC
if (!requireNamespace("BicARE", quietly = TRUE)) install.packages("BicARE")
library(BicARE)

set.seed(1234)

# 1) Äista numeriÄka BASE matrica (skida tibble/data.table/Matrix slojeve)
X <- as.matrix(as.data.frame(GFSI2021.use))  # ili: as.matrix(GFSI2021.use.matrix)
storage.mode(X) <- "double"

# (opciono) ukloni NA redove/kolone â FLOC ne voli NA
X <- X[rowSums(is.na(X)) == 0, , drop = FALSE]
X <- X[, colSums(is.na(X)) == 0, drop = FALSE]

# (opciono) imena zbog kasnijih grafika
if (is.null(rownames(X))) rownames(X) <- paste0("r", seq_len(nrow(X)))
if (is.null(colnames(X))) colnames(X) <- paste0("c", seq_len(ncol(X)))


set.seed(1234)

# 3) Pozovi FLOC iz pravog paketa i sa velikim K
resFLOC <- BicARE::FLOC(
  Data    = X,
  K       = 4,      # <<< VELIKO K, ne k
  pGene   = 0.4,
  pSample = 0.6,
  r       = 70,
  N       = 18,
  M       = 9,
  t       = 700
)



## Parallel coordinates for a FLOC bicluster (e.g., #1)  
bicluster(resFLOC, 1, graph = TRUE)
# Output note: lines within the bicluster indicate similar profiles across selected variables.



################################################################################
# PART C â XâMotif / BCQuestord (ordinal data, StatFear)
# Biclustering for discrete/ordinal matrices (e.g., Likert): detects conserved
# symbol/ordering patterns within a submatrix; tune d, ns/nd/sd, alpha, number.
################################################################################

## Import ordinal dataset (Likert 1â5)  

StatFear <- read.csv(file.choose())
View(StatFear)  # preview. 

## Subset to Likert items (cols 7:16) and make a numeric matrix  
StatFear.use <- subset(StatFear, select = c(7:16))
StatFear.use.matrix <- data.matrix(StatFear.use)
# Maintain slide naming to match code snippets:
StatFear.matrix <- StatFear.use.matrix

## Run BCQuestord (parameters per slides)  
set.seed(1234)
resBCQ <- biclust(StatFear.matrix, method = BCQuestord(),
                  d = 1, ns = 3, nd = 9, sd = 4, alpha = 0.01, number = 4)
resBCQ
# Output note: object prints bicluster count and membership; in ordinal cases,
# focus on which questions (columns) and respondents (rows) coâoccur in each bicluster.

## Membership graph + ordered membership graph  
biclustmember(resBCQ, StatFear.use.matrix)
ord <- bicorder(resBCQ, cols = TRUE, rev = TRUE)
biclustmember(resBCQ, StatFear.use.matrix, which = ord,
              main = "Bicluster membership graph")
# Output note: darker blocks indicate selected rows/columns for each bicluster.

################################################################################

#              So, what? What should I use these results for????????           #

################################################################################

## ========================= BC REPORT (Top-5 + sizes) =========================
## Ulaz: X = data.frame/matrix (numeric), res = Biclust (npr. resBCCC)
## Izlaz: list(sizes = ..., top_vars = ..., narrative = ...)
X <- GFSI2021.use
res <- resBCCC

bc_report <- function(X, res, top_n = 5) {
  stopifnot(inherits(res, "Biclust"))
  n <- nrow(X); p <- ncol(X); K <- res@Number
  
  # --- tabela veliÄina i coverage ---
  sizes <- data.frame(
    bicluster = seq_len(K),
    n_rows    = colSums(res@RowxNumber),
    n_cols    = rowSums(res@NumberxCol),
    stringsAsFactors = FALSE
  )
  sizes$coverage <- (sizes$n_rows * sizes$n_cols) / (n * p)
  
  # --- pomoÄni: pooled SD za Cohen d ---
  pooled_sd <- function(x, y) {
    nx <- sum(!is.na(x)); ny <- sum(!is.na(y))
    if (nx < 2 || ny < 2) return(NA_real_)
    sx <- stats::sd(x, na.rm = TRUE); sy <- stats::sd(y, na.rm = TRUE)
    den <- (nx + ny - 2); if (!is.finite(den) || den <= 0) return(NA_real_)
    psd <- sqrt(((nx - 1) * sx^2 + (ny - 1) * sy^2) / den)
    if (!is.finite(psd) || psd == 0) return(NA_real_) else psd
  }
  
  # --- var-izveÅ¡taj po biclasteru (samo kolone koje ulaze u BC) ---
  top_rows <- list()
  narr_vec <- character(0)
  
  for (k in seq_len(K)) {
    r_in  <- which(res@RowxNumber[, k])
    c_in  <- which(res@NumberxCol[k, ])
    if (!length(r_in) || !length(c_in)) {
      narr_vec[k] <- sprintf("BC-%d: prazan (|I|=%d, |J|=%d).", k, length(r_in), length(c_in))
      next
    }
    r_out <- setdiff(seq_len(n), r_in)
    
    mean_in  <- vapply(c_in, function(j) mean(X[r_in,  j], na.rm = TRUE), numeric(1))
    mean_out <- vapply(c_in, function(j) mean(X[r_out, j], na.rm = TRUE), numeric(1))
    delta    <- mean_in - mean_out
    pct_delta <- ifelse(mean_out == 0, NA_real_, 100 * delta / abs(mean_out))
    d_eff <- vapply(c_in, function(j) {
      psd <- pooled_sd(X[r_in, j], X[r_out, j])
      if (is.na(psd)) NA_real_ else (mean(X[r_in, j], na.rm = TRUE) -
                                       mean(X[r_out, j], na.rm = TRUE)) / psd
    }, numeric(1))
    
    df <- data.frame(
      bicluster = k,
      variable  = if (is.null(colnames(X))) paste0("V", c_in) else colnames(X)[c_in],
      mean_in   = unname(mean_in),
      mean_out  = unname(mean_out),
      delta     = unname(delta),
      pct_delta = unname(pct_delta),
      cohen_d   = unname(d_eff),
      stringsAsFactors = FALSE
    )
    df <- df[order(-abs(df$delta)), , drop = FALSE]
    top_rows[[length(top_rows) + 1L]] <- head(df, top_n)
    
    # kratki narativ za BC-k (Top-3 sa smerom â/â)
    top3 <- head(df, 3)
    arrows <- ifelse(top3$delta >= 0, "\u2191", "\u2193")  # â / â
    items <- sprintf("%s (%s %.2f)", top3$variable, arrows, abs(top3$delta))
    narr_vec[k] <- sprintf("BC-%d: |I|=%d, |J|=%d, coverage=%.1f%%; Top: %s",
                           k, length(r_in), length(c_in), 100 * sizes$coverage[k],
                           paste(items, collapse = "; "))
  }
  
  top_vars <- if (length(top_rows)) do.call(rbind, top_rows) else
    data.frame(bicluster=integer(0), variable=character(0),
               mean_in=numeric(0), mean_out=numeric(0), delta=numeric(0),
               pct_delta=numeric(0), cohen_d=numeric(0))
  list(sizes = sizes, top_vars = top_vars, narrative = narr_vec)
}

## ========================== PRIMENA ==========================
# Podesi inpute:
X   <- GFSI2021.use       # ili GFSI2021.use.matrix
res <- resBCCC

rep <- bc_report(X, res, top_n = 5)

# 1) VeliÄine i coverage po BC:
rep$sizes
#    bicluster n_rows n_cols  coverage
# 1          1     ..     ..   0.0...
# ...

# 2) Top-5 po |delta| za svaku kolonu koja ulazi u odgovarajuÄi BC:
rep$top_vars
#   bicluster variable mean_in mean_out   delta pct_delta cohen_d
# 1         1    X1.5   98.72    58.11  40.61     69.90    1.85
# ...

# 3) Kratak narativ (jedna reÄenica po BC):
# zapravo je to neka vrsta profilisanja....
rep$narrative
# "BC-1: |I|=..., |J|=..., coverage=..%; Top: X1.5 (â 40.61); X1.2 (â 37.68); X1.6 (â 35.68)"





################################################################################

#                 E         O         F                                        #

################################################################################
