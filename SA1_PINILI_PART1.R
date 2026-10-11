# ============================================================
# SA1 Item 1:
# ============================================================

repeat {
  cat("Enter the production proportions x1, x2, and x3:\n")
  
  x_input <- suppressWarnings(as.numeric(c(
    readline("x1 = "),
    readline("x2 = "),
    readline("x3 = ")
  )))
  
  if (
    length(x_input) == 3 &&
    all(is.finite(x_input)) &&
    all(x_input >= 0.10 & x_input <= 0.40) &&
    abs(sum(x_input) - 1) < 1e-8
  ) {
    x1 <- x_input[1]
    x2 <- x_input[2]
    x3 <- x_input[3]
    break
  }
  
  cat(
    "That Ain't valid input. Each x must be between 0.10 and 0.40 ",
    "and the values must sum to 1.\n\n",
    sep = ""
  )
}

repeat {
  cat("\nEnter the defective rates y1, y2, and y3:\n")
  
  y_input <- suppressWarnings(as.numeric(c(
    readline("y1 = "),
    readline("y2 = "),
    readline("y3 = ")
  )))
  
  if (
    length(y_input) == 3 &&
    all(is.finite(y_input)) &&
    all(y_input >= 0.01 & y_input <= 0.05) &&
    abs(sum(y_input) - 0.12) < 1e-8
  ) {
    y1 <- y_input[1]
    y2 <- y_input[2]
    y3 <- y_input[3]
    break
  }
  
  cat(
    "Not a valid input since each y must be between 0.01 and 0.05 ",
    "and the values must sum to 0.12.\n\n",
    sep = ""
  )
}

P_defective <- x1 * y1 + x2 * y2 + x3 * y3

cat(
  "\nProbability that a randomly selected product is defective:",
  sprintf("%.4f", P_defective),
  "\n"
)
cat(
  "Percentage defective:",
  sprintf("%.2f%%", P_defective * 100),
  "\n"
)


# ============================================================
# SA1 Item 2:
# ============================================================


Egg_Label <- "Univariate Values"
Sandwich_Label <- "Probability Values"
Obama_Label <- "Row Variable (X)"
Trump_Label <- "Column Variable (Y)"

# ------------------------------------------------------------
# UNIVARIATE DISCRETE RANDOM VARIABLE
# ------------------------------------------------------------

cat("\n--- Univariate Discrete Random Variable Analysis ---\n")

repeat {
  
  Egg_text <- trimws(
    readline("Enter X values (separated by spaces): ")
  )
  
  Sandwich_text <- trimws(
    readline("Enter corresponding probabilities P(X): ")
  )
  
  Egg_values <- suppressWarnings(
    as.numeric(strsplit(Egg_text, "\\s+")[[1]])
  )
  
  Sandwich_probabilities <- suppressWarnings(
    as.numeric(strsplit(Sandwich_text, "\\s+")[[1]])
  )
  
  valid_input <-
    length(Egg_values) > 0 &&
    length(Egg_values) == length(Sandwich_probabilities) &&
    all(is.finite(Egg_values)) &&
    all(is.finite(Sandwich_probabilities)) &&
    all(Sandwich_probabilities >= 0 &
          Sandwich_probabilities <= 1) &&
    abs(sum(Sandwich_probabilities) - 1) < 1e-8
  
  if (valid_input) {
    
    
    Egg_combined_probabilities <- tapply(
      Sandwich_probabilities,
      Egg_values,
      sum
    )
    
    Egg_values <- as.numeric(
      names(Egg_combined_probabilities)
    )
    
    Sandwich_probabilities <- as.numeric(
      Egg_combined_probabilities
    )
    
    Egg_order <- order(Egg_values)
    
    Egg_values <- Egg_values[Egg_order]
    
    Sandwich_probabilities <-
      Sandwich_probabilities[Egg_order]
    
    break
  }
  
  cat(
    "Invalid input. You have to enter matching numeric values and probabilities ",
    "between 0 and 1 that sum to 1.\n\n",
    sep = ""
  )
}

mean_x <- sum(
  Egg_values * Sandwich_probabilities
)

var_x <- sum(
  (Egg_values - mean_x)^2 *
    Sandwich_probabilities
)

cat("\nUnivariate Results:\n")
cat("Mean E[X] =", round(mean_x, 4), "\n")
cat("Variance Var(X) =", round(var_x, 4), "\n")

par(mfrow = c(1, 2))

plot(
  Egg_values,
  Sandwich_probabilities,
  type = "h",
  lwd = 3,
  col = "blue",
  main = "PMF of X",
  xlab = Egg_Label,
  ylab = Sandwich_Label
)

points(
  Egg_values,
  Sandwich_probabilities,
  pch = 16,
  col = "blue"
)

plot(
  stepfun(
    Egg_values,
    c(0, cumsum(Sandwich_probabilities))
  ),
  col = "red",
  lwd = 2,
  main = "CDF of X",
  xlab = Egg_Label,
  ylab = "F(x)"
)

par(mfrow = c(1, 1))

# ------------------------------------------------------------
# BIVARIATE DISCRETE RANDOM VARIABLE
# ------------------------------------------------------------

cat("\n--- Bivariate Discrete Random Variable Analysis ---\n")

repeat {
  
  Obama_rows_text <- readline(
    "Enter number of X levels (rows): "
  )
  
  Trump_columns_text <- readline(
    "Enter number of Y levels (columns): "
  )
  
  Obama_rows_num <- suppressWarnings(
    as.numeric(Obama_rows_text)
  )
  
  Trump_columns_num <- suppressWarnings(
    as.numeric(Trump_columns_text)
  )
  
  valid_dimensions <-
    length(Obama_rows_num) == 1 &&
    length(Trump_columns_num) == 1 &&
    is.finite(Obama_rows_num) &&
    is.finite(Trump_columns_num) &&
    Obama_rows_num >= 1 &&
    Trump_columns_num >= 1 &&
    Obama_rows_num == floor(Obama_rows_num) &&
    Trump_columns_num == floor(Trump_columns_num)
  
  if (!valid_dimensions) {
    
    cat(
      "Rows and columns must be positive whole numbers.\n\n"
    )
    
    next
  }
  
  nr <- as.integer(Obama_rows_num)
  nc <- as.integer(Trump_columns_num)
  
  x_levels_text <- trimws(
    readline("Enter the X levels (separated by space)s: ")
  )
  
  y_levels_text <- trimws(
    readline("Enter the Y levels (separated by spaces): ")
  )
  
  x_levels <- suppressWarnings(
    as.numeric(strsplit(x_levels_text, "\\s+")[[1]])
  )
  
  y_levels <- suppressWarnings(
    as.numeric(strsplit(y_levels_text, "\\s+")[[1]])
  )
  
  valid_levels <-
    length(x_levels) == nr &&
    length(y_levels) == nc &&
    all(is.finite(x_levels)) &&
    all(is.finite(y_levels)) &&
    !anyDuplicated(x_levels) &&
    !anyDuplicated(y_levels)
  
  if (!valid_levels) {
    
    cat(
      "Enter exactly",
      nr,
      "unique numeric X levels and",
      nc,
      "unique numeric Y levels.\n\n"
    )
    
    next
  }
  
  cat("Enter joint probabilities row by row:\n")
  
  ObamaTrump_matrix <- matrix(
    NA_real_,
    nrow = nr,
    ncol = nc
  )
  
  valid_rows <- TRUE
  
  for (i in seq_len(nr)) {
    
    Egg_row_text <- trimws(
      readline(paste("Row", i, ": "))
    )
    
    Sandwich_row_input <- suppressWarnings(
      as.numeric(
        strsplit(Egg_row_text, "\\s+")[[1]]
      )
    )
    
    if (
      length(Sandwich_row_input) != nc ||
      !all(is.finite(Sandwich_row_input))
    ) {
      
      valid_rows <- FALSE
      break
    }
    
    ObamaTrump_matrix[i, ] <- Sandwich_row_input
  }
  
  if (!valid_rows) {
    
    cat(
      "Invalid row. Just enter exactly",
      nc,
      "finite numeric probabilities per row.\n\n"
    )
    
    next
  }
  
  valid_joint_pmf <-
    all(
      ObamaTrump_matrix >= 0 &
        ObamaTrump_matrix <= 1
    ) &&
    abs(sum(ObamaTrump_matrix) - 1) < 1e-8
  
  if (valid_joint_pmf) {
    break
  }
  
  cat(
    "Joint probabilities must be between 0 and 1 ",
    "and sum to 1.\n\n",
    sep = ""
  )
}

marginal_x <- rowSums(ObamaTrump_matrix)
marginal_y <- colSums(ObamaTrump_matrix)

cond_y_given_x <- matrix(
  NA_real_,
  nrow = nr,
  ncol = nc
)

for (i in seq_len(nr)) {
  
  if (marginal_x[i] > 0) {
    
    cond_y_given_x[i, ] <-
      ObamaTrump_matrix[i, ] /
      marginal_x[i]
  }
}

cond_x_given_y <- matrix(
  NA_real_,
  nrow = nr,
  ncol = nc
)

for (j in seq_len(nc)) {
  
  if (marginal_y[j] > 0) {
    
    cond_x_given_y[, j] <-
      ObamaTrump_matrix[, j] /
      marginal_y[j]
  }
}

EX <- sum(x_levels * marginal_x)
EY <- sum(y_levels * marginal_y)

VarX <- sum(
  (x_levels - EX)^2 *
    marginal_x
)

VarY <- sum(
  (y_levels - EY)^2 *
    marginal_y
)

cat("\nBivariate Results:\n")
cat("Mean of X =", round(EX, 4), "\n")
cat("Variance of X =", round(VarX, 4), "\n")
cat("Mean of Y =", round(EY, 4), "\n")
cat("Variance of Y =", round(VarY, 4), "\n")

cat("\nMarginal Distribution of X:\n")
print(setNames(marginal_x, x_levels))

cat("\nMarginal Distribution of Y:\n")
print(setNames(marginal_y, y_levels))

cat("\nConditional Distribution P(Y | X):\n")
print(round(cond_y_given_x, 4))

cat("\nConditional Distribution P(X | Y):\n")
print(round(cond_x_given_y, 4))

cat(
  "\nNote: Conditional distributions are undefined for ",
  "conditioning values with probability zero (shown as NA).\n",
  sep = ""
)

# ------------------------------------------------------------
# BIVARIATE PLOTS
# ------------------------------------------------------------

par(mfrow = c(2, 3))

image(
  x = y_levels,
  y = x_levels,
  z = t(ObamaTrump_matrix),
  main = "Joint PMF",
  xlab = Trump_Label,
  ylab = Obama_Label,
  col = heat.colors(20),
  axes = FALSE
)

axis(1, at = y_levels, labels = y_levels)
axis(2, at = x_levels, labels = x_levels)

box()

plot(
  x_levels,
  marginal_x,
  type = "h",
  lwd = 3,
  col = "blue",
  main = "Marginal PMF of X",
  xlab = Obama_Label,
  ylab = "P(X)"
)

points(
  x_levels,
  marginal_x,
  pch = 16,
  col = "blue"
)

plot(
  y_levels,
  marginal_y,
  type = "h",
  lwd = 3,
  col = "purple",
  main = "Marginal PMF of Y",
  xlab = Trump_Label,
  ylab = "P(Y)"
)

points(
  y_levels,
  marginal_y,
  pch = 16,
  col = "purple"
)

plot(
  stepfun(
    x_levels,
    c(0, cumsum(marginal_x))
  ),
  main = "CDF of X",
  xlab = Obama_Label,
  ylab = "F(X)",
  col = "red",
  lwd = 2
)

plot(
  stepfun(
    y_levels,
    c(0, cumsum(marginal_y))
  ),
  main = "CDF of Y",
  xlab = Trump_Label,
  ylab = "F(Y)",
  col = "darkgreen",
  lwd = 2
)

plot.new()
title("Conditional distributions has been printed in console")

par(mfrow = c(1, 1))

# ============================================================
# SA1 Item 3:
# ============================================================
cat("\n--- Geometric Simulation & Memoryless Property ---\n")

repeat {
  p_text <- readline(
    paste0(
      "Enter the probability p that any site contains the key phrase ",
      "(0 < p <= 1, default is 0.6 so enter is fine): "
    )
  )
  
  if (nchar(trimws(p_text)) == 0) {
    p <- 0.6
    break
  }
  
  p_num <- suppressWarnings(as.numeric(p_text))
  
  if (length(p_num) == 1 && is.finite(p_num) && p_num > 0 && p_num <= 1) {
    p <- p_num
    break
  }
  
  cat("Not a vali input. p must be greater than 0 and no greater than 1.\n\n")
}

cat(
  "Using probability p =",
  p,
  "for 10,000 simulations...\n\n"
)


# ------------------------------------------------------------
# Generating 10,000 geometric simulations
# ------------------------------------------------------------

set.seed(42)
n_sim <- 10000

simulated_searches <- rgeom(n_sim, prob = p) + 1


# ------------------------------------------------------------
# Complete Distribution Statistics
# ------------------------------------------------------------

mean_full <- mean(simulated_searches)
var_full <- var(simulated_searches)

cat("--- Complete Set Statistics ---\n")
cat(
  "Simulated Mean E[X] =",
  round(mean_full, 4),
  "(Theoretical:",
  round(1 / p, 4),
  ")\n"
)
cat(
  "Simulated Variance Var(X) =",
  round(var_full, 4),
  "(Theoretical:",
  round((1 - p) / p^2, 4),
  ")\n\n"
)


# ------------------------------------------------------------
# Conditional Distribution X > 3
# ------------------------------------------------------------

subset_cond <- simulated_searches[simulated_searches > 3]
shifted_cond <- subset_cond - 3

mean_cond <- mean(shifted_cond)
var_cond <- var(shifted_cond)

cat("--- Conditional Distribution (Given X > 3) ---\n")
cat(
  "Shifted Conditional Mean E[X - 3 | X > 3] =",
  round(mean_cond, 4),
  "\n"
)
cat(
  "Shifted Conditional Variance Var(X - 3 | X > 3) =",
  round(var_cond, 4),
  "\n"
)
cat(
  "Difference in Means =",
  round(abs(mean_full - mean_cond), 6),
  "\n"
)

cat(
  "Difference in Variances =",
  round(abs(var_full - var_cond), 6),
  "\n"
)

cat(
  "Smaller differences indicate agreement with the memoryless property.\n\n"
)


# ------------------------------------------------------------
# Memoryless Property Checks
# ------------------------------------------------------------

prob_x_gt_3 <- mean(simulated_searches > 3)

if (prob_x_gt_3 > 0) {
  p_4_given_3 <- mean(simulated_searches == 4) / prob_x_gt_3
  p_5_given_3 <- mean(simulated_searches == 5) / prob_x_gt_3
} else {
  p_4_given_3 <- NA_real_
  p_5_given_3 <- NA_real_
}

p_1 <- mean(simulated_searches == 1)
p_2 <- mean(simulated_searches == 2)

cat("--- Memoryless Property Check (a) ---\n")
cat("Estimate P(X = 4 | X > 3) =", round(p_4_given_3, 4), "\n")
cat("Estimate P(X = 1)         =", round(p_1, 4), "\n")
cat("Theoretical Value         =", round(p, 4), "\n\n")

cat("--- Memoryless Property Check (b) ---\n")
cat("Estimate P(X = 5 | X > 3) =", round(p_5_given_3, 4), "\n")
cat("Estimate P(X = 2)         =", round(p_2, 4), "\n")
cat("Theoretical Value         =", round((1 - p) * p, 4), "\n\n")


# ------------------------------------------------------------
# PMF and CDF Plots
# ------------------------------------------------------------

par(mfrow = c(2, 2))

# Complete PMF
counts_full <- table(simulated_searches) / n_sim
x_vals_full <- as.numeric(names(counts_full))
prob_full <- as.numeric(counts_full)

plot(
  x_vals_full,
  prob_full,
  type = "h",
  lwd = 3,
  col = "blue",
  main = "Simulated PMF (Complete Set)",
  xlab = "Searches (X)",
  ylab = "Probability"
)
points(x_vals_full, prob_full, pch = 16, col = "blue")

# Complete CDF
cdf_full <- cumsum(prob_full)

plot(
  stepfun(x_vals_full, c(0, cdf_full)),
  col = "red",
  lwd = 2,
  main = "Simulated CDF (Complete Set)",
  xlab = "Searches (X)",
  ylab = "F(X)"
)

# Conditional PMF of the shifted variable X - 3, given X > 3
counts_cond <- table(shifted_cond) / length(shifted_cond)
x_vals_cond <- as.numeric(names(counts_cond))
prob_cond <- as.numeric(counts_cond)

plot(
  x_vals_cond,
  prob_cond,
  type = "h",
  lwd = 3,
  col = "darkgreen",
  main = "Conditional PMF (X - 3 | X > 3)",
  xlab = "X - 3",
  ylab = "Probability"
)
points(x_vals_cond, prob_cond, pch = 16, col = "darkgreen")

# Conditional CDF
cdf_cond <- cumsum(prob_cond)

plot(
  stepfun(x_vals_cond, c(0, cdf_cond)),
  col = "purple",
  lwd = 2,
  main = "Conditional CDF (X - 3 | X > 3)",
  xlab = "X - 3",
  ylab = "F(X - 3)"
)

par(mfrow = c(1, 1))