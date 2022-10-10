## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  install.packages("missoNet")

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  if(!require("devtools")) {
#    install.packages("devtools")
#  }
#  devtools::install_github("yixiao-zeng/missoNet")

## -----------------------------------------------------------------------------
library(missoNet)

## -----------------------------------------------------------------------------
## Specify a random seed for reproducibility.
## The overall missing rate in the response matrix is around 10%.
## The missing mechanism can also be "MCAR" or "MNAR".
sim.dat <- generateData(n = 150, p = 15, q = 12, rho = 0.1, missing.type = "MAR", with.seed = 1512)

tr <- 1:120  # training set indices
tst <- 121:150  # test set indices

## ---- eval = FALSE------------------------------------------------------------
#  Z <- sim.dat$Z  # corrupted response matrix
#  visdat::vis_miss(as.data.frame(Z))

## ---- include = FALSE, eval = FALSE-------------------------------------------
#  png(file = "vismis.png", width = 1900, height = 1300, res = 260)
#  suppressWarnings(visdat::vis_miss(as.data.frame(sim.dat$Z)))
#  dev.off()

## ---- echo = FALSE, fig.align = 'left', out.width = "80%"---------------------
knitr::include_graphics(system.file("extdata", "vismis.png", package = "missoNet"))

## -----------------------------------------------------------------------------
## Training set
X.tr <- sim.dat$X[tr, ]  # predictor matrix
Z.tr <- sim.dat$Z[tr, ]  # corrupted response matrix

## Using the training set to fit the model.
## 'lambda.Beta' and 'lambda.Theta' are arbitrarily set to 0.1.
## 'verbose' = 0 suppresses printing of messages.
fit <- missoNet(X = X.tr, Y = Z.tr, lambda.Beta = 0.1, lambda.Theta = 0.1, verbose = 0)

## -----------------------------------------------------------------------------
lambda.Beta.vec <- 10^(seq(from = 0, to = -1, length.out = 10))  # 10 values on the log scale, from 1 to 0.1
lambda.Theta.vec <- rep(0.1, 10)  # for each value of 'lambda.Beta', 'lambda.Theta' remains constant at 0.1
fit_list <- missoNet(X = X.tr, Y = Z.tr, lambda.Beta = lambda.Beta.vec, lambda.Theta = lambda.Theta.vec, verbose = 0)

## -----------------------------------------------------------------------------
## If 'permute' = FALSE, the samples will be split into k-folds in their original orders,
## i.e. the first (n/'kfold') samples will be assigned to the first fold an so on.

cvfit <- cv.missoNet(X = X.tr, Y = Z.tr, kfold = 5,
                     lambda.Beta = lambda.Beta.vec, lambda.Theta = lambda.Theta.vec,
                     permute = TRUE, with.seed = 433, verbose = 0)

## ---- eval = FALSE------------------------------------------------------------
#  ## 'fit.1se = TRUE' tells the program to make additional estimations of the
#  ## parameters at the largest value of 'lambda.Beta' / 'lambda.Theta' that gives
#  ## the most regularized model such that the cross-validated error is within one
#  ## standard error of the minimum.
#  
#  cl <- parallel::makeCluster(min(parallel::detectCores()-1, 2))
#  cvfit <- cv.missoNet(X = X.tr, Y = Z.tr, kfold = 5,
#                       fit.1se = TRUE, parallel = TRUE, cl = cl,
#                       permute = TRUE, with.seed = 433, verbose = 1)
#  parallel::stopCluster(cl)

## ---- eval = FALSE------------------------------------------------------------
#  ## The values of mean cross-validated errors along with upper and lower standard deviation bounds
#  ## can be accessed via 'cvfit$cvm', 'cvfit$cvup' and 'cvfit$cvlo', respectively.
#  
#  plot(cvfit)

## ---- echo = FALSE, fig.align = 'left', out.width = "80%"---------------------
knitr::include_graphics(system.file("extdata", "cvfitHeat.png", package = "missoNet"))

## ---- eval = FALSE------------------------------------------------------------
#  plot(cvfit, type = "cv.scatter", plt.surf = FALSE)

## ---- echo = FALSE, fig.align = 'left', out.width = "80%"---------------------
knitr::include_graphics(system.file("extdata", "cvfitScat.png", package = "missoNet"))

## ---- eval = FALSE------------------------------------------------------------
#  ## Define a plotting function
#  plot_heatmap <- function(est, col, legend = FALSE, lgd_name, title) {
#    return(ComplexHeatmap::Heatmap(est, cluster_rows = FALSE, cluster_columns = FALSE,
#                                   col = col, show_heatmap_legend = legend, name = lgd_name,
#                                   column_names_gp = grid::gpar(fontsize = 8),
#                                   row_names_gp = grid::gpar(fontsize = 8), row_names_side = "left",
#                                   border = TRUE, column_title = title))
#  }
#  
#  ## Color space
#  col <- circlize::colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))
#  
#  ## Beta*
#  Beta_star <- sim.dat$Beta
#  Beta_star_ht <- plot_heatmap(Beta_star, col, title = expression(paste(bold(Beta), "*")))
#  
#  ## Beta_hat at "lambda.min"
#  Beta_hat_min <- cvfit$est.min$Beta
#  Beta_hat_min_ht <- plot_heatmap(Beta_hat_min, col, title = expression(paste(hat(bold(Beta)), " at `lambda.min`")))
#  
#  ## Beta_hat at "lambda.1se.Beta"
#  Beta_hat_1se <- cvfit$est.1se.B$Beta
#  Beta_hat_1se_ht <- plot_heatmap(Beta_hat_1se, col, legend = TRUE, lgd_name = "values",
#                                  title = expression(paste(hat(bold(Beta)), " at `lambda.Beta.1se`")))
#  
#  ## Theta*
#  Theta_star <- sim.dat$Theta
#  Theta_star_ht <- plot_heatmap(Theta_star, col, title = expression(paste(bold(Theta), "*")))
#  
#  ## Theta_hat at "lambda.min"
#  Theta_hat_min <- cvfit$est.min$Theta
#  Theta_hat_min_ht <- plot_heatmap(Theta_hat_min, col, title = expression(paste(hat(bold(Theta)), " at `lambda.min`")))
#  
#  ## Theta_hat at "lambda.1se.Beta"
#  Theta_hat_1se <- cvfit$est.1se.B$Theta
#  Theta_hat_1se_ht <- plot_heatmap(Theta_hat_1se, col, legend = TRUE, lgd_name = "values",
#                                   title = expression(paste(hat(bold(Theta)), " at `lambda.Beta.1se`")))
#  
#  ## Plot
#  Beta_star_ht + Beta_hat_min_ht + Beta_hat_1se_ht
#  Theta_star_ht + Theta_hat_min_ht + Theta_hat_1se_ht

## ---- echo = FALSE, fig.align = 'left', out.width = "100%"--------------------
knitr::include_graphics(system.file("extdata", "cvfitEst1.png", package = "missoNet"))

## ---- echo = FALSE, fig.align = 'left', out.width = "100%"--------------------
knitr::include_graphics(system.file("extdata", "cvfitEst2.png", package = "missoNet"))

## ---- eval = FALSE------------------------------------------------------------
#  ## Predictions on the test set: newy = mu + newx %*% Beta.
#  
#  ## 's' can also be "lambda.1se.Beta" or "lambda.1se.Theta"
#  ## (why 's' but not 'lambda'? Because we follow the naming rules of glmnet).
#  
#  newy <- predict(cvfit, newx = sim.dat$X[tst, ], s = "lambda.min")
#  
#  cat("dim(newy):", dim(newy))

## ---- echo = FALSE------------------------------------------------------------
cat("dim(newy): 30 12")

## ---- eval = FALSE------------------------------------------------------------
#  ## Load data
#  library(bgsmtr, quietly = TRUE)
#  data(bgsmtr_example_data)
#  
#  ## Transpose data matrix to make rows correspond to
#  ## samples and columns correspond to variables.
#  SNP <- t(bgsmtr_example_data$SNP_data)  # predictor matrix
#  BM <- t(bgsmtr_example_data$BrainMeasures)  # complete response matrix
#  
#  ## Unsupervised filtering of the top 50% variables.
#  SNP_var <- apply(SNP, 2, var)
#  SNP_subset <- SNP_var > quantile(SNP_var, 0.5)
#  SNP <- SNP[ ,SNP_subset]
#  
#  set.seed(123)  # a random seed for reproducibility
#  tr <- sample(1:632, 550, replace = FALSE)  # training set indices
#  tst <- c(1:632)[-tr]  # test set indices
#  
#  cat("dim(SNP):", dim(SNP[tr, ]), "
#  dim(BM):", dim(BM[tr, ]))

## ---- echo = FALSE------------------------------------------------------------
cat("dim(SNP): 550 243
dim(BM): 550 15")

## ---- eval = FALSE------------------------------------------------------------
#  ## Generate the indicator matrix of missingness.
#  M <- matrix(1, nrow(BM), ncol(BM))
#  NA.pos <- do.call("cbind", lapply(1:ncol(BM), function(x) {
#    rbinom(nrow(BM), size = 1, prob = 0.05) == 1
#  }))
#  ## All missing values should be coded as 'NA's or 'NaN's.
#  M[NA.pos] <- NA
#  BM.mis = BM * M
#  
#  cat(BM.mis[1:6, 5:8])

## ---- echo = FALSE------------------------------------------------------------
cat("    Left_InfLatVent.adj Left_LatVent.adj Left_EntCtx.adj Left_Fusiform.adj
V1           -473.42061       -4595.2672      0.60678817                NA
V2            100.12989       -9163.7594      0.29868885       0.196951758
V3            373.36131      -10659.8137      0.18835349       0.105863582
V4            876.03547               NA     -1.19832801      -0.233082858
V5           -887.40929       -6789.0606      0.09203347       0.098447465
V6            354.12157               NA     -0.32757347       0.310694508")

## ---- eval = FALSE------------------------------------------------------------
#  ## Model I (the coarse-grid fit)
#  
#  ## To be more in line with real-world usage, the model is trained without specified
#  ## missing probabilities for the response variables ('rho' = NULL) and lambda sequences
#  ## for the lasso penalties ('lambda.Beta' = NULL, 'lambda.Theta' = NULL), in which case
#  ## the program will automatically compute and use reasonable values.
#  
#  cl <- parallel::makeCluster(min(parallel::detectCores()-1, 2))
#  cvfit.BM <- cv.missoNet(X = SNP[tr, ], Y = BM.mis[tr, ], kfold = 5,
#                          rho = NULL, lambda.Beta = NULL, lambda.Theta = NULL,
#                          lamBeta.min.ratio = 1e-4, lamTheta.min.ratio = 1e-4,
#                          n.lamBeta = 20, n.lamTheta = 20,
#                          lamBeta.scale.factor = 3, lamTheta.scale.factor = 3,
#                          standardize = TRUE, standardize.response = TRUE,
#                          permute = TRUE, with.seed = 433,
#                          parallel = TRUE, cl = cl, verbose = 1)
#  parallel::stopCluster(cl)

## ---- eval = FALSE------------------------------------------------------------
#  plot(cvfit.BM)

## ---- echo = FALSE, fig.align = 'left', out.width = "80%"---------------------
knitr::include_graphics(system.file("extdata", "cvfitBM.png", package = "missoNet"))

## ---- eval = FALSE------------------------------------------------------------
#  ## Model II (the fine-grid fit)
#  
#  ## For brevity, arguments taking the defaults are not specified explicitly.
#  cl <- parallel::makeCluster(min(parallel::detectCores()-1, 2))
#  cvfit2.BM <- cv.missoNet(X = SNP[tr, ], Y = BM.mis[tr, ], kfold = 5,
#                           lamBeta.min.ratio = 0.01, lamTheta.min.ratio = 0.01,
#                           n.lamBeta = 40, n.lamTheta = 40,
#                           lamBeta.scale.factor = 3, lamTheta.scale.factor = 0.3,
#                           fit.relax = TRUE, permute = TRUE, with.seed = 433,
#                           parallel = TRUE, cl = cl, verbose = 1)
#  parallel::stopCluster(cl)
#  
#  ## Certainly, users can specify the lambda values (linear on the log scale) by themselves
#  ## for the cross-validation search rather than use the control arguments. The following
#  ## commands should return almost the same results as the above ones (subtle differences
#  ## come from rounding the float numbers).
#  
#  # cl <- parallel::makeCluster(min(parallel::detectCores()-1, 3))
#  # cvfit2.BM <- cv.missoNet(X = SNP[tr, ], Y = BM.mis[tr, ], kfold = 5,
#  #                          lambda.Beta = 10^(seq(from = log10(2.66), to = log10(2.66*0.01), length.out = 40)),
#  #                          lambda.Theta = 10^(seq(from = log10(2.99), to = log10(2.99*0.01), length.out = 40)),
#  #                          fit.relax = TRUE, permute = TRUE, with.seed = 433,
#  #                          parallel = TRUE, cl = cl, verbose = 0)
#  # parallel::stopCluster(cl)

## ---- eval = FALSE------------------------------------------------------------
#  cat("\"lambda.min\":
#    - lambda.Beta:", cvfit2.BM$est.min$lambda.Beta, "
#    - lambda.Theta:", cvfit2.BM$est.min$lambda.Theta)
#  
#  plot(cvfit2.BM, detailed.axes = FALSE)

## ---- echo = FALSE------------------------------------------------------------
cat("\"lambda.min\":
  - lambda.Beta: 0.198283
  - lambda.Theta: 0.573265")

## ---- echo = FALSE, fig.align = 'left', out.width = "80%"---------------------
knitr::include_graphics(system.file("extdata", "cvfit2BM.png", package = "missoNet"))

## ---- eval = FALSE------------------------------------------------------------
#  ## Theta_hat at "lambda.min"
#  Theta_hat <- cvfit2.BM$est.min$Theta
#  rownames(Theta_hat) <- colnames(Theta_hat) <- colnames(BM)
#  col <- circlize::colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))
#  ht1 <- plot_heatmap(Theta_hat, col = col, legend = TRUE, lgd_name = "values",
#                      title = expression(paste(hat(bold(Theta)), " at `lambda.min` (unclustered)")))
#  plot(ht1)
#  
#  ## Beta_hat at "lambda.min"
#  Beta_hat <- cvfit2.BM$est.min$Beta
#  colnames(Beta_hat) <- colnames(BM)
#  col <- circlize::colorRamp2(c(-0.05, 0, 0.05), c("blue", "white", "red"))
#  ht2 <- plot_heatmap(Beta_hat, col = col, legend = TRUE, lgd_name = "values",
#                      title = expression(paste(hat(bold(Beta)), " at `lambda.min` (unclustered)")))
#  plot(ht2)
#  
#  ## Relaxed network
#  relax.net <- cvfit2.BM$est.min$relax.net
#  rownames(relax.net) <- colnames(relax.net) <- colnames(BM)
#  col <- circlize::colorRamp2(c(-4, 0, 4), c("blue", "white", "red"))
#  ht3 <- plot_heatmap(relax.net, col = col, legend = TRUE, lgd_name = "values",
#                      title = "Relaxed fit of network (unclustered)")
#  plot(ht3)
#  
#  ## Plot the partial correlation matrix of the 15 neuroimaging measures.
#  ## Must supply a dataset without missing values, so we use the original dataset here.
#  pcor.mat <- ppcor::pcor(BM)  # not BM.mis
#  corrplot::corrplot(pcor.mat$estimate, p.mat = pcor.mat$p.value,
#                     sig.level = 0.01, insig = "blank",
#                     tl.cex = 0.5, tl.col = "black", mar = c(.5, .1, 2, .1),
#                     title = "Partial correlation matrix (unclustered)")

## ---- echo = FALSE, fig.align = 'left', out.width = "100%"--------------------
# knitr::include_graphics(paste("vignetteFigs", "BMcomb.png", sep = .Platform$file.sep))
knitr::include_graphics(system.file("extdata", "BMcomb.png", package = "missoNet"))

## ---- eval = FALSE------------------------------------------------------------
#  colnames(Beta_hat) <- colnames(BM)  # brain measure names
#  rownames(Beta_hat) <- colnames(SNP)  # SNP names
#  Gene_groups <- bgsmtr_example_data$SNP_groups[SNP_subset]  # gene names
#  
#  ComplexHeatmap::pheatmap(Beta_hat,
#                           annotation_row = data.frame("Gene" = Gene_groups, row.names = rownames(Beta_hat)),
#                           color = circlize::colorRamp2(c(-0.05,0,0.05), c("blue","white","red")), show_rownames = FALSE,
#                           border_color = NA, border = TRUE, name = "values", cluster_rows = TRUE, cluster_cols = FALSE,
#                           column_title = expression(paste(hat(bold(Beta)), " at `lambda.min` (clustered)")))

## ---- eval = FALSE, include = FALSE-------------------------------------------
#  set.seed(34)  # seed for legend colors
#  ComplexHeatmap::pheatmap(cvfit2.BM$est.min$Beta,
#                           annotation_row = data.frame("Gene" = cvfit2.BM$Gene, row.names = rownames(cvfit2.BM$est.min$Beta)),
#                           color = circlize::colorRamp2(c(-0.05,0,0.05), c("blue","white","red")), show_rownames = FALSE,
#                           border_color = NA, border = TRUE, name = "values", cluster_rows = TRUE, cluster_cols = FALSE,
#                           column_title = expression(paste(hat(bold(Beta)), " at `lambda.min` (clustered)")))

## ---- echo = FALSE, fig.align = 'left', out.width = "80%"---------------------
# knitr::include_graphics(paste("vignetteFigs", "BMcomb.png", sep = .Platform$file.sep))
knitr::include_graphics(system.file("extdata", "cvfit2BMclustB.png", package = "missoNet"))

## ---- eval = FALSE------------------------------------------------------------
#  ## Model III (the complete-data fit)
#  
#  ## All arguments are kept unchanged compared to model II, except BM.mis -> BM.
#  cl <- parallel::makeCluster(min(parallel::detectCores()-1, 2))
#  cvfit3.BM <- cv.missoNet(X = SNP[tr, ], Y = BM[tr, ], kfold = 5,
#                           lamBeta.min.ratio = 0.01, lamTheta.min.ratio = 0.01,
#                           n.lamBeta = 40, n.lamTheta = 40,
#                           lamBeta.scale.factor = 3, lamTheta.scale.factor = 0.3,
#                           fit.relax = TRUE, permute = TRUE, with.seed = 433,
#                           parallel = TRUE, cl = cl, verbose = 1)
#  parallel::stopCluster(cl)

## ---- eval = FALSE------------------------------------------------------------
#  ## Predictions on the test set.
#  newy.model2 <- predict(cvfit2.BM, newx = SNP[tst, ], s = "lambda.min")
#  newy.model3 <- predict(cvfit3.BM, newx = SNP[tst, ], s = "lambda.min")
#  
#  cat("MAE on the test set:
#    - Model II (corrupted):", mean(abs(BM[tst, ] - newy.model2)), "
#    - Model III (complete):", mean(abs(BM[tst, ] - newy.model3)))

## ---- echo = FALSE------------------------------------------------------------
cat("MAE on the test set:
  - Model II (corrupted): 2543.588
  - Model III (complete): 2543.591")

