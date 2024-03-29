update.missoNet <- function(X, Y, lamTh, lamB,
                            Beta.maxit, Beta.thr,
                            Theta.maxit, Theta.thr,
                            verbose, eps, eta,
                            penalize.diag, diag.pf,
                            info, info.update, under.cv,
                            init.obj=NULL, B.init=NULL) {
  if (is.null(info)) {
    n <- nrow(X)
    p <- ncol(X)
    q <- ncol(Y)
    
    X <- scale(X, center = init.obj$mx, scale = init.obj$sdx)
    Y <- scale(Y, center = init.obj$my, scale = init.obj$sdy)
    Z <- Y
    Z[is.na(Z)] <- 0
    
    rho.mat.1 <- t(matrix(rep(1 - init.obj$rho.vec, p), q, p))  # pxq
    rho.mat.2 <- matrix(1 - init.obj$rho.vec, q, 1) %*% matrix(1 - init.obj$rho.vec, 1, q)
    diag(rho.mat.2) <- 1 - init.obj$rho.vec  # qxq
    
    info$n <- n
    info$q <- q
    info$xtx <- crossprod(X)
    info$til.xty <- crossprod(X, Z)/rho.mat.1
    til.ytx <- t(info$til.xty)
    til.yty <- crossprod(Z)/rho.mat.2
    if (min(eigen(til.yty)$value) < eps) {
      til.yty <- maxproj.cov(mat = til.yty, epsilon = eps)
    }
    
    #####################################################
    # Pre-updating several times from a cold start
    #####################################################
    if (verbose == 2) {
      cat("  ---------------- Warming-up -----------------\n")
      cat("\tepoch\t|\t| lik(t + 1) - lik(t) |\n")
    }
    E <- Y - X %*% B.init
    residual.cov <- getResCov(E = E, n = n, rho.mat = rho.mat.2, eps = eps)
    # residual.cov <- getResCov(yty = til.yty, ytx = til.ytx, xty = info$til.xty, xtx = info$xtx, B = B.init, n = n, eps = eps)
    
    if (penalize.diag) {
      lamTh.mat <- lamTh * (1 - diag(info$q)) + lamTh * diag.pf * diag(info$q)
    } else { lamTh.mat <- lamTh * (1 - diag(info$q)) }
    lamB.mat <- matrix(lamB, nrow = p, ncol = q)
    
    lik.new <- sum(diag(1/n * (til.yty - til.ytx %*% B.init - crossprod(B.init, info$til.xty)
                               + crossprod(B.init, info$xtx) %*% B.init) %*% diag(1, q)))
    - determinant(diag(1, q), logarithm = TRUE)$mod[1] + sum(abs(lamTh.mat * diag(1, q))) + sum(abs(lamB.mat * B.init))
    lik.thr <- 1e-08
    lik.old <- lik.new + lik.thr + 1
    
    s <- 0
    while (s < 1000) {
      if (abs(lik.new - lik.old) < lik.thr) {
        if (verbose == 2) {
          cat("  ---------------------------------------------\n")
        }
        break
      } else {
        lik.old <- lik.new
        Theta.out <- glasso(s = residual.cov, rho = lamTh.mat, thr = Theta.thr, maxit = Theta.maxit,
                            approx = FALSE, penalize.diagonal = penalize.diag, trace = FALSE)
        Theta <- (Theta.out$wi + t(Theta.out$wi))/2

        B.out <- updateBeta(Theta = Theta, B0 = B.init, n = info$n, xtx = info$xtx, xty = info$til.xty,
                            lamB = lamB, eta = eta, tolin = Beta.thr, maxitrin = Beta.maxit)
        
        B.init <- B.out$Bhat
        E <- Y - X %*% B.init
        residual.cov <- getResCov(E = E, n = n, rho.mat = rho.mat.2, eps = eps)
        # residual.cov <- getResCov(yty = til.yty, ytx = til.ytx, xty = info$til.xty, xtx = info$xtx, B = B.init, n = n, eps = eps)
        
        lik.new <- sum(diag(1/n * (til.yty - til.ytx %*% B.init - crossprod(B.init, info$til.xty)
                                   + crossprod(B.init, info$xtx) %*% B.init) %*% Theta))
        - determinant(Theta, logarithm = TRUE)$mod[1] + sum(abs(lamTh.mat * Theta)) + sum(abs(lamB.mat * B.init))
        
        if (verbose == 2) {
          cat(sprintf("\t%d\t|\t%f\n", (s + 1), abs(lik.new - lik.old)))
        }
        s <- s + 1
      }
    }
    #####################################################
    # Pre-updating ends
    #####################################################
    info.update$B.init <- B.init
    info.update$residual.cov <- residual.cov
  }
  
  ################################################################################
  # Updating Theta and Beta
  ################################################################################
  if (penalize.diag) {
    lamTh.mat <- lamTh * (1 - diag(info$q)) + lamTh * diag.pf * diag(info$q)
    Theta.out <- glasso(s = info.update$residual.cov, rho = lamTh.mat, thr = Theta.thr, maxit = Theta.maxit,
                        approx = FALSE, penalize.diagonal = TRUE, trace = FALSE)
  } else {
    Theta.out <- glasso(s = info.update$residual.cov, rho = lamTh, thr = Theta.thr, maxit = Theta.maxit,
                        approx = FALSE, penalize.diagonal = FALSE, trace = FALSE)
  }
  Theta <- (Theta.out$wi + t(Theta.out$wi))/2
  
  B.out <- updateBeta(Theta = Theta, B0 = info.update$B.init, n = info$n, xtx = info$xtx, xty = info$til.xty,
                      lamB = lamB, eta = eta, tolin = Beta.thr, maxitrin = Beta.maxit)
  
  if (verbose == 2) {
    cat("  `lambda.Beta`:", lamB, "  `lambda.Theta`:", lamTh, "\n")
    cat("  # iters for updating `Beta` (prox-grad):", B.out$it.final, "\n")
    cat("  # iters for updating `Theta` (glasso):", Theta.out$niter, "\n\n")
  }
  
  if (under.cv) {
    return(B.out$Bhat)
  } else {
    return(list(Beta = B.out$Bhat, Theta = Theta))
  }
}

