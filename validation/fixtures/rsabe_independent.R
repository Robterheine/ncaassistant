# Independent implementation of the FDA RSABE assessment (Appendix G, May 2026)
# for validation section RSA. Deliberately unlike R/be_scaled.R: no lm(), no
# tapply(); one long function on explicit matrices and normal equations.
# Input: a data.frame with subject, period, sequence, treatment ("R"/"T"), y
# (natural log of the metric). Returns a named numeric vector.
rsabe_independent <- function(d) {
  ids <- unique(d$subject); rowsI <- list()
  nper <- max(table(d$subject[!duplicated(paste(d$subject, d$period))]))
  for (id in ids) {
    z <- d[d$subject == id, ]; z <- z[order(z$period), ]
    if (nrow(z) < nper || any(is.na(z$y))) next
    Ri <- which(z$treatment == "R"); Ti <- which(z$treatment == "T")
    rowsI[[length(rowsI) + 1]] <- c(seq = match(z$sequence[1], sort(unique(d$sequence))),
                                    I = sum(z$y[Ti]) / length(Ti) - sum(z$y[Ri]) / length(Ri),
                                    D = z$y[Ri[1]] - z$y[Ri[2]])
  }
  M <- do.call(rbind, rowsI); n <- nrow(M)
  used <- sort(unique(M[, "seq"])); m <- length(used)
  X <- matrix(0, n, m); X[cbind(seq_len(n), match(M[, "seq"], used))] <- 1
  XtXi <- solve(t(X) %*% X)
  bI <- XtXi %*% t(X) %*% M[, "I"]; bD <- XtXi %*% t(X) %*% M[, "D"]
  sseI <- sum((M[, "I"] - X %*% bI)^2); sseD <- sum((M[, "D"] - X %*% bD)^2)
  w <- rep(1 / m, m)
  est <- sum(w * bI)
  se <- sqrt(sseI / (n - m) * drop(t(w) %*% XtXi %*% w))     # Var(w'b) = s2 w'(X'X)^-1 w
  half <- qt(0.95, n - m) * se
  x <- est^2 - se^2; bx <- max((est - half)^2, (est + half)^2)
  s2wr <- sseD / (n - m) / 2
  y <- -(log(1.25) / 0.25)^2 * s2wr; by <- y * (n - m) / qchisq(0.95, n - m)
  c(n = n, pe = 100 * exp(est), lcl = 100 * exp(est - half), ucl = 100 * exp(est + half), swr = sqrt(s2wr),
    dfd = n - m, x = x, boundx = bx, y = y, boundy = by, critbound = x + y + sqrt((bx - x)^2 + (by - y)^2))
}
