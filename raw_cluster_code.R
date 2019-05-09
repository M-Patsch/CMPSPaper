
m <- dim(table(RgdMmFhCorrected$cowccode))
u.clust <- matrix(NA, nrow = m, ncol = length(Nbreg$coefficients))
fc <- factor(RgdMmFhCorrected$cowccode)
for (i in 1:ncol(u.clust)) {
  u.clust[, i] <- tapply(u[, i], fc, sum) 
}

cl.vcov <- vcov %*% ((m/(m - 1)) * t(u.clust) %*% (u.clust)) %*% vcov

cl.se <- sqrt(diag(cl.vcov))
z <- beta/cl.se
pval <- 2 * (1 - pnorm(abs(beta/cl.se)))
cl.tbl <- cbind(beta = round(beta, rnd), se = round(cl.se, rnd),
  z = round(z, 2), pval = round(pval, rnd))
colnames(cl.tbl) <- c("coef", "s.e.", "z", "P>|z|")
print("Results w/ Robust s.e. clustered by Dyad")