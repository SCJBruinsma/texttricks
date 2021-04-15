#' An MSA Confirm Function
#'
#' This function allows you to run a confirmatory MSA
#' @param data .csv file with variables in columns and observations in rows
#' @param scale a character vector specifying the variables that should be included
#' @keywords msa
#' @export
#' @examples
#' msa_confirm()

### Confirmatory MSA ###

msa_confirm <- function(data, scale){

  # Load libraries

  library(mokken)
  library(poLCA)
  library(semTools)

  # Prepare Data

  data <- data[scale]
  data <- na.omit(data)
  data_mat <- as.matrix(data)

  # Run Mokken Scaling

  data_coefH <- coefH(data_mat, se = FALSE, nice.output = TRUE, group.var = NULL)

  data_coefH_hi <- as.data.frame(data_coefH$Hi)
  colnames(data_coefH_hi) <- "H-Value"
  data_coefH_h <- as.data.frame(data_coefH$H)
  colnames(data_coefH_h) <- "H-Value"
  rownames(data_coefH_h) <- "Total H"

  # Monotonicity

  data_monoton <- check.monotonicity(data_mat, minsize=100)
  data_monoton_values <- as.data.frame(summary(data_monoton))$crit
  data_monoton_values <- append(data_monoton_values, NA)

  data_msa <- rbind(data_coefH_hi,data_coefH_h)
  data_msa <- cbind(data_msa,data_monoton_values)
  colnames(data_msa) <- c("H-Value","crit-value")
  data_msa$`H-Value` <- round(data_msa$`H-Value`, digits = 2)

  # Check LCRC and Cronbach a

  vars <- colnames(data)

  f_1 <- paste0(vars,collapse=",")
  f_1 <- paste0("cbind(",f_1,") ~ 1")
  f_1 <- as.formula(f_1)

  min_bic <- 100000000
  classes <- 0

  for(i in 1:ncol(data)){
    lc <- poLCA(f_1, data, nclass=i, maxiter=5000,nrep=5)
    if(lc$bic < min_bic){
      min_bic <- lc$bic
      classes <- i
    }
  }

  LCRC <- as.data.frame(check.reliability(data, LCRC = TRUE, nclass = classes)$LCRC)
  alpha <- as.data.frame(check.reliability(data, LCRC = TRUE, nclass = classes)$alpha)
  LCRC$new <- NA
  alpha$new <- NA
  colnames(LCRC) <- c("H-Value","crit-value")
  colnames(alpha) <- c("H-Value","crit-value")
  rownames(LCRC) <- "LCRC"
  rownames(alpha) <- "Alpha"
  LCRC$`H-Value`[1] <- paste0(round(LCRC$`H-Value`, digits = 2)," (", classes,")")
  alpha$`H-Value`[1] <- round(alpha$`H-Value`, digits = 2)

  data_msa <- rbind(data_msa,alpha,LCRC)

  # Ordinal Cronbach a and ordinal omega

  f_scale_ord <- paste0(vars,collapse=" + ")
  f_scale_ord <- paste0("scale_ord =~" , f_scale_ord, "\n ")

  cfa.exante.fit <- cfa(f_scale_ord, data=data, std.lv=TRUE, ordered=colnames(data))
  omega_alpha <- as.data.frame(reliability(cfa.exante.fit))

  # Merge together

  alpha_ord <- as.data.frame(omega_alpha$scale_ord[1])
  omega_ord <- as.data.frame(omega_alpha$scale_ord[4])
  alpha_ord$new <- NA
  omega_ord$new <- NA
  colnames(alpha_ord) <- c("H-Value","crit-value")
  colnames(omega_ord) <- c("H-Value","crit-value")
  rownames(alpha_ord) <- "Alpha (Ord)"
  rownames(omega_ord) <- "Omega (Ord)"
  alpha_ord$`H-Value`[1] <- round(alpha_ord$`H-Value`, digits = 2)
  omega_ord$`H-Value`[1] <- round(omega_ord$`H-Value`, digits = 2)

  results <- rbind(data_msa,alpha_ord,omega_ord)
  type <- rownames(results)
  rownames(results) <- c()

  out <- cbind(type,results)

  # Make tables

  type <- c("v1","v2","v3","v4","v5","v6","v7","v8","v9","v10","v11","v20","v21","v22","v23","v24","v25","v26","v27","v28","v29","Total H","Alpha","LCRC","Alpha (Ord)","Omega (Ord)")
  type <- data.frame(type)

  out <- merge(type, out, by="type", all="true")
  out <- out[match(type$type, out$type),]
  row.names(out) <- rep(1:26)
  results <- out

  return(results)

}
