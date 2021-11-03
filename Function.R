
Group <- function(database, output = TRUE){
  names(database)[1] <- "group"
  for (k in 1:ncol(database)) {
    if(sum(!is.na(database[, k])) == 0)
      database[, k] <- as.logical(database[, k])
  }
  table_one <- tableby(group ~., data = database)
  table_one <- summary(table_one, text = T, na.rm=TRUE)
  table1 <- as.data.frame(table_one$object)
  data_type <- table1[table1[, 8] == "", 7]
  group_type <- unique(database$group)
  normal_P <- vector(mode = "numeric", length = length(group_type))
  var_level <- vector(mode = "numeric", length = length(group_type))
  sample_size <- vector(mode = "numeric", length = length(group_type))
  for (i in 1:length(data_type)) {
    if(data_type[i] == "numeric"){
      sums <- length(database[, i+1])
      for (l in 1:length(group_type)) {
        sub_data <- subset(database, database$group == group_type[l])
        var_level[l] <- sum(!is.na(unique(sub_data[, i+1])))
        sample_size[l] <- sum(!is.na(sub_data[, i+1]))
        if(var_level[l] >= 2 & sample_size[l] >= 3 & sample_size[l] <= 5000){
          normal_P[l] <- shapiro.test(sub_data[, i+1])$p.value
        }else{
          normal_P[l] <- 0
        }
      }
      if(all(normal_P > 0.05)){
        a <- "anova"
      }else
        a <- "kwt"
    }else{
      a <- "anova"
    }
    
    if(data_type[i] == "categorical"){
      table_cate <- tableby(group ~., data = database[, c(1,i+1)])
      table_cate <- summary(table_cate, text = T, na.rm=TRUE)
      table_cate <- as.data.frame(table_cate)
      if(sum(is.na(database[, c(i+1)])) == 0)
        table_cate <- table_cate[-1, -c(1,ncol(table_cate))] 
      else
        table_cate <- table_cate[-c(1,2), -c(1,ncol(table_cate))] 
      for (j in 1:ncol(table_cate)) {
        table_cate[, j] <- gsub("\\(.*\\)","",table_cate[, j])
        table_cate[, j] <- as.numeric(table_cate[, j])
      }
      table_cate[nrow(table_cate)+1,] <- colSums(table_cate[, 1:ncol(table_cate)])
      table_cate <- table_cate[, c(ncol(table_cate),1:(ncol(table_cate)-1))]
      
      if(nrow(table_cate) > 2){
        exp_fre <- matrix(nrow = nrow(table_cate)-1, ncol = ncol(table_cate)-1)
        for (k in 1:(nrow(table_cate)-1)) {
          for (j in 1:(ncol(table_cate)-1)) {
            exp_fre[k ,j] <- (table_cate[k, 1]*table_cate[nrow(table_cate), j+1])/table_cate[nrow(table_cate), 1]
          }
        }
        if((nrow(exp_fre) == 2 & ncol(exp_fre) == 2) & ((table_cate[nrow(table_cate),1] < 40 | any(exp_fre < 1) == TRUE) | (table_cate[nrow(table_cate),1] >= 40 & any(exp_fre < 5 & exp_fre >= 1) == TRUE))){  
          b <- "fe"
        }else if((nrow(exp_fre) > 2 | ncol(exp_fre) > 2) & (nrow(exp_fre) != 2 & ncol(exp_fre) != 2) & (any(exp_fre < 1) == TRUE | length(which(exp_fre < 5 & exp_fre >= 1)) > ((nrow(table_cate)-1)*(ncol(table_cate)-1))/5)){
          b <- "fe"
        }else{
          b <- "chisq"
        }
      }else{
        b <- "chisq"
      }
    }else{
      b <- "chisq"
    }
    
    my_controls <- tableby.control(
      test = T, total = T,
      numeric.test = a, cat.test = b,
      numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss2"),
      cat.stats = c("countpct", "Nmiss2"),
      stats.labels = list(meansd = "Mean (SD)", medianq1q3 = "Median (Q1, Q3)",
                          range = "Min - Max", Nmiss2 = "Missing"), digits = 2)
    table_two <- tableby(group ~.,control = my_controls, data = database[, c(1,i+1)])
    table_two <- summary(table_two, text = T, na.rm=TRUE)
    table2 <- as.data.frame(table_two$object)
    table_two <- as.data.frame(table_two)
    table_two[, c(1,ncol(table_two)+1)] <- table2[, c("group.label","group.test")]
    table_two[table_two$`p value` == "", ncol(table_two)] <- ""
    table_two$`Statistics` <- ""
    table_two$`95%CI` <- ""
    table_two <- table_two[,
                           c(1:(ncol(table_two)-4),ncol(table_two)-2,ncol(table_two),ncol(table_two)-1,ncol(table_two)-3)]
    
    if(data_type[i] == "categorical"){
      table_cate1 <- table_cate[-nrow(table_cate), -1]
      if(nrow(table_cate) > 2){  
        if(b == "chisq"){
          table_two$`Statistics`[1] <- paste("X-squared =", round(chisq.test(table_cate1, correct = F)$statistic, 2))
          table_two$`p value`[1] <- sprintf("%.3f", chisq.test(table_cate1, correct = F)$p.value)                 
        }else if(b == "fe"){
        }
      }else{
        table_two$`p value`[1] <- "-"
        table_two$`Statistics`[1] <- "-"
        table_two$group.test[1] <- "-"
      }
    }
    
    if(data_type[i] == "numeric"){ 
      normal_P <- round(normal_P, 3)
      for (m in 1:length(group_type)) {
        if(normal_P[m] < 0.001)
          normal_P[m] <- "< 0.001"
      }
      table_two[nrow(table_two)+1, 1] <- "Normal_test_P_value"
      table_name <- colnames(table_two)[2:(length(normal_P)+1)]
      table_name <- gsub(" \\(.*\\)","",table_name)
      for (l in 1:length(normal_P)) {
        for (n in 1:length(normal_P)) {
          if(table_name[l] == group_type[n])
            table_two[nrow(table_two), l+1] <- normal_P[n]
        }
      }
      normal_P <- vector(mode = "numeric", length = length(group_type))

      unmiss <- vector(mode = "numeric", length = length(group_type))
      for (j in 1:length(group_type)) {
        unmiss[j] <- sum(!is.na(database[database$group == group_type[j], i+1]))
      }
      if(all(unmiss) > 0){
        database[, c(1,i+1)]$group <- as.factor(database[, c(1,i+1)]$group)
        names(database)[c(1,i+1)] <- gsub("-", "_", names(database)[c(1,i+1)]) 
        names(database)[c(1,i+1)] <- gsub("/", "_", names(database)[c(1,i+1)])
        names(database)[c(1,i+1)] <- gsub(" ", "", names(database)[c(1,i+1)])
        names(database)[c(1,i+1)] <- gsub("\\(", "", names(database)[c(1,i+1)])
        names(database)[c(1,i+1)] <- gsub("\\)", "", names(database)[c(1,i+1)])
        names(database)[c(1,i+1)] <- gsub("（", "", names(database)[c(1,i+1)])
        names(database)[c(1,i+1)] <- gsub("）", "", names(database)[c(1,i+1)])
        gongshi <- as.formula(paste(names(database)[i+1], "~", names(database)[1]))
        if(a == "anova"){ 
          summary_aov <- summary(aov(gongshi, data = database[, c(1,i+1)]))[[1]]$`F value`[1]
          if(length(group_type) == 2){
            table_two$`Statistics`[1] <- paste("T =", round(t.test(gongshi, data = database[, c(1,i+1)], var.equal = TRUE)$statistic, 2))
            conf_int <- t.test(gongshi, data = database[, c(1,i+1)], var.equal = TRUE)$conf.int
            table_two$`95%CI`[1] <- paste("(", round(conf_int[1], 3), ",", round(conf_int[2], 3), ")")
            table_two$`p value`[1] <- sprintf("%.3f", t.test(gongshi, data = database[, c(1,i+1)], var.equal = TRUE)$p.value)
          }else{
            table_two$`Statistics`[1] <- paste("F =", round(summary_aov, 2))
          }
        }else if(a == "kwt" ){ 
          if(length(group_type) == 2){
            table_two$`Statistics`[1] <- paste("W =", round(wilcox.test(gongshi, data = database[, c(1,i+1)])$statistic, 2))
            if(length(unique(database[!is.na(database[, i+1]), i+1])) != 1){ 
              conf_int <- wilcox.test(gongshi, data = database[, c(1,i+1)], conf.int = T)$conf.int
              table_two$`95%CI`[1] <- paste("(", round(conf_int[1], 3), ",", round(conf_int[2], 3), ")")
            } 
            
            table_two$`p value`[1] <- sprintf("%.3f", wilcox.test(gongshi, data = database[, c(1,i+1)])$p.value)
          }else{
            table_two$`Statistics`[1] <- paste("X-squared =", round(kruskal.test(gongshi, data = database[, c(1,i+1)])$statistic, 2))
          }
          rank_calculate <- database[, c(1,i+1)]
          rank_calculate$`Rank`[!is.na(rank_calculate[, 2])] <- rank(rank_calculate[!is.na(rank_calculate[, 2]), 2])
          table_two[nrow(table_two)+1, 1] <- "Average Rank"
          for (k in 2:(length(group_type)+1)) {
            table_two[nrow(table_two), k] <- round(mean(rank_calculate[rank_calculate$group == gsub(" \\(.*\\)","",names(table_two)[k]), 3], na.rm = T), 3)
          }
          table_two[nrow(table_two), ncol(table_two)-4] <- round(mean(rank_calculate[, 3], na.rm = T), 3)
          table_two <- table_two[c(1:(nrow(table_two)-2),nrow(table_two),nrow(table_two)-1),]
        }
        if(table_two$group.test[1] == ""){
          table_two$`Statistics`[1] <-  ""
        }
      }
    }
    
    names(table_two)[1] <- "Variable"
    if(i==1){
      table_three <- table_two
    }else{
      table_three <- rbind(table_three,table_two)
    }
  }
  
  names(table_three) <- gsub("Total", "Total", names(table_three))
  names(table_three)[names(table_three) == "group.test"] <- "Test Method"
  names(table_three)[names(table_three) == "p value"] <- "P-value"
  if(length(group_type) > 2 | all(data_type == "categorical"))
    table_three <- table_three[, -c(ncol(table_three)-2)]

  if(all(table_three$`Statistics` == "")) 
    table_three <- table_three[, -c(ncol(table_three)-1)]
  
  if(length(group_type) == 2){
    table_three$`Test Method`[table_three$`Test Method` == "Kruskal-Wallis rank sum test"] <- "Wilcoxon rank sum tests"
    table_three$`Test Method`[table_three$`Test Method` == "Linear Model ANOVA"] <- "t-test"
    
    for (i in 1:nrow(table_three)) {
      if(is.na(table_three$`P-value`[i]))
        table_three$`P-value`[i] <- ""
      else if(table_three$`P-value`[i] == "1" | table_three$`P-value`[i] == "1.000")
        table_three$`P-value`[i] <- "> 0.999"
      else if(table_three$`P-value`[i] == "0" | table_three$`P-value`[i] == "0.000")
        table_three$`P-value`[i] <- "< 0.001"
    }
  }else{
    for (i in 1:nrow(table_three)) {
      if(is.na(table_three$`P-value`[i]))
        table_three$`P-value`[i] <- ""
      else if(table_three$`P-value`[i] == "1.000")
        table_three$`P-value`[i] <- "> 0.999"
    }
  }
    table_three
  
}
