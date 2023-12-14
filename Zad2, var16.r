library(factoextra)
library(readr)
library(dplyr)
table = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1",
                 skip=1, comment = "[")
table = table[-1,]
table[table == -9999] = NA
table <- table %>% filter(daytime == "T")
table <- table %>% filter(year(date) == 2013)
table <- table %>% filter(month(date) %in% c(03, 04, 05))
table = table %>% select(-where(is.character))
table = table %>% select(where(is.numeric))
table =-table %>% select(-contains("co2"), co2_flux)
var(table, na.rm = T)
var_data = table %>% summarise_all( ~var(.x,na.rm=T))
var_data[is.na(var_data)] = 0
cpa_data = table[,as.logical(var_data != 0)]
cor_matrix = cor(na.exclude(cpa_data))
heatmap(cor_matrix)
co2_cor = as.numeric(cor_matrix[83,])
names(co2_cor) = names(cor_matrix[83,])
cpa_dataf = cpa_data[,co2_cor > 0.25 | co2_cor < -.25]

cpa_dataf = cpa_dataf %>% rename(T_star = `T*`)
cpa_dataf = cpa_dataf %>% rename(z_sub_d__L = `(z-d)/L`)
cpa_dataf = cpa_dataf %>% rename_with( 
  ~gsub("-","_sub_",.x, fixed = T))
cpa_dataf = cpa_dataf %>% rename_with( 
  ~gsub("*","_star",.x, fixed = T))
cpa_dataf = cpa_dataf %>% rename_with( 
  ~gsub("%","_p",.x, fixed = T))
cpa_dataf = cpa_dataf %>% rename_with( 
  ~gsub("/","__",.x, fixed = T))

table = table %>% rename(T_star = `T*`)
table = table %>% rename(z_sub_d__L = `(z-d)/L`)
table = table %>% rename_with( 
  ~gsub("-","_sub_",.x, fixed = T))
table = table %>% rename_with( 
  ~gsub("*","_star",.x, fixed = T))
table = table %>% rename_with( 
  ~gsub("%","_p",.x, fixed = T))
table = table %>% rename_with( 
  ~gsub("/","__",.x, fixed = T))

data_pca = prcomp(na.exclude(cpa_dataf),scale=TRUE)
data_pca

fviz_pca_var(data_pca,repel = TRUE, col.var = "steelblue")

model = lm(co2_flux ~ air_heat_capacity+qc_LE+T_star+u_star+rand_err_LE, table)
summary(model)

formula = paste(c("co2_flux ~ ",
                  paste(names(cpa_dataf)[-43], collapse = "+")),
                collapse = "")
formula = as.formula(formula)


model_first = lm(formula, cpa_dataf)
summary(model_first)

formula2 = formula(co2_flux ~ Tau + H + rand_err_LE + rand_err_h2o_flux + air_density + 
                     air_molar_volume + x_10_p + x_70_p + x_90_p + un_Tau + h2o_var + 
                     w__ts_cov + w__h2o_cov + flowrate + rand_err_H + u_star + rand_err_Tau + 
                     qc_LE + x_50_p)
model2 = lm(formula2, cpa_dataf)
summary(model2)

formula3 = formula(co2_flux ~ Tau + H + x_10_p + x_70_p + x_90_p + un_Tau + h2o_var + 
                     w__ts_cov + w__h2o_cov + flowrate + rand_err_H + u_star + rand_err_Tau + x_50_p)
model3 = lm(formula3, cpa_dataf)
summary(model3)
anova(model3)

formula4 = formula(co2_flux ~ Tau + H + x_10_p + x_70_p + un_Tau + h2o_var + 
                     w__h2o_cov + flowrate + u_star + rand_err_Tau)
model4 = lm(formula4, cpa_dataf)
summary(model4)
anova(model4)

formula5 = formula(co2_flux ~ Tau + H + x_10_p + x_70_p + un_Tau + 
                     w__h2o_cov + u_star)
model5 = lm(formula5, cpa_dataf)
summary(model5)
anova(model5)
