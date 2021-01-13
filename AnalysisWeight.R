
library(stargazer)
library(dotwhisker)
library(broom)
library(dplyr)
library(gridExtra)
library(grid)

df_treat = read_csv("data/processed/df_treat.csv")

df_treat_basic = df_treat %>% select(LogExp, treatment, year, cowcode2, firm_code2, life, Rauch, employment, new, DiffBin, EmpBin)

df_intensive = df_treat_basic %>% filter(new=="OLD") 
df_extensive = df_treat_basic %>% filter(new=="NEW") 
#df_big = df_treat_basic %>% filter(EmpBin=="BIG")
#df_small = df_treat_basic %>% filter(EmpBin=="SMALL")
#df_diff = df_treat_basic %>% filter(DiffBin=="DIFF")
#df_homo = df_treat_basic %>% filter(DiffBin=="HOMO")

df_inten_diff = df_intensive %>% filter(DiffBin=="DIFF")
df_inten_homo = df_intensive %>% filter(DiffBin=="HOMO")
df_exten_diff = df_extensive %>% filter(DiffBin=="DIFF")
df_exten_homo = df_extensive %>% filter(DiffBin=="HOMO")

# dim(df_big)
# dim(df_small)
# dim(df_diff)
# dim(df_homo)
# dim(df_inten_diff)
# dim(df_inten_homo)

#df_intensive_cow =  %>% group_by(cowcode2, year) %>% summarise( sum = sum(sum_exp), treat = mean(treatment)) 
#df_intensive_firm =  %>% group_by(cowcode2, year, firm_code2) %>% summarise( sum = sum(sum_exp), treat = mean(treatment))

I = lm(LogExp ~ treatment + year + cowcode2 , data=df_intensive)
E = lm(LogExp ~ treatment + year + cowcode2 , data=df_extensive)

Diff_I = lm(LogExp ~ treatment*Rauch + year + cowcode2 , data=df_intensive)
Diff_I_FF = lm(LogExp ~ treatment*Rauch + year + cowcode2 + firm_code2 , data=df_intensive)
Diff_E = lm(LogExp ~ treatment*Rauch + year + cowcode2 , data=df_extensive)
Diff_E_FF = lm(LogExp ~ treatment*Rauch + year + cowcode2 + firm_code2 , data=df_extensive)

Emp_I = lm(LogExp ~ treatment*employment + year + cowcode2 , data=df_intensive)
Emp_I_FF = lm(LogExp ~ treatment*employment + year + cowcode2 + firm_code2 , data=df_intensive)
Emp_E = lm(LogExp ~ treatment*employment + year + cowcode2 , data=df_extensive)
Emp_E_FF = lm(LogExp ~ treatment*employment + year + cowcode2 + firm_code2 , data=df_extensive)


stargazer(I, Diff_I, Diff_I_FF, E, Diff_E, Diff_E_FF, omit = c("year", "cowcode2", "firm_code2") )
stargazer(I, Emp_I, Emp_I_FF, E, Emp_E, Emp_E_FF, omit = c("year", "cowcode2", "firm_code2") )


I = tidy(I) %>% filter(term == "treatment") %>% mutate(model="Without-Interaction")
E = tidy(E) %>% filter(term == "treatment") %>% mutate(model="Without-Interaction")

ID = tidy(Diff_I) %>% filter(term == "treatment") %>% mutate(model="Interaction")
IDF = tidy(Diff_I_FF) %>% filter(term == "treatment") %>% mutate(model="Firm Fixed")
ED = tidy(Diff_E) %>% filter(term == "treatment") %>% mutate(model="Interaction")
EDF = tidy(Diff_E_FF) %>% filter(term == "treatment") %>% mutate(model="Firm Fixed")

IS = tidy(Emp_I) %>% filter(term == "treatment") %>% mutate(model="Interaction")
ISF = tidy(Emp_I_FF) %>% filter(term == "treatment") %>% mutate(model="Firm Fixed")
ES = tidy(Emp_E) %>% filter(term == "treatment") %>% mutate(model="Interaction")
ESF = tidy(Emp_E_FF) %>% filter(term == "treatment") %>% mutate(model="Firm Fixed")

diff_models_inten <- rbind(IDF, ID, I)
diff_models_exten <- rbind(EDF, ED, E)

size_models_inten <- rbind(ISF, IS, I)
size_models_exten <- rbind(ESF, ES, E)


a = dwplot(size_models_inten, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>% relabel_predictors(c(treatment= "FTA")) + theme_bw() + xlab("") + ylab("Intensive Margin") +
     geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
     ggtitle("Firm Size Interaction") +
     theme(plot.title = element_text(face="bold"),
           legend.position = c(0.007, 0.01),
           legend.justification = c(0, 0), 
           legend.text = element_text(size=5),
           legend.background = element_rect(colour="grey80"),
           axis.title.y=element_text(size = 14, face="bold"),
           legend.title = element_blank()) +
    scale_colour_grey(start = .2, end = .8)


b = dwplot(diff_models_inten, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>% relabel_predictors(c(treatment= "FTA")) + theme_bw() + xlab("") + ylab("") +
     geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
     ggtitle("Product Differentiation Interaction") +
     theme(plot.title = element_text(face="bold"),
           legend.position = c(0.007, 0.01),
           legend.justification = c(0, 0), 
           legend.text = element_text(size=5),
           legend.background = element_rect(colour="grey80"),
           legend.title = element_blank()) +
    scale_colour_grey(start = .2, end = .8)

c = dwplot(size_models_exten, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>% relabel_predictors(c(treatment= "FTA")) + theme_bw() + xlab("Coefficient Estimate") + ylab("Extensive Margin") +
     geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
     theme(plot.title = element_text(face="bold"),
           legend.position = c(0.007, 0.01),
           legend.justification = c(0, 0), 
           legend.text = element_text(size=5),
           legend.background = element_rect(colour="grey80"),
           axis.title.y=element_text(size = 14, face="bold"),
           legend.title = element_blank()) +
    scale_colour_grey(start = .2, end = .8)

d = dwplot(diff_models_exten, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>% relabel_predictors(c(treatment= "FTA")) + theme_bw() + xlab("Coefficient Estimate") + ylab("") +
     geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
     theme(plot.title = element_text(face="bold"),
           legend.position = c(0.007, 0.01),
           legend.text = element_text(size=5),
           legend.justification = c(0, 0), 
           legend.background = element_rect(colour="grey80"),
           legend.title = element_blank()) +
    scale_colour_grey(start = .2, end = .8)

jpeg("output/ATE.jpg", width=1000, height=600) 
grid.arrange(a, b, c, d, nrow =2, ncol=2,top= textGrob("Average Treatment Effects of FTA", gp = gpar(fontsize = 20, fontface = 'bold'))) 
dev.off()

#df_extensive_cow = df_extensive  %>% select(sum_exp, cowcode2, firm_code2, year, treatment) %>% group_by(cowcode2, year) %>% summarise( sum = sum(sum_exp), treat = mean(treatment)) 
#df_extensive_firm = df_extensive  %>% select(sum_exp, cowcode2, firm_code2, year, treatment) %>% group_by(cowcode2, year, firm_code2) %>% summarise( sum = sum(sum_exp), treat = mean(treatment)) 

#df_extensive_firm = df_extensive_firm  %>% mutate( logsum= log(sum+1))
#df_extensive_cow= df_extensive_cow  %>% mutate( logsum= log(sum+1))

#ols9 = lm(logsum ~ treat + year + cowcode2, data=df_extensive_cow)
#ols10 = lm(logsum ~ treat + year + cowcode2 + firm_code2, data=df_extensive_firm)

# stargazer(ols9, ols10, omit = c("year", "cowcode2", "firm_code2") )

# library(stargazer)

# ols = lm(sum_exp ~ treatment, data= df_intensive)
# ols2 = lm(sum_exp ~ treatment + year + cowcode2 , data= df_intensive)
# ols3 = lm(sum_exp ~ treatment + year + cowcode2 + firm_code2 , data= df_intensive)

# # library(plm)
# # fe_tw = plm(sum_exp ~ treatment, index = c("cowcode2", "year"), data= df_intensive, model = "within", effect="twoway")
# # fe_tw2 = plm(sum_exp ~ treatment + employment, index = c("cowcode2", "year"), data= df_intensive, model = "within", effect="twoway")

# stargazer(ols, ols2, ols3, omit = c("year", "cowcode2", "firm_code2") )
# ln()
# ols4 = lm(sum_exp ~ treatment, data= df_extensive)
# ols5 = lm(sum_exp ~ treatment + year + cowcode2 , data= df_extensive)
# ols6 = lm(sum_exp ~ treatment + year + cowcode2 + firm_code2 , data= df_extensive)

# stargazer(ols4, ols5, ols6, omit = c("year", "cowcode2", "firm_code2"))