
df_life = read_csv('data/processed/df_life.csv')
df_life_matched = df_life %>% filter(cowcode2 == 2 | cowcode2 == 95 | cowcode2 == 92 | cowcode2 == 42 | cowcode2 == 91 | cowcode2 == 20)

colnames(df_life_matched)

make_treatment = function(df){
    treatment = c()
    for(i in 1:nrow(df)){
        if(df$cowcode2[i] == 2){
            treatment[i] = ifelse(df$year[i] >= 2009, 1, 0)
        }
        else if(df$cowcode2[i] == 95){
            treatment[i] = ifelse(df$year[i] >= 2008, 1, 0)
        }
        else if(df$cowcode2[i] == 92){
            treatment[i] = 1
        }
        else if(df$cowcode2[i] == 42){
            treatment[i] = ifelse(df$year[i] >= 2002, 1, 0)
        }
        else if(df$cowcode2[i] == 91){
            treatment[i] = 1
        }
        else if(df$cowcode2[i] == 20){
            treatment[i] = ifelse(df$year[i] >= 2002, 1, 0)
        }
        else {
            treatment[i] = "error"
        }
    }
    return(treatment)
}

treatment = make_treatment(df_life_matched)
df_treat = add_column(df_life_matched, treatment)

df_weight = read_csv('data/processed/propensity.csv')

df_weight[df_weight$ISO3 %in% c("USA", "PAN", "SVL", "DOM", "HND", "CAN"),]

make_weight = function(df){
    weight = c()
    for(i in 1:nrow(df)){
        if(df$cowcode2[i] == 2){
            #USA
            weight[i] = ifelse(df$year[i] >= 2009, 1, 0)
        }
        else if(df$cowcode2[i] == 95){
            #PAN
            weight[i] = ifelse(df$year[i] >= 2008, 1, 0)
        }
        else if(df$cowcode2[i] == 92){
            #SLV
            weight[i] = 1
        }
        else if(df$cowcode2[i] == 42){
            #DOM
            weight[i] = ifelse(df$year[i] >= 2002, 1, 0)
        }
        else if(df$cowcode2[i] == 91){
            #HND
            weight[i] = 1
        }
        else if(df$cowcode2[i] == 20){
            #CAN
            weight[i] = ifelse(df$year[i] >= 2002, 1, 0)
        }
        else {
            weight[i] = "error"
        }
    }
    return(weight)
}

########
library(PanelMatch)
df_tscs = df_treat %>% group_by(cowcode2, year) %>% summarise( treat_tscs = mean(treatment) )  
df_tscs= add_column( df_tscs, year_int=as.integer(df_tscs$year))
DisplayTreatment(unit.id = "cowcode2",
                 time.id = "year_int", legend.position = "none",
                 xlab = "year", ylab = "country code",
                 treatment = "treat_tscs", data = as.data.frame(df_tscs))
ggsave("treatment.jpg", width=5, height=2)

#########

write_csv(df_treat, "data/processed/df_treat.csv")
