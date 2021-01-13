
##### R virtual environment setup
#renv::init()
renv::restore()
#renv::snapshot()

##### Download packages
library(tidyverse)
library(haven)

##### Load data
df = read_dta('data/source/CAFTA_DATA.dta')
##### Make variable: 
### 1. 
df = df %>% mutate( LogExp= log(sum_exp+1)) 

### 2. Differentiation level of goods from sort of products.
df = df %>% mutate( Rauch = case_when(
    HS2 == "01" ~ 0.4285714, 
    HS2 == "02" ~ 0.125, 
    HS2 == "03" ~ 0.3333333, 
    HS2 == "04" ~ 0, 
    HS2 == "05" ~ 1, 
    HS2 == "06" ~ 1, 
    HS2 == "07" ~ 0, 
    HS2 == "08" ~ 0.2727273, 
    HS2 == "09" ~ 0.7142857, 
    HS2 == "10" ~ 0, 
    HS2 == "11" ~ 1, 
    HS2 == "12" ~ 0.3333333, 
    HS2 == "13" ~ 1,
    HS2 == "14" ~ 0.6666667, 
    HS2 == "15" ~ 0.125, 
    HS2 == "16" ~ 0.2, 
    HS2 == "17" ~ 0.1666667, 
    HS2 == "18" ~ 0.6, 
    HS2 == "19" ~ 1, 
    HS2 == "20" ~ 0.75, 
    HS2 == "21" ~ 1, 
    HS2 == "22" ~ 0.3333333, 
    HS2 == "23" ~ 1, 
    HS2 == "24" ~ 0.2, 
    HS2 == "25" ~ 0.25, 
    HS2 == "26" ~ 0,
    HS2 == "27" ~ 0.4166664, 
    HS2 == "28" ~ 0.07692308, 
    HS2 == "29" ~ 0.1538462, 
    HS2 == "30" ~ 1, 
    HS2 == "31" ~ 0.1666667, 
    HS2 == "32" ~ 0.4444444, 
    HS2 == "33" ~ 0.6666667, 
    HS2 == "34" ~ 0.8571429, 
    HS2 == "35" ~ 0, 
    HS2 == "36" ~ 1, 
    HS2 == "37" ~ 1,
    HS2 == "38" ~ 0.7142857, 
    HS2 == "39" ~ 0.3846154, 
    HS2 == "40" ~ 0.9166667, 
    HS2 == "41" ~ 0.7272727, 
    HS2 == "42" ~ 1, 
    HS2 == "43" ~ 1, 
    HS2 == "44" ~ 0.5384615, 
    HS2 == "45" ~ 1, 
    HS2 == "46" ~ 1, 
    HS2 == "47" ~ 0, 
    HS2 == "48" ~ 0.3636364,
    HS2 == "49" ~ 1, 
    HS2 == "50" ~ 0.5, 
    HS2 == "51" ~ 0.6, 
    HS2 == "52" ~ 0.2, 
    HS2 == "53" ~ 0.5, 
    HS2 == "54" ~ 0.25, 
    HS2 == "55" ~ 0.2307692, 
    HS2 == "56" ~ 1, 
    HS2 == "57" ~ 1, 
    HS2 == "58" ~ 0.875, 
    HS2 == "59" ~ 1,
    HS2 == "60" ~ 1, 
    HS2 == "61" ~ 1, 
    HS2 == "62" ~ 1, 
    HS2 == "63" ~ 1, 
    HS2 == "64" ~ 1, 
    HS2 == "65" ~ 1, 
    HS2 == "66" ~ 1, 
    HS2 == "67" ~ 1, 
    HS2 == "68" ~ 1, 
    HS2 == "69" ~ 1, 
    HS2 == "70" ~ 1,
    HS2 == "71" ~ 0.4, 
    HS2 == "72" ~ 0.2, 
    HS2 == "73" ~ 0.8947368, 
    HS2 == "74" ~ 0.625, 
    HS2 == "75" ~ 0.2, 
    HS2 == "76" ~ 0.5714286, 
    HS2 == "77" ~ 0, 
    HS2 == "78" ~ 0.25, 
    HS2 == "79" ~ 0.25, 
    HS2 == "80" ~ 0.25,
    HS2 == "81" ~ 0.3333333, 
    HS2 == "82" ~ 1, 
    HS2 == "83" ~ 1, 
    HS2 == "84" ~ 1, 
    HS2 == "85" ~ 1, 
    HS2 == "86" ~ 1, 
    HS2 == "87" ~ 1, 
    HS2 == "88" ~ 1, 
    HS2 == "89" ~ 1, 
    HS2 == "90" ~ 1,
    HS2 == "91" ~ 1, 
    HS2 == "92" ~ 1, 
    HS2 == "93" ~ 1, 
    HS2 == "94" ~ 1, 
    HS2 == "95" ~ 1, 
    HS2 == "96" ~ 1, 
    HS2 == "97" ~ 1, 
    HS2 == "98" ~ 0, 
    HS2 == "99" ~ 0, 
    HS2 == "100" ~ 0
    )) 




# how long a product in a firm produced
# however a jump cannot be captured

### 3. big and small companies
df = df %>% mutate( EmpBin = case_when( employment > 92 ~ "BIG", employment <= 92 ~ "SMALL"  )) 

### 4. diff and homo products
df = df %>% mutate( DiffBin = case_when( Rauch > 0.4 ~ "HOMO", Rauch <= 0.4 ~ "DIFF" )) 
dim(df)
dim(df_life)
### 5. old and new products
df_life = df %>% 
    arrange(cowcode, firm_code, product12, year) %>% 
    select(sum_exp, LogExp, EmpBin, DiffBin, employment, cowcode2, country, firm_code2, product12, year, US, Rauch) %>% 
    group_by(country, firm_code2, product12) %>% 
    mutate( life = row_number()) %>% 
    mutate( new = case_when( life > 1 ~ "OLD", 
        life == 1 & year == 2000 ~ "UNKNOWN", 
        life == 1 & year != 2000 ~ "NEW" ))

write_csv(df_life, "data/processed/df_life.csv")

# how many different kinds of products a firm produced in that year
# df_diversity = df %>% arrange(cowcode, firm_code, product12, year) %>% select(sum_exp, employment, cowcode2, firm_code2, year, product12, Rauch) %>% group_by(cowcode2, firm_code2, year) %>% summarise(diversity =n())

#group_by() x!=lag(x)
#drop if  cowcode==92 | cowcode==93 | cowcode==90 | cowcode==42 | cowcode==91 | cowcode==95
#drop if  cowcode==52 | cowcode==53 | cowcode==110 | cowcode==20 | cowcode==155
#library(countrycode)
#countrycode(df$cowcode2, origin="cown", destination="iso3c")