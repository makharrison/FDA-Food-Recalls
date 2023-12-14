
#all_fda_data <- read.csv("FDA_Recall_Data-11-28-2023.csv")

all_fda_data_clean <- data %>%
# Drop rows
filter(country == "United States") %>%
# Drop Columns
select(reason_for_recall, 
       state, 
       report_date, 
       classification, 
       recalling_firm,
       initial_firm_notification,
       status) %>%
# format columns
mutate(report_date = ymd(report_date),
       # Month
       report_month = month(report_date),
       # Year
       report_year = year(report_date),
       # reason_for_recall
       reason_for_recall_clean = tolower(reason_for_recall),
       reason_for_recall_clean = gsub('[[:punct:] ]+',' ',reason_for_recall_clean)
       ) %>%
# Remove NAs
na.omit() %>%
#distinct() %>% # Do we want distinct
mutate(

  ## Reason for Recall ##  

# Write if else for big categories
  
  # Foreign objects #
  
  # Metal
  # Plastic
  # Rocks/stones
  # Rubber
  # Other

foreign_objects = ifelse(grepl("foreign object|foreign|object|metal|plastic|rocks|stones|rubber|material|aluminum|\\bpiece\\b|pieces|\\bglass\\b",reason_for_recall_clean),1,0),

# Bacterial Contamination #

# Listeria
# Hepatitis
# Salmonella
# E. Coli
# Other
  
contamination = ifelse(grepl("contamination|contaminated|listeria|hepatitis|e coli|salmonella|bacteria|norovirus|monocytogenes|herbicide|haloxyfop|cyclospora|sakazakii|clostridium|botulinum|organism|cronobacte|\\bmono\bb|chlorpyrifos|coliform|microbiological|bacillus|cereus",reason_for_recall_clean),1,0) ,

# Undeclared ingredient #

# milk
# wheat
# egg
# peanuts
# tree nuts
# fish
# crustacean shellfish
# soy
# sesame
# other/unknown

undeclared = ifelse(grepl("allergen|undeclared|declare|milk|whey|wheat|\\begg\\b|peanut|\\bnut\\b|\\bfish\\b|\\bsoy\\b|treenut|soybean|crustacean|shellfish|\\bcrab\\b|lobster|shrimp|sesame|anchovy|anchovies|walnut|\\bnuts\\b|coconut|cashew|pistachio|sunflower|\\bseed\\b|sulfite|almond|pecan|hazelnut|gluten",reason_for_recall_clean),1,0),

# Gluten #

#gluten = ifelse(grepl("gluten",reason_for_recall_clean),1,0),

# Mold #
mold = ifelse(grepl("\\bmold\\b|\\bmoldy\\b",reason_for_recall_clean),1,0),

# Temperature Malfunction#
temperature = ifelse(grepl("temperature|mechanical|malfunction",reason_for_recall_clean),1,0),


broad_category = ifelse(undeclared == 1, "undeclared ingredient", "other"),
broad_category = ifelse(broad_category == "other" & foreign_objects == 1, "foreign material",broad_category),
#broad_category = ifelse(broad_category == "other" & gluten == 1, "gluten", broad_category),
broad_category = ifelse(broad_category == "other" & mold == 1, "mold", broad_category),
broad_category = ifelse(broad_category == "other" & contamination == 1, "contamination",broad_category),
broad_category = ifelse(broad_category == "other" & temperature == 1, "mechanical malfunction", broad_category),

## Undeclared ingredient
#milk
milk = ifelse(broad_category=="undeclared ingredient" & grepl("milk|\\bwhey\\b",reason_for_recall_clean),1,0),
#eggs
egg = ifelse(broad_category=="undeclared ingredient" & grepl("\\begg\\b",reason_for_recall_clean),1,0),
#fish, such as bass, flounder, cod
fish = ifelse(broad_category=="undeclared ingredient" & grepl("\\bfish\\b|\\bcod\\b|\\bbass\\b|flounder|anchovy|anchovies",reason_for_recall_clean),1,0),
#Crustacean shellfish, such as crab, lobster, shrimp
shellfish = ifelse(broad_category=="undeclared ingredient" & grepl("crustacean|shellfish|\\bcrab\\b|lobster|shrimp",reason_for_recall_clean),1,0),
#tree nuts, such as almonds, walnuts, pecans
nuts = ifelse(broad_category=="undeclared ingredient" & grepl("\\bnut\\b|\\bnuts\\b|almond|walnut|pecan|coconut|cashew|pistachio|sunflower|\\bseed\\b|hazelnut",reason_for_recall_clean),1,0),
#peanuts
peanut = ifelse(broad_category=="undeclared ingredient" & grepl("peanut",reason_for_recall_clean),1,0),
#wheat
wheat = ifelse(broad_category=="undeclared ingredient" & grepl("wheat",reason_for_recall_clean),1,0),
#soybeans
soybean = ifelse(broad_category=="undeclared ingredient" & grepl("\\bsoy\\b|soybean",reason_for_recall_clean),1,0),
#sesame
sesame = ifelse(broad_category=="undeclared ingredient" & grepl("sesame",reason_for_recall_clean),1,0),
gluten = ifelse(broad_category=="undeclared ingredient" & grepl("gluten",reason_for_recall_clean),1,0),

allergens = rowSums(across(milk:sesame)),
undeclared_allergen = ifelse(milk == 1, "milk","other"),
undeclared_allergen = ifelse(undeclared_allergen == "other" & egg == 1, "egg",undeclared_allergen),
undeclared_allergen = ifelse(undeclared_allergen == "other" & fish == 1, "fish",undeclared_allergen),
undeclared_allergen = ifelse(undeclared_allergen == "other" & shellfish == 1, "shellfish",undeclared_allergen),
undeclared_allergen = ifelse(undeclared_allergen == "other" & nuts == 1, "treenuts",undeclared_allergen),
undeclared_allergen = ifelse(undeclared_allergen == "other" & peanut == 1, "peanuts",undeclared_allergen),
undeclared_allergen = ifelse(undeclared_allergen == "other" & soybean == 1, "soybean",undeclared_allergen),
undeclared_allergen = ifelse(undeclared_allergen == "other" & sesame == 1, "sesame",undeclared_allergen),
undeclared_allergen = ifelse(undeclared_allergen == "other" & gluten == 1, "gluten",undeclared_allergen),
undeclared_allergen = ifelse(undeclared_allergen == "other" & broad_category != "undeclared ingredient",NA,undeclared_allergen),
undeclared_allergen = case_when(
  allergens > 1 ~ "two or more allergens",
  allergens <= 1 ~ undeclared_allergen
)
)


# Write if else for small categories

# shape files
states <- read_sf('cb_2019_us_state_5m.shp')


