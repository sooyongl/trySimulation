library(tidyverse); library(readxl)

getwd()
setwd(your_directory) # or r project

# read and write

my.data <- foreign::read.spss("data/KCYPS2010 m1w1.sav", to.data.frame = T)
class(my.data)
my.data <- readxl::read_excel("data/KCYPS2010 m1w1.xlsx")
class(my.data)

my.data[,1:2]

write_csv(my.data, "data/KCYPS2010 m1w1.csv")
write_tsv(my.data, "data/KCYPS2010 m1w1.dat", col_names = F)

# read.csv
my.data <- read_csv("data/KCYPS2010 m1w1.csv")
read_delim("data/KCYPS2010 m1w1.dat", delim = "\t", col_names = F)

my.data

# Wrangle data ----------------------------------------
# data select / rename
"ID"
"PSY2A01w1"
"PSY2B01w1"
"GENDERw1"

# indexing

data_w1[ 1, 1] # data.frame
data_w1[  , 1]
data_w1[  , "ID"]

data_w1[, "PSY2A01w1"]
data_w1[, c("ID","PSY2A01w1","PSY2B01w1","GENDERw1")]

data_w1[, 103]

data_w1[["PSY2A01w1"]]
data_w1[[103]]

data_w1[, c(paste0("PSY2A0",1:9, "w1"), "PSY2A10w1")]
data_w1[, 103:112]


data_w1[, c("ID","PSY2A01w1","PSY2B01w1","GENDERw1")]
dplyr::select(data_w1, c("ID","PSY2A01w1","PSY2B01w1","GENDERw1"))
dplyr::select(data_w1, ID,PSY2A01w1,PSY2B01w1,GENDERw1)

data_w1 %>% dplyr::select(., "PSY2A10w1")
data_w1 %>% select(., ID, PSY2A10w1, PSY2B10w1, GENDERw1)


data_w1 %>% 
  select(103)

new.data <- 
  data_w1 %>% 
  select(ID, 
         GENDERw1,
         starts_with("PSY2A"),
         starts_with("PSY2B"))

# starts_with(); ends_with(); contains(); matches(); num_range(); one_of(); everything(); group_cols()

# tibble::rownames_to_column()

data_w1 %>% 
  select(p2a_10_w1 = PSY2A10w1)


data_w11 <- data_w1 %>% select(., ID, PSY2A10w1, PSY2B10w1, GENDERw1)

data_w12 <- data_w11 %>% 
  rename(
    id_number = ID,
    psy_a     = PSY2A10w1,
    psy_b     = PSY2B10w1,
    gender    = GENDERw1
  )

data_w1 %>% 
  rename(id_num = ID)

data_w1 %>% 
  rename("id_num" = "ID")

vars <- c(var1 = "cyl", var2 ="am")
new.data %>% rename_if(startsWith(names(.), "A"), ~paste0("df1_", .))

new.data %>% rename_at(vars(starts_with("PSY2A")), ~paste0("selfes_", .))

data_w1_1 <- 
  new.data %>% 
  rename_at(vars(contains('PSY2A')), ~(sub('PSY2A', 'selfes_', .))) %>% 
  rename_at(vars(contains('PSY2B')), ~(sub('PSY2B', 'good_', .)))

new.data %>% set_names(~sub('PSY2A', 'self_', .x))

new.data %>% select_all(~gsub("PSY2A", "self_", .))

new.data %>% select_all(., list(~ toupper(.)))

classification <- c("name", "genus", "vore", "order", "conservation")

msleep %>%
  select(!!classification)

msleep %>%
  select_if(is.numeric) %>%
  glimpse

msleep %>%
  select_if(is.numeric) %>%
  select_if(~mean(., na.rm=TRUE) > 10)

msleep %>%
  select_if(~is.numeric(.) & mean(., na.rm=TRUE) > 10)

msleep2 %>%
  select_all(~str_replace(., " ", "_"))

mtcars %>%
  tibble::rownames_to_column("car_model") %>%
  head

########################################################################
# Transformation


data_w13 <- data_w12 %>% 
  mutate(
    sum_psy = psy_a + psy_b,
    mean_psy = (psy_a+ psy_b)/2
  )


data_w13 %>% 
  filter(mean_psy < 2)

#
data_w1 <- read_excel(data_list[1])
data_ready <- data_w1 %>% 
  select(ID, PSY2A01w1, PSY2B01w1, GENDERw1) %>% 
  rename(
    id = ID,
    psy_a = PSY2A01w1, 
    psy_b = PSY2B01w1, 
    gender = GENDERw1
  ) %>% 
  mutate(
    sum_psy = psy_a + psy_b,
    mean_psy = (psy_a + psy_b)/2,
  ) # %>% 
# filter(mean_psy < 2)

data_ready %>% filter(gender == 1) %>% select(sum_psy) %>% colMeans()

data_ready %>% filter(gender == 2) %>% select(sum_psy) %>% colMeans()


data_ready %>% 
  group_by(gender) %>% 
  summarise(
    group.mean = mean(sum_psy)
  )


data_ready %>% 
  filter(sum_psy > 0) %>% 
  mutate(gender = factor(gender)) %>% 
  ggplot() +
  geom_boxplot(
    aes(x = gender, y = sum_psy, colour = gender)
  ) + 
  theme_void()

#
data_w1_1 %>%
  # select(matches("^selfes")) %>%
  select(3:5) %>%
  mutate(mean_selfes = (selfes_01w1 + selfes_02w1 + selfes_03w1)/3,
         mean_cent_w1 = selfes_01w1 - mean_selfes)

data_w1_1 %>%
  select(3:5) %>%
  rowwise() %>% 
  mutate(mean_selfes = mean(selfes_01w1 + selfes_02w1 + selfes_03w1),
         mean_cent_w1 = selfes_01w1 - mean_selfes) %>% 
  mutate(over = ifelse(selfes_01w1 > mean_selfes, 0, 1)) %>% # or case_when
  mutate_at(vars(contains("selfes")), ~(.*10))


msleep %>%
  mutate(conservation2 = recode(conservation,
                                "en" = "Endangered",
                                "lc" = "Least_Concern",
                                "domesticated" = "Least_Concern",
                                .default = "other")) %>%
  count(conservation2)

msleep %>%
  select(name, conservation) %>%
  mutate(conservation = toupper(conservation)) %>%
  left_join(conservation_table, by = c("conservation" = "abbreviation")) %>%
  mutate(description = ifelse(is.na(description), conservation, description))

msleep %>%
  select(name:order) %>%
  na_if("omni")

msleep %>% 
  select(name, sleep_total) %>% 
  filter(between(sleep_total, 16, 18))

msleep %>% 
  select(name, sleep_total) %>% 
  filter(near(sleep_total, 17, tol = sd(sleep_total)))

remove <- c("Rodentia", "Carnivora", "Primates")
msleep %>% 
  select(order, name, sleep_total) %>% 
  filter(!order %in% remove)

msleep %>% 
  select(name, sleep_total) %>% 
  filter(str_detect(tolower(name), pattern = "mouse"))

msleep %>% 
  select(name, conservation:sleep_cycle) %>% 
  filter(!is.na(conservation))

msleep %>%
  select(order, name, sleep_total) %>%
  group_by(order) %>%
  arrange(desc(sleep_total), .by_group = TRUE)

msleep %>%
  group_by(order) %>%
  summarise(average = mean(sleep_total)) %>%
  top_n(5)

msleep %>%
  group_by(order) %>%
  summarise(average_sleep = mean(sleep_total), max_sleep = max(sleep_total)) %>%
  top_n(5, average_sleep)

msleep %>%
  slice(50:55)

#

data_w1 <- my.data

data_w1_v1 <- 
  data_w1 %>% 
  select(ID, 
         GENDERw1,
         starts_with("PSY2A"),
         starts_with("PSY2B")) %>% 
  rename_at(vars(contains('PSY2A')), ~(sub('PSY2A', 'selfes_', .))) %>% 
  rename_at(vars(contains('PSY2B')), ~(sub('PSY2B', 'confidence_', .))) %>%
  rename("Gender" = GENDERw1 ) %>% 
  na_if(., -9) %>% 
  mutate(
    mean_selfes = rowMeans(select(., contains("selfes_")), na.rm = T),
    mean_conf = rowMeans(select(., contains("confidence_")), na.rm = T)
  ) %>% 
  select(ID, Gender, matches("^mean_")) %>% 
  mutate(Gender = case_when(Gender == 1 ~ "Girl", TRUE ~ "Boy")) %>% 
  filter(Gender == "Girl")

# Save the cleaned data ----------------------------------------
write_csv(data_w1_v1, "data_w1_v1.csv")

# Read the data ----------------------------------------
read_csv("data_w1_v1.csv")
