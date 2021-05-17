library(tidyverse); library(readxl)

getwd()
setwd(your_directory) # or r project

# read and write

my.data <- foreign::read.spss(".introR/data/KCYPS2010 m1w1.sav", to.data.frame = T)
class(my.data)
my.data <- readxl::read_excel(".introR/data/KCYPS2010 m1w1.xlsx")
class(my.data)

my.data[,1:2]

# CSV
write_csv(my.data, ".introR/data/KCYPS2010 m1w1.csv")
# DAT
write_tsv(my.data, ".introR/data/KCYPS2010 m1w1.dat", col_names = F)

# multiple
file_list <- fs::dir_ls(".introR/data")
xlsx_file <- file_list[str_detect(file_list, "xlsx")]
for(i in 1:length(xlsx_file)) {
  data0 <- readxl::read_xlsx(xlsx_file[i])
  write_csv(data0, paste0(".introR/data/data_w", i, ".csv"))
}


# read.csv
my.data <- read_csv(".introR/data/KCYPS2010 m1w1.csv")
# read_delim(".introR/data/KCYPS2010 m1w1.dat", delim = "\t", col_names = F)

my.data

# Wrangle data ----------------------------------------
# data select / rename
"ID"
"PSY2A01w1"
"PSY2B01w1"
"GENDERw1"

# indexing
data_w1 <- my.data
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

new.data %>% rename_if(startsWith(names(.), "A"), ~paste0("df1_", .))

new.data %>% rename_at(vars(starts_with("PSY2A")), ~paste0("selfes_", .))

data_w1_1 <-
  new.data %>%
  rename_at(vars(contains('PSY2A')), ~(sub('PSY2A', 'selfes_', .))) %>%
  rename_at(vars(contains('PSY2B')), ~(sub('PSY2B', 'good_', .)))

new.data %>% set_names(~sub('PSY2A', 'self_', .x))

new.data %>% select_all(~gsub("PSY2A", "self_", .))

new.data %>% select_all(., list(~ toupper(.)))

classification <- c("ID", "GENDERw1", "PSY2A01w1")
new.data %>%
  select(!!classification)

new.data %>%
  select_if(is.numeric) %>%
  glimpse()

new.data %>%
  select_if(is.numeric) %>%
  select_if(~mean(., na.rm=TRUE) > 10)

new.data %>%
  select_if(~is.numeric(.) & mean(., na.rm=TRUE) > 10)

new.data %>%
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


data_w1_1 <- data_w1_1 %>%
  mutate(GENDERchar = recode(GENDERw1,
                                "1" = "female",
                                .default = "male"))
data_w1_1 %>% count(GENDERchar)

data_w1_1 %>%
  select(ID, GENDERchar) %>%
  mutate(GENDERchar = toupper(GENDERchar)) %>%
  left_join(conservation_table, by = c("conservation" = "abbreviation")) %>%
  mutate(description = ifelse(is.na(description), conservation, description))

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


# Multiple data
file_list <- fs::dir_ls(".introR/data")
data_list <- file_list[str_detect(file_list, "xlsx")]

data_container <- list()
for(i in 1:length(data_list)) {
  # i <- 1
  data_w <- readxl::read_excel(data_list[i])
  # names(data_w)

  varName <- paste0("mean_selfes_w",i)

  data_w <-
    data_w %>%
    select(ID,
           starts_with("GENDER"),
           starts_with("PSY2A")) %>%
    rename_at(vars(contains('PSY2A')), ~(sub('PSY2A', 'selfes_', .))) %>%
    rename_at(vars(contains('Gender')), ~(sub(., 'Gender', .))) %>%
    na_if(., -9) %>%
    mutate(
      !!varName := rowMeans(select(., contains("selfes_")), na.rm = T)
    ) %>%
    select(ID, Gender, matches("^mean_")) %>%
    mutate(Gender = case_when(Gender == 1 ~ "Girl", TRUE ~ "Boy"))

  data_container[[i]] <- data_w
}

# Merge data
merged <-
  data_container[[1]] %>%
  left_join(., data_container[[2]], by = "ID") %>%
  left_join(data_container[[3]], by = "ID") %>%
  left_join(data_container[[4]], by = "ID") %>%
  left_join(data_container[[5]], by = "ID") %>%
  left_join(data_container[[6]], by = "ID") %>%
  left_join(data_container[[7]], by = "ID") %>%
  select(-contains("Gender")) %>%
  left_join(., data_container[[1]][,c("ID","Gender")], by = "ID") %>%
  select(ID, Gender, everything())

names(merged) <- c("ID", "gen",paste0("w",1:7))


# test SEM
library(lavaan)
# girl.merged <- merged %>% filter(Gender == "Girl")
model = '

  I =~ 1*w1 + 1*w3 + 1*w5 + 1*w6 + 1*w7
  S =~ 0*w1 + 2*w3 + 4*w5 + 5*w6 + 6*w7

  #I ~ Gender
  #S ~ Gender

'
fit <- lavaan::growth(model = model,
                   data = merged)

summary(fit, fit.measures=TRUE)

latent <- parameterestimates(fit) %>%
  filter(lhs %in% c("I","S"), op == "~1") %>%
  pull(est)

tibble(timepoint = 0:6, estimated = timepoint * latent[2] + latent[1]) %>%
  ggplot(aes(x = timepoint, y = estimated)) +
  geom_line(aes(group = 1)) +
  geom_point() +
  geom_text(aes(label = round(estimated, 2)),
            vjust = -1) +
  scale_y_continuous(limits = c(1, 5)) +
  theme_bw(base_size = 14)




# Additional stuffs...
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
