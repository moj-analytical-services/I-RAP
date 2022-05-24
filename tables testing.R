
df <- iRAP::prison_pop_data("20201231")

df$prison_type <- dplyr::recode(df$prison_type, Male = "Male prison", Female="Female prison")  
  
metadata <- tibble::tribble(~"variable", ~"parent",
                "custody_type", "custody_group",
                "custody_detail", "custody_type",
                "custody_group","",
                "sex","",
                "prison_type","sex",
                "age_group_adult","",
                "custody_sentence_detail","custody_detail",
                "offence_group","",
                "age_group","",
                "nationality_group","",
                "nationality_continent","nationality_group",
                "nationality","nationality_continent",
                "prison_name","prison_type")


vars <- c("sex","age_group_adult","custody_group","custody_type","custody_detail","custody_sentence_detail")

table <- tabulate(df,vars,metadata,"prisoners")


vars <- c("sex","age_group_adult","offence_group","custody_group","custody_type")

table2 <- tabulate(dplyr::filter(df,custody_type %in% c("Untried","Convicted unsentenced") | (custody_group == "Sentenced" & custody_type !="Fine defaulter")),
                            vars,metadata,"prisoners") %>%
  dplyr::filter(custody_group != "Sentenced" | (custody_group == "Sentenced" & custody_type =="Total"))

vars <- c("sex","age_group","custody_group")

table3 <- tabulate(df,vars,metadata,"prisoners")

vars <- c("ethnicity_group","sex")

table4 <- tabulate(df,vars,metadata,"prisoners")

vars <- c("custody_group","custody_type","custody_detail","custody_sentence_detail","nationality_group")

table6 <- tabulate(df,vars,metadata,"prisoners")

vars <- c("sex","nationality_group","nationality_continent","nationality")

table7 <- tabulate(df,vars,metadata,"prisoners")

vars <- c("nationality_group","prison_type","prison_name")

table8 <- tabulate(df,vars,metadata,"prisoners")