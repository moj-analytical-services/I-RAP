
df <- iRAP::prison_pop_data("20201231")

metadata <- tribble(~"variable", ~"parent",
                "custody_type", "custody_group",
                "custody_detail", "custody_type",
                "custody_group","",
                "sex","",
                "prison_type","sex",
                "age_group_adult","",
                "custody_sentence_detail","custody_detail")


vars <- c("sex","age_group_adult","custody_group","custody_type","custody_detail","custody_sentence_detail")

table <- tabulate(df,vars,metadata,"prisoners")
