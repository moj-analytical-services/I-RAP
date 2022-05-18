
df <- iRAP::prison_pop_data("20201231")

metadata <- tribble(~"variable", ~"parent",
                "custody_type", "custody_group",
                "custody_detail", "custody_type",
                "custody_group","",
                "sex","",
                "prison_type","sex")

vars <- c("sex","custody_group","custody_type","custody_detail")


table <- tabulate(df,vars,metadata)