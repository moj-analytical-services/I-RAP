popdata <- prison_pop_data("20200630")

filtervars <- c("sex","age_group_adult")

nestedvars <- list(c("custody_group","custody_type","custody_detail","custody_sentence_detail"))


Table1_1 <- prison_pop_table(popdata,filtervars,nestedvars)

Table1_2ai <- prison_pop_table(filter(popdata,custody_type=="Untried"),c("sex","age_group_adult","offence_group")) 

Table1_2aii <- prison_pop_table(filter(popdata,custody_type=="Convicted unsentenced"),c("sex","age_group_adult","offence_group")) 

Table1_2b <- prison_pop_table(filter(popdata,custody_group=="Sentenced" & custody_type!="Fine defaulter"),c("sex","age_group_adult","offence_group")) 

Table1_3 <- prison_pop_table(popdata,c("sex","age_group","custody_group"))

Table1_4 <- prison_pop_table(popdata,c("sex","ethnicity_group"))

Table1_6 <- prison_pop_table(popdata,"nationality_group",nestedvars)


popdata <- prison_pop_data("20210630")

Table1_7 <- prison_pop_table(popdata,"sex",list(c("nationality_group","nationality_continent","nationality")))

Table1_8 <- prison_pop_table(popdata,"nationality_group",list(c("prison_type","prison_name")))