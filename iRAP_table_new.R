df <- iRAP::prison_pop_data("20201231")


iRAP_table_new <- function(data,vars,count_indicator) {
  
  varcombos <- 1:length(vars) %>% 
    purrr::map(
      ~
        gtools::combinations(length(vars),.x,vars)
    ) %>% 
    lapply(FUN=function(x) split(x,1:nrow(x))) %>%
    do.call(c,.)
  
  
  data <- data %>% 
          dplyr::ungroup() %>%
          dplyr::select(all_of(c(vars,count_indicator)))
  
  tabulate <- function(x){data %>%
      dplyr::group_by(dplyr::across(all_of(x)), .drop=TRUE) %>%
      dplyr::summarise("{count_indicator}" := sum(.data[[count_indicator]]),.groups="drop") %>%
      dplyr::ungroup()}

  table <- lapply(varcombos,FUN=tabulate) %>%
      dplyr::bind_rows() %>%
      dplyr::bind_rows(.,
                        dplyr::summarise(data,"{count_indicator}" := sum(.data[[count_indicator]]),.groups="drop")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(any_of(vars),~dplyr::case_when(is.na(.) ~ "Total", TRUE ~ .))) %>%
      dplyr::select(-any_of(count_indicator),all_of(count_indicator))
      
  
  return(table)
}


t2 <- iRAP_table_new(df,c("sex","custody_group"),"prisoners")

data <- df

t3 <- dplyr::bind_rows(
  data %>%
    dplyr::group_by(sex) %>%
    dplyr::summarise(prisoners = sum(prisoners)) %>%
    dplyr::ungroup(),
  data %>%
    dplyr::group_by(custody_group) %>%
    dplyr::summarise(prisoners = sum(prisoners)) %>%
    dplyr::ungroup(),
  data %>%
    dplyr::group_by(sex,custody_group) %>%
    dplyr::summarise(prisoners = sum(prisoners)) %>%
    dplyr::ungroup(),
  data %>% 
    dplyr::filter(custody_type != custody_group) %>%
    dplyr::group_by(custody_group,custody_type) %>%
    dplyr::summarise(prisoners = sum(prisoners)) %>%
    dplyr::ungroup(),
  data %>% 
    dplyr::filter(custody_type != custody_group) %>%
    dplyr::group_by(sex, custody_group,custody_type) %>%
    dplyr::summarise(prisoners = sum(prisoners)) %>%
    dplyr::ungroup(),
  data %>% 
    dplyr::filter(custody_detail != custody_type) %>%
    dplyr::group_by(custody_group,custody_type,custody_detail) %>%
    dplyr::summarise(prisoners = sum(prisoners)) %>%
    dplyr::ungroup(),
  data %>% 
    dplyr::filter(custody_detail != custody_type) %>%
    dplyr::group_by(sex,custody_group,custody_type,custody_detail) %>%
    dplyr::summarise(prisoners = sum(prisoners)) %>%
    dplyr::ungroup()
  )

t4 <- dplyr::bind_rows(
  data %>%
    dplyr::group_by(custody_group) %>%
    dplyr::summarise(prisoners = sum(prisoners)) %>%
    dplyr::ungroup(),
  data %>% 
    dplyr::filter(custody_type != custody_group) %>%
    dplyr::group_by(custody_group,custody_type) %>%
    dplyr::summarise(prisoners = sum(prisoners)) %>%
    dplyr::ungroup(),
  data %>% 
    dplyr::filter(custody_detail != custody_type) %>%
    dplyr::group_by(custody_group,custody_type,custody_detail) %>%
    dplyr::summarise(prisoners = sum(prisoners)) %>%
    dplyr::ungroup()
)


t5 <- dplyr::bind_rows(
  data %>% 
    dplyr::filter(custody_type != custody_group) %>%
    dplyr::group_by(custody_group,custody_type) %>%
    dplyr::summarise(prisoners = sum(prisoners)) %>%
    dplyr::ungroup(),
  data %>% 
    dplyr::filter(custody_type != custody_group) %>%
    dplyr::group_by(sex, custody_group,custody_type) %>%
    dplyr::summarise(prisoners = sum(prisoners)) %>%
    dplyr::ungroup(),

  data %>% 
    dplyr::filter(custody_detail != custody_type) %>%
    dplyr::group_by(custody_group,custody_type,custody_detail) %>%
    dplyr::summarise(prisoners = sum(prisoners)) %>%
    dplyr::ungroup(),
  data %>% 
    dplyr::filter(custody_detail != custody_type) %>%
    dplyr::group_by(sex,custody_group,custody_type,custody_detail) %>%
    dplyr::summarise(prisoners = sum(prisoners)) %>%
    dplyr::ungroup()
)
 

filterlist <- 1:length(vars) %>% 
  purrr::map(
    ~
      gtools::combinations(length(vars),.x,vars)
  ) %>% 
  lapply(FUN=function(x) split(x,1:nrow(x))) %>%
  do.call(c,.)
