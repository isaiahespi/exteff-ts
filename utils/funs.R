# functions
# https://adv-r.hadley.nz/functions.html
# https://dereksonderegger.github.io/570L/12-user-defined-functions.html
# 

# fun: crab ---------------------------------------------------------------

# wrapper around `janitor::tabyl` and the janitor `adorn_*` family of functions

crab <- function(dat, group_var, x, digits = 2, ...){
  
  if(!missing(group_var)){
    out <- dat |> 
    janitor::tabyl({{ group_var }}, {{ x }}, ...) |> 
    janitor::adorn_totals(c("row", "col")) |> 
    janitor::adorn_percentages('row') |> 
    janitor::adorn_pct_formatting(digits = digits) |> 
    janitor::adorn_ns()
  } else {
    out <- dat |> 
      janitor::tabyl({{ x }}, ...) |>
      janitor::adorn_totals("row") |>
      janitor::adorn_rounding(digits = digits)
  }
  out
}

# crab(dat = data, x = cit, show_na = T)
# crab(dat = data, group_var = electionvig, x = trust1, show_na = F)

# fun: `freaks` -----------------------------------------------------

# get one-way frequency counts with raw and valid percentages for multiple
# variables in a dataframe.

# this function creates frequency table for each variable or factor in a data
# frame I use `purrr::map` to call `janitor::tabyl` on each variable in the
# `vars` argument. This essentially creates a one-way frequency table with
# percentages and valid percents for each variable column. Since each table is a
# data frame, the output is a list of data frames. Thus I continue to use
# `purrr::map` to apply functions to each frequency table/data frame.

# the important part is that the 1st column of each frequency table is renamed
# to reflect the name of the corresponding data frame in the list. Otherwise,
# the 1st column of each table outputs to ".x[[i]]".

# this function works seems to work well with factors and also labelled
# "<dbl+lbl>" class vectors

freaks <- function(dat, vars, ..., digits = 3){
  
  freak_tabyls <- {{ dat }} |> 
    dplyr::select({{ vars }}) |> 
    purrr::map(janitor::tabyl, ...) |> 
    purrr::map(janitor::adorn_rounding, digits = digits) |>
    # strip each table of `tabyl` class metadata
    purrr::map(janitor::untabyl) |>
    # convert each data.frame to tibble i.e., `tbl_df`
    purrr::map(dplyr::as_tibble) |> 
    purrr::imap(~.x |> dplyr::rename_with(.fn = \(x) paste(.y), .cols = 1))
  return(freak_tabyls)
}

# examples:

# freaks(data, vars = dplyr::contains("_vig_manip"))

# data |> 
#   # convert variable columns to `labelled` class (<dbl+lbl>)
#   dplyr::mutate(dplyr::across(where(is.factor), ~sjlabelled::as_labelled(.))) |> 
#   freaks(vars = dplyr::contains("_vig_manip"))




# fun: codebook -----------------------------------------------------------

# custom function to generate codebook/data dictionary from a data frame
# my take on the data dictionary/codebook functions
codebook <- function(x){
  lapply(1:ncol(x),
         function(i){
           namex <- names(x)[[i]]
           
           label <- purrr::pluck(x, namex, purrr::attr_getter("label"))
           
           val_lab <- purrr::pluck(x, namex, purrr::attr_getter("labels")) |>
               names() |>  
               paste(collapse = "; ") |> 
               stringr::str_trunc(width = 32760)
           
           val <- x[[namex]] |> 
             unique() |> 
             sort() |> 
             paste(collapse = "; ") |> 
             stringr::str_trunc(width = 32760)
           
           fct_lvls <- purrr::pluck(x, namex, purrr::attr_getter("levels")) |>
             paste(collapse = "; ")
           
           fct_lvls <- dplyr::if_else(fct_lvls == "", NA_character_, fct_lvls)
           label <- dplyr::if_else(label=="", NA_character_, label)
           val_lab <- dplyr::if_else(val_lab=="", NA_character_, val_lab)
           
           data.frame(
             variable = namex, 
             label = label,
             levels = fct_lvls,
             value_labels = val_lab, 
             values = val)
         }) |> 
    dplyr::bind_rows() |> 
    dplyr::as_tibble() |> 
    dplyr::mutate(
      type = unlist(lapply(x, vctrs::vec_ptype_abbr)),
      .before = levels)
}


# fun: `count_prop` -------------------------------------

# compute counts and proportions
count_prop <- function(df, var, sort = FALSE) {
  df |>
    count({{ var }}, sort = sort) |>
    mutate(prop = n / sum(n),
           n_miss = sum(is.na({{ var }})))
}



# fun: `count_missing` ----------------------------------------------------



# counts the number of missing observations in rows

count_missing <- function(df, group_vars, x_var) {
  df |> 
    group_by(pick({{ group_vars }})) |> 
    summarize(
      n_miss = sum(is.na({{ x_var }})),
      .groups = "drop"
  )
}


# fun: `count_wide` ---------------------------------------------------------



# count using all the variables in the rows and columns, then use pivot_wider()
# to rearrange the counts into a grid

count_wide <- function(data, rows, cols) {
  data |> 
    count(pick(c({{ rows }}, {{ cols }}))) |> 
    pivot_wider(
      names_from = {{ cols }}, 
      values_from = n,
      names_sort = TRUE,
      values_fill = 0
    )
}




# fun: `var_label_tab` ------------------------------------------------------


# this function puts the variable name in the first column, and the variable
# label in the next
var_label_tab <- function(x){
    purrr::map(x, ~attr(., "label")) |>
    purrr::map(~ifelse(purrr::is_null(.), "No label", .)) |> 
    tibble::enframe(name = "var", value = "var_label") |>
    tidyr::unnest(cols = c(var_label))
}

# fun: logit.gof.stats ------------------------------------------------------

# custom function to return logistic regression model goodness-of-fit statistics 

# creating my own for now
# logistic regression model goodness-of-fit statistics
# NOTE: previous function call was `lrm.gof.stats`
logit.gof.stats <- function(model, ...){
  
  out <- data.frame(
    nobs = stats::nobs(model),
    null.deviance = model$null.deviance,
    df.null = model$df.null,
    logLik = as.numeric(stats::logLik(model)),
    deviance = stats::deviance(model),
    df.residual = stats::df.residual(model),
    chisq = as.numeric(model$null.deviance - stats::deviance(model)),
    df = as.numeric(model$df.null - stats::df.residual(model)),
    'P(>chi)' = pchisq(q=model$null.deviance - stats::deviance(model),
                       df = model$df.null - stats::df.residual(model), 
                       lower.tail = F)
  )
  out <- dplyr::as_tibble(out) |>  
  # rename column variables
  dplyr::rename(
    "Num.Obs."      = "nobs",
    "Log.Lik"       = "logLik",
    "Deviance"      = "deviance",
    "Deviance Null" = "null.deviance",
    "DF"            = "df",
    "chisq"          = "chisq",
    "P(>chisq)"     = "P..chi."
  )
  return(out)
}
# fun: %nin% infix --------------------------------------------------------

# %in% is currently defined as
# "%in%" <- function(x, table) match(x, table, nomatch = 0) > 0

# I want to create the opposite, %nin% or %not_in%
'%nin%' <- function(x, table) is.na(match(x, table, nomatch=NA_integer_))


# fun: calcps -------------------------------------------------------------

# function copied over from following URL
# https://tidy-survey-r.github.io/tidy-survey-book/c05-descriptive-analysis.html#example-3-purrrmap

# original function
# calcps <- function(var) {
#   anes_des |>
#     tidyr::drop_na(!!rlang::sym(var)) |>
#     srvyr::group_by(!!rlang::sym(var)) |>
#     srvyr::summarize(p = srvyr::survey_prop() * 100) |>
#     srvyr::mutate(Variable = var) |>
#     srvyr::rename(Answer := !!rlang::sym(var)) |>
#     srvyr::select(Variable, dplyr::everything())
# }

# NOTE: the `calcps` function has been generalized to allow for different data sets.
# The original function from the URL had hard-coded in the `anes_des` dataframe

calcps <- function(var, data, drop_na = TRUE, ...) {
  
  if (drop_na == TRUE){
    ps <- {{ data }} |>
    tidyr::drop_na(!!rlang::sym(var)) |>
    srvyr::group_by(!!rlang::sym(var)) |>
    srvyr::summarize(
      valid_pct = srvyr::survey_prop(...) * 100,
      unweighted_n = srvyr::unweighted(srvyr::n())               
      ) |>
    srvyr::mutate(Variable = var) |>
    srvyr::rename(Response := !!rlang::sym(var)) |>
    srvyr::select(Variable, dplyr::everything())
  } else {
    
    ps <- {{ data }} |>
    srvyr::group_by(!!rlang::sym(var)) |>
    srvyr::summarize(
      pct = srvyr::survey_prop(...) * 100,
      unweighted_n = srvyr::unweighted(srvyr::n())               
      ) |>
    srvyr::mutate(Variable = var) |>
    srvyr::rename(Response := !!rlang::sym(var)) |>
    srvyr::select(Variable, dplyr::everything())
  }
  ps
}

# fun: calcps2 ------------------------------------------------------------

# NOTE: this is an alternative function of the `calcps` function that puts `data` as the first argument and doesn't require `rlang::sym`.
# NOTE: I can't figure out how to use this with `purrr::map` as of yet.

calcps2 <- function(data, var, drop_na = TRUE, ...) {
  
  if (drop_na == TRUE){
    counts <- {{ data }} |>
    srvyr::filter(!is.na({{ var }})) |>
    srvyr::group_by({{ var }}) |>
    srvyr::summarize(valid_pct = srvyr::survey_prop(...) * 100,
                     unweighted_n = srvyr::unweighted(srvyr::n())) |>  
    srvyr::rename(Response := {{ var }}) |>
    srvyr::mutate(Variable := stringr::str_to_upper(rlang::englue("{{ var }}"))) |>
    srvyr::select(Variable, dplyr::everything()) 
  } else {
    counts <- {{ data }} |>
    srvyr::group_by({{ var }}) |>
    srvyr::summarize(pct = srvyr::survey_prop(...) * 100,
                     unweighted_n = srvyr::unweighted(srvyr::n())) |>  
    srvyr::rename(Response := {{ var }}) |>
    srvyr::mutate(Variable := stringr::str_to_upper(rlang::englue("{{ var }}"))) |>
    srvyr::select(Variable, dplyr::everything())
  }
  counts
}

# calcps(cdf_wt, nosay)



# alternate version: without the arg `drop_na`
# calcps <- function(data, var) {
#   counts <- {{ data }} |>
#     tidyr::drop_na({{ var }}) |>
#     srvyr::group_by({{ var }}) |>
#     srvyr::summarize(p = srvyr::survey_prop() * 100) |>
#     srvyr::rename(Response := {{ var }}) |>
#     srvyr::mutate(Variable := stringr::str_to_upper(rlang::englue("{{ var }}"))) |>
#     srvyr::select(Variable, dplyr::everything())
#   counts
# }


# alternate version: requires `var` argument to be given as string
# "var"
# calcps <- function(data, var) {
#   counts <- {{ data }} |>
#     tidyr::drop_na(!!rlang::sym(var)) |>
#     srvyr::group_by(!!rlang::sym(var)) |>
#     srvyr::summarize(p = srvyr::survey_prop() * 100) |> 
#     srvyr::mutate(Variable = stringr::str_to_upper(var)) |> 
#     srvyr::rename(Response := !!rlang::sym(var)) |> 
#     srvyr::select(Variable, dplyr::everything())
#   counts
# }




# fun: counts and proportions for single factor of tbl_svy object --------

# function that returns a table of counts and proportions for a single
# variable (factor) by grouping variable
# proportions are returned by default. 
# for use only with tbl_svy objects
# variables must be quoted "var"

# this function is a modified version of the one from the following url
# (https://tidy-survey-r.github.io/tidy-survey-book/c05-descriptive-analysis.html#example-3-purrrmap)

svy_count_group <- function(dat, var, group_var, props = TRUE) {
  counts <- { dat } |>  
    srvyr::drop_na({{ var }}) |> 
    srvyr::group_by({{ group_var }}, {{ var }}) |> 
    srvyr::survey_count({{ group_var }}, {{ var }}) |> 
    srvyr::mutate(Variable = stringr::str_to_upper(rlang::englue("{{ var }}"))) |> 
    srvyr::rename(Response := {{ var }}) |> 
    srvyr::select({{ group_var }}, Variable, dplyr::everything())
  
  if (props == TRUE){
    props <- { dat } |>  
    srvyr::drop_na({{ var }}) |> 
    srvyr::group_by({{ group_var }}, {{ var }}) |> 
    srvyr::summarize(prop = srvyr::survey_prop() * 100) |> 
    srvyr::mutate(Variable = stringr::str_to_upper(rlang::englue("{{ var }}"))) |> 
    srvyr::rename(Response := {{ var }}) |> 
    srvyr::select({{ group_var }}, Variable, dplyr::everything())
    
    combo <- dplyr::left_join(counts, props) |>
      dplyr::relocate(prop, .before = n_se)
    
    return(combo)
  } else {
    return(counts)
  }
}

# svy_count_group <- function(dat, var, group_var, props = TRUE) {
#   counts <- { dat } |>  
#     srvyr::drop_na(!!rlang::sym(var)) |> 
#     srvyr::group_by(!!rlang::sym(group_var), !!rlang::sym(var)) |> 
#     srvyr::survey_count(!!rlang::sym(group_var), !!rlang::sym(var)) |> 
#     srvyr::mutate(Variable = stringr::str_to_upper(var)) |> 
#     srvyr::rename(Response := !!rlang::sym(var)) |> 
#     srvyr::select(!!rlang::sym(group_var), Variable, dplyr::everything())
#   
#   if (props == TRUE){
#     props <- { dat } |>  
#     srvyr::drop_na(!!rlang::sym(var)) |> 
#     srvyr::group_by(!!rlang::sym(group_var), !!rlang::sym(var)) |> 
#     srvyr::summarize(prop = srvyr::survey_prop() * 100) |> 
#     srvyr::mutate(Variable = stringr::str_to_upper(var)) |> 
#     srvyr::rename(Response := !!rlang::sym(var)) |> 
#     srvyr::select(!!rlang::sym(group_var), Variable, dplyr::everything())
#     
#     combo <- dplyr::left_join(counts, props) |>
#       dplyr::relocate(prop, .before = n_se)
#     
#     return(combo)
#   } else {
#     return(counts)
#   }
# }


# fun: Likert Plots -------------------------------------------------------

# To avoid excessive copy/paste, I made a function to create Likert plots
# it is mostly a wrapper around `ggstats::gglikert`, but with a specific set up
# particular colors, theme, etc. 
likert_plot <- function(data,
                        x,
                        ...,
                        symmetric = FALSE,
                        variable_labels = NULL,
                        label_size = 2.7,
                        vline = FALSE,
                        title = waiver(),
                        subtitle = waiver(),
                        caption = waiver(),
                        xlab = waiver()) {
  p <- ggstats::gglikert(
    data = data,
    include = {{ x }},
    ...,
    variable_labels = variable_labels,
    symmetric = symmetric
  ) +
    
    # customize color
    ggplot2::scale_fill_grey(start = 0.5, end = 0.1) +
    # custom theme
    theme_bw(base_family = "TeX Gyre Pagella") +
    theme(
      legend.position = 'bottom',
      # place legend on bottom
      axis.text.x = element_blank(),
      # remove percentage text along x-axis
      strip.text.y.right = element_text(angle = 0) # make facet label horizontal
    )
  
  if (vline == TRUE) {
    p <- p + geom_vline(
      xintercept = 0,
      color = 'black',
      linewidth = 1.2
    )
  } else {
    p
  }
  p <- p + ggplot2::labs(
    ...,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = xlab
  )
  p
  
}

# working example:
# df |> likert_plot(
#   x = trust.az.items,
#   variable_labels = trust.az.varlabels,
#   title = "Trust in Maricopa County, AZ, Elections"
# )

# working example: x and y var
# df |> likert_plot(
#   x = trust.az.items,
#   y = 'group',
#   facet_rows = vars(.question),
#   facet_label_wrap = 15,
#   variable_labels = trust.az.varlabels,
#   title = "Trust in Maricopa County, AZ, Elections"
# )


# fun: custom barplot -----------------------------------------------------

custom_barplot <- function(data,
                           x,
                           group,
                           title = NULL,
                           subtitle = NULL,
                           caption = NULL,
                           legend_fill = NULL,
                           xlab = NULL) {
  b <- data |>
    group_by({{ group }}, {{ x }}) |>
    count() |>
    drop_na() |>
    group_by({{ group }}) |>
    mutate(
      prop = round(n / sum(n), digits = 3),
      pct = prop * 100,
      res = str_c(pct, '% (', n, ')', sep = "")
    ) |>
    ggplot(aes(x = {{ x }}, y = pct, fill = {{ group }})) +
    geom_bar(position = 'dodge', stat = 'identity') +
    geom_text(
      aes(label = res),
      position = position_dodge(1.0),
      size = 2.5,
      vjust = -0.5
    ) +
    scale_fill_grey(start = 0.5, end = 0.1)
  
  b <- b + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = stringr::str_wrap(caption, width = 99),
    fill = legend_fill,
    y = "Percentage",
    x = stringr::str_wrap(xlab, width = 85)
  )
  b
}


# fun: nest_paragraphs ----------------------------------------------------

# I have no idea what this does at the moment. I might not need it at all.

# stolen from `ggpage` R package
# https://github.com/EmilHvitfeldt/ggpage/blob/master/R/nest_paragraphs.R

nest_paragraphs <- function(data, input, ...) {
  quo_input <- rlang::quo_name(rlang::enquo(input))

  sections <- data[[quo_input]] %>%
    stringr::str_wrap(...) %>%
    stringr::str_split("\n")

  purrr::map_df(seq_len(nrow(data)),
                ~ data.frame(text = sections[[.x]], stringsAsFactors = FALSE) %>%
                  bind_cols(
                    bind_rows(
                      replicate(
                        length(sections[[.x]]),
                        dplyr::select(data, -which(names(data) == quo_input))[.x, ],
                        simplify = FALSE)
                      )
                    )
                )
}

# fun: `continuous_stats` -------------------------------------------------

# function derived from: 
# https://brad-cannell.github.io/r4epi_quarto/chapters/using_purrr/using_purrr.html#example-1-continuous-statistics

# I modified this function slightly for general use. 
# Original function commented below

continuous_stats <- function(data, var) {
  data |>  
    dplyr::summarise(
      variable = rlang::as_name(var), # Add variable name to the output
      n_miss   = sum(is.na({{ var }})),
      mean     = mean({{ var }}, na.rm = TRUE),
      median   = median({{ var }}, na.rm = TRUE),
      min      = min({{ var }}, na.rm = TRUE),
      max      = max({{ var }}, na.rm = TRUE)
    ) 
}

# continuous_stats <- function(var) {
#   study %>% 
#     summarise(
#       variable = quo_name(var), # Add variable name to the output
#       n_miss   = sum(is.na({{ var }})),
#       mean     = mean({{ var }}, na.rm = TRUE),
#       median   = median({{ var }}, na.rm = TRUE),
#       min      = min({{ var }}, na.rm = TRUE),
#       max      = max({{ var }}, na.rm = TRUE)
#     ) 
# }



# fun: `categorical_stats` -----------------------------------------------------

# this might just be a simple wrapper.

# categorical_stats <- function(data, x){
#   data |> 
#     dplyr::count({{ x }}) |> 
#     dplyr::mutate(variable = names(.)[1]) |> 
#     dplyr::rename(category = 1) |> 
#     dplyr::select(variable, category, n)
# }
# 

# This is for later. Refer again to this page (scroll down to categorical example)
# https://brad-cannell.github.io/r4epi_quarto/chapters/using_purrr/using_purrr.html#example-1-continuous-statistics

# purrr::map_dfr(
#   .x = rlang::quos(age_group, gender, bmi_3cat),
#   .f = function(x, data = data) {
#     data |> 
#       count({{ x }}) %>% 
#       mutate(variable = names(.)[1]) %>% 
#       rename(category = 1) %>% 
#       select(variable, category, n)
#   }
# )

# We haven’t seen the quos() function before. It’s another one of those tidy
# evaluation functions. You can type ?rlang::quos in your console to read more
# about it.

# When we can wrap a single column name with the quo() function, or a list of
# column names with the quos() function, we are telling R to look for them in
# the data frame being passed to a dplyr verb rather than looking for them as
# objects in the global environment.