read_preset <- function(preset,arglist = FALSE,...) {

  extra_args <- list(...)

  paramlist <- fromJSON("presets.json")

  if (missing(preset)) {

    stop(paste0("Argument 'preset' must be defined. Available values are:\n\t",
               paste(names(paramlist),
               collapse = "\n\t")))

  } else if (length(preset) > 1) {

    stop("Argument 'preset' must have length 1")

  } else if (!preset %in% names(paramlist)) {

    stop(paste0("'",preset,"' is not a valid value for argument 'preset'. Available values are:\n\t",
               paste(names(paramlist),
               collapse = "\n\t")))
  }

  if (arglist == TRUE) {

    return(message(paste0("Preset '",preset,"' takes the following arguments:\n\t",
                  paste(sapply(paramlist[[preset]]$path$argument, '[', 'name'),
                  ": ",sapply(paramlist[[preset]]$path$argument, '[', 'description'),
                  collapse = "\n\t"))))

  }

  if (names(extra_args) != names(paramlist[[preset]]$path$arguments) ||
             length(names(extra_args)) != length(names(paramlist[[preset]]$path$arguments))) {

    stop(paste0("Preset '",preset,"' must include the following arguments:\n\t",
                paste(names(paramlist[[preset]]$path$arguments),
                      collapse = "\n\t")))
  }

  paramlist <- paramlist[[preset]]

  for (i in 1:length(paramlist$path$arguments)) {

    assign(paramlist$path$arguments[[i]]$name,
           extra_args[[paramlist$path$arguments[[i]]$name]])

  }

  read_cases_to_data(format = paramlist$format,
                        path = eval(parse(text=paramlist$path)),
                        varlist = paramlist$varlist)
}

read_preset(preset="isp_pop",arglist=TRUE)
