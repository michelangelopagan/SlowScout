gt_shading <- function(gtable, zero=FALSE) {
  if (!zero) {
    try({
      gtable <-
        gtable %>%
        tab_style(
          locations = cells_body(
            columns = `2P%`,
            rows = (`2P%` >= filter(shading_df, Stat == "2P%")$Upper)
          ),
          style = list(cell_fill(color = "#dbf1e5"))
        ) %>%
        tab_style(
          locations = cells_body(
            columns = `2P%`,
            rows = (`2P%` <= filter(shading_df, Stat == "2P%")$Lower)
          ),
          style = list(cell_fill(color = "#fdf2f0"))
        )
    },
    silent=TRUE)
    
    try({
      gtable <-
        gtable %>%
        tab_style(
          locations = cells_body(
            columns = `3P%`,
            rows = (`3P%` >= filter(shading_df, Stat == "3P%")$Upper)
          ),
          style = list(cell_fill(color = "#dbf1e5"))
        ) %>%
        tab_style(
          locations = cells_body(
            columns = `3P%`,
            rows = (`3P%` <= filter(shading_df, Stat == "3P%")$Lower)
          ),
          style = list(cell_fill(color = "#fdf2f0"))
        )
    },
    silent=TRUE)
    
    try({
      gtable <-
        gtable %>%
        tab_style(
          locations = cells_body(
            columns = `FG%`,
            rows = (`FG%` >= filter(shading_df, Stat == "FG%")$Upper)
          ),
          style = list(cell_fill(color = "#dbf1e5"))
        ) %>%
        tab_style(
          locations = cells_body(
            columns = `FG%`,
            rows = (`FG%` <= filter(shading_df, Stat == "FG%")$Lower)
          ),
          style = list(cell_fill(color = "#fdf2f0"))
        )
    },
    silent=TRUE)
  } else {
    try({
      gtable <-
        gtable %>%
        tab_style(
          locations = cells_body(
            columns = `FG%`,
            rows = ((`FG%` >= filter(shading_df, Stat == "FG%")$Upper) & (`FG` != "0-0"))
          ),
          style = list(cell_fill(color = "#dbf1e5"))
        ) %>%
        tab_style(
          locations = cells_body(
            columns = `FG%`,
            rows = ((`FG%` <= filter(shading_df, Stat == "FG%")$Lower) & (`FG` != "0-0"))
          ),
          style = list(cell_fill(color = "#fdf2f0"))
        )
    },
    silent=TRUE)
    
    try({
      gtable <-
        gtable %>%
        tab_style(
          locations = cells_body(
            columns = `2P%`,
            rows = ((`2P%` >= filter(shading_df, Stat == "2P%")$Upper) & (`2P` != "0-0"))
          ),
          style = list(cell_fill(color = "#dbf1e5"))
        ) %>%
        tab_style(
          locations = cells_body(
            columns = `2P%`,
            rows = ((`2P%` <= filter(shading_df, Stat == "2P%")$Lower) & (`2P` != "0-0"))
          ),
          style = list(cell_fill(color = "#fdf2f0"))
        )
    },
    silent=TRUE)
    
    try({
      gtable <-
        gtable %>%
        tab_style(
          locations = cells_body(
            columns = `3P%`,
            rows = ((`3P%` >= filter(shading_df, Stat == "3P%")$Upper) & (`3P` != "0-0"))
          ),
          style = list(cell_fill(color = "#dbf1e5"))
        ) %>%
        tab_style(
          locations = cells_body(
            columns = `3P%`,
            rows = ((`3P%` <= filter(shading_df, Stat == "3P%")$Lower) & (`3P` != "0-0"))
          ),
          style = list(cell_fill(color = "#fdf2f0"))
        )
    },
    silent=TRUE)
  }
  
  try({
    gtable <-
      gtable %>%
      tab_style(
        locations = cells_body(
          columns = `FT%`,
          rows = (`FT%` >= filter(shading_df, Stat == "FT%")$Upper)
        ),
        style = list(cell_fill(color = "#dbf1e5"))
      ) %>%
      tab_style(
        locations = cells_body(
          columns = `FT%`,
          rows = (`FT%` <= filter(shading_df, Stat == "FT%")$Lower)
        ),
        style = list(cell_fill(color = "#fdf2f0"))
      )
  },
  silent=TRUE)
  
  try({
    gtable <-
      gtable %>%
      tab_style(
        locations = cells_body(
          columns = `ORB%`,
          rows = (`ORB%` >= filter(shading_df, Stat == "ORB%")$Upper)
        ),
        style = list(cell_fill(color = "#dbf1e5"))
      ) %>%
      tab_style(
        locations = cells_body(
          columns = `ORB%`,
          rows = (`ORB%` <= filter(shading_df, Stat == "ORB%")$Lower)
        ),
        style = list(cell_fill(color = "#fdf2f0"))
      )
  },
  silent=TRUE)
  
  try({
    gtable <-
      gtable %>%
      tab_style(
        locations = cells_body(
          columns = `OPP ORB%`,
          rows = (`OPP ORB%` >= filter(shading_df, Stat == "OPP ORB%")$Upper)
        ),
        style = list(cell_fill(color = "#fdf2f0"))
      ) %>%
      tab_style(
        locations = cells_body(
          columns = `OPP ORB%`,
          rows = (`OPP ORB%` <= filter(shading_df, Stat == "OPP ORB%")$Lower)
        ),
        style = list(cell_fill(color = "#dbf1e5"))
      )
  },
  silent=TRUE)
  
  try({
    gtable <-
      gtable %>%
      tab_style(
        locations = cells_body(
          columns = `DRB%`,
          rows = (`DRB%` >= filter(shading_df, Stat == "DRB%")$Upper)
        ),
        style = list(cell_fill(color = "#dbf1e5"))
      ) %>%
      tab_style(
        locations = cells_body(
          columns = `DRB%`,
          rows = (`DRB%` <= filter(shading_df, Stat == "DRB%")$Lower)
        ),
        style = list(cell_fill(color = "#fdf2f0"))
      )
  },
  silent=TRUE)
  
  try({
    gtable <-
      gtable %>%
      tab_style(
        locations = cells_body(
          columns = `OPP DRB%`,
          rows = (`OPP DRB%` >= filter(shading_df, Stat == "OPP DRB%")$Upper)
        ),
        style = list(cell_fill(color = "#fdf2f0"))
      ) %>%
      tab_style(
        locations = cells_body(
          columns = `OPP DRB%`,
          rows = (`OPP DRB%` <= filter(shading_df, Stat == "OPP DRB%")$Lower)
        ),
        style = list(cell_fill(color = "#dbf1e5"))
      )
  },
  silent=TRUE)
  
  try({
    gtable <-
      gtable %>%
      tab_style(
        locations = cells_body(
          columns = `REB%`,
          rows = (`REB%` >= filter(shading_df, Stat == "REB%")$Upper)
        ),
        style = list(cell_fill(color = "#dbf1e5"))
      ) %>%
      tab_style(
        locations = cells_body(
          columns = `REB%`,
          rows = (`REB%` <= filter(shading_df, Stat == "REB%")$Lower)
        ),
        style = list(cell_fill(color = "#fdf2f0"))
      )
  },
  silent=TRUE)
  
  try({
    gtable <-
      gtable %>%
      tab_style(
        locations = cells_body(
          columns = `OPP REB%`,
          rows = (`OPP REB%` >= filter(shading_df, Stat == "OPP REB%")$Upper)
        ),
        style = list(cell_fill(color = "#fdf2f0"))
      ) %>%
      tab_style(
        locations = cells_body(
          columns = `OPP REB%`,
          rows = (`OPP REB%` <= filter(shading_df, Stat == "OPP REB%")$Lower)
        ),
        style = list(cell_fill(color = "#dbf1e5"))
      )
  },
  silent=TRUE)
  
  try({
    gtable <-
      gtable %>%
      tab_style(
        locations = cells_body(
          columns = `TO%`,
          rows = (`TO%` >= filter(shading_df, Stat == "TO%")$Upper)
        ),
        style = list(cell_fill(color = "#fdf2f0"))
      ) %>%
      tab_style(
        locations = cells_body(
          columns = `TO%`,
          rows = (`TO%` <= filter(shading_df, Stat == "TO%")$Lower)
        ),
        style = list(cell_fill(color = "#dbf1e5"))
      )
  },
  silent=TRUE)
  
  try({
    gtable <-
      gtable %>%
      tab_style(
        locations = cells_body(
          columns = `OPP TO%`,
          rows = (`OPP TO%` >= filter(shading_df, Stat == "OPP TO%")$Upper)
        ),
        style = list(cell_fill(color = "#dbf1e5"))
      ) %>%
      tab_style(
        locations = cells_body(
          columns = `OPP TO%`,
          rows = (`OPP TO%` <= filter(shading_df, Stat == "OPP TO%")$Lower)
        ),
        style = list(cell_fill(color = "#fdf2f0"))
      )
  },
  silent=TRUE)
  
  try({
    gtable <-
      gtable %>%
      tab_style(
        locations = cells_body(
          columns = `FT-R`,
          rows = (`FT-R` >= filter(shading_df, Stat == "FT-R")$Upper)
        ),
        style = list(cell_fill(color = "#dbf1e5"))
      ) %>%
      tab_style(
        locations = cells_body(
          columns = `FT-R`,
          rows = (`FT-R` <= filter(shading_df, Stat == "FT-R")$Lower)
        ),
        style = list(cell_fill(color = "#fdf2f0"))
      )
  },
  silent=TRUE)
  
  try({
    gtable <-
      gtable %>%
      tab_style(
        locations = cells_body(
          columns = `OPP FT-R`,
          rows = (`OPP FT-R` >= filter(shading_df, Stat == "OPP FT-R")$Upper)
        ),
        style = list(cell_fill(color = "#fdf2f0"))
      ) %>%
      tab_style(
        locations = cells_body(
          columns = `OPP FT-R`,
          rows = (`OPP FT-R` <= filter(shading_df, Stat == "OPP FT-R")$Lower)
        ),
        style = list(cell_fill(color = "#dbf1e5"))
      )
  },
  silent=TRUE)
  
  try({
    gtable <-
      gtable %>%
      tab_style(
        locations = cells_body(
          columns = `PPP`,
          rows = (`PPP` >= filter(shading_df, Stat == "PPP")$Upper)
        ),
        style = list(cell_fill(color = "#dbf1e5"))
      ) %>%
      tab_style(
        locations = cells_body(
          columns = `PPP`,
          rows = (`PPP` <= filter(shading_df, Stat == "PPP")$Lower)
        ),
        style = list(cell_fill(color = "#fdf2f0"))
      )
  },
  silent=TRUE)
  
  try({
    gtable <-
      gtable %>%
      tab_style(
        locations = cells_body(
          columns = `OPP PPP`,
          rows = (`OPP PPP` >= filter(shading_df, Stat == "OPP PPP")$Upper)
        ),
        style = list(cell_fill(color = "#fdf2f0"))
      ) %>%
      tab_style(
        locations = cells_body(
          columns = `OPP PPP`,
          rows = (`OPP PPP` <= filter(shading_df, Stat == "OPP PPP")$Lower)
        ),
        style = list(cell_fill(color = "#dbf1e5"))
      )
  },
  silent=TRUE)
  
  try({
    gtable <-
      gtable %>%
      tab_style(
        locations = cells_body(
          columns = `PPS`,
          rows = (`PPS` >= filter(shading_df, Stat == "PPS")$Upper)
        ),
        style = list(cell_fill(color = "#dbf1e5"))
      ) %>%
      tab_style(
        locations = cells_body(
          columns = `PPS`,
          rows = (`PPS` <= filter(shading_df, Stat == "PPS")$Lower)
        ),
        style = list(cell_fill(color = "#fdf2f0"))
      )
  },
  silent=TRUE)
  
  try({
    gtable <-
      gtable %>%
      tab_style(
        locations = cells_body(
          columns = `AST/TO`,
          rows = (`AST/TO` >= filter(shading_df, Stat == "AST/TO")$Upper)
        ),
        style = list(cell_fill(color = "#dbf1e5"))
      ) %>%
      tab_style(
        locations = cells_body(
          columns = `AST/TO`,
          rows = (`AST/TO` <= filter(shading_df, Stat == "AST/TO")$Lower)
        ),
        style = list(cell_fill(color = "#fdf2f0"))
      )
  },
  silent=TRUE)
  
  gtable
}

dt_shading = function(df) {
  
  counter = 0
  for (stat in shading_df$Stat) {
    if (stat %in% colnames(df)) {
      counter = counter + 1
      
      lower = filter(shading_df, Stat == stat)$Lower
      upper = filter(shading_df, Stat == stat)$Upper
      
      df[ paste0(stat, "_GOAL") ] = ifelse(df[stat] >= upper, 1,
                                           ifelse(df[stat] <= lower, -1,
                                                  0))
    }
  }
  
  dt =
    df %>%
    datatable(
      rownames=F,
      options = list(
        columnDefs = list(
          list(targets = tail(seq(ncol(df)), counter) - 1, visible=F)
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$('body') .css({'font-family': 'Helvetica'});",
          "}")
      ),
      escape=F)
  
  for (stat in shading_df$Stat) {
    if (stat %in% colnames(df)) {
      dt =
        dt %>%
        formatStyle(
          columns = stat,
          valueColumns = paste0(stat, "_GOAL"),
          backgroundColor = styleEqual(levels=c(-1, 0, 1), 
                                       values=c("#fdf2f0","white","#dbf1e5")))
    }
  }
  
  dt
}