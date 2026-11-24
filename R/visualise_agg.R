plot_agg <- function(year, primary_path) {
  agg_gears <-
    fs::dir_ls(
      path = glue::glue("{year}/submissions/"),
      regexp = "agg_catch"
    ) |>
    list.files(full.names = TRUE) |>
    stringr::str_extract("(?<=agg_catch_)[a-zA-Z]{2}")

  agg_plot1_single <- function(
    gear = gear,
    save = TRUE
  ) {
    p <-
      read_dataset(year = year, type = glue::glue("agg_catch_{gear}")) |>
      ggplot2::ggplot() +
      {
        if (gear == "ll") {
          list(
            ggplot2::aes(x = sp_n, y = sp_mt, col = as.factor(is_discarded)),
            ggplot2::geom_point(),
            ggplot2::labs(
              x = "Catch (numbers)",
              y = "Catch (mt)"
            )
          )
        } else {
          list(
            ggplot2::aes(x = log(sp_mt), col = as.factor(is_discarded)),
            ggplot2::geom_density(),
            ggplot2::labs(
              x = "Catch (mt), Log-scale"
            )
          )
        }
      } +
      ggplot2::facet_wrap(~sp_code, scales = "free", ncol = 4) +
      ggplot2::scale_x_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      ggplot2::theme_bw(11) +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 9)) +
      ggplot2::theme(legend.position = "bottom")

    if (save) {
      ggplot2::ggsave(
        filename = file.path(primary_path, glue::glue("agg_catch_{gear}.png")),
        plot = p,
        height = 10,
        width = 12
      )
    }
  }

  purrr::walk(agg_gears, \(g) agg_plot1_single(gear = g))
}
