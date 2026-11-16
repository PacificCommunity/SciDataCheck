# test_flag <- NULL

# is there only ever going to be one ace file?

ace_byocean <- function(year) {
  gears <-
    read_dataset(year, "ace") |>
    dplyr::pull(gear_code) |>
    unique()

  ace_byocean_single <- function(year = year, gear = gear) {
    read_dataset(year, "ace") |>
      dplyr::filter(gear_code == gear) |>
      dplyr::arrange(retain_mt) |>
      dplyr::mutate(sp_code = forcats::fct_inorder(sp_code)) |>
      tidyr::pivot_longer(cols = c(retain_mt, discard_mt)) |>
      ggplot2::ggplot() +
      ggplot2::aes(y = sp_code, x = value, fill = name) +
      ggplot2::geom_col() +
      ggplot2::facet_wrap(~ocean_code) +
      ggplot2::labs(x = "Catch (mt)", y = NULL, title = gear) +
      ggplot2::scale_x_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      ggplot2::theme_bw(20)
  }

  plots <- purrr::map2(year, gears, ace_byspp_single) |>
    rlang::set_names(paste(year, gears, sep = "_"))

  return(plots)
}


ace_byspp <- function(year) {
  gears <-
    read_dataset(year, "ace") |>
    dplyr::pull(gear_code) |>
    unique()

  ace_byspp_single <- function(year = year, gear = gear) {
    read_ace(year) |>
      dplyr::filter(gear_code == gear) |>
      dplyr::mutate(
        grouping = dplyr::case_when(
          ocean_code %in% c("NP", "SP") ~ "NP+SP",
          ocean_code %in% c("NX", "SX") ~ "NX+SX",
          .default = ocean_code
        )
      ) |>
      tidyr::pivot_longer(c(retain_mt, discard_mt)) |>
      dplyr::filter(sum(value) > 0, .by = sp_code) |>
      dplyr::mutate(
        ocean_code = factor(ocean_code),
        name = factor(
          name,
          levels = c("retain_mt", "discard_mt")
        ),
        sp_code = forcats::fct_reorder(
          sp_code,
          value,
          .fun = sum,
          .desc = TRUE
        ),
        stack_order = as.integer(interaction(ocean_code, name))
      ) |>
      ggplot2::ggplot() +
      ggplot2::aes(
        y = grouping,
        x = value,
        fill = ocean_code,
        alpha = name,
        group = stack_order
      ) +
      ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::facet_wrap(~sp_code, scales = "free_x", ncol = 4) +
      ggplot2::scale_alpha_manual(values = c(retain_mt = 1, discard_mt = 0.3)) +
      ggplot2::scale_x_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      ggplot2::labs(
        x = "Catch (mt)",
        y = NULL,
        title = NULL,
        alpha = NULL,
        fill = NULL
      ) +
      ggplot2::theme_bw(11) +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 9)) +
      ggplot2::theme(legend.position = "bottom")
  }

  plots <- purrr::map2(year, gears, ace_byspp_single) |>
    rlang::set_names(gears)

  return(plots)
}
