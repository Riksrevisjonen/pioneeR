sysfonts::font_add_google('Zilla Slab', 'pf', regular.wt = 500)
fig <- magick::image_read_svg('inst/logo/pilgrim-blue.svg')

hexSticker::sticker(
  subplot = fig,
  package = 'pioneeR',
  p_size = 27,
  p_color = '#183271',
  p_x = 1,
  p_y = 1.4,
  p_family = 'pf',
  s_width = 0.9,
  s_height = 0.9,
  s_x = 0.97,
  s_y = .75,
  h_fill = '#E9F8FF',
  h_color = '#183271',
  h_size = 1.5,
  dpi = 320,
  filename = 'man/figures/logo.png') |> print()

usethis::use_logo(img = 'man/figures/logo.png')
