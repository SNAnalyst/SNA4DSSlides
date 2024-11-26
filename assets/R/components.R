

`%||%` <- function (x, y) {
  if (is.null(x)) 
    y
  else x
}


`%>%` <- magrittr::`%>%`



htmlent <- list(
  "alarm_clock" = "&#9200;",
  "done" = "&#10004;",
  "envelope" = "&#9993;",
  "hourglass" = "&#8987;",
  "mailbox" = "&#128237;",
  "pin" = "&#128204;",
  "play_backward" = "&#9193;",
  "play_forward" = "&#9194;",
  "play_next" = "&#9197;",
  "play_pause" = "&#9199;",
  "play_previous" = "&#9198;",
  "pumpkinface" = "&#127875;",
  "right_arrow_heavy" = "&#10142;",
  "right_arrow_big" = "&#10145;",
  "right_Arrow" = "&rArr;",
  "right_arrow" = "&rarr;",
  "stopwatch" = "&#9200;",
  "text_balloon_dots" = "&#128172;",
  "text_balloon_empty" = "&#128173;",
  "triangle_up" = "&#9650;",
  "triangle_up_small" = "&#9652;",
  "triangle_right" = "&#9654;",
  "triangle_right_small" = "&#9656;",
  "triangle_down" = "&#9660;",
  "triangle_down_small" = "&#9662;",
  "triangle_left" = "&#9664;",
  "triangle_left_small" = "&#9666;",
  "watch" = "&#8986;",
  "x" = "&#10006;"
)



spaces <- function(aantal = 1) {
  rep.int("&nbsp;", times = aantal) |> noquote()
}


rewrite_chunk <- function(label, include_label = FALSE, nbt = 3) {
  chunk <- knitr::knit_code$get(label)
  opts <- attr(chunk, "chunk_opts")
  bt <- strrep("`", nbt)
  engine <- opts$engine %||% "r"
  exclude <- c("engine")
  if (is.logical(include_label) && !isTRUE(include_label)) {
    exclude <- c(exclude, "label")
  }
  if (is.character(include_label)) {
    label <- include_label
  }
  opts <- opts[setdiff(names(opts), exclude)]
  opts <- paste(
    vapply(names(opts), FUN.VALUE = character(1), function(on) {
      paste0(
        if (on != "label") paste(on, "= "), 
        if (is.character(opts[[on]])) {
          if (on == "label") label else dQuote(opts[[on]], q = 0) 
        } else {
          deparse(opts[[on]])
        }
      )
    }),
    collapse = ", "
  )
  header <- paste0(bt, "{", engine, if (length(opts) && nzchar(opts)) " ", opts, "}")
  paste(
    header,
    paste(chunk, collapse = "\n"),
    bt,
    sep = "\n"
  )
}



fullscreen_bg_image <- function(
  path,
  size = "cover",
  class = NULL,
  position = "top left"
) {
  class <- paste(class, collapse = " ")
  glue::glue(
    "class: fullscreen {class}
    background-image: url('{path}')
    background-size: {size}
    background-position: {position}
    "
  )
}


iframe_slide_fullscreen <- function(path, scale = 1) {
  glue::glue(
    "
    class: fullscreen

    {iframe_fullscreen(path, scale)}
    "
  )
}

iframe_fullscreen <- function(path, scale = 1) {
  scale <- as.numeric(scale)
  scale <- switch(
    sprintf("%1.2f", scale),
    "1.25" = "-scale-125",
    "1.50" = "-scale-150",
    "2.00" = "-scale-200",
    ""
  )
  iframe <- if (isTRUE(getOption("iframe_placeholder", FALSE))) {
    glue::glue('content from <a href="{path}">{path}</a> here')
  } else {
    glue::glue(
      '<div data-iframe data-src="{path}" class="fullscreen-iframe{scale}">
       <a href="{path}">iframe content</a>
    </div>'
    )
  }
  glue::glue(
    '<div class="fullscreen-iframe-container">{iframe}</div>'
  )
}


repl_iframe_url <- function(code, local = getOption("js4shiny_repl_local", TRUE)) {
  code <- httpuv::encodeURIComponent(jsonlite::toJSON(code))
  base_url <- if (local) "http://127.0.0.1:9876" else "https://apps.garrickadenbuie.com/u/garrick/repljs"
  glue::glue("{base_url}/?_inputs_&code_js={code}")
}


repl_iframe <- function(code, class = NULL, ...) {
  class <- if (!is.null(class)) paste0(" ", paste(class, collapse = "\n")) else ""
  glue::glue(
    "class: class",
    "",
    "```js",
    "{code}",
    "```",
    .sep = "\n"
  )
}




slides_from_images <- function(
  path = "assets/images",
  regexp = NULL,
  class = "hide-count",
  background_size = "contain",
  background_position = "top left"
) {
  if (isTRUE(getOption("slide_image_placeholder", FALSE))) {
    return(glue::glue("Slides to be generated from [{path}]({path})"))
  }
  if (fs::is_dir(path)) {
    imgs <- fs::dir_ls(path, regexp = regexp, type = "file", recurse = FALSE)
  } else if (all(fs::is_file(path) && fs::file_exists(path))) {
    imgs <- path
  } else {
    stop("path must be a directory or a vector of images")
  }
  imgs <- fs::path_rel(imgs, ".")
  breaks <- rep("\n---\n", length(imgs))
  breaks[length(breaks)] <- ""

  txt <- glue::glue("

  class: {class}
  background-image: url('{imgs}')
  background-size: {background_size}
  background-position: {background_position}

  {breaks}

  ")

  paste(txt, sep = "", collapse = "")
}


unsplash <- function(id, size = "1920x1080") {
  size <- if (!is.null(size)) glue::glue("/{size}") else ""
  glue::glue("https://source.unsplash.com/{id}{size}")
}


unsplash_bg <- function(id, size = "1920x1080") {
  glue::glue(
    "background-image: url('{unsplash(id, size)}')
    background-size: cover"
  )
}


unsplash_get <- function(id, size = "1920x1080", path = here::here("static/slides/assets/img/bg")) {
  url <- unsplash(id, size)
  file <- glue::glue("unsplash_{id}.jpg")
  download.file(url, fs::path(path, file))
  code_img <- glue::glue("background-image: url('assets/img/bg/{file}')")
  code <- paste(code_img, "background-size: cover", sep = "\n")
  if (clipr::clipr_available()) clipr::write_clip(code)
  message(code)
  invisible(code_img)
}


chunk_file_name <- function(...) {
  name <- paste0(..., collapse = "")
  glue::glue('<div class="pre-name">{name}</div>')
}


list_all_assets <- function(only_files = FALSE, ext = "jpg|png|gif|mov|mp4|svg") {
  rmds <- fs::dir_ls(here::here(), regexp = "Rmd$", recurse = TRUE)
  pattern <- glue::glue(
    "assets/{file}[.]({ext})",
    file = if (only_files) "[^ ]+" else ".+"
  )
  rmds %>%
    purrr::set_names() %>%
    purrr::map(js4shiny:::read_lines) %>%
    purrr::map(stringr::str_extract, pattern = pattern) %>%
    purrr::map(~ .[!is.na(.)])
}


jump_to <- function(anchor = NULL) {
  if (is.null(anchor)) {
    return(invisible())
  }
  htmltools::tags$div(
    class = "f4 ma0 pa0 hover-show o-40 pointer absolute right-1 bottom-1",
    htmltools::tags$a(href = paste0("#", anchor), "&#x23ED;")
  )
}

live_coding <- function(
  emoji = "&#x1F468;&#x1F3FC;&#x200D;&#x1F4BB;",
  class = "break center middle",
  message = "[live coding]"
) {
  x <- glue::glue(
    " class: {class}

  .f1[{emoji}]<br>
  .code.f6[{message}]
  "
  )
  knitr::asis_output(x)
}


############################################################################ svg
# set fontawesome icon colours

h_col <- function(header_level){
  ifelse(header_level == "default", "#CA3E34",
         ifelse(header_level == "h2", "#2C5577",
                ifelse(header_level == "h3", "#136CB9",
                       ifelse(header_level == "h4", "#CA3E34",
                              ifelse(header_level == "tip", "#2C5577",
                                     ifelse(header_level == "warning", "#775418", "#2F4F4F"))))))
}


# set fontawesome icon class

set_svg <- function(x){
  svg <- htmltools::HTML(x)
  class(svg) <- c("fontawesome", class(svg))
  svg
}

# set fontawesome icon size and fill properties

set_svg_props <- function(x, fill, height){
  x <- gsub("fill: fill", paste("fill:", h_col(fill)), x)
  if(height != 1){
    x <- gsub("height: 1em", paste0("height:", height, "em"), x)
  }
  set_svg(x)
}

# set fontawesome print properties

knit_print.fontawesome <- function(x, ...) { # nocov start
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::knit_print(as.character(x), ...)
  }
}

rproj <- function(fill = "h3", height = 1){
  code <- "<svg style=\"height: 1em; top:.04em; position: relative; fill: fill;\" viewBox=\"0 0 581 512\"><path d=\"M581 226.6C581 119.1 450.9 32 290.5 32S0 119.1 0 226.6C0 322.4 103.3 402 239.4 418.1V480h99.1v-61.5c24.3-2.7 47.6-7.4 69.4-13.9L448 480h112l-67.4-113.7c54.5-35.4 88.4-84.9 88.4-139.7zm-466.8 14.5c0-73.5 98.9-133 220.8-133s211.9 40.7 211.9 133c0 50.1-26.5 85-70.3 106.4-2.4-1.6-4.7-2.9-6.4-3.7-10.2-5.2-27.8-10.5-27.8-10.5s86.6-6.4 86.6-92.7-90.6-87.9-90.6-87.9h-199V361c-74.1-21.5-125.2-67.1-125.2-119.9zm225.1 38.3v-55.6c57.8 0 87.8-6.8 87.8 27.3 0 36.5-38.2 28.3-87.8 28.3zm-.9 72.5H365c10.8 0 18.9 11.7 24 19.2-16.1 1.9-33 2.8-50.6 2.9v-22.1z\"/></svg>"
  set_svg_props(code, fill, height)
}

rstudio <- function(height = 1){
  img_text <- "<img src=\"./images/rstudio_logo.png\" alt = \"The R-project logo.\" class=\"inline-image\">"
  htmltools::HTML(img_text)
}


insert_img <- function(location){
  img_text <- paste0('<img src="', location, '" class="inline-image">')
  htmltools::HTML(img_text)
}

insert_img2 <- function(location, width = 640){
  img_text <- paste0('<img src="', location, '" class="inline-image" width="', width, '">')
  htmltools::HTML(img_text)
}




smilebeam <- function(fill = "h3", height = 1) {
  code <- "<svg style=\"height: 1em; top:.04em; position: relative; fill: fill;\"
viewBox=\"0 0 32 32\"><path d=\"M 16 3 C 8.832 3 3 8.832 3 16 C 3 23.168 8.832 29 16 29 C 23.168 29 29 23.168 29 16 C 29 8.832 23.168 3 16 3 z M 16 5 C 22.065 5 27 9.935 27 16 C 27 22.065 22.065 27 16 27 C 9.935 27 5 22.065 5 16 C 5 9.935 9.935 5 16 5 z M 11 12 C 8.906 12 7.390625 13.207031 7.390625 13.207031 L 8.609375 14.792969 C 8.609375 14.792969 9.6929531 14 11.001953 14 C 12.310953 14 13.392578 14.792969 13.392578 14.792969 L 14.611328 13.207031 C 14.609328 13.207031 13.094 12 11 12 z M 21 12 C 18.906 12 17.390625 13.207031 17.390625 13.207031 L 18.609375 14.792969 C 18.609375 14.792969 19.692953 14 21.001953 14 C 22.310953 14 23.392578 14.792969 23.392578 14.792969 L 24.611328 13.207031 C 24.609328 13.207031 23.094 12 21 12 z M 10.810547 19 L 9.0898438 20 C 9.2635938 20.29875 9.455 20.584668 9.6640625 20.857422 C 9.873125 21.130176 10.100547 21.389297 10.341797 21.632812 C 10.824297 22.119844 11.368438 22.545859 11.960938 22.896484 C 12.257188 23.071797 12.56375 23.228887 12.882812 23.365234 C 13.201875 23.501582 13.532344 23.616797 13.871094 23.710938 C 14.209844 23.805078 14.55875 23.87709 14.914062 23.925781 C 15.269375 23.974473 15.63125 24 16 24 C 16.36875 24 16.730625 23.974473 17.085938 23.925781 C 18.8625 23.682324 20.451953 22.850391 21.658203 21.632812 C 21.899453 21.389297 22.126875 21.130176 22.335938 20.857422 C 22.545 20.584668 22.736406 20.29875 22.910156 20 L 21.189453 19 C 20.279453 20.56625 18.695957 21.689238 16.820312 21.945312 C 16.552363 21.981895 16.27875 22 16 22 C 15.72125 22 15.447637 21.981895 15.179688 21.945312 C 14.911738 21.90873 14.649453 21.853906 14.394531 21.783203 C 13.884688 21.641797 13.403359 21.435 12.958984 21.171875 C 12.514609 20.90875 12.107188 20.589766 11.746094 20.224609 C 11.565547 20.042031 11.396621 19.846973 11.240234 19.642578 C 11.083848 19.438184 10.940547 19.22375 10.810547 19 z\"/></svg>"
  set_svg_props(code, fill, height)
}

arrow_right <- function(fill = "h3", height = 1){
  code <- "<svg style=\"height: 1em; top:.04em; position: relative; fill: fill;\"
viewBox=\"0 0 512 512\"><path d=\"M256 8c137 0 248 111 248 248S393 504 256 504 8 393 8 256 119 8 256 8zm-28.9 143.6l75.5 72.4H120c-13.3 0-24 10.7-24 24v16c0 13.3 10.7 24 24 24h182.6l-75.5 72.4c-9.7 9.3-9.9 24.8-.4 34.3l11 10.9c9.4 9.4 24.6 9.4 33.9 0L404.3 273c9.4-9.4 9.4-24.6 0-33.9L271.6 106.3c-9.4-9.4-24.6-9.4-33.9 0l-11 10.9c-9.5 9.6-9.3 25.1.4 34.4z\"/></svg>"
  set_svg_props(code, fill, height)
}

chevron_right <- function(fill = "h3", height = 1){
  code <- '<svg style=\"height: 1em; top:.04em; position: relative; fill: fill;\" viewBox="0 0 512 512"><path d="M256 8c137 0 248 111 248 248S393 504 256 504 8 393 8 256 119 8 256 8zm113.9 231L234.4 103.5c-9.4-9.4-24.6-9.4-33.9 0l-17 17c-9.4 9.4-9.4 24.6 0 33.9L285.1 256 183.5 357.6c-9.4 9.4-9.4 24.6 0 33.9l17 17c9.4 9.4 24.6 9.4 33.9 0L369.9 273c9.4-9.4 9.4-24.6 0-34z"/></svg>'
  set_svg_props(code, fill, height)
}



circle_arrow_right <- function(fill = "h3", height = 1){
  code <- '<svg style=\"height: 1em; top:.04em; position: relative; fill: fill;\"  viewBox="0 0 512 512"><path d="M256 0C114.6 0 0 114.6 0 256c0 141.4 114.6 256 256 256s256-114.6 256-256C512 114.6 397.4 0 256 0zM406.6 278.6l-103.1 103.1c-12.5 12.5-32.75 12.5-45.25 0s-12.5-32.75 0-45.25L306.8 288H128C110.3 288 96 273.7 96 256s14.31-32 32-32h178.8l-49.38-49.38c-12.5-12.5-12.5-32.75 0-45.25s32.75-12.5 45.25 0l103.1 103.1C414.6 241.3 416 251.1 416 256C416 260.9 414.6 270.7 406.6 278.6z"/></svg>'
  set_svg_props(code, fill, height)
}




triangle_exclamation <- function(fill = "h3", height = 1) {
  code <- '<svg style=\"height: 1em; top:.04em; position: relative; fill: fill;\"  viewBox="0 0 512 512"><path d="M256 320c4.406 0 8.013-3.594 8.013-8v-160c0-4.406-3.607-8-8.013-8c-4.406 0-7.987 3.594-7.987 8v160C248 316.4 251.6 320 256 320zM506.3 417l-213.3-364C284.8 39 270.4 32 256 32C241.7 32 227.2 39 218.1 53l-213.2 364C-10.62 444.9 9.848 480 42.74 480h426.6C502 480 522.7 445 506.3 417zM492.6 450.7C487.8 459 479.1 464 469.3 464H42.73c-9.789 0-18.49-4.982-23.27-13.33c-4.678-8.162-4.641-17.49 .1035-25.58L232.8 61.1C237.6 52.9 246.3 48 256 48c9.699 0 18.34 4.887 23.14 13.09l213.3 364C497.2 433.2 497.2 442.5 492.6 450.7zM256.1 368c-8.822 0-16 7.178-16 16s7.138 16 15.96 16c8.822 0 16.04-7.178 16.04-16S264.9 368 256.1 368z"/></svg>'
  set_svg_props(code, fill, height)
}




