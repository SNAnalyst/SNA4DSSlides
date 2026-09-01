# Tests for slide_urls() -- run with:
#   tinytest::run_test_dir("tests/tinytest")
#
# The tests below are built on small hand-made "decks": a minimal html file that
# only contains the <textarea id="source"> element, which is exactly what the
# html engine of slide_urls() reads. That keeps the tests fast and free of
# pandoc/Chrome. The one test that needs a browser is skipped automatically when
# chromote or a rendered deck is not available.

# --- locate and load the function under test --------------------------------
# tinytest runs a test file with its own directory as working directory, but
# sourcing from the console should work too, hence the two candidates.
if (!exists("slide_urls")) {
  cand <- c("../../slide_urls.R", "slide_urls.R",
            file.path(getwd(), "slide_urls.R"))
  hit <- cand[file.exists(cand)][1]
  if (is.na(hit)) stop("cannot find slide_urls.R")
  source(hit)
}


#' Write a minimal xaringan-like html file for testing
#'
#' @param src the remark markdown that goes into the textarea.
#' @param file optional path; a temp file is used when missing.
#' @return the path to the html file.
make_deck <- function(src, file = tempfile(fileext = ".html")) {
  writeLines(c("<!DOCTYPE html><html><head></head><body>",
               "<textarea id=\"source\">",
               src,
               "</textarea>",
               "</body></html>"),
             file)
  file
}

# a fixed base_url keeps the expectations readable and independent of tempdir()
BASE <- "https://example.org/deck.html"

# convenience wrapper: html engine, no messages, fixed base
u <- function(src, ...) {
  slide_urls(make_deck(src), base_url = BASE, engine = "html", panels = "none",
             quiet = TRUE, ...)
}


# --- the basics -------------------------------------------------------------

# three slides separated by ---
expect_equal(u("# one\n\n---\n\n# two\n\n---\n\n# three"),
             paste0(BASE, "#", 1:3))

# a deck without any separator is a single slide
expect_equal(u("# only one"), paste0(BASE, "#1"))

# an empty deck still yields the (empty) first slide, never zero addresses
expect_equal(length(u("")), 1L)


# --- incremental slides -----------------------------------------------------

# '--' creates a slide of its own and therefore its own address
expect_equal(u("# one\n\n--\n\nmore\n\n---\n\n# two"),
             paste0(BASE, "#", 1:3))

# countIncrementalSlides: false only changes the printed slide number, not the
# address, so it must not change the result either
expect_equal(length(u("# one\n\n--\n\nmore\n\n--\n\nand more")), 3L)

# '--' with trailing whitespace is not a separator for remark (the regex wants a
# newline straight after the dashes), so this is one slide
expect_equal(length(u("# one\n\n-- \n\nmore")), 1L)

# four dashes are not a separator either
expect_equal(length(u("# one\n\n----\n\nmore")), 1L)


# --- properties that change the numbering -----------------------------------

# exclude: true removes the slide completely
expect_equal(u("# one\n\n---\nexclude: true\n\n# gone\n\n---\n\n# two"),
             paste0(BASE, "#", 1:2))

# layout: true is a template, not a slide
expect_equal(u("# one\n\n---\nlayout: true\n\n# template\n\n---\n\n# two"),
             paste0(BASE, "#", 1:2))

# layout: false switches the template off but is a normal slide itself
expect_equal(length(u("layout: false\n\n# one\n\n---\n\n# two")), 2L)

# other properties leave the numbering alone
expect_equal(length(u("# one\n\n---\nclass: center, middle\ncount: false\n\n# two")), 2L)


# --- separators that only look like separators -------------------------------

# --- inside a fenced code block belongs to the code
expect_equal(length(u("# one\n\n```r\nx <- 1\n---\ny <- 2\n```\n\n---\n\n# two")), 2L)

# ... and so does --- inside a 4-space indented code block
expect_equal(length(u("# one\n\n    ---\n\n---\n\n# two")), 2L)

# --- inside a .class[...] content block is content, not a separator
expect_equal(length(u("# one\n\n.pull-left[\n---\n]\n\n---\n\n# two")), 2L)

# nested content blocks are skipped as a whole
expect_equal(length(u(".left-column[.content[\n---\n]]\n\n---\n\n# two")), 2L)

# ??? starts the presenter notes of the same slide, it is not a new slide
expect_equal(length(u("# one\n\n???\n\nmy notes\n\n---\n\n# two")), 2L)


# --- slide names ------------------------------------------------------------

# named slides get their numeric address; the #name form is opt-in
expect_equal(u("# one\n\n---\nname: intro\n\n# two"),
             paste0(BASE, "#", 1:2))

expect_equal(u("# one\n\n---\nname: intro\n\n# two", named_anchors = TRUE),
             c(paste0(BASE, "#1"), paste0(BASE, "#2"), paste0(BASE, "#intro")))

# names can also be given in a html comment
expect_equal(u("# one\n\n---\n<!-- name: intro -->\n\n# two", named_anchors = TRUE),
             c(paste0(BASE, "#1"), paste0(BASE, "#2"), paste0(BASE, "#intro")))

# several properties in a row are all read, including the name
expect_equal(u("class: center\nname: intro\ncount: false\n\n# one", named_anchors = TRUE),
             c(paste0(BASE, "#1"), paste0(BASE, "#intro")))

# a dash in a name is allowed (remark's property key/value pattern)
expect_equal(u("name: start-of-sna\n\n# one", named_anchors = TRUE),
             c(paste0(BASE, "#1"), paste0(BASE, "#start-of-sna")))

# a name deeper in the slide (not in the leading text) is not a property
expect_equal(u("# one\n\n.left[x]\n\nname: nope", named_anchors = TRUE),
             paste0(BASE, "#1"))


# --- html handling ----------------------------------------------------------

# the textarea content is html-escaped in the file and has to be un-escaped
# before parsing; &amp; must not be expanded twice
deck <- make_deck("# one &lt;b&gt; &amp;amp; done\n\n---\n\n# two")
expect_equal(length(slide_urls(deck, base_url = BASE, engine = "html",
                               panels = "none", quiet = TRUE)), 2L)
expect_equal(.su_read_source(deck),
             "\n# one <b> &amp; done\n\n---\n\n# two\n")

# windows line endings are normalised, so they do not break the separators
deck <- make_deck("# one\r\n\r\n---\r\n\r\n# two")
expect_equal(length(slide_urls(deck, base_url = BASE, engine = "html",
                               panels = "none", quiet = TRUE)), 2L)


# --- urls -------------------------------------------------------------------

# without base_url a file:/// address of the html file itself is used
deck <- make_deck("# one")
expect_true(grepl("^file:///", slide_urls(deck, engine = "html", panels = "none",
                                          quiet = TRUE)))
expect_true(grepl(basename(deck), slide_urls(deck, engine = "html",
                                             panels = "none", quiet = TRUE),
                  fixed = TRUE))

# spaces in the path are escaped the way a browser shows them
spaced <- file.path(tempdir(), "a deck with spaces.html")
make_deck("# one", file = spaced)
expect_true(grepl("a%20deck%20with%20spaces.html",
                  slide_urls(spaced, engine = "html", panels = "none", quiet = TRUE),
                  fixed = TRUE))

# the html engine warns that it cannot do tabs, unless quiet
expect_message(slide_urls(make_deck("# one"), base_url = BASE, engine = "html",
                          panels = "each"))
expect_silent(slide_urls(make_deck("# one"), base_url = BASE, engine = "html",
                         panels = "each", quiet = TRUE))


# --- input checking ---------------------------------------------------------

expect_error(slide_urls("this_file_does_not_exist.html"))
expect_error(slide_urls(c("a.html", "b.html")))
expect_error(u("# one", named_anchors = "yes"))

# a html file that is not a xaringan deck
notadeck <- tempfile(fileext = ".html")
writeLines("<html><body>nothing here</body></html>", notadeck)
expect_error(slide_urls(notadeck, engine = "html", panels = "none", quiet = TRUE))


# --- helpers ----------------------------------------------------------------

# bracket matching used for .class[...] blocks
expect_equal(.su_bracket_content("[abc]", 2L), "abc")
expect_equal(.su_bracket_content("[a[b]c]", 2L), "a[b]c")
expect_null(.su_bracket_content("[abc", 2L))

# property parsing
expect_equal(.su_properties("\nname: intro\nclass: center\n\n# title"),
             list(name = "intro", class = "center"))
expect_equal(.su_properties("# no properties here"), list())
expect_equal(.su_properties("")$name, NULL)

# url building from a hand-made slide list (no file involved)
sl <- list(list(no = 1L, name = "", tabs = list()),
           list(no = 2L, name = "tabs", tabs = list(
             list(param = "panelset_001", value = "code", active = TRUE),
             list(param = "panelset_001", value = "plot", active = FALSE),
             list(param = "panelset_002", value = "a", active = TRUE),
             list(param = "panelset_002", value = "b", active = FALSE))))

# panels = "none": only the plain addresses
expect_equal(.su_build_urls(BASE, sl, panels = "none"),
             paste0(BASE, "#", 1:2))

# panels = "each": one address per non-active tab, one panelset at a time
expect_equal(.su_build_urls(BASE, sl, panels = "each"),
             c(paste0(BASE, "#1"),
               paste0(BASE, "#2"),
               paste0(BASE, "?panelset_001=plot#2"),
               paste0(BASE, "?panelset_002=b#2")))

# panels = "all": every combination except the all-default one
expect_equal(.su_build_urls(BASE, sl, panels = "all"),
             c(paste0(BASE, "#1"),
               paste0(BASE, "#2"),
               paste0(BASE, "?panelset_001=plot&panelset_002=a#2"),
               paste0(BASE, "?panelset_001=code&panelset_002=b#2"),
               paste0(BASE, "?panelset_001=plot&panelset_002=b#2")))

# named anchors come directly after the slide they belong to
expect_equal(.su_build_urls(BASE, sl, panels = "none", named_anchors = TRUE),
             c(paste0(BASE, "#1"), paste0(BASE, "#2"), paste0(BASE, "#tabs")))


# --- the real thing: both engines have to agree ------------------------------
# only run when a rendered deck and a browser are available
deck_real <- c("../../Week01_Lecture_Introduction.html",
               "Week01_Lecture_Introduction.html")
deck_real <- deck_real[file.exists(deck_real)][1]

if (!is.na(deck_real) && .su_has_chromote()) {
  by_html <- slide_urls(deck_real, engine = "html", panels = "none", quiet = TRUE)
  by_chrome <- slide_urls(deck_real, engine = "chrome", panels = "none", quiet = TRUE)
  expect_equal(by_html, by_chrome)
  expect_true(length(by_chrome) > 1L)
}
