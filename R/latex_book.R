#' Convert R Markdown to a PDF book LaTeX file
#'
#' Convert R Markdown files to LaTeX file after resolving the special tokens of \pkg{bookdown} (e.g., the tokens for references and labels) to native LaTeX commands.
#' Identical to `bookdown::pdf_book`, except that the tex file created by Pandoc is not compiled.
#'
#'
#' @inheritParams bookdown::pdf_book
#'
#' @return  Command to insert a figure in Markdown
#' @import rmarkdown bookdown
#' @importFrom xfun read_utf8 write_utf8 with_ext

#' @export
latex_book <- function(
  toc = TRUE, number_sections = TRUE, fig_caption = TRUE, pandoc_args = NULL, ...,
  base_format = rmarkdown::pdf_document, toc_unnumbered = TRUE,
  toc_appendix = FALSE, toc_bib = FALSE, quote_footer = NULL, highlight_bw = FALSE
) {
  config = get_base_format(base_format, list(
    toc = toc, number_sections = number_sections, fig_caption = fig_caption,
    pandoc_args = pandoc_args2(pandoc_args), keep_tex = TRUE, ...
  ))
  config$pandoc$ext = '.tex'
  post = config$post_processor  # in case a post processor have been defined
  config$post_processor = function(metadata, input, output, clean, verbose) {
    if (is.function(post)) output = post(metadata, input, output, clean, verbose)
    f = xfun::with_ext(output, '.tex')
    x = read_utf8(f)
    x = restore_block2(x, !number_sections)
    x = resolve_refs_latex(x)
    x = resolve_ref_links_latex(x)
    x = restore_part_latex(x)
    x = restore_appendix_latex(x, toc_appendix)
    if (!toc_unnumbered) x = remove_toc_items(x)
    if (toc_bib) x = add_toc_bib(x)
    if (!is.null(quote_footer)) {
      if (length(quote_footer) != 2 || !is.character(quote_footer)) warning(
        "The 'quote_footer' argument should be a character vector of length 2"
      ) else x = process_quote_latex(x, quote_footer)
    }
    if (highlight_bw) x = highlight_grayscale_latex(x)
    post = getOption('bookdown.post.latex')
    if (is.function(post)) x = post(x)
    xfun::write_utf8(x, f)
    # tinytex::latexmk(
    #   f, config$pandoc$latex_engine,
    #   if ('--biblatex' %in% config$pandoc$args) 'biber' else 'bibtex'
    # )

    output = with_ext(output, '.tex')
    o = opts$get('output_dir')
    keep_tex = isTRUE(config$pandoc$keep_tex)
    if (!keep_tex) file.remove(f)
    if (is.null(o)) return(output)

    output2 = file.path(o, output)
    file.rename(output, output2)
    if (keep_tex) file.rename(f, file.path(o, f))
    output2
  }
  # always enable tables (use packages booktabs, longtable, ...)
  pre = config$pre_processor
  config$pre_processor = function(...) {
    c(
      if (is.function(pre)) pre(...), '--variable', 'tables=yes', '--standalone',
      if (rmarkdown::pandoc_available('2.7.1')) '-Mhas-frontmatter=false'
    )
  }
  config = common_format_config(config, 'latex')
  config
}

get_base_format <- utils::getFromNamespace("get_base_format", "bookdown")
restore_block2 <- utils::getFromNamespace("restore_block2", "bookdown")
pandoc_args2 <- utils::getFromNamespace("pandoc_args2", "bookdown")
resolve_refs_latex <- utils::getFromNamespace("resolve_refs_latex", "bookdown")
resolve_ref_links_latex <- utils::getFromNamespace("resolve_ref_links_latex", "bookdown")
restore_part_latex <- utils::getFromNamespace("restore_part_latex", "bookdown")
restore_appendix_latex <- utils::getFromNamespace("restore_appendix_latex", "bookdown")
common_format_config <- utils::getFromNamespace("common_format_config", "bookdown")
opts <- utils::getFromNamespace("opts", "bookdown")
add_toc_bib <- utils::getFromNamespace("add_toc_bib", "bookdown")
highlight_grayscale_latex <- utils::getFromNamespace("highlight_grayscale_latex", "bookdown")
opts_current <- utils::getFromNamespace("opts_current", "knitr")
process_quote_latex <- utils::getFromNamespace("process_quote_latex", "bookdown")
remove_toc_items <- utils::getFromNamespace("remove_toc_items", "bookdown")
