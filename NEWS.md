# microCTr 1.1.2

## Minor improvements

- Fix typo in the "Create and Run a Standard microCT Analysis" website article.
- Update the website homepage to contain more links.

# microCTr 1.1.1

## Minor improvements

- `knit_with_colored_text()` was added to more seamlessly handle rendering R
  Markdown documents with colored text.
- Add step-by-step articles to the package website.

# microCTr 1.1.0

## Minor improvements

- The Comparing Groups R Markdown template now starts with a Background section
  which has a sub-section called "Original Data", under which is a link to
  download the original data file, which is embedded using `xfun::embed_file()`.
- The `print_results()` function now has a `sig_color` argument, which specifies
  what color to print results when they are significant. This is implemented
  using a Pandoc Lua filter, which is automatically copied to the working
  directory when rendering the Comparing Groups R Markdown template.

# microCTr 1.0.0

- This is the first functional release of microCTr, containing the ability to
  compare microCT data between two genotypes or two treatment groups.
