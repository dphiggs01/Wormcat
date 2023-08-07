library(lintr)
library(devtools)
lint_dir <- "/Users/dan/Code/R_Workspace/Wormcat/R"
lint_file <- sprintf("%s/worm_cat_controller.R", lint_dir)
my_linters <- linters_with_defaults(
    line_length_linter = line_length_linter(132),
    indentation_linter = indentation_linter(indent = 4)
)

# Run at the console to make interactive
devtools::lint(pkg = lint_dir, cache = FALSE, linters = my_linters)
