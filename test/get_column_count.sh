#!/bin/bash

get_column_count() {
    local file="$1"

    # Read the first line of the file and count the number of columns
    local first_line
    first_line=$(head -n 1 "$file")

    # Use the comma as the delimiter to count the number of fields
    local column_count
    column_count=$(echo "$first_line" | awk -F',' '{print NF}')

    echo "The number of columns in '$file' is: $column_count"
}

# Usage example:
get_column_count "/Users/dan/Code/R_Workspace/Wormcat/inst/extdata/whole_genome_v2_nov-11-2021.csv"
