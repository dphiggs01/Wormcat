#!/bin/bash

# test_outputs is a simple script to compare the results of a Wormcat run
# from the results of a run with the same data on the website (wormcat.com)

# The Output Data in RGS_Jul-29-2023-12_37_39 has come from Wormcat.com
# While refactoring the code, we want to get the same results as the current
# code.


control_dir="RGS_Jul-29-2023-12_37_39"
test_dir="wormcat_out"

compare_csv() {
    local file1="$1"
    local file2="$2"

    # Use awk to extract the first three fields and round the fourth field to 4 decimal places from each file and compare them
    # We are rounding because the precision from AWS and the local laptop differ
    if diff <(awk -F',' -v OFS=',' '{printf "%.4f,", $4; print $1,$2,$3}' "$file1") <(awk -F',' -v OFS=',' '{printf "%.4f,", $4; print $1,$2,$3}' "$file2") &>/dev/null; then
        echo "'$file1' is equal '$file2'."
    else
        echo "'$file1' is NOT equal '$file2'."
        diff <(awk -F',' -v OFS=',' '{printf "%.4f,", $4; print $1,$2,$3}' "$file1") <(awk -F',' -v OFS=',' '{printf "%.4f,", $4; print $1,$2,$3}' "$file2")
    fi
}

echo "############## START TEST ##########################"
for f in `ls "${control_dir}" |grep ".csv\$"`;
do
    echo "diff $control_dir/$f" $test_dir/$f
    compare_csv "$control_dir/$f" $test_dir/$f
    echo " "
done
echo "###################################################"
