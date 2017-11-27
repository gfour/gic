#!/bin/bash
# Compiles the file to Maude code and evaluates it with the Maude model.
# Arguments: <filename> <number of warehouses>

set -e

function usage {
    echo "Usage: ./eval_maude.sh [run|search] <file.hs> <number of warehouses>"
    exit
}

if [ $# -ne 3 ]
then
    usage
else

source find-gic.sh
echo "Using GIC=${GIC}"

cp maude/eduction.maude test.maude
${GIC} -cm $2 -wh $3  >> test.maude
if [ "$1" = "search" ]; then
    echo "(search init =>! C:Configuration .)" >> test.maude
    echo Starting search in Maude...
else
    if [ "$1" = "run" ]; then
	echo Rewriting in Maude...
    else
	usage
    fi
fi

(maude maude/full-maude26.maude < test.maude) |
sed -e "s/< /\n < /g" |
sed -e "s/notify/\n notify/g" |
sed -e "s/continue/\n continue/g" |
sed -e "s/demand/\n demand/g" |
sed -e "s/regVal/\n regVal/g"

fi
