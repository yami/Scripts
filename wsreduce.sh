#!/bin/bash


# This script reduces a network capture file which:
#   - crashes Tshark, so probably wireshark too, or
#   - or there is a dissector bug message printed
#
# The original file is intacted, and resulted file name is prefixed 'xcap_'.
#
# It works simply by dividing the capture file to see if the divided part
# is still buggy or not; if yes, it divides the buggy part further.
#
# It is very effective, usually only 1 or two packets that cause the bug
# are left in the final file.

CAPINFOS='capinfos'
EDITCAP='editcap'
TSHARK='tshark'
CAP_PREFIX='xcap_'
SPLIT_PREFIX='xsplit_'

get_npackets () {
    local cap=$1

    $CAPINFOS -c $cap | tail -n 1 | awk -F ':' '{print $2;}'
}

split_capfile () {
    local npackets_per_file=$1
    local capfile=$2
    local outfile=$3
    
    $EDITCAP -c $npackets_per_file $capfile $outfile
}

shark_buggy () {
    local temp=`mktemp buggy.XXXXXX`
    local buggy=0

    $TSHARK -nVxr $1 2>$temp 1>/dev/null

    if [ $? -eq 134 ]; then
        buggy=0
    elif grep "Dissector bug" $temp >& /dev/null; then
        buggy=0
    else
        buggy=1
    fi 

    rm -rf $temp
    return $buggy
}

#### main ####

if [ $# -ne 1 ]; then
    echo "Usage: $0 <capture file>"
    exit 1
fi

orgfile=$1
nparts=2
iteration=0

capfile="${CAP_PREFIX}${nparts}"
cp $orgfile $capfile

while true; do
    ((iteration++))

    npackets=`get_npackets $capfile`
    ((n = npackets/nparts))

    echo "nparts=${nparts}, packets_per_file=${n}"
    split_capfile $n $capfile "${SPLIT_PREFIX}${nparts}_"

    for splitcap in `ls ${SPLIT_PREFIX}*`; do
        if shark_buggy $splitcap; then
            echo "shark is crashed: $splitcap"
            rm -rf "$capfile"
            capfile="${CAP_PREFIX}${iteration}"
            mv "$splitcap" "$capfile"
            nparts=1
            break
        fi
    done

    ((nparts++))
    rm -rf ${SPLIT_PREFIX}*

    if [ "$n" -le "1" ]; then
        break
    fi
done
