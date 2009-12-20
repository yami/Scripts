#!/bin/bash

#
# This script tries to monitor Wireshark fuzz testing, when it finds there's a failure,
#   - moves the problematic capture file to a tmp directory, and
#   - restart fuzz testing
# It supports several fuzz testing suite so that multiple fuzz testing can run simutaneously.
#

PROGRAM_NAME=`basename $0`

test_dir="/home/yami/test"
tmp_dir="/home/yami/tmp"
bin_dir="/home/yami/project/wsclean/wireshark/tools"

FUZZ_MAX=1
FUZZ_TEST_SH=("$bin_dir/fuzz-test.sh")
FUZZ_SUITE=(all)
FUZZ_CAPDIR=("$test_dir/all")
FUZZ_TMPDIR=("$tmp_dir/all")

# in seconds
FUZZ_CHECK_INTERVAL=30

function fuzz_log () {
    echo "[`date`] $*"
}

function get_fuzzcap () {
    local logfile=$1

    local capfile=`grep "Output file: " $logfile | awk -F ':' '{print $2;}'`
    
    if [ -z "$capfile" ]; then
        echo "ERROR: $logfile has not fuzzed capture file!"
        exit 2
    fi

    echo "$capfile"
}

function get_origcap () {
    local logfile=$1
    local origcap=`grep "Original file: " $logfile | awk -F ':' '{print $2;}'`

    if [ -z "$origcap" ]; then
        echo "ERROR: $logfile has not original capture file!"
        exit 2
    fi
    echo "$origcap"
}

function fuzz_index () {
    local fuzz="$1"
    local idx="0"

    for f in "${FUZZ_SUITE[@]}"; do
        if [ "$f" == "fuzz" ]; then
            echo "$idx"
            return 0
        fi

        (( idx++ ))
    done
 
    return 1
}

function start_fuzz () {
    local i=$1

    touch ${FUZZ_SUITE[$i]}.log
    ${FUZZ_TEST_SH[$i]} -d ${FUZZ_TMPDIR[$i]} -c ${FUZZ_CAPDIR[$i]} >& ${FUZZ_SUITE[$i]}.log &
}

function start_all () {
    for ((i=0; i<FUZZ_MAX; i++)); do
        fuzz_log "start fuzz: ${FUZZ_SUITE[$i]}"
        start_fuzz $i
    done
}

function restart_fuzz () {
    local fuzz="$1"
    local i=`fuzz_index "$fuzz"`

    local gout=`ps aux | grep -v "grep" | grep "${FUZZ_TEST_SH[$i]}.*${fuzz}"`

    if [ -z "$gout" ]; then
        start_fuzz $i
    fi
}

# main #

fuzz_log "start main"
start_all

while true; do
    sleep $FUZZ_CHECK_INTERVAL
    for fuzz in "${FUZZ_SUITE[@]}"; do
        if grep "^Processing failed\.  Capture info follows" ${fuzz}.log; then
            fuzz_log "bug found: $fuzz"
            stamp=`date +%Y_%m_%d_%H_%M_%S`
            idx=`fuzz_index ${fuzz}`
            tmp="${FUZZ_TMPDIR[$idx]}"
            cp -p ${fuzz}.log $tmp/${fuzz}.log."$stamp"
            mv `get_origcap "${fuzz}.log"` $tmp/
            restart_fuzz "$fuzz"
        fi
    done
done
