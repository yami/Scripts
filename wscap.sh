#!/bin/bash

#
# Get all sample capture files from Wireshark Wiki.
#


WIKI_URL="http://wiki.wireshark.org"
LINKS_PAGE_URL="$WIKI_URL/SampleCaptures?action=AttachFile"

# We need to predend as a normal browser, otherwise Wireshark web
# refuses to accept us.
USER_AGENT="User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.10) Gecko/2009042523 Ubuntu/9.04 (jaunty) Firefox/3.0.10"

LINKS_PAGE_FILE=`mktemp links_page.XXXXXX`
CMDS_FILE=`mktemp cmds.XXXXXX`

DEBUG="no"


function rmempty () {
    for file in `ls *`; do
        if [ `wc -c $file | awk '{print $1;}'` == 0 ];then
            rm -rf $file
        fi
    done
}

######
# Main
######

if [ $# -eq 1 ]; then
    if [ "$1" == "debug" ]; then
        DEBUG="yes"
    else
        echo "Usage: $0 [debug]"
        exit 1
    fi
fi

# get attachment wiki page which contains all links to attachments
wget -U "$USER_AGENT" "$LINKS_PAGE_URL" --output-document "$LINKS_PAGE_FILE"

# work flow:
#  line 1: find all attachments, whose link's rel is "Appendix", this link is for viewing
#  line 2: change action from 'view' to 'get'
#  line 3: extract the filename (title, group 1) and relative link (href, group 2) to the attachment
#  line 4: find all .cap, .pcap, .tgz, .zip files
#  line 5: generate a file which will be executed, each line will wget one attachment
#          we need to sleep 2 seconds, otherwise Wireshark will consider us attacking.
grep "^<link rel=\"Appendix\" title=" "$LINKS_PAGE_FILE" |\
    sed -n -e "s/do=view/do=get/p" |\
    sed -n -e "s/.*title=\"\([^\"]*\)\" href=\"\([^\"]*\)\">/\1 \2/p" |\
    grep -e '.*\.cap' -e '.*\.pcap' -e '.*\.tgz' -e '.*\.zip' |\
    awk -v agent="$USER_AGENT" -v wiki_url="$WIKI_URL" '{printf "if [ ! -e \"%s\" ]; then wget -U \"%s\" --output-document %s \"%s%s\"; sleep 2;fi\n", $1, agent, $1, wiki_url, $2;}' > "$CMDS_FILE"

rmempty

bash "$CMDS_FILE"

rmempty

if [ "$DEBUG" == "no" ]; then
    rm -rf "$LINKS_PAGE_FILE"
    rm -rf "$CMDS_FILE"
fi
