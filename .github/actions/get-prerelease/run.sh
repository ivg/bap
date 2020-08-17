#!/bin/sh

set -e

#TODO: change  the repo name!!!
echo "get the releases"
RELEASES=`curl -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/gitoleg/bap/releases`


# check if the first argument in the list (the most recent one) is
# a prerelease
IsPRERELEASE=`echo "$RELEASES" | jq -r '.[0].prerelease'`

echo "check is it's true"
PRERELEASE=
if [ "check$IsPRERELEASE" = "checktrue" ]; then
    PRERELEASE=`echo $RELEASES | jq -r '.[0].id'`
fi

echo "::set-output name=id::$PRERELEASE"
