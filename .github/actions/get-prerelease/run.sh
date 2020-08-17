#!/bin/sh

#TODO: change  the repo name!!!

RELEASES=`curl -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/gitoleg/bap/releases`

# check if the first argument in the list (the most recent one) is
# a prerelease
IsPRERELEASE=`echo $RELEASES | jq '.[0].prerelease'`

PRERELEASE=
TAG=
if [ "check$IsPRERELEASE" = "checktrue" ]; then
    PRERELEASE=`echo $RELEASES | jq '.[0].id'`
    TAG=`echo $RELEASES | jq '.[0].tag_name'`
fi

echo "::set-output name=id::$PRERELEASE"
echo "::set-output name=tag::$TAG"
