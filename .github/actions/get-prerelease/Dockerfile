FROM ubuntu:latest

RUN apt-get update && apt-get install jq curl --yes

COPY run.sh /run.sh

ENTRYPOINT ["/run.sh"]