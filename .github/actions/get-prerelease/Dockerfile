FROM ubuntu:latest

RUN sudo apt-get update && sudo apt-get install jq curl --yes

COPY run.sh /run.sh

ENTRYPOINT ["/run.sh"]