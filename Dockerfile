FROM ubuntu:14.04
MAINTAINER Eduardo Santana <efzambom@ime.usp.br>

WORKDIR /root

RUN apt-get update && apt-get install -y bzip2 coreutils build-essential g++         \
   libncurses5-dev openssl libssl-dev libwxgtk2.8-dev               \
   libgl1-mesa-dev libglu1-mesa-dev libpng3                 \
   python-docutils eog evince gcc gnuplot gnuplot-nox   \
   graphviz 

RUN  apt-get update \
  && apt-get install -y wget \
  && rm -rf /var/lib/apt/lists/*

RUN mkdir scsimulator && cd scsimulator && wget https://www.ime.usp.br/~efzambom/scsimulator.zip
RUN cd /root/scsimulator && unzip scsimulator && rm scsimulator.zip
RUN cd /root/scsimulator/scsimulator/common/conf && chmod 777 install-erlang.sh
RUN cd /root/scsimulator/scsimulator/common/conf && ./install-erlang.sh
RUN cd /root/scsimulator/scsimulator/ && make all
#RUN cd /root/scsimulator/scsimulator/mock-simulators/smart-city_v2/src && make smart_city_run CMD_LINE_OPT="--batch"

