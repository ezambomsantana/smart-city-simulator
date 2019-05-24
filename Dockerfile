#InterSCSimulator Dockerfile
FROM erlang:20-alpine

RUN apk add --update make

RUN apk add util-linux-dev

ADD . /src
RUN cd /src && make all


CMD [ "make", "smart_city_run", "CMD_LINE_OPT='--batch'" ]

#RUN sudo docker run -t -w /src/mock-simulators/smart_city_model/src --net="host" -v /home/eduardo/volume2:/src/mock-simulators/smart_city_model/output -e USER=root image

# Create a new docker container from image
# docker run -h test2.example.com -d -p 2223:22 -e SSH_KEY="$(cat ~/.ssh/id_rsa.pub)" 7f18a0bf96f

# Start a stopped container
# docker start 393805450e09

# list all docker containers
# docker ps -a

# verify container ID
# docker inspect 393805450e09
