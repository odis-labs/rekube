FROM node:latest
WORKDIR /root
RUN npm install --unsafe-perm=true -g esy@0.5
RUN git clone https://github.com/rizo/rekube
WORKDIR /root/rekube
RUN esy
