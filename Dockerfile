FROM odis/esy:0.6.2_ocaml-4.6.1000

WORKDIR /root/rekube
COPY . .
RUN esy
