FROM odis/esy:0.6.2_ocaml-4.6.1000

WORKDIR /root
RUN git clone https://github.com/rizo/rekube
WORKDIR /root/rekube
RUN esy
