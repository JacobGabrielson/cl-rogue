FROM ubuntu:22.04

RUN apt-get update && apt-get install -y --no-install-recommends \
  sbcl \
  && rm -rf /var/lib/apt/lists/*

# ~/common-lisp is in ASDF's default source registry
RUN mkdir -p /root/common-lisp/cl-rogue
WORKDIR /root/common-lisp/cl-rogue
COPY etc/*.lisp *.lisp *.asd ./

# Pre-compile to fasls so startup is fast
RUN sbcl --load load-rogue.lisp

ENTRYPOINT ["sbcl", "--load", "run-rogue.lisp"]
