FROM ubuntu:18.04
RUN apt-get update && apt-get install -y --no-install-recommends \
  build-essential \
  ca-certificates \
  curl \
  libncurses5-dev \
  sbcl \
  && rm -rf /var/lib/apt/lists/*

RUN mkdir /quicklisp
WORKDIR /quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN echo '(load "quicklisp.lisp") \
  (quicklisp-quickstart:install)  \
  (ql::without-prompting          \
    (ql:add-to-init-file))' > install-quicklisp.lisp
RUN sbcl --load install-quicklisp.lisp

# Using ~/common-lisp means ASDF will automatically find it
RUN mkdir -p /root/common-lisp/cl-rogue
WORKDIR /root/common-lisp/cl-rogue
COPY *.lisp *.asd ./
COPY /etc/*.lisp ./
RUN sbcl --load load-rogue.lisp
ENTRYPOINT sbcl --load run-rogue.lisp
