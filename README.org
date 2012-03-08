#+TITLE:  Bookshelf
#+AUTHOR: Tim Dysinger
#+EMAIL:  dysinger@opscode.com

* Description

  Bookshelf is an S3 API compatible object store.

* Setup

*** [[http://erlang.org][Erlang]] R15

*** [[https://github.com/basho/rebar][Rebar]] (recent)

* Build

  #+BEGIN_SRC: sh
rebar get-deps
rebar compile
rebar generate
  #+END_SRC

* Configuration

  Standard OTP application configuration

* Start

  #+BEGIN_SRC: sh
./start.sh
  #+END_SRC
