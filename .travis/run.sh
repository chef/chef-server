#!/bin/bash

BUNDLE_GEMFILE=$(pwd)/Gemfile
cd $(bundle show chef-zero) && bundle exec ruby spec/run_oc_pedant.rb
