#!/bin/bash -xe
cp omnibus.rb.example omnibus.rb
rm pkg/* || true
bundle install --deployment --without development
bundle exec rake projects:private-chef:deb
