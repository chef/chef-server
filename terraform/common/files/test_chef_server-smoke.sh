#!/bin/bash

set -evx

echo -e '\nBEGIN SMOKE TEST\n'

sudo chef-server-ctl test

echo -e '\nEND SMOKE TEST\n'
