#!/bin/bash

set -evx

echo -e '\nBEGIN GATHER LOGS TEST\n'

sudo chef-server-ctl gather-logs

echo -e '\nEND GATHER LOGS TEST\n'
