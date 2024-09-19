#!/usr/bin/env bash

function log_section_start {
    echo "--- [$(date -u)] $*"
}

function log {
  echo "[$(date -u)]$*" >&2
}

function log_info {
  log "[info] $*"
}

function log_warning {
  log "[warning] $*"
}

function log_error {
  log "[error] $*"
}

function log_line {
  log "[info] $*"
}