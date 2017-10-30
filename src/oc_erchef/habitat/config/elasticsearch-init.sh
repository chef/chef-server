#!/bin/bash

export PATH=$PATH:$(hab pkg path "core/curl")/bin

{{#if bind.elasticsearch}}
  {{#eachAlive bind.elasticsearch.members as |member|}}
    {{#if @last}}
HOST="{{member.sys.ip}}"
PORT="{{member.cfg.http-port}}"
    {{/if}}
  {{/eachAlive}}
{{else}}
HOST="{{cfg.elasticsearch.vip}}"
PORT="{{cfg.elasticsearch.port}}"
{{/if}}

curl -XPUT http://${HOST}:${PORT}/chef/ -d @{{pkg.svc_config_path}}/elasticsearch-index-init.json
