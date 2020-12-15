+++
title = "{{ .Name | humanize | title }}"

date = {{ .Date }}
draft = false

[menu]
  [menu.server]
    title = "{{ .Name | humanize | title }}"
    identifier = "server/{{ .Name | humanize | title }}"
    parent = "server"
    weight = 10
+++

