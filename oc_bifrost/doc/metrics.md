Bifrost Metrics
================

The following metrics are collected and sent to Graphite.

We perform aggregation in [estatsd][].  Part of this aggregation
modifies the path of the metrics we generate.  All `meter` metrics
will be prefixed with `stats`, while all `histogram` metrics are
prefixed with `stats.timers`.  This is what allows us to use metrics
from [stats_hero][] that have the same name, but different type.

Additionally, [estatsd][] also generates metrics prefixed with
`stats_counts`.  This collects the number of individual metrics of a
given label were sent.  However, this is only for `meter` metrics;
`histogram` metrics have their counts tallied separately.  Since all
our `meter` metrics are simple "increment by 1" metrics, the
`stats_counts` metrics will represent the same general data (but see
the `Caveats` section below).

Additionally, for all `histogram` metrics, [estatsd][] will compute
aggregates for the statistics that were pulled in during its batching
period.  These include:

- `count`
- `lower`
- `mean`
- `upper`
- `upper_90`

Again, these counts only apply to `histogram` metrics.  These will be
found under the original metric key.  For instance, sending a
histogram metric for `bifrost.application.allRequests` will result in
the following metrics being available in Graphite:

- `stats.timers.bifrost.application.allRequests.count`
- `stats.timers.bifrost.application.allRequests.lower`
- `stats.timers.bifrost.application.allRequests.mean`
- `stats.timers.bifrost.application.allRequests.upper`
- `stats.timers.bifrost.application.allRequests.upper_90`

These measurements are taken over a 10 second window.  See `Caveats` below.

# Caveats

It is _very important_ to note that the `stats` metrics (i.e., all the
`meter` metrics) are scaled by [estatsd][] and effectively become
'events per second' rates.

The `stats_counts` metrics, however, are **not** scaled at all.  These
are counts per 10 second intervals; thus, they need to be
divided by 10 in order to represent per second rates.  Additionally,
because of our current Graphite storage schema configuration, the
`stats_counts` metrics are stored at a lower resolution than the
`stats` metrics, so their graphs will appear different.

The `stats.timers` metrics (being histogram metrics) are not scaled,
either.  The the `lower`, `mean`, `upper`,
and `upper_90` metrics in this hierarchy are
consolidated over a 10 second window.  They aren't "meters", though,
so these don't need to be scaled, since the actually represent real
request time measurements (albeit "special" ones in the 10 second window
in which they were collected).

If, however, you need to use the `*.count` metrics from
`stats.timers.*`, you must once again divide by 10 to get to accurate
per-second rates.  This **is** properly thought of as a meter metric
(if this wasn't all confusing enough already), since it is a count of
requests coming in over a 10-second interval.

# Beginning of Request

Bifrost, via [stats_hero][] creates a few metrics at the beginning of
the request.

* `bifrost.application.allRequests`

  A `meter` metric; increments by 1 on each request.  Keeps a tally of
  all requests sent to the Bifrost application.

  Will be found in Graphite as `stats.bifrost.application.allRequests`.

* `bifrost.$HOST.allRequests`

  A `meter` metric; increments by 1 on each request. Keeps a tally of
  all requests sent to `$HOST`.

  Will be found in Graphite as `stats.bifrost.$HOST.allRequests`.

* `bifrost.application.byRequestType.$REQUEST_LABEL.$REQUEST_VERB`

  A `meter` metric; increments by 1 on each request.

  `$REQUEST_LABEL` is a label generated within Bifrost that reflects
  what kind of request is being processed (e.g., `actor`, `object`,
  etc.).  `$REQUEST_VERB` is one of the HTTP verbs that Bifrost
  supports (currently `GET`, `POST`, `PUT`, and `DELETE`).

  Will be found in Graphite as `stats.bifrost.application.byRequestType.$REQUEST_LABEL.$REQUEST_VERB`.

# End of Request

When the request is finished, several more metrics are generated.

* `bifrost.application.byStatusCode.$HTTP_STATUS`

  A `meter` metric; increments by 1 on each request.  Keeps a tally of
  how many overall requests resulted in the given HTTP status.

  Will be found in Graphite as `stats.bifrost.application.byStatusCode.$HTTP_STATUS`

* `bifrost.$HOST.byStatusCode.$HTTP_STATUS`

  A `meter` metric; increments by 1 on each request.  Same as
  `bifrost.application.byStatusCode.$HTTP_STATUS`, but broken down by
  server.

  Will be found in Graphite as `stats.bifrost.$HOST.byStatusCode.$HTTP_STATUS`

* `bifrost.application.allRequests`

  A `histogram` metric.  The total request processing time is sent.

  Will be found in Graphite under `stats.timers.bifrost.application.allRequests`.

* `bifrost.$HOST.allRequests`

  A `histogram` metric.  The total request processing time is sent.

  Will be found in Graphite under `stats.timers.bifrost.$HOST.allRequests`.

* `bifrost.application.byRequestType.$REQUEST_LABEL.$REQUEST_VERB`

  A `histogram` metric.  The total request processing time is sent.

  Will be found in Graphite under `stats.timers.bifrost.application.byRequestType.$REQUEST_LABEL.$REQUEST_VERB`

# Upstreams

  [stats_hero][] does some aggregation of so-called "upstream"
  metrics.  These are measurements that are taken within an
  application of calls to other services.  In Bifrost, this currently
  is only the relational database calls.

  These metrics are all `histogram` metrics, so they will be found
  under the `stats.timers` prefix (thanks, [estatsd][]!).  All
  previously stated caveats apply!

  These simply record the amount of time spent executing each function
  call, as well as the total time spent interacting with the database
  over the course of the request.

  Examples:

  - `bifrost.upstreamRequests.rdbms`
  - `bifrost.upstreamRequests.rdbms.bifrost_db.my_fun1`
  - `bifrost.upstreamRequests.rdbms.bifrost_db.my_fun2`
  - `bifrost.upstreamRequests.rdbms.bifrost_db.my_fun3`
  - `bifrost.application.byRequestType.$REQUEST_LABEL.$REQUEST_VERB.upstreamRequests.rdbms`
  - `bifrost.application.byRequestType.$REQUEST_LABEL.$REQUEST_VERB.upstreamRequests.rdbms.bifrost_db.my_fun1`
  - `bifrost.application.byRequestType.$REQUEST_LABEL.$REQUEST_VERB.upstreamRequests.rdbms.bifrost_db.my_fun2`
  - `bifrost.application.byRequestType.$REQUEST_LABEL.$REQUEST_VERB.upstreamRequests.rdbms.bifrost_db.my_fun3`

[estatsd]:https://github.com/opscode/estatsd
[stats_hero]:https://github.com/opscode/stats_hero
