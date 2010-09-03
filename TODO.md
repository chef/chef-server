## MUST ##
* include a timestamp in the envelope sent by the producer. Log the transit time
  in the consumers
* show the status of all vnode queues. Show the total queue backlog, 
  min/max/avg-per queue, plus the backlog of each queue individually
* show the status of all nodes: total vnodes, list of vnodes, etc. combine with
  the above to show how backed up each node is.

## Auto Cluster Stuff ##
* Turn vnode table gossiping back on
* timestamp vnode table updates
* periodically scan the vnode table and prune stale entries
* vnode release protocol (for nodes joining an existing cluster)

## MIGHT ##
* measure the rate of messages, total and per-org (?)
* "blow-off valve:" "do something" with messages matching given criteria
