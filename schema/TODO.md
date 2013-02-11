Authz Schema TODOs
==================

- [ ] Consider the use of [index-only scans][] for certain queries

      Specifically, `(id, authz_id)` indexes for everything, and an
      `(id, name)` index for containers.  The idea is that these will
      be the most frequently requested data, and if we can avoid I/O
      to answer requests, then all the better.  However, since there
      are already a lot or indexes for `PRIMARY KEY` and `UNIQUE`
      constraints, adding more indexes to support index-only scans may
      impair overall performance due to the increased cost of index
      updates.  It is unclear if there will be any concrete benefits
      to supporting index-only scans without getting some performance
      numbers on real data.

[index-only scans]:http://wiki.postgresql.org/wiki/What%27s_new_in_PostgreSQL_9.2#Index-only_scans
