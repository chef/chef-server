-- Verify cookbook_versions_cookbook_id_index

BEGIN;

select
    (1/COUNT(*)) as result
from
    pg_class t,
    pg_class i,
    pg_index ix,
    pg_attribute a
where
    t.oid = ix.indrelid
    and i.oid = ix.indexrelid
    and a.attrelid = t.oid
    and a.attnum = ANY(ix.indkey)
    and t.relkind = 'r'
    and t.relname = 'cookbook_versions'
    and i.relname = 'cookbook_versions_cookbook_id_index';
    
ROLLBACK;
