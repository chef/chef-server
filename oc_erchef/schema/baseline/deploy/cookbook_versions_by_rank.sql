-- Deploy cookbook_versions_by_rank
-- requires: cookbook_versions
-- requires: cookbooks

BEGIN;

CREATE OR REPLACE VIEW cookbook_versions_by_rank(
    -- Cookbook Version fields
    major, -- these 3 are needed for version information (duh)
    minor,
    patch,
    version, -- concatenated string of the complete version
    serialized_object, -- needed to access recipe manifest

    -- Cookbook fields
    org_id, -- used for filtering
    name, -- both version and recipe queries require the cookbook name

    -- View-specific fields
    -- (also used for filtering)
    rank) AS
SELECT v.major,
       v.minor,
       v.patch,
       v.major || '.' || v.minor || '.' || v.patch,
       v.serialized_object,
       c.org_id,
       c.name,
       rank() OVER (PARTITION BY v.cookbook_id
                    ORDER BY v.major DESC, v.minor DESC, v.patch DESC)
FROM cookbooks AS c
JOIN cookbook_versions AS v
  ON c.id = v.cookbook_id;

COMMIT;
