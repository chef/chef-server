-- Verify opc_customers

BEGIN;

SELECT id, name, display_name, domain, contact,
       priority, created_at, updated_at,
       osc_customer, ohc_customer, opc_customer,
       support_plan
FROM opc_customers
WHERE FALSE;

ROLLBACK;
