-- Verify users_add_disabled_functions

BEGIN;

-- Verify add_user function accepts disabled parameter (17 params now)
SELECT has_function_privilege('add_user(character,character,text,text,text,integer,text,text,password_hash_type,character,timestamp without time zone,timestamp without time zone,text,boolean,text,boolean,boolean)', 'execute');

-- Verify update_user function accepts disabled parameter (15 params now)
SELECT has_function_privilege('update_user(integer,text,text,text,password_hash_type,text,text,boolean,text,text,character,timestamp without time zone,boolean,boolean,character)', 'execute');

ROLLBACK;
