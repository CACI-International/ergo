-- Clean up unused stored values.
-- () -> ()
--
-- This should be called repeatedly until no rows are deleted.
--
-- We could use triggers and a reference count to delete things, but it's not strictly necessary.
DELETE FROM stored WHERE (id_u8,id_l8) NOT IN (
	SELECT DISTINCT value_u8, value_l8 FROM cache_entries
	UNION
	SELECT DISTINCT to_u8, to_l8 FROM value_references
);
