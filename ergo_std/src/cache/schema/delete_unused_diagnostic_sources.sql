-- Clean up unused diagnostic sources.
-- () -> ()
DELETE FROM associated_diagnostic_sources WHERE value_u8 IS NULL;
