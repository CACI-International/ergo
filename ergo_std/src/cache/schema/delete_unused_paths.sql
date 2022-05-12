-- Clean up unused paths.
-- () -> (path)
DELETE FROM associated_paths WHERE value_u8 IS NULL RETURNING path;
