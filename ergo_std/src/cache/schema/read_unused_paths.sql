-- Read unused paths.
-- () -> (path)
SELECT path FROM associated_paths WHERE value_u8 IS NULL;
