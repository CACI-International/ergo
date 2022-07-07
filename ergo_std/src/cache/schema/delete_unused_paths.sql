-- Clean up unused paths.
-- () -> ()
-- In sqlite 3.35, we can use `DELETE FROM ... RETURNING path`.
DELETE FROM associated_paths WHERE value_u8 IS NULL;
