-- Write a cache entry.
-- (key[2], value[2], expiration_time) -> ()
INSERT OR REPLACE INTO cache_entries (
	key_u8, key_l8,
	value_u8, value_l8,
	creation_time,
	expiration_time
) VALUES (?,?,?,?,strftime('%s'),?);
-- In sqlite 3.38.0, we can use `unixepoch()` rather than `strftime`
