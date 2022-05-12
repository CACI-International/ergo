-- Read the value identity from an entry key.
-- (key[2]) -> (id[2])
SELECT value_u8, value_l8 FROM cache_entries WHERE key_u8=? AND key_l8=?
