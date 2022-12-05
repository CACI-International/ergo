-- Delete an entry based on key.
-- (key[2]) -> ()
DELETE FROM cache_entries WHERE key_u8=? AND key_l8=?;
