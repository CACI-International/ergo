-- Read stored diagnostic sources for an identity.
-- (id[2]) -> (path, binary)
SELECT path, binary FROM associated_diagnostic_sources WHERE value_u8=? AND value_l8=?
