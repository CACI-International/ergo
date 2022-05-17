-- Write an associated diagnostic source.
-- (value[2], path, binary) -> ()
INSERT INTO associated_diagnostic_sources (
	value_u8, value_l8,
	path,
	binary
) VALUES (?,?,?,?);
