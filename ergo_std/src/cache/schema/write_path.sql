-- Write an associated path.
-- (value[2], path) -> ()
INSERT INTO associated_paths (
	value_u8, value_l8,
	path
) VALUES (?,?,?);
