-- Write a value reference.
-- (from[2], to[2]) -> ()
INSERT INTO value_references (
	from_u8, from_l8,
	to_u8, to_l8
) VALUES (?,?,?,?);
