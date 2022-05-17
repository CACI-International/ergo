-- Write a stored value.
-- (id[2], evaluated[2], type, data) -> ()
INSERT OR IGNORE INTO stored (
	id_u8, id_l8,
	evaluated_u8, evaluated_l8,
	type,
	data
) VALUES (?,?,?,?,?,?);
