-- Read a stored value from an identity.
-- (id[2]) -> (evaluated[2], type, data)
SELECT evaluated_u8, evaluated_l8, type, data FROM stored WHERE id_u8=? AND id_l8=?
