PRAGMA foreign_keys = ON;
PRAGMA journal_mode = WAL;
PRAGMA user_version = 1;

-- All value identities are 16-byte values, so we split them into 2 INTEGER
-- fields (each can be up to 8 bytes). These are indicated by <name>_u8 and <name>_l8.

-- Stored values (not necessarily explicitly cached).
CREATE TABLE IF NOT EXISTS stored (
	-- Value identity
	id_u8 INTEGER NOT NULL,
	id_l8 INTEGER NOT NULL,
	-- Evaluated identity, for use in reconstructing the value.
	evaluated_u8 INTEGER NOT NULL,
	evaluated_l8 INTEGER NOT NULL,
	-- Value serialized type
	type BLOB NOT NULL,
	-- Value serialized data
	data BLOB NOT NULL,
	PRIMARY KEY (id_u8, id_l8)
);

-- A table of (optional) paths associated with stored values (typically used for stored owned Paths).
CREATE TABLE IF NOT EXISTS associated_paths (
	-- Value identity
	value_u8 INTEGER,
	value_l8 INTEGER,
	-- A path (can be a file or directory) to associate with the value.
	path TEXT NOT NULL,
	FOREIGN KEY (value_u8, value_l8) REFERENCES stored (id_u8, id_l8) ON DELETE SET NULL
);

-- A table of (optional) diagnostic sources associated with stored values (typically used for stored Errors).
CREATE TABLE IF NOT EXISTS associated_diagnostic_sources (
	-- Value identity
	value_u8 INTEGER,
	value_l8 INTEGER,
	-- The path to the source.
	path TEXT NOT NULL,
	-- Whether the source is a binary or file source.
	binary INTEGER NOT NULL,
	FOREIGN KEY (value_u8, value_l8) REFERENCES stored (id_u8, id_l8) ON DELETE SET NULL
);

-- References between values.
--
-- Using these and the cache entries as roots, we can determine if values are
-- no longer needed.
CREATE TABLE IF NOT EXISTS value_references (
	-- Source value id.
	from_u8 INTEGER NOT NULL,
	from_l8 INTEGER NOT NULL,
	-- Id of a value to which the source value refers.
	to_u8 INTEGER NOT NULL,
	to_l8 INTEGER NOT NULL,
	FOREIGN KEY (from_u8, from_l8) REFERENCES stored (id_u8, id_l8) ON DELETE CASCADE,
	FOREIGN KEY (to_u8, to_l8) REFERENCES stored (id_u8, id_l8) ON DELETE RESTRICT
);

-- The cache entries (root values).
CREATE TABLE IF NOT EXISTS cache_entries (
	-- The cache key
	key_u8 INTEGER NOT NULL,
	key_l8 INTEGER NOT NULL,
	-- The stored value id
	value_u8 INTEGER NOT NULL,
	value_l8 INTEGER NOT NULL,
	-- The creation time (unix epoch UTC)
	creation_time INTEGER NOT NULL,
	-- The expiration time (unix epoch UTC), if any
	expiration_time INTEGER,
	PRIMARY KEY (key_u8, key_l8),
	FOREIGN KEY (value_u8, value_l8) REFERENCES stored (id_u8, id_l8) ON DELETE RESTRICT
);

-- Create indices on the commonly queried keys.
CREATE UNIQUE INDEX IF NOT EXISTS cache_entries_keys ON cache_entries (key_u8, key_l8);
CREATE UNIQUE INDEX IF NOT EXISTS stored_ids ON stored (id_u8, id_l8);
