CREATE TABLE IF NOT EXISTS loot_assets (
  id          INTEGER  PRIMARY KEY AUTOINCREMENT,
  uuid        TEXT     NOT NULL,
  name        TEXT,

  UNIQUE (uuid)
);

CREATE TABLE IF NOT EXISTS loot_asset_containers (
  id          INTEGER  PRIMARY KEY AUTOINCREMENT,
  asset_id    INT      NOT NULL,
  world       TEXT     NOT NULL,
  x           INT      NOT NULL,
  y           INT      NOT NULL,
  z           INT      NOT NULL,
  block_id    TEXT     NOT NULL,
  facing      TEXT,
  waterlogged BOOLEAN,
  chest_type  TEXT,

  FOREIGN KEY (asset_id) REFERENCES loot_assets (id),
  UNIQUE (asset_id, world, x, y, z)
);

CREATE TABLE IF NOT EXISTS loot_asset_items (
  id          INTEGER  PRIMARY KEY AUTOINCREMENT,
  asset_id    INT      NOT NULL,
  slot        INT      NOT NULL,
  item        TEXT     NOT NULL,
  quantity    INT      NOT NULL,

  FOREIGN KEY (asset_id) REFERENCES loot_assets (id),
  UNIQUE (asset_id, slot)
);
