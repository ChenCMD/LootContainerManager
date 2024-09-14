ALTER TABLE loot_assets ADD COLUMN typ TEXT NOT NULL DEFAULT 'fixed';

ALTER TABLE loot_assets ADD COLUMN loot_table TEXT;
