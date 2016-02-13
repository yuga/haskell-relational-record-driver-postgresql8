{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Database.HDBC.Schema.PgCatalog8.PgAttribute
-- Copyright   : 2013 Kei Hibino, 2014 Shohei Murayama
-- License     : BSD3
--
-- Maintainer  : shohei.murayama@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.PgCatalog8.PgAttribute where

import Data.Int (Int16, Int32)

import Database.Record.TH (derivingShow)
import Database.Relational.Query.TH (defineTableTypesAndRecord)
import Database.Relational.Schema.PgCatalog8.Config (config)

$(defineTableTypesAndRecord config
  "PG_CATALOG" "pg_attribute"

  [
-- Table "pg_catalog.pg_attribute"
--     Column     |   Type    | Modifiers
-- ---------------+-----------+-----------
--  attrelid      | oid       | not null
    ("attrelid"     , [t|Int32|]),
--  attname       | name      | not null
    ("attname"      , [t|String|]),
--  atttypid      | oid       | not null
    ("atttypid"     , [t|Int32|]),
--  attstattarget | integer   | not null
    ("attstattarget", [t|Int32|]),
--  attlen        | smallint  | not null
    ("attlen"       , [t|Int16|]),
--  attnum        | smallint  | not null
    ("attnum"      , [t|Int16|]),
--  attndims      | integer   | not null
    ("attndims"    , [t|Int32|]),
--  attcacheoff   | integer   | not null
    ("attcacheoff" , [t|Int32|]),
--  atttypmod     | integer   | not null
    ("atttypmod"   , [t|Int32|]),
--  attbyval      | boolean   | not null
    ("attbyval"    , [t|Bool|]),
--  attstorage    | "char"    | not null
    ("attstorage"  , [t|Char|]),
--  attalign      | "char"    | not null
    ("attalign"    , [t|Char|]),
--  attnotnull    | boolean   | not null
    ("attnotnull"  , [t|Bool|]),
--  atthasdef     | boolean   | not null
    ("atthasdef"   , [t|Bool|]),
--  attisdropped  | boolean   | not null
    ("attisdropped", [t|Bool|]),
--  attislocal    | boolean   | not null
    ("attislocal"  , [t|Bool|]),
--  attinhcount   | integer   | not null
    ("attinhcount" , [t|Int32|])
--  attcollation  | oid       | not null  <= since 9.1
    -- ("attcollation", [t|Int32|]),
--  attacl        | aclitem[] |           <= since 8.4
    -- ("attacl"      , [t|String|]),
--  attoptions    | text[]    |           <= since 9.0
    -- ("attoptions"  , [t|String|]),
--  attfdwoptions | text[]    |           <- since 9.2
    -- ("attfdwoptions", [t|String|])
  ]
  [derivingShow])
