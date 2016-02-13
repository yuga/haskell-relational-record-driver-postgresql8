{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Database.Relational.Schema.PgCatalog8.PgType
-- Copyright   : 2013 Kei Hibino, 2014 Shohei Murayama
-- License     : BSD3
--
-- Maintainer  : shohei.murayama@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.PgCatalog8.PgType where

import Data.Int (Int16, Int32)

import Database.Relational.Query.TH (defineTableTypesAndRecord)
import Database.Relational.Schema.PgCatalog8.Config (config)

$(defineTableTypesAndRecord config
  "PG_CATALOG" "pg_type"
  [
    ("oid", [t|Int32|]),
-- Table "pg_catalog.pg_type"
--      Column     |     Type     | Modifiers
-- ----------------+--------------+-----------
--  oid            | oid          | not null <= since 9.3
--  typname        | name         | not null
    ("typname", [t|String|]),
--  typnamespace   | oid          | not null
    ("typnamespace", [t|Int32|]),
--  typowner       | oid          | not null
    ("typowner", [t|Int32|]),
--  typlen         | smallint     | not null
    ("typlen", [t|Int16|]),
--  typbyval       | boolean      | not null
    ("typbyval", [t|Bool|]),
--  typtype        | "char"       | not null
    ("typtype", [t|Char|]),
--  typcategory    | "char"       | not null <= since 8.4
    --("typcategory", [t|Char|]),
--  typispreferred | boolean      | not null <= since 8.4
    --("typispreferred", [t|Bool|]),
--  typisdefined   | boolean      | not null
    ("typisdefined", [t|Bool|]),
--  typdelim       | "char"       | not null
    ("typdelim", [t|Char|]),
--  typrelid       | oid          | not null
    ("typrelid", [t|Int32|]),
--  typelem        | oid          | not null
    ("typelem", [t|Int32|]),
--  typarray       | oid          | not null <= since 8.3
    --("typarray", [t|Int32|]),
--  typinput       | regproc      | not null
    -- ("typinput", [t||]),
--  typoutput      | regproc      | not null
    -- ("typoutput", [t||]),
--  typreceive     | regproc      | not null
    -- ("typreceive", [t||]),
--  typsend        | regproc      | not null
    -- ("typsend", [t||]),
--  typmodin       | regproc      | not null <= since 8.3
    -- ("typmodin", [t||]),
--  typmodout      | regproc      | not null <= since 8.3
    -- ("typmodout", [t||]),
--  typanalyze     | regproc      | not null
    -- ("typanalyze", [t||]),
--  typalign       | "char"       | not null
    ("typalign", [t|Char|]),
--  typstorage     | "char"       | not null
    ("typstorage", [t|Char|]),
--  typnotnull     | boolean      | not null
    ("typnotnull", [t|Bool|]),
--  typbasetype    | oid          | not null
    ("typbasetype", [t|Int32|]),
--  typtypmod      | integer      | not null
    ("typtypmod", [t|Int32|]),
--  typndims       | integer      | not null
    ("typndims", [t|Int32|]),
--  typcollation   | oid          | not null <= since 9.1
    --("typcollation", [t|Int32|]),
--  typdefaultbin  | pg_node_tree |
    -- ("typdefaultbin", [t||]),
--  typdefault     | text         |
    ("typdefault", [t|Maybe String|])
--  typacl         | aclitem[]    |          <= since 9.2
  ]
  [''Show])
