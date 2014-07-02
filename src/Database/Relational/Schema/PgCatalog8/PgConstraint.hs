{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Database.Relational.Schema.PgCatalog8.PgConstraint
-- Copyright   : 2013 Kei Hibino, 2014 Shohei Murayama
-- License     : BSD3
--
-- Maintainer  : shohei.murayama@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.PgCatalog8.PgConstraint where

import Data.Int (Int32)

import Database.Record.TH (derivingShow)
import Database.Relational.Query.TH (defineTableTypesAndRecordDefault)


$(defineTableTypesAndRecordDefault
  "PG_CATALOG" "pg_constraint"
  [ -- ("oid"    , [t| Int32 |]),
 -- conname       | name         | not null
 -- connamespace  | oid          | not null
 -- contype       | "char"       | not null
    ("contype",   [t| Char |]),
 -- condeferrable | boolean      | not null
 -- condeferred   | boolean      | not null
 -- convalidated  | boolean      | not null <= since 9.1
 -- conrelid      | oid          | not null
    ("conrelid",  [t| Int32 |])
 -- contypid      | oid          | not null
 -- conindid      | oid          | not null <= since 9.0
 -- confrelid     | oid          | not null
 -- confupdtype   | "char"       | not null
 -- confdeltype   | "char"       | not null
 -- confmatchtype | "char"       | not null
 -- conislocal    | boolean      | not null <= since 8.4
 -- coninhcount   | integer      | not null <= since 8.4
 -- connoinherit  | bool         | not null <= since 9.3
 -- conkey        | smallint[]   |
    -- ("conkey",  ???),
 -- confkey       | smallint[]   |
 -- conpfeqop     | oid[]        |          <= since 8.3
 -- conppeqop     | oid[]        |          <= since 8.3
 -- conffeqop     | oid[]        |          <= since 8.3
 -- conexclop     | oid[]        |          <= since 9.0
 -- conbin        | pg_node_tree |
 -- consrc        | text         |
  ]
  [derivingShow])
