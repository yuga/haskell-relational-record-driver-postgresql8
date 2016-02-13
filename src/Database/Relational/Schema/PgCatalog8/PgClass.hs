{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Database.Relational.Schema.PgCatalog8.PgClass
-- Copyright   : 2013 Kei Hibino, 2014 Sohei Murayama
-- License     : BSD3
--
-- Maintainer  : shohei.murayama@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.PgCatalog8.PgClass where

import Data.Int (Int32)

import Database.Relational.Query.TH (defineTableTypesAndRecord)
import Database.Relational.Schema.PgCatalog8.Config (config)

$(defineTableTypesAndRecord config
  "PG_CATALOG" "pg_class"
  [("oid"         , [t| Int32 |]),
 -- relname        | name      | not null
   ("relname"     , [t| String |]),
 -- relnamespace   | oid       | not null
   ("relnamespace", [t| Int32 |])
 -- reltype        | oid       | not null
 -- reloftype      | oid       | not null <= since 9.0
 -- relowner       | oid       | not null
 -- relam          | oid       | not null
 -- relfilenode    | oid       | not null
 -- reltablespace  | oid       | not null
 -- relpages       | integer   | not null
 -- reltuples      | real      | not null
 -- reltoastrelid  | oid       | not null
 -- reltoastidxid  | oid       | not null
 -- relhasindex    | boolean   | not null
 -- relisshared    | boolean   | not null
 -- relpersistence | "char"    | not null <= since 9.1
 -- relkind        | "char"    | not null
 -- relnatts       | smallint  | not null
 -- relchecks      | smallint  | not null
 -- reltriggers    | smallint  | not null <= until 8.3
 -- relukeys       | smallint  | not null <= until 8.3
 -- relfkeys       | smallint  | not null <= until 8.3
 -- relrefs        | smallint  | not null <= until 8.3
 -- relhasoids     | boolean   | not null
 -- relhaspkey     | boolean   | not null
 -- relhasrules    | boolean   | not null
 -- relhastriggers | boolean   | not null <= since 8.4
 -- relhassubclass | boolean   | not null
 -- relfrozenxid   | xid       | not null <= since 8.2
 -- relminmxid     | xid       | not null <= since 9.3
 -- relacl         | aclitem[] |
 -- reloptions     | text[]    |          <= since 8.2
  ]
  [''Show])
