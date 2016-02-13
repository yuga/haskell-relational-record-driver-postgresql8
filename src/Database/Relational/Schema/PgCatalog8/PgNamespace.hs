{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Database.Relational.Schema.PgCatalog8.PgNamespace
-- Copyright   : 2013 Kei Hibino, 2014 Shohei Murayama
-- License     : BSD3
--
-- Maintainer  : shohei.murayama@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.PgCatalog8.PgNamespace where

import Data.Int (Int32)

import Database.Relational.Query.TH (defineTableTypesAndRecord)
import Database.Relational.Schema.PgCatalog8.Config (config)

$(defineTableTypesAndRecord config
  "PG_CATALOG" "pg_namespace"
  [("oid"    , [t| Int32 |]),
 -- nspname  | name      | not null
   ("nspname", [t| String |])
 -- nspowner | oid       | not null
 -- nspacl   | aclitem[] |
  ]
  [''Show])
