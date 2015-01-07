-- |
-- Module      : Database.Relational.Schema.PgCatalog8.Config
-- Copyright   : 2013 Kei Hibino, 2014 Shohei Murayama
-- License     : BSD3
--
-- Maintainer  : shohei.murayama@gmail.com
-- Stability   : experimental
-- Portability : unknown
module Database.Relational.Schema.PgCatalog8.Config (config) where

import Database.Relational.Query (Config, defaultConfig)


-- | Configuration parameter against PostgreSQL v8.x
config :: Config
config =  defaultConfig

