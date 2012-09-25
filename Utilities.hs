

-- A File to hold utility properties


module Utilities where


import Data.Time


type Title 		= String
type Student 	= String
type Limit		= Integer


type Y 	= Integer
type M 	= Int
type D 	= Int

type From 	= (D,M,Y)
type To 	= (D,M,Y)



getDate :: (D,M,Y) -> Day
getDate (d,m,y) = fromGregorian y m d


dbName :: String
dbName = "db"
