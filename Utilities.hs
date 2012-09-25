

-- A File to hold utility properties


module Utilities where


import Data.Time


type Title 		= String
type Student 	= String
type Limit		= Integer


type Y 	= Integer
type M 	= Int
type D 	= Int

type Date 	= (D,M,Y)



getDate :: Date -> Day
getDate (d,m,y) = fromGregorian y m d


dbName :: String
dbName = "db"
