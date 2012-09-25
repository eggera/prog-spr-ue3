

-- This file will be overwritten by Fabian


module Constraint where



data Constraint = Constraint Quantor Selector
                | And [Constraint]
                | Or  [Constraint]
                | Not Constraint
					deriving (Read,Show)

data Quantor = All
             | Some
             | Exactly Int
					deriving (Read,Show)

data Selector = SelectByCourse String
--            | SelectByType   EventType
              | Union Selector Selector
					deriving (Read,Show)
