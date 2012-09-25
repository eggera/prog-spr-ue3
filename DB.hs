

-- This file contains the datatype db that wraps all datatypes and is read/written from a file

-- Since Events are contained within Courses and Constraints are contained within Events, 
-- the DB datatype consists only of a list of courses


module DB where


import List

import Event
import Constraint
import Utilities


data DB = DB { courses :: [String] }
			deriving (Read,Show)



-- Creates an empty db

new :: DB
new = DB []


-- Adds a new Course to the db

addCourse :: DB -> Title -> DB
addCourse _ [] 	= error "No title specified"
addCourse (DB courses) course
			| sameTitles /= []		= error "Duplicate course"
			| otherwise				= DB (courses ++ [course])
				where
					sameTitles = filter (== course) courses



