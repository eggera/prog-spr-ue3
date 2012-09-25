

-- This file contains the datatype db that wraps all datatypes and is read/written from a file

-- Since Events are contained within Courses and Constraints are contained within Events, 
-- the DB datatype consists only of a list of courses


module DB where


import List

import Course
import Event
import Constraint
import Utilities


data DB = DB { courses :: [Course] }
			deriving (Read,Show)



-- Creates an empty db

new :: DB
new = DB []


-- Adds a new Course to the db

addCourse :: DB -> Title -> DB
addCourse _ [] 	= error "No title specified"
addCourse (DB courses) title
			| duplicate == True 	= error "Duplicate course"
			| otherwise				= DB (newCourse:courses)
				where
					duplicate = courseExists title courses
					newCourse = Course.new title




-- ****** HELPER *******

-- Checks if a course with a given title already exists

courseExists :: Title -> [Course] -> Bool
courseExists [] 	_		= error "No title specified"
courseExists title [] 		= False
courseExists title ((Course t _ _):cs)
			| title == t 	= True
			| otherwise		= courseExists title cs



-- Gets the course related to the given title (which is unique)

--getCourse :: Title -> [Course] -> Maybe Course
--getCourse [] = Nothing
--getCourse title cs = 
