{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE FlexibleInstances      #-}


module Constraint where

import ReadPUtils

import Data.List
import Debug.Trace

import Utilities

import Control.Applicative 

-- *********************************************************************************************************************
-- *********************************************************************************************************************
-- ***** BASIC DATA TYPES STORED IN DB                                                                             *****
-- *********************************************************************************************************************
-- *********************************************************************************************************************

data Db = Db {
	students :: [Student],
	events   :: [Event]
}

type Student = String

data Event = Course      { title :: Title, regStudents :: [Student], from, to :: Date, constraints :: [Constraint] }
           | Exam        { title :: Title, regStudents :: [Student], from, to :: Date, constraints :: [Constraint], course :: String }
           | ExInterview { title :: Title, regStudents :: [Student], from, to :: Date, constraints :: [Constraint], course :: String }
	deriving (Read,Show)

isCourse Course{..} = True
isCourse _          = False

isExam Exam{..} = True
isExam _        = False

isExInterview ExInterview{..} = True
isExInterview _               = False

evtCourse Course{..} = Nothing
evtCourse evt        = Just (course evt)

getEventsForStudent db s = filter (\e -> elem s (regStudents e)) (events db)
getStudentsForEvent db e = regStudents e

-- *********************************************************************************************************************
-- *********************************************************************************************************************
-- ***** DATABASE CONSTRAINTS                                                                                      *****
-- *********************************************************************************************************************
-- *********************************************************************************************************************

-- ^ check if a student fulfills a constraint.
--   returns False iff constraint is violated and db action should not be allowed

checkConstraint :: Constraint -> Student -> Db -> Bool
checkConstraint (Constraint All  q)        student db = error "TODO"
checkConstraint (Constraint Some q)        student db = error "TODO"
checkConstraint (Constraint (Exactly n) q) student db = error "TODO"
checkConstraint (And cs)                   student db = all (\c -> checkConstraint c student db) cs
checkConstraint (Or  cs)                   student db = any (\c -> checkConstraint c student db) cs
checkConstraint (Not c)                    student db = not (checkConstraint c student db)

-- ***** INTERNALS *****************************************************************************************************

data Constraint = Constraint Quantor (Query Event)
                | And [Constraint]
                | Or  [Constraint]
                | Not Constraint
	deriving (Read,Show)

data Quantor = All
             | Some
             | Exactly Int
	deriving (Read,Show)

-- *********************************************************************************************************************
-- *********************************************************************************************************************
-- ***** DATABASE QUERIES                                                                                          *****
-- *********************************************************************************************************************
-- *********************************************************************************************************************

-- ***** EVALUATING QUERIES ********************************************************************************************

eval :: Query a -> Db -> [a]
eval (Query s fs m) db = execMapping m db . execFilters fs . execSelect s $ db

-- ***** INTERNALS *****************************************************************************************************

data Query a where
	Query :: Selector a -> [Filter a] -> Mapping a b -> Query b

	Union :: Query a -> Query a -> Query a

data Selector a where
	Students :: Selector Student
	Events   :: Selector Event

data Filter a where
	IsCourse    :: Filter Event
	IsInterview :: Filter Event
	IsExam      :: Filter Event

	Student_NameEq :: String -> Filter Student

	Event_NameEq   :: String -> Filter Event
	Event_CourseEq :: String -> Filter Event

data Mapping a b where
	Id :: Mapping a a

	EventsForStudents :: Mapping Student Event
	StudentsForEvents :: Mapping Event   Student

-- ** EVALUATING QUERIES

execSelect :: Selector a -> Db -> [a]
execSelect  Students           = students
execSelect  Events             = events

execFilters :: [Filter a] -> [a] -> [a]
execFilters = foldl (.) id . map execFilter

execFilter :: Filter a -> [a] -> [a]
execFilter  IsCourse           = filter isCourse
execFilter  IsInterview        = filter isExInterview
execFilter  IsExam             = filter isExam
execFilter  (Student_NameEq n) = filter (n==)
execFilter  (Event_NameEq   n) = filter (\e -> title e     == n)
execFilter  (Event_CourseEq c) = filter (\e -> evtCourse e == Just c)

execMapping :: Mapping a b -> Db -> [a] -> [b]
execMapping Id                 db = id
execMapping EventsForStudents  db = concatMap (getEventsForStudent db)
execMapping StudentsForEvents  db = concatMap (getStudentsForEvent db)

-- ***** SHOW INSTANCES ************************************************************************************************

instance Show (Selector a) where
	show Students = "students"
	show Events   = "events"

instance Show (Filter a) where
	show IsCourse    = "is course"
	show IsInterview = "is interview"
	show IsExam      = "is exam"

	show (Student_NameEq n) = "name = " ++ show n

	show (Event_NameEq   n) = "name = "   ++ show n
	show (Event_CourseEq c) = "course = " ++ show c

instance Show (Mapping a b) where
	show Id                = "id"
	show EventsForStudents = "events"
	show StudentsForEvents = "students"

instance Show (Query a) where
	show (Query q [] Id) = show q
	show (Query q fs Id) = show q ++ concatMap ((" | "++) . show) fs
	show (Query q fs m)  = show q ++ concatMap ((" | "++) . show) fs ++ " | " ++ show m
	
	show (Union q1@Query{..} q2@Query{..}) = "(" ++ show q1 ++ ") + (" ++ show q2 ++ ")"
	show (Union q1@Query{..} u1)           = "(" ++ show q1 ++ ") + "  ++ show u1
	show (Union u1           q1@Query{..}) =        show u1 ++  " + (" ++ show q1 ++ ")"
	show (Union u1           u2)           =        show u1 ++  " + "  ++ show u2

-- ***** READ INSTANCES ************************************************************************************************

instance Read (Query Student) where
	readPrec = between skipSpaces skipSpaces (parens rStudentQuery)

instance Read (Query Event) where
	readPrec = between skipSpaces skipSpaces (parens rEventQuery)

--inParens r = between (char '(') (char ')') r

rQuery selector filter mapping = Query <$> selector <*> rFilters filter <*> mapping

rStudentQuery :: ReadPrec (Query Student)
rStudentQuery =   rQuery rStudentSelector rStudentFilter rId
              <++ rQuery rEventSelector   rEventFilter   (char '|' >> rEventMapping)
              <++ (foldl1 Union <$> sepBy1 (between (char '(') (char ')') rStudentQuery) (char '+'))

rEventQuery :: ReadPrec (Query Event)
rEventQuery =   rQuery rEventSelector   rEventFilter   rId
            +++ rQuery rStudentSelector rStudentFilter (char '|' >> rStudentMapping)
              <++ (foldl1 Union <$> sepBy1 (between (char '(') (char ')') rEventQuery) (char '+'))

rFilters :: ReadPrec (Filter a) -> ReadPrec [Filter a]
rFilters f = option [] (char '|' >> sepBy1 f (char '|'))

rStudentSelector = keyword "students" >> return Students
rEventSelector   = keyword "events" >> return Events

rStudentFilter = keyword "name" >> char '=' >> fmap Student_NameEq readPrec
rEventFilter   = choice [
	keywords ["is", "course"]    >> return IsCourse,
	keywords ["is", "interview"] >> return IsInterview,
	keywords ["is", "exam"]      >> return IsExam,
	keyword "name"   >> char '=' >> fmap Event_NameEq   readPrec,
	keyword "course" >> char '=' >> fmap Event_CourseEq readPrec]

rId :: ReadPrec (Mapping a a)
rId = return Id

rStudentMapping = keyword "events"   >> return EventsForStudents
rEventMapping   = keyword "students" >> return StudentsForEvents


