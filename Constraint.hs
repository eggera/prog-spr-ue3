{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE FlexibleInstances      #-}

module Constraint where

import Prelude hiding (Ordering(..))

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

data DB = DB {
	students :: [Student],
	events   :: [Event]
}
	deriving (Read,Show)

type Student = String

data Event = Course      { title :: Title, regStudents :: [Student], from, to :: Date, constraints :: [Constraint] }
           | Exam        { title :: Title, regStudents :: [Student], from, to :: Date, constraints :: [Constraint], course :: String }
           | ExInterview { title :: Title, regStudents :: [Student], from, to :: Date, constraints :: [Constraint], course :: String }
	deriving (Read,Show)

-- ***** db accessors/mutators

-- Creates an empty db
new :: DB
new = DB [] []

-- Adds a new Event/Student to the db
addStudent db s = db { students = s:students db }
addEvent   db e = db { events   = e:events   db }

-- ***** event accessors/mutators

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

newCourseEvt :: Title -> Date -> Date -> [Constraint] -> Event
newCourseEvt []    _    _  _  = error "No title specified"
newCourseEvt title from to cs = Course title [] from to cs

newExamEvt :: Title -> String -> Date -> Date -> [Constraint] -> Event
newExamEvt []    _      _    _  _  = error "No title specified"
newExamEvt title course from to cs = Exam title [] from to cs course 

newExInterviewEvt :: Title -> String -> Date -> Date -> [Constraint] -> Event
newExInterviewEvt []    _      _    _  _  = error "No title specified"
newExInterviewEvt title course from to cs = ExInterview title [] from to cs course 

addConstraint :: Event -> Constraint -> Event
addConstraint e c = e { constraints = c:constraints e }

-- ***** LENSES FOR ACCESSING EVENTS

data Lens a where
	TitleLens  :: Lens String
	FromLens   :: Lens Date
	ToLens     :: Lens Date
	CourseLens :: Lens String

instance Show (Lens a) where
	show TitleLens  = "title"
	show FromLens   = "from"
	show ToLens     = "to"
	show CourseLens = "course"

get :: Event -> Lens a -> a
get e          TitleLens  = title  e
get e          FromLens   = from   e
get e          ToLens     = to     e
get Course{..} CourseLens = error "Can't get a courses 'course' attribute"
get e          CourseLens = course e

set :: Event -> Lens a -> a -> Event
set e          TitleLens  t = e { title  = t }
set e          FromLens   f = e { from   = f }
set e          ToLens     t = e { to     = t }
set Course{..} CourseLens _ = error "Can't set a courses 'course' attribute"
set e          CourseLens c = e { course = c }

-- *********************************************************************************************************************
-- *********************************************************************************************************************
-- ***** STATEMENTS THAT THE PROGRAM EXECUTES                                                                      *****
-- *********************************************************************************************************************
-- *********************************************************************************************************************

data Statement = StmtQuery 
               | StmtUpdate        
               | StmtInsertStudent Student
               | StmtInsertEvent   Event

-- ^ execute statement
--execStmt Statement


-- *********************************************************************************************************************
-- *********************************************************************************************************************
-- ***** DATABASE CONSTRAINTS                                                                                      *****
-- *********************************************************************************************************************
-- *********************************************************************************************************************

-- ^ check if a student fulfills a constraint.
--   returns False iff constraint is violated and db action should not be allowed

checkConstraint :: Constraint -> Student -> DB -> Bool
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

eval :: Query a -> DB -> [a]
eval (Query s fs m) db = execMapping m db . execFilters fs . execSelect s $ db

-- ***** INTERNALS *****************************************************************************************************

data Query a where
	Query :: Selector a -> [Filter a] -> Mapping a b -> Query b

	Union :: Query a -> Query a -> Query a

-- ^ extracts raw data from the DB (selects a 'table')
data Selector a where
	Students :: Selector Student
	Events   :: Selector Event

-- ^ filters intermediate query results
data Filter a where
	IsCourse    :: Filter Event
	IsInterview :: Filter Event
	IsExam      :: Filter Event

	Student_ByName :: Relation -> String -> Filter Student
	Event_ByField  :: (Read a, Show a, Ord a) => Lens a -> Relation -> a -> Filter Event

data Relation = LT
              | LTEQ
              | EQ
              | GTEQ
              | GT

-- ^ maps query results to another type
data Mapping a b where
	Id :: Mapping a a

	EventsForStudents :: Mapping Student Event
	StudentsForEvents :: Mapping Event   Student

-- ** EVALUATING QUERIES

execSelect :: Selector a -> DB -> [a]
execSelect  Students           = students
execSelect  Events             = events

execFilters :: [Filter a] -> [a] -> [a]
execFilters = foldl (.) id . map execFilter

execFilter :: Filter a -> [a] -> [a]
execFilter  IsCourse               = filter isCourse
execFilter  IsInterview            = filter isExInterview
execFilter  IsExam                 = filter isExam
execFilter  (Student_ByName   r n) = filter (cmp r n)
execFilter  (Event_ByField  l r a) = filter (\e -> cmp r a (get e l))

execMapping :: Mapping a b -> DB -> [a] -> [b]
execMapping Id                 db = id
execMapping EventsForStudents  db = concatMap (getEventsForStudent db)
execMapping StudentsForEvents  db = concatMap (getStudentsForEvent db)

-- ^ check if a relation between to elements holds
cmp :: Ord a => Relation -> a -> a -> Bool
cmp LT   = (<)
cmp LTEQ = (<=)
cmp EQ   = (==)
cmp GTEQ = (>=)
cmp GT   = (>)

-- ***** SHOW INSTANCES ************************************************************************************************

instance Show (Selector a) where
	show Students = "students"
	show Events   = "events"

instance Show (Filter a) where
	show IsCourse    = "is course"
	show IsInterview = "is interview"
	show IsExam      = "is exam"

	show (Student_ByName  r n) = "name"  ++ " " ++ show r ++ " " ++ show n
	show (Event_ByField l r a) = show l  ++ " " ++ show r ++ " " ++ show a

instance Show Relation where
	show LT   = "<"
	show LTEQ = "<="
	show EQ   = "="
	show GTEQ = ">="
	show GT   = ">"

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
	readPrec = between skipSpaces skipSpaces (optionalParens rStudentQuery)

instance Read (Query Event) where
	readPrec = between skipSpaces skipSpaces (optionalParens rEventQuery)



-- ^ given a parser for selectors, filters and mappings, parse a query
rQuery selector filter mapping = Query <$> selector <*> rFilters filter <*> mapping

-- ^ parse a student query
rStudentQuery :: ReadPrec (Query Student)
rStudentQuery =   rQuery rStudentSelector rStudentFilter rId
              <++ rQuery rEventSelector   rEventFilter   rEventMapping
              <++ (foldl1 Union <$> sepBy1 (parens rStudentQuery) (char '+'))

-- ^ parse an event query
rEventQuery :: ReadPrec (Query Event)
rEventQuery =   rQuery rEventSelector   rEventFilter   rId
            <++ rQuery rStudentSelector rStudentFilter (char '|' >> rStudentMapping)
            <++ (foldl1 Union <$> sepBy1 (parens rEventQuery) (char '+'))

-- ^ given a parser for filters, parse a list of filters
rFilters :: ReadPrec (Filter a) -> ReadPrec [Filter a]
rFilters f = option [] (char '|' >> sepBy1 f (char '|'))

-- ^ parse a selector for students
rStudentSelector = keyword "students" >> return Students

-- ^ parse a selector for events
rEventSelector   = keyword "events" >> return Events

-- ^ parse a students filter
rStudentFilter = keyword "name" >> (Student_ByName <$> rRel <*> readPrec)

-- ^ parse an event filter
rEventFilter   = choice [
	keywords ["is", "course"]    >> return IsCourse,
	keywords ["is", "interview"] >> return IsInterview,
	keywords ["is", "exam"]      >> return IsExam,
	(keyword "name" +++ keyword "title") >> ((Event_ByField TitleLens)  <$> rRel <*> readPrec),
	keyword "from"                       >> ((Event_ByField FromLens)   <$> rRel <*> readPrec),
	keyword "to"                         >> ((Event_ByField ToLens)     <$> rRel <*> readPrec),
	keyword "course"                     >> ((Event_ByField CourseLens) <$> rRel <*> readPrec)]

-- ^ parse a relation operator
rRel = choice [
	operator "<"  >> return LT,
	operator "<=" >> return LTEQ,
	operator "="  >> return EQ,
	operator ">=" >> return GTEQ,
	operator ">"  >> return GT]

-- ^ parse the identity mapping
rId :: ReadPrec (Mapping a a)
rId = (char '|' >> keyword "id" >> return Id) +++ return Id

-- ^ parse a mapping from students to events
rStudentMapping = char '|' >> keyword "events"   >> return EventsForStudents

-- ^ parse a mapping from events to students
rEventMapping   = char '|' >> keyword "students" >> return StudentsForEvents


