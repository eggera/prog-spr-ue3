{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE StandaloneDeriving     #-}

module Query (
	Query, eval
) where

import ReadPUtils
import Db

import Data.List
import Debug.Trace

import Control.Applicative 

-- ***** DATA STRUCTURES ***********************************************************************************************

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

data Query a where
	Query :: Selector a -> [Filter a] -> Mapping a b -> Query b

	Union :: Query a -> Query a -> Query a

-- ***** EVALUATING QUERIES ********************************************************************************************

eval :: Query a -> Db -> [a]
eval (Query s fs m) = execMapping m . execFilters fs . execSelect s

execSelect :: Selector a -> Db -> [a]
execSelect  Students           = error "TODO"
execSelect  Events             = error "TODO"

execFilters :: [Filter a] -> [a] -> [a]
execFilters = foldl (.) id . map execFilter

execFilter :: Filter a -> [a] -> [a]
execFilter  IsCourse           = error "TODO"
execFilter  IsInterview        = error "TODO"
execFilter  IsExam             = error "TODO"
execFilter  (Student_NameEq n) = error "TODO"
execFilter  (Event_NameEq   n) = error "TODO"
execFilter  (Event_CourseEq c) = error "TODO"

execMapping :: Mapping a b -> [a] -> [b]
execMapping Id                 = error "TODO"
execMapping EventsForStudents  = error "TODO"
execMapping StudentsForEvents  = error "TODO"

-- ***** SHOW INSTANCES ************************************************************************************************

{-
deriving instance Show (Selector a)
deriving instance Show (Filter a)
deriving instance Show (Mapping a b)
deriving instance Show (Query a)
-}

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

inParens r = between (char '(') (char ')') r

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


