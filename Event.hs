

-- Defines the data structure and operations that an event contains



module Event where


import Constraint
import Utilities



data Course	 		= Course 		{ cTitle :: Title, cRegStudents :: [Student], cFrom :: Date, cTo :: Date, cConstraints :: [Constraint] }
									deriving (Read, Show)

data Exam 			= Exam 			{ exmTitle :: Title, exmCourse :: String, exmRegStudents :: [Student], 
											exmFrom :: Date, exmTo :: Date, exmConstraints :: [Constraint] }
									deriving (Read, Show)

data ExInterview 	= ExInterview	{ intTitle :: Title, intCourse :: String, intRegStudents :: [Student], 
											intFrom :: Date, intTo :: Date, intConstraints :: [Constraint] }
									deriving (Read, Show)


data Event =  EvtCourse 		Course
			| EvtExam			Exam
			| EvtExInterview 	ExInterview
				deriving (Read,Show)








newCourseEvt :: Title -> Date -> Date -> [Constraint] -> Event
newCourseEvt [] _ _ _ = error "No title specified"
newCourseEvt title from to cs = EvtCourse (Course title [] from to cs)


newExamEvt :: Title -> String -> Date -> Date -> [Constraint] -> Event
newExamEvt [] _ _ _ _ = error "No title specified"
newExamEvt title course from to cs = EvtExam (Exam title course [] from to cs)


newExInterviewEvt :: Title -> String -> Date -> Date -> [Constraint] -> Event
newExInterviewEvt [] _ _ _ _ = error "No title specified"
newExInterviewEvt title course from to cs = EvtExInterview (ExInterview title course [] from to cs)



addConstraint :: Event -> Constraint -> Event
addConstraint (EvtCourse (Course t s fr to cs)) c 					= EvtCourse (Course t s fr to (cs++[c]))
addConstraint (EvtExam (Exam t crs s fr to cs)) c 					= EvtExam (Exam t crs s fr to (cs++[c]))
addConstraint (EvtExInterview (ExInterview t crs s fr to cs)) c 	= EvtExInterview (ExInterview t crs s fr to (cs++[c]))




--registerStudent :: Event -> Student -> Date -> Maybe Event





