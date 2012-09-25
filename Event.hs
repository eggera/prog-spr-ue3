

-- Defines the data structure and operations that an event contains



module Event where


import Constraint
import Utilities



data Cours	 		= Cours 		{ cTitle :: Title, cRegStudents :: [Student], cFrom :: Date, cTo :: Date, cConstraints :: [Constraint] }
									deriving (Read, Show)

data Exam 			= Exam 			{ exmTitle :: Title, exmRegStudents :: [Student], exmFrom :: Date, exmTo :: Date, exmConstraints :: [Constraint] }
									deriving (Read, Show)

data ExInterview 	= ExInterview	{ intTitle :: Title, intRegStudents :: [Student], intFrom :: Date, intTo :: Date, intConstraints :: [Constraint] }
									deriving (Read, Show)


data Event =  EvtCourse 		Cours
			| EvtExam			Exam
			| EvtExInterview 	ExInterview
				deriving (Read,Show)








newCourseEvt :: Title -> Date -> Date -> [Constraint] -> Event
newCourseEvt [] _ _ _ = error "No title specified"
newCourseEvt title from to cs = EvtCourse (Cours title [] from to cs)


newExamEvt :: Title -> Date -> Date -> [Constraint] -> Event
newExamEvt [] _ _ _ = error "No title specified"
newExamEvt title from to cs = EvtExam (Exam title [] from to cs)


newExInterviewEvt :: Title -> Date -> Date -> [Constraint] -> Event
newExInterviewEvt [] _ _ _ = error "No title specified"
newExInterviewEvt title from to cs = EvtExInterview (ExInterview title [] from to cs)



addConstraint :: Event -> Constraint -> Event
addConstraint (EvtCourse (Cours t s fr to cs)) c 				= EvtCourse (Cours t s fr to (cs++[c]))
addConstraint (EvtExam (Exam t s fr to cs)) c 					= EvtExam (Exam t s fr to (cs++[c]))
addConstraint (EvtExInterview (ExInterview t s fr to cs)) c 	= EvtExInterview (ExInterview t s fr to (cs++[c]))




--registerStudent :: Event -> Student -> Date -> Maybe Event





