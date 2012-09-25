

-- Defines the data structure and operations an event contains



module Event where


import Utilities


data Event =  EvtCourse 		{ title :: Title, registeredStudents :: [Student], from :: From, to :: To, limit :: Limit }
			| EvtExam			{ title :: Title, registeredStudents :: [Student], from :: From, to :: To, limit :: Limit }
			| EvtExInterview 	{ title :: Title, registeredStudents :: [Student], from :: From, to :: To, limit :: Limit }
				deriving (Read,Show)








newCourseEvt :: Title -> From -> To -> Limit -> Event
newCourseEvt [] _ _ _ = error "No title specified"
newCourseEvt title from to limit = EvtCourse title [] from to limit


newExamEvt :: Title -> From -> To -> Limit -> Event
newExamEvt [] _ _ _ = error "No title specified"
newExamEvt title from to limit = EvtExam title [] from to limit


newExInterviewEvt :: Title -> From -> To -> Limit -> Event
newExInterviewEvt [] _ _ _ = error "No title specified"
newExInterviewEvt title from to limit = EvtExInterview title [] from to limit
