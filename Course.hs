

-- Defines the data structure and operations a course contains

module Course where


import Event
import Utilities



-- students registration should be only one for each course, in this event every student can register for the course

data Course = Course { title :: Title, studentsRegistration :: [Event], listOfEvents :: [Event] }
					deriving (Read,Show)




-- Create a new course

new :: Title -> Course
new title = Course title [] []



--addExam :: Event -> Title -> Event



--addExInterview :: Event -> Title -> Event



