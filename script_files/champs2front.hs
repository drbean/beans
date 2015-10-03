{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import GHC.Generics
import Data.Yaml
import Data.Aeson
import Data.Aeson.Types
import Options.Applicative

data Member = Member Text deriving (Show,Generic)
data League = League { member :: [Member] } deriving (Show,Generic)
data Group = G [Member] deriving (Show,Generic)
data Session = S { eleven :: Group, twelve :: Group, twentyone :: Group } deriving (Show,Generic)
data Question = Q Text deriving (Show,Generic)
data Option = O Text deriving (Show,Generic)
data Answer = A Int deriving (Show,Generic,Eq)
data Item = I { q :: Question, o :: [Option], a :: Answer } deriving (Show,Generic)
data Quiz = Qz [Item] deriving (Show,Generic)
data Response = R Int deriving (Show,Generic)
data Grade = Gr { tardy :: [Member], absent :: [Member], rs :: [Response], merits :: Int } deriving (Show,Generic)
data Classwork = Cwk { topic :: Text , eleven' :: Grade, twelve' :: Grade, twentyone' :: Grade, qz :: Quiz } deriving (Show,Generic)
instance FromJSON Member
instance FromJSON League
instance FromJSON Group
instance FromJSON Session where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rewriteSessionField }
instance FromJSON Grade
instance FromJSON Classwork where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = rewriteClassworkField }
instance FromJSON Question
instance FromJSON Option
instance FromJSON Answer
instance FromJSON Item
instance FromJSON Quiz
instance FromJSON Response
instance ToJSON Member
instance ToJSON League
instance ToJSON Group
instance ToJSON Session
instance ToJSON Grade
instance ToJSON Classwork where toJSON = genericToJSON defaultOptions { fieldLabelModifier = rewriteClassworkField }
instance ToJSON Question
instance ToJSON Option
instance ToJSON Answer
instance ToJSON Item
instance ToJSON Quiz
instance ToJSON Response

rewriteSessionField	s = case s of
	"eleven"	-> "1-1" ; "twelve"	-> "1-2"
	"thirteen"	-> "1-3" ; "fourteen"	-> "1-4"
	"twentyone"	-> "2-1" ; "twentytwo"	-> "2-2"
	"twentythree"	-> "2-3" ; "twentyfour"	-> "2-4"
	"twentyfive"	-> "2-5" ; "twentysix"	-> "2-6"
	"thirtyone"	-> "3-1" ; "thirtytwo"	-> "3-2"
	"thirtythree"	-> "3-3" ; "thirtyfour"	-> "3-4"
	"fortyone"	-> "4-1" ; "fortytwo"	-> "4-2"
	"fortythree"	-> "4-3" ; "fortyfour"	-> "4-4"
	_	-> error ("No " ++ s ++ "field")

rewriteClassworkField s = case s of
	"eleven'"	-> "1-1" ; "twelve'"	-> "1-2"
	"thirteen'"	-> "1-3" ; "fourteen'"	-> "1-4"
	"twentyone'"	-> "2-1" ; "twentytwo'"	-> "2-2"
	"twentythree'"	-> "2-3" ; "twentyfour'"	-> "2-4"
	"twentyfive'"	-> "2-5" ; "twentysix'"	-> "2-6"
	"thirtyone'"	-> "3-1" ; "thirtytwo'"	-> "3-2"
	"thirtythree'"	-> "3-3" ; "thirtyfour'"	-> "3-4"
	"fortyone'"	-> "4-1" ; "fortytwo'"	-> "4-2"
	"fortythree'"	-> "4-3" ; "fortyfour'"	-> "4-4"
	"topic" -> "topic" ; "qz" -> "qz" ; "r" -> "r" ;
	_	-> error ("No " ++ s ++ "field")

s = "/home/drbean/041/FLA0008/session/1/groups.yaml"
f = "/home/drbean/041/FLA0008/classwork/1.yaml"
q2is :: Quiz -> [Item]
q2is (Qz is) = is
q2is _ = error "No items in quiz?"
r2a :: Response -> Answer
r2a (R int) = (A int)
a2int (A int) = int
r2int (R int) = int

test_cwk :: Value
test_cwk = object [ "topic" .= "lerman",
	"eleven'" .= object [ "tardy" .= [], "absent" .= [], "merits" .= 0, "responses" .= [ 0 1 ] ],
	"twelve'" .= object [ "tardy" .= [], "absent" .= [], "merits" .= 0, "responses" .= [ 0 1 ] ],
	"twentyone'" .= object [ "tardy" .= [], "absent" .= [], "merits" .= 0, "responses" .= [ 0 1 ] ],
	"quiz" .= [ (object [ "q" .= "What is the answer", "o" .= [ "True", "False" ], "a" .= 0 ]), (object [ "q" .= "What is the answer", "o" .= [ "True", "False" ], "a" .= 0 ]) ]
	]

main = do
	y <- Data.Yaml.decodeFile s :: IO (Maybe Session)
	let group = case y of
		Just s -> s
		Nothing -> error "no parse of groups.yaml"
	let f = "/home/drbean/041/FLA0008/classwork/1.yaml"
	z <- Data.Yaml.decodeFile f :: IO (Maybe Classwork)
	let cwk = case z of 
		Just c -> c
		Nothing -> error "no parse of classwork/1.yaml"
	let quiz = qz cwk
	let groups = Prelude.map (\f -> f cwk ) [ eleven', twelve', twentyone' ]
	let grades = Prelude.map (\g -> let
			is = q2is (qz cwk)
			a0 = ((a (is!!0)) == (r2a (rs g!!0)))
			a1 = ((a (is!!1)) == (r2a (rs g!!1)))
			-- a2 = ((a (is!!2)) == (r2a (rs g!!2)))
			m = Prelude.length (Prelude.filter ( (==) True) [a0,a1])
			in
			(Gr {tardy = tardy g, absent = absent g, merits = m, rs = rs g})) groups
	let cwk' = Cwk { topic = "lerman", eleven' = grades!!0, twelve' = grades!!1, twentyone' = grades!!2, qz = quiz }
	Data.Yaml.encodeFile "/home/drbean/041/FLA0008/classwork/1.yaml" cwk
	return (cwk, cwk')
	--case y of (Just (Hwk t g)) -> print ("topic: " <> t ) ; Nothing -> error "No parse"
	--let players = [ "fdsfds", "dfsd", "fdssd" ]
	-- let z = Data.Aeson.toJSON (Hwk "gillian" grade )
	-- print (Hwk "self" players)
