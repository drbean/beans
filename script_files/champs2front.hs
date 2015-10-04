{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import GHC.Generics
import Data.Yaml
import Data.Aeson
import Data.Aeson.Types
import Options.Applicative

data CommandLine = Cline { league :: String, round :: String }

cline :: Options.Applicative.Parser CommandLine
cline = Cline
	<$> strOption
	(	long "league"
		<> short 'l'
		<> metavar "LEAGUE"
		<> help "FLA0008, etc" )
	<*> strOption
	(	long "round"
		<> short 'r'
		<> metavar "ROUND"
		<> help "1, 2, 3tc" )

data Member = Member Text deriving (Show,Generic,Eq)
data League = League { member :: [Member] } deriving (Show,Generic)
data Group = G [Member] deriving (Show,Generic)
data Session = S { eleven :: Group, twelve :: Group,
	twentyone :: Group, twentytwo :: Group,
	thirtyone :: Group, thirtytwo :: Group,
	fortyone :: Group, fortytwo :: Group
	} deriving (Show,Generic)
data Question = Q Text deriving (Show,Generic)
data Option = O Text deriving (Show,Generic)
data Answer = A Int deriving (Show,Generic,Eq)
data Item = I { q :: Question, o :: [Option], a :: Answer } deriving (Show,Generic)
data Quiz = Qz [Item] deriving (Show,Generic)
data Response = R Int deriving (Show,Generic,Eq)
data Grade = Gr { tardy :: [Member], absent :: [Member], rs :: [Response], merits :: Float, p :: Int } deriving (Show,Generic,Eq)
data Classwork = Cwk { topic :: Text ,
	eleven' :: Grade, twelve' :: Grade,
	twentyone' :: Grade, twentytwo' :: Grade,
	thirtyone' :: Grade, thirtytwo' :: Grade,
	fortyone' :: Grade, fortytwo' :: Grade,
	qz :: Quiz } deriving (Show,Generic)
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
q2is (Qz (i:[])) = [i]
q2is (Qz (i:is)) = i : (q2is (Qz is) )
q2is _ = error "No items in quiz?"
r2a :: Response -> Answer
r2a (R int) = (A int)
a2int (A int) = int
r2int (R int) = int
point :: Grade -> [( Grade, Int )] -> Int
point g pts = maybe 0 id (lookup g pts)

--test_cwk :: Value
--test_cwk = object [ "topic" .= "lerman",
--	"eleven'" .= object [ "tardy" .= [], "absent" .= [], "merits" .= 0, "rs" .= [ 0 1 ] ],
--	"twelve'" .= object [ "tardy" .= [], "absent" .= [], "merits" .= 0, "rs" .= [ 0 1 ] ],
--	"twentyone'" .= object [ "tardy" .= [], "absent" .= [], "merits" .= 0, "rs" .= [ 0 1 ] ],
--	"quiz" .= [ (object [ "q" .= "What is the answer", "o" .= [ "True", "False" ], "a" .= 0 ]), (object [ "q" .= "What is the answer", "o" .= [ "True", "False" ], "a" .= 0 ]) ]
--	]

champed :: CommandLine -> IO ()
champed (Cline l r) = do
	let f = "/home/drbean/041/" <> l <> "/classwork/" <> r <> ".yaml"
	z <- Data.Yaml.decodeFile f :: IO (Maybe Classwork)
	let cwk = case z of 
		Just c -> c
		Nothing -> error "no parse of classwork/1.yaml"
	let top = topic cwk
	let quiz = qz cwk
	let groups = Prelude.map (\f -> f cwk ) [
		eleven', twelve',
		twentyone', twentytwo',
		thirtyone', thirtytwo',
		fortyone', fortytwo'
		]
	let points = Prelude.map (\g -> let
			is = q2is (qz cwk)
			a0 = ((a (is!!0)) == (r2a (rs g!!0)))
			a1 = ((a (is!!1)) == (r2a (rs g!!1)))
			a2 = ((a (is!!2)) == (r2a (rs g!!2)))
			in
			(g, Prelude.length (Prelude.filter ( (==) True) [a0,a1,a2] ) )
			) groups
	let max = Prelude.maximum (Prelude.map snd points)
	let min = Prelude.minimum (Prelude.map snd points)
	let grades = Prelude.map (\g -> let
		raw = point g points
		merit :: Int -> Float
		merit p | p == max = 3
		merit p | p == min = 2
		merit p = 2 + fromIntegral (raw - min) / fromIntegral (max - min)
		in
		(Gr {tardy = tardy g, absent = absent g, merits = merit raw, rs = rs g, p = raw})) groups
	let cwk' = Cwk { topic = top,
		eleven' = grades!!0, twelve' = grades!!1,
		twentyone' = grades!!2, twentytwo' = grades!!3,
		thirtyone' = grades!!4, thirtytwo' = grades!!5,
		fortyone' = grades!!6, fortytwo' = grades!!7,
		qz = quiz }
	Data.Yaml.encodeFile f cwk'

main :: IO ()
main = execParser opts >>= champed where
	opts = info (helper <*> cline) (fullDesc
		<> progDesc "Grade champs 2 front for LEAGUE in ROUND"
		<> header "champs2front.hs - grade champs2front" )

