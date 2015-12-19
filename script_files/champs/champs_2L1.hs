#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude as Pre
import Data.Text
import GHC.Generics
import Data.ByteString
import Data.Yaml
import Data.Yaml.Pretty
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Calendar.MonthDay
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
data Question = Q Text deriving (Show,Generic)
data Option = O Text deriving (Show,Generic)
data Answer = A Int deriving (Show,Generic,Eq)
data Item = I { q :: Question, o :: [Option], a :: Answer } deriving (Show,Generic)
data Quiz = Qz [Item] deriving (Show,Generic)
data Response = R Int deriving (Show,Generic,Eq)
data Grade = Gr { tardy :: [Member], absent :: [Member], rs :: [Response], merits :: Float, p :: [Int] } deriving (Show,Generic,Eq)
data Classwork = Cwk { topic :: Text 
	, eleven :: Grade, twelve :: Grade
	, twentyone :: Grade, twentytwo :: Grade
	, thirtyone :: Grade, thirtytwo :: Grade
	, fortyone :: Grade, fortytwo :: Grade
	, fiftyone :: Grade, fiftytwo :: Grade
	, qz :: Quiz, day :: String } deriving (Show,Generic)
instance FromJSON Member
instance FromJSON League
instance FromJSON Group
instance FromJSON Grade where
	parseJSON = withObject "grade" $ \o -> do
		tardy <- o .:? "tardy" .!= []
		absent <- o .:? "absent" .!= []
		rs <- o .: "rs"
		p <- o .:? "p" .!= []
		merits <- o .:? "merits" .!= 0
		return Gr {..}
instance FromJSON Classwork where
	parseJSON = withObject "classwork" $ \o -> do
		topic <- o .: "topic"
		day <- o .:? "day" .!= ""
		qz <- o .: "qz"
		eleven <- o .: "1-1"
		twelve <- o .: "1-2"
		twentyone <- o .: "2-1"
		twentytwo <- o .: "2-2"
		thirtyone <- o .: "3-1"
		thirtytwo <- o .: "3-2"
		fortyone <- o .: "4-1"
		fortytwo <- o .: "4-2"
		fiftyone <- o .: "5-1"
		fiftytwo <- o .: "5-2"
		return Cwk {..}

instance FromJSON Question
instance FromJSON Option
instance FromJSON Answer
instance FromJSON Item
instance FromJSON Quiz
instance FromJSON Response
instance ToJSON Member
instance ToJSON League
instance ToJSON Group
instance ToJSON Grade
instance ToJSON Classwork where toJSON = genericToJSON defaultOptions { fieldLabelModifier = rewriteClassworkField }
instance ToJSON Question
instance ToJSON Option
instance ToJSON Answer
instance ToJSON Item
instance ToJSON Quiz
instance ToJSON Response

rewriteClassworkField	s = case s of
	"eleven"	-> "1-1" ; "twelve"	-> "1-2"
	"thirteen"	-> "1-3" ; "fourteen"	-> "1-4"
	"twentyone"	-> "2-1" ; "twentytwo"	-> "2-2"
	"twentythree"	-> "2-3" ; "twentyfour"	-> "2-4"
	"twentyfive"	-> "2-5" ; "twentysix"	-> "2-6"
	"thirtyone"	-> "3-1" ; "thirtytwo"	-> "3-2"
	"thirtythree"	-> "3-3" ; "thirtyfour"	-> "3-4"
	"fortyone"	-> "4-1" ; "fortytwo"	-> "4-2"
	"fortythree"	-> "4-3" ; "fortyfour"	-> "4-4"
	"fortyfive"	-> "4-5" ; "fortysix"	-> "4-6"
	"fiftyone"	-> "5-1" ; "fiftytwo"	-> "5-2"
	"topic" -> "topic" ; "qz" -> "qz" ; "r" -> "r" ; "day" -> "day"
	_	-> error ("No " ++ s ++ " field")

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
day_zero = 250
addDayFor :: String -> Int
addDayFor "2L1" = 0
addDayFor "FLA0003" = 0
addDayFor "FLA0011" = 2
addDayFor "FLA0008" = 3
addDayFor "FLA0024" = 3
addDayFor "MB1" = 5
addDayFor "KB1" = 5

--test_cwk :: Value
--test_cwk = object [ "topic" .= "lerman",
--	"eleven'" .= object [ "tardy" .= [], "absent" .= [], "merits" .= 0, "rs" .= [ 0 1 ] ],
--	"twelve'" .= object [ "tardy" .= [], "absent" .= [], "merits" .= 0, "rs" .= [ 0 1 ] ],
--	"twentyone'" .= object [ "tardy" .= [], "absent" .= [], "merits" .= 0, "rs" .= [ 0 1 ] ],
--	"quiz" .= [ (object [ "q" .= "What is the answer", "o" .= [ "True", "False" ], "a" .= 0 ]), (object [ "q" .= "What is the answer", "o" .= [ "True", "False" ], "a" .= 0 ]) ]
--	]

champed :: CommandLine -> IO ()
champed (Cline l r) = do
	let f = "/home/drbean/041/" <> l <> "/classwork/" <> r <> ".yaml.table"
	y <- Data.ByteString.readFile f
	let z = Data.Yaml.decodeEither y :: Either String Classwork
	let cwk = case z of 
		Right c -> c
		Left what -> error ("No parse of classwork/" <> r <> ".yaml.table: " <> what)
	let top = topic cwk
	let quiz = qz cwk
	let groups = Pre.map (\f -> f cwk ) [
		eleven, twelve
		, twentyone, twentytwo
		, thirtyone, thirtytwo
		, fortyone, fortytwo
		, fiftyone, fiftytwo
		]
	let points = Pre.map (\g -> let
			p_sum = Pre.sum (p g)
			is = q2is (qz cwk)
			as = Pre.zipWith (\i r -> ((a i) == (r2a r)))
				is (rs g)
			in
			(g, p_sum + Pre.length (Pre.filter ( (==) True) as ) )
			) groups
	let max = Pre.maximum (Pre.map snd points)
	let min = Pre.minimum (Pre.map snd points)
	let grades = Pre.map (\g -> let
		raw = point g points
		merit :: Int -> Float
		merit p | p == max = 3
		merit p | p == min = 2
		merit p = 2 + fromIntegral (raw - min) / fromIntegral (max - min)
		in
		(Gr {tardy = tardy g, absent = absent g, merits = merit raw, rs = rs g, p = p g})) groups
	let monday = day_zero + 7 * (read r)
	let date = monday + (addDayFor l)
	let (month,day) = dayOfYearToMonthAndDay False date
	let iso8601_date = "(014) 2015-" ++ (show month) ++ "-" ++ (show day)
	let cwk' = Cwk { topic = top
		, eleven = grades!!0, twelve = grades!!1
		, twentyone = grades!!2, twentytwo = grades!!3
		, thirtyone = grades!!4, thirtytwo = grades!!5
		, fortyone = grades!!6, fortytwo = grades!!7
		, fiftyone = grades!!8, fiftytwo = grades!!9
		, qz = quiz, day = iso8601_date }
	Data.ByteString.putStrLn (encodePretty (setConfCompare compare defConfig) cwk')

main :: IO ()
main = execParser opts >>= champed where
	opts = info (helper <*> cline) (fullDesc
		<> progDesc "Grade champs 2 front for LEAGUE in ROUND"
		<> header "champs2front.hs - grade champs2front" )

