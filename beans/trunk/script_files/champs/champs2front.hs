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
	, eleven :: Maybe Grade, twelve :: Maybe Grade
	, thirteen :: Maybe Grade, fourteen :: Maybe Grade
	, twentyone :: Maybe Grade, twentytwo :: Maybe Grade
	, twentythree :: Maybe Grade, twentyfour :: Maybe Grade
	, thirtyone :: Maybe Grade, thirtytwo :: Maybe Grade
	, thirtythree :: Maybe Grade, thirtyfour :: Maybe Grade
	, fortyone :: Maybe Grade, fortytwo :: Maybe Grade
	, fiftyone :: Maybe Grade, fiftytwo :: Maybe Grade
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
		eleven <- o .:? "1-1" .!= Nothing
		twelve <- o .:? "1-2" .!= Nothing
		thirteen <- o .:? "1-3" .!= Nothing
		fourteen <- o .:? "1-4" .!= Nothing
		twentyone <- o .:? "2-1" .!= Nothing
		twentytwo <- o .:? "2-2" .!= Nothing
		twentythree <- o .:? "2-3" .!= Nothing
		twentyfour <- o .:? "2-4" .!= Nothing
		thirtyone <- o .:? "3-1" .!= Nothing
		thirtytwo <- o .:? "3-2" .!= Nothing
		thirtythree <- o .:? "3-3" .!= Nothing
		thirtyfour <- o .:? "3-4" .!= Nothing
		fortyone <- o .:? "4-1" .!= Nothing
		fortytwo <- o .:? "4-2" .!= Nothing
		fiftyone <- o .:? "5-1" .!= Nothing
		fiftytwo <- o .:? "5-2" .!= Nothing
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
		, thirteen, fourteen
		, twentyone, twentytwo
		, twentythree, twentyfour
		, thirtyone, thirtytwo
		, thirtythree, thirtyfour
		, fortyone, fortytwo
		, fiftyone, fiftytwo
		]
	let points = Pre.map (\group-> let
			is = q2is (qz cwk)
			g = Pre.maybe (Gr {tardy = [], absent = [], p = [], rs = []})
				id group
			p_sum = Pre.sum (p g)
			as = Pre.zipWith (\i r -> ((a i) == (r2a r)))
				is (rs g)
			in
			(g, p_sum + Pre.length (Pre.filter ( (==) True) as ) )
			) groups
	let max = Pre.maximum (Pre.map snd points)
	let min = Pre.minimum (Pre.map snd points)
	let grade :: Maybe Grade -> Maybe Grade ; grade Nothing = Nothing; grade (Just g)  = let
		raw = point g points
		merit :: Int -> Float
		merit p | p == max = 3
		merit p | p == min = 2
		merit p = 2 + fromIntegral (raw - min) / fromIntegral (max - min)
		in
		Just (Gr {tardy = tardy g, absent = absent g, merits = merit raw, rs = rs g, p = p g})
	let monday = day_zero + 7 * (read r)
	let date = monday + (addDayFor l)
	let (month,day) = dayOfYearToMonthAndDay False date
	let iso8601_date = "(014) 2015-" ++ (show month) ++ "-" ++ (show day)
	let cwk' = Cwk { topic = top
		, eleven = grade (eleven cwk), twelve = grade (twelve cwk)
		, thirteen = grade (thirteen cwk), fourteen = grade (fourteen cwk)
		, twentyone = grade (twentyone cwk), twentytwo = grade (twentytwo cwk)
		, twentythree = grade (twentythree cwk), twentyfour = grade (twentyfour cwk)
		, thirtyone = grade (thirtyone cwk), thirtytwo = grade (thirtytwo cwk)
		, thirtythree = grade (thirtythree cwk), thirtyfour = grade (thirtyfour cwk)
		, fortyone = grade (fortyone cwk), fortytwo = grade (fortyone cwk)
		, fiftyone = grade (fiftyone cwk), fiftytwo = grade (fiftyone cwk)
		, qz = quiz, day = iso8601_date }
	Data.ByteString.putStrLn (encodePretty (setConfCompare compare defConfig) cwk')

main :: IO ()
main = execParser opts >>= champed where
	opts = info (helper <*> cline) (fullDesc
		<> progDesc "Grade champs 2 front for LEAGUE in ROUND"
		<> header "champs2front.hs - grade champs2front" )

