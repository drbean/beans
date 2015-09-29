{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import GHC.Generics
import Data.Yaml
import Data.Aeson
import Options.Applicative

data Member = Member Text deriving (Show,Generic)
data League = League { member :: [Member] } deriving (Show,Generic)
data Group = G [Member] deriving (Show,Generic)
data Session = S { eleven :: Group, twelve :: Group, twentyone :: Group } deriving (Show,Generic)
data Grade = Gr { tardy :: [Member], absent :: [Member], merits :: Int } deriving (Show,Generic)
data Classwork = Cwk { topic :: Text , eleven :: Grade, twelve :: Grade, twentyone :: Grade, qz :: Quiz, r :: [Responses] } deriving (Show,Generic)
data Question = Q Text deriving (Show,Generic)
data Option = O Text deriving (Show,Generic)
data Answer = A Text deriving (Show,Generic)
data Item = I { q :: Question, o :: [Option], a :: Answer } deriving (Show,Generic)
data Quiz = Qz [Item] deriving (Show,Generic)
data Response = R Answer Answer Answer deriving (Show,Generic)
instance FromJSON Member
instance FromJSON League
instance FromJSON Group
instance FromJSON Classwork
instance FromJSON Question
instance FromJSON Option
instance FromJSON Answer
instance FromJSON Item
instance FromJSON Quiz
instance FromJSON Response
instance ToJSON Member
instance ToJSON League
instance ToJSON Group
instance ToJSON Classwork
instance ToJSON Question
instance ToJSON Option
instance ToJSON Answer
instance ToJSON Item
instance ToJSON Quiz
instance ToJSON Response

group = [ G "1-1", G "1-2", G "2-1", G "2-2", G "3-1", G "3-2", G "4-1", G "4-2" ]

main = do
	let yaml = "/home/drbean/041/FLA0008/session/1/groups.yaml"
	y <- Data.Yaml.decodeFile yaml :: IO (Maybe [Group])
	let member = case y of
		Just ( League l ) -> l
		Nothing -> error "no parse of league.yaml"
		_ -> error "league.yaml parsed, but no members"
	let f = "/home/drbean/041/FLA0008/homework/1.yaml"
	let grades = Prelude.map (\m -> Grade { i = (Main.id m), grade_point = 0 } ) member
	let hwk = Hwk { exercise = "lerman", grades = grades }
	y <- Data.Yaml.decodeFile f :: IO (Maybe Homework)
	Data.Yaml.encodeFile "/home/drbean/041/FLA0008/homework/1.yaml" hwk
	-- let f'' = fromText f
	-- f''' <- (Data.ByteString.readFile f')
	return (hwk, y)
	--case y of (Just (Hwk t g)) -> print ("topic: " <> t ) ; Nothing -> error "No parse"
	--let players = [ "fdsfds", "dfsd", "fdssd" ]
	-- let z = Data.Aeson.toJSON (Hwk "gillian" grade )
	-- print (Hwk "self" players)
