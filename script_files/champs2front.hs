{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import GHC.Generics
import Data.Yaml
import Data.Aeson
import Options.Applicative

data Member = Member { id :: Id, name :: Text } deriving (Show)
data League = League { member :: [Member] } deriving (Show)
data Id = Player Text deriving (Show,Generic)
data Grade = Grade { i :: Id, grade_point :: Int } deriving (Show,Generic)
data Homework = Hwk { exercise :: Text , grades :: [Grade] } deriving (Show,Generic)
instance FromJSON Member where parseJSON = withObject "member" $ \o -> Member <$> o .: "id" <*> o .: "name"
instance FromJSON League where parseJSON = withObject "league" $ \o -> League <$> o .: "member"
instance FromJSON Id
instance FromJSON Grade
instance FromJSON Homework
instance ToJSON Id
instance ToJSON Grade -- where toJSON g = object [ "player" .= player g, "grade" .= grade g ]
instance ToJSON Homework 

main = do
	let yaml = "/home/drbean/041/FLA0008/league.yaml"
	y <- Data.Yaml.decodeFile yaml :: IO (Maybe League)
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
