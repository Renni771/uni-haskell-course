module MediaLib
  ( User,
  Title,
  Artist,
  Duration,
  UserRating,
  Rating,
  Track,
  MediaLibrary
  )
where

-- import Test.QuickCheck
import Tracks
import Data.Maybe

type User = String
type Title = String
type Artist = String
type Duration = Int
type AlbumName = String

newtype MediaLibrary = MediaLibrary [Album]

instance Show MediaLibrary where
    show (MediaLibrary albums)
        | null albums = "This media library is empty."
        | otherwise = show albums

data Album = Album AlbumName [Track]
    deriving (Eq)
instance Show Album where
    show (Album name tracks) =
        "\nAlbum: " ++ show name ++ "\nSongs: " ++ show tracks

data UserRating = UserRating User Rating
    deriving (Eq, Ord)
instance Show UserRating where
    show (UserRating u r) = "(" ++ show r ++ ") " ++ u

data Rating = Good | Bad
    deriving (Show, Eq, Ord)

data Track = Track Title Artist Duration [UserRating]
    deriving (Eq, Ord)
instance Show Track where
    show (Track title artist duration r) =
        " \n" ++ title ++ " by " ++ artist ++ " (" ++ show duration ++ "s) | Ratings: " ++ ratings ++ " "
            where ratings = show (map show r)

-- | Add a song to an album
addAlbum :: Track -> Album -> Album
addAlbum track (Album name tracks) =
    let updatedTracklist = tracks ++ [track] in
        if track `elem` tracks then Album name tracks else Album name updatedTracklist

-- | Rate a song for a particular user
rateTrack :: User -> Track -> Rating -> Track
rateTrack user (Track title artist duration ratings) rating =
    if ratedByUser user ratings then Track title artist duration updatedRating  else Track title artist duration addRating
        where
            addRating = ratings ++ [UserRating user rating]
            updatedRating = updateRating user rating ratings

updateRating :: User -> Rating -> [UserRating]  -> [UserRating]
updateRating user newRating = map (\(UserRating u rating) -> if u == user then UserRating u newRating else UserRating u rating)

ratedByUser :: User -> [UserRating] -> Bool
ratedByUser user = any (\(UserRating u rating) -> u == user)


-- Test Data
constructLibraryFromTestData :: MediaLibrary
constructLibraryFromTestData =  MediaLibrary (map tupleToAlbum tracklist)

tupleToAlbum :: (Maybe String, String, String, Int) -> Album
tupleToAlbum (album, artist, title, duration) =  
        case album of 
            Nothing -> Album "No Album" [Track title artist duration []]
            Just a -> Album a [Track title artist duration []]

tracks = tracklist
tenBillionYears = Track "Ten Billion Years" "The Faceless" 250 [UserRating "Rendani" Good]
hymnOfSanity = Track "Hymn of Sanity" "The Faceless" 230 [UserRating "Rendani" Good, UserRating "Fantano" Bad]
everlong = Track "Everlong" "Foo Fighters" 300 [UserRating "Rendani" Bad]
highRoad = Track "High Road" "Mastodon" 230 [UserRating "Rendani" Good, UserRating "Fantano" Good]