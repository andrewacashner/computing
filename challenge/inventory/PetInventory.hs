-- could not compile bc of stack install hell

import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')
import Data.List
import Data.Text

data Pet = Pet { name :: String, says :: String }
type Dog = Pet
type Cat = Pet

toString :: Pet -> String
toString p = p.name

speak :: Pet -> String
speak p = p.says

speak :: Pet -> String
speak pet = "?"

speak :: Dog -> String
speak dog = "Woof"

speak :: Cat -> String
speak cat = "Meow"

rollCall :: [Pet] -> String
rollCall pets = intercalate "\n" messages
    where 
        messages = map greet pets
        greet pet = printf "%s says \"%s\"!" (toString pet) (speak pet)

main :: IO()
main = do

    let 
        generator = newStdGen

        dogNames = [ "Bowser", "Rex", "Professor McMann" ]
        catNames = [ "Fluffy", "Miss Thang", "Monique" ]

        dogs = map Dog dogNames :: [Dog]
        cats = map Cat catNames :: [Cat]
        pets = [dogs, cats]

        -- pets = concat 
        --         $ map (\(d, c) -> [Dog d, Cat c]) 
        --         $ zip dogs cats

        pets = pets + Pet "Baron von Fiend"

        pets = shuffle' pets (length pets) generator
        
        msg = rollCall pets

    putStrLn(msg)








