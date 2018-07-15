{-# LANGUAGE DeriveDataTypeable, GADTs #-}
import Data.Typeable
import Data.Data
data Person = Person { name :: String, age :: Int } deriving (Typeable, Data)

{-
*Main> typeOf (undefined :: Person)
Person
*Main> tyConName $ typeRepTyCon $ typeOf (undefined :: Person)
"Person"
*Main> tyConModule $ typeRepTyCon $ typeOf (undefined :: Person)
"Main"
*Main> dataTypeOf (undefined :: Person)
DataType {tycon = "Person", datarep = AlgRep [Person]}
*Main> dataTypeConstrs $ dataTypeOf (undefined :: Person)
[Person]
*Main> constrFields $ head $ dataTypeConstrs $ dataTypeOf (undefined :: Person)
["name","age"]
-}

f :: Typeable a => a -> String
f x = case (cast x :: Maybe Int) of
           Just i -> "I can treat i as an int in this branch " ++ show (i * i)
           Nothing -> case (cast x :: Maybe Bool) of
                           Just b -> "I can treat b as a bool in this branch " ++ if b then "yes" else "no"
                           Nothing -> "x was of some type other than Int or Bool"

{-
*Main> f True
"I can treat b as a bool in this branch yes"
*Main> f (3 :: Int)
"I can treat i as an int in this branch 9"
-}

data Admissible a where
    AdInt :: Admissible Int
    AdBool :: Admissible Bool
f2 :: Admissible a -> a -> String
f2 AdInt i = "I can treat i as an int in this branch " ++ show (i * i)
f2 AdBool b = "I can treat b as a bool in this branch " ++ if b then "yes" else "no"

{-
*Main> f2 AdInt 3
"I can treat i as an int in this branch 9"
*Main> f2 AdBool True
"I can treat b as a bool in this branch yes"
-}
