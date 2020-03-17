module Types where

data Ingredient = Nuts | Gluten | Soy | Dairy   deriving (Show, Eq)
type Recipe = [Ingredient]
data Price = P Int                              deriving (Show, Eq, Ord)
data Cupcake = CC Price Recipe                  deriving (Show, Eq)

type Tin = [Recipe]
data Spec = And Spec Spec | Or Spec Spec | Not Spec | HasCup Int Ingredient  deriving (Show,Eq)

data Bin = L | B Bin Bin  deriving (Show,Eq)
