{-# LANGUAGE TemplateHaskell #-}

module TutorialPractice where

-- import Control.Lens.Tutorial
import Control.Lens hiding (element)

data Atom = Atom { _element :: String, _point :: Point } deriving Show

data Point = Point { _x :: Double, _y :: Double } deriving Show

atom = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }

-- non-lens method
shiftAtomX :: Atom -> Atom
shiftAtomX (Atom e (Point x y)) = Atom e $ Point (x + 1) y

-- with lenses
makeLenses ''Atom
makeLenses ''Point

shiftAtomX' :: Atom -> Atom
shiftAtomX' = over (point . x) (+ 1)

data Molecule = Molecule { _atoms :: [Atom] } deriving Show

atom1 = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
atom2 = Atom { _element = "O", _point = Point { _x = 3.0, _y = 4.0 } }
molecule = Molecule { _atoms = [atom1, atom2] }

makeLenses ''Molecule

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+ 1)

-- manually creating the `point` lens
point' :: Lens' Atom Point
point' = lens _point (\atom' newPoint -> atom' { _point = newPoint })

-- same without depening on Lens
point'' :: Functor f => (Point -> f Point) -> Atom -> f Atom
point'' k atom = fmap (\newPoint -> atom { _point = newPoint }) (k (_point atom))
