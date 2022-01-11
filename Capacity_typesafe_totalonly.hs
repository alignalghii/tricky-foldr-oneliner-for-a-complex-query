module Capacity_typesafe_totalonly where

import Data.List (partition)

data Storage = HDD String Int Int | SSD String Int deriving (Show, Eq)

capacity :: Storage -> Int 
capacity (HDD _ _ cap) = cap 
capacity (SSD _ cap)= cap 

isHDD :: Storage -> Bool
isHDD (HDD _ _ _) = True
isHDD (SSD _ _)   = False

-- A maximumszámításnál szeretnénk elkerülni a parciális függvények használatát: még feltételvizsgálattal lekezelve sem szeretnénk ilyeneket használni.

-- Az egyik megoldás a minuszvégtelen használata
-- Egyetlen hátrány, hogy elvileg ez becsapható, ha az SSD-tárolók kapacitása is épp minusz égtelen. Ez azonban az adott feladatnál lehetetlen, hisz a tárolókapacitás nemnegatív.

hugeHDDs :: [Storage] -> [Storage]
hugeHDDs allStorages = let (hdds, ssds) = partition isHDD allStorages
                           maxSSDCap = foldr (max . capacity) minBound ssds
                       in filter ((> maxSSDCap) . capacity) hdds

-- Viszont ha mégis ragszkodunk a típusszintű védelmhez, van típusbiztos megoldás is: algebrai adattípus szintjén bővítjük ki az Int-et a minusz végtelen fogalmával.
--- Erre legegyszerűbb a meglevő Maybe használata.
-- Külön szerencse, hogy Just _ > Nothing, ez pont a kezünkre játszik: Maybe Int-en belül a Nothing természetes módon játssza a minusz végtelen szerepét.


hugeHDDs' :: [Storage] -> [Storage]
hugeHDDs' allStorages = let (hdds, ssds) = partition isHDD allStorages
                            maybeMaxSSDCap = foldr (max . Just . capacity) Nothing ssds
                        in filter ((> maybeMaxSSDCap) . Just . capacity) hdds


-- Tesztesetek, amiknek True-t kell visszadniuk:


bothAlternativesTest :: Bool
bothAlternativesTest = hugeHDDsTest && hugeHDDsTest'

hugeHDDsTest :: Bool
hugeHDDsTest =  hugeHDDs [] == []
             && hugeHDDs [HDD "Seagate" 5600 250 , HDD "Verbatim" 7200 1000, SSD "Samsung" 500, SSD "Samsung" 750] == [HDD "Verbatim" 7200 1000]
             && hugeHDDs [HDD "Seagate" 5600 250 , HDD "Verbatim" 7200 100 , SSD "Samsung" 250, SSD "Samsung" 100] == []
             && hugeHDDs [HDD "Seagate" 5600 1000, HDD "Verbatim" 7200 1500, SSD "Samsung" 250, SSD "Samsung" 500] == [HDD "Seagate" 5600 1000, HDD "Verbatim" 7200 1500]
             && hugeHDDs [HDD "Seagate" 5600 1000, HDD "Verbatim" 7200 1500                                      ] == [HDD "Seagate" 5600 1000, HDD "Verbatim" 7200 1500]

hugeHDDsTest' :: Bool
hugeHDDsTest' =  hugeHDDs' [] == []
             && hugeHDDs' [HDD "Seagate" 5600 250 , HDD "Verbatim" 7200 1000, SSD "Samsung" 500, SSD "Samsung" 750] == [HDD "Verbatim" 7200 1000]
             && hugeHDDs' [HDD "Seagate" 5600 250 , HDD "Verbatim" 7200 100 , SSD "Samsung" 250, SSD "Samsung" 100] == []
             && hugeHDDs' [HDD "Seagate" 5600 1000, HDD "Verbatim" 7200 1500, SSD "Samsung" 250, SSD "Samsung" 500] == [HDD "Seagate" 5600 1000, HDD "Verbatim" 7200 1500]
             && hugeHDDs' [HDD "Seagate" 5600 1000, HDD "Verbatim" 7200 1500                                      ] == [HDD "Seagate" 5600 1000, HDD "Verbatim" 7200 1500]
