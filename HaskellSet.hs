-- COSC 290 Fall 2018
-- Lab3 Sept 18, 2018
-- DTULab3
module HaskellSet where

data SET a = E | Add(a, SET a) deriving (Eq, Ord, Read, Show)

--inS :: Eq t => t -> SET t -> Bool
inS a E = False
inS a (Add(first,restofset)) =
    if (a==first)
    then True
    else inS a restofset

--addtoSet :: Eq a => a -> SET a -> SET a
addtoSet a E = Add(a, E)
addtoSet a restofset =
    if inS a restofset
    then restofset
    else Add(a, restofset)


-- union :: Eq a => (SET a, SET a) -> SET a
-- union (E, E) = E
-- union (set1, E) = set1
union (E, set2) = set2
union ((Add(first, restofset)), set2) = union (restofset, (addtoSet first set2))

-- inter :: Eq a => (SET a, SET a) -> SET a
-- inter (E, E) = E
-- inter (set1, E) = E
inter (E, set2) = E
inter ((Add(first, restofset)), set2) =
    if (inS first set2) == False
    then inter (restofset, set2)
    else addtoSet first (inter (restofset, set2))

-- subSet :: Eq t => (SET t, SET t) -> Bool
-- subSet (E, E) = True
subSet (E, set2) = True
subSet (set1, E) = False
subSet ((Add(first, restofset)), set2) =
    if (inS first set2) == True
    then (subSet (restofset, set2))
    else False

-- setEq :: Eq t => (SET t, SET t) -> Bool
setEq (set1, set2) =
    if (subSet(set1, set2))==True
    then (subSet(set2, set1))
    else False

-- setToList :: SET a -> [a]
setToList E = []
setToList (Add(first, restofset)) = first:setToList(restofset)

-- listToSet :: Eq a => [a] -> SET a
listToSet [] = E
listToSet (first:restoflist) = (addtoSet first (listToSet restoflist))

set1 = listToSet("abcd")
set2 = listToSet("cdef")
set3 = listToSet([1,2,3,4])
set4 = listToSet([5,6,7,8])
set5 = listToSet [3,4,4,4]
set6 = listToSet [3,4,7,8]
set7 = listToSet [1,2,3,4,5,6,7,8]
set9 =listToSet []

test1 = set1
test2 = union (set1, set2)
test3 = union (set3, set4)
test4 = inter (set3, set6)
test5 = subSet (set5, set3)
test6 = set7 == union(set3, set4)
test7 = setEq(set7, union(set3, set4))

-- my tests
tes = addtoSet 6 set4
tes1 = union (set9, set9)
tes2 = union (set7, set9)
tes3 = union (set9, set7)
tes4 = inter (set9, set9)
tes5 = inter (set7, set9)
tes6 = inter (set9, set7)
tes8 = subSet (set9, set9)
tes9 = subSet (set7, set9)
tes10 = subSet (set9, set7)
tes11 = setEq (set9, set9)
tes12 = setEq (set7, set9)
tes13 = setEq (set9, set7)

 -- setEq(set1,set2) gives False
