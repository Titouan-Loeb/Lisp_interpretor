--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- MyMaybe
--

module MyMaybe where

data MyMaybe a = MyNothing String | MyJust a

myIsNothing :: MyMaybe a -> Bool
myIsNothing (MyNothing str) = True
myIsNothing (MyJust a) = False

myIsJust :: MyMaybe a -> Bool
myIsJust (MyJust a) = True
myIsJust (MyNothing str) = False

myFromNothing :: MyMaybe a -> String
myFromNothing (MyNothing str) = str
myFromNothing a = error "Error MyMaybe: myFromNothing: cannot handle MyJust"

myFromJust :: MyMaybe a -> a
myFromJust (MyJust a) = a
myFromJust a = error "Error MyMaybe: myFromJust: cannot handle MyNothing"