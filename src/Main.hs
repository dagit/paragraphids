module Main where

import Text.XML.Light
import Text.XML.Light.Cursor
import System.Environment ( getArgs )
import System.IO ( openBinaryFile, IOMode(ReadMode), hGetContents )
import Data.List ( unfoldr )
import Data.Char ( toUpper, isSpace )
import Data.ByteString.Lazy.Char8 ( pack )
import Data.Digest.Pure.SHA ( sha1, showDigest )

main = do
  [file] <- getArgs
{-  h      <- openBinaryFile file ReadMode
  text   <- hGetContents h -}
  text <- readFile file
  let Just forest = fromForest (parseXML text)
--  mapM_ print $ toForest $ root $ addPIds forest
  mapM_ (putStr . showContent) $ toForest $ root $ addPIds forest
{-
  mapM_ (\x -> case hasTag "p" x of
               True  -> modifyContent (addAttrToContent p'id) x 
               False -> return ()) $ unfoldr nextDF' forest
-}
p'id :: String -> Attr
p'id name = Attr { attrKey = QName { qName = "id"
                                   , qURI = Just "http://www.w3.org/1999/xhtml"
                                   , qPrefix = Nothing }
                 , attrVal = name }

addPIds :: Cursor -> Cursor 
addPIds c = case nextDF c' of
  Just c'' -> addPIds c''
  Nothing -> c' 
  where c' = addPId c

addPId :: Cursor -> Cursor 
addPId c | hasTag "p" (current c) &&
           (not . null) body = modifyContent (addAttrToContent (p'id name)) c
         | otherwise         = c 
  where
  body = filter (not.isSpace) $ showContent . current $ c
  name = 'p' : (showDigest . sha1 . pack $ body)

addAttrToContent :: Attr -> Content -> Content
addAttrToContent a (Elem e) = Elem (addAttrToElement a e)

addAttrToElement :: Attr -> Element -> Element 
addAttrToElement a e = e { elAttribs = a : elAttribs e }
{-
  mapM_ (\x -> case hasTag "p" x && hasAttrib' "id" x of
               True -> print x
               False -> return ()) $ unfoldr nextDF' forest
-}
nextDF' :: Cursor -> Maybe (Content, Cursor)
nextDF' c = do
  c' <- nextDF c
  return (current c', c')

hasTag :: String -> Content -> Bool
hasTag t (Elem e) = isTag t e
hasTag _ _        = False

isTag :: String -> Element -> Bool
isTag t e = map toUpper (qName (elName e)) == map toUpper t

hasAttrib' :: String -> Content -> Bool
hasAttrib' a (Elem e) = hasAttrib a e
hasAttrib' _ _        = False

hasAttrib :: String -> Element -> Bool
hasAttrib a e = lookup a' attrs' /= Nothing 
  where
  attrs  = elAttribs e
  attrs' = map (\x -> (map toUpper . qName . attrKey $ x, attrVal x)) attrs 
  a'     = map toUpper a
