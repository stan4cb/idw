{-# LANGUAGE OverloadedStrings #-}

import qualified Network.Curl.Download as Curl (openURI, openURIString)
import qualified System.Environment as SEnv (getArgs)
import qualified Data.ByteString as BS (writeFile)
import qualified Data.List as DL (isInfixOf)

import qualified Imgur

testIUrl = "imgur.com/nwwGrdD"
testIGUrl = "imgur.com/a/QSpYE"

-- no use for now
hosts = [
  "instagram",
  "imgur"]

-- used to detect if it's a hotlink
formats =
  [".jpg",".jpeg",
   ".gif",".webm",
   ".mp4","?fb"]


-- folder name
main = do
  args <- SEnv.getArgs
  if (length args) > 0
    then
      crawler (args !! 0)
    else do
      putStrLn "Enter url : "
      url <- getLine
      crawler url


crawler url =
  if isHotlink url
  then
    downloadNSaveImage url
  else do
    rData <- Curl.openURIString url
    case rData of
      Left  err  -> putStrLn "Error downloading url"
      Right html -> do
        if Imgur.isGallery html
        then do
          putStrLn "Album download :"
          mapM downloadNSaveImage (Imgur.findImages html)
          putStrLn "-------\nAlbum download complete"
        else
          downloadNSaveImage $ Imgur.findImage html


-- there is no control if its a image
downloadNSaveImage url = do
  if not $ isHotlink url
  then putStrLn ("Found url is not valid : " ++ url)
  else do
    putStr ("Starting to download -> " ++ url)
    imgOr <- Curl.openURI url
    case imgOr of
      Left  err -> putStrLn "Error downloading image."
      Right img -> do
        putStrLn " : done."
        BS.writeFile (Imgur.fileName url) img


isHotlink = isHotlink' formats
-- needs to get clean url
isHotlink' []     url = False
isHotlink' (f:fs) url =
  if DL.isInfixOf f url
  then True
  else isHotlink' fs url
