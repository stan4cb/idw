module Imgur where

import qualified Data.String.Utils as SUtil (replace, split)
import qualified Data.List as DL (isInfixOf)

clData = ["\"" , "content=" , "?fb"]
grupId = "twitter:card\"content=\"gallery\""

imgRefBgn = "og:image\""
imgRefEnd = "/>"

fileName url = last (SUtil.split "/" url)


isGallery html = DL.isInfixOf grupId (clearWP html)


findImage html = clear (urlE ((urlB html) !! 1) !! 0)


findImages html = init $ findImages' (urlB html) []


findImages' [] sites     = sites
findImages' (x:xs) sites = findImages' xs $ (clear ((urlE x) !! 0)) : sites


clear = clear' clData

clear' []     str  = str
clear' (x:xs) str  = clear' xs (SUtil.replace x "" str)


urlB    = SUtil.split imgRefBgn
urlE    = SUtil.split imgRefEnd
clearWP = SUtil.replace " " ""
