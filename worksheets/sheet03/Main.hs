{-# LANGUAGE OverloadedStrings #-}

import Graphics.Svg

myline = path_ [D_ <<- mA 0 100 <> lA 30 40]
svg = doctype <> with (svg11_ myline) [Width_ <<- "100", Height_ <<- "100"]
main = renderToFile "path/to/file" svg