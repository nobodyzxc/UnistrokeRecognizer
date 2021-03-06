import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import V1unirec
import Templates

main :: IO ()
main = startGUI defaultConfig setup

canvasWidth = 420
canvasHeight = 400

coorTit = " Coordinates: "

rtext = mkReadAttr $ \el ->
        callFunction $ ffi "$(%1).text()" el

toDouble (x, y) = (fromIntegral x, fromIntegral y)

setup :: Window -> UI ()
setup window = do
    return window # set title "Canvas - Examples"

    points   <- UI.span  # set text "[]"
    clicking <- UI.span  # set text "no"
    result   <- UI.span  # set text "No Result"
    url      <- UI.loadFile "image/png" "img/lib0.png"
    libimg   <- UI.img   # set UI.src url
    curlib   <- UI.span  # set text "img/lib0.png"
    ptnumlab <- UI.span  # set text "sample point number: "
    ptnumber <- UI.input # set value "64"

    out  <- UI.span
        # set text coorTit
        # set (attr "tabindex") "1" -- allow key presses

    canvas <- UI.canvas
        # set UI.width  canvasWidth
        # set UI.height canvasHeight
        # set style [("border", "solid black 1px"),
                     ("background", "#eee")]

    change'lib <- UI.button #+ [string "change library"]

    getBody window #+
        [ element result
        , UI.br
        , element canvas
        , element libimg
        , UI.br
        , element change'lib
        , UI.br
        , element ptnumlab
        , element ptnumber
        , UI.br
        , element out
        , UI.br
        , element points]

    on UI.click change'lib $ const $ do
        libname <- get rtext curlib
        let refresh = if libname == "img/lib0.png"
                        then "img/lib1.png"
                        else "img/lib0.png"
          in do
          url <- UI.loadFile "image/png" refresh
          element libimg # set UI.src url
          element curlib # set text refresh

    on UI.mousedown canvas $ \xy -> do
        result <- UI.span # set text "No Result"
        element clicking # set text ("yes")
        canvas # UI.closePath
        canvas # UI.clearCanvas
        canvas # UI.beginPath
        element points # set text ("[" ++ show xy ++ "]")

    on UI.mouseup canvas $ \xy -> do
        pointstr <- get rtext points
        libname <- get rtext curlib
        ptnumberstr <- get value ptnumber
        element clicking # set text ("no")
        let pts = read pointstr
            sam'pt'num = fromIntegral $ read ptnumberstr 
            lib = if libname == "img/lib0.png"
                    then lib0 else lib1
            ((tml, _), _) = recognize sam'pt'num (reverse pts) lib
            in element result # set text tml

    on UI.mousemove canvas $ \xy -> do
      mouseDown <- get rtext clicking
      pointstr <- get rtext points
      if mouseDown == "yes"
        then let pts = read pointstr in do
          canvas # UI.lineTo (toDouble xy)
          canvas # UI.stroke
          element points # set text (show (xy:pts))
        else do
          element out
      element out # set text (coorTit ++ show xy)

