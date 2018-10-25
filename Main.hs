import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import V1unirec
import Templates

getp x = p where (_, p) = lib0 !! x

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

    points <- UI.span # set text "[]"
    clicking  <- UI.span # set text "no"
    result <- UI.span # set text "No Result"
    url <- UI.loadFile "image/png" "lib0.png"
    libimg    <- UI.img # set UI.src url
    curlib <- UI.span # set text "lib0.png"

    out  <- UI.span
        # set text coorTit
        # set (attr "tabindex") "1" -- allow key presses

    canvas <- UI.canvas
        # set UI.width  canvasWidth
        # set UI.height canvasHeight
        # set style [("border", "solid black 1px"),
                     ("background", "#eee")]

    draw <- UI.button #+ [string "draw"]
    change'lib <- UI.button #+ [string "change lib"]

    getBody window #+
        [ element canvas
        , element libimg
        , UI.br
        , element draw
        , element change'lib
        , UI.br
        -- , element clicking
        , UI.br
        , element out
        , UI.br
        , element result
        , UI.br
        , element points]

    -- on UI.click draw $ const $ do
    --   canvas # UI.beginPath
    --   let p = getp 11
    --       l = length p in
    --     forM_ (take (div l 2) p) (flip UI.lineTo canvas)
    --   canvas # UI.closePath
    --   canvas # UI.stroke

    on UI.click change'lib $ const $ do
        libname <- get rtext curlib
        let refresh = if libname == "lib0.png"
                        then "lib1.png"
                        else "lib0.png"
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
        element clicking # set text ("no")
        let pts = read pointstr
            lib = if libname == "lib0.png"
                    then lib0 else lib1
            ((tml, _), _) = recognize (reverse pts) lib
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

