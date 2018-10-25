-- module V1unirec (recognize, toRad) where
module V1unirec where

-- $1 Unistroke Recognizer

distance (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2

toRad deg = pi / 180 * deg

-- step 1

resample points n = loop points 0 []
  where i = path'length points / (n - 1)
        loop (p:p':ps) d nps =
          let d' = distance p p'
              (px, py) = p
              (p'x, p'y) = p'
              q = (px + ((i - d) / d') * (p'x - px),
                   py + ((i - d) / d') * (p'y - py)) in
          if d + d' >= i
            then loop (p':q:ps) 0 (q:nps)
            else loop (p':ps) (d + d') nps
        loop _ d nps = nps

path'length points =
  sum $ zipWith distance points (tail points)

-- step 2
centroid points = (x / l, y / l)
    where x = sum [a | (a,_) <- points]
          y = sum [b | (_,b) <- points]
          l = fromIntegral $ length points

indicative'angle points = atan2 (cx - ox) (cy - oy)
          where (cx, cy) = centroid points
                (ox, oy) = head points

rotate'by points w = map rot points
          where (cx, cy) = centroid points
                rot (px, py) =
                  (dx * cos w - dy * sin w + cx,
                   dx * sin w + dy * cos w + cy)
                   where dx = px - cx
                         dy = py - cy
-- step 3

scale'to points size =
  map (\(x, y) -> (x * size / b "width",
                   y * size / b "height")) points
     where b = bouding'box points

bouding'box points attr =
  case attr of
      "width"   -> maximum xs - minimum xs
      "height"  -> maximum ys - minimum ys
    where xs = [x | (x, _) <- points]
          ys = [y | (_, y) <- points]

translate'to points (kx, ky) =
  map (\(x, y) -> (x + kx - cx, y + ky - cy)) points
    where (cx, cy) = centroid points

-- step 4

preproc raw'points =
  let resam'points = resample raw'points 64
      resam'rot = indicative'angle resam'points
      roted'points = rotate'by resam'points (-resam'rot)
      scaled'points = scale'to roted'points 250
      transed'points = translate'to scaled'points (0, 0)
    in transed'points

recognize raw'points raw'templates =
  let
      points = preproc raw'points
      templates = [(t, preproc p) | (t, p) <- raw'templates]
      size = 250
      halfDiagonal = sqrt $ (size ** 2) + (size ** 2)
      theta = toRad 45
      theta'delta = toRad 2
      ds = map (\t -> distance'at'best'angle
                points t (-theta) theta theta'delta)
                templates
                in
      head [(t, b / (0.5 * halfDiagonal)) |
              (t, b) <- zip templates ds,
              b <= minimum ds]

distance'at'best'angle
  points template theta'a theta'b theta'delta =
    loop x1 f1 x2 f2 theta'a theta'b theta'delta
    where phi = (-1 + sqrt 5) / 2
          x1 = phi * theta'a + (1 - phi) * theta'b
          f1 = distance'at'angle points template x1
          x2 = (1 - phi) * theta'a + phi * theta'b
          f2 = distance'at'angle points template x2
          loop x1 f1 x2 f2 the'a the'b the'd =
            if abs (the'b - the'a) > the'd
            then
              if f1 < f2
              then let
                the'b' = x2
                x2' = x1
                f2' = f1
                x1' = (phi * the'a + (1 - phi) * the'b)
                f1' = distance'at'angle points template x1
                in loop x1' f1' x2' f2' the'a the'b' the'd
              else let
                the'a' = x1
                x1' = x2
                f1' = f2
                x2' = (1 - phi) * the'a + phi * the'b
                f2' = distance'at'angle points template x2
                in loop x1' f1' x2' f2' the'a' the'b the'd
            else
              min f1 f2

distance'at'angle points (tml, path) angle =
  path'distance (rotate'by points angle) path

path'distance a b = sum (zipWith distance a b) / l
  where l = fromIntegral $ length a
