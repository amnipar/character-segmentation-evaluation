module Main where

import CVSU.Types
import CVSU.PixelImage as P
import CVSU.Integral
import CVSU.ConnectedComponents
import CVSU.QuadForest
import CVSU.OpenCV

import CV.Image
import CV.Morphology
import CV.Filters
import CV.Matrix as M
import CV.Drawing
import CV.ImageOp
import Utils.Rectangle as R

import Data.Marking.WebMark hiding (width,height)

import ReadArgs
import Foreign.Ptr
import Data.List
import Data.Ord
import Control.Monad
import Control.Applicative
import System.IO.Unsafe
import System.Directory
import System.FilePath
import Debug.Trace

fi = fromIntegral

-- | Evaluate the Gaussian with given sigma at the specified point
g :: Float -> (Int,Int) -> Float
g s (x,y) = (1 / (2 * pi * s**2)) * exp(-(((fi x)**2 + (fi y)**2) / (2 * s**2)))

-- | Generate a list of values for a kernel matrix
kernel :: ((Int,Int) -> Float) -> Int -> [Float]
kernel f r = [f (x,y) | y <- [-r..r] , x <- [-r..r] ]

-- | Given a function, mask size, and sigma, generate a kernel matrix
createMask :: (Float -> (Int,Int) -> Float) -> Float -> Int -> Matrix D32
createMask f s r =
  M.fromList (d,d) $ kernel (f s) r
  where
    d = 2 * r + 1

-- | Smooth image by convolving with a Gaussian kernel
gaussianSmooth :: Float -> Int -> Image GrayScale D32 -> Image GrayScale D8
gaussianSmooth s r img = unsafeImageTo8Bit $ convolve2D (createMask g s r) (r,r) img

-- | Generate a CVSU PixelImage from a CV Image
fromCVImage :: Image GrayScale D8 -> IO (PixelImage)
fromCVImage img = do
  saveImage "temp.png" img
  withGenImage img $ \pimg ->
    fromIplImage (castPtr pimg)

-- | Generate a CV GrayScale Image from CVSU PixelImage
toCVImageG :: PixelImage -> IO (Image GrayScale D8)
toCVImageG img = creatingImage $ toBareImage $ toIplImage img
  where
    toBareImage :: IO (Ptr C'IplImage) -> IO (Ptr BareImage)
    toBareImage = liftM castPtr

-- | Generate a CV RGB Image from CVSU PixelImage
toCVImage :: PixelImage -> IO (Image RGB D8)
toCVImage img = creatingImage $ toBareImage $ toIplImage img
  where
    toBareImage :: IO (Ptr C'IplImage) -> IO (Ptr BareImage)
    toBareImage = liftM castPtr

-- | Draw rectangles over an image using the given color
drawRects :: (D32,D32,D32) -> [R.Rectangle Int] -> Image RGB D32 -> Image RGB D32
drawRects c rs img =
  img <## [rectOp c 1 r | r <- rs]

-- | Draw a quad forest into an image and save it to a file
drawForest :: String -> QuadForest -> IO ()
drawForest file forest = do
  fimg <- toCVImage =<< quadForestDrawImage True True forest
  saveImage file fimg

-- | Find the minimum covering rectangle for a collection of rectangles
minCoveringRect :: [R.Rectangle Int] -> R.Rectangle Int
minCoveringRect [] = Rectangle 0 0 0 0
minCoveringRect rects = mkRectCorners (x1,y1) (x2,y2)
  where
    x1 = minimum $ map R.left rects
    y1 = minimum $ map R.top rects
    x2 = maximum $ map R.right rects
    y2 = maximum $ map R.bottom rects

-- | Calculate the area of a rectangle by the coordinates of two corners. The
--   first corner should be the top left corner; if it isn't, the corners
--   correspond to an empty intersection of rectangles and the area is 0.
area (x1,y1) (x2,y2)
  | x2 > x1 && y2 > y1 = (x2-x1)*(y2-y1)
  | otherwise = 0

-- | Convert a connected component to a rectangle
compToRect :: ConnectedComponent -> R.Rectangle Int
compToRect (ConnectedComponent x y w h _) = R.Rectangle x y w h

-- | Convert a segment to a rectangle
segmentToRect :: ForestSegment -> R.Rectangle Int
segmentToRect (ForestSegment _ x y w h _ _) = R.Rectangle x y w h

-- | Find the collection of rectangles that are larger than 256 pixels
--   and have width and height larger than 8, and that intersect the
--   given rectangle by more than 90%.
getIntersectingRects :: [R.Rectangle Int] -> R.Rectangle Int
    -> (R.Rectangle Int, R.Rectangle Int, [R.Rectangle Int])
getIntersectingRects [] r = (r, Rectangle 0 0 0 0, [])
getIntersectingRects rects r@(R.Rectangle rx ry rw rh) =
  (r, minCoveringRect rects', rects')
  where
    rects' = filter (intersecting rx ry rw rh) $ filter bySize rects
    bySize (R.Rectangle _ _ w h) = w > 8 && h > 8 && rArea r > 256
    intersecting rx ry rw rh (R.Rectangle x y w h)
      | area (max rx x, max ry y) (min (rx+rw) (x+w), min (ry+rh) (y+h))
          > (round $ 0.9 * (fromIntegral $ w * h)) = True
      | otherwise = False

-- | Folds two result objects together using the sum of its components.
foldResults :: (Int, Int, Double, Double, Double)
    -> (Int, Int, Double, Double, Double)
    -> (Int, Int, Double, Double, Double)
foldResults (cnum,ccorrect,ccount,cratio,clargest)
            (snum,scorrect,scount,sratio,slargest) =
  (cnum+snum,ccorrect+scorrect,ccount+scount,cratio+sratio,clargest+slargest)

-- | Generates the results from a collection of intersecting rects. Calculates
--   how many rects were available, how many were recovered correctly, how many
--   parts were found, the sum of size ratios of discovered rects to ground truth,
--   and the sum of ratios of largest rect to the covering rect.
getResults :: [(R.Rectangle Int, R.Rectangle Int, [R.Rectangle Int])]
    -> (Int, Int, Double, Double, Double)
getResults rs = (snum, scorrect, scount, sratio, slargest)
  where
    -- number of rects under the max covering rect
    count (_,_,r) = length r
    counts = map count rs
    -- ratio of max cover of discovered rect to correct rect
    ratio (r1,r2,_) = 1 - ((fromIntegral $ abs $ rArea r1 - rArea r2) / (fromIntegral $ rArea r1))
    ratios = map ratio rs
    largestRatios = map largestRatio rs
    -- ratio of largest discovered rect to max covering rect
    largestRatio (_,r2,rs)
      | rArea r2 == 0 = 0
      | otherwise = (fromIntegral $ largest ss) / (fromIntegral $ rArea r2)
      where
        ss = reverse $ sortBy (comparing rArea) rs
        largest [] = 0
        largest ss = rArea $ head ss
    isCorrect r = r > 0.5
    snum = length rs
    scorrect = length $ filter isCorrect ratios
    scount = fromIntegral $ sum counts
    sratio = sum ratios
    slargest = sum largestRatios

-- | Exctracts the covering rect from the tuple
getCoveringRect :: (R.Rectangle Int, R.Rectangle Int, [R.Rectangle Int]) -> R.Rectangle Int
getCoveringRect (_,r,_) = r

-- | Creates a quad forest from a pixel image.
makeForest :: PixelImage -> IO QuadForest
makeForest img = quadForestCreate img 16 4

-- | Examines a file using the thresholding method.
handleFileThresh sourceFile = do
  print sourceFile
  oimg <- readFromFile $ "original/" ++ sourceFile
  -- smooth with a gaussian
  pimg <- fromCVImage $ gaussianSmooth 2.3 7 oimg
  -- create the integral image
  int <- createIntegralImage pimg
  -- threshold using Feng's method with radius 7 and multiplier 3 for the
  -- larger neighborhood; estimate min value using mean - 3 * deviation.
  timg <- integralThresholdFeng True True 3 7 3 int
  -- create connected components
  comp <- createConnectedComponents timg
  cimg <- drawConnectedComponents comp
  saveImage ("threshseg/" ++ sourceFile ++ ".png") =<< toCVImage cimg
  -- read ground truth rectangles from a json file
  (Just es) <- readElementsFromFile $ "original/" ++ sourceFile ++ ".json"
  let
    rects = map fst es
    crects = map compToRect $ components comp
    scs = map (getIntersectingRects crects) rects
    rcs = getResults scs
  print rcs
  return $! rcs

-- | Examines a file using the graph segmentation method.
handleFileGraph sourceFile = do
  print sourceFile
  -- segmentation results are generated using external program, load from file
  simg <- readPNMPixelImage $ "graphseg/" ++ sourceFile ++ ".ppm"
  -- create connected components from the segmentation results
  scomp <- createConnectedComponents simg
  -- read ground truth rectangles from a json file
  (Just es) <- readElementsFromFile $ "original/" ++ sourceFile ++ ".json"
  let
    rects = map fst es
    srects = map compToRect $ components scomp
    scs = map (getIntersectingRects srects) rects
    rcs = getResults scs
  print rcs
  return $! rcs

-- | Examines a file using the quad forest segmentation method
handleFileQuad alpha overlapTree overlapSegment sourceFile = do
  print sourceFile
  pimg <- readPixelImage $ "original/" ++ sourceFile
  -- read ground truth rectangles from a json file
  (Just es) <- readElementsFromFile $ "original/" ++ sourceFile ++ ".json"
  -- create the forest using tree max size 16 and min size 4
  forest <- quadForestCreate pimg 16 4
  withQuadForest forest $ \f -> do
    -- segment using the overlap method
    sf <- quadForestSegmentByOverlap alpha overlapTree overlapSegment f
    -- get the segments
    segments <- quadForestGetSegments sf
    drawForest ("quadseg/" ++ sourceFile ++ ".png") sf
    let
      rects = map fst es
      frects = map segmentToRect segments
      scs = map (getIntersectingRects frects) rects
      rcs = getResults scs
    print rcs
    return $! rcs

-- | Get filenames for all jpg files in the given path
getFiles :: FilePath -> IO [FilePath]
getFiles p =
  map (snd.splitFileName) <$> filter (\f -> takeExtension f == ".jpg") <$> getDirectoryContents p

-- | Examine the list of files using the given method
handleFiles op fs = do
  rs <- mapM op fs
  let
    (num, correct, count, ratio, largest) = foldl' foldResults (0,0,0,0,0) rs
    n = fromIntegral num
  -- calculate and print the averages
  print ((fromIntegral correct / n), count / n, ratio / n, largest / n)

main = do
  mode <- readArgs
  fs <- getFiles "./original/"
  case mode of
         "graph" -> handleFiles handleFileGraph fs
         "thresh" -> handleFiles handleFileThresh fs
         "quad1" -> handleFiles (handleFileQuad 2.5 0.7 0.6) fs
         "quad2" -> handleFiles (handleFileQuad 2.0 0.6 0.5) fs
         "quad3" -> handleFiles (handleFileQuad 2.5 0.7 0.5) fs
         _ -> print "Usage: evaluate [graph|thresh|graph1|graph2|graph3]"

-- these are the exact results from the evaluation runs:
-- graph:  (0.9544546544147023,8.019176987614863,0.8389797534043905,0.640789469998774)
-- thresh: (0.8721534159009189,4.814222932481023,0.7877522022675046,0.4791346709844725)
-- quad1:  (0.9700359568517779,10.905713144226928,0.8149007420516166,0.5529790608658863)
-- quad2:  (0.9388733519776269,7.764682381142629,0.7837850567574401,0.6149343587635131)
-- quad3:  (0.9176987614862165,6.647223332001598,0.7662246061085743,0.6290658888083044)
