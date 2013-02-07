module Main where

import CVSU.Types
import CVSU.PixelImage as CVSU
import CVSU.Integral
import CVSU.ConnectedComponents
import CVSU.QuadForest

import CV.Image
import CV.CVSU as CV
import CV.CVSU.Rectangle
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

-- | Draw a quad forest into an image and save it to a file
drawForest :: String -> QuadForest -> IO ()
drawForest file forest = do
  saveImage file =<< expectByteRGB =<< fromPixelImage =<<
    quadForestDrawImage True True forest

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
getResults :: [(R.Rectangle Int, [R.Rectangle Int])]
    -> (Int, Int, Double, Double, Double)
getResults rs = (snum, scorrect, scount, sratio, slargest)
  where
    -- number of rects under the max covering rect
    count (_,r) = length r
    counts = map count rs
    -- ratio of max cover of discovered rect to correct rect
    ratio (r1,rs) = 1 - ((fromIntegral $ abs $ rArea r1 - rArea r2) / (fromIntegral $ rArea r1))
      where r2 = minCoveringRect rs
    ratios = map ratio rs
    largestRatios = map largestRatio rs
    -- ratio of largest discovered rect to max covering rect
    largestRatio (_,rs)
      | rArea r2 == 0 = 0
      | otherwise = (fromIntegral $ largest ss) / (fromIntegral $ rArea r2)
      where
        r2 = minCoveringRect rs
        ss = reverse $ sortBy (comparing rArea) rs
        largest [] = 0
        largest ss = rArea $ head ss
    isCorrect r = r > 0.5
    snum = length rs
    scorrect = length $ filter isCorrect ratios
    scount = fromIntegral $ sum counts
    sratio = sum ratios
    slargest = sum largestRatios

-- | Creates a quad forest from a pixel image.
makeForest :: PixelImage -> IO QuadForest
makeForest img = quadForestCreate 16 4 img

-- | For excluding too small rectangles from the result.
bySize (R.Rectangle _ _ w h) = w > 8 && h > 8 && w*h > 256

-- | Examines a file using the thresholding method.
handleFileThresh sourceFile = do
  print sourceFile
  oimg <- readFromFile $ "original/" ++ sourceFile
  -- smooth with a gaussian
  pimg <- toPixelImage $ gaussianSmooth 2.3 7 oimg
  -- create the integral image
  int <- createIntegralImage pimg
  -- threshold using Feng's method with radius 7 and multiplier 3 for the
  -- larger neighborhood; estimate min value using mean - 3 * deviation.
  timg <- integralThresholdFeng True True 3 7 3 int
  -- create connected components
  comp <- createConnectedComponents timg
  cimg <- drawConnectedComponents comp
  saveImage ("threshseg/" ++ sourceFile ++ ".png")
    =<< expectByteRGB =<< fromPixelImage cimg
  -- read ground truth rectangles from a json file
  (Just es) <- readElementsFromFile $ "original/" ++ sourceFile ++ ".json"
  let
    rects = map fst es
    crects = map compToRect $ connectedComponents comp
    scs = map (getIntersectingRects 0.9 crects) $ filter bySize rects
    rcs = getResults scs
  print rcs
  return $! rcs

-- | Examines a file using the graph segmentation method.
handleFileGraph sourceFile = do
  print sourceFile
  -- segmentation results are generated using external program, load from file
  simg <- CVSU.readPixelImage $ "graphseg/" ++ sourceFile ++ ".ppm"
  -- create connected components from the segmentation results
  scomp <- createConnectedComponents simg
  -- read ground truth rectangles from a json file
  (Just es) <- readElementsFromFile $ "original/" ++ sourceFile ++ ".json"
  let
    rects = map fst es
    srects = map compToRect $ connectedComponents scomp
    scs = map (getIntersectingRects 0.9 srects) $ filter bySize rects
    rcs = getResults scs
  print rcs
  return $! rcs

-- | Examines a file using the quad forest segmentation method
handleFileQuad alpha overlapTree overlapSegment sourceFile = do
  print sourceFile
  pimg <- CV.readPixelImage $ "original/" ++ sourceFile
  -- read ground truth rectangles from a json file
  (Just es) <- readElementsFromFile $ "original/" ++ sourceFile ++ ".json"
  -- create the forest using tree max size 16 and min size 4
  forest <- quadForestCreate 16 4 pimg
  withQuadForest forest $ \f -> do
    -- segment using the overlap method
    sf <- quadForestSegmentByOverlap alpha overlapTree overlapSegment f
    -- get the segments
    segments <- quadForestGetSegments sf
    drawForest ("quadseg/" ++ sourceFile ++ ".png") sf
    let
      rects = map fst es
      frects = map segToRect segments
      scs = map (getIntersectingRects 0.9 frects) $ filter bySize rects
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
