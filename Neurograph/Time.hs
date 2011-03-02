module Neurograph.Time where

import Neurograph.Brain
import Neurograph.Node

import Data.List

{-|
   Given a 'Brain' and a 'Node', update the value of
   said node by pulling data from via 'nodesIn'.

   We didn't know how precisely to manage this data transmission
   yet, so we devised a sample scheme that takes the log of some
   attached values... this behavior can be trivially changed,
   as this function has access to the entire brain structure
   upon being called.
-}
pull :: [Node] -> Node -> Node
pull brain (Node pos inL outL exc val) = 
	let
		lg x = (log x) / (log 2.0)
		newVal = round $ sum $ map
			(\n -> (fromIntegral (sign n) :: Float)
			     * (lg (fromIntegral $ (value n)+1 :: Float)))
			$ map (nodeLookup brain) $ inL

	in Node pos inL outL exc (max (val + newVal) 0)

-- | Reduce the value of a node by 1; in our sample scheme, this happens
-- | every timestep.
decay :: Node -> Node
decay (Node pos inL outL exc val) = Node pos inL outL exc (max 0 (val-1))

-- | Update all the brain's nodes and then induce a time decay.
step :: Brain -> Brain
step brain = map (decay . pull brain) brain

-- | Helper function that executes a given number of timesteps.
evolve :: Int -> Brain -> Brain 
evolve t brain = ( iterate step brain ) !! t

-- | Helpful function for diagnostic print-outs
prettyPrint :: Brain -> String
prettyPrint nodes =
	let

		time = value $ nodeLookup nodes (0,0,1,0)
		ordered = zip [1..] (map (nodeLookup nodes) $ sort (map position $ nodes))
	in
		concat $ map (\(m,n) -> (show $ position n) ++ "\t" ++ (show m) ++ "\t" ++ (show $ (sign n) * (value n)) ++ "\t" ++ (show time) ++ "\n") ordered

-- | Endlessly update and brint out the brain
evolution :: Brain -> IO()
evolution brain = do
                         putStrLn $ prettyPrint (step brain)
                         evolution (step brain)
