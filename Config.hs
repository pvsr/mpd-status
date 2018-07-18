module Config(buttonToOp, volStep) where

import Click
import Operation

volStep :: Int
volStep = 5

buttonToOp :: Button -> Maybe Operation
buttonToOp LeftClick = Just Toggle
buttonToOp MiddleClick = Just AllRandom
buttonToOp RightClick = Just Stop
buttonToOp ScrollUp = Just $ VolumeUp volStep
buttonToOp ScrollDown = Just $ VolumeDown volStep
buttonToOp Back = Just Previous
buttonToOp Forward = Just Next
buttonToOp _ = Nothing
