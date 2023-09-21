{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Levels where

import GameCore
import IdentityList
import Types

boundaries :: [Rct]
boundaries =
  [ Rct 0 0 800 30
  , Rct 0 0 30 1000
  , Rct 770 0 30 1000
  ]

map1 :: [Rct]
map1 =
  boundaries
    ++ [Rct 200 100 50 50]

map2 :: [Rct]
map2 =
  boundaries
    ++ [ Rct 100 100 300 50
       , Rct 450 200 300 50
       , Rct 200 400 250 50
       , Rct 500 320 50 50
       ]

map3 :: [Rct]
map3 =
  boundaries
    ++ [ Rct 100 100 50 25
       , Rct 100 300 50 25
       , -- , Rct 100 500  50  10
         Rct 200 200 50 25
       , Rct 200 400 50 25
       , Rct 300 400 50 25
       , Rct 450 200 100 300
       -- , Rct 200 600  50  10
       ]

map4 :: [Rct]
map4 =
  boundaries
    ++ [ Rct 200 30 50 70
       ]

platformWithAnEnemy :: Rct -> Double -> Double -> Double -> [Object]
platformWithAnEnemy platformRct xOffset enemyWidth enemyHeight =
  let platform = staticObject $ MovementState platformRct (Vct 0 0) (Vct 0 0)
      enemy =
        enemyObject
          ( MovementState
              ( Rct
                  (pX platformRct + xOffset)
                  (pY platformRct + h platformRct + 1)
                  enemyWidth
                  enemyHeight
              )
              (Vct 0 0)
              (Vct 0 0)
          , blockingDataDefault
          , defaultControlIntention
          , Just $ pX platformRct
          , Just $ pX platformRct + w platformRct - enemyWidth
          )
   in [platform, enemy]

level1 :: IL Object
level1 =
  listToIL
    ( [ playerObject
          ( MovementState (Rct 100 50 20 50) (Vct 0 0) (Vct 0 0)
          , blockingDataDefault
          , defaultControlIntention
          , letterSpaceScheme
          )
      ]
        ++ platformWithAnEnemy (Rct 300 200 340 20) 0 50 50
        ++ platformWithAnEnemy (Rct 500 300 240 20) 20 50 50
        ++ map (\rct -> staticObject $ MovementState rct (Vct 0 0) (Vct 0 0)) map1
    )

level2 :: IL Object
level2 =
  listToIL
    ( [ enemyObject
          ( MovementState (Rct 100 150 20 50) (Vct 0 0) (Vct 0 0)
          , blockingDataDefault
          , defaultControlIntention
          , Just 100
          , Just 380
          )
      , enemyObject
          ( MovementState (Rct 600 250 20 50) (Vct 0 0) (Vct 0 0)
          , blockingDataDefault
          , defaultControlIntention
          , Just 450
          , Just 730
          )
      , enemyObject
          ( MovementState (Rct 300 500 50 50) (Vct 0 0) (Vct 0 0)
          , blockingDataDefault
          , defaultControlIntention
          , Just 200
          , Just 400
          )
      , playerObject
          ( MovementState (Rct 100 50 20 50) (Vct 0 0) (Vct 0 0)
          , blockingDataDefault
          , defaultControlIntention
          , letterSpaceScheme
          )
      ]
        ++ map (\rct -> staticObject $ MovementState rct (Vct 0 0) (Vct 0 0)) map2
    )

level3 :: IL Object
level3 =
  listToIL
    ( [ enemyObject
          ( MovementState (Rct 500 40 20 50) (Vct 0 0) (Vct 0 0)
          , blockingDataDefault
          , defaultControlIntention
          , Nothing
          , Nothing
          )
      , playerObject
          ( MovementState (Rct 100 40 20 50) (Vct 0 0) (Vct 0 0)
          , blockingDataDefault
          , defaultControlIntention
          , letterSpaceScheme
          )
      ]
        ++ platformWithAnEnemy (Rct 300 200 150 20) 10 40 30
        ++ platformWithAnEnemy (Rct 640 250 200 20) 10 50 50
        ++ map (\rct -> staticObject $ MovementState rct (Vct 0 0) (Vct 0 0)) map3
    )

level4 :: IL Object
level4 =
  listToIL
    ( [ enemyObject
          ( MovementState (Rct 500 40 20 50) (Vct 0 0) (Vct 0 0)
          , blockingDataDefault
          , defaultControlIntention
          , Nothing
          , Nothing
          )
      , playerObject
          ( MovementState (Rct 100 40 20 50) (Vct 0 0) (Vct 0 0)
          , blockingDataDefault
          , defaultControlIntention
          , letterSpaceScheme
          )
      ]
        ++ platformWithAnEnemy (Rct 280 100 600 20) 10 50 50
        ++ platformWithAnEnemy (Rct 550 200 200 20) 15 60 40
        ++ platformWithAnEnemy (Rct 450 330 200 20) 40 40 40
        ++ platformWithAnEnemy (Rct 200 300 200 20) 80 60 40
        ++ platformWithAnEnemy (Rct 150 400 100 20) 50 40 40
        ++ map (\rct -> staticObject $ MovementState rct (Vct 0 0) (Vct 0 0)) map4
    )

multiLevelSpec =
  multiLevelGame'
    [
      ( level1
      ,
        [ "Level 1."
        , "You are the Blue Rectangle."
        , "A and D to move, Space to jump."
        , "Eliminate the Red Rectangles!"
        ]
      )
    , (level2, ["Level 2.", "Eliminate the Red Rectangles!"])
    , (level3, ["Level 3.", "Eliminate more of the Red Rectangles!"])
    , (level4, ["Level 4.", "Eliminate even more of the Red Rectangles!"])
    ]
