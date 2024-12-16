{-# LANGUAGE OverloadedStrings #-}

module Day15Spec (spec) where

import Data.Text (Text)
import Data.Text qualified as T (unlines)
import Day15 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

example1, example2, example3 :: Text
example1 =
  T.unlines
    [ "##########",
      "#..O..O.O#",
      "#......O.#",
      "#.OO..O.O#",
      "#..O@..O.#",
      "#O#..O...#",
      "#O..O..O.#",
      "#.OO.O.OO#",
      "#....O...#",
      "##########",
      "",
      "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^",
      "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v",
      "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<",
      "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^",
      "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><",
      "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^",
      ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^",
      "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>",
      "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>",
      "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
    ]
example2 =
  T.unlines
    [ "########",
      "#..O.O.#",
      "##@.O..#",
      "#...O..#",
      "#.#.O..#",
      "#...O..#",
      "#......#",
      "########",
      "",
      "<^^>>>vv<v>>v<<"
    ]
example3 =
  T.unlines
    [ "#######",
      "#...#.#",
      "#.....#",
      "#..OO@#",
      "#..O..#",
      "#.....#",
      "#######",
      "",
      "<vv<<^^<<^^"
    ]

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example2 `shouldBe` Right 2028
      part1 example1 `shouldBe` Right 10092
  describe "part 2" $ do
    it "examples" $ do
      part2 example3 `shouldBe` Right 618
      part2 example1 `shouldBe` Right 9021
