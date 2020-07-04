module Patterns.Test

open NUnit.Framework
open FsUnitTyped


[<Test>]
let ``An empty string is converted to an empty list`` () =
  toCells "" |> shouldEqual []

[<Test>]
let ``An empty list is converted to an empty string`` () =
  fromCells [] |> shouldEqual ""

[<Test>]
let ``Conversions to black, white, and unknown occur correctly`` () =
  toCells "B" |> fromCells |> shouldEqual "b"
  toCells "W" |> fromCells |> shouldEqual "w"
  toCells "/" |> fromCells |> shouldEqual "."
  toCells "!" |> fromCells |> shouldEqual "."

[<Test>]
let ``Complex sequences are round-tripped correctly`` () =
  toCells "Black, white, and Unknown!" |> fromCells |> shouldEqual "b......w...............w.."

  toCells "`1234567890[]',.pyfgcrl/=\\aoeuidhtns-;qjkxbmwvz~!@#$%^&*(){}\"<>PYFGCRL?+|AOEUIDHTNS_:QJKXBMWV Z"
  |> fromCells
  |> shouldEqual "..........................................b.w............................................b.w..."

  toCells "bw WB" |> fromCells |> shouldEqual "bw.wb"

  toCells "WB bw" |> fromCells |> shouldEqual "wb.bw"

// UNCOMMENT the tests below, once you have created your
// pattern data-type.
// If the code doesn't compile, you've probably messed up
// your data-type.

(* // BEGINNING OF COMMENTED SECTION

// to convert from a list of cells to a textual form
let fromMatch = Option.map fromCells
// match against the pattern, then give back the result as text
let doMatch s p = toCells s |> patternMatch p |> fromMatch

// The following tests are for the 'patternMatch' function.

module PatternMatch =
  [<Test>]
  let ``Positive patterns work`` () =
    doMatch "b" BlackP |> shouldEqual (Some "b")
    doMatch "" BlackP |> shouldEqual None
    doMatch "bb" BlackP |> shouldEqual (Some "b")
    doMatch "w" BlackP |> shouldEqual None
    doMatch "x" BlackP |> shouldEqual None

  [<Test>]
  let ``Negative patterns work`` () =
    doMatch "w" WhiteP |> shouldEqual (Some "w")
    doMatch "" WhiteP |> shouldEqual None
    doMatch "ww" WhiteP |> shouldEqual (Some "w")
    doMatch "b" WhiteP |> shouldEqual None
    doMatch "!" WhiteP |> shouldEqual None

  [<Test>]
  let ``Unknown patterns work`` () =
    doMatch "@" UnknownP |> shouldEqual (Some ".")
    doMatch "" UnknownP |> shouldEqual None
    doMatch " ~" UnknownP |> shouldEqual (Some ".")
    doMatch "b" UnknownP |> shouldEqual None
    doMatch "W" UnknownP |> shouldEqual None

  [<Test>]
  let ``A ZeroOrMore pattern captures zero or more of the pattern`` () =
    doMatch "" (ZeroOrMore BlackP) |> shouldEqual (Some "")
    doMatch "b" (ZeroOrMore BlackP) |> shouldEqual (Some "b")
    doMatch "w" (ZeroOrMore BlackP) |> shouldEqual (Some "")
    doMatch "wbb" (ZeroOrMore BlackP) |> shouldEqual (Some "")
    doMatch "bbbbbbwb" (ZeroOrMore BlackP) |> shouldEqual (Some "bbbbbb")
  
  [<Test>]
  let ``A OneOrMore pattern must capture at least one of the pattern`` () =
    doMatch "" (OneOrMore BlackP) |> shouldEqual None
    doMatch "b" (OneOrMore BlackP) |> shouldEqual (Some "b")
    doMatch "w" (OneOrMore BlackP) |> shouldEqual None
    doMatch "wbb" (OneOrMore BlackP) |> shouldEqual None
    doMatch "bbbbbbwb" (OneOrMore BlackP) |> shouldEqual (Some "bbbbbb")
  
  [<Test>]
  let ``An Exactly pattern must capture the exact number of the pattern`` () =
    doMatch "" (Exactly (0, UnknownP)) |> shouldEqual (Some "")
    doMatch "p" (Exactly (0, WhiteP)) |> shouldEqual (Some "")
    doMatch "p" (Exactly (0, UnknownP)) |> shouldEqual (Some "")
    doMatch "xxbwwwb" (Exactly (0, UnknownP)) |> shouldEqual (Some "")
    doMatch "xxbwwwb" (Exactly (1, UnknownP)) |> shouldEqual (Some ".")
    doMatch "xxbwwwb" (Exactly (2, UnknownP)) |> shouldEqual (Some "..")
    doMatch "xxbwwwb" (Exactly (3, UnknownP)) |> shouldEqual None
    doMatch "xx" (Exactly (2, UnknownP)) |> shouldEqual (Some "..")
    doMatch "xx" (Exactly (3, UnknownP)) |> shouldEqual None

  [<Test>]
  let ``A FewerThan pattern must match fewer items than its number`` () =
    doMatch "" (FewerThan (5, UnknownP)) |> shouldEqual (Some "")
    doMatch "VBww" (FewerThan (5, WhiteP)) |> shouldEqual (Some "")
    doMatch "wwwww" (FewerThan (5, WhiteP)) |> shouldEqual (Some "wwww")
    doMatch "BB" (FewerThan (3, BlackP)) |> shouldEqual (Some "bb")
    doMatch "B" (FewerThan (3, BlackP)) |> shouldEqual (Some "b")

  [<Test>]
  let ``You cannot match fewer than 0 things`` () =
    doMatch "." (FewerThan (0, UnknownP)) |> shouldEqual None

  [<Test>]
  let ``A Sequence pattern matches a series of patterns`` () =
    doMatch "wbwbb" (Sequence []) |> shouldEqual (Some "")
    doMatch "wbwbb" (Sequence [WhiteP]) |> shouldEqual (Some "w")
    doMatch "wbwbb" (Sequence [BlackP]) |> shouldEqual None
    doMatch "wbwbb" (Sequence [WhiteP; BlackP]) |> shouldEqual (Some "wb")
    doMatch "wbwbb" (Sequence [WhiteP; WhiteP]) |> shouldEqual None
    doMatch "wbwbb" (Sequence [WhiteP; BlackP; WhiteP; BlackP; BlackP]) |> shouldEqual (Some "wbwbb")
    doMatch "wbwbb" (Sequence [WhiteP; BlackP; WhiteP; BlackP; WhiteP]) |> shouldEqual None
    doMatch "wbwbb" (Sequence [WhiteP; BlackP; WhiteP; WhiteP; BlackP]) |> shouldEqual None

  [<Test>]
  let ``An Either pattern matches only one of the patterns`` () =
    doMatch "" (Either (BlackP, WhiteP)) |> shouldEqual None
    doMatch "." (Either (BlackP, WhiteP)) |> shouldEqual None
    doMatch "b" (Either (BlackP, WhiteP)) |> shouldEqual (Some "b")
    doMatch "W" (Either (BlackP, WhiteP)) |> shouldEqual (Some "w")
    doMatch "bw" (Either (BlackP, WhiteP)) |> shouldEqual (Some "b")
    doMatch "wb" (Either (BlackP, WhiteP)) |> shouldEqual (Some "w")
    doMatch ".bw" (Either (BlackP, WhiteP)) |> shouldEqual None

  [<Test>]
  let ``An Anything pattern matches any single cell`` () =
    doMatch "#" Anything |> shouldEqual (Some ".")
    doMatch "b" Anything |> shouldEqual (Some "b")
    doMatch "w" Anything |> shouldEqual (Some "w")
    doMatch "_%" Anything |> shouldEqual (Some ".")
    doMatch "w*" Anything |> shouldEqual (Some "w")
    doMatch "bw" Anything |> shouldEqual (Some "b")
    doMatch "wb" Anything |> shouldEqual (Some "w")

  [<Test>]
  let ``If there's nothing to match, the Anything pattern can't match`` () =
    doMatch "" Anything |> shouldEqual None

  [<Test>]
  let ``EndOfCells matches the end of an input`` () =
    doMatch "" EndOfCells |> shouldEqual (Some "")
    doMatch "!" EndOfCells |> shouldEqual None

  [<Test>]
  let ``We can match between 1 and n using a combination of patterns`` () =
    let between a b pattern = Sequence [Exactly (a, pattern); FewerThan (b-a+1, pattern)]
    doMatch "bbbbbb" (between 3 5 BlackP) |> shouldEqual (Some "bbbbb")
    doMatch "bbbbb" (between 3 5 BlackP) |> shouldEqual (Some "bbbbb")
    doMatch "bbbb" (between 3 5 BlackP) |> shouldEqual (Some "bbbb")
    doMatch "bbb" (between 3 5 BlackP) |> shouldEqual (Some "bbb")
    doMatch "bb" (between 3 5 BlackP) |> shouldEqual None
    doMatch "b" (between 3 5 BlackP) |> shouldEqual None
    doMatch "" (between 3 5 BlackP) |> shouldEqual None

  [<Test>]
  let ``Either Patterns must capture the longest possible sequence`` () =
    doMatch "wwbbb" (Either (OneOrMore WhiteP, OneOrMore Anything)) |> shouldEqual (Some "wwbbb")
    doMatch "wwbbb" (Either (OneOrMore Anything, OneOrMore WhiteP)) |> shouldEqual (Some "wwbbb")
    doMatch "wbw@."
      (Either (
        Exactly (2, Sequence [WhiteP; Either (BlackP, UnknownP)]),
        ZeroOrMore (Either (BlackP, WhiteP))
      ))
    |> shouldEqual (Some "wbw.")

  [<Test>]
  let ``More interesting combinations 1`` () =
    // Patterns take the LONGEST sequence that they can.
    // Therefore, (OneOrMore Anything) will consume EVERYTHING.
    // If I ask for TWO of them, then there's nothing
    // for the second (OneOrMore Anything) to match.
    // Since two such patterns are required, the match fails.
    doMatch "wab" (Exactly (2, OneOrMore Anything)) |> shouldEqual None

  [<Test>]
  let ``More interesting combinations 2`` () =
    // similar reasoning to the above.
    doMatch "bbb" (Sequence [ZeroOrMore Anything; OneOrMore Anything])
    |> shouldEqual None
    doMatch "bbbbb" (Sequence [OneOrMore BlackP; Anything]) |> shouldEqual None


  [<Test>]
  let ``More interesting combinations 3`` () =
    doMatch "bbbw." (ZeroOrMore (FewerThan (2, Anything))) |> shouldEqual (Some "bbbw.")

  [<Test>]
  let ``More interesting combinations 4`` () =
    doMatch "..bbb...b"
      (Exactly (3, Either
          (Sequence [ZeroOrMore BlackP; UnknownP; UnknownP],
           FewerThan (3, UnknownP)
          )
      ))
    |> shouldEqual (Some "..bbb...")

// The following tests are for the 'find' function.

let doFind s p = toCells s |> find p |> Option.map (fun (a,b) -> fromCells a, b)

module Find =
  [<Test>]
  let ``If there are no matches anywhere, "find" won't find a match`` () =
    doFind "bwb" UnknownP |> shouldEqual None
    doFind "" BlackP |> shouldEqual None

  [<Test>]
  let ``"find" will find the first match and its offset`` () =
    doFind "b" BlackP |> shouldEqual (Some ("b", 0))
    doFind "ww" WhiteP |> shouldEqual (Some ("w", 0))
    doFind "w..bbbwb" (ZeroOrMore BlackP) |> shouldEqual (Some ("", 0))
    doFind "w..bbbwb" (OneOrMore BlackP) |> shouldEqual (Some ("bbb", 3))
    doFind "xwx!!" (Exactly (3, UnknownP)) |> shouldEqual (Some ("...", 2))
    doFind "wBxBB" (FewerThan (3, BlackP)) |> shouldEqual (Some ("", 0))
    doFind "@bwXwbb" (Sequence [WhiteP; BlackP]) |> shouldEqual (Some ("wb", 4))
    doFind "wBxBB" (Sequence [FewerThan (3, BlackP); EndOfCells]) |> shouldEqual (Some ("bb", 3))

module Map =
  let makeUnknown =
    fromCells >> String.map (fun _ -> '.') >> toCells
  let makeWhite =
    fromCells >> String.map (fun _ -> 'W') >> toCells
  let delete _ = []
  let double x = x @ x

  let doMap s p f = map f p (toCells s) |> fromCells

  [<Test>]
  let ``If there isn't a match, the cells are unchanged`` () =
    doMap "bwb" UnknownP makeUnknown |> shouldEqual "bwb"
    doMap "" BlackP makeUnknown |> shouldEqual ""

  [<Test>]
  let ``We can apply different functions`` () =
    doMap "bw." BlackP makeUnknown |> shouldEqual ".w."
    doMap "bw." BlackP makeWhite |> shouldEqual "ww."
    doMap "bw." BlackP delete |> shouldEqual "w."
    doMap "bw." BlackP double |> shouldEqual "bbw."

  [<Test>]
  let ``The mapping is applied to all matches`` () =
    let pattern =
      Either (
        Sequence [BlackP; Anything],
        Sequence [Anything; BlackP]
      )
    doMap "bw.w.wb" pattern makeUnknown |> shouldEqual "...w..."
    doMap "bw.w.wb" pattern makeWhite |> shouldEqual "ww.w.ww"
    doMap "bw.w.wb" pattern delete |> shouldEqual ".w."
    doMap "bw.w.wb" pattern double |> shouldEqual "bwbw.w.wbwb"

*) // END OF COMMENTED SECTION