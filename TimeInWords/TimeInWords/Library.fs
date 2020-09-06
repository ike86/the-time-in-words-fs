namespace TimeInWords

open Xunit

(*
Given the time in numerals we may convert it into words, as shown below:

At minutes = 00, use o' clock.
For 1<= minutes <=30, use past, and for minutes > 30 use to.
Note the space between the apostrophe and clock in o' clock.

Constraints
1 <= hour <= 12
0 <= minute < 60
*)

module TimeInWords =
    let timeToWords hour minute =
        let (|Past|To|Whole|) minute =
            if minute = 0 then Whole
            else if 1 <= minute && minute <= 30 then Past
            else To

        let toWord minute =
            if minute = 15 then "quarter"
            else if minute = 30 then "half"
            else minute.ToString()

        match minute with
        | Past -> toWord minute + " past " + hour.ToString()
        | To -> toWord (60 - minute) + " to " + (hour + 1).ToString()
        | Whole -> hour.ToString() + " o' clock"


module Tests =
    [<Theory>]
    [<InlineData(5, 00, "5 o' clock")>]
    [<InlineData(5, 10, "10 past 5")>]
    [<InlineData(5, 20, "20 past 5")>]
    [<InlineData(5, 15, "quarter past 5")>]
    [<InlineData(5, 30, "half past 5")>]
    [<InlineData(5, 40, "20 to 6")>]
    [<InlineData(5, 45, "quarter to 6")>]
    [<InlineData(5, 50, "10 to 6")>]
    let ``timeToWords returns expected`` hour minute expected =
        let result = TimeInWords.timeToWords hour minute
        Assert.Equal(expected, result)
