namespace TimeInWords

open Xunit

(*
Given the time in numerals we may convert it into words, as shown below:

At minutes = 00, use o' clock.
For 1<= minutes <=30, use past, and for minutes > 30 use to.
Note the space between the apostrophe and clock in o' clock.

Constraints
1 <= hour <= 23
0 <= minute < 60
*)

module TimeInWords =
    let timeToWords hour minute =

        let (|Past|To|Whole|) minute =
            if minute = 0 then Whole
            else if 1 <= minute && minute <= 30 then Past
            else To
            
        let (|Quarter|Half|Other|) minute =
            if minute = 15 then Quarter
            else if minute = 30 then Half
            else Other

        let toWords minute =
            let units =
                if minute = 1 then "minute"
                else "minutes"
            
            match minute with
            | Quarter -> "quarter"
            | Half -> "half"
            | Other -> minute.ToString() + " " + units 

        let toWords hour minute =
            match minute with
            | Past -> toWords minute + " past " + hour.ToString()
            | To -> toWords (60 - minute) + " to " + (hour + 1).ToString()
            | Whole -> hour.ToString() + " o' clock"
        
        if hour <= 12 then toWords hour minute
        else toWords (hour - 12) minute


module Tests =
    [<Theory>]
    [<InlineData(5, 00, "5 o' clock")>]
    [<InlineData(5, 01, "1 minute past 5")>]
    [<InlineData(5, 10, "10 minutes past 5")>]
    [<InlineData(5, 20, "20 minutes past 5")>]
    [<InlineData(5, 15, "quarter past 5")>]
    [<InlineData(5, 30, "half past 5")>]
    [<InlineData(5, 40, "20 minutes to 6")>]
    [<InlineData(5, 45, "quarter to 6")>]
    [<InlineData(5, 50, "10 minutes to 6")>]
    [<InlineData(17, 00, "5 o' clock")>]
    [<InlineData(17, 10, "10 minutes past 5")>]
    [<InlineData(17, 20, "20 minutes past 5")>]
    [<InlineData(17, 15, "quarter past 5")>]
    [<InlineData(17, 30, "half past 5")>]
    [<InlineData(17, 40, "20 minutes to 6")>]
    [<InlineData(17, 45, "quarter to 6")>]
    [<InlineData(17, 50, "10 minutes to 6")>]
    let ``timeToWords returns expected`` hour minute expected =
        let result = TimeInWords.timeToWords hour minute
        Assert.Equal(expected, result)
