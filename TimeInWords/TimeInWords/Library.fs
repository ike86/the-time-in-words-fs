namespace TimeInWords

open Xunit

(*
Given the time in numerals we may convert it into words, as shown below:

At minutes = 00, use o' clock.
For 1<= minutes <=30, use past, and for minutes > 30 use to.
Note the space between the apostrophe and clock in o' clock.
*)

module TimeInWords =
    let timeToWords hour minute =
        let (|Past|To|Whole|) minute =
            if minute = 0 then Whole
            else if 1 <= minute && minute<= 30 then Past
            else To

        match minute with
        | Past -> minute.ToString() + " past " + hour.ToString()
        | To -> failwith "Not implemented"
        | Whole -> hour.ToString() + " o' clock"
        

module Tests =
    [<Theory>]
    [<InlineData (5, 00, "5 o' clock")>]
    [<InlineData (5, 10, "10 past 5")>]
    [<InlineData (5, 20, "20 past 5")>]
    let ``timeToWords returns expected`` hour minute expected =
        let result = TimeInWords.timeToWords hour minute
        Assert.Equal(expected, result)
