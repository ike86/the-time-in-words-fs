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
        hour.ToString() + " o' clock"

module Tests =
    [<Fact>]
    let ``5 o' clock`` () =
        let result = TimeInWords.timeToWords 5 00
        Assert.Equal("5 o' clock", result)
