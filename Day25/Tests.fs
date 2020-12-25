module Tests

open Xunit

let round (value: uint64) (subjectNumber: uint64) =
    (value * subjectNumber) % 20201227uL

let encrypt subjectNumber loops =
    let mutable value =  1uL
    for i in 1 ..loops do
        value <- round value subjectNumber
    value

let findLoopSize publicKey =
    let mutable rounds = 0;
    let mutable value = 1uL
    while value <> publicKey do
        value <- round value 7uL
        rounds <- rounds + 1
    rounds

let doorPub () = 8987316uL
let doorLoop () = findLoopSize (doorPub())

let keyPub () = 14681524uL
let keyLoop () = findLoopSize (keyPub())

[<Fact>]
let ``Can reverse loop size`` () =
    Assert.Equal(5764801uL, encrypt 7uL 8)
    Assert.Equal(8, findLoopSize 5764801uL)
    
[<Fact>]
let ``Can find loop sizes`` () =
    Assert.Equal(4208732, keyLoop())
    Assert.Equal(2541700, doorLoop())

[<Fact>]
let ``Can find encryption key``() =
    let enc = encrypt (keyPub()) (doorLoop())
    let enc2 = encrypt (doorPub()) (keyLoop())
    Assert.Equal(enc, enc2)
    Assert.Equal(15217943uL, enc)
