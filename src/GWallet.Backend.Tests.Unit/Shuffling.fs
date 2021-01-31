namespace GWallet.Backend.Tests.Unit

open System.Linq

open NUnit.Framework

open GWallet.Backend

[<TestFixture>]
type Shuffling() =

    [<Test>]
    member __.``retrieves same number of elements``() =
        let someList = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; ]
        let randomizedList = Shuffler.Unsort someList

        Assert.That(randomizedList.Count(), Is.EqualTo(10))

    [<Test>]
    member __.``doesn't return same list'``() =
        let someList = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; ]
        let randomizedList = Shuffler.Unsort someList

        // very unlikely to give a false positive:
        Assert.That(someList, Is.Not.EqualTo(randomizedList))

    [<Test>]
    member __.``doesn't randomize in the same way'``() =
        let someList = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; ]
        let randomizedList1 = Shuffler.Unsort someList
        let randomizedList2 = Shuffler.Unsort someList

        // very unlikely to give a false positive:
        Assert.That(randomizedList1, Is.Not.EqualTo(randomizedList2))

    [<Test>]
    member __.``replaces every n-th element with one chosen random element from the rest of the list``() =
        let someList = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; ]

        let shuffledList = Shuffler.RandomizeEveryNthElement someList 3u

        Assert.That(shuffledList, Is.Not.EqualTo someList)
        Assert.That(shuffledList.[0], Is.EqualTo 1)
        Assert.That(shuffledList.[1], Is.EqualTo 2)
        Assert.That(shuffledList.[2], Is.Not.EqualTo 3)
        let chosen1 = shuffledList.[2]
        Assert.That(shuffledList.[3], Is.EqualTo 3)
        if chosen1 = 4 then
            Assert.That(shuffledList.[4], Is.EqualTo 5)
            Assert.That(shuffledList.[5], Is.Not.EqualTo 6)
        else
            Assert.That(shuffledList.[4], Is.EqualTo 4)
            Assert.That(shuffledList.[4], Is.Not.EqualTo 5)

