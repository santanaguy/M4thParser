module Cenas

open NUnit.Framework
open FluentAssertions.Common

[<Test>]
let ``When 2 is added to 2 expect 4``() = 
    Assert.AreEqual(4, 2 + 2)

[<Test>]
let cenas() = Assert.AreEqual(4, 2 + 2)
