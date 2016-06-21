module Cenas
open NUnit.Framework
open FluentAssertions.Common

[<TestFixture>]
type TestClass() = 

    [<Test>]
    member this.``When 2 is added to 2 expect 4``() = 
        
        Assert.AreEqual(4, 2+2)

    [<Test>]
    member this.cenas() = Assert.AreEqual(4, 2+2)
