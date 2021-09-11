module LibraryTests

open Libraries
open Xunit

[<Fact>]
let ``Match library names`` () =
    Assert.True(matchLibraryName [] [])
    Assert.True(matchLibraryName ["test"] ["test"])
    Assert.True(matchLibraryName ["scheme";"base"] ["scheme";"base"])
    Assert.False(matchLibraryName ["scheme"] ["scheme";"base"])
    Assert.False(matchLibraryName ["scheme";"base"] ["scheme"])

[<Fact>]
let ``pretty names`` () =
    Assert.Equal("()", prettifyLibraryName [])
    Assert.Equal("(test library)", prettifyLibraryName ["test";"library"])
    Assert.Equal("(foo)", prettifyLibraryName ["foo"])
