// Warning: generated file; your changes could be lost when a new file is generated.
#I __SOURCE_DIRECTORY__
#r "System.Core.dll"
#r "System.dll"
#r "System.Numerics.dll"
#r "System.Xml.dll"
#r "System.Xml.Linq.dll"
#r "../../fsharpLearn/bin/Debug/fsharpLearn.exe"
#r "../../packages/NUnit.3.4.1/lib/net45/nunit.framework.dll"
#r "../../packages/NUnit3TestAdapter.3.4.1/lib/Mono.Cecil.dll"
#r "../../packages/NUnit3TestAdapter.3.4.1/lib/Mono.Cecil.Mdb.dll"
#r "../../packages/NUnit3TestAdapter.3.4.1/lib/Mono.Cecil.Pdb.dll"
#r "../../packages/NUnit3TestAdapter.3.4.1/lib/Mono.Cecil.Rocks.dll"
#r "../../packages/NUnit3TestAdapter.3.4.1/lib/nunit.engine.api.dll"
#r "../../packages/NUnit3TestAdapter.3.4.1/lib/nunit.engine.dll"
#r "../../packages/NUnit3TestAdapter.3.4.1/lib/NUnit3.TestAdapter.dll"
#r "../../packages/Unquote.3.1.2/lib/net45/Unquote.dll"

// Warning: generated file; your changes coul d be lost when a new file is generated.
#I __SOURCE_DIRECTORY__
#load "load-references-debug.fsx"
#load "../AssemblyInfo.fs"
      "../Tests.fs"

open fsharpLearn.Types
open fsharpLearn.Parser
open fsharpLearn.mainModule 
open Swensen.Unquote
open NUnit.Framework
open fsharpLearn.mainModule
open fsharpLearn.Parser
open fsharpLearn.Types
open System
open System.Collections
open System.Linq
open System.Text.RegularExpressions
open fsharpLearn.Types
open fsharpLearn.Parser
open Cenas

parseInput "(1+sqrt(1+sqrt(1)))*sqrt (1)"
