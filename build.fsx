#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"
#r "netstandard"

open System.IO
open Fake.Core
open Fake.DotNet
open Fake.IO

Target.initEnvironment ()

let libPath = "./src"
let testsPath = "./test"

let npm args workingDir =
    let npmPath =
        match ProcessUtils.tryFindFileOnPath "npm" with
        | Some path -> path
        | None ->
            "npm was not found in path. Please install it and make sure it's available from your path. " +
            "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
            |> failwith

    let arguments = args |> String.split ' ' |> Arguments.OfArgs

    RawCommand (npmPath, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let dotnet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

let delete file =
    if File.Exists(file)
    then File.Delete file
    else ()

let cleanBundles() =
    Path.Combine("public", "bundle.js")
        |> Path.GetFullPath
        |> delete
    Path.Combine("public", "bundle.js.map")
        |> Path.GetFullPath
        |> delete

let cleanDirs dirs = dirs |> List.iter Shell.cleanDir

Target.create "Clean" <| fun _ ->
    [ Path.Combine(testsPath, "bin")
      Path.Combine(testsPath, "obj")
      Path.Combine(libPath, "bin")
      Path.Combine(libPath, "obj") ]
    |> cleanDirs

    cleanBundles()

Target.create "InstallNpmPackages" <| fun _ -> npm "install" "."

Target.create "Live" <| fun _ ->
    npm "start" "."

Target.create "RunTests" <| fun _ ->
    printfn "Building %s with Fable" testsPath
    npm "test" "."
    npm "run headless-tests" "."
    cleanBundles()
    cleanDirs [ "dist" ]

open Fake.Core.TargetOperators

"Clean"
  ==> "InstallNpmPackages"
  ==> "Live"

"Clean"
    ==> "InstallNpmPackages"
    ==> "RunTests"

"Clean"
    ==> "InstallNpmPackages"
    ==> "RunTests"

Target.runOrDefaultWithArguments "RunTests"