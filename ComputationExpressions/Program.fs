module Program

type ZeroBuilder() =
  member __.Zero() = printfn "computation expressions!"

let zero = ZeroBuilder()

type SampleBuilder() =
  member __.Zero() = ()
  member __.Quote() = ()
  member __.Run(f) = f
  member __.Delayf(f) =f
let sample = SampleBuilder()

module HelloExtension =

  type ZeroBuilder with
    member __.Delay(f) = f
    member __.Run(f: unit -> unit) =
      printf "Hello "
      f ()

type ZeroWrapperBuilder() =
  member __.Zero() =
    printf "Hello "
    zero.Zero()

let wrapper = ZeroWrapperBuilder()

type OptionBuilder() =
  member inline __.Return(x) = Some x
  member inline __.ReturnFrom(x: _ option) = x
  member inline __.Bind(x, f) = Option.bind f x

let option = OptionBuilder()

let guard cond = if cond then Some () else None

module Column =

  type OptionBuilder() =
    member __.ReturnFrom(x: _ option) = x
    member __.Source(x: Result<_, _>) =
      match x with Ok r -> Some r | Error _ -> None
  let option = OptionBuilder()

  let test = option {
    return! Ok 0
  }

type ListBuilder() =
  member __.Yield(x) = [x]
  member __.YieldFrom(xs: _ list) = xs
  member __.Combine(xs, f: unit -> _ list) =
    List.append xs (f ())
  member __.Delay(f) = f
  member __.Run(f) = f ()
let list = ListBuilder()

open System

type OptionBuilder with
  member __.Zero() = None
  member __.Using(x: #IDisposable, f: #IDisposable -> _ option) =
    try f x
    finally match box x with null -> () | notNull -> x.Dispose()
  member __.TryWith(f, h) = try f () with e -> h e
  member __.TryFinally(f, g) = try f () finally g ()
  member __.Delay(f: unit -> _ option) = f
  member __.Run(f) = f ()

type WithZeroBuilder(zero: string) =
  member __.Zero() = printfn "%s!" zero
let withZero value = WithZeroBuilder(value)

type InputOptions = {
  Label: String
  Input: String
  Validators : (string -> bool) array
}
type InputBuilder() =
  member __.Yield(()) = ()
  [<CustomOperation("text")>]
  member __.Text(label, input) =
    { Label = label; Input = input; Validators = [||] }
  [<CustomOperation("label")>]
  member this.Label((),label) = label
  [<CustomOperation("validators")>]
  member this.Validators(options, [<ParamArray>] validators) =
    { options with Validators = validators }
let input = InputBuilder()

open System.Reflection

type Line1 = Line1
type Line2 = Line2
type Line3 = Line3
type Line4 = Line4

let inline operatorName () =
  (MethodBase.GetCurrentMethod()
     .GetCustomAttribute(typeof<CustomOperationAttribute>)
  :?> CustomOperationAttribute).Name

let inline printName line before =
  let name = operatorName ()
  (line, async {
    do! before
    do! Async.Sleep 1000
    printfn "%s" name
  })

type MonadAriaBuilder() =
  member __.Delay(f: unit -> _) = f
  member __.Run(f: unit -> Line4 * Async<_>) =
    f () |> snd |> Async.RunSynchronously
  member __.Yield(()) = async { () }
  [<CustomOperation("単なる")>]
  member __.Line1(x: Async<unit>) =
    printName Line1 x
  [<CustomOperation("自己関手の")>]
  member __.Line2((Line1, x)) =
    printName Line2 x
  [<CustomOperation("圏における")>]
  member __.Line3((Line2, x)) =
    printName Line3 x
  [<CustomOperation("モノイド対象だよ")>]
  member __.Line4((Line3, x)) =
    printName Line4 x

let Monadとは = MonadAriaBuilder()



// debug mode

module Debug =

  open FSharp.Quotations

  type ZeroBuilder with
    member __.Quote() = ()
    member __.Run(expr: Expr<_>) =
      printfn "%A" expr

  type OptionBuilder with
    member __.Quote() = ()

//open Debug

let ``minimal builder`` () =
  printfn "minimal builer"
  zero { () }
  zero { (); }
  zero { printf "Hello " }
  zero { printf "Hello "; printf "F# " }

let ``zero wrapper`` () =
  printfn "zero wrapper"
  wrapper { () }

let ``monad option`` () =
  printfn "option builder"
  option { return 0 }
  |> printfn "%A"
  option { return! None }
  |> printfn "%A"
  option {
    let! a = Some 1 in return a
  }
  |> printfn "%A"
  option {
    let! a = Some 1
    let! b = None
    return a + b
  }
  |> printfn "%A"
  option.Bind(Some 1, fun a ->
    option.Bind(None, fun b -> option.Return(a + b))
  )
  |> printfn "%A"
  option {
    let! a = Some 2
    do! guard (a > 1)
    return a
  } |> printfn "%A"
  option.Bind(Some 2, fun a ->
    option.Bind(guard (a > 1), fun () -> option.Return(a))
  )
  |> printfn "%A"
  option {
    let! a = Some 2
    do! guard (a > 1)
  } |> printfn "%A"
  option.Bind(Some 2, fun a ->
    option.Bind(guard (a > 1), fun () -> option.Return())
  )
  |> printfn "%A"

let ``monoid list`` () =
  printfn "list builder"
  list { yield 0 }
  |> printfn "%A"
  list { yield! [1; 2] }
  |> printfn "%A"
  list {
    yield 0
    yield! [1; 2]
  }
  |> printfn "%A"
  list.Run(list.Delay(fun () ->
    list.Combine(list.Yield(0), list.Delay(fun () ->
      list.YieldFrom([1; 2])
    ))
  ))
  |> printfn "%A"
  list {
    yield 0
    yield! [1; 2]
    yield 3
  }
  |> printfn "%A"
  list.Run(list.Delay(fun () ->
    list.Combine(list.Yield(0), list.Delay(fun () ->
      list.Combine(list.YieldFrom([1; 2]), list.Delay(fun () ->
          list.Yield(3)
      ))
    ))
  ))
  |> printfn "%A"

type Hoge = Hoge of int

type Foo = Foo of int
with
  interface IDisposable with
    member __.Dispose() = printfn "call Dispose()"

let ``more option`` () =
  printfn "more option builder"
  option {
    let! a = Some 1
    if a > 1 then return a
    else return -1
  }
  |> printfn "%A"
  option {
    let! a = Some(Hoge 1)
    match a with
    | Hoge b -> return b
  }
  |> printfn "%A"
  option {
    match! Some(Hoge 1) with
    | Hoge b -> return b
  }
  |> printfn "%A"
  option {
    use a = Foo 1
    return match a with | Foo b -> b
  }
  |> printfn "%A"
  option {
    use! a = Some(Foo 1)
    return match a with | Foo b -> b
  }
  |> printfn "%A"
  option {
    try return 1
    with _ ->
      printfn "catch!"
      return -1
  }
  |> printfn "%A"
  option {
    try return 1
    finally printfn "finally!"
  }
  |> printfn "%A"

let ``with zero`` () =
  printfn "with zero"
  withZero "Zero!" { () }

let ``custom operator`` () =
  input {
    label "Name"
    text "Your name"
    validators
        (String.IsNullOrWhiteSpace >> not)
  }
  |> printfn "%A"

let ``aria monad`` () = Monadとは {
  単なる
  自己関手の
  圏における
  モノイド対象だよ
}

[<EntryPoint>]
let main argv =
  ``minimal builder`` ()
  ``zero wrapper`` ()
  ``monad option`` ()
  ``monoid list`` ()
  ``more option`` ()
  ``with zero`` ()
  ``custom operator`` ()
  ``aria monad`` ()
  0

