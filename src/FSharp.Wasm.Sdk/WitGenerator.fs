namespace FSharp.Wasm.Sdk

open System.Text

/// <summary>WIT type representation for interface generation.</summary>
type WitType =
    /// <summary>Unsigned 8-bit integer.</summary>
    | WitU8
    /// <summary>Unsigned 16-bit integer.</summary>
    | WitU16
    /// <summary>Unsigned 32-bit integer.</summary>
    | WitU32
    /// <summary>Unsigned 64-bit integer.</summary>
    | WitU64
    /// <summary>Signed 8-bit integer.</summary>
    | WitS8
    /// <summary>Signed 16-bit integer.</summary>
    | WitS16
    /// <summary>Signed 32-bit integer.</summary>
    | WitS32
    /// <summary>Signed 64-bit integer.</summary>
    | WitS64
    /// <summary>32-bit floating point.</summary>
    | WitF32
    /// <summary>64-bit floating point.</summary>
    | WitF64
    /// <summary>Boolean value.</summary>
    | WitBool
    /// <summary>UTF-8 string.</summary>
    | WitString
    /// <summary>Homogeneous list of values.</summary>
    | WitList of WitType
    /// <summary>Optional value.</summary>
    | WitOption of WitType
    /// <summary>Result type with ok and error variants.</summary>
    | WitResult of ok: WitType * err: WitType
    /// <summary>Reference to a named record type.</summary>
    | WitRecord of name: string
    /// <summary>Reference to a named variant type.</summary>
    | WitVariant of name: string

/// <summary>A function signature in a WIT interface.</summary>
type WitFunction =
    { /// <summary>Function name in kebab-case.</summary>
      Name: string
      /// <summary>Parameter list as (name, type) pairs.</summary>
      Params: (string * WitType) list
      /// <summary>Return types.</summary>
      Results: WitType list }

/// <summary>A WIT interface grouping related functions.</summary>
type WitInterface =
    { /// <summary>Interface name in kebab-case.</summary>
      Name: string
      /// <summary>Functions exposed by this interface.</summary>
      Functions: WitFunction list }

/// <summary>A WIT world defining imports and exports.</summary>
type WitWorld =
    { /// <summary>World name in kebab-case.</summary>
      Name: string
      /// <summary>Imported interfaces required by this world.</summary>
      Imports: WitInterface list
      /// <summary>Exported interfaces provided by this world.</summary>
      Exports: WitInterface list }

/// <summary>WIT file generation from typed interface definitions.</summary>
module WitGenerator =

    /// <summary>Convert a WitType to its WIT string representation.</summary>
    /// <param name="witType">The type to convert.</param>
    /// <returns>WIT-formatted type string.</returns>
    let rec witTypeToString (witType: WitType) : string =
        match witType with
        | WitU8 -> "u8"
        | WitU16 -> "u16"
        | WitU32 -> "u32"
        | WitU64 -> "u64"
        | WitS8 -> "s8"
        | WitS16 -> "s16"
        | WitS32 -> "s32"
        | WitS64 -> "s64"
        | WitF32 -> "f32"
        | WitF64 -> "f64"
        | WitBool -> "bool"
        | WitString -> "string"
        | WitList inner -> sprintf "list<%s>" (witTypeToString inner)
        | WitOption inner -> sprintf "option<%s>" (witTypeToString inner)
        | WitResult(ok, err) ->
            sprintf "result<%s, %s>" (witTypeToString ok) (witTypeToString err)
        | WitRecord name -> name
        | WitVariant name -> name

    /// <summary>Render a WIT function signature.</summary>
    let private renderFunction (indent: string) (func: WitFunction) : string =
        let sb = StringBuilder()
        sb.Append(indent).Append(func.Name).Append(": func(") |> ignore
        let paramStr =
            func.Params
            |> List.map (fun (name, typ) -> sprintf "%s: %s" name (witTypeToString typ))
            |> String.concat ", "
        sb.Append(paramStr).Append(")") |> ignore
        match func.Results with
        | [] -> ()
        | [ single ] ->
            sb.Append(" -> ").Append(witTypeToString single) |> ignore
        | multiple ->
            let resultStr =
                multiple
                |> List.map witTypeToString
                |> String.concat ", "
            sb.Append(" -> (").Append(resultStr).Append(")") |> ignore
        sb.Append(";") |> ignore
        sb.ToString()

    /// <summary>Render a WIT interface block.</summary>
    let private renderInterface (iface: WitInterface) : string =
        let sb = StringBuilder()
        sb.AppendFormat("interface {0} {{\n", iface.Name) |> ignore
        for func in iface.Functions do
            sb.AppendLine(renderFunction "  " func) |> ignore
        sb.Append("}") |> ignore
        sb.ToString()

    /// <summary>Generate a complete WIT file from a world definition.</summary>
    /// <param name="world">The WIT world to render.</param>
    /// <returns>Complete .wit file content as a string.</returns>
    let generateWit (world: WitWorld) : string =
        let sb = StringBuilder()
        // Render all interfaces
        let allInterfaces =
            (world.Imports @ world.Exports)
            |> List.distinctBy (fun i -> i.Name)
        for iface in allInterfaces do
            sb.AppendLine(renderInterface iface) |> ignore
            sb.AppendLine() |> ignore

        // Render world
        sb.AppendFormat("world {0} {{\n", world.Name) |> ignore
        for imp in world.Imports do
            sb.AppendFormat("  import {0};\n", imp.Name) |> ignore
        for exp in world.Exports do
            sb.AppendFormat("  export {0};\n", exp.Name) |> ignore
        sb.Append("}") |> ignore
        sb.ToString()
