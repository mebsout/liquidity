(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2019 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                                                                          *)
(*  This program is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU General Public License as published by    *)
(*  the Free Software Foundation, either version 3 of the License, or       *)
(*  (at your option) any later version.                                     *)
(*                                                                          *)
(*  This program is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*  GNU General Public License for more details.                            *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(****************************************************************************)

let gen_info ~decompile files =
  Printf.sprintf
    "%s**********************************************************************\n \
    File generated by Liquidity %s version %s\n \
    (commit %s, compiled on %s)\n \
    Date: %s\
    %s\n \
    Options:\n  \
    - Inlining: %b\n  \
    - Simplifications: %b\n  \
    - Peephole optimizations: %b\n  \
    %s\
    - Uncurrying: %b\n\
    **********************************************************************%s\n\n"
    (if decompile && !LiquidOptions.ocaml_syntax then "(*" else "/*")
    (if decompile then "decompiler" else "compiler")
    LiquidVersion.version
    LiquidVersion.commit
    LiquidVersion.en_date
    CalendarLib.(Printer.Calendar.to_string (Calendar.now ()))
    (match files with
     | [] -> ""
     | [f] -> "\n From file: " ^ f
     | _ -> "\n From files: " ^ String.concat ", " files)
    !LiquidOptions.inline
    !LiquidOptions.simplify
    !LiquidOptions.peephole
    (if decompile then
       Printf.sprintf
         "- Ignore annotations: %b\n  "
         !LiquidOptions.ignore_annots
     else
       Printf.sprintf "- Single line output: %b\n  \
                       - Generate annotations: %b\n  "
         !LiquidOptions.singleline
         (not !LiquidOptions.no_annot))
    (not !LiquidOptions.no_uncurrying)
    (if decompile then "*)" else "*/")
