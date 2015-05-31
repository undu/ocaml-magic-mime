(*
 * Copyright (c) 2015 Len Maxwell <len@lnmx.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let default_type = "application/octet-stream"

(* return true if b is a "binary byte"
 * ref https://mimesniff.spec.whatwg.org/#binary-data-byte *)
let is_binary_byte b =
  match b with
  | '\x00' .. '\x08' -> true
  | '\x0b'           -> true
  | '\x0e' .. '\x1a' -> true
  | '\x1c' .. '\x1f' -> true
  | _ -> false


(* return true if b is a "whitespace byte"
 * ref https://mimesniff.spec.whatwg.org/#whitespace-byte *)
let is_whitespace_byte b =
  match b with
  | '\x09' | '\x0a' | '\x0c' | '\x0d' | '\x20' -> true
  | _ -> false


(* return s without its first byte *)
let tail_of_string s =
  match s with
  | "" -> s
  | _ -> (String.sub s 1 ((String.length s) - 1))


(* return s without leading whitespace *)
let rec without_leading_ws s =
  match s with
  | "" -> s
  | _ -> if is_whitespace_byte s.[0] then
      without_leading_ws (tail_of_string s)
    else
      s


(* return [mime], ignore [content] *)
let match_any mime content =
  Some( mime )


(* return [mime] if [content] starts with [pattern] *)
let match_exact mime pattern content =
  if pattern = content then
    Some( mime )
  else
    None


(* return [mime] if [content] starts with [pattern], using '_' as a pattern wildcard *)
let rec match_mask_str mime pattern content =
  let skip = '_' in
  match pattern, content with
  | ("",  _) -> Some( mime ) (* no more pattern => match ok *)
  | ( _, "") -> None         (* no more content => match fail *)
  | _ -> if ( pattern.[0] == skip ) || ( pattern.[0] == content.[0] ) then
           match_mask_str mime (tail_of_string pattern) (tail_of_string content)
         else
           None


(* return true if [content] starts with "<tag " or "<tag>", case insensitive *)
let matches_html_tag tag content =
  let tag_len = (String.length tag) + 2 in
  if (String.length content) >= tag_len then
    let target = (String.lowercase (String.sub content 0 tag_len)) in
    ( target = ( "<" ^ tag ^ " " ) ) || ( target = ( "<" ^ tag ^ ">" ) )
  else
    false


(* return [mime] if [content] starts with any of the provided html [tags],
 * after skipping any leading whitespace *)
let match_html_tags mime tags content =
  let rec fn mime tags content =
    match tags with
    | [] -> None
    | tag :: rest ->
      if matches_html_tag tag content then
        Some( mime )
      else
        fn mime rest content
  in
  let content = without_leading_ws content in
  fn mime tags content


(* TODO S6.2.1 mp4 *)
let match_mp4 mime content =
  None


(* returns [mime] if content has no "binary bytes" *)
let rec match_text mime content =
  match content with
  | "" -> Some( mime )
  | _  -> if is_binary_byte content.[0] then
            None
          else 
            match_text mime (tail_of_string content)


(* [match_fn content] tests [content] against a signature,
 * returning a mime type string on success, and None on failure *)
type match_fn = string -> string option

(* list of signatures to test, in order, against content supplied to [detect] *)
let signatures : match_fn list = [

  (* ref https://mimesniff.spec.whatwg.org/#identifying-a-resource-with-an-unknown-mime-type *)

  (* empty input *)
  match_exact default_type "";

  (* S-7.1 html document *)
  match_html_tags "text/html" [
      "!doctype html"; "html"; "head"; "script"; "iframe";
      "h1"; "div"; "font"; "table"; "a"; "style"; "title";
      "b"; "body"; "br"; "p"; "!--"; "?xml";
  ];

  (* S-7.1 PDF, PS, plain text *)
  match_exact    "applicaiton/pdf"        "%PDF-";
  match_exact    "application/postscript" "%!PS-Adobe-";
  match_mask_str "text/plain"             "\xFE\xFF__";    (* UTF-16BE BOM *)
  match_mask_str "text/plain"             "\xFF\xFE__";    (* UTF-16LE BOM *)
  match_mask_str "text/plain"             "\xEF\xBB\xBF_"; (* UTF-16LE BOM *)


  (* S-6.1 image types *)
  match_exact    "image/x-icon" "\x00\x00\x01\x00";
  match_exact    "image/x-icon" "\x00\x00\x02\x00";
  match_exact    "image/bmp"    "BM";
  match_exact    "image/gif"    "GIF87a";
  match_exact    "image/gif"    "GIF89a";
  match_mask_str "image/webp"   "RIFF____WEBPVP";
  match_exact    "image/png"    "\x89PNG\x0d\x01\x1a\x0a";
  match_exact    "image/jpeg"   "\xFF\xD8\xFF";


  (* S-6.2 audio/video types *)
  match_exact    "video/webm"      "\x1A\x45\xDF\xA3";
  match_exact    "audio/basic"     "\x2E\x73\x6E\x64";
  match_mask_str "audio/aiff"      "FORM____AIFF";
  match_exact    "audio/mpeg"      "ID3";
  match_exact    "application/ogg" "OggS\x00";
  match_exact    "audio/midi"      "MThd\x00\x00\x00\x06";
  match_mask_str "video/avi"       "RIFF____AVI ";
  match_mask_str "audio/wave"      "RIFF____WAVE";
  match_mp4      "video/mp4";


  (* S-6.3 font types *)
  match_mask_str "application/vnd.ms-fontobject" ( (String.make 34 '_') ^ "LP" );
  (* TODO unknown mime types
  match_exact    "truetype"              "\x00\x01\x00\x00";
  match_exact    "opentype"              "OTTO";
  match_exact    "truetypecol"           "ttcf";
  *)
  match_exact    "application/font-woff" "wOFF";


  (* S-6.4 archive types *)
  match_exact "application/x-gzip"           "\x1F\x8B\x08";
  match_exact "application/zip"              "PK\x03\x04";
  match_exact "applicaiton/x-rar-compressed" "Rar \x1A\x07\x00";


  (* S-7.1 no "binary bytes" *)
  match_text "text/plain";


  (* fallback to octet-stream *)
  match_any default_type;
]

(* return at most 512 bytes of content
 * https://mimesniff.spec.whatwg.org/#resource-header *)
let header_of content =
    let header_length = 512 in
    if String.length( content ) > header_length then
        String.sub content 0 header_length
    else
        content

(* return the first successful match of [content] against
 * each entry in [sigs]; otherwise, return [default_type] *)
let rec detect_first content sigs =
  match sigs with
  | [] -> default_type
  | head :: rest -> match head content with
    | Some( mime ) -> mime
    | None -> detect_first content rest

let detect content =
  let header = header_of content in
  detect_first header signatures

