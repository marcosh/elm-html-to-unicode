module ElmEscapeHtml exposing (escape, unescape)

{-| This library allows to escape html string and unescape named and numeric
character references (e.g. &gt;, &#62;, &x3e;) to the corresponding unicode
characters

#Definition

@docs escape, unescape

-}

import Char
import Dict
import Result
import String


{-| Escapes a string converting characters that could be used to inject XSS
vectors (<http://wonko.com/post/html-escaping>). At the moment we escape &, <, >,
", ', \`, , !, @, $, %, (, ), =, +, {, }, [ and ]

for example

escape "&<>\"" == "&amp;&lt;&gt;&quot;"

-}
escape : String -> String
escape =
    convert escapeChars


{-| Unescapes a string, converting all named and numeric character references
(e.g. &gt;, &#62;, &x3e;) to their corresponding unicode characters.

for example

unescape "&quot;&amp;&lt;&gt;" == "\"&<>"

-}
unescape : String -> String
unescape =
    convert unescapeChars



{- Helper function that applies a converting function to a string as a list of
   characters
-}


convert : (List Char -> List Char) -> String -> String
convert convertChars string =
    string
        |> String.toList
        |> convertChars
        |> List.map String.fromChar
        |> String.concat



{- escapes the characters one by one -}


escapeChars : List Char -> List Char
escapeChars list =
    list
        |> List.map escapeChar
        |> List.concat



{- function that actually performs the escaping of a single character -}


escapeChar : Char -> List Char
escapeChar char =
    Maybe.withDefault [ char ] (Dict.get char escapeDictionary)



{- dictionary that keeps track of the characters that need to be escaped -}


escapeDictionary : Dict.Dict Char (List Char)
escapeDictionary =
    Dict.fromList <|
        List.map (\( char, string ) -> ( char, String.toList string ))
            [ ( '&', "&amp;" )
            , ( '<', "&lt;" )
            , ( '>', "&gt;" )
            , ( '"', "&quot;" )
            , ( '\'', "&#39;" )
            , ( '`', "&#96;" )
            , ( ' ', "&#32;" )
            , ( '!', "&#33;" )
            , ( '@', "&#64;" )
            , ( '$', "&#36;" )
            , ( '%', "&#37;" )
            , ( '(', "&#40;" )
            , ( ')', "&#41;" )
            , ( '=', "&#61;" )
            , ( '+', "&#43;" )
            , ( '{', "&#123;" )
            , ( '}', "&#125;" )
            , ( '[', "&#91;" )
            , ( ']', "&#93;" )
            ]



{- unescapes the characters one by one -}


unescapeChars : List Char -> List Char
unescapeChars list =
    parser list [] []



{- recursive function that perform the parsing of a list of characters, keeping
   track of what still need to be parsed, what constitutes the list of characters
   considered for the unencoding, and what has already been parsed

   @par list of characters to parsed
   @par list of characters that are on hold to create the next character
   @par list of characters already parsed
   @ret parsed list of characters
-}


parser : List Char -> List Char -> List Char -> List Char
parser charsToBeParsed charsOnParsing charsParsed =
    case charsToBeParsed of
        [] ->
            charsParsed

        head :: tail ->
            if head == '&' then
                parser tail [ head ] charsParsed

            else if head == ';' then
                parser tail [] (List.append charsParsed (unicodeConverter head charsOnParsing))

            else if not (List.isEmpty charsOnParsing) then
                parser tail (List.append charsOnParsing [ head ]) charsParsed

            else
                parser tail [] (List.append charsParsed [ head ])



{- unencodes the next char considering what other characters are expecting to be
   parsed.
   At the moment called only when post is equal to ';'
-}


unicodeConverter : Char -> List Char -> List Char
unicodeConverter post list =
    case list of
        [] ->
            [ post ]

        head :: tail ->
            noAmpUnicodeConverter head post tail



{- unencodes the next char considering what other characters are expecting to be
   parsed, isolating the first of these characters
   At the moment called with pre = '&' and post = ';', to delimitate an encoded
   character

   @par character to prepend if the conversion is not possible
   @par character to append if the conversion is not possible
   @par list of characters to convert
   @ret parsed list of characters
-}


noAmpUnicodeConverter : Char -> Char -> List Char -> List Char
noAmpUnicodeConverter pre post list =
    case list of
        [] ->
            [ pre, post ]

        '#' :: tail ->
            convertNumericalCode [ pre, '#' ] [ post ] tail

        head :: tail ->
            convertFriendlyCode [ pre ] [ post ] (head :: tail)



{- unencodes a list of characters representing a numerically encoded character

   @par list of characters to prepend if the conversion is not possible
   @par list of characters to append if the conversion is not possible
   @par list of characters to convert
   @ret parsed list of characters
-}


convertNumericalCode : List Char -> List Char -> List Char -> List Char
convertNumericalCode pre post list =
    case list of
        [] ->
            List.concat [ pre, post ]

        'x' :: tail ->
            convertHexadecimalCode (List.append pre [ 'x' ]) post tail

        anyOtherList ->
            convertDecimalCode pre post anyOtherList



{- helper function to create unescaping functions -}


convertCode : (String -> Maybe a) -> (a -> List Char) -> List Char -> List Char -> List Char -> List Char
convertCode mayber lister pre post list =
    let
        string =
            String.fromList list

        maybe =
            mayber string
    in
    case maybe of
        Nothing ->
            List.concat [ pre, list, post ]

        Just something ->
            lister something



{- Convert String to Int assuming base 16. I include this here until elm-parseint is upgraded to Elm 0.19 -}


parseIntHex : String -> Maybe Int
parseIntHex string =
    parseIntR (String.reverse string)


parseIntR : String -> Maybe Int
parseIntR string =
    case
        String.uncons string
    of
        Nothing ->
            Just 0

        Just ( c, tail ) ->
            intFromChar c
                |> Maybe.andThen
                    (\ci ->
                        parseIntR tail
                            |> Maybe.andThen (\ri -> Just (ci + ri * 16))
                    )


intFromChar : Char -> Maybe Int
intFromChar c =
    let
        toInt =
            if isBetween '0' '9' c then
                Just (charOffset '0' c)

            else if isBetween 'a' 'z' c then
                Just (10 + charOffset 'a' c)

            else if isBetween 'A' 'Z' c then
                Just (10 + charOffset 'A' c)

            else
                Nothing

        validInt i =
            if i < 16 then
                Just i

            else
                Nothing
    in
    toInt |> Maybe.andThen validInt


isBetween : Char -> Char -> Char -> Bool
isBetween lower upper c =
    let
        ci =
            Char.toCode c
    in
    Char.toCode lower <= ci && ci <= Char.toCode upper


charOffset : Char -> Char -> Int
charOffset basis c =
    Char.toCode c - Char.toCode basis



{- unencodes a list of characters representing a hexadecimally encoded character -}


convertHexadecimalCode : List Char -> List Char -> List Char -> List Char
convertHexadecimalCode =
    convertCode parseIntHex (\int -> [ Char.fromCode int ])



{- unencodes a list of characters representing a decimally encoded character -}


convertDecimalCode : List Char -> List Char -> List Char -> List Char
convertDecimalCode =
    convertCode String.toInt (\int -> [ Char.fromCode int ])



{- unencodes a list of characters representing a friendly encoded character -}


convertFriendlyCode : List Char -> List Char -> List Char -> List Char
convertFriendlyCode =
    convertCode convertFriendlyCodeToChar (\char -> [ char ])



{- unencodes a string looking in the friendlyConverterDictionary -}


convertFriendlyCodeToChar : String -> Maybe Char
convertFriendlyCodeToChar string =
    Dict.get string friendlyConverterDictionary



{- dictionary to keep track of the sequence of characters that represent a
   friendly html encoded character
-}


friendlyConverterDictionary : Dict.Dict String Char
friendlyConverterDictionary =
    Dict.fromList <|
        List.map (\( a, b ) -> ( a, Char.fromCode b ))
            [ ( "quot", 34 )
            , ( "amp", 38 )
            , ( "lt", 60 )
            , ( "gt", 62 )
            , ( "nbsp", 160 )
            , ( "iexcl", 161 )
            , ( "cent", 162 )
            , ( "pound", 163 )
            , ( "curren", 164 )
            , ( "yen", 165 )
            , ( "brvbar", 166 )
            , ( "sect", 167 )
            , ( "uml", 168 )
            , ( "copy", 169 )
            , ( "ordf", 170 )
            , ( "laquo", 171 )
            , ( "not", 172 )
            , ( "shy", 173 )
            , ( "reg", 174 )
            , ( "macr", 175 )
            , ( "deg", 176 )
            , ( "plusmn", 177 )
            , ( "sup2", 178 )
            , ( "sup3", 179 )
            , ( "acute", 180 )
            , ( "micro", 181 )
            , ( "para", 182 )
            , ( "middot", 183 )
            , ( "cedil", 184 )
            , ( "sup1", 185 )
            , ( "ordm", 186 )
            , ( "raquo", 187 )
            , ( "frac14", 188 )
            , ( "frac12", 189 )
            , ( "frac34", 190 )
            , ( "iquest", 191 )
            , ( "Agrave", 192 )
            , ( "Aacute", 193 )
            , ( "Acirc", 194 )
            , ( "Atilde", 195 )
            , ( "Auml", 196 )
            , ( "Aring", 197 )
            , ( "AElig", 198 )
            , ( "Ccedil", 199 )
            , ( "Egrave", 200 )
            , ( "Eacute", 201 )
            , ( "Ecirc", 202 )
            , ( "Euml", 203 )
            , ( "Igrave", 204 )
            , ( "Iacute", 205 )
            , ( "Icirc", 206 )
            , ( "Iuml", 207 )
            , ( "ETH", 208 )
            , ( "Ntilde", 209 )
            , ( "Ograve", 210 )
            , ( "Oacute", 211 )
            , ( "Ocirc", 212 )
            , ( "Otilde", 213 )
            , ( "Ouml", 214 )
            , ( "times", 215 )
            , ( "Oslash", 216 )
            , ( "Ugrave", 217 )
            , ( "Uacute", 218 )
            , ( "Ucirc", 219 )
            , ( "Uuml", 220 )
            , ( "Yacute", 221 )
            , ( "THORN", 222 )
            , ( "szlig", 223 )
            , ( "agrave", 224 )
            , ( "aacute", 225 )
            , ( "acirc", 226 )
            , ( "atilde", 227 )
            , ( "auml", 228 )
            , ( "aring", 229 )
            , ( "aelig", 230 )
            , ( "ccedil", 231 )
            , ( "egrave", 232 )
            , ( "eacute", 233 )
            , ( "ecirc", 234 )
            , ( "euml", 235 )
            , ( "igrave", 236 )
            , ( "iacute", 237 )
            , ( "icirc", 238 )
            , ( "iuml", 239 )
            , ( "eth", 240 )
            , ( "ntilde", 241 )
            , ( "ograve", 242 )
            , ( "oacute", 243 )
            , ( "ocirc", 244 )
            , ( "otilde", 245 )
            , ( "ouml", 246 )
            , ( "divide", 247 )
            , ( "oslash", 248 )
            , ( "ugrave", 249 )
            , ( "uacute", 250 )
            , ( "ucirc", 251 )
            , ( "uuml", 252 )
            , ( "yacute", 253 )
            , ( "thorn", 254 )
            , ( "yuml", 255 )
            , ( "Amacr", 256 )
            , ( "amacr", 257 )
            , ( "Abreve", 258 )
            , ( "abreve", 259 )
            , ( "Aogon", 260 )
            , ( "aogon", 261 )
            , ( "Cacute", 262 )
            , ( "cacute", 263 )
            , ( "Ccirc", 264 )
            , ( "ccirc", 265 )
            , ( "Cdod", 266 )
            , ( "cdot", 267 )
            , ( "Ccaron", 268 )
            , ( "ccaron", 269 )
            , ( "Dcaron", 270 )
            , ( "dcaron", 271 )
            , ( "Dstork", 272 )
            , ( "dstork", 273 )
            , ( "Emacr", 274 )
            , ( "emacr", 275 )
            , ( "Edot", 278 )
            , ( "edot", 279 )
            , ( "Eogon", 280 )
            , ( "eogon", 281 )
            , ( "Ecaron", 282 )
            , ( "ecaron", 283 )
            , ( "Gcirc", 284 )
            , ( "gcirc", 285 )
            , ( "Gbreve", 286 )
            , ( "gbreve", 287 )
            , ( "Gdot", 288 )
            , ( "gdot", 289 )
            , ( "Gcedil", 290 )
            , ( "gcedil", 291 )
            , ( "Hcirc", 292 )
            , ( "hcirc", 293 )
            , ( "Hstork", 294 )
            , ( "hstork", 295 )
            , ( "Itilde", 296 )
            , ( "itilde", 297 )
            , ( "Imacr", 298 )
            , ( "imacr", 299 )
            , ( "Iogon", 302 )
            , ( "iogon", 303 )
            , ( "Idot", 304 )
            , ( "inodot", 305 )
            , ( "IJlog", 306 )
            , ( "ijlig", 307 )
            , ( "Jcirc", 308 )
            , ( "jcirc", 309 )
            , ( "Kcedil", 310 )
            , ( "kcedil", 311 )
            , ( "kgreen", 312 )
            , ( "Lacute", 313 )
            , ( "lacute", 314 )
            , ( "Lcedil", 315 )
            , ( "lcedil", 316 )
            , ( "Lcaron", 317 )
            , ( "lcaron", 318 )
            , ( "Lmodot", 319 )
            , ( "lmidot", 320 )
            , ( "Lstork", 321 )
            , ( "lstork", 322 )
            , ( "Nacute", 323 )
            , ( "nacute", 324 )
            , ( "Ncedil", 325 )
            , ( "ncedil", 326 )
            , ( "Ncaron", 327 )
            , ( "ncaron", 328 )
            , ( "napos", 329 )
            , ( "ENG", 330 )
            , ( "eng", 331 )
            , ( "Omacr", 332 )
            , ( "omacr", 333 )
            , ( "Odblac", 336 )
            , ( "odblac", 337 )
            , ( "OEling", 338 )
            , ( "oelig", 339 )
            , ( "Racute", 340 )
            , ( "racute", 341 )
            , ( "Rcedil", 342 )
            , ( "rcedil", 343 )
            , ( "Rcaron", 344 )
            , ( "rcaron", 345 )
            , ( "Sacute", 346 )
            , ( "sacute", 347 )
            , ( "Scirc", 348 )
            , ( "scirc", 349 )
            , ( "Scedil", 350 )
            , ( "scedil", 351 )
            , ( "Scaron", 352 )
            , ( "scaron", 353 )
            , ( "Tcedil", 354 )
            , ( "tcedil", 355 )
            , ( "Tcaron", 356 )
            , ( "tcaron", 357 )
            , ( "Tstork", 358 )
            , ( "tstork", 359 )
            , ( "Utilde", 360 )
            , ( "utilde", 361 )
            , ( "Umacr", 362 )
            , ( "umacr", 363 )
            , ( "Ubreve", 364 )
            , ( "ubreve", 365 )
            , ( "Uring", 366 )
            , ( "uring", 367 )
            , ( "Udblac", 368 )
            , ( "udblac", 369 )
            , ( "Uogon", 370 )
            , ( "uogon", 371 )
            , ( "Wcirc", 372 )
            , ( "wcirc", 373 )
            , ( "Ycirc", 374 )
            , ( "ycirc", 375 )
            , ( "Yuml", 376 )
            , ( "Zacute", 377 )
            , ( "zacute", 378 )
            , ( "Zdot", 379 )
            , ( "zdot", 380 )
            , ( "Zcaron", 381 )
            , ( "zcaron", 382 )
            , ( "fnof", 402 )
            , ( "imped", 437 )
            , ( "gacute", 501 )
            , ( "jmath", 567 )
            , ( "circ", 710 )
            , ( "tilde", 732 )
            , ( "Alpha", 913 )
            , ( "Beta", 914 )
            , ( "Gamma", 915 )
            , ( "Delta", 916 )
            , ( "Epsilon", 917 )
            , ( "Zeta", 918 )
            , ( "Eta", 919 )
            , ( "Theta", 920 )
            , ( "Iota", 921 )
            , ( "Kappa", 922 )
            , ( "Lambda", 923 )
            , ( "Mu", 924 )
            , ( "Nu", 925 )
            , ( "Xi", 926 )
            , ( "Omicron", 927 )
            , ( "Pi", 928 )
            , ( "Rho", 929 )
            , ( "Sigma", 931 )
            , ( "Tau", 932 )
            , ( "Upsilon", 933 )
            , ( "Phi", 934 )
            , ( "Chi", 935 )
            , ( "Psi", 936 )
            , ( "Omega", 937 )
            , ( "alpha", 945 )
            , ( "beta", 946 )
            , ( "gamma", 947 )
            , ( "delta", 948 )
            , ( "epsilon", 949 )
            , ( "zeta", 950 )
            , ( "eta", 951 )
            , ( "theta", 952 )
            , ( "iota", 953 )
            , ( "kappa", 954 )
            , ( "lambda", 955 )
            , ( "mu", 956 )
            , ( "nu", 957 )
            , ( "xi", 958 )
            , ( "omicron", 959 )
            , ( "pi", 960 )
            , ( "rho", 961 )
            , ( "sigmaf", 962 )
            , ( "sigma", 963 )
            , ( "tau", 934 )
            , ( "upsilon", 965 )
            , ( "phi", 966 )
            , ( "chi", 967 )
            , ( "psi", 968 )
            , ( "omega", 969 )
            , ( "thetasym", 977 )
            , ( "upsih", 978 )
            , ( "straightphi", 981 )
            , ( "piv", 982 )
            , ( "Gammad", 988 )
            , ( "gammad", 989 )
            , ( "varkappa", 1008 )
            , ( "varrho", 1009 )
            , ( "straightepsilon", 1013 )
            , ( "backepsilon", 1014 )
            , ( "ensp", 8194 )
            , ( "emsp", 8195 )
            , ( "thinsp", 8201 )
            , ( "zwnj", 8204 )
            , ( "zwj", 8205 )
            , ( "lrm", 8206 )
            , ( "rlm", 8207 )
            , ( "ndash", 8211 )
            , ( "mdash", 8212 )
            , ( "lsquo", 8216 )
            , ( "rsquo", 8217 )
            , ( "sbquo", 8218 )
            , ( "ldquo", 8220 )
            , ( "rdquo", 8221 )
            , ( "bdquo", 8222 )
            , ( "dagger", 8224 )
            , ( "Dagger", 8225 )
            , ( "bull", 8226 )
            , ( "hellip", 8230 )
            , ( "permil", 8240 )
            , ( "prime", 8242 )
            , ( "Prime", 8243 )
            , ( "lsaquo", 8249 )
            , ( "rsaquo", 8250 )
            , ( "oline", 8254 )
            , ( "frasl", 8260 )
            , ( "sigma", 963 )
            , ( "euro", 8364 )
            , ( "image", 8465 )
            , ( "weierp", 8472 )
            , ( "real", 8476 )
            , ( "trade", 8482 )
            , ( "alefsym", 8501 )
            , ( "larr", 8592 )
            , ( "uarr", 8593 )
            , ( "rarr", 8594 )
            , ( "darr", 8595 )
            , ( "harr", 8596 )
            , ( "crarr", 8629 )
            , ( "lArr", 8656 )
            , ( "uArr", 8657 )
            , ( "rArr", 8658 )
            , ( "dArr", 8659 )
            , ( "hArr", 8660 )
            , ( "forall", 8704 )
            , ( "part", 8706 )
            , ( "exist", 8707 )
            , ( "empty", 8709 )
            , ( "nabla", 8711 )
            , ( "isin", 8712 )
            , ( "notin", 8713 )
            , ( "ni", 8715 )
            , ( "prod", 8719 )
            , ( "sum", 8721 )
            , ( "minus", 8722 )
            , ( "lowast", 8727 )
            , ( "radic", 8730 )
            , ( "prop", 8733 )
            , ( "infin", 8734 )
            , ( "ang", 8736 )
            , ( "and", 8743 )
            , ( "or", 8744 )
            , ( "cap", 8745 )
            , ( "cup", 8746 )
            , ( "int", 8747 )
            , ( "there4", 8756 )
            , ( "sim", 8764 )
            , ( "cong", 8773 )
            , ( "asymp", 8776 )
            , ( "ne", 8800 )
            , ( "equiv", 8801 )
            , ( "le", 8804 )
            , ( "ge", 8805 )
            , ( "sub", 8834 )
            , ( "sup", 8835 )
            , ( "nsub", 8836 )
            , ( "sube", 8838 )
            , ( "supe", 8839 )
            , ( "oplus", 8853 )
            , ( "otimes", 8855 )
            , ( "perp", 8869 )
            , ( "sdot", 8901 )
            , ( "loz", 9674 )
            , ( "spades", 9824 )
            , ( "clubs", 9827 )
            , ( "hearts", 9829 )
            , ( "diams", 9830 )
            ]
