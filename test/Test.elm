import String
import ElmTest exposing (..)

import ElmEscapeHtml exposing (escape, unescape)


tests : Test
tests =
    suite "ElmEscapeHtml Test Suite"
        [ test "escape &<>\"'` !@$%()=+{}[]"
            ( assertEqual
                ( escape "&<>\"'` !@$%()=+{}[]" )
                "&amp;&lt;&gt;&quot;&#39;&#96;&#32;&#33;&#64;&#36;&#37;&#40;&#41;&#61;&#43;&#123;&#125;&#91;&#93;"
            )
        , test "escape \"helloworld\""
            ( assertEqual
                ( escape "helloworld" )
                "helloworld"
            )
        , test "escape <script>alert('inject')</script>"
            ( assertEqual
                ( escape "<script>alert('inject')</script>" )
                "&lt;script&gt;alert&#40;&#39;inject&#39;&#41;&lt;/script&gt;"
            )
        , test "unescape html friendly characters"
            ( assertEqual
                ( unescape "&quot;&amp;&lt;&gt;" )
                "\"&<>"
            )
        , test "fail to unescape false friendly characters"
            ( assertEqual
                ( unescape "&quotamp;" )
                "&quotamp;"
            )
        , test "unescape unicode characters"
            ( assertEqual
                ( unescape "&#39;&#40;&#41;" )
                "'()"
            )
        , test "unescape hexadecimal unicode characters"
            ( assertEqual
                ( unescape "&#x0027;&#x0028;&#x0029;" )
                "'()"
            )
        ]


main : Program Never
main =
    runSuite tests