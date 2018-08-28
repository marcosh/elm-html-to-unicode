module Tests exposing (tests)

import ElmEscapeHtml exposing (escape, unescape)
import Expect exposing (equal)
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "ElmEscapeHtml Test Suite"
        [ test "escape &<>\"'` !@$%()=+{}[]"
            (\() ->
                equal (escape "&<>\"'` !@$%()=+{}[]")
                    "&amp;&lt;&gt;&quot;&#39;&#96;&#32;&#33;&#64;&#36;&#37;&#40;&#41;&#61;&#43;&#123;&#125;&#91;&#93;"
            )
        , test "escape \"helloworld\""
            (\() ->
                equal (escape "helloworld")
                    "helloworld"
            )
        , test "escape <script>alert('inject')</script>"
            (\() ->
                equal (escape "<script>alert('inject')</script>")
                    "&lt;script&gt;alert&#40;&#39;inject&#39;&#41;&lt;/script&gt;"
            )
        , test "unescape html friendly characters"
            (\() ->
                equal (unescape "&quot;&amp;&lt;&gt;")
                    "\"&<>"
            )
        , test "fail to unescape false friendly characters"
            (\() ->
                equal (unescape "&quotamp;")
                    "&quotamp;"
            )
        , test "unescape unicode characters"
            (\() ->
                equal (unescape "&#39;&#40;&#41;")
                    "'()"
            )
        , test "unescape hexadecimal unicode characters"
            (\() ->
                equal (unescape "&#x0027;&#x0028;&#x0029;")
                    "'()"
            )
        ]
