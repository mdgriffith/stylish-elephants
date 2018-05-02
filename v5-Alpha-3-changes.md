# New Testing Capabilities

A test suite of ~1.6k layout tests was written(whew!).  All of these tests pass on Chrome, Firefox, Safari, Edge, and IE11.

# Overview of other changes

- `Font.lineHeight` has been removed.  Instead, `spacing` now works on paragraphs.
- `Element.empty` has been renamed `Element.none` to be more consistent with other elm libraries.
- `Device` no longer includes `window.width` and `window.height`.  Previously every view function that depends on `device` was forced to rerender when the window was resized, which meant you couldn't take advantage of lazy.  If you do need the window coordinates you can save them separately.
- *Fewer nodes rendered* - So, things should be faster!
- `fillBetween` has been replaced by `Element.minimum` and `Element.maximum`.

So now you can do things like

```elm
view =
    el 
        [ width 
            (fill
                |> minimum 20
                |> maximum 200
            )
        ]
        (text "woohoo, I have a min and max")

```


## Full Diff

```
This is a MAJOR change.

------ Changes to module Element - MAJOR ------

    Added:
        maximum : Int -> Element.Length -> Element.Length
        minimum : Int -> Element.Length -> Element.Length
        none : Element.Element msg

    Removed:
        empty : Element.Element msg
        fillBetween : { min : Maybe.Maybe Int,
                        max : Maybe.Maybe Int
                      } -> Element.Length
        fillPortionBetween : { portion : Int,
                               min : Maybe.Maybe Int,
                               max : Maybe.Maybe Int
                             } -> Element.Length

    Changed:
      - type alias Device =
            { width : Int,
              height : Int,
              phone : Bool,
              tablet : Bool,
              desktop : Bool,
              bigDesktop : Bool,
              portrait : Bool
            }
      + type alias Device =
            { phone : Bool,
              tablet : Bool,
              desktop : Bool,
              bigDesktop : Bool,
              portrait : Bool
            }



------ Changes to module Element.Background - MAJOR ------

    Added:
        uncropped : String -> Element.Attribute msg

    Removed:
        fittedImage : String -> Element.Attribute msg


------ Changes to module Element.Font - MAJOR ------

    Removed:
        lineHeight : Float -> Element.Attr decorative msg


------ Changes to module Element.Input - MINOR ------

    Added:
        type OptionState = Focused | Idle | Selected

```