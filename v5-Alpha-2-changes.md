# New Version of the Alpha

- `Font.weight` has been removed in favor of `Font.extraBold`, `Font.regular`, `Font.light`, etc.  All weights from 100 - 900 are represented.
- `Background.image` and `Background.fittedImage` will place a centered background image, instead of anchoring at the top left.
- `fillBetween { min : Maybe Int, max : Maybe Int}` is now present for `min/max height/width` behavior.  It works like fill, but with an optional top and lower bound.
- `transparent` - Set an element as transparent.  It will take up space, but otherwise be transparent and unclickable.
- `alpha` can now be set for an element.
- `attribute` has been renamed `htmlAttribute` to better convey what it's used for.
- `Element.Area` has been renamed `Element.Region` to avoid confusion with `WAI ARIA` stuff.
- `center` has been renamed `centerX`



# New Default Behavior

The default logic has been made more consistent and hopefully more intuitive.  

The parent will set the default alignment for the children and the children can can override this default by setting their own alignment.

Here are the new defaults. 

**el**(and all special els like `link`, `download`, etc.)

- width/height shrink
- child aligned left and top

**row**

- width fill
- height shrink
- child aligned left and centered y 


**column**

- width fill
- height fill
- children aligned left and top

**textColumn**

- `width (px 550)`  This is shooting for ~60 characters per line at 20px sized font for readability
- `height shrink`

**paragraph**

- `width fill`
- `height shrink`

Inputs will generally default to `width fill` and `height shrink`

I realize changing defaults like this can cause subtle breakage, which I'm sorry about, but it's important to get this right.  Hopefully these new defaults make sense.



# Transparency

The toggle from `inFront`, `above` and friends has been removed, though you could implement it manually if you want. 

```
inFront toggle el =
    Element.inFront
        (if toggle then el else empty)
```

This was done so to keep the primitives of visibility and layout separate.


`hidden` has been removed.  `hidden` would `display: none` on an element, though we already have a way to not render something by using `empty` and an if statement. I think empty and an if statement better convey that some html isn't being rendered.

If you need the previous behavior of `hidden`, you can always craft it yourself :D

Also, display:none isn't great for hiding something and then transitioning it in.

So, instead, we now have `transparent`, which will leave the item to take up space , but otherwise make it transparent and unclickable (like `visibility` in css land).  You can also toggle it and the element will fade in, which is nice.


# PseudoClass Support

`Element.mouseOver`, `Element.focused`, and `Element.mouseDown` are available to style `:hover`, `:focus` and `:active`.  

Only a small subset of properties are allowed here or else the compiler will give you an error.

This also introduced some new type aliases for attributes.

`Attribute msg` - What you're used to.  This **cannot** be used in a mouseOver/focused/etc.

`Attr decorative msg` - A new attribute alias for attributes that can be used as a normal attribute or in `mouseOver`, `focused`, etc.  I like to think of this as a *Decorative Attribute*.


# Input

`Input.select` has been removed.  Ultimately this came down to it being recommended against for most UX purposes. 

If you're looking for a replacement, consider any of these options which will likely create a better experience:

- Input.checkbox
- Input.radio/Input.radioRow with custom styling
- Input.text with some sort of suggestion/autocomplete attached to it.

If you still need to have a select menu, you can either:

- *Embed one* using `html` 
- [Craft one by having a hidden `radio` that is shown on focus.](https://gist.github.com/mdgriffith/b99b7ee04eaabaac042572e328a85345)  You'll have to store some state that indicates if the menu is open or not, but you'd have to do that anyway if this library was directly supporting `select`.

*Input.Notices* have been removed, which includes warnings and errors.  Accessibility is important to this library and this change is actually meant to make it easier to have good form validation feedback.

You can just use `above`/`below` when you need to show a validation message and it will be announced politely to users using screen readers.

Notices were originally annotated as errors or warnings so that `aria-invalid` could be attached.  However, it seems to me that having the changes be announced politely is better than having the screen reader just say "Yo, something's invalid".  You now have more control over the feedback!  Craft your messages well :)


Type aliases for the records used for inputs were also removed because it gives a nicer error message which references specific fields instead of the top level type alias.



# A Full Diff from the Previous Version


------ Added modules - MINOR ------

    Element.Region


------ Removed modules - MAJOR ------

    Element.Area


------ Changes to module Element - MAJOR ------

    Added:
        type alias Attr decorative msg =
            Internal.Model.Attribute decorative msg
        type alias Decoration =
            Internal.Model.Attribute Basics.Never Basics.Never
        type alias Device =
            { width : Int,
              height : Int,
              phone : Bool,
              tablet : Bool,
              desktop : Bool,
              bigDesktop : Bool,
              portrait : Bool
            }
        alpha : Float -> Element.Attr decorative msg
        centerX : Element.Attribute msg
        classifyDevice : { window |
                           height : Int, width : Int
                         } -> Element.Device
        fillBetween : { min : Maybe.Maybe Int,
                        max : Maybe.Maybe Int
                      } -> Element.Length
        fillPortionBetween : { portion : Int,
                               min : Maybe.Maybe Int,
                               max : Maybe.Maybe Int
                             } -> Element.Length
        focused : List Element.Decoration -> Element.Attribute msg
        htmlAttribute : Html.Attribute msg -> Element.Attribute msg
        modular : Float -> Float -> Int -> Float
        mouseDown : List Element.Decoration -> Element.Attribute msg
        mouseOver : List Element.Decoration -> Element.Attribute msg
        noStaticStyleSheet : Element.Option
        transparent : Bool -> Element.Attr decorative msg

    Removed:
        attribute : Html.Attribute msg -> Element.Attribute msg
        center : Element.Attribute msg
        description : String -> Element.Attribute msg
        hidden : Bool -> Element.Attribute msg
        mouseOverScale : Float -> Element.Attribute msg

    Changed:
      - type alias Attribute msg =
            Internal.Model.Attribute msg
      + type alias Attribute msg =
            Internal.Model.Attribute () msg

      - above : Bool -> Element.Element msg -> Element.Attribute msg
      + above : Element.Element msg -> Element.Attribute msg

      - behind : Bool -> Element.Element msg -> Element.Attribute msg
      + behind : Element.Element msg -> Element.Attribute msg

      - below : Bool -> Element.Element msg -> Element.Attribute msg
      + below : Element.Element msg -> Element.Attribute msg

      - inFront : Bool -> Element.Element msg -> Element.Attribute msg
      + inFront : Element.Element msg -> Element.Attribute msg

      - moveDown : Float -> Element.Attribute msg
      + moveDown : Float -> Element.Attr decorative msg

      - moveLeft : Float -> Element.Attribute msg
      + moveLeft : Float -> Element.Attr decorative msg

      - moveRight : Float -> Element.Attribute msg
      + moveRight : Float -> Element.Attr decorative msg

      - moveUp : Float -> Element.Attribute msg
      + moveUp : Float -> Element.Attr decorative msg

      - onLeft : Bool -> Element.Element msg -> Element.Attribute msg
      + onLeft : Element.Element msg -> Element.Attribute msg

      - onRight : Bool -> Element.Element msg -> Element.Attribute msg
      + onRight : Element.Element msg -> Element.Attribute msg

      - rotate : Float -> Element.Attribute msg
      + rotate : Float -> Element.Attr decorative msg

      - scale : Float -> Element.Attribute msg
      + scale : Float -> Element.Attr decorative msg



------ Changes to module Element.Background - MAJOR ------

    Removed:
        mouseOverColor : Color.Color -> Internal.Model.Attribute msg

    Changed:
      - color : Color.Color -> Internal.Model.Attribute msg
      + color : Color.Color -> Element.Attr decorative msg

      - fittedImage : String -> Internal.Model.Attribute msg
      + fittedImage : String -> Element.Attribute msg

      - gradient : Float -> List Color.Color -> Internal.Model.Attribute msg
      + gradient : Float -> List Color.Color -> Element.Attribute msg

      - image : String -> Internal.Model.Attribute msg
      + image : String -> Element.Attribute msg

      - tiled : String -> Internal.Model.Attribute msg
      + tiled : String -> Element.Attribute msg

      - tiledX : String -> Internal.Model.Attribute msg
      + tiledX : String -> Element.Attribute msg

      - tiledY : String -> Internal.Model.Attribute msg
      + tiledY : String -> Element.Attribute msg



------ Changes to module Element.Border - MAJOR ------

    Removed:
        mouseOverColor : Color.Color -> Internal.Model.Attribute msg

    Changed:
      - color : Color.Color -> Internal.Model.Attribute msg
      + color : Color.Color -> Element.Attr decorative msg

      - dashed : Internal.Model.Attribute msg
      + dashed : Element.Attribute msg

      - dotted : Internal.Model.Attribute msg
      + dotted : Element.Attribute msg

      - glow : Color.Color -> Float -> Internal.Model.Attribute msg
      + glow : Color.Color -> Float -> Element.Attr decorative msg

      - innerGlow : Color.Color -> Float -> Internal.Model.Attribute msg
      + innerGlow : Color.Color -> Float -> Element.Attr decorative msg

      - innerShadow : { offset : (Float, Float),
                        size : Float,
                        blur : Float,
                        color : Color.Color
                      } -> Internal.Model.Attribute msg
      + innerShadow : { offset : (Float, Float),
                        size : Float,
                        blur : Float,
                        color : Color.Color
                      } -> Element.Attr decorative msg

      - roundEach : { topLeft : Int,
                      topRight : Int,
                      bottomLeft : Int,
                      bottomRight : Int
                    } -> Internal.Model.Attribute msg
      + roundEach : { topLeft : Int,
                      topRight : Int,
                      bottomLeft : Int,
                      bottomRight : Int
                    } -> Element.Attribute msg

      - rounded : Int -> Internal.Model.Attribute msg
      + rounded : Int -> Element.Attribute msg

      - shadow : { offset : (Float, Float),
                   blur : Float,
                   size : Float,
                   color : Color.Color
                 } -> Internal.Model.Attribute msg
      + shadow : { offset : (Float, Float),
                   blur : Float,
                   size : Float,
                   color : Color.Color
                 } -> Element.Attr decorative msg

      - solid : Internal.Model.Attribute msg
      + solid : Element.Attribute msg

      - width : Int -> Internal.Model.Attribute msg
      + width : Int -> Element.Attribute msg

      - widthEach : { bottom : Int,
                      left : Int,
                      right : Int,
                      top : Int
                    } -> Internal.Model.Attribute msg
      + widthEach : { bottom : Int,
                      left : Int,
                      right : Int,
                      top : Int
                    } -> Element.Attribute msg

      - widthXY : Int -> Int -> Internal.Model.Attribute msg
      + widthXY : Int -> Int -> Element.Attribute msg



------ Changes to module Element.Font - MAJOR ------

    Added:
        extraBold : Element.Attribute msg
        extraLight : Element.Attribute msg
        hairline : Element.Attribute msg
        heavy : Element.Attribute msg
        medium : Element.Attribute msg
        regular : Element.Attribute msg
        semiBold : Element.Attribute msg
        unitalicized : Element.Attribute msg

    Removed:
        mouseOverColor : Color.Color -> Internal.Model.Attribute msg
        weight : Int -> Internal.Model.Attribute msg

    Changed:
      - alignLeft : Internal.Model.Attribute msg
      + alignLeft : Element.Attribute msg

      - alignRight : Internal.Model.Attribute msg
      + alignRight : Element.Attribute msg

      - bold : Internal.Model.Attribute msg
      + bold : Element.Attribute msg

      - center : Internal.Model.Attribute msg
      + center : Element.Attribute msg

      - color : Color.Color -> Internal.Model.Attribute msg
      + color : Color.Color -> Element.Attr decorative msg

      - family : List Element.Font.Font -> Internal.Model.Attribute msg
      + family : List Element.Font.Font -> Element.Attribute msg

      - glow : Color.Color -> Float -> Internal.Model.Attribute msg
      + glow : Color.Color -> Float -> Element.Attr decorative msg

      - italic : Internal.Model.Attribute msg
      + italic : Element.Attribute msg

      - justify : Internal.Model.Attribute msg
      + justify : Element.Attribute msg

      - letterSpacing : Float -> Internal.Model.Attribute msg
      + letterSpacing : Float -> Element.Attribute msg

      - light : Internal.Model.Attribute msg
      + light : Element.Attribute msg

      - lineHeight : Float -> Internal.Model.Attribute msg
      + lineHeight : Float -> Element.Attr decorative msg

      - shadow : { offset : (Float, Float),
                   blur : Float,
                   color : Color.Color
                 } -> Internal.Model.Attribute msg
      + shadow : { offset : (Float, Float),
                   blur : Float,
                   color : Color.Color
                 } -> Element.Attr decorative msg

      - size : Int -> Internal.Model.Attribute msg
      + size : Int -> Element.Attr decorative msg

      - strike : Internal.Model.Attribute msg
      + strike : Element.Attribute msg

      - underline : Internal.Model.Attribute msg
      + underline : Element.Attribute msg

      - wordSpacing : Float -> Internal.Model.Attribute msg
      + wordSpacing : Float -> Element.Attribute msg



------ Changes to module Element.Input - MAJOR ------

    Added:
        focusedOnLoad : Element.Attribute msg
        optionWith : value -> (Element.Input.OptionState -> Element.Element msg) -> Element.Input.Option value msg
        spellChecked : List (Element.Attribute msg) -> { onChange : Maybe.Maybe (String -> msg),
                                                         text : String,
                                                         placeholder : Maybe.Maybe (Element.Input.Placeholder msg),
                                                         label : Element.Input.Label msg
                                                       } -> Element.Element msg

    Removed:
        type Menu option msg
        type Notice msg
        type alias Button msg =
            { onPress : Maybe.Maybe msg,
              label : Element.Element msg
            }
        type alias Checkbox msg =
            { onChange : Maybe.Maybe (Bool -> msg),
              icon : Maybe.Maybe (Element.Element msg),
              checked : Bool,
              label : Element.Input.Label msg,
              notice : Maybe.Maybe (Element.Input.Notice msg)
            }
        type alias Select option msg =
            { onChange : Maybe.Maybe (option -> msg),
              selected : Maybe.Maybe option,
              menu : Element.Input.Menu option msg,
              placeholder : Maybe.Maybe (Element.Element msg),
              label : Element.Input.Label msg,
              notice : Maybe.Maybe (Element.Input.Notice msg)
            }
        type alias Text msg =
            { onChange : Maybe.Maybe (String -> msg),
              text : String,
              placeholder : Maybe.Maybe (Element.Input.Placeholder msg),
              label : Element.Input.Label msg,
              notice : Maybe.Maybe (Element.Input.Notice msg)
            }
        errorAbove : List (Element.Attribute msg) -> Element.Element msg -> Element.Input.Notice msg
        errorBelow : List (Element.Attribute msg) -> Element.Element msg -> Element.Input.Notice msg
        errorLeft : List (Element.Attribute msg) -> Element.Element msg -> Element.Input.Notice msg
        errorRight : List (Element.Attribute msg) -> Element.Element msg -> Element.Input.Notice msg
        menuAbove : List (Element.Attribute msg) -> List (Element.Input.Option option msg) -> Element.Input.Menu option msg
        menuBelow : List (Element.Attribute msg) -> List (Element.Input.Option option msg) -> Element.Input.Menu option msg
        select : List (Element.Attribute msg) -> Element.Input.Select option msg -> Element.Element msg
        spellcheckedMultiline : List (Element.Attribute msg) -> Element.Input.Text msg -> Element.Element msg
        warningAbove : List (Element.Attribute msg) -> Element.Element msg -> Element.Input.Notice msg
        warningBelow : List (Element.Attribute msg) -> Element.Element msg -> Element.Input.Notice msg
        warningLeft : List (Element.Attribute msg) -> Element.Element msg -> Element.Input.Notice msg
        warningRight : List (Element.Attribute msg) -> Element.Element msg -> Element.Input.Notice msg

    Changed:
      - type alias Radio option msg =
            { onChange : Maybe.Maybe (option -> msg),
              options : List (Element.Input.Option option msg),
              selected : Maybe.Maybe option,
              label : Element.Input.Label msg,
              notice : Maybe.Maybe (Element.Input.Notice msg)
            }
      + type alias Radio option msg =
            { onChange : Maybe.Maybe (option -> msg),
              options : List (Element.Input.Option option msg),
              selected : Maybe.Maybe option,
              label : Element.Input.Label msg
            }

      - button : List (Element.Attribute msg) -> Element.Input.Button msg -> Element.Element msg
      + button : List (Element.Attribute msg) -> { onPress : Maybe.Maybe msg,
                                                   label : Element.Element msg
                                                 } -> Element.Element msg

      - checkbox : List (Element.Attribute msg) -> Element.Input.Checkbox msg -> Element.Element msg
      + checkbox : List (Element.Attribute msg) -> { onChange : Maybe.Maybe (Bool -> msg),
                                                     icon : Maybe.Maybe (Element.Element msg),
                                                     checked : Bool,
                                                     label : Element.Input.Label msg
                                                   } -> Element.Element msg

      - currentPassword : List (Element.Attribute msg) -> Element.Input.Text msg -> Element.Element msg
      + currentPassword : List (Element.Attribute msg) -> { onChange : Maybe.Maybe (String -> msg),
                                                            text : String,
                                                            placeholder : Maybe.Maybe (Element.Input.Placeholder msg),
                                                            label : Element.Input.Label msg,
                                                            show : Bool
                                                          } -> Element.Element msg

      - email : List (Element.Attribute msg) -> Element.Input.Text msg -> Element.Element msg
      + email : List (Element.Attribute msg) -> { onChange : Maybe.Maybe (String -> msg),
                                                  text : String,
                                                  placeholder : Maybe.Maybe (Element.Input.Placeholder msg),
                                                  label : Element.Input.Label msg
                                                } -> Element.Element msg

      - multiline : List (Element.Attribute msg) -> Element.Input.Text msg -> Element.Element msg
      + multiline : List (Element.Attribute msg) -> { onChange : Maybe.Maybe (String -> msg),
                                                      text : String,
                                                      placeholder : Maybe.Maybe (Element.Input.Placeholder msg),
                                                      label : Element.Input.Label msg,
                                                      spellcheck : Bool
                                                    } -> Element.Element msg

      - newPassword : List (Element.Attribute msg) -> Element.Input.Text msg -> Element.Element msg
      + newPassword : List (Element.Attribute msg) -> { onChange : Maybe.Maybe (String -> msg),
                                                        text : String,
                                                        placeholder : Maybe.Maybe (Element.Input.Placeholder msg),
                                                        label : Element.Input.Label msg,
                                                        show : Bool
                                                      } -> Element.Element msg

      - search : List (Element.Attribute msg) -> Element.Input.Text msg -> Element.Element msg
      + search : List (Element.Attribute msg) -> { onChange : Maybe.Maybe (String -> msg),
                                                   text : String,
                                                   placeholder : Maybe.Maybe (Element.Input.Placeholder msg),
                                                   label : Element.Input.Label msg
                                                 } -> Element.Element msg

      - text : List (Element.Attribute msg) -> Element.Input.Text msg -> Element.Element msg
      + text : List (Element.Attribute msg) -> { onChange : Maybe.Maybe (String -> msg),
                                                 text : String,
                                                 placeholder : Maybe.Maybe (Element.Input.Placeholder msg),
                                                 label : Element.Input.Label msg
                                               } -> Element.Element msg

      - username : List (Element.Attribute msg) -> Element.Input.Text msg -> Element.Element msg
      + username : List (Element.Attribute msg) -> { onChange : Maybe.Maybe (String -> msg),
                                                     text : String,
                                                     placeholder : Maybe.Maybe (Element.Input.Placeholder msg),
                                                     label : Element.Input.Label msg
                                                   } -> Element.Element msg
