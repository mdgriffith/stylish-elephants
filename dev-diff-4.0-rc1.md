This is a MAJOR change.

------ Added modules - MINOR ------

    Element.Input
    Element.Location


------ Changes to module Element - MAJOR ------

    Added:
        type alias GridPosition =
            { start : (Int, Int), width : Int, height : Int }
        type alias NamedGridPosition =
            Style.Internal.Model.NamedGridPosition
        cell : Element.GridPosition -> Element.Element style variation msg -> Element.OnGrid (Element.Element style variation msg)
        decorativeImage : style -> List (Element.Attribute variation msg) -> { src : String
                                                                             } -> Element.Element style variation msg
        download : String -> Element.Element style variation msg -> Element.Element style variation msg
        downloadAs : { src : String,
                       filename : String
                     } -> Element.Element style variation msg -> Element.Element style variation msg
        h1 : style -> List (Element.Attribute variation msg) -> Element.Element style variation msg -> Element.Element style variation msg
        h2 : style -> List (Element.Attribute variation msg) -> Element.Element style variation msg -> Element.Element style variation msg
        h3 : style -> List (Element.Attribute variation msg) -> Element.Element style variation msg -> Element.Element style variation msg
        h4 : style -> List (Element.Attribute variation msg) -> Element.Element style variation msg -> Element.Element style variation msg
        h5 : style -> List (Element.Attribute variation msg) -> Element.Element style variation msg -> Element.Element style variation msg
        h6 : style -> List (Element.Attribute variation msg) -> Element.Element style variation msg -> Element.Element style variation msg
        newTab : String -> Element.Element style variation msg -> Element.Element style variation msg
        subheading : style -> List (Element.Attribute variation msg) -> String -> Element.Element style variation msg

    Removed:
        type Option style variation msg
        area : Element.GridPosition -> Element.Element style variation msg -> Element.OnGrid (Element.Element style variation msg)
        audio : Element.Element style variation msg -> Element.Element style variation msg
        break : Element.Element style variation msg
        canvas : Element.Element style variation msg -> Element.Element style variation msg
        checkbox : Bool -> style -> List (Element.Attribute variation msg) -> Element.Element style variation msg -> Element.Element style variation msg
        embed : Style.StyleSheet style variation -> Html.Html msg
        form : Element.Element style variation msg -> Element.Element style variation msg
        header : Element.Element style variation msg -> Element.Element style variation msg
        iframe : Element.Element style variation msg -> Element.Element style variation msg
        inputText : style -> List (Element.Attribute variation msg) -> String -> Element.Element style variation msg
        label : style -> List (Element.Attribute variation msg) -> Element.Element style variation msg -> Element.Element style variation msg -> Element.Element style variation msg
        labelBelow : style -> List (Element.Attribute variation msg) -> Element.Element style variation msg -> Element.Element style variation msg -> Element.Element style variation msg
        nav : Element.Element style variation msg -> Element.Element style variation msg
        option : String -> Bool -> Element.Element style variation msg -> Element.Option style variation msg
        radio : String -> style -> List (Element.Attribute variation msg) -> List (Element.Option style variation msg) -> Element.Element style variation msg
        render : Style.StyleSheet style variation -> Element.Element style variation msg -> Html.Html msg
        root : Style.StyleSheet style variation -> Element.Element style variation msg -> Html.Html msg
        select : String -> style -> List (Element.Attribute variation msg) -> List (Element.Option style variation msg) -> Element.Element style variation msg
        textArea : style -> List (Element.Attribute variation msg) -> String -> Element.Element style variation msg
        video : Element.Element style variation msg -> Element.Element style variation msg

    Changed:
      - type alias Grid =
            { rows : List Style.Internal.Model.Length,
              columns : List Style.Internal.Model.Length
            }
      + type alias Grid style variation msg =
            { rows : List Element.Attributes.Length,
              columns : List Element.Attributes.Length,
              cells : List (Element.OnGrid (Element.Element style variation msg))
            }

      - type alias NamedGrid =
            { rows : List (Style.Internal.Model.Length, List Style.Internal.Model.NamedGridPosition),
              columns : List Style.Internal.Model.Length
            }
      + type alias NamedGrid style variation msg =
            { rows : List (Element.Attributes.Length, List Style.Internal.Model.NamedGridPosition),
              columns : List Element.Attributes.Length,
              cells : List (Element.NamedOnGrid (Element.Element style variation msg))
            }

      - article : Element.Element style variation msg -> Element.Element style variation msg
      + article : style -> List (Element.Attribute variation msg) -> Element.Element style variation msg -> Element.Element style variation msg

      - aside : Element.Element style variation msg -> Element.Element style variation msg
      + aside : style -> List (Element.Attribute variation msg) -> Element.Element style variation msg -> Element.Element style variation msg

      - button : Element.Element style variation msg -> Element.Element style variation msg
      + button : style -> List (Element.Attribute variation msg) -> Element.Element style variation msg -> Element.Element style variation msg

      - grid : style -> Element.Grid -> List (Element.Attribute variation msg) -> List (Element.OnGrid (Element.Element style variation msg)) -> Element.Element style variation msg
      + grid : style -> List (Element.Attribute variation msg) -> Element.Grid style variation msg -> Element.Element style variation msg

      - image : String -> style -> List (Element.Attribute variation msg) -> Element.Element style variation msg -> Element.Element style variation msg
      + image : style -> List (Element.Attribute variation msg) -> { src : String,
                                                                     caption : String
                                                                   } -> Element.Element style variation msg

      - namedGrid : style -> Element.NamedGrid -> List (Element.Attribute variation msg) -> List (Element.NamedOnGrid (Element.Element style variation msg)) -> Element.Element style variation msg
      + namedGrid : style -> List (Element.Attribute variation msg) -> Element.NamedGrid style variation msg -> Element.Element style variation msg

      - section : Element.Element style variation msg -> Element.Element style variation msg
      + section : style -> List (Element.Attribute variation msg) -> Element.Element style variation msg -> Element.Element style variation msg



------ Changes to module Element.Attributes - MAJOR ------

    Added:
        type alias Length =
            Style.Internal.Model.Length
        fillPortion : Int -> Element.Attributes.Length
        spread : Element.Internal.Model.Attribute variation msg
        verticalSpread : Element.Internal.Model.Attribute variation msg

    Removed:
        accept : String -> Element.Internal.Model.Attribute variation msg
        acceptCharset : String -> Element.Internal.Model.Attribute variation msg
        accesskey : Char -> Element.Internal.Model.Attribute variation msg
        action : String -> Element.Internal.Model.Attribute variation msg
        align : String -> Element.Internal.Model.Attribute variation msg
        alt : String -> Element.Internal.Model.Attribute variation msg
        async : Bool -> Element.Internal.Model.Attribute variation msg
        autocomplete : Bool -> Element.Internal.Model.Attribute variation msg
        autofocus : Bool -> Element.Internal.Model.Attribute variation msg
        autoplay : Bool -> Element.Internal.Model.Attribute variation msg
        challenge : String -> Element.Internal.Model.Attribute variation msg
        charset : String -> Element.Internal.Model.Attribute variation msg
        checked : Bool -> Element.Internal.Model.Attribute variation msg
        cite : String -> Element.Internal.Model.Attribute variation msg
        cols : Int -> Element.Internal.Model.Attribute variation msg
        colspan : Int -> Element.Internal.Model.Attribute variation msg
        contenteditable : Bool -> Element.Internal.Model.Attribute variation msg
        contextmenu : String -> Element.Internal.Model.Attribute variation msg
        controls : Bool -> Element.Internal.Model.Attribute variation msg
        coords : String -> Element.Internal.Model.Attribute variation msg
        datetime : String -> Element.Internal.Model.Attribute variation msg
        default : Bool -> Element.Internal.Model.Attribute variation msg
        defaultValue : String -> Element.Internal.Model.Attribute variation msg
        defer : Bool -> Element.Internal.Model.Attribute variation msg
        dir : String -> Element.Internal.Model.Attribute variation msg
        disabled : Bool -> Element.Internal.Model.Attribute variation msg
        download : Bool -> Element.Internal.Model.Attribute variation msg
        downloadAs : String -> Element.Internal.Model.Attribute variation msg
        draggable : String -> Element.Internal.Model.Attribute variation msg
        dropzone : String -> Element.Internal.Model.Attribute variation msg
        enctype : String -> Element.Internal.Model.Attribute variation msg
        for : String -> Element.Internal.Model.Attribute variation msg
        form : String -> Element.Internal.Model.Attribute variation msg
        formaction : String -> Element.Internal.Model.Attribute variation msg
        headers : String -> Element.Internal.Model.Attribute variation msg
        href : String -> Element.Internal.Model.Attribute variation msg
        hreflang : String -> Element.Internal.Model.Attribute variation msg
        httpEquiv : String -> Element.Internal.Model.Attribute variation msg
        ismap : Bool -> Element.Internal.Model.Attribute variation msg
        itemprop : String -> Element.Internal.Model.Attribute variation msg
        justify : Element.Internal.Model.Attribute variation msg
        keytype : String -> Element.Internal.Model.Attribute variation msg
        kind : String -> Element.Internal.Model.Attribute variation msg
        lang : String -> Element.Internal.Model.Attribute variation msg
        language : String -> Element.Internal.Model.Attribute variation msg
        list : String -> Element.Internal.Model.Attribute variation msg
        loop : Bool -> Element.Internal.Model.Attribute variation msg
        manifest : String -> Element.Internal.Model.Attribute variation msg
        max : String -> Element.Internal.Model.Attribute variation msg
        maxlength : Int -> Element.Internal.Model.Attribute variation msg
        media : String -> Element.Internal.Model.Attribute variation msg
        method : String -> Element.Internal.Model.Attribute variation msg
        min : String -> Element.Internal.Model.Attribute variation msg
        minlength : Int -> Element.Internal.Model.Attribute variation msg
        moveX : Float -> Element.Internal.Model.Attribute variation msg
        moveXY : Float -> Float -> Element.Internal.Model.Attribute variation msg
        moveY : Float -> Element.Internal.Model.Attribute variation msg
        multiple : Bool -> Element.Internal.Model.Attribute variation msg
        name : String -> Element.Internal.Model.Attribute variation msg
        novalidate : Bool -> Element.Internal.Model.Attribute variation msg
        pattern : String -> Element.Internal.Model.Attribute variation msg
        ping : String -> Element.Internal.Model.Attribute variation msg
        placeholder : String -> Element.Internal.Model.Attribute variation msg
        poster : String -> Element.Internal.Model.Attribute variation msg
        preload : String -> Element.Internal.Model.Attribute variation msg
        pubdate : String -> Element.Internal.Model.Attribute variation msg
        readonly : Bool -> Element.Internal.Model.Attribute variation msg
        rel : String -> Element.Internal.Model.Attribute variation msg
        required : Bool -> Element.Internal.Model.Attribute variation msg
        reversed : Bool -> Element.Internal.Model.Attribute variation msg
        rows : Int -> Element.Internal.Model.Attribute variation msg
        rowspan : Int -> Element.Internal.Model.Attribute variation msg
        sandbox : String -> Element.Internal.Model.Attribute variation msg
        scope : String -> Element.Internal.Model.Attribute variation msg
        scoped : Bool -> Element.Internal.Model.Attribute variation msg
        seamless : Bool -> Element.Internal.Model.Attribute variation msg
        selected : Bool -> Element.Internal.Model.Attribute variation msg
        shape : String -> Element.Internal.Model.Attribute variation msg
        size : Int -> Element.Internal.Model.Attribute variation msg
        spellcheck : Bool -> Element.Internal.Model.Attribute variation msg
        src : String -> Element.Internal.Model.Attribute variation msg
        srcdoc : String -> Element.Internal.Model.Attribute variation msg
        srclang : String -> Element.Internal.Model.Attribute variation msg
        start : Int -> Element.Internal.Model.Attribute variation msg
        step : String -> Element.Internal.Model.Attribute variation msg
        tabindex : Int -> Element.Internal.Model.Attribute variation msg
        target : String -> Element.Internal.Model.Attribute variation msg
        title : String -> Element.Internal.Model.Attribute variation msg
        type_ : String -> Element.Internal.Model.Attribute variation msg
        usemap : String -> Element.Internal.Model.Attribute variation msg
        value : String -> Element.Internal.Model.Attribute variation msg
        wrap : String -> Element.Internal.Model.Attribute variation msg

    Changed:
      - content : String -> Element.Internal.Model.Attribute variation msg
      + content : Element.Attributes.Length

      - fill : Float -> Style.Internal.Model.Length
      + fill : Element.Attributes.Length

      - height : Style.Internal.Model.Length -> Element.Internal.Model.Attribute variation msg
      + height : Element.Attributes.Length -> Element.Internal.Model.Attribute variation msg

      - maxHeight : Style.Internal.Model.Length -> Element.Internal.Model.Attribute variation msg
      + maxHeight : Element.Attributes.Length -> Element.Internal.Model.Attribute variation msg

      - maxWidth : Style.Internal.Model.Length -> Element.Internal.Model.Attribute variation msg
      + maxWidth : Element.Attributes.Length -> Element.Internal.Model.Attribute variation msg

      - minHeight : Style.Internal.Model.Length -> Element.Internal.Model.Attribute variation msg
      + minHeight : Element.Attributes.Length -> Element.Internal.Model.Attribute variation msg

      - minWidth : Style.Internal.Model.Length -> Element.Internal.Model.Attribute variation msg
      + minWidth : Element.Attributes.Length -> Element.Internal.Model.Attribute variation msg

      - percent : Float -> Style.Internal.Model.Length
      + percent : Float -> Element.Attributes.Length

      - px : Float -> Style.Internal.Model.Length
      + px : Float -> Element.Attributes.Length

      - width : Style.Internal.Model.Length -> Element.Internal.Model.Attribute variation msg
      + width : Element.Attributes.Length -> Element.Internal.Model.Attribute variation msg



------ Changes to module Element.Keyed - MAJOR ------

    Added:
        type alias Grid style variation msg =
            { rows : List Style.Internal.Model.Length,
              columns : List Style.Internal.Model.Length,
              cells : List (Element.OnGrid (String, Element.Element style variation msg))
            }
        type alias NamedGrid style variation msg =
            { rows : List (Style.Internal.Model.Length, List Style.Internal.Model.NamedGridPosition),
              columns : List Style.Internal.Model.Length,
              cells : List (Element.NamedOnGrid (String, Element.Element style variation msg))
            }
        cell : Element.GridPosition -> (String, Element.Element style variation msg) -> Element.OnGrid (String, Element.Element style variation msg)
        named : String -> Element.Element style variation msg -> Element.NamedOnGrid (String, Element.Element style variation msg)

    Changed:
      - grid : style -> Element.Grid -> List (Element.Attribute variation msg) -> List (Element.OnGrid (String, Element.Element style variation msg)) -> Element.Element style variation msg
      + grid : style -> List (Element.Attribute variation msg) -> Element.Keyed.Grid style variation msg -> Element.Element style variation msg

      - namedGrid : style -> Element.NamedGrid -> List (Element.Attribute variation msg) -> List (Element.NamedOnGrid (String, Element.Element style variation msg)) -> Element.Element style variation msg
      + namedGrid : style -> List (Element.Attribute variation msg) -> Element.Keyed.NamedGrid style variation msg -> Element.Element style variation msg



------ Changes to module Style - MAJOR ------

    Added:
        type alias Font = Style.Internal.Model.Font
        type alias Transform =
            Style.Internal.Model.Transformation

    Removed:
        type alias Filter =
            Style.Internal.Model.Filter
        type alias Shadow =
            Style.Internal.Model.ShadowModel
        filters : List Style.Filter -> Style.Property class variation
        paddingBottomHint : Float -> Style.Property class variation
        paddingHint : Float -> Style.Property class variation
        paddingLeftHint : Float -> Style.Property class variation
        paddingRightHint : Float -> Style.Property class variation
        paddingTopHint : Float -> Style.Property class variation
        shadows : List Style.Shadow -> Style.Property class variation
        stylesheet : List (Style.Style elem variation) -> Style.StyleSheet elem variation
        stylesheetWith : List Style.Option -> List (Style.Style elem variation) -> Style.StyleSheet elem variation


------ Changes to module Style.Background - MAJOR ------

    Added:
        contain : Style.Background.Size
        cover : Style.Background.Size
        coverImage : String -> Style.Property class variation
        height : Style.Internal.Model.Length -> Style.Background.Size
        natural : Style.Background.Size
        size : { height : Style.Internal.Model.Length,
                 width : Style.Internal.Model.Length
               } -> Style.Background.Size
        stretch : Style.Background.Repeat
        width : Style.Internal.Model.Length -> Style.Background.Size

    Removed:
        round : Style.Background.Repeat

    Changed:
      - imageWith : { src : String,
                      position : (Float, Float),
                      repeat : Style.Background.Repeat
                    } -> Style.Property class variation
      + imageWith : { src : String,
                      position : (Float, Float),
                      repeat : Style.Background.Repeat,
                      size : Style.Background.Size
                    } -> Style.Property class variation



------ Changes to module Style.Filter - MAJOR ------

    Changed:
      - blur : Float -> Style.Filter
      + blur : Float -> Style.Property class variation

      - brightness : Float -> Style.Filter
      + brightness : Float -> Style.Property class variation

      - contrast : Float -> Style.Filter
      + contrast : Float -> Style.Property class variation

      - grayscale : Float -> Style.Filter
      + grayscale : Float -> Style.Property class variation

      - hueRotate : Float -> Style.Filter
      + hueRotate : Float -> Style.Property class variation

      - invert : Float -> Style.Filter
      + invert : Float -> Style.Property class variation

      - opacity : Float -> Style.Filter
      + opacity : Float -> Style.Property class variation

      - saturate : Float -> Style.Filter
      + saturate : Float -> Style.Property class variation

      - sepia : Float -> Style.Filter
      + sepia : Float -> Style.Property class variation

      - url : String -> Style.Filter
      + url : String -> Style.Property class variation



------ Changes to module Style.Font - MAJOR ------

    Added:
        cursive : Style.Font
        fantasy : Style.Font
        font : String -> Style.Font
        importUrl : { url : String, name : String
                    } -> Style.Font
        monospace : Style.Font
        sansSerif : Style.Font
        serif : Style.Font

    Removed:
        noWrap : Style.Property class variation
        pre : Style.Property class variation
        preLine : Style.Property class variation
        preWrap : Style.Property class variation
        wrap : Style.Property class variation

    Changed:
      - typeface : List String -> Style.Property class variation
      + typeface : List Style.Font -> Style.Property class variation



------ Changes to module Style.Shadow - MAJOR ------

    Changed:
      - box : { offset : (Float, Float),
                size : Float,
                blur : Float,
                color : Color.Color
              } -> Style.Shadow
      + box : { offset : (Float, Float),
                size : Float,
                blur : Float,
                color : Color.Color
              } -> Style.Property class variation

      - drop : { offset : (Float, Float),
                 blur : Float,
                 color : Color.Color
               } -> Style.Shadow
      + drop : { offset : (Float, Float),
                 blur : Float,
                 color : Color.Color
               } -> Style.Property class variation

      - inset : { offset : (Float, Float),
                  size : Float,
                  blur : Float,
                  color : Color.Color
                } -> Style.Shadow
      + inset : { offset : (Float, Float),
                  size : Float,
                  blur : Float,
                  color : Color.Color
                } -> Style.Property class variation

      - text : { offset : (Float, Float),
                 blur : Float,
                 color : Color.Color
               } -> Style.Shadow
      + text : { offset : (Float, Float),
                 blur : Float,
                 color : Color.Color
               } -> Style.Property class variation

