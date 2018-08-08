# A New Languge for Layout and Interface

CSS and HTML are actually quite difficult to use when you're trying to do the layout and styling of a web page.

This library is a complete alternative to HTML and CSS.  Basically you can just write your app using this library and (mostly) never have to think about HTML and CSS again.

The high level goal of this library is to be a **design toolkit** that draws inspiration from the domains of design, layout, and typography, as opposed to drawing from the ideas as implemented in CSS and HTML.

This means:

* Making writing layout and designing your `view` as **simple and fun** as possible.
* Making many layout errors (like you'd run into using CSS) **just not possible to write** in the first place!
* **Run Fast.**
* **Make designs explicit and easy to modify.**  CSS and HTML as tools for a layout language are hard to modify because there's no central place that represents your layout.  You're generally forced to bounce back and forth between multiple definitions in multiple files in order to adjust layout, even though it's probably the most common thing you'll do.


```elm
import Color exposing (blue, darkBlue)
import Element exposing (Element, el, text, row, alignRight)
import Element.Background as Background
import Element.Border as Border


main = 
    Element.layout []
        myElement


myRowOfStuff =
    row [ width fill ]
        [ myElement
        , myElement
        , el [ alignRight ] myElement
        ]


myElement : Element msg
myElement =
    el
        [ Background.color blue
        , Border.color darkBlue
        ]
        (text "You've made a stylish element!")
```

The work here started as a major rewrite of the [Style Elements](https://github.com/mdgriffith/style-elements).

Though, it will be released under a different name shortly.



