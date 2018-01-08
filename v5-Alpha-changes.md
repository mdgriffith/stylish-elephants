# Version 5 Alpha

This was a MAJOR rewrite of the internal html generation, which is one of the reasons it's taken so long to get to the alpha stage :)

* **Major Performance improvement** - Style Elements v5 is much faster than v4 due to a better rendering strategy and generating very minimal html. The rewritten architecture also allows me to explore a few other optimizations, so things may get even faster than they are now.

* **Lazy is here!** - It works with no weird caveats.

## No Stylesheet

You now define styles on the element itself.

```
el [ Background.color blue, Font.color white ] (text "I'm so stylish!")
```

These styles are gathered and rendered into a `stylesheet`.  This has a few advantages:

1. Much faster than rendering as actual inline styles
2. More expressive power by allowing style-elements compile to pseudoclasses, css animations and the like.
3. Defining styles like this is a really nice workflow.

## Reorganization

* No `Style` modules anymore, it's all under `Element`.
* `Style.Color` and `Style.Shadow` have been merged into `Element.Font`, `Element.Border`, and `Element.background`.  So things like `Font.color` and `Border.glow` now exist.
* No more `Element.Attributes`, everything has been either moved to `Element` or to a more appropriate module.


## Wait isn't Style Elements about separation of layout and style?!

It was!  So why is there only `Element` and no `StyleSheet` now?

The key insight is that it's not so much the separation of layout and style that is important as it is that _properties affecting layout should all be in the view function_.  The main thing is _not_ having layout specified through several layers of indirection, but having everything explict and in one place.

The new version moves to a more refined version of this idea: **Everything should be explicit and in your view!**


## Style Organization

Your next question might be "if we don't have a stylesheet, how to we capture our style logic in a nice way?"

The main thing I've found is that stylesheets in general seem to be pretty hard to maintain.  Even well-typed stylesheets that only allow single classes!  You have to manage names for everything, and in the case of large style refactors it's not obvious that style classes would be organized the same way.

So, I'm not sure that stylesheets or something like stylesheets are the way to go.

My current thinking is that you have 2 really powerful methods of capturing your styling logic.

1. **Just put it in a view function.** If you have a few button variations you want, just create a function for each variation.  You probably don't need a huge number of variations.  You don't need to think of it like recreating bootstrap in style-elements.

2. **Capture your colors, font sizes, etc.**  Create a `Style` module that captures the **values** you use for your styling. Keep your colors there, as well as values for spacing, font names, and font sizes all in one place.  You should consider using a scaling function for things like spacing and fontsizes. 


## Element.Area

The `Element.Area` module is now how you can do accessibility markup.

You can do this by adding an area notation to an element's attributes.

```elm
import Element.Area as Area

row [ Area.navigation ] 
    [ --..my navigation links
    ]

```
Or you can make something an `<h1>`

```
el [ Area.heading 1 ] (text "Super important stuff")

```

This means your accessibility markup is separate from your layout markup, which turns out to be really nice.



## Alignment

Alignment got a _bunch_ of attention to make it more powerful and intuitive.

* _Alignment_ now applies to the element it's attached to, so Alignment on `row` and `column` does not apply to the children but to the `row` or `column` itself. 

* _It works everywhere!_  Previously, if you set a child as `alignLeft` in a row, nothing would happen.  The main weirdness that had to be resolved is what happens to the other elements when an el in the middle of a row is aligned.  The answer I came up with is that it will push other elements to the side.

```
         alignLeft(pushes element on the left of it, to the left)
         |              center(is the default now)
         |              |                    alignRight
         v              v                    v
  |-el-|-el-|---------|-el-|---------------|-el-|
```

Also of note, is that if something is `center`, then it will truly be in the center.

* _Centered by Default_ - `el`s are centered by default.


## Things that have been removed/deprecated


**Percent** - `percent` is no longer there for width/height values. Just use `fill`, because you were probably just using `percent 100`, right?  If you really need something like percent, you can manage it pretty easy with `fillPortion`. The main reason this goes away is that `percent` allows you to accidently overflow your element when trying to sum up multiple elements.

**Grids** - `grid`, and `namedGrid` are gone.  The reason for this is that for 95% of cases, just composing something with `row` and `column` results in _much_ nicer code.  I'm open to see arguments for `grid`, but I need to see specific realworld usecases that can't be done using `row` and `column`.

**WrappedRows/Columns** - `wrappedRow` and `wrappedColumn` are gone.  From what I can see these cases don't show up very often.  Removing them allows me to be cleaner with the internal style-elements code as well.

**When/WhenJust** - `when` and `whenJust` are removed, though you can easily make a convenience function for yourself!  I wanted the library to place an emphasis on common elm constructs instead of library-specific ones.  As far as shortcuts, they don't actually save you much anyway.

**Full, Spacer** - `full` and `spacer` have been removed in order to follow the libraries priority of explicitness.  `full` would override the parents padding, while `spacer` would override the parent's `spacing`.  Both can be achieved with the more common primities of `row`, `column` and `spacing`, and potentially some nesting of layouts.

