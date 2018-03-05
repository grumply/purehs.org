----------------
title: First-Class Templating
highlights: []
----------------

First-class functional views give Pure the most expressive templating system, making it easy to compose, analyze, and transform views.

# Code

```haskell
data Button ctx = Button Txt

instance Typeable ctx => Pure Button ctx where
  render (Button txt) = HTML.Button [] [fromTxt txt]

-- Template
button txt = View (Button txt)

-- Template transformation/composition
button' = button . Txt.replace "_" " "

-- View Transformation
upcaseButton (View (Button txt)) = View (Button (T.map toUpper txt))

-- View Pattern; bi-directional
pattern Btn txt = View (Button txt)

-- View Transformation via Pattern
upcaseBtn (Btn txt) = Btn (T.map toUpper txt)

-- Or skip the Data entirely
pattern Bttn txt = View (HTML.Button [] [fromTxt txt])
```
