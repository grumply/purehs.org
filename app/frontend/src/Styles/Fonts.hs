module Styles.Fonts where

import Pure.Elm

-- fast, performant, well-optimized default for copy
defaultFont :: Txt
defaultFont = "-apple-system, system-ui, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Oxygen, Ubuntu, Cantarell, 'Fira Sans', 'Droid Sans', sans-serif;"

-- familiar fonts for code
defaultMonoFont :: Txt
-- defaultMonoFont = "source-code-pro, Menlo, Monaco, Consolas, 'Courier New', monospace"
defaultMonoFont = "Consolas, 'Andale Mono WT', 'Andale Mono', 'Lucida Console', 'Lucida Sans Typewriter', 'DejaVu Sans Mono', 'Bitstream Vera Sans Mono', 'Liberation Mono', 'Nimbus Mono L', Monaco, 'Courier New', Courier, monospace"

-- nice looking default for titles
titleFont :: Txt
titleFont = "museo-sans, 'Segoe UI', 'Open Sans', sans-seri"

serifFont :: Txt
serifFont = "kopius, serif"