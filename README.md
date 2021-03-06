# pl16_3
Programming Languages - Third Assignment

# Setup

We need the following libraries, for Ubuntu and others

    $ sudo apt-get install libncursesw5-dev

I recommend you use a cabal sandbox

    $ cabal sandbox init

Next, install dependencies (this might take a while)

    $ cabal install happy  # needed for c2hs
    $ cabal install alex   # needed for c2hs
    $ cabal install c2hs   # a tool we need
    $ cabal install --only-dependencies

You should now be able to build and run the program

    $ cabal build
    $ cabal run

## Using the editor

Call with a file argument to open

    $ cabal run program.txt

or without:

    $ cabal run # you will be presented a very crude open file dialog

You can open files with Ctrl-o, save using Ctrl-w (w for "write", s for "save"
is already used for another function in most shells) or close the editor
using Ctrl-x (eXit, q was already taken).

Lines are wrapped and scrolling should happen once the cursor is about to
leave the screen.

## Notes

### Colors

Colors are defined in the `src/Render/Tagged/Tag.hs`. Feel free to change colors.

Available colors:

    ColorBlack
    ColorRed
    ColorGreen
    ColorYellow
    ColorBlue
    ColorMagenta
    ColorCyan
    ColorWhite
    ColorDefault

### EBNF

I rearranged the EBNF to

    block             ::=  '{' { command } '}'

    command           ::=  '[' guard ':' { command } ']'
                        |  expression ';'
                        |  nameWithLevel '=' expression ';'
                        |  '^' expression ';'

    guard             ::=  expression ( '=' | '#' ) expression [ ',' guard ]

    expression        ::=  single_expression [ '+' expression ]

    single_expression ::=  expression_base { '.' name }

    expression_base   ::=  ( string_literal
                           | block
                           | nameWithLevel
                           | '(' expression ')'
                           )

    nameWithLevel     ::=  { '*' } name

While (I hope!) this is equivalent to the original EBNF, the implementation
follows this EBNF more closely.
