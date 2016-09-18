# pl16_3
Programming Languages - Third Assignment

# Setup

I recommend you use a cabal sandbox

    $ cabal sandbox init

Next, install dependencies (this might take a while)

    $ cabal install --only-dependencies

You should now be able to build or run the program

    $ cabal build
    $ cabal run


I rearranged the EBNF to

    block             ::=  '{' { command } '}'

    command           ::=  '[' guard ':' { command } ']'
                        |  [ { '*' } name '=' ] expression ';'
                        |  '^' expression ';'

    guard             ::=  expression ( '=' | '#' ) expression [ ',' guard ]

    expression        ::=  single_expression [ '+' expression ]

    single_expression ::=  expression_base { '.' name }

    expression_base   ::=  ( string_literal
                           | block
                           | { '*' } name
                           | '(' expression ')'
                           )

