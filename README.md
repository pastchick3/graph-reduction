# A Functional Programming Langauge based on Graph Reduction

## FL

``` EBNF
<digit> ::= "0"-"9";
<lower> ::= "a"-"z";
<upper> ::= "A"-"Z";

<prefix-op> ::= "not" | "-";
<infix-op> ::= "+" | "-" | "*" | "/" | "==" | "/=" | "<" | ">" | "<=" | ">=" | "&&" | "||" | "++" | ":" | "!!";

<lower-var> ::= <lower> (<digit> | <lower> | <upper>)*;
<upper-var> ::= <upper> (<digit> | <lower> | <upper>)*;
<int> ::= ("+" | "-")? <digit>+;
<char> ::= "'" (<digit> | <lower> | <upper> | " ") "'";
<bool> ::= "True" | "False";
<str> ::= "\"" <char>* "\"";
<list> ::= "[" "]" | "[" <exp> ("," <exp>)* "]";
<tuple> ::= "(" <exp> ("," <exp>)* ")";
<pat> ::= <lower-var>
    | <int>
    | <char>
    | <bool>
    | <str>
    | "[" "]"
    | <pat> (":" <pat>)+
    | <pat> ("," <pat>)+
    | <upper-var> <pat>*;
<type> ::= <lower-var>
    |"Int"
    | "Char"
    | "Bool"
    | "[" <type> "]"
    | "(" <type> ("," <type>)+ ")"
    | <type> ("->" <type>)+;

<list-comp> ::= "[" <exp> "|" <qual> ("," <qual>)* "]";
<qual> ::= <exp> | <pat> "<-" <exp>;
<case> ::= "case" <exp> "of" <case-cl>+ "where" <var-def>+;
<case-cl> ::= <pat> (<guard-cl>? "->" <exp>)+;
<guard-cl> ::= "|" <exp>;
<func> ::= <lower-var> <func-cl>+;
<func-cl> ::= <pat>* (<guard-cl>? "=" <exp>)+;
<func-app> ::= <exp> <exp>;
<exp> ::= <lower-var>
    | <int>
    | <char>
    | <bool>
    | <str>
    | <list>
    | <tuple>
    | <list-comp>
    | <case>
    | <func>
    | <func-app>
    | <prefix-op> <exp>
    | <exp> <infix-op> <exp>
    | <upper-var> <exp>*;

<type-def> ::= <lower-var> "::" <type>;
<adt-def> ::= "data" <upper-var> <type>* "=" <upper-var> <type>* ("|" <upper-var> <type>*)*;
<var-def> ::= <pat> "=" <exp>;

<def> ::= <type-def> | <adt-def> | <var-def>;
<prog> ::= <def>* <exp>;
```

| Precedence | Left associative operators | Non-associative operators | Right associative operators |
| --- | --- | --- | --- |
| 9 | !! | | |
| 7 | *, / | | |
| 6 | +, - (both prefix and infix), not | | |
| 5 | | | :, ++|
| 4 | | ==, /=, <, <=, >, >= | |
| 3 | | | && |
| 2 | | | \|\| |

## Enriched Lambda Expressions

``` EBNF
<const> ::= <int>
    | <char>
    | <bool>
    | <str>;
<ctor> ::= "Nil"
    | "Cons"
    | "Tuple2"-"Tuple4"
    | <upper-var>;
<pat> ::= <const>
    | <lower-var>
    | <ctor> <pat>*;
<exp> ::= <const>
    | <lower-var>
    | <prefix-op> <exp>
    | <exp> <infix-op> <exp>
    | <exp> <exp>
    | "\\" <pat> "." <exp>
    | "let" <pat> "=" <exp> "in" <exp>
    | "letrec" (<pat> "=" <exp>)+ "in" <exp>
    | <exp> "<>" <exp>
    | "case" <var> of (<pat> "->" <exp>)+;
```

## Lambda Expressions

``` EBNF
<exp> ::= <const>
    | <lower-var>
    | <prefix-op> <exp>
    | <exp> <infix-op> <exp>
    | <exp> <exp>
    | "\\" <lower-var> "." <exp>;
```
