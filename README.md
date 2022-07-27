# A Functional Programming Langauge based on Graph Reduction

## Functional Language

``` EBNF
<digit> ::= "0"-"9";
<lower> ::= "a"-"z";
<upper> ::= "A"-"Z";

<prefix-op> ::= "not" | "-";
<infix-op> ::= "+" | "-" | "*" | "/" | "==" | "/=" | "<" | "<=" | ">" | ">=" | "&&" | "||" | "++" | ":" | "!!";

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

<type> ::= "(" <type> ")"
    | <lower-var>
    | "Int"
    | "Char"
    | "Bool"
    | "String"
    | "[" <type> "]"
    | "(" <type> ("," <type>)+ ")"
    | <type> ("->" <type>)+;

<list-comp> ::= "[" <exp> "|" <qual-cl> ("," <qual-cl>)* "]";
<qual-cl> ::= <pat> "<-" <exp> | <exp>;
<case> ::= "case" <exp> "of" <case-cl>+;
<case-cl> ::= <pat> (("|" <exp>)? "->" <exp>)+;
<func-app> :: <exp> <exp>;

<exp> ::= "(" <exp> ")"
    | <lower-var>
    | <int>
    | <char>
    | <bool>
    | <str>
    | <list>
    | <tuple>
    | <upper-var> <exp>*
    | <list-comp>
    | <case>
    | <func-app>
    | <prefix-op> <exp>
    | <exp> <infix-op> <exp>;

<adt-def> ::= "data" <upper-var> <type>* "=" <upper-var> <type>* ("|" <upper-var> <type>*)*;
<type-def> ::= <lower-var> "::" <type>;
<var-def> ::= <pat> "=" <exp>;
<func-def> ::= <lower-var> <pat>+ (("|" <exp>)? "=" <exp>)+;
<where> ::= "where" (<var-def> | <func-def>)+

<def> ::= <adt-def> | <type-def> | <var-def> | (<func-def> <where>?);
<prog> ::= <def>+;
```

| Precedence | Left associative operators | Non-associative operators | Right associative operators |
| --- | --- | --- | --- |
| 9 | !!, not | | |
| 7 | *, / | | |
| 6 | +, - (both prefix and infix) | | |
| 5 | | | :, ++|
| 4 | | ==, /=, <=, <, >=, > | |
| 3 | | | && |
| 2 | | | \|\| |

## Enriched Lambda Calculus

``` EBNF
<pat> ::= <lower-var>
    | <int>
    | <char>
    | <bool>
    | <str>
    | "Nil"
    | "Cons" <pat> <pat>
    | "Tuple2" <pat> <pat>
    | "Tuple3" <pat> <pat> <pat>
    | "Tuple4" <pat> <pat> <pat> <pat>
    | <upper-var> <pat>*;
<exp> ::= <lower-var>
    | <int>
    | <char>
    | <bool>
    | <str>
    | "Nil"
    | "Cons" <exp> <exp>
    | "Tuple2" <exp> <exp>
    | "Tuple3" <exp> <exp> <exp>
    | "Tuple4" <exp> <exp> <exp> <exp>
    | <upper-var> <pat>*
    | <prefix-op> <exp>
    | <exp> <infix-op> <exp>
    | <exp> <exp>
    | "\\" <pat> "." <exp>
    | "let" <pat> "=" <exp> "in" <exp>
    | "letrec" (<pat> "=" <exp>)+ "in" <exp>
    | <exp> "<>" <exp>
    | "case" <var> of (<pat> "->" <exp>)+;
```

## Lambda Calculus

``` EBNF
<exp> ::= <lower-var>
    | <int>
    | <char>
    | <bool>
    | <str>
    | <prefix-op> <exp>
    | <exp> <infix-op> <exp>
    | <exp> <exp>
    | "\\" <lower-var> "." <exp>;
```
