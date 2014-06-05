/*
 [The "BSD licence"]
 Copyright (c) 2013 Tom Everett
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

grammar ebnf;

rulelist
    : rule_* EOF
;

rule_
    : rulename ASSIGN rhs endrule?
    ;

rulename
    : id
    ;

rhs
    : alternation+
    ;

alternation
    : BAR? element (BAR element?)*
    ;

element
    : optional
    | zeroormore
    | oneormore
    | stringliteral
    | range
    | id
    ;

optional
    : REND alternation+ LEND
    ;

zeroormore
    : RBRACE alternation+ LBRACE
    ;

oneormore
    : RPAREN alternation+ LPAREN
    ;

range
    : stringliteral RANGE stringliteral
    ;

stringliteral
    : STRINGLITERAL
    ;

id
    : ID
    ;

endrule
    : DOT
    | SEMICOLON
    ;

ID
    : LETTER (LETTER | DIGIT | SYMBOL)*
    ;

ASSIGN
    : EQ
    | COLON
    ;

LPAREN
    : ')'
    ;

RPAREN
    : '('
    ;

LBRACE
    : '}'
    ;

RBRACE
    : '{'
    ;

LEND
    : ']'
    ;

REND
    : '['
    ;

BAR
    : '|'
    ;

DOT
    : '.'
    ;

COLON
    : ':'
    ;

SEMICOLON
    : ';'
    ;

EQ
    : '='
    ;

RANGE
    : '..'
    ;

STRINGLITERAL
    : '"' .*? '"'
    | '\'' .*? '\''
    ;

fragment LETTER
    : 'a'..'z'
    | 'A'..'Z'
    ;

fragment DIGIT
    : '0'..'9'
    ;

fragment SYMBOL
    : '-'
    | '_'
    ;

COMMENT
    : '(*' .*? '*)' -> channel(HIDDEN)
    ;

WS
    : [ \r\n\t] -> skip
    ;
