/*
Python grammar
The MIT License (MIT)
Copyright (c) 2022 Robert Einhorn

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
 */

 /*
 * Project      : an ANTLR4 parser grammar based on the official Python 3.8.12 grammar
 *                https://github.com/RobEin/tiny-python
 * Developed by : Robert Einhorn, robert.einhorn.hu@gmail.com
 */

parser grammar PythonParser; // Tiny Python based on: https://docs.python.org/3.8/reference/grammar.html
options { tokenVocab=PythonLexer; superClass=PythonParserBase; }
// ANTLR4 grammar for Tiny Python


file_input: (NEWLINE | stmt)* EOF; // start rule

stmt: simple_stmt | compound_stmt;

simple_stmt: small_stmt NEWLINE;
small_stmt: expr_stmt | flow_stmt;
expr_stmt: NAME '=' expr;
flow_stmt: break_stmt | continue_stmt;
break_stmt: 'break';
continue_stmt: 'continue';

compound_stmt: if_stmt | while_stmt;
if_stmt: 'if' namedexpr_test ':' suite ('elif' namedexpr_test ':' suite)* ('else' ':' suite)?;
while_stmt: 'while' namedexpr_test ':';
suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT;

namedexpr_test: test;
test: expr (comp_op expr)*;

comp_op: '<'|'>'|'=='|'>='|'<='|'!=';

expr: expr (( '+' | '-' ) expr)+
    | NAME
    | NUMBER
    | '(' expr ')'
    ;
