/*
 [The "BSD licence"]
 Copyright (c) 2023 Andrzej Borucki
 All rights reserved.
 Parts from C grammar Copyright (c) 2013 Sam Harwell

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

grammar C90;

compilationUnit
    :   declaration* EOF
    ;

declaration
    :   functionDefinition
    |   varFuncDeclaration
    |   typeDeclaration ';'
    |   typeDefinition
    |   typeWillBeDeclared
    |   ';'
    ;

storageFuncSpecifier
    :   'static'
    |   '__inline__'
    |  '__inline'
    |   'extern'
    |   '__thread'
    |   'register'
    |   'auto'
    |   '_Thread_local'
    ;

typeWillBeDeclared
    : ('struct'|'union') Identifier ';'
    ;

//if type not spedified : default return int
type
    :   '__extension__'? gccDeclaratorExtension* storageFuncSpecifier* typeQualifier*
        gccDeclaratorExtension*
        (storageFuncSpecifier| typeQualifier| typeName | typeDeclaration | typeofExpr)
        storageFuncSpecifier* gccDeclaratorExtension*
    ;

/*
two similar alts is faster here than
type? attributedDeclarator parametersKandRlist? compoundStatement
*/
functionDefinition
    :   type? attributedDeclarator compoundStatement
    |   type? attributedDeclarator parametersKandRlist compoundStatement
    ;

varListKandR
    : Identifier (',' Identifier)*
    ;

typeQualifier
    :   'const'
    |   '__const'
    |   '__restrict'
    |   '__restrict__'
    |   volatile
    ;

typeDeclaration
    : structDeclaration
    | enumDeclaration
    ;

complex
    :   '_Complex' | '__complex__'
    ;

signedUnsigned
    : 'unsigned'
    | 'signed'
    ;

longShort
    :  'long'
    |  'short'
    ;

intTypeName
    : ('int'| complex | signedUnsigned | longShort)+
    ;

int128TypeName
    : '__int128' complex? signedUnsigned?
    | '__int128' signedUnsigned complex
    | signedUnsigned '__int128' complex?
    | signedUnsigned complex '__int128'
    | complex '__int128' signedUnsigned?
    | complex signedUnsigned '__int128'
    ;

charTypeName
    : 'char' complex? signedUnsigned?
    | 'char' signedUnsigned complex
    | signedUnsigned 'char' complex?
    | signedUnsigned complex 'char'
    | complex 'char' signedUnsigned?
    | complex signedUnsigned 'char'
    ;

floatTypeName
    : 'float'
    | 'double'
    | 'long' 'double'
    | 'double' 'long'
    | complex 'float'
    | complex 'double'
    | complex 'long' 'double'
    | complex 'double' 'long'
    | 'float' complex
    | 'double' complex
    | 'long' 'double' complex
    | 'dobile' 'long' complex
    | 'long' complex 'double'
    | 'double' complex 'long'
    | '__float128'
    | '__float80'
    | '_Decimal32'
    | '_Decimal64'
    | '_Decimal128'
    ;


typeName
    : intTypeName
    | int128TypeName
    | charTypeName
    | floatTypeName
    | 'void'
    | ('struct'|'union') Identifier
    | 'enum' Identifier
    | '__builtin_va_list'
    | Identifier
    ;

typeModifier
    : '*'
    ;

fixedParameterOrTypeList
    : parameterOrType (',' parameterOrType)*
    ;

parameterOrTypeList
    : fixedParameterOrTypeList (',' '...')?
    | 'void'
    ;

parametersKandRlist
    : (parametersKandR ';')+
    ;

parametersKandR
    : type declarator (',' declarator)*
    ;

parameterOrType
    :   type (declarator | declaratorPlace) gccDeclaratorExtension*
    ;

compoundStatement
    :   '{' (declaration | labeledStatement)*'}'
    ;

gotoLabel
    : Identifier ':'
    ;

varFuncDeclaration
    :   type varFuncList ';'
    ;

varFuncList
    : attributedDeclarator (',' attributedDeclarator)*
    ;

attributedDeclarator
    :   declarator gccDeclaratorExtension* ('=' initializer)?
    ;

/*** declarator ***/
declarator
    :   (typeModifier | typeQualifier) gccAttributeSpecifier* declarator
    |   baseDeclarator
    |   declaratorName
    |   '(' gccAttributeSpecifier* declarator ')'
    ;

baseDeclarator
    :   declaratorName (functionParameters | array)
    |   '('  declarator ')' (functionParameters | array)
    ;

declaratorName
    :   visualExtensionFCall* Identifier
    ;

declaratorPlace
    :   (typeModifier | typeQualifier) declaratorPlace
    |   baseDeclaratorPlace
    |   declaratorNamePlace
    |   '(' declaratorPlace ')'
    ;

baseDeclaratorPlace
    :   declaratorNamePlace (functionParameters | array)
    |   '('  declaratorPlace ')' (functionParameters | array)
    ;

declaratorNamePlace
    :   visualExtensionFCall*
    ;
/*** end declarator ***/

structInitializer
    : '{' (fieldInitializer (',' fieldInitializer)* ','? )? '}'
    ;

initializer
    :  assignmentExpression
    |  structInitializer
    |  arrayInitializer
    ;

fieldInitializer
    : initializer
    | '.' Identifier '=' initializer
    | Identifier ':' initializer
    ;

arrayInitializer
    : '{' assignmentExpression (',' assignmentExpression)* '}'
    | '{' arrayCellInitializer (',' arrayCellInitializer)* '}' //for incomplete initialization
    ;

arrayCellInitializer
    : '[' conditionalExpression']' ('.' Identifier)? '=' commaExpression
    | '[' conditionalExpression '...' conditionalExpression ']' '=' commaExpression
    ;

surroundedVariableName
    : '(' surroundedVariableName ')'
    | typeModifier surroundedVariableName
    |  '(' variableName ')'
    ;

variableName
    : typeModifier variableName
    | variableName arrayOneDim
    | Identifier
    ;


fieldDeclaration //typeName can't be void without modifiers ; bitField: anonymous field
    : type (fieldList | bitField)? gccDeclaratorExtension*
    ;

fieldList
    :   fieldDeclarator (',' fieldDeclarator)*
    ;

fieldDeclarator
    :    attributedDeclarator bitField?
    ;

functionParameters
    : '(' parameterOrTypeList? ')'
    ;

array
    : arrayOneDim+
    ;

arrayOneDim // [*] can be in function prototype declaration
    : '[' typeQualifier* (conditionalExpression | '*')? ']'
    ;

bitField
    : ':' conditionalExpression
    ;

structDeclaration
        : '__extension__'? ('struct'|'union') gccDeclaratorExtension* Identifier? '{' fieldDeclarations? '}' gccDeclaratorExtension*
        ;

fieldDeclarations
        : fieldDeclaration (';' fieldDeclaration?)*
        | ';'+
        ;

labeledStatement
    : attributedLabel* statement
    ;

statement
    :   compoundStatement
    |   expressionStatement
    |   loopStatement
    |   ifStatement
    |   switchStatement
    |   asmStatement
    |   'goto' Identifier ';'
    |   'goto' '*' castExpression ';'
    |   'return' commaExpression? ';'
    |   'continue' ';'
    |   'break' ';'
    |   gccAttributeSpecifier* ';'
    ;

label
    :   gotoLabel
    |   caseLabel
    |   defaulLabel
    ;

attributedLabel
    :   label (gccAttributeSpecifier* ';')?
    ;

asm
    :  '__asm__' |  '__asm'
    ;

volatile
    :   'volatile' | '__volatile__'
    ;

asmStatement
    :   asm volatile? '(' StringLiteral (':' asmPart?)* ')' ';'
    ;

asmPart
    :   asmElement (',' asmElement)*
    |   StringLiteral
    ;

asmElement
    :  StringLiteral '(' castExpression ')'
    ;

ifStatement
    : 'if' '(' commaExpression ')' labeledStatement ('else' labeledStatement )?
    ;

/* Relation 'case' to 'switch' is like relation contionue/break to for/while loops:
   must be inside these statement, but it can't be checked with non context gramamr,
   if we want to avoid duplicate rules and is left to be checked by semantics */
switchStatement
    : 'switch' '(' commaExpression ')' labeledStatement
    ;

caseLabel
    : 'case' conditionalExpression ('...' conditionalExpression)? ':'
    ;

defaulLabel
    : 'default' ':'
    ;


loopStatement
    :   'while' '(' commaExpression ')' labeledStatement
    |   'do' labeledStatement 'while' '(' commaExpression ')' ';'
    |   'for' '(' commaExpression? ';' commaExpression? ';' commaExpression? ')' labeledStatement
    ;

expressionStatement
    :   commaExpression ';'
    ;

commaExpression
    :   assignmentExpression (',' assignmentExpression)*
    ;

assignmentOperator
    :   '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|='
    ;

assignmentExpression
    :   conditionalExpression
    |   unaryExpression assignmentOperator assignmentExpression
    ;

conditionalExpression
    :   logicalOrExpression ('?' commaExpression? ':' conditionalExpression)?
    ;

logicalOrExpression
    :   logicalAndExpression ('||' logicalAndExpression)*
    ;

logicalAndExpression
    :   inclusiveOrExpression ('&&' inclusiveOrExpression)*
    ;

inclusiveOrExpression
    :   exclusiveOrExpression ('|' exclusiveOrExpression)*
    ;

exclusiveOrExpression
    :   andExpression ('^' andExpression)*
    ;

andExpression
    :   equalityExpression ('&' equalityExpression)*
    ;

eqop
    :   '==' | '!='
    ;

equalityExpression
    :   relationalExpression (eqop relationalExpression)*
    ;

relop
    : '<' | '>' | '<=' | '>='
    ;

relationalExpression
    :   shiftExpression (relop shiftExpression)*
    ;


shiftop
    :   '<<' | '>>'
    ;

shiftExpression
    :   additiveExpression (shiftop additiveExpression)*
    ;


addop
    : '+' | '-'
    ;

additiveExpression
    :   multiplicativeExpression (addop multiplicativeExpression)*
    ;


mulop
    :   '*' | '/' | '%'
    ;

multiplicativeExpression
    :   castExpression (mulop castExpression)*
    ;

castExpression
    :   unaryExpression
    |   '__extension__'? '(' gccDeclaratorExtension* typeSpecifier ')' (castExpression | arrayInitializer | structInitializer)
    ;

unaryOperator
    :   '&' | '*' | '+' | '-' | '~' | '!'
    ;

unaryExpression
    :   postfixExpression
    |   '++' castExpression
    |   '--' castExpression
    |   '&&' Identifier
    |   unaryOperator castExpression
    |   sizeofOrAlignof '(' typeSpecifier ')'
    |   sizeofOrAlignof '(' conditionalExpression ')'
    |   sizeofOrAlignof typeSpecifier
    |   sizeofOrAlignof conditionalExpression
    |   '__builtin_offsetof' '(' type ',' postfixExpression ')'
    |  '__builtin_va_arg' '(' postfixExpression ',' typeSpecifier ')'
    |  ('__real__'|'__imag__') unaryExpression
    ;

sizeofOrAlignof
    :   'sizeof'
    |   '_Alignof'
    |   '__alignof__'
    ;

typeSpecifier
    :   type declaratorPlace
    ;

postfixExpression
    :   primaryExpression
    |   postfixExpression '[' assignmentExpression ']'
    |   postfixExpression '(' argumentExpressionList? ')'
    |   postfixExpression '.' Identifier
    |   postfixExpression '->' Identifier
    |   postfixExpression '++'
    |   postfixExpression '--'
    ;


argumentExpressionList
    :   assignmentExpression (',' assignmentExpression)* (',' '__extension__' '__PRETTY_FUNCTION__' )?
    ;


primaryExpression
    :   Identifier
    |   literal
    |   StringLiteral+
    |   '(' commaExpression ')'
    |   '__extension__'? '(' compoundStatement ')'
    ;


literal
    :   integerConstant
    |   FloatingConstant
    |   CharacterConstant
    ;


typeDefinition
    : '__extension__'? 'typedef' gccDeclaratorExtension* typeQualifier*
        type attributedDeclarator (',' attributedDeclarator)* ';'
    ;


typeofKeyword
    :   '__typeof' | '__typeof__'
    ;

typeofExpr
    : typeofKeyword '(' ( typeSpecifier | conditionalExpression ) ')'
    ;

visualExtensionFCall
    : '__cdecl'
    ;

gccDeclaratorExtension
    :  gccAttributeSpecifier
    |  asm '(' StringLiteral+ ')'
    | '_Alignas' '(' conditionalExpression ')'
    ;

attribute
    : '__attribute__' | '__attribute'
    ;

gccAttributeSpecifier
    :   attribute '(' '(' gccAttributeList? ')' ')'
    ;

gccAttributeList
    :   gccAttribute (',' gccAttribute)*
    ;

attributeAlignment
    :   '__alignof__' '(' typeName ')'
    |   conditionalExpression
    ;

gccAttribute
    : Identifier ('(' (attributeAlignment | StringLiteral | (Identifier | integerConstant) ( ',' integerConstant )*) ')')?
    | 'const'
    ;

enumDeclaration
    : 'enum' Identifier? '{' enumList '}'
    ;

enumList
    : enumItem (',' enumItem)* ','?
    ;

enumItem
        : Identifier gccAttributeSpecifier* ('=' inclusiveOrExpression)?
    ;

Identifier
    :   Nondigit
        (   Nondigit
        |   Digit
        )*
    ;

fragment
Digit
    :   [0-9]
    ;

fragment
NonzeroDigit
    :   [1-9]
    ;

fragment
Nondigit
    :   [a-zA-Z_]
    ;


integerConstant
    :   sign? DecimalConstant
    |   BinaryConstant
    |   OctalConstant
    |   HexadecimalConstant //to do integer suffix?
    ;

sign
    :   '+' | '-'
    ;

DecimalConstant
    :   (NonzeroDigit Digit* | '0') IntegerSuffix?
    ;

fragment
IntegerSuffix
    :   UnsignedSuffix (LongSuffix | LongLongSuffix)?
    |   (LongSuffix | LongLongSuffix) UnsignedSuffix?
    ;

fragment
UnsignedSuffix
    :   [uU]
    ;

fragment
LongSuffix
    :   [lL]
    ;

fragment
LongLongSuffix
    :   'll' | 'LL'
    ;

fragment
FloatingSuffix
    :  'f' | 'l' | 'F' | 'L'
    |  'fi'| 'fj'| 'i'  /* imag part of complex */
    ;

fragment
Sign
    :   '+' | '-'
    ;

fragment
OctalDigit
    :   [0-7]
    ;


fragment
BinaryDigit
    :   [01]
    ;


fragment
HexadecimalDigit
    :   [0-9a-fA-F]
    ;

OctalConstant
    :   '0' OctalDigit* IntegerSuffix?
    ;

BinaryConstant
    :   '0'[Bb] BinaryDigit* IntegerSuffix?
    ;

HexadecimalConstant
    :   HexadecimalPrefix HexadecimalDigitSequence IntegerSuffix?
    ;

fragment
HexadecimalPrefix
    :   '0' [xX]
    ;

FloatingConstant
    :   DecimalFloatingConstant
    |   HexadecimalFloatingConstant
    ;

fragment
DecimalFloatingConstant
    :   (FractionalConstant ExponentPart? | DigitSequence ExponentPart) FloatingSuffix?
    ;


fragment
HexadecimalDigitSequence
    :   HexadecimalDigit+
    ;

fragment
HexadecimalFractionalConstant
    :   HexadecimalDigitSequence? '.' HexadecimalDigitSequence
    |   HexadecimalDigitSequence '.'
    ;

fragment
BinaryExponentPart
    :   [pP] Sign? DigitSequence
    ;

fragment
HexadecimalFloatingConstant
    :   HexadecimalPrefix (HexadecimalFractionalConstant | HexadecimalDigitSequence) BinaryExponentPart FloatingSuffix?
    ;

fragment
DigitSequence
    :   Digit+
    ;

fragment
ExponentPart
    :   [eE] Sign? DigitSequence
    ;

fragment
FractionalConstant
    :   DigitSequence? '.' DigitSequence
    |   DigitSequence '.'
    ;


fragment
StringLiteralOne
    :   '"' SChar* '"'
    ;

fragment
StringLiteralMulti
    : '"' SChar* '\\'Newline (SChar* '\\'Newline)* SChar* '"'
    ;

StringLiteral
    :   'L'? (StringLiteralOne | StringLiteralMulti) //L"abc" - elements are ints
    ;

fragment
SChar
    :   ~["\\\r\n]
    |   EscapeSequence
    ;

CharacterConstant
    :  'L'? '\'' CChar '\''
    ;

fragment
CChar
    :   ~['\\\r\n]
    |   EscapeSequence
    ;

fragment
EscapeSequence
    :   SimpleEscapeSequence
    |   OctalEscapeSequence
    |   HexadecimalEscapeSequence
    |   UnicodeEscapeSequence
    ;


fragment
SimpleEscapeSequence
    :   '\\' ['"?abfnrtvhe\\]
    ;

fragment
OctalEscapeSequence
    :   '\\' OctalDigit (OctalDigit OctalDigit?)?
    ;

fragment
HexadecimalEscapeSequence
    :   '\\x' HexadecimalDigit HexadecimalDigit?
    ;

fragment
UnicodeEscapeSequence
    :   '\\u' HexQuad
    |   '\\U' HexQuad HexQuad
    ;

fragment
HexQuad
    :   HexadecimalDigit HexadecimalDigit HexadecimalDigit HexadecimalDigit
    ;

Whitespace
    :   [ \t]+
        -> channel(HIDDEN)
    ;

Newline
    :   (   '\r' '\n'?
        |   '\n'
        )
        -> channel(HIDDEN)
    ;

BlockComment
    :   '/*' .*? '*/'
        -> channel(HIDDEN)
    ;

Preprocessor
    :   '#' ~[\r\n]*
        -> channel(HIDDEN)
    ;
