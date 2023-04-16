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
    |   functionDefinitionKandR
    |   varFuncDeclaration
    |   typeDeclaration ';'
    |   typeDefinition
    |   typeWillBeDeclared
    |   ';'
    ;

storageFuncSpecifier
    :   'static'
    |   '__inline__'
    |   'extern'
    ;

storageVarSpecifier
    :   'register'
    |   'static'
    |   '_Thread_local'
    ;


typeWillBeDeclared
    : ('struct'|'union') Identifier ';'
    ;

functionSpecifier
    :  '__inline'
    ;

//if type not spedified : default return int
externalType
    :   '__extension__'? gccDeclaratorExtension* storageFuncSpecifier* functionSpecifier?
        gccDeclaratorExtension* type storageFuncSpecifier*
    ;

functionExt
    : function gccDeclaratorExtension*
    ;

function
    : Identifier functionParameters
    | '(' function ')'
    | visualExtension function
    | gccDeclaratorExtension function
    | typeModifier function
    | function arrayOneDim //returns pointer to array
    | function functionParameters //returns pointer to function
    ;

functionDefinition
    :   externalType? functionExt compoundStatement
    ;

functionDefinitionKandR
    :   externalType? typeModifier* Identifier '(' varListKandR? ')'
            parametersKandRlist? compoundStatement
    ;

varListKandR
    : Identifier (',' Identifier)*
    ;

typeQualifier
    :   'const'
    |   '__const'
    |   '__restrict'
    |   '__restrict__'
    |   'volatile'
    ;

typeDeclaration
    : structDeclaration
    | enumDeclaration
    ;

type
    : storageVarSpecifier* typeQualifier* (typeName | typeDeclaration)
    | storageVarSpecifier typeQualifier*
    | storageVarSpecifier* typeQualifier
    ;

complex
    :   '_Complex' | '__complex__'
    ;

typeName
    : 'int'
    | 'int' 'long'
    | unsignedOrSigned 'int'?
    | unsignedOrSigned? 'long' 'int'?
    | '__extension__'? unsignedOrSigned? 'long' 'long' 'int'?
    | 'long'? unsignedOrSigned 'int'?
    | 'short'
    | unsignedOrSigned? 'short' 'int'?
    | 'short'? unsignedOrSigned 'int'?
    | unsignedOrSigned? 'char'
    | 'float'
    | 'long'? 'double'
    | 'float'? complex
    | 'long'? 'double' complex
    | complex 'float'
    | complex 'long'? 'double'
    | 'long'? unsignedOrSigned? 'int'? complex
    | 'void'
    | ('struct'|'union') Identifier
    | 'enum' Identifier
    | '__builtin_va_list'
    | Identifier
    ;

unsignedOrSigned
    : 'unsigned'
    | 'signed'
    ;

typeModifier
    : '*' gccDeclaratorExtension?
    ;

fixedParameterOrTypeList
    : parameterOrType (',' parameterOrType)*
    ;

parameterOrTypeList
    : fixedParameterOrTypeList (',' '...')?
    ;

parametersKandRlist
    : (parametersKandR ';')+
    ;

parametersKandR
    : type parameterKandR (',' parameterKandR)*
    ;

parameterKandR
    : gccDeclaratorExtension* typeModifier* typeQualifier* variable (array | functionParameters)? gccDeclaratorExtension*
    ;

parameterOrType
    : gccDeclaratorExtension* type variablePlace (array | functionParameters)? gccDeclaratorExtension*
    ;

compoundStatement
    :   '{' (declaration | statement)*'}'
    ;

label
    : Identifier ':'
    ;

varFuncDeclaration
    : 'extern'? '__extension__'? gccDeclaratorExtension* externalType? varFuncList ';' //typeName can't be void without modifiers
    ;

varFuncList
    : varFuncDeclarator (',' varFuncDeclarator)*
    ;

varFuncDeclarator
    :   gccDeclaratorExtension* fieldDeclarator ('=' initializer)?
    |   functionExt
    ;

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
    ;

arrayInitializer
    : '{' assignmentExpression (',' assignmentExpression)* '}'
    | '{' arrayCellInitializer (',' arrayCellInitializer)* '}' //for incomplete initialization
    ;

arrayCellInitializer
    : '[' conditionalExpression']' '=' assignmentExpression
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

variable
     : '(' variable ')'
    | (typeModifier | typeQualifier) variable
    | variable arrayOneDim
    | Identifier
    ;

variablePlace
     : '(' variablePlace ')'
    | (typeModifier | typeQualifier) variablePlace
    | variablePlace arrayOneDim
    | Identifier
    | /*empty*/
    ;

fieldDeclaration //typeName can't be void without modifiers ; bitField: anonymous field
    : '__extension__'? gccDeclaratorExtension* type (fieldList | bitField)? gccDeclaratorExtension*
    ;

fieldList
    : fieldDeclarator (',' fieldDeclarator)*
    ;

fieldDeclarator
    : typeModifier* typeQualifier* (variableName | surroundedVariableName) (bitField | array)? gccDeclaratorExtension*
    | typeModifier* typeQualifier* surroundedVariableName functionParameters gccDeclaratorExtension*
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

statement
    :   compoundStatement
    |   expressionStatement
    |   loopStatement
    |   ifStatement
    |   switchStatement
    |   caseLabel
    |   defaulLabel
    |   asmStatement
    |   label
    |   'goto' Identifier ';'
    |   'goto' '*' unaryExpression ';'
    |   'return' commaExpression? ';'
    |   'continue' ';'
    |   'break' ';'
    |   ';'
    ;

asm
    :  '__asm__' |  '__asm'
    ;

asmStatement
    :   asm '__volatile__'? '(' StringLiteral (':' asmPart?)* ')' ';'
    ;

asmPart
    :   asmElement (',' asmElement)*
    |   StringLiteral
    ;

asmElement
    :  StringLiteral '(' postfixExpression ')'
    ;

ifStatement
    : 'if' '(' commaExpression ')' statement ('else' statement )?
    ;

/* Relation 'case' to 'switch' is like relation contionue/break to for/while loops:
   must be inside these statement, but it can't be checked with non context gramamr,
   if we want to avoid duplicate rules and is left to be checked by semantics */
switchStatement
    : 'switch' '(' commaExpression ')' statement
    ;

caseLabel
    : 'case' (literal | conditionalExpression) ':'
    ;

defaulLabel
    : 'default' ':'
    ;


loopStatement
    :   'while' '(' commaExpression ')' statement
    |   'do' statement 'while' '(' commaExpression ')' ';'
    |   'for' '(' commaExpression? ';' commaExpression? ';' commaExpression? ')' statement
    ;

expressionStatement
    :   commaExpression ';'
    ;

commaExpression
    :   assignmentExpression
    |   commaExpression ',' assignmentExpression
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
    :   logicalAndExpression
    |   logicalOrExpression '||' logicalAndExpression
    ;

logicalAndExpression
    :   inclusiveOrExpression
    |   logicalAndExpression '&&' inclusiveOrExpression
    ;

inclusiveOrExpression
    :   exclusiveOrExpression
    |   inclusiveOrExpression '|' exclusiveOrExpression
    ;

exclusiveOrExpression
    :   andExpression
    |   exclusiveOrExpression '^' andExpression
    ;

andExpression
    :   equalityExpression
    |   andExpression '&' equalityExpression
    ;

equalityExpression
    :   relationalExpression
    |   equalityExpression '==' relationalExpression
    |   equalityExpression '!=' relationalExpression
    ;

relationalExpression
    :   shiftExpression
    |   relationalExpression '<' shiftExpression
    |   relationalExpression '>' shiftExpression
    |   relationalExpression '<=' shiftExpression
    |   relationalExpression '>=' shiftExpression
    ;

shiftExpression
    :   additiveExpression
    |   shiftExpression '<<' additiveExpression
    |   shiftExpression '>>' additiveExpression
    ;

additiveExpression
    :   multiplicativeExpression
    |   additiveExpression '+' multiplicativeExpression
    |   additiveExpression '-' multiplicativeExpression
    ;

multiplicativeExpression
    :   castExpression
    |   multiplicativeExpression '*' castExpression
    |   multiplicativeExpression '/' castExpression
    |   multiplicativeExpression '%' castExpression
    ;

castExpression
    :   unaryExpression
    |   '(' gccDeclaratorExtension* typeSpecifier ')' (castExpression | arrayInitializer | structInitializer)
    ;

unaryOperator
    :   '&' | '*' | '+' | '-' | '~' | '!'
    ;

unaryExpression
    :   postfixExpression
    |   '++' unaryExpression
    |   '--' unaryExpression
    |   '&&' Identifier
    |   unaryOperator castExpression
    |   sizeofOrAlignof '(' typeSpecifier ')'
    |   sizeofOrAlignof '(' conditionalExpression ')'
    |   sizeofOrAlignof typeSpecifier
    |   sizeofOrAlignof conditionalExpression
    |   '__builtin_offsetof' '(' type ',' postfixExpression ')'
    |  '__builtin_va_arg' '(' postfixExpression ',' type ')'
    |  ('__real__'|'__imag__') unaryExpression
    ;

sizeofOrAlignof
    :   'sizeof'
    |   '_Alignof'
    |   '__alignof__'
    ;

modifiersWithoutVariable
    : '(' modifiersWithoutVariable')'
    | (typeModifier | typeQualifier) modifiersWithoutVariable
    | modifiersWithoutVariable arrayOneDim
    | modifiersWithoutVariable functionParameters
    | /*empty*/
    ;

typeSpecifier
    :   type modifiersWithoutVariable
    ;

postfixExpression
    :   primaryExpression
    |   postfixExpression '[' conditionalExpression ']'
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
    : '__extension__'? 'typedef' typeQualifier* type
        definedType (',' definedType)*
        gccDeclaratorExtension* ';'
    ;

definedType
    :   gccDeclaratorExtension* (fieldDeclarator | function)
    ;


visualExtension
    : '__cdecl'
    ;

gccDeclaratorExtension
    :  gccAttributeSpecifier
    |  asm '(' StringLiteral+ ')'
    | '_Alignas' '(' conditionalExpression ')'
    ;

gccAttributeSpecifier
    :   '__attribute__' '(' '(' gccAttributeList? ')' ')'
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
    : Identifier ('=' inclusiveOrExpression)?
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
    |  'fi' /* imag part of complex */
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
    :   HexadecimalPrefix HexadecimalDigit+ IntegerSuffix?
    ;

fragment
HexadecimalPrefix
    :   '0' [xX]
    ;

FloatingConstant
    :   DecimalFloatingConstant FloatingSuffix?
    ;

fragment
DecimalFloatingConstant
    :   (FractionalConstant ExponentPart? | DigitSequence ExponentPart) FloatingSuffix?
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
    :   '\\' ['"?abfnrtv\\]
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
