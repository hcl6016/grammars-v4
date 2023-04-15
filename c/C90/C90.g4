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
    :   externalDeclaration* EOF
    ;

externalDeclaration
    :   functionDefinition
    |   functionDefinitionKandR
    |   functionMultiDeclaration
    |   'extern'? variableDeclaration
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
functionRetType
    :   '__extension__'? gccAttributeSpecifierFuncs* storageFuncSpecifier* functionSpecifier?
        gccAttributeSpecifierFuncs* type?
    ;

functionExt
    : function gccDeclaratorExtension2?
    ;

function
    : Identifier functionParameters
    | '(' function ')'
    | visualExtension function
    | gccAttributeSpecifierFuncs function
    | typeModifier function
    | function arrayOneDim //returns pointer to array
    | function functionParameters //returns pointer to function
    ;

functionMultiDeclaration
    :   functionRetType functionExt (',' functionExt)* ';'
    ;

functionDefinition
    :   functionRetType functionExt compoundStatement
    ;

functionDefinitionKandR
    :   functionRetType typeModifier* Identifier '(' varListKandR? ')' gccDeclaratorExtension2?
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
    : storageVarSpecifier* typeQualifier* typeName
    | storageVarSpecifier* typeQualifier* typeDeclaration
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
    | 'float'? '_Complex'
    | 'long'? 'double' '_Complex'
    | '_Complex' 'float'
    | '_Complex' 'long'? 'double'
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
    : '*'
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
    : gccAttributeOrAlignas? type parameterKandR (',' parameterKandR)*
    ;

parameterKandR
    : typeModifier* typeQualifier* variable (array | functionParameters)? gccAttributeSpecifierFuncs?
    ;

parameterOrType
    : gccAttributeOrAlignas? type variablePlace (array | functionParameters)? gccAttributeSpecifierFuncs?
    ;

compoundStatement
    :   '{' blockItem* '}'
    ;

blockItem
    :   variableDeclaration
    |   typeDeclaration ';'
    |   typeDefinition
    |   statement
    |   label
    ;

label
    : Identifier ':'
    ;

variableDeclaration
    : '__extension__'? gccAttributeOrAlignas* type variableList ';' //typeName can't be void without modifiers
    ;

variableList
    : variableDeclarator (',' variableDeclarator)*
    ;

variableDeclarator
    : fieldDeclarator ('=' initializer)?
    ;

structInitializer
    : '{' (fieldInitializer (',' fieldInitializer)* ','? )? '}'
    ;

initializer
    :  conditionalExpression
    |  structInitializer
    |  arrayInitializer
    ;

fieldInitializer
    : initializer
    | '.' Identifier '=' initializer
    ;

arrayInitializer
    : '{' conditionalExpression (',' conditionalExpression)* '}'
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
    : '__extension__'? gccAttributeOrAlignas* type (fieldList | bitField)? gccAttributeOrAlignas*
    ;

fieldList
    : fieldDeclarator (',' fieldDeclarator)*
    ;

fieldDeclarator
    : typeModifier* typeQualifier* (variableName | surroundedVariableName) (bitField | array)? gccAttributeOrAlignas*
    | typeModifier* typeQualifier* surroundedVariableName functionParameters gccAttributeOrAlignas*
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

fieldName
    : Identifier
    ;

structDeclaration
        : '__extension__'? ('struct'|'union') Identifier? '{' fieldDeclarations? '}' gccAttributeSpecifierFuncs?
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
    |   asmStatement
    |   'goto' Identifier ';'
    |   'goto' '*' unaryExpression ';'
    |   'return' commaExpression? ';'
    |   'continue' ';'
    |   'break' ';'
    |   ';'
    ;

asmStatement
    :   '__asm__' '__volatile__'? '(' StringLiteral (':' asmPart?)* ')' ';'
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

switchStatement
    : 'switch' '(' commaExpression ')' '{' (caseLabel | label)* defaultLabel? '}'
    ;

caseLabel
    : 'case' literal ':' statement*
    | 'case' Identifier ':' statement*
    ;

defaultLabel
    : 'default' ':' statement*
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
    :   logicalOrExpression ('?' commaExpression ':' conditionalExpression)?
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
    |   '(' typeSpecifier ')' (castExpression | arrayInitializer | structInitializer)
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
    |   '__builtin_offsetof' '(' type ',' postfixExpression ')'
    ;

sizeofOrAlignof
    :   'sizeof'
    |   '_Alignof'
    ;

modifiersWithoutVariable
    : '(' modifiersWithoutVariable')'
    | (typeModifier | typeQualifier) modifiersWithoutVariable
    | modifiersWithoutVariable arrayOneDim
    | modifiersWithoutVariable functionParameters
    | /*empty*/
    ;

typeSpecifier
    : type modifiersWithoutVariable
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
    |   '('commaExpression ')'
    ;


literal
    :   integerConstant
    |   FloatingConstant
    |   CharacterConstant
    ;


argumentList
    : assignmentExpression (',' assignmentExpression)*
    ;

typeDefinition
    : '__extension__'? 'typedef' typeQualifier* type fieldDeclarator gccAttributeSpecifierFuncs? ';'
    | '__extension__'? 'typedef' typeQualifier* type? function gccAttributeSpecifierFuncs? ';'
    ;

visualExtension
    : '__cdecl'
    ;

gccDeclaratorExtension2
    :  (gccAttributeSpecifierFuncs | '__asm__' '(' StringLiteral+ ')')+
    ;

//Identifier is usually __aligned__ but it is not keyword
gccAttributeOrAlignas
    :   '__attribute__' '(' '(' Identifier ('(' attributeAlignment ')')? ')' ')'
    |   '_Alignas' '(' conditionalExpression ')'
    ;

attributeAlignment
    :   '__alignof__' '(' typeName ')'
    |   conditionalExpression
    ;

gccAttributeSpecifierFuncs
    :   '__attribute__' '(' '(' gccAttributeList? ')' ')'
    ;

gccAttributeList
    :   gccAttribute (',' gccAttribute)*
    ;

gccAttribute
    : Identifier ('(' (StringLiteral | (Identifier | integerConstant) ( ',' integerConstant )*) ')')?
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
