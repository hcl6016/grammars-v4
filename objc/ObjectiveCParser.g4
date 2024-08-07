/*
Objective-C grammar.
The MIT License (MIT).
Copyright (c) 2016-2017, Alex Petuschak (alex@swiftify.io).
Copyright (c) 2016-2017, Ivan Kochurkin (kvanttt@gmail.com).
Converted to ANTLR 4 by Terence Parr; added @property and a few others.
Updated June 2014, Carlos Mejia.  Fix try-catch, add support for @( @{ @[ and blocks
June 2008 Cedric Cuche

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

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar ObjectiveCParser;

options {
    tokenVocab = ObjectiveCLexer;
}

translationUnit
    : topLevelDeclaration* EOF
    ;

topLevelDeclaration
    : importDeclaration
    | functionDeclaration
    | declaration
    | classInterface
    | classImplementation
    | categoryInterface
    | categoryImplementation
    | protocolDeclaration
    | protocolDeclarationList
    | classDeclarationList
    | functionDefinition
    | ';'
    ;

importDeclaration
    : '@import' identifier ';'
    ;

classInterface
    : IB_DESIGNABLE? '@interface' className = genericTypeSpecifier (
        ':' superclassName = identifier
    )? (LT (protocolList | genericConformanceList) GT)* instanceVariables? interfaceDeclarationList? '@end'
    ;

categoryInterface
    : '@interface' className = genericTypeSpecifier LP categoryName = identifier? RP (
        LT protocolList GT
    )? instanceVariables? interfaceDeclarationList? '@end'
    ;

classImplementation
    : '@implementation' className = genericTypeSpecifier (':' superclassName = identifier)? instanceVariables? implementationDefinitionList? '@end'
    ;

categoryImplementation
    : '@implementation' className = genericTypeSpecifier LP categoryName = identifier RP implementationDefinitionList? '@end'
    ;

genericTypeSpecifier
    : identifier (LT (protocolList | genericConformanceList) GT)?
    ;

genericConformanceList
    : genericConformance (',' genericConformance)*
    ;

genericConformance
    : genericsType = declarationSpecifiers (':' genericsParentType = declarationSpecifiers)?
    ;

protocolDeclaration
    : macro? '@protocol' protocolName (LT protocolList GT)? protocolDeclarationSection* '@end'
    ;

protocolDeclarationSection
    : modifier = (REQUIRED | OPTIONAL) interfaceDeclarationList*
    | interfaceDeclarationList+
    ;

protocolDeclarationList
    : '@protocol' protocolList ';'
    ;

classDeclarationList
    : '@class' genericTypeSpecifier (',' genericTypeSpecifier)* ';'
    ;

protocolList
    : protocolName (',' protocolName)*
    ;

propertyDeclaration
    : '@property' (LP propertyAttributesList RP)? ibOutletQualifier? IB_INSPECTABLE? fieldDeclaration
    ;

propertyAttributesList
    : propertyAttribute (',' propertyAttribute)*
    ;

propertyAttribute
    : ATOMIC
    | NONATOMIC
    | STRONG
    | WEAK
    | RETAIN
    | ASSIGN
    | UNSAFE_UNRETAINED
    | COPY
    | READONLY
    | READWRITE
    | GETTER '=' identifier
    | SETTER '=' identifier ':'
    | nullabilitySpecifier
    | identifier
    ;

protocolName
    : LT protocolList GT
    | ('__covariant' | '__contravariant')? identifier
    ;

instanceVariables
    : '{' visibilitySection* '}'
    ;

visibilitySection
    : accessModifier fieldDeclaration*
    | fieldDeclaration+
    ;

accessModifier
    : PRIVATE
    | PROTECTED
    | PACKAGE
    | PUBLIC
    ;

interfaceDeclarationList
    : (
        declaration
        | classMethodDeclaration
        | instanceMethodDeclaration
        | propertyDeclaration
        | functionDeclaration
        | ';'
    )+
    ;

classMethodDeclaration
    : '+' methodDeclaration
    ;

instanceMethodDeclaration
    : '-' methodDeclaration
    ;

methodDeclaration
    : methodType? methodSelector macro* ';'
    ;

implementationDefinitionList
    : (
        functionDefinition
        | declaration
        | classMethodDefinition
        | instanceMethodDefinition
        | propertyImplementation
    )+
    ;

classMethodDefinition
    : '+' methodDefinition
    ;

instanceMethodDefinition
    : '-' methodDefinition
    ;

methodDefinition
    : methodType? methodSelector initDeclaratorList? ';'? compoundStatement
    ;

methodSelector
    : selector
    | keywordDeclarator+ (',' '...')?
    ;

keywordDeclarator
    : selector? ':' methodType* arcBehaviourSpecifier? identifier
    ;

selector
    : identifier
    | 'return'
    ;

methodType
    : LP typeName RP
    ;

propertyImplementation
    : '@synthesize' propertySynthesizeList ';'
    | '@dynamic' propertySynthesizeList ';'
    ;

propertySynthesizeList
    : propertySynthesizeItem (',' propertySynthesizeItem)*
    ;

propertySynthesizeItem
    : identifier ('=' identifier)?
    ;

blockType
    : NS_NOESCAPE? nullabilitySpecifier? typeSpecifier nullabilitySpecifier? LP NS_NOESCAPE? '^'
        nullabilitySpecifier? RP blockParameters?
    ;

genericsSpecifierList
    : LT (genericsSpecifier (',' genericsSpecifier)*)? GT
    ;
    
genericsSpecifier
    : genericsType = typeSpecifier (':' genericsConformanceType = typeSpecifier)?
    ;

dictionaryExpression
    : '@' '{' (dictionaryPair (',' dictionaryPair)* ','?)? '}'
    ;

dictionaryPair
    : castExpression ':' expression
    ;

arrayExpression
    : '@' '[' (expressions ','?)? ']'
    ;

boxExpression
    : '@' LP expression RP
    | '@' (constant | identifier)
    ;

blockParameters
    : LP ((typeVariableDeclaratorOrName | 'void') (',' typeVariableDeclaratorOrName)*)? RP
    ;

typeVariableDeclaratorOrName
    : typeVariableDeclarator
    | typeName
    ;

blockExpression
    : '^' typeSpecifier? nullabilitySpecifier? blockParameters? compoundStatement
    ;

messageExpression
    : '[' receiver messageSelector ']'
    ;

receiver
    : expression
    | typeSpecifier
    ;

messageSelector
    : selector
    | keywordArgument+
    ;

keywordArgument
    : selector? ':' keywordArgumentType (',' keywordArgumentType)*
    ;

keywordArgumentType
    : expressions nullabilitySpecifier? ('{' initializerList '}')?
    ;

selectorExpression
    : '@selector' LP selectorName RP
    ;

selectorName
    : selector
    | UNDERSCORE
    | (selector? ':')+
    ;

protocolExpression
    : '@protocol' LP protocolName RP
    ;

encodeExpression
    : '@encode' LP typeName RP
    ;

typeVariableDeclarator
    : declarationSpecifiers declarator
    ;

throwStatement
    : '@throw' LP identifier RP
    | '@throw' expression
    ;

tryBlock
    : '@try' tryStatement = compoundStatement catchStatement* (
        '@finally' finallyStatement = compoundStatement
    )?
    ;

catchStatement
    : '@catch' LP typeVariableDeclarator RP compoundStatement
    ;

synchronizedStatement
    : '@synchronized' LP expression RP compoundStatement
    ;

autoreleaseStatement
    : '@autoreleasepool' compoundStatement
    ;

functionDeclaration
    : functionSignature ';'
    ;

functionDefinition
    : functionSignature compoundStatement
    ;

functionSignature
    : declarationSpecifiers? identifier (LP parameterList? RP) attributeSpecifier?
    ;

attribute
    : attributeName attributeParameters?
    ;

attributeName
    : 'const'
    | identifier
    ;

attributeParameters
    : LP attributeParameterList? RP
    ;

attributeParameterList
    : attributeParameter (',' attributeParameter)*
    ;

attributeParameter
    : attribute
    | constant
    | stringLiteral
    | attributeParameterAssignment
    ;

attributeParameterAssignment
    : attributeName '=' (constant | attributeName | stringLiteral)
    ;

declaration
    : (
        functionCallExpression
        | enumDeclaration
        | varDeclaration
        | typedefDeclaration
    ) macro? ';'
    ;

functionCallExpression
    : attributeSpecifier? identifier attributeSpecifier? LP declarator RP
    ;

enumDeclaration
    : attributeSpecifier? TYPEDEF? (enumSpecifier identifier | nsEnumOrOptionSpecifier)
    ;

varDeclaration
    : declarationSpecifiers initDeclaratorList?
    ;

typedefDeclaration
    : attributeSpecifier? TYPEDEF declarationSpecifiers typeDeclaratorList?
    ;

typeDeclaratorList
    : declarator (',' declarator)*
    ;

declarationSpecifiers
    : (
        storageClassSpecifier
        | attributeSpecifier
        | arcBehaviourSpecifier
        | nullabilitySpecifier
        | ibOutletQualifier
        | NS_NOESCAPE
        | typePrefix
        | typeQualifier
    )* typeSpecifier (
        attributeSpecifier
    )*
    ;

attributeSpecifier
    : '__attribute__' LP LP attribute (',' attribute)* RP RP
    ;

initDeclaratorList
    : initDeclarator (',' initDeclarator)*
    ;

initDeclarator
    : declarator ('=' initializer)?
    ;

structOrUnionSpecifier
    : ('struct' | 'union') (identifier | identifier? '{' fieldDeclaration+ '}')
    ;

fieldDeclaration
    : declarationSpecifiers fieldDeclaratorList macro? ';'
    ;

ibOutletQualifier
    : IB_OUTLET_COLLECTION LP identifier RP
    | IB_OUTLET
    ;

arcBehaviourSpecifier
    : WEAK_QUALIFIER
    | STRONG_QUALIFIER
    | AUTORELEASING_QUALIFIER
    | UNSAFE_UNRETAINED_QUALIFIER
    ;

nullabilitySpecifier
    : NULL_UNSPECIFIED
    | NULLABLE
    | NONNULL
    | NULL_RESETTABLE
    ;

storageClassSpecifier
    : AUTO
    | REGISTER
    | STATIC
    | EXTERN
    ;

typePrefix
    : BRIDGE
    | BRIDGE_TRANSFER
    | BRIDGE_RETAINED
    | BLOCK
    | INLINE
    | NS_INLINE
    | KINDOF
    ;

typeQualifier
    : CONST
    | VOLATILE
    | RESTRICT
    | protocolQualifier
    ;

protocolQualifier
    : 'in'
    | 'out'
    | 'inout'
    | 'bycopy'
    | 'byref'
    | 'oneway'
    ;
    
typeSpecifierModifier
    : 'short'
    | 'long'
    | 'signed'
    | 'unsigned'
    ;

typeSpecifier
    : 'void' typeQualifier*
    | typeSpecifierModifier* 'char' typeQualifier*
    | typeSpecifierModifier* 'short' typeQualifier*
    | typeSpecifierModifier* 'int' typeQualifier*
    | typeSpecifierModifier* 'long' typeQualifier*
    | typeSpecifierModifier* 'float' typeQualifier*
    | typeSpecifierModifier* 'double' typeQualifier*
    | typeofExpression
    | structOrUnionSpecifier
    | enumSpecifier
    | nsEnumOrOptionSpecifier
    | 'id' (LT protocolList GT)? (arcBehaviourSpecifier | nullabilitySpecifier | typeQualifier)*
    | genericTypeSpecifier (arcBehaviourSpecifier | nullabilitySpecifier | typeQualifier)*
    | identifier (arcBehaviourSpecifier | nullabilitySpecifier | typeQualifier)*
    | typeSpecifier '*' (arcBehaviourSpecifier | nullabilitySpecifier | typeQualifier)*
    ;

typeofExpression
    : TYPEOF (LP expression RP)
    ;

fieldDeclaratorList
    : fieldDeclarator (',' fieldDeclarator)*
    ;

fieldDeclarator
    : declarator
    | declarator? ':' constant
    ;

enumSpecifier
    : 'enum' (identifier? ':' typeName)? (
        identifier ('{' enumeratorList '}')?
        | '{' enumeratorList '}'
    )
    ;
    
nsEnumOrOptionSpecifier
    : ('NS_OPTIONS' | 'NS_ENUM' | 'NS_CLOSED_ENUM' | 'NS_ERROR_ENUM') LP typeName ',' identifier RP ('{' enumeratorList '}')?
    ;

enumeratorList
    : enumerator (',' enumerator)* ','?
    ;

enumerator
    : enumeratorIdentifier ('=' expression)?
    ;

enumeratorIdentifier
    : identifier
    | 'default'
    ;

declarator
    : (identifier | LP declarator RP) declaratorSuffix*
    | LP '^' nullabilitySpecifier? identifier? RP blockParameters
    ;

declaratorSuffix
    : '[' constantExpression? ']'
    ;

parameterList
    : parameterDeclarationList (',' '...')?
    ;

macro
    : identifier (LP primaryExpression (',' primaryExpression)* RP)?
    | NS_UNAVAILABLE
    | NS_SWIFT_NAME LP (swiftAliasExpression | swiftSelectorExpression) RP
    | API_AVAILABLE LP apiAvailableOsVersion (',' apiAvailableOsVersion)* RP
    | API_UNAVAILABLE LP identifier (',' identifier)* RP
    | NS_SWIFT_UNAVAILABLE LP stringLiteral RP
    | ATTRIBUTE LP LP clangAttribute (',' clangAttribute)* RP RP
    ;

// A list of __attribute__ are elaborated https://nshipster.com/__attribute__/
clangAttribute
    : identifier
    | identifier LP clangAttributeArgument (',' clangAttributeArgument)* RP
    ;
    
clangAttributeArgument
    : identifier
    | DECIMAL_LITERAL
    | stringLiteral
    | identifier '=' version
    | identifier '=' stringLiteral
    ;
    
swiftAliasExpression
    : identifier ('.' identifier)*
    ;

swiftSelectorExpression
    : identifier LP (swiftSelector ':')* RP
    ;
    
// Swift selector may use reserved words
swiftSelector
    : identifier
    | UNDERSCORE
    | 'for'
    ;

apiAvailableOsVersion
    : identifier LP version RP
    ;
    
version
    : FLOATING_POINT_LITERAL
    | DECIMAL_LITERAL ('.' DECIMAL_LITERAL)*
    ;

arrayInitializer
    : '{' (expressions ','?)? '}'
    ;

structInitializer
    : '{' ('.' expression (',' '.' expression)* ','?)? '}'
    ;

initializerList
    : initializer (',' initializer)* ','?
    ;

typeName
    : declarationSpecifiers abstractDeclarator?
    | blockType
    ;

abstractDeclarator
    : LP abstractDeclarator? RP abstractDeclaratorSuffix+
    | ('[' constantExpression? ']')+
    ;

abstractDeclaratorSuffix
    : '[' constantExpression? ']'
    | LP parameterDeclarationList? RP
    ;

parameterDeclarationList
    : parameterDeclaration (',' parameterDeclaration)*
    ;

parameterDeclaration
    : declarationSpecifiers declarator
    | 'void'
    ;

statement
    : labeledStatement ';'?
    | compoundStatement ';'?
    | selectionStatement ';'?
    | iterationStatement ';'?
    | jumpStatement ';'?
    | synchronizedStatement ';'?
    | autoreleaseStatement ';'?
    | throwStatement ';'?
    | tryBlock ';'?
    | expressions ';'?
    | ';'
    ;

labeledStatement
    : identifier ':' statement
    ;

rangeExpression
    : constantExpression ('...' constantExpression)?
    ;

compoundStatement
    : '{' (declaration | statement)* '}'
    ;

selectionStatement
    : IF LP expression RP ifBody = statement (ELSE elseBody = statement)?
    | switchStatement
    ;

switchStatement
    : 'switch' LP expression RP switchBlock
    ;

switchBlock
    : '{' switchSection* '}'
    ;

switchSection
    : switchLabel+ statement+
    ;

switchLabel
    : 'case' (rangeExpression | LP rangeExpression RP) ':'
    | 'default' ':'
    ;

iterationStatement
    : whileStatement
    | doStatement
    | forStatement
    | forInStatement
    ;

whileStatement
    : 'while' LP expression RP statement
    ;

doStatement
    : 'do' statement 'while' LP expression RP ';'
    ;

forStatement
    : 'for' LP forLoopInitializer? ';' expression? ';' expressions? RP statement
    ;

forLoopInitializer
    : declarationSpecifiers initDeclaratorList
    | expressions
    ;

forInStatement
    : 'for' LP typeVariableDeclarator 'in' expression? RP statement
    ;

jumpStatement
    : GOTO identifier
    | CONTINUE
    | BREAK
    | RETURN expression?
    ;

expressions
    : expression (',' expression)*
    ;

expression
    : castExpression
    | expression op = (MUL | DIV | MOD) expression
    | expression op = (ADD | SUB) expression
    | expression (LT LT | GT GT) expression
    | expression op = (LE | GE | LT | GT) expression
    | expression op = (NOTEQUAL | EQUAL) expression
    | expression op = BITAND expression
    | expression op = BITXOR expression
    | expression op = BITOR expression
    | expression op = AND expression
    | expression op = OR expression
    | expression QUESTION trueExpression = expression? COLON falseExpression = expression
    | LP compoundStatement RP
    | unaryExpression assignmentOperator assignmentExpression = expression
    ;

assignmentOperator
    : '='
    | '*='
    | '/='
    | '%='
    | '+='
    | '-='
    | '<<='
    | '>>='
    | '&='
    | '^='
    | '|='
    ;

castExpression
    : unaryExpression
    | (LP typeName RP) (castExpression | initializer)
    ;

initializer
    : expression
    | arrayInitializer
    | structInitializer
    ;

constantExpression
    : identifier
    | constant
    ;

unaryExpression
    : postfixExpression
    | SIZEOF (unaryExpression | LP typeSpecifier RP)
    | op = (INC | DEC) unaryExpression
    | unaryOperator castExpression
    ;

unaryOperator
    : '&'
    | '*'
    | '+'
    | '-'
    | '~'
    | BANG
    ;

postfixExpression
    : primaryExpression postfix*
    | postfixExpression (DOT | STRUCTACCESS) identifier postfix* // TODO: get rid of property and postfix expression.
    ;

postfix
    : LBRACK expression RBRACK
    | LP argumentExpressionList? RP
    | LP (COMMA | macroArguments += ~RP)+ RP
    | op = (INC | DEC)
    ;

argumentExpressionList
    : argumentExpression (',' argumentExpression)*
    ;

argumentExpression
    : expression
    | typeSpecifier
    ;

primaryExpression
    : identifier
    | constant
    | stringLiteral
    | LP expression RP
    | messageExpression
    | selectorExpression
    | protocolExpression
    | encodeExpression
    | dictionaryExpression
    | arrayExpression
    | boxExpression
    | blockExpression
    ;

constant
    : HEX_LITERAL
    | OCTAL_LITERAL
    | BINARY_LITERAL
    | ('+' | '-')? DECIMAL_LITERAL
    | ('+' | '-')? FLOATING_POINT_LITERAL
    | CHARACTER_LITERAL
    | NIL
    | NULL_
    | YES
    | NO
    | TRUE
    | FALSE
    ;

stringLiteral
    : (STRING_START (STRING_VALUE | STRING_NEWLINE)* STRING_END)+
    ;

identifier
    : IDENTIFIER
    | BOOL
    | Class
    | BYCOPY
    | BYREF
    | ID
    | IMP
    | IN
    | INOUT
    | ONEWAY
    | OUT
    | PROTOCOL_
    | SEL
    | SELF
    | SUPER
    | ATOMIC
    | NONATOMIC
    | RETAIN
    | REGISTER
    | AUTORELEASING_QUALIFIER
    | BLOCK
    | BRIDGE_RETAINED
    | BRIDGE_TRANSFER
    | COVARIANT
    | CONTRAVARIANT
    | DEPRECATED
    | KINDOF
    | UNUSED
    | NS_INLINE
    | NS_ENUM
    | NS_OPTIONS
    | NS_SWIFT_NAME
    | NULL_UNSPECIFIED
    | NULLABLE
    | NONNULL
    | NULL_RESETTABLE
    | ASSIGN
    | COPY
    | GETTER
    | SETTER
    | STRONG
    | READONLY
    | READWRITE
    | WEAK
    | UNSAFE_UNRETAINED
    | IB_OUTLET
    | IB_OUTLET_COLLECTION
    | IB_INSPECTABLE
    | IB_DESIGNABLE
    ;
