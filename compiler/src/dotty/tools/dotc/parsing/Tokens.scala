package dotty.tools
package dotc
package parsing

import collection.immutable.BitSet
import core.Decorators._

abstract class TokensCommon {
  val maxToken: Int

  type Token = Int
  type TokenSet = BitSet

  def tokenRange(lo: Int, hi: Int): TokenSet = BitSet(lo to hi: _*)

  def showTokenDetailed(token: Int) = debugString(token)

  def showToken(token: Int) = {
    val str = tokenString(token)
    if (isKeyword(token)) s"'$str'" else str
  }

  val tokenString, debugString = new Array[String](maxToken + 1)

  def enter(token: Int, str: String, debugStr: String = ""): Unit = {
    assert(tokenString(token) == null)
    tokenString(token) = str
    debugString(token) = if (debugStr.isEmpty) str else debugStr
  }

  /** special tokens */
  final val EMPTY = 0;             enter(EMPTY, "<empty>") // a missing token, used in lookahead
  final val ERROR = 1;             enter(ERROR, "erroneous token") // an erroneous token
  final val EOF = 2;               enter(EOF, "eof")

  /** literals */
  final val CHARLIT = 3;           enter(CHARLIT, "character literal")
  final val INTLIT = 4;            enter(INTLIT, "integer literal")
  final val LONGLIT = 5;           enter(LONGLIT, "long literal")
  final val FLOATLIT = 6;          enter(FLOATLIT, "float literal")
  final val DOUBLELIT = 7;         enter(DOUBLELIT, "double literal")
  final val STRINGLIT = 8;         enter(STRINGLIT, "string literal")
  final val STRINGPART = 9;        enter(STRINGPART, "string literal", "string literal part")
  //final val INTERPOLATIONID = 10;  enter(INTERPOLATIONID, "string interpolator")
  //final val SYMBOLLIT = 11;        enter(SYMBOLLIT, "symbol literal") // TODO: deprecate

  /** identifiers */
  final val IDENTIFIER = 12;       enter(IDENTIFIER, "identifier")
  //final val BACKQUOTED_IDENT = 13; enter(BACKQUOTED_IDENT, "identifier", "backquoted ident")

  /** alphabetic keywords */
  final val IF = 20;               enter(IF, "sofern")
  final val FOR = 21;              enter(FOR, "für")
  final val ELSE = 22;             enter(ELSE, "andernfalls")
  final val THIS = 23;             enter(THIS, "ich")
  final val NULL = 24;             enter(NULL, "nichtig")
  final val NEW = 25;              enter(NEW, "erzeuge")
  //final val WITH = 26;             enter(WITH, "with")
  final val SUPER = 27;            enter(SUPER, "vorgesetzte")
  //final val CASE = 28;             enter(CASE, "muster")
  //final val CASECLASS = 29;        enter(CASECLASS, "muster class")
  //final val CASEOBJECT = 30;       enter(CASEOBJECT, "muster object")
  //final val VAL = 31;              enter(VAL, "unveränderliche")
  final val ABSTRACT = 32;         enter(ABSTRACT, "implementationsdefiniert")
  final val FINAL = 33;            enter(FINAL, "letzte")
  final val PRIVATE = 34;          enter(PRIVATE, "vertraulich")
  final val PROTECTED = 35;        enter(PROTECTED, "zugangsbeschränkt")
  final val OVERRIDE = 36;         enter(OVERRIDE, "überschreiben")
  //final val IMPLICIT = 37;         enter(IMPLICIT, "stillschweigend")
  //final val VAR = 38;              enter(VAR, "opportunistisch")
  //final val DEF = 39;              enter(DEF, "verfahrensweise")
  //final val TYPE = 40;             enter(TYPE, "sorte")
  final val EXTENDS = 41;          enter(EXTENDS, "erbt")
  final val TRUE = 42;             enter(TRUE, "wahrhaftig")
  final val FALSE = 43;            enter(FALSE, "unrichtig")
  //final val OBJECT = 44;           enter(OBJECT, "entität")
  final val CLASS = 45;            enter(CLASS, "klasse")
  final val IMPORT = 46;           enter(IMPORT, "einführen")
  final val PACKAGE = 47;          enter(PACKAGE, "paket")
  //final val YIELD = 48;            enter(YIELD, "vorfahrtGewähren")
  final val DO = 49;               enter(DO, "mach")
  //final val TRAIT = 50;            enter(TRAIT, "charakterzug")
  //final val SEALED = 51;           enter(SEALED, "versiegelt")
  final val THROW = 52;            enter(THROW, "wirf")
  final val TRY = 53;              enter(TRY, "versuche")
  final val CATCH = 54;            enter(CATCH, "fang")
  final val FINALLY = 55;          enter(FINALLY, "endlich")
  final val WHILE = 56;            enter(WHILE, "während")
  final val RETURN = 57;           enter(RETURN, "retoure")
  //final val MATCH = 58;            enter(MATCH, "vergleiche")
  //final val LAZY = 59;             enter(LAZY, "faul")
  //final val THEN = 60;             enter(THEN, "gegebenenfalls")
  //final val FORSOME = 61;          enter(FORSOME, "gegeben") // TODO: deprecate
  //final val INLINE = 62;           enter(INLINE, "auffaltungsanleitung")

  /** special symbols */
  final val COMMA = 70;            enter(COMMA, "','")
  final val SEMI = 71;             enter(SEMI, "';'")
  final val DOT = 72;              enter(DOT, "'.'")
  //final val NEWLINE = 78;          enter(NEWLINE, "end of statement", "new line")
  //final val NEWLINES = 79;         enter(NEWLINES, "end of statement", "new lines")

  /** special keywords */
  //final val USCORE = 73;           enter(USCORE, "_")
  final val COLON = 74;            enter(COLON, ":")
  final val EQUALS = 75;           enter(EQUALS, "=")
  //final val LARROW = 76;           enter(LARROW, "<-")
  //final val ARROW = 77;            enter(ARROW, "=>")
  //final val SUBTYPE = 80;          enter(SUBTYPE, "<:")
  //final val SUPERTYPE = 81;        enter(SUPERTYPE, ">:")
  //final val HASH = 82;             enter(HASH, "#")
  final val AT = 83;               enter(AT, "@")
  //final val VIEWBOUND = 84;        enter(VIEWBOUND, "<%") // TODO: deprecate

  val keywords: TokenSet

  def isKeyword(token: Token): Boolean = keywords contains token

  /** parentheses */
  final val LPAREN = 90;           enter(LPAREN, "'('")
  final val RPAREN = 91;           enter(RPAREN, "')'")
  final val LBRACKET = 92;         enter(LBRACKET, "'['")
  final val RBRACKET = 93;         enter(RBRACKET, "']'")
  final val LBRACE = 94;           enter(LBRACE, "'{'")
  final val RBRACE = 95;           enter(RBRACE, "'}'")

  final val firstParen = LPAREN
  final val lastParen = RBRACE

  def buildKeywordArray(keywords: TokenSet) = {
    def start(tok: Token) = tokenString(tok).toTermName.start
    def sourceKeywords = keywords.toList.filter { (kw: Token) =>
      val ts = tokenString(kw)
      (ts != null) && !ts.contains(' ')
    }

    val lastKeywordStart = sourceKeywords.map(start).max

    val arr = Array.fill(lastKeywordStart + 1)(IDENTIFIER)
    for (kw <- sourceKeywords) arr(start(kw)) = kw
    (lastKeywordStart, arr)
  }
}

object Tokens extends TokensCommon {
  final val minToken = EMPTY
  final val maxToken = XMLSTART

  final val INTERPOLATIONID = 10;  enter(INTERPOLATIONID, "string interpolator")
  final val SYMBOLLIT = 11;        enter(SYMBOLLIT, "symbol literal") // TODO: deprecate

  final val BACKQUOTED_IDENT = 13; enter(BACKQUOTED_IDENT, "identifier", "backquoted ident")

  final val identifierTokens = BitSet(IDENTIFIER, BACKQUOTED_IDENT)

  def isIdentifier(token : Int) =
    token >= IDENTIFIER && token <= BACKQUOTED_IDENT

  /** alphabetic keywords */
  final val WITH = 26;             enter(WITH, "außerdem")
  final val CASE = 28;             enter(CASE, "muster")
  final val CASECLASS = 29;        enter(CASECLASS, "muster klasse")
  final val CASEOBJECT = 30;       enter(CASEOBJECT, "muster entität")
  final val VAL = 31;              enter(VAL, "unveränderliche")
  final val IMPLICIT = 37;         enter(IMPLICIT, "stillschweigend")
  final val VAR = 38;              enter(VAR, "opportunistisch")
  final val DEF = 39;              enter(DEF, "verfahrensweise")
  final val TYPE = 40;             enter(TYPE, "sorte")
  final val OBJECT = 44;           enter(OBJECT, "entität")
  final val YIELD = 48;            enter(YIELD, "vorfahrtGewähren")
  final val TRAIT = 50;            enter(TRAIT, "charakterzug")
  final val SEALED = 51;           enter(SEALED, "versiegelt")
  final val MATCH = 58;            enter(MATCH, "vergleiche")
  final val LAZY = 59;             enter(LAZY, "faul")
  final val THEN = 60;             enter(THEN, "gegebenenfalls")
  final val FORSOME = 61;          enter(FORSOME, "gegeben") // TODO: deprecate
  final val INLINE = 62;           enter(INLINE, "auffaltungsanleitung")

  /** special symbols */
  final val NEWLINE = 78;          enter(NEWLINE, "end of statement", "new line")
  final val NEWLINES = 79;         enter(NEWLINES, "end of statement", "new lines")

  /** special keywords */
  final val USCORE = 73;           enter(USCORE, "_")
  final val LARROW = 76;           enter(LARROW, "<-")
  final val ARROW = 77;            enter(ARROW, "=>")
  final val SUBTYPE = 80;          enter(SUBTYPE, "<:")
  final val SUPERTYPE = 81;        enter(SUPERTYPE, ">:")
  final val HASH = 82;             enter(HASH, "#")
  final val VIEWBOUND = 84;        enter(VIEWBOUND, "<%") // TODO: deprecate

  /** XML mode */
  final val XMLSTART = 96;         enter(XMLSTART, "$XMLSTART$<") // TODO: deprecate

  final val alphaKeywords = tokenRange(IF, INLINE)
  final val symbolicKeywords = tokenRange(USCORE, VIEWBOUND)
  final val symbolicTokens = tokenRange(COMMA, VIEWBOUND)
  final val keywords = alphaKeywords | symbolicKeywords

  final val allTokens = tokenRange(minToken, maxToken)

  final val simpleLiteralTokens = tokenRange(CHARLIT, STRINGLIT) | BitSet(TRUE, FALSE)
  final val literalTokens = simpleLiteralTokens | BitSet(INTERPOLATIONID, SYMBOLLIT, NULL)

  final val atomicExprTokens = literalTokens | identifierTokens | BitSet(
    USCORE, NULL, THIS, SUPER, TRUE, FALSE, RETURN, XMLSTART)

  final val canStartExpressionTokens = atomicExprTokens | BitSet(
    LBRACE, LPAREN, IF, DO, WHILE, FOR, NEW, TRY, THROW)

  final val canStartTypeTokens = literalTokens | identifierTokens | BitSet(
    THIS, SUPER, USCORE, LPAREN, AT)

  final val canStartBindingTokens = identifierTokens | BitSet(USCORE, LPAREN)

  final val templateIntroTokens = BitSet(CLASS, TRAIT, OBJECT, CASECLASS, CASEOBJECT)

  final val dclIntroTokens = BitSet(DEF, VAL, VAR, TYPE)

  final val defIntroTokens = templateIntroTokens | dclIntroTokens

  final val localModifierTokens = BitSet(
    ABSTRACT, FINAL, SEALED, IMPLICIT, INLINE, LAZY)

  final val accessModifierTokens = BitSet(
    PRIVATE, PROTECTED)

  final val modifierTokens = localModifierTokens | accessModifierTokens | BitSet(
    OVERRIDE)

  /** Is token only legal as start of statement (eof also included)? */
  final val mustStartStatTokens = defIntroTokens | modifierTokens | BitSet(
    IMPORT, PACKAGE)

  final val canStartStatTokens = canStartExpressionTokens | mustStartStatTokens | BitSet(
    AT, CASE)

  final val canEndStatTokens = atomicExprTokens | BitSet(
    TYPE, RPAREN, RBRACE, RBRACKET)

  final val numericLitTokens = BitSet(INTLIT, LONGLIT, FLOATLIT, DOUBLELIT)
}
