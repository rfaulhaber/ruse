use nom_locate::LocatedSpan;
use std::borrow::Cow;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug)]
pub struct Token<'t> {
    pub kind: TokenKind<'t>,
    pub span: Span<'t>,
}

/// Represents the distinct kinds of tokens recognized by an R7RS Scheme lexer.
///
/// Assumes whitespace (` `, `\t`, `\n`, etc.) and comments (`;...` and `#|...|#`)
/// have already been skipped by the lexing process.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind<'a> {
    // --- Structural Punctuation ---
    /// Left parenthesis: `(`
    LParen,
    /// Right parenthesis: `)`
    RParen,
    /// Vector start: `#(`
    VectorStart,
    /// Bytevector start: `#u8(`
    BytevectorStart,
    /// The dot `.` used for improper lists or potentially in identifiers/numbers.
    /// The parser needs context to distinguish its use.
    Dot,

    // --- Abbreviations (Quote Syntax) ---
    /// Quote abbreviation: `'`
    QuoteTick,
    /// Quasiquote abbreviation: `` ` ``
    QuasiquoteTick,
    /// Unquote abbreviation: `,` (only valid within quasiquote)
    UnquoteTick,
    /// Unquote-splicing abbreviation: `,@` (only valid within quasiquote)
    UnquoteSplicing,

    // --- Identifiers ---
    /// An identifier (variable, keyword, etc.).
    /// The `Cow` allows storing either a borrowed slice (common case)
    /// or an owned `String` (for identifiers needing processing, like `|...|` escaping).
    Identifier(Cow<'a, str>),

    // --- Literals ---
    /// Boolean literal true: `#t` or `#true`
    BooleanTrue,
    /// Boolean literal false: `#f` or `#false`
    BooleanFalse,

    /// A numeric literal (integer, real, rational, complex).
    /// Includes all R7RS syntax like prefixes (`#b`, `#o`, `#d`, `#x`, `#e`, `#i`),
    /// decimals, exponents, `/` for rationals, `+i`/`-i`/`@` for complex.
    /// The lexer provides the *full slice* representing the number;
    /// detailed parsing into a numeric type happens later (parser or eval).
    Number(&'a str),

    /// A character literal (e.g., `#\a`, `#\space`, `#\newline`, `#\xHH`).
    /// The `char` value is the result *after* processing the name or hex code.
    Character(char),

    /// A string literal (e.g., `"hello \n world"`).
    /// The `String` value is the result *after* processing escape sequences.
    String(String), // Owned String because escapes are processed

    // --- Datum Labels/References (for shared structure) ---
    /// Datum label definition prefix: `#<digits>=` (e.g., `#123=`).
    /// Stores the numeric label identifier.
    DatumLabelDefine(u64),
    /// Datum label reference: `#<digits>#` (e.g., `#123#`).
    /// Stores the numeric label identifier.
    DatumLabelRef(u64),

    // --- Directives ---
    /// A directive indicator, like `# !`. Often followed by an identifier token.
    /// Examples: `# !fold-case`, `# !no-fold-case`.
    SharpBang,

    // --- End of Input ---
    /// Special token indicating the end of the input stream has been reached.
    Eof,
}
