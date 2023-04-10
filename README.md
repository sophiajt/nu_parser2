# NOTE: this repo is for an experimental Nushell parser

# Replacement for Nushell's parser

This experiment hopes to grow to replace Nushell's current parser.

TODO:

- [x] Tables
- [x] Units
- [x] Binary literals
- [x] Floats
- [x] Hex, octal, binary integer values
- [x] Ranges
- [x] String interpolation
- [x] Cell paths
- [x] Row conditions (will be handled by the typechecker)
- [x] Records
- [x] Closures
- [ ] Shorthand flags in custom commands
- [ ] Default args in custom commands
- [ ] Aliases
- [x] Mixing symbols and barewords, eg) `ls *`
- [x] Barewords as implied strings. Or disallow this in 0.80. eg) `[larry moe curly]`
- [ ] Unescaping escaped strings
- [x] Comments
- [ ] Attaching comments to parsed nodes
- [ ] Date literals (new kind)
- [ ] Match expressions

# Adding new grammar element

## Lexing

The lexing process tokenizes text into `TokenTypes` that will be used later in parsing.
These changes need to happen in lexer.rs.

1. Add a `TokenType` to the enum.
2. Add a way to parse the new `TokenType` like `lex_number()`, `lex_symbol()`, `lex_newline()`, etc.

## Parsing

Parsing is similar to lexing in that it segments text into `NodeTypes` with location spans, in order to understand the meaning of the source code, by breaking down the component parts, looking for specific sequences or rules, for execution.

When parsing is executed, `parse()` is called, which in turn calls `program()`, which in turn calls `code_block()`. You can follow this much in the same way as using a grammar.

A few things to note about the parser:

* The parser has one step of look-ahead (via `.peek()`). If you need to make a parsing decision between two paths, for example between parsing a list and a table, you'll need to put the parsing choice right at the point the parser would know if it was one or the other. This grammar does not work like a PEG grammar, which may have a long lookahead.
* Everything is interned in the parser into a struct-of-arrays. The `NodeId` is the key for the components like node type, span, etc. 

These changes need to happen in parser.rs.

1. Add a `NodeType` to the enum.
2. Add a `is_your_node_type` (eg `is_match_expression`) to the code to detect if the current text is your new type.
3. Add a `your_node_type()` (eg `match_expression()`) to the code to create a `NodeId` with spans.

