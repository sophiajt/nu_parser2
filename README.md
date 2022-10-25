# Replacement for Nushell's parser

This experiment hopes to grow to replace Nushell's current parser.

TODO:

- [x] Tables
- [x] Units
- [ ] Binary literals
- [x] Floats
- [x] Hex, octal, binary integer values
- [x] Ranges
- [ ] String interpolation
- [ ] Cell paths
- [ ] Row conditions
- [ ] Records
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

# Adding new grammar element

## Lexing

The lexing process tokenizes text into `TokenTypes` that will be used later in parsing.
These changes need to happen in lexer.rs.

1. Add a `TokenType` to the enum.
2. Add a way to parse the new `TokenType` like `lex_number()`, `lex_symbol()`, `lex_newline()`, etc.

## Parsing

Parsing is similar to lexing in that it segments text into `NodeTypes` with location spans, in order to understand the meaning of the source code, by breaking down the component parts, looking for specific sequences or rules, for execution.

When parsing is executed, `parse()` is called, which in turn calls `program()`, which in turn calls `code_block()`. It's important to follow this trail to ensure that your new grammar is parsed. If your new grammar is not called out in one of the conditional items in `code_block()`, then your new grammar won't be parsed.

For instance, if you were adding a new item to locate the three stooges, `moe`, `larry`, `curly`. You'd have to have call a function like `self.is_stooge()` somewhere in the code block to check if the current text is a stooge or not. Then, separately, you need to create a `stooge()` function in order to create a `NodeId` for the current stooge.

Note that `rcurly` and `lcurly` do not refer to the last stooge. :)

These changes need to happen in parser.rs.

1. Add a `NodeType` to the enum.
2. Add a `is_your_node_type` to the code to detect if the current text is your new type.
3. Add a `your_token_type` to the code to create a `NodeId` with spans.

