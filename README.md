# Replacement for Nushell's parser

This experiment hopes to grow to replace Nushell's current parser.

TODO:

- [x] Tables
- [ ] Units
- [ ] Binary literals
- [x] Floats
- [x] Hex, octal, binary integer values
- [ ] Ranges
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