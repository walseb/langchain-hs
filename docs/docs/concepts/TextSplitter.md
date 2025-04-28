---
sidebar_position: 10
---

# Text Splitter
`TextSplitter` typeclass provides an interface to split text into smaller chunks. This is useful for processing large texts, such as documents or articles, into manageable pieces.
It is used in conjunction with the `VectorStore` typeclass to split documents into smaller chunks for embedding and retrieval. The `TextSplitter` typeclass defines methods for splitting text based on various criteria, such as size, number of tokens, or specific delimiters.
It also provides methods for managing the metadata associated with the chunks, such as their IDs and distances. The `TextSplitter` typeclass is designed

`splitText` to be flexible and extensible, allowing developers to implement their own text splitters with custom splitting algorithms and criteria. `CharacterSplitterOps` is a type. `defaultCharacterSplitterOps` is a default value for `CharacterSplitterOps`.