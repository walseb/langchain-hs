---
sidebar_position: 10
---

# Text Splitter

## Overview

The `TextSplitter` is a utility that allows you to split text into smaller chunks. This is particularly useful when dealing with large documents or when you need to process text in smaller segments.

langchain-hs provides `splitText` function that can be used to split text into smaller chunks. The function takes a string as input, `CharacterSplitterOps` type and returns a list of strings, each representing a chunk of the original text.

```haskell
splitText :: CharacterSplitterOps -> Text -> [Text]
```

```haskell
data CharacterSplitterOps = CharacterSplitterOps
  { chunkSize :: Int
  , separator :: Text
  }
  deriving (Show, Eq)
```