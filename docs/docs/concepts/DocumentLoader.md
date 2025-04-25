---
sidebar_position: 2
---

# Document Loaders

The `DocumentLoader` provides a unified interface for loading `Document` Types from various sources.

## Document

In langchain-hs, a `Document` is a Haskell type with two fields 
  - pageContent = The textual content of the document
  - metadata = A map of text and `Value` containing extra information about the document. for e.g page number, links.

Document as Monoid instance, so two documents can be appended together.

## BaseLoader

DocumentLoader provides a typeclass called `BaseLoader`. Every BaseLoader should provide implementation of two functions:

 - load 
 - loadAndSplit

## Integrations 

Right now, langchain-hs provides below integrations, with more integrations planned in the roadmap:

- FileLoader = Load simple text files (uses readFile under the hood).
- PdfLoader = Load pdf files (uses [pdf-toolbox](pdf-toolbox-document) under the hood).
- DirectoryLoader = Takes a filePath of a directory and *recursively* reads all possible files. You can pass in recursion depth via params.

## Examples 

- FileLoader

```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import Langchain.DocumentLoader.FileLoader
import Langchain.DocumentLoader.Core

runApp :: IO ()
runApp = do
    let loader = FileLoader "/home/user/haskell/sample-proj/README.md"
    docs <- load loader
    chunks <- loadAndSplit loader
    print docs
    print chunks
```

- PdfLoader 

```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.PdfLoader 

runApp :: IO ()
runApp = do
    let loader = PdfLoader  "/home/user/Documents/TS/langchain/SOP.pdf"
    docs <- load loader
    chunks <- loadAndSplit loader
    print docs
    print chunks
```

- DirectoryLoader

```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import Langchain.DocumentLoader.Core
import Langchain.DocumentLoader.DirectoryLoader

runApp :: IO ()
runApp = do
    let loader = DirectoryLoader { 
        dirPath = "/home/user/Documents/TS/langchain"
      , directoryLoaderOptions = defaultDirectoryLoaderOptions {
          recursiveDepth = Just 2
      }
    }
    docs <- load loader
    chunks <- loadAndSplit loader
    print docs
    print chunks
```

### DirectoryLoaderOptions

- recursiveDepth :: Maybe Int = Decide the depth of directory you should go in. The default is unlimited.
- extensions :: [String] = List of extensions that want to load. For e.g [".txt", ".pdf"]
- excludeHidden = Exclude hidden files or folders
- useMultithreading = Apply multithreading, default is off.


These documents are useful to embed into vector store and build RAG tools.
