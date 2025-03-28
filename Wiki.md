TODO:
 - https://github.com/tmc/langchaingo/tree/main/memory
   Have sqlite3 memory
 - https://python.langchain.com/docs/concepts/output_parsers/
   Add some output parser support, at least for json
 - https://python.langchain.com/docs/concepts/prompt_templates/
   Add prompt_templates, here, we can use hashmap to pass key value pairs
 - https://python.langchain.com/docs/concepts/agents/
   Add some agents
 - https://python.langchain.com/docs/concepts/rag/
   Rag is the goal
 - https://python.langchain.com/docs/concepts/retrieval/
   No idea what's happening here
 - https://python.langchain.com/docs/concepts/retrievers/
   Implement BaseRetriver Interface with function that takes a query (Text) and returns a [Documents]
 - https://python.langchain.com/docs/concepts/vectorstores/
   Have vectorstores interface with add_documents, delete and similarity_search
 - https://python.langchain.com/docs/concepts/embedding_models/
   Have an Embeddings interface with embed_documents and embed_query
 - https://python.langchain.com/docs/concepts/text_splitters/
   See how we can make text splitters more abstract or scalable. 
   Have document structured based splits, Markdown split, HTML split, JSON split, code split.
   Semantic split.
 - https://python.langchain.com/docs/integrations/document_loaders/web_base/
   Implement a web based document loader, that should at least extract text from web page.
 - https://python.langchain.com/docs/concepts/tools/
   Ollama-Haskell supports tool and tool_calls, pass/receive those from invoke and chat.
 - https://python.langchain.com/docs/concepts/structured_outputs/
   Use Ollama-haskell's `format`.
 - https://python.langchain.com/docs/concepts/multimodality/
   Use Ollama-haskell's image passing
 - https://python.langchain.com/docs/how_to/trim_messages/
    I want to provide some convience function for trimming message list.
