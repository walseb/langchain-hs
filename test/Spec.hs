
import Test.Tasty
import qualified Test.Langchain.PromptTemplate as PromptTemplateTest
import qualified Test.Langchain.LLM.Core as LLMCoreTest
import qualified Test.Langchain.LLM.Ollama as OllamaLLMTest
import qualified Test.Langchain.OutputParser.Core as OutputParserTest
import qualified Test.Langchain.TextSplitter.Character as TextSplitterTest
import qualified Test.Langchain.DocumentLoader.Core as DocumentLoaderTest
import qualified Test.Langchain.Memory.Core as MemoryTest
import qualified Test.Langchain.VectorStore.Core as VectorStoreTest
import qualified Test.Langchain.Embeddings.Core as EmbeddingsTest

main :: IO ()
main = defaultMain $ testGroup "Langchain" [ 
        LLMCoreTest.tests
      -- , OllamaLLMTest.tests
      -- , PromptTemplateTest.tests
      -- , OutputParserTest.tests
      -- , TextSplitterTest.tests
      -- , DocumentLoaderTest.tests
      -- , MemoryTest.tests
      -- , VectorStoreTest.tests
      , EmbeddingsTest.tests
    ]
