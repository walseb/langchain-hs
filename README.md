# langchain-haskell

Langchain is a Haskell library designed to facilitate interaction with Language Model APIs such as OpenAI and Ollama. 
It provides abstractions over different language model APIs, allowing users to easily call these APIs 
and process their responses.

## Usage

To use the Langchain library, you will need to:

1. Import the necessary modules.
2. Use the `call` function from the `LLM` typeclass to interact with the language model.

Here's a basic example:

```haskell
import Langchain.LLM
import Langchain.OpenAI
import Data.Text (Text)
import qualified Data.List.NonEmpty as NonEmpty

main :: IO ()
main = do
    let openAI = OpenAI (Just <your-api-key-here>)
    let messages = NonEmpty.fromList [
            Message User "Why is sky blue?"
        ]
    response <- call openAI messages Nothing
    putStrLn (show response)
```

### Project status

The project is currently under development and is in premature state.

### License

This project is licensed under the MIT License.

### Contributing

Contributions are welcome! Please feel free to submit a pull request or open an issue.
