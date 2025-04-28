---
sidebar_position: 7
---

# Prompt Template

Prompt templates are a powerful feature in Langchain that allow you to create dynamic prompts for your language models. They enable you to define a template with placeholders that can be filled with specific values at runtime, making it easy to generate customized prompts for different use cases.
This is particularly useful when you want to create prompts that are tailored to specific inputs or contexts, such as generating responses for different users or scenarios.

renderPrompt is a function that takes a prompt template and a set of values to fill in the placeholders. It returns the final prompt with the placeholders replaced by the provided values. This allows you to create dynamic prompts that can be easily customized for different situations.

`FewShotPromptTemplate` is a specific type of prompt template that allows you to create few-shot learning prompts. Few-shot learning is a technique where you provide a few examples of the desired output along with the input, allowing the model to learn from those examples and generate more accurate responses. This is particularly useful when you want to train a model on a specific task or domain with limited data.

`renderFewShotPrompt` is a function that takes a few-shot prompt template and a set of examples to fill in the placeholders. It returns the final few-shot prompt with the examples replaced by the provided values. This allows you to create few-shot prompts that can be easily customized for different tasks or domains.