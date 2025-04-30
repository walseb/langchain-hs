---
sidebar_position: 11
---

# Tool 

`Tool` typeclass provides an interface to define a tool that can be used in a chain. runTool is a function that takes an input and produces an output. The `Tool` typeclass defines methods for executing the tool, as well as for managing the metadata associated with the tool, such as its name and description.

The `Tool` typeclass is designed to be flexible and extensible, allowing developers to implement their own tools with custom input and output types. The `Tool` typeclass is used in conjunction with the `Chain` typeclass to provide a complete solution for building and executing chains of tools. By implementing the `Tool` typeclass, developers can create custom tools that meet their specific needs and requirements.

## Supported Integrations

At this moment, following integrations available,

- WikipediaTool       
