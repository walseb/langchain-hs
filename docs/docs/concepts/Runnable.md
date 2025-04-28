---
sidebar_position: 13
---

# Runnable

`Runnable` typeclass provides an interface to define a runnable that can be used in a chain. A runnable is a function that takes an input and produces an output. The `Runnable` typeclass defines methods for executing the runnable, as well as for managing the metadata associated with the runnable, such as its name and description.
The `Runnable` typeclass is designed to be flexible and extensible, allowing developers to implement their own runnables with custom input and output types. The `Runnable` typeclass is used in conjunction with the `Chain` typeclass to provide a complete solution for building and executing chains of runnables. By implementing the `Runnable` typeclass, developers can create custom runnables that meet their specific needs and requirements.

* 'RunnableSequence' - Chain multiple runnables sequentially
* 'RunnableBranch' - Select different processing branches based on input conditions
* 'RunnableMap' - Transform inputs or outputs when composing runnables

Above runnables are used to create complex workflows that require multiple steps and decision-making. Runnables are useful for building chatbots, recommendation systems, and other applications that require a sequence of actions to be performed based on user input or other data.
