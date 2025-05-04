---
sidebar_position: 13
---

# Runnable

:::warning

As of 0.0.2.0 the `Runnable` typeclass is pretty unstable. You can use it but expect breaking changes in the future.

:::

All components from langchain-hs have Runnable Instances. This is a typeclass will help you *compose* multiple components together. 
The main idea is that you can use the `run` method to run a component and get the result **as long as the input is compatible with the output of the previous component**. 

## Chain functions

Components having runnable instances can utilize the following functions to compose them together:

  - runBranch
  - runMap
  - runSequence
  - chain
  - branch
  - buildSequence
  - appendSequence