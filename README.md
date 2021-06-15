# DynamicPipeline

![badge](https://github.com/jproyo/dynamic-pipeline/actions/workflows/dynamic-pipeline.yml/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/dynamic-pipeline.svg?style=flat)](https://hackage.haskell.org/package/dynamic-pipeline)


**DynamicPipeline** is a *Type Safe* Dynamic and Parallel Streaming Library, which is an implementation of **Dynamic Pipeline Paradigm (DPP)**
proposed in this paper [DPP](https://biblioteca.sistedes.es/articulo/the-dynamic-pipeline-paradigm/).

The aim of this Library is to provide all the **Type level** constructs to guide the user in building a *DPP* flow to solve any algorithm that fits on this computational model.  
 
This implementation has been developed using *Type Level Programming* techniques like `Type families`, `Defunctionalization`, `Existential Types` and `Dynamic Record Tagged Types` among others.

Using all this techniques, we provide a *High Level and Type Safe* **DPP** Library to build a Data Flow Algorithm avoiding as much as possible boilerplate code, but maintaining safety and robustness.

## Prerequisites

This project has been built and developed with the following versions:

- [Stack](https://docs.haskellstack.org/en/stable/README/) `2.1.3` or higher
- Stack LTS `17.7`
- [GHC](https://www.haskell.org/ghc/) `8.10.3` 

## Cloning the project 

```bash
git clone https://github.com/jproyo/dynamic-pipeline
```

## Issues 

https://github.com/jproyo/dynamic-pipeline/issues

## Pull Request

https://github.com/jproyo/dynamic-pipeline/pulls

## Licensing

`dynamic-pipeline` is an open source project available under a liberal [BSD-3-Clause license](./LICENSE)





