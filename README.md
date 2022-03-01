# SMap

![Build](https://github.com/dadhi/SMap/actions/workflows/scala.yml/badge.svg)

Fast persistent immutable Map data-structure for Scala with minimal memory footprint.  

The SMap is based on the ImMap from my .NET C# library https://github.com/dadhi/ImTools  

Btw, **S** in Map is for **Speedy**.

### Goals

- [X] Port ImHashMap to SMap with complete addition and lookup ops and minimal tests for sanity
- [X] Benchmark with *ScalaMeter* -> so far inconclusive results
- [X] Benchmark with *JMH* -> initial results are [here](https://jmh.morethan.io/?source=https://raw.githubusercontent.com/dadhi/SMap/main/benchmarks/jmh-results.json)
- [X] Optimize the entry footprint for the Int keys
- [ ] Port the rest of the methods including foreach
- [ ] Port *CsCheck* test to *ScalaCheck*
- [ ] Conform to `immutable.Map` trait as much as possible
- [ ] Publish the package with initial version
- [ ] Optimize entry to create a Set
