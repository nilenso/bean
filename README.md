# Bean

Spreadsheets are an outstanding tool for building small software. However, mainstream spreadsheet software doesn't have the primitives and tools to make "production-ready" spreadsheets – spreadsheets that are maintainable, error-free and easy to work with.

Bean is an exploration into new and overlooked ideas in the spreadsheet paradigm. It's intended to be a playground for trying out features (and _maybe_ eventually become a full-fledged spreadsheet software).

This involves looking at spreadsheets as a programming environment from the ground up. The full background is [here](put link to blog). [This talk](https://www.youtube.com/watch?v=0yKf8TrLUOw) is a great introduction to the problem.

## State of affairs

**Updated on:** 31-Oct-2023

Bean has these basics in place: a grid, a parser, an interpreter for a small formula language and reactive recalculation (with dynamic arrays). The formula language (Leaf) is documented [here](formula-language.md).

Please shoot us an email at moshimoshi @ nilenso dot com and we'd be happy to walk you through the project.

We are trying out the following features
- A "secondary space" for linear thinking
- Generating provenance for all calculations for auditability
- Flexible tables as a primary data structure

A repository with the accompanying research work will be put up soon.

## Setting up

Bean is written in [ClojureScript](https://clojurescript.org/). You'll need [npm and nodejs](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm) installed. Then run

```
npx install
shadow-cljs watch ui
```

You can access Bean running locally at http://localhost:8090.

## Authors
- [Prabhanshu Gupta](https://github.com/prabhanshuguptagit)
- [Ravi Chandra Padmala](https://github.com/neenaoffline)
