# Bean

Spreadsheets are an outstanding tool for building small software. However, mainstream spreadsheet software lacks the tools to create spreadsheets that are maintainable, error-free and easy to work with – spreadsheets that are "production-ready".

Bean is an exploration into new/overlooked ideas in the spreadsheet paradigm to solve this problem. It's intended to be a playground for trying out features (and _maybe_ eventually become a full-fledged spreadsheet software).

This involves looking at spreadsheets as a programming environment from the ground up. Read [this blogpost](https://bean.nilenso.com/blog/posts/spreadsheets-and-small-software/) for the full background. [This talk](https://www.youtube.com/watch?v=0yKf8TrLUOw) is a great introduction to the problem.

[Play around with it](https://bean.nilenso.com) (it's super early).

## State of affairs

23-Jun-2025

This project is no longer being actively maintained.

31-Oct-2023

Bean has these basics in place: a grid, a parser, an interpreter for a small formula language and reactive recalculation (with dynamic arrays).

Please shoot us an email at bean @ nilenso dot com and we'd be happy to walk you through.

We are currently trying out the following features
- A "secondary space" for linear thinking
- Generating provenance for all calculations for auditability
- Flexible tables as a primary data structure

A repository with the accompanying research work will be put up soon.

## Setting up

Bean is written in [ClojureScript](https://clojurescript.org/). You'll need [java](https://www.java.com/en/download/), [npm and nodejs](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm) installed. Then run

```
npm install
npx shadow-cljs watch ui
```

You can access Bean running locally at http://localhost:8090.

Installation instructions for Java can be found [here](https://github.com/supertokens/supertokens-core/wiki/Installing-OpenJDK-for-Mac-and-Linux).

#### Tests
```
npx shadow-cljs compile test && node tests.js
```

## Authors
- [Prabhanshu Gupta](https://github.com/prabhanshuguptagit)
- [Ravi Chandra Padmala](https://github.com/neenaoffline)
