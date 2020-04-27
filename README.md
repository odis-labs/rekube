# Rekube

Rekube is a ReasonML toolkit for Kubernetes configuration.

![rekube](https://odis-labs.github.io/rekube/example.gif)

The main components of the project are:

- [Kubernetes API](https://odis-labs.github.io/rekube/rekube/index.html#api): The entire Kubernetes API is exposed as a ReasonML library.
- [Configuration DSL](https://odis-labs.github.io/rekube/rekube/index.html#dsl): A custom PPX (preprocessor extension) can be used to write Kubernetes declarations using a JSON-like syntax with type annotations.

Note: This project is currently unstable and unreleased.

## Quickstart

To install rekube in an esy project, add the following dependency to your package.json file:

```json
"dependencies": {
  "rekube": "github:odis-labs/rekube#f9d8e20"
}
```

In your dune project add the following dependencies to your dune file:


```lisp
(executable
  (name Infra)
  (public_name gen-infra)
  (libraries rekube console.lib)
  (preprocess (pps rekube.ppx)))
```

Finally, describe your configuration in a file called Infra.re and execute it to generate the raw JSON files.

```
$ esy gen-infra
```
