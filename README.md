# Wyz

This is a collection of modules that I find useful and have probably torn out of
other projects when I discovered I needed to re-use them.

## Contents

- `Markdown`: specialized Markdown renderer. This wraps `Earmark` and also
  produces a structured table of contents. It will gain other functionality
  (such as YAML frontmatter) as I extract it from my websites.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `wyz` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:wyz, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/wyz>.
