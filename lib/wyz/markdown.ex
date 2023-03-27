defmodule Wyz.Markdown do
  @moduledoc """
  Extends the Earmark processor with some custom manipulations.
  """

  alias __MODULE__.Heading
  import __MODULE__.Heading, only: [is_heading: 1]
  use OK.Pipe
  require Logger

  @opts %Earmark.Options{
    breaks: false,
    code_class_prefix: "lang- language- codeblock-",
    footnotes: true,
    gfm: true,
    gfm_tables: true,
    sub_sup: true
  }

  @doc """
  Renders some Markdown, producing both HTML and a structured table of contents.
  """
  @spec render(String.t(), Range.t(1, 6)) ::
          Wyz.result(
            {String.t(), Heading.tree(), [{atom(), number(), String.t()}]},
            {any(), [{atom(), number(), String.t()}]}
          )
  def render(source, levels \\ 1..6) do
    opts =
      case Application.fetch_env(:wyz, :markdown) do
        {:ok, opts} -> opts |> Keyword.delete(:unescaped_tags) |> Map.new()
        :error -> @opts |> Map.from_struct()
      end

    OK.for do
      opts <- Map.merge(@opts, opts) |> Earmark.Options.make_options()
      # Parse the source text first. If it fails, nothing else matters
      {ast, messages} <-
        source |> parse_markdown(opts)
    after
      {ast, _} = Earmark.Transform.map_ast_with(ast, %{}, &walker/2)
      toc_tree = Task.async(fn -> build_toc(ast, levels) end)
      html = Task.async(fn -> Earmark.Transform.transform(ast, opts) |> restore_tags() end)

      {Task.await(html, :infinity), Task.await(toc_tree, :infinity), messages}
    end
  end

  @doc """
  Translates Earmark's `{status, output, messages}` into
  `{status, {output, messages}}`.
  """
  @spec parse_markdown(String.t(), Earmark.Options.t()) ::
          Wyz.result({Earmark.ast(), [String.t()]}, any())
  def parse_markdown(source, opts \\ @opts) do
    case EarmarkParser.as_ast(source, opts) do
      {status, ast, messages} -> {status, {ast, messages}}
    end
  end

  @doc """
  Transforms the provided AST node when invoked by the Earmark descender.
  """
  @spec walker(Earmark.ast_node(), %{String.t() => pos_integer()}) ::
          {Earmark.ast_node(), %{String.t() => pos_integer()}}
  def walker(ast, ids) do
    ast
    |> retag()
    |> mark_codeblock()
    |> dedup_classes()
    |> process_heading(ids)
  end

  @doc """
  Replaces the HTML tag of an AST node, if it specifies a `tag="new_tag"`
  attribute.

  If the node is text, or does not have a `tag` attribute, it is unchanged.

  ## Examples

  ```elixir
  iex> retag("hello")
  "hello"
  iex> retag({"div", [], ["hello"], []})
  {"div", [], ["hello"], []}
  iex> retag({"div", [{"tag", "aside"}], ["hello"], []})
  {"aside", [], ["hello"], []}
  ```
  """
  @spec retag(Earmark.ast_node()) :: Earmark.ast_node()
  def retag(node)

  def retag({_, attrs, inner, meta} = node) do
    case List.keytake(attrs, "tag", 0) do
      nil -> node
      {{"tag", new_tag}, rest} -> {new_tag, rest, inner, meta}
    end
  end

  def retag(node), do: node

  @doc """
  Detects `<pre><code>` sections and attaches `.codeblock` to the `<pre>`.
  """
  @spec mark_codeblock(Earmark.ast_node()) :: Earmark.ast_node()
  def mark_codeblock(node)

  def mark_codeblock({"pre", pre_attrs, [{"code", code_attrs, _, _} | _] = inner, meta}) do
    {classes, rest} =
      case List.keytake(pre_attrs, "class", 0) do
        nil -> {[], pre_attrs}
        {{"class", classes}, rest} -> {String.split(classes), rest}
      end

    extra_classes =
      case List.keytake(code_attrs, "class", 0) do
        nil ->
          ["codeblock"]

        {{"class", classes}, _} ->
          lang =
            classes
            |> String.split()
            |> Enum.find("language-plaintext", &String.starts_with?(&1, "language-"))
            |> String.trim_leading("language-")

          ["codeblock", "codeblock-#{lang}"]
      end

    classes = (extra_classes ++ classes) |> Enum.uniq() |> Enum.join(" ")
    {"pre", [{"class", classes} | rest], inner, meta}
  end

  def mark_codeblock(node), do: node

  @doc """
  Attempts to produce an `id` attribute for a heading. If the heading element
  comes with an `id` attribute already, it is used as provided, otherwise, one
  is generated from the flattened text contents of the heading.

  The provided (or generated) ID text is fed into the ID collector, which
  produces a string that is guaranteed to be unique within this parse.
  """
  @spec process_heading(Earmark.ast_node(), %{String.t() => pos_integer()}) ::
          {Earmark.ast_node(), %{String.t() => pos_integer()}}
  def process_heading(node, ids)

  def process_heading({tag, attrs, inner, meta} = node, ids) when is_heading(tag) do
    {id, attrs} =
      case List.keytake(attrs, "id", 0) do
        {{"id", id}, rest} ->
          {id, rest}

        nil ->
          anchor = inner |> plaintext() |> to_ident
          {anchor, attrs}
      end

    {id, ids} =
      Map.get_and_update(ids, id, fn
        nil -> {id, 1}
        num -> {"#{id}-#{num}", num + 1}
      end)

    out =
      case id do
        "" -> node
        id -> {tag, [{"id", id} | attrs], inner, meta}
      end

    {out, ids}
  end

  def process_heading(node, state), do: {node, state}

  @doc """
  Strip duplicate classes. HTML parsers don't actually care about this but it's
  still nice.
  """
  @spec dedup_classes(Earmark.ast_node()) :: Earmark.ast_node()
  def dedup_classes(node)

  def dedup_classes({tag, attrs, inner, meta}) do
    {classes, rest} =
      case List.keytake(attrs, "class", 0) do
        nil -> {[], attrs}
        {{"class", classes}, rest} -> {classes |> String.split(), rest}
      end

    case classes |> Enum.uniq() |> Enum.join(" ") do
      "" -> {tag, rest, inner, meta}
      classes -> {tag, [{"class", classes} | rest], inner, meta}
    end
  end

  def dedup_classes(node), do: node

  @doc """
  Flattens the entire text contents of an AST subtree.

  ```elixir
  iex> "> Hello, *world*!" |> Earmark.as_ast!() |> plaintext()
  "Hello, world!"
  ```
  """
  @spec plaintext(Earmark.ast_node() | [Earmark.ast_node()]) :: String.t()
  def plaintext(ast)
  def plaintext(text) when is_binary(text), do: String.replace(text, ~r/\s+/, " ")

  def plaintext({_, _, children, _}) when is_list(children), do: plaintext(children)
  def plaintext(nodes) when is_list(nodes), do: nodes |> Enum.map(&plaintext/1) |> Enum.join("")

  @doc """
  Degrades a text span into an HTML anchor name.

  Can be used to give a heading the `id` attribute of its text.

  ```elixir
  iex> "# Hello, *world*!"
  ...> |> Earmark.as_ast!()
  ...> |> plaintext()
  ...> |> to_ident()
  "hello-world"
  ```
  """
  @spec to_ident(String.t()) :: String.t()
  def to_ident(text) do
    text
    |> String.trim()
    |> String.downcase()
    |> String.replace(~r/\s+/u, "-")
    |> String.replace(~r/[^\w-]/u, "")
    |> String.trim("-")
  end

  @doc """
  Builds a table of contents out of a parsed AST.

  This function:

  - flattens the tree into a list of `<hN>` elements
  - converts them into `Heading` structures
  - arranges the list of structures into a ranked tree of `Headings`
  """
  @spec build_toc(Earmark.ast(), Range.t(1, 6) | nil) :: Heading.tree()
  def build_toc(ast, levels \\ 1..6) do
    ast
    |> ast_to_headings()
    |> Stream.filter(fn {_, attrs, _, _} ->
      !(attrs
        |> List.keyfind("class", 0, {"class", ""})
        |> elem(1)
        |> String.split()
        |> Enum.any?(&(&1 == "no-toc")))
    end)
    |> Stream.map(&Heading.from_node/1)
    |> Stream.filter(&(&1.rank in levels))
    |> Enum.to_list()
    |> toc_tree()
  end

  @doc """
  Converts an AST into a flat list of `<hN>` elements.

  When a heading node is discovered, its children are flattened into a plaintext
  string, and descent stops. Otherwise, this descends into each child node, but
  does not add the current node to the output.

  This structure allows headings to be discovered even when they are not
  top-level elements in the fragment (such as the `> #` construct).
  """
  @spec ast_to_headings(Earmark.ast() | Earmark.ast_node()) :: [Earmark.ast_tuple()]
  def ast_to_headings(node)

  def ast_to_headings({tag, attrs, inner, meta}) when is_heading(tag) do
    [{tag, attrs, [plaintext(inner)], meta}]
  end

  def ast_to_headings({tag, _, inner, _}) when not is_heading(tag) do
    ast_to_headings(inner)
  end

  def ast_to_headings(text) when is_binary(text), do: []

  def ast_to_headings(nodes) when is_list(nodes) do
    nodes |> Enum.map(&ast_to_headings/1) |> List.flatten()
  end

  @doc """
  Converts a flat list of `Heading` structures into a tree.

  The tree is structured by rank: A given heading takes as children all
  immediately subsequent headings with rank greater than its own. The capture
  ends when it discovers a peer or superior heading.
  """
  @spec toc_tree([Heading.t()]) :: Heading.tree()
  def toc_tree(headings)

  def toc_tree([]), do: []

  def toc_tree([heading]), do: [{heading, []}]

  def toc_tree([head | rest]) do
    pivot = Enum.find_index(rest, fn h -> Heading.compare(h, head) != :gt end)

    if pivot do
      {children, rest} = Enum.split(rest, pivot)
      [{head, toc_tree(children)} | toc_tree(rest)]
    else
      [{head, toc_tree(rest)}]
    end
  end

  @doc """
  Un-escapes certain tags. Does not support attributes in those tags.

  This is not aware of context, and replaces *all* escaped tag instances.
  """
  @spec restore_tags(String.t()) :: String.t()
  def restore_tags(html) do
    tags =
      case Application.fetch_env(:wyz, :markdown) do
        {:ok, cfg} -> cfg
        :error -> Keyword.new()
      end
      |> Keyword.get(:unescaped_tags, [])
      |> Enum.join("|")

    re = ~r/&lt;(?<c>\/)?(?<t>#{tags})&gt;/

    Regex.replace(
      re,
      html,
      "<\\1\\2>"
    )
  end
end
