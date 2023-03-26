defmodule Wyz.Markdown.Heading do
  @moduledoc """
  Manages `<hN>` elements.

  In HTML, heading tags do not actually contain subheadings inside themselves.
  This module helps the Markdown processor convert a stream of heading elements
  into a structured tree, organized by rank.
  """

  @typedoc """
  A rank, a display text, and an HTML identifier.
  """
  @type t() :: %__MODULE__{
          rank: 1..6,
          text: String.t(),
          id: String.t() | nil
        }
  @typedoc """
  A collection of headings which have folded their subheadings into a tree.

  Each heading has relocated its following subheadings from being siblings in
  the list to being its direct children.
  """
  @type tree() :: [item()]
  @typedoc """
  A tuple of a heading, and a list of all its subheadings.
  """
  @type item() :: {t(), tree()}

  defstruct [:rank, :text, :id]

  @doc """
  Tests if an HTML tag is one of the six headings.
  """
  defguard is_heading(tag)
           when is_binary(tag) and
                  (tag == "h1" or tag == "h2" or tag == "h3" or tag == "h4" or tag == "h5" or
                     tag == "h6")

  @spec new(1..6, String.t(), String.t() | nil) :: t()
  def new(rank, text, id) do
    %__MODULE__{rank: rank, text: text, id: id}
  end

  @doc """
  Converts an Earmark AST node into a Heading.

  This converts the HTML tag name into an integer, preserves the ID attribute,
  and sets the display text to be the `Wyz.Markdown.plaintext()` representation
  of the entire node. All other metadata is discarded.
  """
  @spec from_node(Earmark.ast_tuple()) :: t()
  def from_node({tag, attrs, inner, _}) when is_heading(tag) do
    rank =
      case tag do
        "h1" -> 1
        "h2" -> 2
        "h3" -> 3
        "h4" -> 4
        "h5" -> 5
        "h6" -> 6
      end

    text = Wyz.Markdown.plaintext(inner)

    id =
      case List.keytake(attrs, "id", 0) do
        {{"id", id}, _} -> id
        nil -> Wyz.Markdown.to_ident(text)
      end

    %__MODULE__{rank: rank, text: text, id: id}
  end

  @doc """
  Compares two headings by rank only. A lesser rank is higher in the TOC tree.
  """
  @spec compare(t(), t()) :: :lt | :eq | :gt
  def compare(%__MODULE__{} = this, %__MODULE__{} = that) do
    cond do
      this.rank < that.rank -> :lt
      this.rank == that.rank -> :eq
      this.rank > that.rank -> :gt
    end
  end
end
