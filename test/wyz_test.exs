defmodule WyzTest do
  use ExUnit.Case
  import Wyz.Markdown
  alias Wyz.Markdown.Heading
  use OK.Pipe

  doctest Wyz
  doctest Wyz.Markdown

  test "retains only headings" do
    # note that the heading discovery descends through arbitrary enclosures, so
    # even inside a blockquote it still gets discovered (and the blockquote is
    # ignored)
    text = """
    > # Title

    ## Chapter *One*

    some paragraph text

    > a blockquote

    ## Chapter *Two*

    some more paragraph text

    ```elixir
    even() |> a() |> code() |> block()
    ```
    """

    {:ok, headings} =
      text |> parse_markdown() ~> elem(0) ~> ast_to_headings() ~> Enum.map(&Heading.from_node/1)

    assert headings == [
             %Heading{rank: 1, text: "Title", id: "title"},
             %Heading{rank: 2, text: "Chapter One", id: "chapter-one"},
             %Heading{rank: 2, text: "Chapter Two", id: "chapter-two"}
           ]
  end

  test "builds toc tree" do
    text = """
    # Title

    ## Section 1

    ### Chapter 1

    ### Chapter 2

    ## Section 2

    ### Chapter 3

    ### Chapter 4
    """

    {:ok, tree} =
      text
      |> parse_markdown()
      ~> elem(0)
      ~> ast_to_headings()
      ~> Enum.map(&Heading.from_node/1)
      ~> toc_tree()

    assert tree == [
             {%Heading{rank: 1, text: "Title", id: "title"},
              [
                {%Heading{rank: 2, text: "Section 1", id: "section-1"},
                 [
                   {%Heading{rank: 3, text: "Chapter 1", id: "chapter-1"}, []},
                   {%Heading{rank: 3, text: "Chapter 2", id: "chapter-2"}, []}
                 ]},
                {%Heading{rank: 2, text: "Section 2", id: "section-2"},
                 [
                   {%Heading{rank: 3, text: "Chapter 3", id: "chapter-3"}, []},
                   {%Heading{rank: 3, text: "Chapter 4", id: "chapter-4"}, []}
                 ]}
              ]}
           ]
  end

  test "builds a filtered TOC tree" do
    text = """
    # Title

    ## Subtitle

    ## Filtered {:.no-toc}

    This heading is ignored even though it is within the filter because it has
    explicitly opted out. The filtering occurs before its children are detected,
    so the subsequent headings are re-parented to `## Subtitle`.

    ### Section

    #### Chapter

    This heading is ignored because it is not within the filter.
    """

    {:ok, toc} = text |> parse_markdown() ~> elem(0) ~> build_toc(2..3)

    assert toc == [
             {%Heading{rank: 2, text: "Subtitle", id: "subtitle"},
              [
                {%Heading{rank: 3, text: "Section", id: "section"}, []}
              ]}
           ]
  end
end
