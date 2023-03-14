defmodule WyzTest do
  use ExUnit.Case
  doctest Wyz

  test "greets the world" do
    assert Wyz.hello() == :world
  end
end
