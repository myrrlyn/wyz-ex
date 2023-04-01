defmodule MaybeTest do
  use ExUnit.Case
  import Wyz.Maybe

  doctest Wyz.Maybe

  test "normalization" do
    assert normalize(nil) == nil
    assert normalize(5) == 5

    assert normalize(:ok) == {:ok, nil}
    assert normalize({:ok, 1, 2}) == {:ok, {1, 2}}
  end

  test "making attempts" do
    res =
      attempt do
        5
      end

    assert res == 5

    res =
      attempt(res) do
        throw(10)
      end

    assert res == {:error, 10}

    res =
      res
      |> attempt do
        fn
          {:error, err} -> raise "Found an error: #{inspect(err)}"
          {:ok, val} -> val * 2
        end
      end

    assert res == {:error, %RuntimeError{message: "Found an error: 10"}}
  end
end
