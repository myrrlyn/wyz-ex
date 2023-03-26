defmodule Wyz do
  @moduledoc """
  A collection of utility modules I think is useful.
  """

  @typedoc """
  Syntax sugar for the `{:ok, val}` or `{:error, err}` customary tuples.
  """
  @type result(t, e) :: {:ok, t} | {:error, e}
end
