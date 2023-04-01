defmodule Wyz.Maybe.FailureException do
  @moduledoc """
  Raised when calling `Maybe.unwrap!` on `nil` or an `{:error, err}` result.
  """
  defexception [:message, :error]

  @impl true
  def exception(args) do
    {message, args} = Keyword.pop(args, :message, "Monadic failure")
    {error, _} = Keyword.pop(args, :error)

    %__MODULE__{
      message: message,
      error: error
    }
  end

  @impl true
  def message(%__MODULE__{message: msg, error: err}) do
    case err do
      nil -> msg
      err when is_binary(err) -> "#{msg}: #{err}"
      err -> "#{msg}: #{inspect(err)}"
    end
  end
end

defmodule Wyz.Maybe.SuccessException do
  @moduledoc """
  Raised when calling `Maybe.unwrap_err!` on a non-`nil` value or an
  `{:ok, val}` result.
  """
  defexception [:message, :value]

  @impl true
  def exception(args) do
    {message, args} = Keyword.pop(args, :message, "Monadic unexpected success")
    {value, _} = Keyword.pop(args, :value)

    %__MODULE__{
      message: message,
      value: value
    }
  end

  @impl true
  def message(%__MODULE__{message: msg, value: val}) do
    "#{msg}: #{inspect(val)}"
  end
end
