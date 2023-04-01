defmodule Wyz.Maybe do
  @moduledoc """
  Utilities for working with both optionally-present values (`nil` vs anything)
  and Elixir's success/failure idioms (`{:ok, value}` and
  `{:error, description}`).

  The functions in this module operate on both optionals and fallibles. The
  entry and exit functions will distinguish between bare objects/`nil` and the
  idiomatic result tuples, but the intermediate transform functions will work on
  both forms.

  To begin using this module, you should invoke `normalize/1`. Because Elixir is
  dynamically typed, idiomatic usage allows a broad variety of shapes of
  fallible symbols: bare `:ok` or `:error` atoms, `{status, object}` pairs, or
  `{status, object, object…}` large tuples. The normalizer converts all of these
  into ordinary pairs: bare atoms gain a `nil` payload, and large tuples are
  split into `{status, {rest…}}`.

  There are four general categories of function in this module:

  - ingress functions receive values from the surrounding Elixir program and
    usher them into a monadic pipeline
  - transform functions perform operations on monad instances, allowing the
    carried values to be used and manipulated without having to constantly check
    what the carrier state is
  - branch functions allow carried values to be discarded and replaced with
    entirely new values when appropriate
  - egress functions remove the monadic wrapper and restore the carried inner
    value so that the surrounding program can work with it directly.
  """
  alias Wyz.Maybe.{FailureException, SuccessException}

  @typedoc """
  A value which may or may not exist. Non-existent values are represented by
  `nil`.
  """
  @type option(t) :: nil | t

  @typedoc """
  A computation which may or may not have succeeded.

  Either `{:ok, success}` or `{:error, error}`.
  """
  @type result(t, e) :: {:ok, t} | {:error, e}

  @typedoc """
  Either an `option` or a `result`.
  """
  @type maybe(t, e) :: option(t) | result(t, e)

  @typedoc """
  An object, or a zero-argument function which when called produces an object.
  """
  @type lazy(t) :: t | (() -> t)

  @typedoc """
  An object, or a zero-or-one-argument function which when called produces an
  object.
  """
  @type lazy(i, t) :: lazy(t) | (i -> t)

  @typep t :: term()
  @typep u :: term()
  @typep e :: term()
  @typep f :: term()

  @doc """
  Tests if an object exists. Equivalent to `not is_nil`.
  """
  defguard is_some(val) when not is_nil(val)

  @doc """
  Tests if an object does not exist. Equivalent to `is_nil`.
  """
  defguard is_none(val) when is_nil(val)

  @doc """
  Tests if an object is `{:ok, _}` or `:ok`.
  """
  defguard is_ok(val) when (tuple_size(val) == 2 and elem(val, 0) == :ok) or val == :ok

  @doc """
  Tests if an object is `{:error, _}`.
  """
  defguard is_err(val) when tuple_size(val) == 2 and elem(val, 0) == :error

  @doc """
  Tests if an object is a result of either variant.
  """
  defguard is_result(val)
           when is_ok(val) or is_err(val)

  ############################## Ingress ##############################

  @doc """
  Normalizes incoming values to shapes that this module expects.

  Bare `:ok` and `:error` become `{status, nil}` pairs; any tuple of the form
  `{:ok | :error, fields…}` become `{status, {fields…}}`, and all other terms
  are passed through unmodified.
  """
  @spec normalize(nil) :: nil
  @spec normalize(:ok | :error) :: {:ok | :error, nil}
  @spec normalize(tuple()) :: result(tuple(), tuple()) | tuple()
  def normalize(value)

  # nil doesn't change
  def normalize(nil), do: nil

  # Bare atoms (including in 1-tuples) become 2-tuples with a nil payload
  def normalize(:ok), do: {:ok, nil}
  def normalize(:error), do: {:error, nil}
  def normalize({:ok}), do: normalize(:ok)
  def normalize({:error}), do: normalize(:error)

  # Results don't need to change
  def normalize(result) when is_result(result), do: result

  # Large tuples with :ok or :error become 2-tuples. All other tuples are just
  # regular values
  def normalize(tuple) when is_tuple(tuple) and tuple_size(tuple) > 2 do
    case elem(tuple, 0) do
      :ok -> {:ok, Tuple.delete_at(tuple, 0)}
      :error -> {:error, Tuple.delete_at(tuple, 0)}
      other -> other
    end
  end

  # Regular values don't change
  def normalize(value), do: value

  @doc """
  Transforms a value into a successful Maybe monad.

  `nil` and `{:error, …}` values are unchanged; all other values become
  `{:ok, input}`.
  """
  @spec ok(maybe(t, e)) :: nil | result(t, e)
  def ok(value) do
    case normalize(value) do
      nil -> nil
      result when is_result(result) -> result
      other -> {:ok, other}
    end
  end

  @doc """
  Transforms a value into a failed Maybe monad.

  `nil` and `{:ok, …}` values are unchanged; all other values become
  `{:error, input}`.
  """
  @spec err(maybe(t, e)) :: nil | result(t, e)
  def err(value) do
    case normalize(value) do
      nil -> nil
      result when is_result(result) -> result
      other -> {:error, other}
    end
  end

  ############################# Transform #############################

  @doc """
  Transforms the success value of a fallible.

  `nil` and `{:error, err}` are passed through untouched. `{:ok, val}` and `val`
  are given to the `mapper`, which can either be a new value replacing the old
  value, or a function that *produces* the new value when invoked.

  When the fallible is `{:ok, val}`, the return value of `mapper.(val)` is then
  given to `ok/1`. When it is a bare value, no such transformation occurs.
  """
  @spec map(maybe(t, e), lazy(t, u)) :: maybe(u, e)
  def map(maybe, mapper) do
    case normalize(maybe) do
      nil -> nil
      {:error, err} -> {:error, err}
      {:ok, val} -> mapper |> lazy(val) |> ok()
      val -> lazy(mapper, val)
    end
  end

  @doc """
  Transforms the failure value of a fallible.

  `nil` and `{:error, err}` values are
  """
  @spec map_err(maybe(t, e), lazy(e, f)) :: maybe(t, f)
  def map_err(maybe, mapper) do
    case normalize(maybe) do
      nil -> mapper |> lazy(nil)
      {:error, err} -> mapper |> lazy(err) |> err()
      other -> other
    end
  end

  @doc """
  Runs a side-effect when a Result is the success variant.

  The side-effect receives the carried value as its argument. The Result is
  returned unmodified.
  """
  @spec tap_ok(maybe(t, e), (t -> any)) :: maybe(t, e)
  def tap_ok(maybe, val_tapper)
  def tap_ok(nil, _), do: nil
  def tap_ok({:error, err}, _), do: err(err)

  def tap_ok({:ok, val}, val_tapper) do
    lazy(val_tapper, val)
    ok(val)
  end

  def tap_ok(val, val_tapper) do
    lazy(val_tapper, val)
    ok(val)
  end

  @doc """
  Runs a side-effect when a Result is the error variant.

  The side-effect receives the carried error (if any) as its argument. The
  Result is returned unmodified.
  """
  @spec tap_err(maybe(t, e), (e -> any) | (() -> any)) :: maybe(t, e)
  def tap_err(maybe, err_tapper)

  def tap_err(nil, err_tapper) do
    lazy(err_tapper)
    nil
  end

  def tap_err({:error, err}, err_tapper) do
    lazy(err_tapper, err)
    err(err)
  end

  def tap_err({:ok, val}, _), do: ok(val)
  def tap_err(val, _), do: ok(val)

  ############################### Branch ###############################

  @doc """
  Gets and transforms the carried value of a fallible, or the provided fallback.

  The mapper can either be a 1-arity function, in which case it is invoked with
  the carried value when it exists, or an ordinary value.

  The default can be:

  - a 1-arity function, invoked with the error when one is present
  - a 0-arity function, invoked when no value exists
  - a regular value
  """
  @spec map_or(maybe(t, e), lazy(t, u), lazy(e, u)) :: u
  def map_or(maybe, mapper, default)
  def map_or(nil, _, default), do: lazy(default)
  def map_or(:ok, _, default), do: lazy(default)
  def map_or({:error, err}, _, default), do: lazy(default, err)

  def map_or({:ok, val}, mapper, default) do
    case lazy(mapper, val) do
      {:error, err} -> lazy(default, err) |> err()
      {:ok, val} -> ok(val)
      nil -> lazy(default)
      out -> {:ok, out}
    end
  end

  @doc """
  Produces a closure which expects to receive one input argument, and wraps the
  provided expression in a `try`/`rescue` block. If the expression does not
  throw when invoked with an argument, then the output is returned as a Maybe;
  if the expression does throw when invoked, then the exception is caught and
  returned as the payload of an `{:error, exn}` tuple.
  """
  defmacro attempt(input, do: expr) do
    quote do
      (fn arg ->
         try do
           Wyz.Maybe.lazy(unquote(expr), arg)
         rescue
           exn -> err(exn)
         catch
           thrown -> err(thrown)
         end
       end).(unquote(input))
    end
  end

  defmacro attempt(do: expr) do
    quote do
      (fn ->
         try do
           Wyz.Maybe.lazy(unquote(expr))
         rescue
           exn -> err(exn)
         catch
           thrown -> err(thrown)
         end
       end).()
    end
  end

  @doc """
  Produces the right value if the left is not nil or an error; otherwise,
  produces the left value.

  ## Examples

  ```elixir
  iex> nil &&& 5
  nil
  iex> ok(5) &&& nil
  nil
  iex> ok(5) &&& ok(10)
  {:ok, 10}
  iex> ok(5) &&& 10
  {:ok, 10}
  ```
  """
  @spec maybe(t, e) &&& maybe(t, e) :: maybe(t, e)
  def left &&& right
  def left &&& _ when is_nil(left) or is_err(left), do: err(left)
  def left &&& right when is_ok(left) or is_some(left), do: right |> lazy() |> ok()

  @doc """
  Produces the right value when the left is an error or nil, and the left value
  when it is success or non-nil.

  ## Examples

  ```elixir
  iex> nil ||| 5
  {:ok, 5}
  iex> ok(5) ||| nil
  {:ok, 5}
  iex> err(:a) ||| 10
  {:ok, 10}
  iex> err(:a) ||| err(:b)
  {:error, :b}
  ```
  """
  @spec maybe(t, e) ||| maybe(t, e) :: maybe(t, e)
  def left ||| right
  def left ||| right when is_nil(left) or is_err(left), do: right |> lazy() |> ok()
  def left ||| _ when is_ok(left) or is_some(left), do: ok(left)

  ############################### Egress ###############################

  @doc """
  Tests if *any* value is non-nil. `false` is not `nil`, and `{:error, err}`
  tuples are not `nil`. They are `some`.

  ## Examples

  ```elixir
  iex> false |> some?()
  true
  iex> nil |> some?()
  false
  ```
  """
  @spec some?(option(any)) :: boolean
  def some?(value)
  def some?(nil), do: false
  def some?(_), do: true

  @doc """
  Tests if a value is `nil`, and *only* `nil`. Equivalent to `Kernel.is_nil/1`.

  ## Examples

  ```elixir
  iex> false |> none?()
  false
  iex> nil |> none?()
  true
  ```
  """
  @spec none?(option(any)) :: boolean
  def none?(value)
  def none?(nil), do: true
  def none?(_), do: false

  @doc """
  Tests if a result is the `:ok` variant.
  """
  @spec ok?(result(t, e)) :: boolean
  def ok?(result)
  def ok?(:ok), do: true
  def ok?({:ok, _}), do: true
  def ok?({:error, _}), do: false

  @doc """
  Tests if a result is the `:error` variant.
  """
  @spec err?(result(t, e)) :: boolean
  def err?(result)
  def err?(nil), do: true
  def err?({:error, _}), do: true
  def err?({:ok, _}), do: false

  @doc """
  Forcibly accesses the carried value of a fallible.

  Raises if the fallible is `nil` or an `:error`.

  ```elixir
  iex> ok(5) |> unwrap!()
  5
  iex> ok({:error, 5}) |> unwrap!()
  ** (Wyz.Maybe.FailureException) unwrapped an error: 5
  ```
  """
  @spec unwrap!(maybe(t, e)) :: t
  def unwrap!(maybe)

  def unwrap!({:error, err}),
    do: raise(FailureException, message: "unwrapped an error", error: err)

  def unwrap!({:ok, val}), do: val
  def unwrap!(nil), do: raise(FailureException, message: "cannot unwrap `nil`", error: nil)

  # Attempt to normalize other values. If they become a result, unwrap the
  # result; otherwise, return the value directly. This handles large tuples.
  def unwrap!(val) do
    case normalize(val) do
      res when is_result(res) -> unwrap!(res)
      val -> val
    end
  end

  @doc """
  Forcibly accesses the carried error (including `nil`) of a fallible.

  Raises if the fallible is carrying a success value.
  """
  @spec unwrap_err!(maybe(t, e)) :: e | nil
  def unwrap_err!(maybe)
  def unwrap_err!(nil), do: nil
  def unwrap_err!({:error, err}), do: err

  def unwrap_err!({:ok, val}),
    do: raise(SuccessException, message: "unexpectedly succeeded", value: val)

  def unwrap_err!(val) do
    case normalize(val) do
      res when is_result(res) -> unwrap_err!(res)
      val -> raise SuccessException, message: "expected `nil`, got", value: val
    end
  end

  @doc """
  Gets the carried value of a fallible, or the provided fallback.
  """
  @spec unwrap_or(maybe(t, e), lazy(t)) :: t
  def unwrap_or(maybe, default) do
    unwrap!(maybe)
  catch
    %FailureException{} -> lazy(default)
  end

  ######################################################################

  # Evaluate a term -- if it is a function, call it; if not, return it as-is.

  @doc false
  def lazy(obj_or_func) do
    case obj_or_func do
      func when is_function(func, 0) -> func.()
      obj -> obj
    end
  end

  @doc false
  def lazy(obj_or_func, arg) do
    case obj_or_func do
      func when is_function(func, 1) -> func.(arg)
      other -> lazy(other)
    end
  end
end
