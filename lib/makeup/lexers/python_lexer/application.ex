defmodule Makeup.Lexers.PythonLexer.Application do
  @moduledoc false
  use Application

  alias Makeup.Registry
  alias Makeup.Lexers.PythonLexer

  def start(_type, _args) do
    Registry.register_lexer(PythonLexer,
      options: [],
      names: ["python"],
      extensions: ["py"]
    )

    Supervisor.start_link([], strategy: :one_for_one)
  end
end
