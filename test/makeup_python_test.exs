defmodule MakeupPythonTest do
  use ExUnit.Case
  doctest MakeupPython

  test "greets the world" do
    assert MakeupPython.hello() == :world
  end
end
