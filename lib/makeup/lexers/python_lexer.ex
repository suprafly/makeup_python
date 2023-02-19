defmodule Makeup.Lexers.PythonLexer do
  @moduledoc """
  A `Makeup` lexer for the Python language.
  """

  import NimbleParsec
  import Makeup.Lexer.Combinators
  import Makeup.Lexer.Groups
  # import Makeup.Lexers.PythonLexer.Helpers

  @behaviour Makeup.Lexer

  # ###################################################################
  # # Step #1: tokenize the input (into a list of tokens)
  # ###################################################################
  # # We will often compose combinators into larger combinators.
  # # Sometimes, the smaller combinator is usefull on its own as a token, and sometimes it isn't.
  # # We'll adopt the following "convention":
  # #
  # # 1. A combinator that ends with `_name` returns a string
  # # 2. Other combinators will *usually* return a token
  # #
  # # Why this convention? Tokens can't be composed further, while raw strings can.
  # # This way, we immediately know which of the combinators we can compose.


  whitespace = ascii_string([?\r, ?\s, ?\n, ?\f, ?\t], min: 1) |> token(:whitespace)

  newlines =
    ascii_string([?\s, ?\t, ?\r], min: 1)
    |> optional()
    |> choice([string("\r\n"), string("\n")])
    |> optional(ascii_string([?\s, ?\n, ?\f, ?\r], min: 1))
    |> token(:whitespace)

  any_char = utf8_char([]) |> token(:error)

  line =
    ascii_char([?\n])
    |> lookahead_not()
    |> utf8_string([], 1)
    |> repeat()

  inline_comment =
    string("#")
    |> concat(line)
    |> eos()
    |> token(:comment_single)

  unicode_char_in_string =
    string("\\u")
    |> ascii_string([?0..?9, ?a..?f, ?A..?F], 4)
    |> token(:string_escape)

  escaped_char =
    string("\\")
    |> utf8_string([], 1)
    |> token(:string_escape)

  variable =
    ascii_string([?a..?z, ?A..?Z, ?_,], 1)
    |> optional(ascii_string([?a..?z, ?_, ?0..?9, ?A..?Z], min: 1))
    |> lexeme()
    |> token(:name)

  decorator =
    string("@")
    |> ascii_string([?0..?9, ?a..?z, ?A..?Z], min: 1)
    |> token(:string_symbol)

  # %-formatting
  # "%s %s" %('Hello','World',)
  percent_string_interp = string_like("%(", ")", [variable], :string_interpol)

  combinators_inside_string = [
    percent_string_interp,
    unicode_char_in_string,
    escaped_char,
  ]

  single_quoted_heredocs = string_like("'''", "'''", combinators_inside_string, :comment_multiline)
  double_quoted_heredocs = string_like(~S["""], ~S["""], combinators_inside_string, :comment_multiline)

  # f-strings
  # f'Hello {name}! This is {program}'
  inner_f_string_interp = string_like("{", "}", [variable], :string_interpol)

  f_string_interp_choices = [
    inner_f_string_interp,
    unicode_char_in_string,
    escaped_char,
  ]

  single_quoted_f_string_interpolation = string_like("f'", "'", f_string_interp_choices, :string)
  double_quoted_f_string_interpolation = string_like(~S[f"], ~S["], f_string_interp_choices, :string)

  single_quoted_string = string_like("'", "'", combinators_inside_string, :string)
  double_quoted_string = string_like("\"", "\"", combinators_inside_string, :string)

  number_integer = integer(min: 1) |> token(:number_integer)

  float_scientific_notation =
    ascii_string([?e, ?E], 1)
    |> optional(choice([string("-"), string("+")]))
    |> concat(number_integer)

  number_float =
    number_integer
    |> string(".")
    |> concat(number_integer)
    |> optional(float_scientific_notation)
    |> token(:number_float)

  number_bin =
    string("0b")
    |> concat(ascii_string([?0..?1], min: 1))
    |> token(:number_bin)

  number_oct =
    string("0c")
    |> concat(ascii_string([?0..?7], min: 1))
    |> token(:number_oct)

  number_hex =
    string("0x")
    |> concat(ascii_string([?0..?9, ?a..?f, ?A..?F], min: 1))
    |> token(:number_hex)

  # Python3 has ellipsis: ...
  operator =
    ~W(
        + - * / % ** //
        == != < > <= >= <>
        = += -= *= /= %= **= //=
        & | ^ ~ << >> ...
      )
    |> word_from_list()
    |> token(:operator)

  punctuation =
    [
      ":", ";", ",", ".", "%",
      "'", "\"", "#", "\\", "@"
    ]
    |> word_from_list()
    |> token(:punctuation)

  delimiters_punctuation =
    ~W(( \) [ ] { } , : . ` ;)
    |> word_from_list()
    |> token(:punctuation)

  _special_symbols =
    ~W(' " # \ @)
    |> word_from_list()
    |> token(:punctuation)

  keyword =
    ~W(
        False await else import pass
        None break except in raise
        True class finally is return
        and continue for lambda try
        as def from nonlocal while
        assert del global not with
        async elif if or yield
      )
    |> word_from_list()
    |> ascii_string([?\s, ?\t], min: 1)
    |> token(:keyword)


  delimiter_pairs = [
    delimiters_punctuation,
  ]

  # This does the work of parsing
  root_element_combinator =
    choice(
      [
        newlines,
        whitespace,

        # Comments
        inline_comment,

        # Syntax sugar for keyword lists (must come before variables and strings)
        keyword,

        single_quoted_heredocs,
        double_quoted_heredocs,

        single_quoted_string,
        double_quoted_string,

        # String interpolation
        single_quoted_f_string_interpolation,
        double_quoted_f_string_interpolation,
      ] ++
        delimiter_pairs ++
        [
          # Operators
          operator,
          # Numbers
          number_bin,
          number_oct,
          number_hex,
          # Floats must come before integers
          number_float,
          number_integer,
          # Names
          variable,

          decorator,

          # from C:
          # define,

          # from elixir:
          # Module names
          # module,

          punctuation,
          # If we can't parse any of the above, we highlight the next character as an error
          # and proceed from there.
          # A lexer should always consume any string given as input.
          any_char
        ]
    )

  # By default, don't inline the lexers.
  # Inlining them increases performance by ~20%
  # at the cost of doubling the compilation times...
  @inline false

  @doc false
  def __as_python_language__({ttype, meta, value}) do
    {ttype, Map.put(meta, :language, :python), value}
  end

  # Semi-public API: these two functions can be used by someone who wants to
  # embed an Elixir lexer into another lexer, but other than that, they are not
  # meant to be used by end-users.

  @impl Makeup.Lexer
  defparsec(
    :root_element,
    root_element_combinator |> map({__MODULE__, :__as_python_language__, []}),
    inline: @inline
  )

  @impl Makeup.Lexer
  defparsec(
    :root,
    repeat(parsec(:root_element)),
    inline: @inline
  )

  # ###################################################################
  # # Step #2: postprocess the list of tokens
  # ###################################################################

  @keyword_declaration ~W[def class]
  @keyword ~W[
    await else pass
    break except in raise
    class finally is return
    and continue for lambda try
    as def from nonlocal while
    assert del global not with
    async elif if or yield
  ]
  @operator_word ["and", "or", "not", "in", "is"]
  @keyword_namespace ~W[import from]
  @name_constant ~W[True False None]

  defp postprocess_helper([]), do: []

  # # def add(x, y):
  # defp postprocess_helper([
  #        {:name, attrs1, text1},
  #        {:whitespace, _, _} = ws1,
  #        {:name, _, _text2} = param,
  #        {:whitespace, _, _} = ws2,
  #        {:punctuation, _, _} = p
  #        | tokens
  #      ])
  #      when text1 == "def" do
  #   [{:keyword_declaration, attrs1, text1}, ws1, param, ws2, p | postprocess_helper(tokens)]
  # end

  # # class ClassName:
  # defp postprocess_helper([
  #        {:name, attrs1, text1},
  #        {:whitespace, _, _} = ws1,
  #        {:name, _, _text2} = param,
  #        {:punctuation, _, _} = p
  #        | tokens
  #      ])
  #      when text1 == "class" do
  #   [{:keyword_declaration, attrs1, text1}, ws1, param, p | postprocess_helper(tokens)]
  # end

  # @staticmethod
  defp postprocess_helper([{:string_symbol, attrs, [text1, _text2] = decorator} | tokens]) when text1 == "@" do
    # todo - not clear what token we should use, so I am guessing...
    [{:name_tag, attrs, decorator} | postprocess_helper(tokens)]
  end

  defp postprocess_helper([{:name, attrs, text} | tokens]) when text in @keyword do
    [{:keyword, attrs, text} | postprocess_helper(tokens)]
  end

  defp postprocess_helper([{:name, attrs, text} | tokens]) when text in @keyword_declaration do
    [{:keyword_declaration, attrs, text} | postprocess_helper(tokens)]
  end

  defp postprocess_helper([{:name, attrs, text} | tokens]) when text in @operator_word do
    [{:operator_word, attrs, text} | postprocess_helper(tokens)]
  end

  defp postprocess_helper([{:name, attrs, text} | tokens]) when text in @keyword_namespace do
    [{:keyword_namespace, attrs, text} | postprocess_helper(tokens)]
  end

  defp postprocess_helper([{:name, attrs, text} | tokens]) when text in @name_constant do
    [{:name_constant, attrs, text} | postprocess_helper(tokens)]
  end

  # Unused variables
  defp postprocess_helper([{:name, attrs, "_" <> _name = text} | tokens]),
    do: [{:comment, attrs, text} | postprocess_helper(tokens)]

  # Otherwise, don't do anything with the current token and go to the next token.
  defp postprocess_helper([token | tokens]), do: [token | postprocess_helper(tokens)]

  # Public API
  @impl Makeup.Lexer
  def postprocess(tokens, _opts \\ []), do: postprocess_helper(tokens)

  # ###################################################################
  # # Step #3: highlight matching delimiters
  # ###################################################################

  @impl Makeup.Lexer
  defgroupmatcher(:match_groups,
    parentheses: [
      open: [[{:punctuation, %{language: :c}, "("}]],
      close: [[{:punctuation, %{language: :c}, ")"}]]
    ],
    array: [
      open: [[{:punctuation, %{language: :c}, "["}]],
      close: [[{:punctuation, %{language: :c}, "]"}]]
    ],
    brackets: [
      open: [[{:punctuation, %{language: :c}, "{"}]],
      close: [[{:punctuation, %{language: :c}, "}"}]]
    ]
    # percent_string_interpolation: [
    #   open: [[{:punctuation, %{language: :c}, "%("}]],
    #   close: [[{:punctuation, %{language: :c}, ")"}]]
    # ],
    # f_string_interpolation: [
    #   open: [[{:punctuation, %{language: :c}, "f'"}]],
    #   close: [[{:punctuation, %{language: :c}, "'"}]]
    # ],
  )

  defp remove_initial_newline([{ttype, meta, text} | tokens]) do
    case to_string(text) do
      "\n" -> tokens
      "\n" <> rest -> [{ttype, meta, rest} | tokens]
    end
  end

  @impl Makeup.Lexer
  def lex(text, opts \\ []) do
    group_prefix = Keyword.get(opts, :group_prefix, random_prefix(10))
    {:ok, tokens, "", _, _, _} = root("\n" <> text)

    tokens
    |> remove_initial_newline()
    |> postprocess([])
    |> match_groups(group_prefix)
  end
end
