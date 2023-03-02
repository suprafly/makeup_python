defmodule Makeup.Lexers.PythonLexer do
  @moduledoc """
  A `Makeup` lexer for the Python language.

  This lexers handles Python 3.x code.

  TODO:

        'rfstringescape': [
            (r'\{\{', String.Escape),
            (r'\}\}', String.Escape),
        ],

        'soft-keywords-inner': [
            # optional `_` keyword
            (r'(\s+)([^\n_]*)(_\b)', bygroups(Whitespace, using(this), Keyword)),
            default('#pop')
        ],
  """

  import NimbleParsec
  import Makeup.Lexer.Combinators
  import Makeup.Lexer.Groups

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

  newlines =
    ascii_string([?\n, ?\f, ?\r], min: 1)
    |> token(:whitespace)

  # pygments chooses the `:text` token,
  # but pattern matching later it is much simpler if we choose `:whitespace`
  text =
    ascii_string([?\s, ?\t], min: 1)
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
    |> token(:comment_single)

  hashbang_comment =
    string("#!")
    |> concat(line)
    |> token(:comment_hashbang)

  unicode_char_in_string =
    string("\\u")
    |> ascii_string([?0..?9, ?a..?f, ?A..?F], 4)
    |> token(:string_escape)

  escaped_char =
    string("\\")
    |> utf8_string([], 1)
    |> token(:string_escape)

  variable =
    ascii_string([?a..?z, ?A..?Z, ?_], 1)
    |> optional(ascii_string([?a..?z, ?_, ?0..?9, ?A..?Z], min: 1))
    |> lexeme()
    |> token(:name)

  fromimport =
    string("from")
    |> ascii_string([?\s, ?\t], min: 1)
    |> ascii_string([?a..?z, ?A..?Z, ?_, ?0..?9, ?.], min: 1)
    |> repeat()
    |> ascii_string([?\s, ?\t], min: 1)
    |> lookahead(string("import"))
    |> token(:name)

  single_quoted_heredoc_affix =
    ascii_string([?\s, ?\t, ?\n, ?\r, ?\f], min: 0)
    |> ascii_char([?r, ?R, ?u, ?U, ?b, ?B])
    |> times(min: 1, max: 2)
    |> lookahead(string("'''"))
    |> token(:string_affix)

  double_quoted_heredoc_affix =
    ascii_string([?\s, ?\t, ?\n, ?\r, ?\f], min: 0)
    |> ascii_char([?r, ?R, ?u, ?U, ?b, ?B])
    |> times(min: 1, max: 2)
    |> lookahead(string(~S["""]))
    |> token(:string_affix)

  single_quoted_string_affix =
    ascii_string([?\s, ?\t, ?\n, ?\r, ?\f], min: 0)
    |> ascii_char([?r, ?R, ?u, ?U, ?b, ?B])
    |> times(min: 1, max: 2)
    |> lookahead(string("'"))
    |> token(:string_affix)

  double_quoted_string_affix =
    ascii_string([?\s, ?\t, ?\n, ?\r, ?\f], min: 0)
    |> ascii_char([?r, ?R, ?u, ?U, ?b, ?B])
    |> times(min: 1, max: 2)
    |> lookahead(string("\""))
    |> token(:string_affix)

  # %-formatting
  # "%s %s" %('Hello','World',)
  percent_string_interp = string_like("%(", ")", [variable], :string_interpol)

  bytes_escape =
    string("\\")
    |> choice([
        ascii_char([?\\, ?a, ?b, ?f, ?n, ?r, ?t, ?v, ?", ?']),
        ascii_char([?\n]),
        string("x") |> ascii_string([?a..?f, ?A..?F, ?0..?9], 2),
        ascii_string([?0..?7], min: 1, max: 3)
      ])
    |> token(:string_escape)

  string_escape =
    string("\\")
    |> choice([
      string("N{") |> ascii_string([0..122, 124, 126, 127], min: 1) |> string("}"),
      string("u") |> ascii_string([?a..?f, ?A..?F, ?0..?9], 4),
      string("U") |> ascii_string([?a..?f, ?A..?F, ?0..?9], 8)
      ])
    |> token(:string_escape)

  combinators_inside_string = [
    bytes_escape,
    string_escape,
    percent_string_interp,
    unicode_char_in_string,
    escaped_char
  ]

  single_quoted_heredocs = string_like("'''", "'''", combinators_inside_string, :string_doc)
  double_quoted_heredocs = string_like(~S["""], ~S["""], combinators_inside_string, :string_doc)

  # f-strings
  # f'Hello {name}! This is {program}'
  inner_f_string_interp = string_like("{", "}", [variable], :string_interpol)

  f_string_interp_choices = [
    inner_f_string_interp,
    unicode_char_in_string,
    escaped_char
  ]

  single_quoted_f_string_interpolation = string_like("f'", "'", f_string_interp_choices, :string)

  double_quoted_f_string_interpolation =
    string_like(~S[f"], ~S["], f_string_interp_choices, :string)

  single_quoted_string = string_like("'", "'", combinators_inside_string, :string) # |> lookahead(string("\n"))
  double_quoted_string = string_like("\"", "\"", combinators_inside_string, :string) # |> lookahead(string("\n"))

  # f-string escapes
  # rf_string_escape_open = string("{{")
  # rf_string_escape_close = string("}}")

  match_case =
    ascii_string([?\t, ?\s], min: 0)
    |> choice([string("match"), string("case")])
    |> ascii_char([?\s])
    |> lookahead_not(ascii_char([?:, ?,, ?;, ?=, ?^, ?&, ?|, ?@, ?~, ?), ?], ?}]))
    |> lookahead(string(":"))
    |> token(:name)

  number_integer =
    ascii_char([?0..?9])
    |> ascii_string([?0..?9, ?_], min: 0)
    |> token(:number_integer)

  float_scientific_notation =
    ascii_string([?e, ?E], 1)
    |> optional(choice([string("-"), string("+")]))
    |> concat(ascii_char([?0..?9]) |> ascii_string([?0..?9, ?_], min: 0))

  float_exponent =
    ascii_char([?0..?9])
    |> ascii_string([?0..?9, ?_], min: 0)
    |> lookahead_not(string("_e"))

  # floats can start with a "." and no integer part
  number_float =
    ascii_char([?0..?9])
    |> optional(ascii_string([?0..?9, ?_], min: 0))
    |> string(".")
    |> optional(float_exponent)
    |> optional(float_scientific_notation)
    |> optional(ascii_char([?j, ?J]))
    |> token(:number_float)

  number_float_no_exponent =
    ascii_char([?0..?9])
    |> optional(ascii_string([?0..?9, ?_], min: 0))
    |> string(".")
    |> token(:number_float)

  number_float_no_mantissa =
    string(".")
    |> choice([
        float_exponent |> optional(float_scientific_notation),
        float_scientific_notation
      ])
    |> optional(ascii_char([?j, ?J]))
    |> token(:number_float)

  number_float_no_decimal =
    ascii_char([?0..?9])
    |> optional(ascii_string([?0..?9, ?_], min: 0))
    |> ascii_string([?e, ?E], 1)
    |> optional(choice([string("-"), string("+")]))
    |> ascii_char([?0..?9])
    |> optional(ascii_string([?0..?9, ?_], min: 0))
    |> optional(ascii_char([?j, ?J]))
    |> token(:number_float)

  number_bin =
    string("0b")
    |> concat(ascii_string([?0..?1, ?_], min: 1))
    |> token(:number_bin)

  number_oct =
    string("0o")
    |> concat(ascii_string([?0..?7, ?_], min: 1))
    |> token(:number_oct)

  number_hex =
    string("0x")
    |> concat(ascii_string([?0..?9, ?a..?f, ?A..?F, ?_], min: 1))
    |> token(:number_hex)

  decorator_line =
    choice([
      ascii_char([?\n]),
      ascii_char([40])
    ])
    |> lookahead_not()
    |> utf8_string([], 1)
    |> repeat()

  decorator =
    ascii_char([?\r, ?\n, ?\f])
    |> token(:whitespace)
    |> ascii_string([?\r, ?\s, ?\n, ?\f, ?\t], min: 0)
    |> token(:whitespace)
    |> string("@")
    |> concat(decorator_line)
    |> token(:name_decorator)
    |> post_traverse({:parse_decorator_whitespace, []})

  # Python3 has ellipsis: ...
  # and matrix mult: @

  operator =
    ~W(
        + - * / % ** //
        == != < > <= >= <>
        = += -= *= /= %= **= //= :=
        & | ^ ~ << >> ... @
        .
      )
    |> word_from_list()
    |> token(:operator)

  punctuation =
    [
      ":",
      ";",
      ",",
      ".",
      "%",
      "'",
      "\"",
      "#",
      "\\",
      "@"
    ]
    |> word_from_list()
    |> token(:punctuation)

  delimiters_punctuation =
    ~W(( \) [ ] { } , : ` ;)
    |> word_from_list()
    |> token(:punctuation)

  delimiter_pairs = [
    delimiters_punctuation
  ]

  # This does the work of parsing
  root_element_combinator =
    choice(
      [
        # Floats must come before integers and before punctuation, because a float can
        # start with a ".". They must also come before white space because a float
        # can be missing the mantissa, ex: .7
        number_float,
        number_float_no_decimal,
        number_float_no_exponent,
        number_float_no_mantissa,

        # Decorator needs to come before Operators, because @ is also used for matrix multiplication
        decorator,

        newlines,
        text,

        # Comments
        hashbang_comment,
        inline_comment,
        single_quoted_string_affix,
        double_quoted_string_affix,
        single_quoted_heredoc_affix,
        double_quoted_heredoc_affix,
        single_quoted_heredocs,
        double_quoted_heredocs,

        # escapes,
        # bytes_escape,
        # string_escape,
        # rf_string_escape_open,
        # rf_string_escape_close,

        single_quoted_string,
        double_quoted_string,

        # String interpolation
        single_quoted_f_string_interpolation,
        double_quoted_f_string_interpolation
      ] ++
        [
          fromimport,

          # Operators
          operator,
        ] ++
        delimiter_pairs ++
        # escapes ++
        [
          # Numbers
          number_bin,
          number_oct,
          number_hex,
          number_integer,

          match_case,

          # Names
          variable,
          decorator,
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

  @keywords [
    "assert",
    "async for",
    "async",
    "await",
    "break",
    "continue",
    "del",
    "elif",
    "else",
    "except",
    "finally",
    "for",
    "global",
    "if",
    "lambda",
    "pass",
    "raise",
    "nonlocal",
    "return",
    "try",
    "while",
    "yield from",
    "yield",
    "as",
    "with",
    "match",
    "case"
  ]

  @builtins [
    "__import__",
    "abs",
    "aiter",
    "all",
    "any",
    "bin",
    "bool",
    "bytearray",
    "breakpoint",
    "bytes",
    "callable",
    "chr",
    "classmethod",
    "compile",
    "complex",
    "delattr",
    "dict",
    "dir",
    "divmod",
    "enumerate",
    "eval",
    "filter",
    "float",
    "format",
    "frozenset",
    "getattr",
    "globals",
    "hasattr",
    "hash",
    "hex",
    "id",
    "input",
    "int",
    "isinstance",
    "issubclass",
    "iter",
    "len",
    "list",
    "locals",
    "map",
    "max",
    "memoryview",
    "min",
    "next",
    "object",
    "oct",
    "open",
    "ord",
    "pow",
    "print",
    "property",
    "range",
    "repr",
    "reversed",
    "round",
    "set",
    "setattr",
    "slice",
    "sorted",
    "staticmethod",
    "str",
    "sum",
    "super",
    "tuple",
    "type",
    "vars",
    "zip"
  ]

  @builtin_pseudos [
    "self",
    "Ellipsis",
    "NotImplemented",
    "cls"
  ]

  @exceptions [
    "ArithmeticError",
    "AssertionError",
    "AttributeError",
    "BaseException",
    "BufferError",
    "BytesWarning",
    "DeprecationWarning",
    "EOFError",
    "EnvironmentError",
    "Exception",
    "FloatingPointError",
    "FutureWarning",
    "GeneratorExit",
    "IOError",
    "ImportError",
    "ImportWarning",
    "IndentationError",
    "IndexError",
    "KeyError",
    "KeyboardInterrupt",
    "LookupError",
    "MemoryError",
    "NameError",
    "NotImplementedError",
    "OSError",
    "OverflowError",
    "PendingDeprecationWarning",
    "ReferenceError",
    "ResourceWarning",
    "RuntimeError",
    "RuntimeWarning",
    "StopIteration",
    "SyntaxError",
    "SyntaxWarning",
    "SystemError",
    "SystemExit",
    "TabError",
    "TypeError",
    "UnboundLocalError",
    "UnicodeDecodeError",
    "UnicodeEncodeError",
    "UnicodeError",
    "UnicodeTranslateError",
    "UnicodeWarning",
    "UserWarning",
    "ValueError",
    "VMSError",
    "Warning",
    "WindowsError",
    "ZeroDivisionError",
    "BlockingIOError",
    "ChildProcessError",
    "ConnectionError",
    "BrokenPipeError",
    "ConnectionAbortedError",
    "ConnectionRefusedError",
    "ConnectionResetError",
    "FileExistsError",
    "FileNotFoundError",
    "InterruptedError",
    "IsADirectoryError",
    "NotADirectoryError",
    "PermissionError",
    "ProcessLookupError",
    "TimeoutError",
    "StopAsyncIteration",
    "ModuleNotFoundError",
    "RecursionError",
    "EncodingWarning"
  ]

  @magic_funcs [
    "__abs__",
    "__add__",
    "__aenter__",
    "__aexit__",
    "__aiter__",
    "__and__",
    "__anext__",
    "__await__",
    "__bool__",
    "__bytes__",
    "__call__",
    "__complex__",
    "__contains__",
    "__del__",
    "__delattr__",
    "__delete__",
    "__delitem__",
    "__dir__",
    "__divmod__",
    "__enter__",
    "__eq__",
    "__exit__",
    "__float__",
    "__floordiv__",
    "__format__",
    "__ge__",
    "__get__",
    "__getattr__",
    "__getattribute__",
    "__getitem__",
    "__gt__",
    "__hash__",
    "__iadd__",
    "__iand__",
    "__ifloordiv__",
    "__ilshift__",
    "__imatmul__",
    "__imod__",
    "__imul__",
    "__index__",
    "__init__",
    "__instancecheck__",
    "__int__",
    "__invert__",
    "__ior__",
    "__ipow__",
    "__irshift__",
    "__isub__",
    "__iter__",
    "__itruediv__",
    "__ixor__",
    "__le__",
    "__len__",
    "__length_hint__",
    "__lshift__",
    "__lt__",
    "__matmul__",
    "__missing__",
    "__mod__",
    "__mul__",
    "__ne__",
    "__neg__",
    "__new__",
    "__next__",
    "__or__",
    "__pos__",
    "__pow__",
    "__prepare__",
    "__radd__",
    "__rand__",
    "__rdivmod__",
    "__repr__",
    "__reversed__",
    "__rfloordiv__",
    "__rlshift__",
    "__rmatmul__",
    "__rmod__",
    "__rmul__",
    "__ror__",
    "__round__",
    "__rpow__",
    "__rrshift__",
    "__rshift__",
    "__rsub__",
    "__rtruediv__",
    "__rxor__",
    "__set__",
    "__setattr__",
    "__setitem__",
    "__str__",
    "__sub__",
    "__subclasscheck__",
    "__truediv__",
    "__xor__"
  ]

  @magic_vars [
    "__annotations__",
    "__bases__",
    "__class__",
    "__closure__",
    "__code__",
    "__defaults__",
    "__dict__",
    "__doc__",
    "__file__",
    "__func__",
    "__globals__",
    "__kwdefaults__",
    "__module__",
    "__mro__",
    "__name__",
    "__objclass__",
    "__qualname__",
    "__self__",
    "__slots__",
    "__weakref__"
  ]

  @operator_word ["and", "or", "not", "in", "is"]
  @keyword_namespace ~W[import from as]
  @keyword_constant ~W[True False None]
  @whitespace ["\n", "\t"]

  defp postprocess_helper([]), do: []

  defp postprocess_helper([
         {:name, attrs1, text1},
         {:whitespace, _, _} = ws,
         {:name, attrs2, text2} | tokens
       ])
       when text1 == "def" do
    [
      {:keyword_declaration, attrs1, text1},
      postprocess_whitespace(ws),
      {:name_function, attrs2, text2} | postprocess_helper(tokens)
    ]
  end

  defp postprocess_helper([
         {:name, attrs1, text1},
         {:whitespace, _, _} = ws,
         {:name, attrs2, text2} | tokens
       ])
       when text1 == "class" do
    [
      {:keyword_declaration, attrs1, text1},
      postprocess_whitespace(ws),
      {:name_class, attrs2, text2} | postprocess_helper(tokens)
    ]
  end

  defp postprocess_helper([
         {:name, attrs, text},
         {:whitespace, _, _} = ws
         | tokens
       ])
       when text in @keywords do
    [
      {:keyword, attrs, text},
      postprocess_whitespace(ws) | postprocess_helper(tokens)
    ]
  end

  defp postprocess_helper([
         {:name, attrs, text},
         {:punctuation, _, text2} = p
         | tokens
       ])
       when text in @keywords and text2 in ["(", ":"] do
    [
      {:keyword, attrs, text}, p | postprocess_helper(tokens)
    ]
  end

  defp postprocess_helper([
         {:name, attrs, text},
         {:whitespace, _, _} = ws
         | tokens
       ])
       when text in @exceptions do
    [
      {:name_exception, attrs, text},
      postprocess_whitespace(ws) | postprocess_helper(tokens)
    ]
  end

  defp postprocess_helper([
         {:name, attrs, text},
         {:whitespace, _, _} = ws
         | tokens
       ])
       when text in @builtin_pseudos do
    [
      {:name_builtin_pseudo, attrs, text},
      postprocess_whitespace(ws) | postprocess_helper(tokens)
    ]
  end

  defp postprocess_helper([
         {:name, attrs, text},
         {:whitespace, _, _} = ws
         | tokens
       ])
       when text in @builtins do
    [
      {:name_builtin, attrs, text},
      postprocess_whitespace(ws) | postprocess_helper(tokens)
    ]
  end

  defp postprocess_helper([
         {:name, attrs, text},
         {:whitespace, _, _} = ws
         | tokens
       ])
       when text in @magic_funcs do
    [
      {:name_function_magic, attrs, text},
      postprocess_whitespace(ws) | postprocess_helper(tokens)
    ]
  end

  defp postprocess_helper([
         {:name, attrs, text},
         {:whitespace, _, _} = ws
         | tokens
       ])
       when text in @magic_vars do
    [
      {:name_variable_magic, attrs, text},
      postprocess_whitespace(ws) | postprocess_helper(tokens)
    ]
  end

  defp postprocess_helper([{:name, attrs, text} | tokens]) when text in @operator_word do
    [{:operator_word, attrs, text} | postprocess_helper(tokens)]
  end

  defp postprocess_helper([
         {:name, attrs, [text | rem_text]} | tokens
       ])
       when text in @keyword_namespace do
    [ws1 | rem_text] = rem_text
    {ws2, [inner_text]} = List.pop_at(rem_text, -1)

    # We want to parse 'import' properly, so add the whitespace and continue
    tokens = [{:whitespace, attrs, ws2} | tokens]

    [
      {:keyword_namespace, attrs, text},
      postprocess_whitespace({:whitespace, attrs, ws1}),
      {:name, attrs, inner_text}
       | postprocess_helper(tokens)
    ]
  end

  defp postprocess_helper([
         {:name, attrs, text},
         {:whitespace, _, _} = ws,
         {:name, attrs2, text2} | tokens
       ])
       when text in @keyword_namespace do
    [
      {:keyword_namespace, attrs, text},
      postprocess_whitespace(ws),
      {:name, attrs2, text2} | postprocess_helper(tokens)
    ]
  end

  defp postprocess_helper([
         {:name, attrs, text},
         {:whitespace, _, _} = ws,
         {:operator, attrs2, "*"} | tokens
       ])
       when text in @keyword_namespace do
    [
      {:keyword_namespace, attrs, text},
      postprocess_whitespace(ws),
      {:operator, attrs2, "*"} | postprocess_helper(tokens)
    ]
  end

  defp postprocess_helper([
         {:name, attrs, text},
         {:whitespace, _, _} = ws,
         {:name, attrs2, text2} | tokens
       ])
       when text in @keyword_namespace do
    [
      {:keyword_namespace, attrs, text},
      postprocess_whitespace(ws),
      {:name, attrs2, text2} | postprocess_helper(tokens)
    ]
  end

  defp postprocess_helper([{:name, attrs, text} | tokens]) when text in @keyword_constant do
    [{:keyword_constant, attrs, text} | postprocess_helper(tokens)]
  end

  defp postprocess_helper([{:number_integer, attrs, text} | tokens]) when is_list(text) do
    split_underscores(:number_integer, attrs, text, tokens)
  end

  defp postprocess_helper([{:number_float, attrs, text} | tokens]) when is_list(text) do
    split_underscores(:number_float, attrs, text, tokens)
  end

  defp postprocess_helper([{:number_bin, attrs, text} | tokens]) when is_list(text) do
    split_underscores(:number_bin, attrs, text, tokens)
  end

  defp postprocess_helper([{:number_oct, attrs, text} | tokens]) when is_list(text) do
    split_underscores(:number_oct, attrs, text, tokens)
  end

  defp postprocess_helper([{:number_hex, attrs, text} | tokens]) when is_list(text) do
    split_underscores(:number_hex, attrs, text, tokens)
  end

  defp postprocess_helper([{:comment_single, attrs, text} | tokens]) do
    [{:comment_single, attrs, Enum.join(text)} | postprocess_helper(tokens)]
  end

  defp postprocess_helper([{:comment_hashbang, attrs, text} | tokens]) do
    [{:comment_hashbang, attrs, Enum.join(text)} | postprocess_helper(tokens)]
  end

  defp postprocess_helper([{:string_doc, attrs, text} | tokens]) do
    [{:string_doc, attrs, to_string(text)} | postprocess_helper(tokens)]
  end

  defp postprocess_helper([{:string_affix, attrs, text} | tokens]) do
    [{:string_affix, attrs, to_string(text)} | postprocess_helper(tokens)]
  end

  defp postprocess_helper([{:string, attrs, text} | tokens]) do
    [{:string, attrs, to_string(text)} | postprocess_helper(tokens)]
  end

  defp postprocess_helper([{:string_escape, attrs, text} | tokens]) do
    [{:string_escape, attrs, to_string(text)} | postprocess_helper(tokens)]
  end

  # ----------------------------------------------------------------------------
  # todo - I think we do not need this
  # ----------------------------------------------------------------------------
  # defp postprocess_helper([
  #        {:name_decorator, attrs, [text1, _text2] = decorator},
  #        {next_token, _, _} = t2 | tokens
  #      ])
  #      when text1 == "@" and next_token != :whitespace do
  #   [{:name_decorator, attrs, decorator} | postprocess_helper([t2 | tokens])]
  # end
  # ----------------------------------------------------------------------------

  defp postprocess_helper([{:name_decorator, attrs, text} | tokens]) do
    [{:name_decorator, attrs, to_string(text)} | postprocess_helper(tokens)]
  end

  # Unused variables
  defp postprocess_helper([{:name, attrs, "_" <> _name = text} | tokens]) do
    [{:Name, attrs, text} | postprocess_helper(tokens)]
  end

  # Otherwise, don't do anything with the current token and go to the next token.
  defp postprocess_helper([token | tokens]) do
    [token | postprocess_helper(tokens)]
  end

  defp split_underscores(token_type, attrs, text, tokens) do
    # Put the string together, then attempt to split it on the first double underscores
    [first_part | second_part] = text |> to_string() |> String.split("__", parts: 2)

    with [] <- second_part,
         true <- String.ends_with?(first_part, "_")
    do
      # In the case of a trailing underscore but with no double underscores, we
      # pop it off and tokenize it as a :name
      first_part = String.replace_trailing(first_part, "_", "")
      extra_tokens = [{:name, attrs, "_"}]
      [{token_type, attrs, first_part} | postprocess_helper(extra_tokens ++ tokens)]
    else
      [second_part] ->
        # We don't need to worry about a trailing underscore, because everything after
        # the double underscore is tokenized as a :name
        extra_tokens = [{:name, attrs, "__#{second_part}"}]
        [{token_type, attrs, first_part} | postprocess_helper(extra_tokens ++ tokens)]

      false ->
        # In this case, there is not trailing underscore, so we proceed as normal
        [{token_type, attrs, first_part} | postprocess_helper(tokens)]
    end
  end

  defp postprocess_whitespace({:whitespace, attrs, text}) when text in @whitespace do
    {:whitespace, attrs, text}
  end

  defp postprocess_whitespace({:whitespace, attrs, text}) do
    {:text, attrs, text}
  end

  # Public API
  @impl Makeup.Lexer
  def postprocess(tokens, _opts \\ []), do: postprocess_helper(tokens)
    # |> IO.inspect(limit: :infinity, pretty: true) # for debugging
    # |> tap(&Enum.map(&1, fn {t, _, v} -> {t, v} end) |> IO.inspect(limit: :infinity, pretty: true))

  # ###################################################################
  # # Step #3: highlight matching delimiters
  # ###################################################################

  @impl Makeup.Lexer
  defgroupmatcher(:match_groups,
    parentheses: [
      open: [[{:punctuation, %{language: :python}, "("}]],
      close: [[{:punctuation, %{language: :python}, ")"}]]
    ],
    array: [
      open: [[{:punctuation, %{language: :python}, "["}]],
      close: [[{:punctuation, %{language: :c}, "]"}]]
    ],
    brackets: [
      open: [[{:punctuation, %{language: :python}, "{"}]],
      close: [[{:punctuation, %{language: :python}, "}"}]]
    ],
    percent_interpolation: [
      open: [
        [{:string_interpol, %{language: :python}, "%("}]
      ],
      close: [
        [{:string_interpol, %{language: :python}, ")"}]
      ]
    ],
    f_string_interpolation: [
      open: [
        [{:string_interpol, %{language: :python}, "f'"}]
      ],
      close: [
        [{:string_interpol, %{language: :python}, "'"}]
      ]
    ]
  )

  defp remove_initial_newline([{ttype, meta, text} | tokens]) do
    case to_string(text) do
      "\n" -> tokens
      "\n" <> rest -> [{ttype, meta, rest} | tokens]
    end
  end

  defp parse_decorator_whitespace(rest, args, context, _line, _offset) do
    # parse the newline and optional whitespace from the decorators
    # and rebuild the token list with them in front of the decorator.
    [{token, attrs, [nested_tokens | text]}] = args
    decorator_token = {token, attrs, text}
    {ws1, ws_attrs1, [n1 | n2]} = nested_tokens
    args =
      if length(n2) > 0 do
        [n2] = n2
        t1 = {ws1, ws_attrs1, n2}
        [decorator_token, t1, n1]
      else
        [decorator_token, n1]
      end

    {rest, args, context}
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
