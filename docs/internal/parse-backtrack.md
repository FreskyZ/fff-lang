
`a.0.0` and format string require some level of backtracking (I expect not actually backtracking but only discard lookahead buffer), write down them for (will be complex) +1/-1 problem

parse_expr: buf=['a', '.']
parse_postfix_expr: current_expr=name{a}, buf=['.', '0.0']
parse_tuple_index_expr: dot_span=..., buf=['0.0']: not expect_numeric, should be expect_int: scanner.retry_non_rational(spans[0]) -> chars.set_position(spans[0].start)


`f"abc {def + ghi} jkl"` or `f"abc {def + ghi:?} jkl"`

parse_primary_expr: buf[f"abc {", def]
parse_format_string: segments=["abc "], buf=[def, +]
parse_binary_expr: current_expr=name{def}, op=+, buf=[ghi, }]
parse_format_string: segments=["abc ", expr{def + ghi}], buf=[}, jkl], not expect_str_lit, should be expect_format_string_continue: scanner.retry_format_string_continue(spans[0]) -> chars.set_position, scanner.parse_string_literal(as_format_string_continue)

or
parse_format_string: segments=["abc", expr{def + ghi}], buf=[':', '?'], colon_span=..., scanner.retry_format_specifier() -> chars.set_position, scanner.parse_string_literal(as_format_specifier)
parse_format_string: segments=["abc", segment{expr{def + ghi}, colon_span, format_specifier{?}}], buf=[}, jkl], scanner.retry_format_string_continue() -> chars.set_position, scanner.parse_string_literal(as_format_string_continue)
