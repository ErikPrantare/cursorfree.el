from talon import Context, Module
import talon, subprocess, json, threading

module = Module()

@module.capture(rule="[{user.color}] [{user.hat_shape}] <user.any_alphanumeric_key>")
def cursorfree_hat(m) -> list[str]:
    color = m.color if hasattr(m, "color") else "nil"
    shape = m.hat_shape if hasattr(m, "hat_shape") else "nil"

    escape = {"\\": "\\\\",
              "(": "\\(",
              ")": "\\)",
              "[": "\\[",
              "]": "\\]"}
    character = escape.get(m.any_alphanumeric_key, m.any_alphanumeric_key)
    return f"(cursorfree--pusher (cursorfree--make-target-from-hat ?{character} {color} {shape}))"

@module.capture(rule="car <user.any_alphanumeric_key>")
def cursorfree_quoted_char(m) -> list[str]:
    return f"(cursorfree--pusher ?{m.any_alphanumeric_key})"

@module.capture(rule=
                "{user.cursorfree_modifier}"
                "| <user.cursorfree_hat>"
                "| <user.cursorfree_quoted_char>"
                )
def cursorfree_nonterminator(m) -> list[str]:
    return m[0]

@module.capture(rule=
                "<user.cursorfree_nonterminator>+"
                "{user.cursorfree_action}")
def cursorfree_command(m) -> list[str]:
    nonterminators = " ".join(m.cursorfree_nonterminator_list)
    return f"(list {nonterminators} {m.cursorfree_action})";
