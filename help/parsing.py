import logging
import uuid
from enum import StrEnum, auto

from treelib import Tree

class TokenType(StrEnum):
    DO = "do"
    ELSE = "else"
    IF = "if"
    WHILE = "while"
    LBRA = "{"
    RBRA = "}"
    LPAR = "("
    RPAR = ")"
    PLUS = "+"
    MINUS = "-"
    LESS = "<"
    SEMI = ";"
    EQUAL = "="
    INT = "INT"
    ID = "ID"
    EOI = "EndOfInput"


class Token():
    def __init__(self, type, value=None):
        self.type = type
        self.value = value

    def __repr__(self):
        match self.type:
            case TokenType.INT:
                return f"INT<{self.value}>"
            case TokenType.ID:
                return f"ID<{self.value}>"
            case _:
                return str(self.type)

    def __eq__(self, other):
        return (isinstance(other, Token)
                and other.type == self.type
                and other.value == self.value)


class LexerError(Exception):
    pass


class Lexer:
    def __init__(self, input):
        self._current_char = None
        assert isinstance(input, str)
        self._character_iter = iter(input)
        self.skip_once = False

    def next_character(self):
        if self.skip_once:
            self.skip_once = False
            return

        self._current_char = next(self._character_iter, None)
        logger.debug(f"Current character: {self._current_char}")

    def next_token(self):
        self.next_character()
        while self._current_char is not None:
            if self._current_char in (' ', '\n'):
                self.next_character()
                continue

            try:
                yield Token(TokenType(self._current_char))
                self.next_character()
                continue
            except ValueError:
                pass

            def is_digit():
                return self._current_char is not None and self._current_char.isnumeric()

            if is_digit():
                value = 0
                while is_digit():
                    value = value * 10 + int(self._current_char)
                    self._current_char = next(self._character_iter, None)
                self.skip_once = True
                yield Token(TokenType.INT, value)
                continue

            def is_alpha():
                return self._current_char is not None and self._current_char.isalpha()

            if is_alpha():
                id_name = ""
                while is_alpha():
                    id_name += self._current_char
                    self.next_character()
                self.skip_once = True
                try:
                    yield Token(TokenType(id_name))
                    continue
                except ValueError:
                    if len(id_name) == 1:
                        yield Token(TokenType.ID, id_name)
                        continue

                raise LexerError()

        yield Token(TokenType.EOI)

def add_nodes(treelib_tree, parent_id, node):
    if node is None:
        return

    # Add the current node to the treelib tree
    treelib_tree.create_node(node.kind, node.id, parent=parent_id)

    # Recur for each child
    for child in node.children:
        add_nodes(treelib_tree, node.id, child)


def convert_to_treelib(root):
    # Initialize the treelib tree
    treelib_tree = Tree()

    # Add the root node
    treelib_tree.create_node(root.kind, root.id)

    # Add children recursively
    for child in root.children:
        add_nodes(treelib_tree, root.id, child)

    return treelib_tree


def print_step(node):
    print(convert_to_treelib(node))


class NodeType(StrEnum):
    VAR = auto()
    CST = auto()
    ADD = auto()
    SUB = auto()
    LT = auto()
    SET = auto()
    IF1 = auto()
    IF2 = auto()
    WHILE = auto()
    DO = auto()
    EMPTY = auto()
    SEQ = auto()
    EXPR = auto()
    PROG = auto()


class Node:
    kind: NodeType

    def __init__(self, kind):
        self.kind = kind
        self.id = uuid.uuid1()

    @property
    def children(self):
        def get_child(name):
            if hasattr(self, name):
                return self.__dict__[name]

            return None

        return [get_child('o1'), get_child('o2'), get_child('o3')]


lexer: Lexer
sym = None
root = None


def next_sym():
    global sym
    sym = next(lexer.next_token()).type


def term():
    if sym == TokenType.ID:
        x = Node(NodeType.VAR)
        next_sym()
    elif sym == TokenType.INT:
        x = Node(NodeType.CST)
        next_sym()
    else:
        x = paren_expr()

    return x


def parse_sum():
    x = term()
    while sym == TokenType.PLUS or sym == TokenType.MINUS:
        t = x
        if sym == TokenType.PLUS:
            x = Node(NodeType.ADD)
        else:
            x = Node(NodeType.SUB)

        next_sym()
        x.o1 = t
        x.o2 = term()

    return x


def test():
    x = parse_sum()
    if sym == TokenType.LESS:
        t = x
        x = Node(NodeType.LT)
        next_sym()
        x.o1 = t
        x.o2 = parse_sum()

    return x


def expr():
    if sym != TokenType.ID:
        return test()

    x = test()
    if x.kind == NodeType.VAR and sym == TokenType.EQUAL:
        t = x
        x = Node(NodeType.SET)
        next_sym()
        x.o1 = t
        x.o2 = expr()

    return x


def paren_expr():
    if sym == TokenType.LPAR:
        next_sym()
    else:
        raise Exception()

    x = expr()

    if sym == TokenType.RPAR:
        next_sym()
    else:
        raise Exception()

    return x


def statement():
    if sym == TokenType.IF:
        x = Node(NodeType.IF1)
        next_sym()
        x.o1 = paren_expr()
        x.o2 = statement()

        if (sym == TokenType.ELSE):
            x.kind = NodeType.IF2
            next_sym()
            x.o3 = statement()
    elif sym == TokenType.WHILE:
        x = Node(NodeType.WHILE)
        next_sym()
        x.o1 = paren_expr()
        x.o2 = statement()

    elif sym == TokenType.DO:
        x = Node(NodeType.DO)
        next_sym()
        x.o1 = statement()
        if sym == TokenType.WHILE:
            next_sym()
        else:
            raise Exception()

        x.o2 = paren_expr()
        if sym == TokenType.SEMI:
            next_sym()
        else:
            raise Exception()

    elif sym == TokenType.SEMI:
        x = Node(NodeType.EMPTY)
        next_sym()

    elif sym == TokenType.LBRA:
        x = parse_sequence()
    else:
        x = Node(NodeType.EXPR)
        x.o1 = expr()
        if sym == TokenType.SEMI:
            next_sym()
        else:
            raise Exception()

    return x


def parse_sequence():
    x = Node(NodeType.EMPTY)
    next_sym()
    while sym != TokenType.RBRA:
        t = x
        x = Node(NodeType.SEQ)
        x.o1 = t
        x.o2 = statement()
    next_sym()
    return x


def parse_sequence_recursive():
    if sym == TokenType.LBRA:
        next_sym()
    if sym == TokenType.RBRA:
        node = Node(NodeType.EMPTY)
    else:
        node = Node(NodeType.SEQ)
        node.o1 = statement()
        node.o2 = parse_sequence_recursive()

    next_sym()
    return node


def program():
    global root
    root = Node(NodeType.PROG)
    next_sym()
    root.o1 = statement()
    if sym != TokenType.EOI:
        raise Exception()

    return root


lexer = Lexer("{ i=7; if (i<5) x=1; if (i<10) y=2; }")
print_step(program())


# now with pure recursive descent
lexer = Lexer("{ i=7; if (i<5) x=1; if (i<10) y=2; }")
parse_sequence = parse_sequence_recursive
print_step(program())

