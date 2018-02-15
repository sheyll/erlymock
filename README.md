# Erlang Module Mocking

**Easy to use mocking library for unit testing**

* **[API Documentation](https://hexdocs.pm/em)**
* **[Rebar3 (hex) Installation](https://hex.pm/packages/em)**

## Example

`prouction.erl`:

    find_and_read_book(Isbn) ->
        {ok, Book} = library:lookup({isbn, Isbn}),
        media:view_book(Book).

`production_test.erl`:

    take_book_test() ->
        Mock = em:new(),
        ISBN = '978-3-596-17577-2',

        %% expectations:
        em:strict(Mock, library, lookup, [{isbn, ISBN}], {return, {ok, some_book_id}}).
        em:strict(Mock, media, view_book, [some_book_id], {return, ok}),
        em:replay(Mock),

        %% Run code under test:
        production:find_and_read_book(ISBN),
        %% assertions:
        em:verify(Mock).


## Installation

**Rebar3** and **[hex.pm](https://hex.pm/packages/em)** recommended.

Add this to your `rebar.config`:

    {em, "7.0.1"}
