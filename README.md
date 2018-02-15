# Erlang Module Mocking

**Easy to use mocking library for unit testing**

* **[Documentation](https://hexdocs.pm/em)**
* **[Hex-Installation](https://hex.pm/packages/em)**
* **[GitHub](https://github.com/sheyll/erlymock)**

## Example

`prouction.erl`:

    read_a_book({isbn, Isbn}) ->
        {ok, Book} = library:lookup_isbn(Isbn),
        media:view_book(Book).

`production_test.erl`:

    take_book_test() ->
        Mock = em:new(),
        ISBN = '978-3-596-17577-2',
        BookQuery = {isbn, ISBN},
        Book = some_book_id,

        %% expectations:
        em:strict(Mock, library, lookup_isbn, [ISBN], {return, {ok, Book}}).
        em:strict(Mock, media, view_book, [Book], {return, ok}),
        em:replay(Mock),

        %% Run code under test:
        production:find_and_view(BookQuery),
        %% assertions:
        em:verify(Mock).


## Installation

**Rebar3** and **[hex.pm](https://hex.pm/packages/em)** recommended.

Add this to your `rebar.config`:

    {em, "7.0.0"}
