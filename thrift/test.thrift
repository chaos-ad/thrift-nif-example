struct BookID
{
    1: required i32                         id;
}

struct PersonID
{
    1: required i64                         id;
}

struct Person
{
    1: required PersonID                    id;
    2: optional string                      first_name;
    3: optional string                      second_name;
    4: list<BookID>                         books_written;
}

struct Book
{
    1: required BookID                      id;
    3: optional PersonID                    author;
    4: optional string                      title;
    5: optional string                      content;
}

struct Library
{
    1: required map<BookID, Book>           books;
}

struct Registry
{
    1: required map<PersonID, Person>       persons;
}
