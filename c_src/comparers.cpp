#include "thrift/test_types.h"

/////////////////////////////////////////////////////////////////////////////

// Write an absent operator< functions, in case you use your custom
// data as a key in maps and sets

bool BookID::operator<(BookID const& other) const { return id < other.id; }
bool PersonID::operator<(PersonID const& other) const { return id < other.id; }

/////////////////////////////////////////////////////////////////////////////
