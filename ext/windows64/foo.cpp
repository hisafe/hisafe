// foo.cpp
#include "foo.hpp"


// Constructor
Foo::Foo()
{
}

int Foo::init(float initState)
{
	m_state = initState;
}

char const * Foo::getVersion ()
{
	return "4.5.6";
}

float Foo::getState ()
{
	m_state += 0.1;
	return m_state;
}
