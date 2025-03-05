// foo.hpp
#include <string>

class Foo
{
private:
	char const * m_version;
	float m_state;

public:
	Foo ();
	int init (float initState);
	char const * getVersion ();
	float getState ();
};
