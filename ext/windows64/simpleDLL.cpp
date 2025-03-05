
#include "simpleDLL.h"

#include <stdexcept>

using namespace std;

int multiply (float a, float b, float & result)
{
	result = a * b;
	return 0;
}

int sumArray (float a [], int length, float & result) {

	result = 0;
	for(int i=0; i<length; i++) {
		result += a[i];
	}
	return 0;
}

char const * getVersion()
{
	return "1.2.3";
}

int addValue (float a [], int length, float value)
{
	for (int i = 0; i < length; i++) {
		a[i] += value;
	}
	return 0;
}

// extern C functions wrapping the Foo class
// They can be called from Java by JNA
int initFoo (float initState)
{
	foo = new Foo ();
	foo->init(initState);
}

char const * getFooVersion ()
{
	return foo->getVersion ();
}

float getFooState ()
{
	return foo->getState ();
}
