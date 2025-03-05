#include "foo.hpp"

Foo * foo;

extern "C" {

int __declspec(dllexport) multiply (float a, float b, float & result);
int __declspec(dllexport) sumArray (float a [], int length, float & result);
char const * __declspec(dllexport) getVersion();
int __declspec(dllexport) addValue (float a [], int length, float value);

int __declspec(dllexport) initFoo (float initState);
char const * __declspec(dllexport) getFooVersion ();
float __declspec(dllexport) getFooState ();

}
