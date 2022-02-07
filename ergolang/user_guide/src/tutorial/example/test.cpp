#define CATCH_CONFIG_MAIN
#include "catch2/catch.hpp"
#include "lib.h"

TEST_CASE("Adds 2 to the input value", "[add_two]") {
	REQUIRE(lib::add_two(0) == 2);
	REQUIRE(lib::add_two(1) == 3);
	REQUIRE(lib::add_two(10) == 12);
}
