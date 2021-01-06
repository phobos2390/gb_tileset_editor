NAME := tileset_editor
TEST_NAME := test_$(NAME)
MBC_TYPE := 0x1B
RAM_SIZE := 0x2

build:
	@mkdir -p build
	rgbasm -i src src/*.asm -o build/$(NAME).o 
	rgblink -o build/$(NAME).gb build/$(NAME).o -m build/$(NAME).map -n build/$(NAME).sym
	rgbfix -m $(MBC_TYPE) -r $(RAM_SIZE) -v -p 0 build/$(NAME).gb

build_test:
	@mkdir -p build
	rgbasm -i src -i src/test_engine -i src/test src/test_engine/*.asm -o build/$(TEST_NAME).o 
	rgblink -o build/$(TEST_NAME).gb build/$(TEST_NAME).o -m build/$(TEST_NAME).map -n build/$(TEST_NAME).sym
	rgbfix -v -p 0 build/$(TEST_NAME).gb

clean:
	rm -rf build/

test: build_test
	sameboy_debugger build/$(TEST_NAME).gb

run: build
	sameboy build/$(NAME).gb

debug: build
	sameboy_debugger build/$(NAME).gb

all: build build_test