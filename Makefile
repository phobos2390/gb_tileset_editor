NAME := tileset_editor
TEST_NAME := test_$(NAME)
MBC_TYPE := 0x1B
RAM_SIZE := 0x2
UTILS_DIR := ./gb_asm_utils
TEST_DIR := ./gb_asm_test
TEST_ENGINE_DIR := $(TEST_DIR)/src
TEST_DIRECTORY := ./src/test
ADDITIONAL_INCLUDES := "-i ./src"
EMULATOR := sameboy

build:
	@mkdir -p build
	rgbasm -i $(UTILS_DIR)/src -i src src/*.asm -o build/$(NAME).o 
	rgblink -o build/$(NAME).gb build/$(NAME).o -m build/$(NAME).map -n build/$(NAME).sym
	rgbfix -c -m $(MBC_TYPE) -r $(RAM_SIZE) -v -p 0 build/$(NAME).gb

build_test:
	make -f $(TEST_DIR)/Makefile build_test \
	    ADDITIONAL_INCLUDES=$(ADDITIONAL_INCLUDES) \
	    NAME=$(NAME) \
	    TEST_NAME=$(TEST_NAME) \
	    TEST_ENGINE_DIR=$(TEST_ENGINE_DIR) \
	    TEST_DIRECTORY=$(TEST_DIRECTORY) 

clean:
	rm -rf build/

test: build_test
	sameboy_debugger build/$(TEST_NAME).gb

run: build
	$(EMULATOR) build/$(NAME).gb

debug: build
	sameboy_debugger build/$(NAME).gb

all: build build_test