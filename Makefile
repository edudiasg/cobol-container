DOCKER_COMPOSE := docker compose
SERVICE        := cobol

ifeq ($(RUNNING_IN_DOCKER),1)
COB      := cobc
SRC_DIR  := src
BIN_DIR  := bin

SOURCES  := $(wildcard $(SRC_DIR)/*.cbl)
TARGETS  := $(patsubst $(SRC_DIR)/%.cbl,$(BIN_DIR)/%,$(SOURCES))

.PHONY: all clean build run

# For targets that need a specific program, require program=NAME (without .cbl).
NEED_PROG_TARGETS := build run
ifneq ($(filter $(MAKECMDGOALS),$(NEED_PROG_TARGETS)),)
ifndef program
$(error define program=ex01 or similar)
endif
endif

all: $(TARGETS)

$(BIN_DIR)/%: $(SRC_DIR)/%.cbl | $(BIN_DIR)
	$(COB) -x -o $@ $<

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

build: $(BIN_DIR)/$(program)

run: build
	@echo ">> running $(program)"
	@$(BIN_DIR)/$(program); RC=$$?; echo "program exit code: $$RC"; exit $$RC

clean:
	rm -f $(BIN_DIR)/*
else
.PHONY: all clean build run shell image

all clean build run:
	$(DOCKER_COMPOSE) run --rm $(SERVICE) make RUNNING_IN_DOCKER=1 $@ program=$(program)

# Ad hoc shell access when you need to debug inside the image.
shell:
	$(DOCKER_COMPOSE) run --rm $(SERVICE)

image:
	$(DOCKER_COMPOSE) build

# Fallback: forward any other target to the container.
%:
	$(DOCKER_COMPOSE) run --rm $(SERVICE) make RUNNING_IN_DOCKER=1 $@
endif
