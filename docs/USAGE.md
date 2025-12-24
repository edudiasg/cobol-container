# Usage Guide

Refer to the root `README.md` for the full overview. This guide is a concise reference for day-to-day tasks with Docker Compose and `make`.

## Build the image
- `make image`

## Compile
- All programs: `make`
- One program (no extension): `make build program=ex01`

## Run
- Compile and run one program: `make run program=ex01`  
  (shows DISPLAY output and exit code)

## Clean
- Remove executables: `make clean`

## Debug
- Open a shell inside the container: `make shell`
