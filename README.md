# COBOL Project

Dockerized environment to build and run COBOL programs with GNUCobol, using `make` to orchestrate everything inside the container.

## Layout
- `src/` — COBOL sources (`.cbl`, e.g., `ex01.cbl`).
- `bin/` — generated executables (git-ignored).
- `data/input/` and `data/output/` — input/output files for the programs.
- `Makefile` — build/run commands that call Docker Compose.
- `docker-compose.yml` / `Dockerfile` — image and `cobol` service definition.
- `docs/` — extra docs.

## Prerequisites
- Docker + Docker Compose v2 (`docker compose`).
- `make` on the host (the actual build/run happens inside the container).

## First time
```sh
make image   # build the gnucobol-base image
```

## Main commands
- Build everything:  
  `make`
- Build a specific program (without extension):  
  `make build program=ex01`
- Build and run a specific program:  
  `make run program=ex01`  
  (prints DISPLAY output and the program exit code)
- Clean executables:  
  `make clean`
- Debug shell inside the container:  
  `make shell`

## Data files
- Example: `ex01.cbl` reads `data/input/ex01.dat` and writes `data/output/ex01.out`.
- For new programs/files, update the `SELECT ... ASSIGN TO` paths accordingly.

## Notes
- The `.:/usr/src/app` volume lets the container see the whole project, including the `Makefile`.
- If something loops, check file paths and the reported `program exit code: ...`.
- Quick reference: see `docs/USAGE.md`.
