# Build and run a preloaded CWP dataset comparison image

This document explains how to build, run, tag, and publish a Docker image of the **CWP Dashboard** with two datasets already embedded for comparison.

The intended workflow is:

```text
Dashboard application image
        +
Data image containing .qs datasets
        |
        v
Dockerfile.with-gta-data
        |
        v
Standalone comparison image
        |
        v
docker run / docker compose
```

A comparison image contains:

- the Shiny Dashboard application;
- dataset 1;
- dataset 2;
- the default titles associated with both datasets;
- the configuration required to start directly in preloaded comparison mode.

Once built, the image can be launched without mounting the datasets manually.

---

## 1. Requirements

You need:

- Docker;
- access to the Dashboard repository;
- an application image containing the Dashboard code;
- a data image containing the `.qs` files to compare.

Example images used in this project:

```text
ghcr.io/firms-gta/dashboard-cwp-app:main
ghcr.io/firms-gta/tunaatlas-data:main
```

The data image is expected to expose its datasets under:

```text
/data
```

For example:

```text
/data/global_catch_ird_level1_1950_2024_harmonized_20834708.qs
/data/global_catch_ird_level2_1950_2024_harmonized_20834708.qs
```

---

## 2. Dataset requirements

The Dashboard comparison mode expects two datasets compatible with the CWP data model.

At minimum, the datasets must contain the variables required by the Dashboard and `CWP.dataset`, including:

```text
measurement_value
measurement_unit
time_start
geographic_identifier
```

Additional dimensions can include, for example:

```text
source_authority
species
gear_type
fishing_fleet
fishing_mode
measurement
measurement_type
measurement_processing_level
```

The Dashboard automatically identifies columns that are common to both datasets.

Only common compatible dimensions are proposed for comparison and filtering.

Some compatibility preparation is performed automatically by the Dashboard before analysis, including:

- deriving `year`, `month`, and `quarter` from `time_start` when missing;
- harmonising categorical variables as character columns;
- converting `measurement_unit = TRUE` to `measurement_unit = "t"`.

Columns existing only in one of the two datasets are not proposed as common comparison dimensions.

---

# Quick start

## 3. Build the Dashboard application image

Rebuild the application image whenever the Dashboard code changes.

From the root of `DashboardCWPdataset`:

```bash
docker build \
  -f Dockerfile \
  -t dashboard-cwp-app:local \
  .
```

To build directly with a GHCR name:

```bash
docker build \
  -f Dockerfile \
  -t ghcr.io/firms-gta/dashboard-cwp-app:main \
  .
```

If required, push it:

```bash
docker push ghcr.io/firms-gta/dashboard-cwp-app:main
```

If the application image already exists and no code change is required:

```bash
docker pull ghcr.io/firms-gta/dashboard-cwp-app:main
```

---

## 4. Inspect the available datasets in a data image

The final `tunaatlas-data` image can be minimal and may not contain a shell.

Therefore, do not assume that commands such as `sh`, `bash`, or `find` can be executed directly inside it.

To inspect its `/data` directory, create a temporary container and copy the directory locally:

```bash
docker rm -f tunaatlas-data-check 2>/dev/null || true

docker create \
  --name tunaatlas-data-check \
  ghcr.io/firms-gta/tunaatlas-data:main \
  /bin/true

rm -rf /tmp/tunaatlas-data-check
mkdir -p /tmp/tunaatlas-data-check

docker cp \
  tunaatlas-data-check:/data/. \
  /tmp/tunaatlas-data-check/

find /tmp/tunaatlas-data-check \
  -type f \
  -printf '%f\t%k KB\n'

docker rm tunaatlas-data-check
```

This is useful to identify the exact filenames to pass to:

```text
DATASET_1_FILE
DATASET_2_FILE
```

---

# Manual Docker workflow

## 5. Build a comparison image

The comparison image is built with:

```text
Dockerfile.with-gta-data
```

The main build arguments are:

| Argument | Purpose |
|---|---|
| `APP_IMAGE` | Dashboard application image |
| `DATA_IMAGE` | Image containing the datasets |
| `DATASET_1_FILE` | First `.qs` file under `/data` |
| `DATASET_2_FILE` | Second `.qs` file under `/data` |
| `DATASET_1_TITLE` | Display name for dataset 1 |
| `DATASET_2_TITLE` | Display name for dataset 2 |

### Example: GTA Level 1 vs GTA Level 2

```bash
docker build \
  -f Dockerfile.with-gta-data \
  --build-arg APP_IMAGE=ghcr.io/firms-gta/dashboard-cwp-app:main \
  --build-arg DATA_IMAGE=ghcr.io/firms-gta/tunaatlas-data:main \
  --build-arg DATASET_1_FILE=global_catch_ird_level1_1950_2024_harmonized_20834708.qs \
  --build-arg DATASET_2_FILE=global_catch_ird_level2_1950_2024_harmonized_20834708.qs \
  --build-arg DATASET_1_TITLE="GTA Level 1" \
  --build-arg DATASET_2_TITLE="GTA Level 2" \
  -t ghcr.io/firms-gta/dashboard-cwp:level1-vs-level2 \
  .
```

During this build:

```text
/data/<DATASET_1_FILE>
```

from the data image is copied to:

```text
/data/dataset_1.qs
```

and:

```text
/data/<DATASET_2_FILE>
```

is copied to:

```text
/data/dataset_2.qs
```

inside the final comparison image.

The application then loads these standard paths automatically.

---

## 6. Run the comparison image

Run:

```bash
docker run --rm \
  -p 3838:3838 \
  ghcr.io/firms-gta/dashboard-cwp:level1-vs-level2
```

Then open:

```text
http://localhost:3838
```

The Dashboard starts directly in preloaded comparison mode.

No data volume is required at runtime.

---

## 7. Run on another local port

For example, to expose the application on port `8080`:

```bash
docker run --rm \
  -p 8080:3838 \
  ghcr.io/firms-gta/dashboard-cwp:level1-vs-level2
```

Then open:

```text
http://localhost:8080
```

---

## 8. Build using a local application image

During development, the application image can remain local.

Example:

```bash
docker build \
  -f Dockerfile \
  -t dashboard-cwp-app:local \
  .
```

Then build the comparison image from it:

```bash
docker build \
  -f Dockerfile.with-gta-data \
  --build-arg APP_IMAGE=dashboard-cwp-app:local \
  --build-arg DATA_IMAGE=ghcr.io/firms-gta/tunaatlas-data:main \
  --build-arg DATASET_1_FILE=global_catch_ird_level1_1950_2024_harmonized_20834708.qs \
  --build-arg DATASET_2_FILE=global_catch_ird_level2_1950_2024_harmonized_20834708.qs \
  --build-arg DATASET_1_TITLE="GTA Level 1" \
  --build-arg DATASET_2_TITLE="GTA Level 2" \
  -t dashboard-cwp:level1-vs-level2 \
  .
```

Run:

```bash
docker run --rm \
  -p 3838:3838 \
  dashboard-cwp:level1-vs-level2
```

This is the recommended workflow while modifying the Dashboard code locally.

---

# Building other comparisons

## 9. Compare another pair of datasets

Only the following values normally need to change:

```text
DATA_IMAGE
DATASET_1_FILE
DATASET_2_FILE
DATASET_1_TITLE
DATASET_2_TITLE
final Docker tag
```

For example:

```bash
docker build \
  -f Dockerfile.with-gta-data \
  --build-arg APP_IMAGE=ghcr.io/firms-gta/dashboard-cwp-app:main \
  --build-arg DATA_IMAGE=ghcr.io/firms-gta/tunaatlas-data:main \
  --build-arg DATASET_1_FILE=old_dataset.qs \
  --build-arg DATASET_2_FILE=updated_dataset.qs \
  --build-arg DATASET_1_TITLE="Previous version" \
  --build-arg DATASET_2_TITLE="Updated version" \
  -t ghcr.io/firms-gta/dashboard-cwp:old-vs-updated \
  .
```

Run:

```bash
docker run --rm \
  -p 3838:3838 \
  ghcr.io/firms-gta/dashboard-cwp:old-vs-updated
```

---

## 10. Suggested image naming convention

Use a tag describing the comparison rather than the build date whenever possible.

Examples:

```text
ghcr.io/firms-gta/dashboard-cwp:level1-vs-level2
ghcr.io/firms-gta/dashboard-cwp:level0-vs-level1
ghcr.io/firms-gta/dashboard-cwp:2024-vs-2026
ghcr.io/firms-gta/dashboard-cwp:previous-vs-updated
```

If reproducibility requires an immutable version, an additional commit or version tag can also be pushed:

```text
ghcr.io/firms-gta/dashboard-cwp:level1-vs-level2
ghcr.io/firms-gta/dashboard-cwp:level1-vs-level2-<git-sha>
```

---

# Publishing to GHCR

## 11. Login to GHCR

If required:

```bash
echo "$CR_PAT" | \
  docker login ghcr.io \
  -u YOUR_GITHUB_USERNAME \
  --password-stdin
```

The token must have the permissions required to push packages to the target organisation.

---

## 12. Push a comparison image

Example:

```bash
docker push \
  ghcr.io/firms-gta/dashboard-cwp:level1-vs-level2
```

Another user can then run:

```bash
docker pull \
  ghcr.io/firms-gta/dashboard-cwp:level1-vs-level2

docker run --rm \
  -p 3838:3838 \
  ghcr.io/firms-gta/dashboard-cwp:level1-vs-level2
```

---

# Docker Compose workflow

The manual `docker build` command is useful because it makes every build argument explicit.

For repeated comparisons, `docker compose` with an `.env` file is easier.

---

## 13. Recommended comparison service

The comparison build service in `compose.yml` can be configured as follows:

```yaml
services:

  dashboard-local-comparison:
    image: ${COMPARISON_IMAGE:-dashboard-cwp:local}

    build:
      context: .
      dockerfile: Dockerfile.with-gta-data
      args:
        APP_IMAGE: ${APP_IMAGE:-ghcr.io/firms-gta/dashboard-cwp-app:main}
        DATA_IMAGE: ${DATA_IMAGE:-ghcr.io/firms-gta/tunaatlas-data:main}
        DATASET_1_FILE: ${DATASET_1_FILE}
        DATASET_2_FILE: ${DATASET_2_FILE}
        DATASET_1_TITLE: ${DATASET_1_TITLE:-Dataset 1}
        DATASET_2_TITLE: ${DATASET_2_TITLE:-Dataset 2}

    profiles:
      - build-comparison

    ports:
      - "${DASHBOARD_PORT:-3838}:3838"

    environment:
      SHINY_PRELOAD_DATA: "TRUE"
      DASHBOARD_PROFILE: generic
      DASHBOARD_MODE: comparison
```

This lets each comparison be described by a small `.env` file rather than a long Docker command.

---

## 14. Create a comparison configuration

A convenient directory structure is:

```text
comparisons/
├── level1-vs-level2.env
├── level0-vs-level1.env
└── previous-vs-updated.env
```

Example:

```text
comparisons/level1-vs-level2.env
```

Contents:

```bash
APP_IMAGE=ghcr.io/firms-gta/dashboard-cwp-app:main
DATA_IMAGE=ghcr.io/firms-gta/tunaatlas-data:main

DATASET_1_FILE=global_catch_ird_level1_1950_2024_harmonized_20834708.qs
DATASET_2_FILE=global_catch_ird_level2_1950_2024_harmonized_20834708.qs

DATASET_1_TITLE=GTA Level 1
DATASET_2_TITLE=GTA Level 2

COMPARISON_IMAGE=ghcr.io/firms-gta/dashboard-cwp:level1-vs-level2

DASHBOARD_PORT=3838
```

---

## 15. Build and run using the `.env` file

From the repository root:

```bash
docker compose \
  --env-file comparisons/level1-vs-level2.env \
  --profile build-comparison \
  up --build
```

The application is then available at:

```text
http://localhost:3838
```

Stop it with:

```bash
docker compose \
  --env-file comparisons/level1-vs-level2.env \
  --profile build-comparison \
  down
```

---

## 16. Build without starting

To only build the image:

```bash
docker compose \
  --env-file comparisons/level1-vs-level2.env \
  --profile build-comparison \
  build
```

The resulting image name is controlled by:

```text
COMPARISON_IMAGE
```

For example:

```text
ghcr.io/firms-gta/dashboard-cwp:level1-vs-level2
```

---

## 17. Push an image built with Compose

After building:

```bash
docker push \
  ghcr.io/firms-gta/dashboard-cwp:level1-vs-level2
```

Or, if the Compose service has an explicit `image:` field:

```bash
docker compose \
  --env-file comparisons/level1-vs-level2.env \
  --profile build-comparison \
  push
```

---

# Dashboard behaviour

## 18. Dataset titles

Dataset titles are passed at image build time:

```text
DATASET_1_TITLE
DATASET_2_TITLE
```

For example:

```bash
--build-arg DATASET_1_TITLE="GTA Level 1"
--build-arg DATASET_2_TITLE="GTA Level 2"
```

These values are stored in the final image and used as the default titles in tables and plots.

This avoids hard-coded names such as `FS` and `GTA` when the Dashboard is reused for another comparison.

---

## 19. Common dimensions

In preloaded comparison mode, the Dashboard determines:

```text
common columns
```

from the intersection of the column names of dataset 1 and dataset 2.

The Dashboard then separates:

- mandatory CWP variables;
- time and geographical variables;
- selectable comparison dimensions;
- filterable columns.

This allows the same Dashboard image to be reused for datasets with different structures.

---

## 20. Selecting dimensions to analyse

The Dashboard proposes the dimensions that are available in both datasets.

Examples may include:

```text
source_authority
species
gear_type
fishing_fleet
fishing_mode
measurement
measurement_type
measurement_processing_level
```

The user can remove dimensions that are not relevant for a specific comparison.

At least one comparison dimension must remain selected.

---

## 21. Dynamic filters

Filters are generated from columns present in both datasets.

This avoids hard-coded filters such as:

```text
species_name
fishing_fleet_label
Ocean
```

which may exist for one dataset family but not another.

Depending on the datasets, filterable dimensions may include:

```text
species
fishing_fleet
gear_type
fishing_mode
source_authority
measurement_unit
year
month
quarter
```

Filters are applied to both datasets before calling the CWP comparison analysis.

---

# Rebuilding strategy

## 22. When do I need to rebuild the application image?

Rebuild:

```text
dashboard-cwp-app
```

when application code changes, for example:

- `app.R`;
- `global.R`;
- files under `R/`;
- package dependencies;
- UI or filtering logic.

Example:

```bash
docker build \
  -f Dockerfile \
  -t ghcr.io/firms-gta/dashboard-cwp-app:main \
  .
```

---

## 23. When do I need to rebuild only the comparison image?

If the application code is unchanged but you want to compare another pair of datasets, there is no need to rebuild the application image.

Only rebuild:

```text
Dockerfile.with-gta-data
```

with different:

```text
DATASET_1_FILE
DATASET_2_FILE
DATASET_1_TITLE
DATASET_2_TITLE
```

This keeps comparison builds relatively simple and reproducible.

---

## 24. When do I need to rebuild the data image?

Rebuild the data image when:

- a required dataset has been added;
- a dataset has been updated;
- the files published in the data image have changed.

Then build the comparison image against the new data image tag.

For reproducible comparisons, prefer an immutable data image tag rather than relying only on `main`.

Example:

```text
ghcr.io/firms-gta/tunaatlas-data:<commit-sha>
```

---

# Troubleshooting

## 25. `manifest unknown`

Example:

```text
Error response from daemon: manifest unknown
```

The requested tag does not exist in the registry.

Check the image/tag and pull it explicitly:

```bash
docker pull IMAGE:TAG
```

Do not assume that a Git commit SHA has automatically been published as a Docker tag.

---

## 26. Dataset file not found during build

Example:

```text
"/source-data/<file>": not found
```

Check:

1. the `DATA_IMAGE` tag;
2. that the file exists in `/data` in that image;
3. the exact value of `DATASET_1_FILE`;
4. the exact value of `DATASET_2_FILE`.

Inspect the data image using the temporary-container method documented above.

---

## 27. `Invalid 'parameter_init'` or `Invalid 'parameter_final'`

Check that both `.qs` files contain data frames.

Example:

```bash
docker run --rm \
  --entrypoint Rscript \
  IMAGE_TO_TEST \
  -e '
d1 <- qs::qread("/data/dataset_1.qs")
d2 <- qs::qread("/data/dataset_2.qs")

print(class(d1))
print(dim(d1))

print(class(d2))
print(dim(d2))
'
```

Both objects must be valid data-frame-like objects.

---

## 28. Check the columns embedded in a comparison image

Run:

```bash
docker run --rm \
  --entrypoint Rscript \
  IMAGE_TO_TEST \
  -e '
library(qs)

d1 <- qread("/data/dataset_1.qs")
d2 <- qread("/data/dataset_2.qs")

cat("\n=== DATASET 1 ===\n")
print(names(d1))

cat("\n=== DATASET 2 ===\n")
print(names(d2))

cat("\n=== COMMON ===\n")
print(intersect(names(d1), names(d2)))

cat("\n=== DATASET 1 ONLY ===\n")
print(setdiff(names(d1), names(d2)))

cat("\n=== DATASET 2 ONLY ===\n")
print(setdiff(names(d2), names(d1)))
'
```

This is useful when deciding which dimensions should be displayed by default.

---

## 29. Old titles still appear in plots

If old dataset titles remain visible:

1. verify `DATASET_1_TITLE` and `DATASET_2_TITLE`;
2. rebuild the comparison image;
3. make sure an old precomputed result is not being reused.

If necessary, remove a local precomputed result before rebuilding:

```bash
rm -f data/PRELOADED_RESULT.qs
```

Then rebuild the application/comparison image.

---

## 30. Port already in use

Example:

```text
Bind for 0.0.0.0:3838 failed: port is already allocated
```

Use another local port:

```bash
docker run --rm \
  -p 3839:3838 \
  IMAGE_TO_RUN
```

or set:

```bash
DASHBOARD_PORT=3839
```

in the comparison `.env` file.

---

# Recommended workflow for a new comparison

For a new comparison, the shortest workflow is:

### 1. Identify the two files

```text
dataset_A.qs
dataset_B.qs
```

### 2. Create a configuration

```text
comparisons/dataset-a-vs-dataset-b.env
```

Example:

```bash
APP_IMAGE=ghcr.io/firms-gta/dashboard-cwp-app:main
DATA_IMAGE=ghcr.io/firms-gta/tunaatlas-data:main

DATASET_1_FILE=dataset_A.qs
DATASET_2_FILE=dataset_B.qs

DATASET_1_TITLE=Dataset A
DATASET_2_TITLE=Dataset B

COMPARISON_IMAGE=ghcr.io/firms-gta/dashboard-cwp:dataset-a-vs-dataset-b

DASHBOARD_PORT=3838
```

### 3. Build and start

```bash
docker compose \
  --env-file comparisons/dataset-a-vs-dataset-b.env \
  --profile build-comparison \
  up --build
```

### 4. Validate

Open:

```text
http://localhost:3838
```

Check:

- dataset titles;
- row and column counts;
- available dimensions;
- dynamic filters;
- comparison plots.

### 5. Push

```bash
docker push \
  ghcr.io/firms-gta/dashboard-cwp:dataset-a-vs-dataset-b
```

---

# Summary

The reusable comparison workflow is based on three independent layers:

```text
1. dashboard-cwp-app
   Application code and dependencies

2. tunaatlas-data
   Published datasets

3. dashboard-cwp:<comparison-tag>
   Standalone image combining one application version
   with two selected datasets
```

For a new comparison, the Dashboard code normally does not need to change.

In most cases, only these values need to be provided:

```text
DATA_IMAGE
DATASET_1_FILE
DATASET_2_FILE
DATASET_1_TITLE
DATASET_2_TITLE
COMPARISON_IMAGE
```

Using one `.env` file per comparison makes the process reproducible and avoids maintaining a separate Dockerfile for each pair of datasets.
