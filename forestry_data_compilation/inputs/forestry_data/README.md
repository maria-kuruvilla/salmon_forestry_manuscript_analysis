# Forestry Reconstruction Datasets — Coastal BC Salmon Watersheds

**Authors:** Joao Braga (TCC)
**Contact:** joao.braga@trinityconsultants.com
**Last updated:** 2026-05-06

## Overview

This folder contains the forestry reconstruction datasets used to estimate
**Equivalent Clear-cut Area (ECA)** and **Cumulative Disturbed Area (CDA)** for
coastal British Columbia salmon watersheds. All values are derived from the
**2022 Vegetation Resources Inventory (VRI)** and a hydrology layer drawn from
the BC Freshwater Atlas.

Two parallel sets of files are provided:

- **Default**: ECA / CDA estimates that *include* permanent disturbances
  (e.g., roads, urban footprint).
- **`without_perm_dist`**: the same estimates with permanent disturbances
  *excluded*, so harvest signal can be examined in isolation.

Each set contains one *metadata* file (one row per watershed, current snapshot)
and one *timeseries* file (one row per watershed-year, 1883–2022).

## Files

| File | Rows | Content |
|---|---|---|
| `forestry_data_metadata.csv` | 1,745 watersheds | Current (2022) ECA / CDA per watershed, **with** permanent disturbances |
| `forestry_data_timeseries.csv` | ~244,300 (watershed × year) | Annual ECA / CDA reconstruction 1883–2022, **with** permanent disturbances |
| `forestry_data_without_perm_dist_metadata.csv` | 1,745 watersheds | Current (2022) ECA / CDA per watershed, **excluding** permanent disturbances |
| `forestry_data_without_perm_dist_timeseries.csv` | ~244,300 (watershed × year) | Annual ECA / CDA reconstruction 1883–2022, **excluding** permanent disturbances |

Areas are reported in square metres in the BC Albers projection (**EPSG:3005**),
which is reflected in the `_m2_3005` suffix on area columns.

## Schema

### `forestry_data_metadata.csv`

| Column | Type | Description |
|---|---|---|
| `group` | string | Freshwater Atlas watershed group code |
| `this_name` | string | Waterbody name |
| `this_lfid` | integer | Freshwater Atlas linear feature ID |
| `Area_m2_3005` | numeric | Total watershed area (m², EPSG:3005) |
| `ECA_age_proxy_forested_only_current` | numeric (0–1) | Current (2022) ECA, expressed over the forested portion of the watershed |
| `haarea_prct_cs_current` | numeric (0–100) | Current (2022) cumulative harvested area as a percent of forested area |
| `Forested_area_m2` | numeric | Forested area within the watershed, derived from the VRI Forest Management Land Base attribute (m², EPSG:3005) |
| `portion_reporting_vri_cover1_missing` | numeric (0–1) | Proportion of the watershed lacking VRI cover information |

### `forestry_data_timeseries.csv`

| Column | Type | Description |
|---|---|---|
| `group` | string | Freshwater Atlas watershed group code |
| `year` | integer | Year (1883–2022) |
| `LINEAR_FEATURE_ID` | integer | Freshwater Atlas linear feature ID (joins to `this_lfid` in the metadata file) |
| `ECA_age_proxy` | numeric | ECA estimate for the year, expressed over the **entire watershed** |
| `ECA_age_proxy_forested_only` | numeric | ECA estimate for the year, expressed over the **forested portion only** |
| `hararea_m2_3005` | numeric | Harvested forest area in that year (m², EPSG:3005) |
| `haarea_m2_cs` | numeric | Cumulative harvested forest area through that year (m²) |
| `haarea_prct_cs` | numeric | Cumulative harvested area through that year as a proportion of forested area |
| `permanent_dist_m2` | numeric | Area of permanent disturbances detected in the watershed (m²) |

### `forestry_data_without_perm_dist_metadata.csv`

Same schema as `forestry_data_metadata.csv`.

| Column | Type | Description |
|---|---|---|
| `group` | string | Freshwater Atlas watershed group code |
| `this_name` | string | Waterbody name |
| `this_lfid` | integer | Freshwater Atlas linear feature ID |
| `Area_m2_3005` | numeric | Total watershed area (m², EPSG:3005) |
| `ECA_age_proxy_forested_only_current` | numeric (0–1) | Current (2022) ECA, **excluding permanent disturbances** |
| `haarea_prct_cs_current` | numeric (0–100) | Current (2022) cumulative harvested area as a percent of forested area, **excluding permanent disturbances** |
| `portion_reporting_vri_cover1_missing` | numeric (0–1) | Proportion of the watershed lacking VRI cover information |

### `forestry_data_without_perm_dist_timeseries.csv`

Same schema as `forestry_data_timeseries.csv` **except** that
`permament_dist_m2` is **not** included.

| Column | Type | Description |
|---|---|---|
| `group` | string | Freshwater Atlas watershed group code |
| `year` | integer | Year (1883–2022) |
| `LINEAR_FEATURE_ID` | integer | Freshwater Atlas linear feature ID |
| `ECA_age_proxy` | numeric | ECA estimate for the year, expressed over the entire watershed (no permanent disturbances) |
| `ECA_age_proxy_forested_only` | numeric | ECA estimate for the year, forested portion only (no permanent disturbances) |
| `hararea_m2_3005` | numeric | Harvested forest area in that year (m², EPSG:3005) |
| `haarea_m2_cs` | numeric | Cumulative harvested forest area through that year (m²) |
| `haarea_prct_cs` | numeric | Cumulative harvested area through that year as a proportion of forested area |

## Joins

Use `this_lfid` (metadata) ↔ `LINEAR_FEATURE_ID` (timeseries) to link a
watershed to its annual reconstruction. `group` is shared across both files
and is the Freshwater Atlas watershed-group code.
