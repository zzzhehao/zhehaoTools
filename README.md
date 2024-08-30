# ZhehaoTools

This is my personal tool kit where I store some handy functions that I wrote in my own works.

## Installing

Run following command to install the package from this repo.

```r
devtools::install_github("https://github.com/zzzhehao/zhehaoTools")
```

## Functions

The following functions are now available to use, with detailed documentation in the package:

### GIS related

- `dms_to_decimal`: transform lon/lat coordinates from DMS format to decimal format.

### Notion

These functions require your personal Notion token. For instructions on how to create one, see the [official tutorial](https://www.notion.so/help/create-integrations-with-the-notion-api).

- `getPage`: get a Notion page and its meta.
- `getNotionDatabase`: from notionR (WIP), get a Notion database contents.