# *propose*: a Shiny app for proposing and evaluating targeted intervention scenarios in an emerging epidemic or pandemic using the {ringbp} R package <img src="www/logo.svg" align="right" width="120" />

> [!WARNING]
> Under active development

## {ringbp} R package

The epidemic simulation model used in `{propose}` is from the [`{ringbp}` R package](https://github.com/epiforecasts/ringbp). `{ringbp}` is an open source R package hosted on the [epiforecasts GitHub organisation](https://github.com/epiforecasts) and can be installed from GitHub using:

```r
# check whether {pak} is installed
if (!require("pak")) install.packages("pak")
pak::pak("epiforecasts/ringbp")
```

## Help

To report a bug please open an
[issue](https://github.com/joshwlambert/propose/issues/new).