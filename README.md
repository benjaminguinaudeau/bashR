
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bashr <img src="man/figures/bashr_logo.jpeg" width="160px" align="right" />

    #> ✔ Setting active project to '/Users/benjaminguinaudeau/Library/CloudStorage/
    #> GoogleDrive-ben.gui.spam@gmail.com/.My Drive/Konstanz/SideProjects/package/
    #> bashR'

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/github/languages/code-size/benjaminguinaudeau/bashR.svg)](https://github.com/benjaminguinaudeau/bashR)
[![](https://img.shields.io/github/last-commit/benjaminguinaudeau/bashR.svg)](https://github.com/benjaminguinaudeau/bashR/commits/master)

Some usefull functions to execute some bash code in R

## Sudo

Execute command as sudo

``` r
set_sudo("secret_sudo_password")
sudo("ls")
```

## Parse Curl request

Parse a request, that was copied in chrome from DevTools \> Network

``` r
input <- r"(curl 'https://github.com/benjaminguinaudeau/' \
  -H 'authority: github.com' \
  -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' \
  -H 'accept-language: en-GB,en;q=0.9' \
  -H 'sec-ch-ua: "Google Chrome";v="107", "Chromium";v="107", "Not=A?Brand";v="24"' \
  -H 'sec-ch-ua-mobile: ?0' \
  -H 'sec-ch-ua-platform: "macOS"' \
  -H 'sec-fetch-dest: document' \
  -H 'sec-fetch-mode: navigate' \
  -H 'sec-fetch-site: none' \
  -H 'sec-fetch-user: ?1' \
  -H 'upgrade-insecure-requests: 1' \
  -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/107.0.0.0 Safari/537.36' \
  --compressed
  )"
```

``` r
parsed_request <- parse_curl_request(req_string = input, execute = T) %>% 
  dplyr::glimpse()
#> Rows: 1
#> Columns: 4
#> $ url     <chr> "https://github.com/benjaminguinaudeau/"
#> $ headers <list> <"github.com", "text\\/html,application\\/xhtml+xml,applicatio…
#> $ r_code  <glue> "httr::GET(url = \"https://github.com/benjaminguinaudeau/\", …
#> $ req     <list> [https://github.com/benjaminguinaudeau/, 200, GitHub.com, Thu…
```

``` r
parsed_request$headers
#> [[1]]
#>                                                                                                                                                 authority 
#>                                                                                                                                              "github.com" 
#>                                                                                                                                                    accept 
#> "text\\/html,application\\/xhtml+xml,application\\/xml;q=0.9,image\\/avif,image\\/webp,image\\/apng,*\\/*;q=0.8,application\\/signed-exchange;v=b3;q=0.9" 
#>                                                                                                                                           accept-language 
#>                                                                                                                                          "en-GB,en;q=0.9" 
#>                                                                                                                                                 sec-ch-ua 
#>                                                                           "\"Google Chrome\";v=\"107\", \"Chromium\";v=\"107\", \"Not=A?Brand\";v=\"24\"" 
#>                                                                                                                                          sec-ch-ua-mobile 
#>                                                                                                                                                      "?0" 
#>                                                                                                                                        sec-ch-ua-platform 
#>                                                                                                                                               "\"macOS\"" 
#>                                                                                                                                            sec-fetch-dest 
#>                                                                                                                                                "document" 
#>                                                                                                                                            sec-fetch-mode 
#>                                                                                                                                                "navigate" 
#>                                                                                                                                            sec-fetch-site 
#>                                                                                                                                                    "none" 
#>                                                                                                                                            sec-fetch-user 
#>                                                                                                                                                      "?1" 
#>                                                                                                                                 upgrade-insecure-requests 
#>                                                                                                                                                       "1" 
#>                                                                                                                                                user-agent 
#>                           "Mozilla\\/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit\\/537.36 (KHTML, like Gecko) Chrome\\/107.0.0.0 Safari\\/537.36"
```

``` r
parsed_request$r_code
#> httr::GET(url = "https://github.com/benjaminguinaudeau/", 
#> httr::add_headers(.headers = c(c(authority = "github.com", accept = "text\\/html,application\\/xhtml+xml,application\\/xml;q=0.9,image\\/avif,image\\/webp,image\\/apng,*\\/*;q=0.8,application\\/signed-exchange;v=b3;q=0.9", 
#> `accept-language` = "en-GB,en;q=0.9", `sec-ch-ua` = "\"Google Chrome\";v=\"107\", \"Chromium\";v=\"107\", \"Not=A?Brand\";v=\"24\"", 
#> `sec-ch-ua-mobile` = "?0", `sec-ch-ua-platform` = "\"macOS\"", 
#> `sec-fetch-dest` = "document", `sec-fetch-mode` = "navigate", 
#> `sec-fetch-site` = "none", `sec-fetch-user` = "?1", `upgrade-insecure-requests` = "1", 
#> `user-agent` = "Mozilla\\/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit\\/537.36 (KHTML, like Gecko) Chrome\\/107.0.0.0 Safari\\/537.36"
#> )
#> )))
```

``` r
parsed_request$req
#> [[1]]
#> Response [https://github.com/benjaminguinaudeau/]
#>   Date: 2022-11-24 10:20
#>   Status: 200
#>   Content-Type: text/html; charset=utf-8
#>   Size: 197 kB
#> 
#> 
#> <!DOCTYPE html>
#> <html lang="en" data-color-mode="auto" data-light-theme="light" data-dark-the...
#>   <head>
#>     <meta charset="utf-8">
#>   <link rel="dns-prefetch" href="https://github.githubassets.com">
#>   <link rel="dns-prefetch" href="https://avatars.githubusercontent.com">
#>   <link rel="dns-prefetch" href="https://github-cloud.s3.amazonaws.com">
#>   <link rel="dns-prefetch" href="https://user-images.githubusercontent.com/">
#> ...
```

The code below test each header: given a testing function applied to the
return request, it tests whether a header is required for the request.

``` r
test_headers(url = parsed_request$url, headers = parsed_request$headers[[1]], test_fun = status_code_is, code = 200) %>%
  dplyr::glimpse()
#> Rows: 12
#> Columns: 2
#> $ removed_header <chr> "authority", "accept", "accept-language", "sec-ch-ua", …
#> $ required       <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,…
```

## Thanks

A huge thank you to [Favstats](https://github.com/favstats) for
designing the hex-sticker.
