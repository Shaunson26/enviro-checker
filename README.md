# gnaf-shiny-finder

Attempt to work out best strategy for dynamic address filtering with shiny and
a data.frame with 2.5 M rows. There is a short lag (~ 1-2 seconds) for filtering
and updating select boxes

There are various console logs when something is done.

## Solution

The select boxes return a character value such that this was slow

```
filter(a_numeric_column == input$value) # input$value as character
```

`as.numeric()/as.interger()` solved the lag issue ...

```
filter(a_numeric_column == as.numeric(input$value)) 
```
