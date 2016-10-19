#' Load and Clean Stitch Orders
#'
#' Opens and performs basic cleaning on order data exported from Stitch Labs.
#'
#' @param f The path/name of the stitch export to open (must be .csv)
#' @return A cleaned dataframe ready for analysis
#' @export
load_orders <- function(f){
  # load and clean Stitch export
  df <- read.csv(f, stringsAsFactors = F) # load Stitch export

  names(df) <- tolower(names(df))

  df <- select(df, ordernumber, orderdate, sku=lineitemsku, quantity=lineitemquantity, total)

  df <- df %>%
    # set order date to Date class
    mutate(orderdate = as.Date(orderdate, "%Y-%m-%d"),
           # floor week
           week = as.factor(floor_date(orderdate, unit = "week")),
           # week, number of weeks from Jan 1
           week_num = as.factor(week(floor_date(orderdate, unit = "week"))),
           # month as ordered factor with abbreviated labels
           month = as.factor(month(floor_date(orderdate, unit = "month"), label = T)),
           # month as first date of month
           month_date = floor_date(orderdate, unit = "month"),
           # quarter as ordered factor
           quarter = as.factor(floor_date(orderdate, unit="quarter")),
           # year as ordered factor
           year = as.factor(year(floor_date(orderdate, unit = "year")))) %>%
    arrange(desc(ordernumber))

  # all months
  df$year_month = with(df, paste0(year," - ",month))


  # merge product tags
  # load Product information
  products <- read.csv("products.csv", skip = 1, stringsAsFactors = F)
  # drop excess variables
  products_merge <- products %>%
    select(sku, style_num, description, description_linesheet,
           graphic, cut, gender, color, size)
  # merge product tags to orders
  df <- merge(df, products_merge, by = "sku", all.x = T)

  # Remove data from 2014 and prior
  df <- df %>% filter(as.numeric(as.character(year)) > 2014)


  # Mark untracked sales
  # label orders with no sku value as "SKU not tracked"
  df$sku[df$sku == ""] <- "! SKU not tracked"
  # label missing linesheet descriptions as "No linesheet description"
  df$description_linesheet[df$description_linesheet == "0"] <- "! No linesheet description"

  # convert tags to factor and drop unused levels
  df$graphic <- droplevels(factor(df$graphic))
  df$cut <- droplevels(factor(df$cut))
  df$gender <- droplevels(factor(df$gender))
  df$color <- droplevels(factor(df$color))
  df$size <- droplevels(factor(df$size))

  return(df)
}


