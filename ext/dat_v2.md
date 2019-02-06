# takiwaR Data types and structures
2016-02-21

## General Data Types
- numeric
	- decimal
	- integer
- logicals
- text
- missing
- mixed

## General Data Structures
- list [key / value pairs]
- data.frame [2D, multiple data types]
- matrix [2D, one data type]

`?as.matix`: "... the usual coercion hierarchy (logical < integer < double < complex) will be used."  


## Input Data Directions (str_in)
- `row` [1D mixed data. First column is the keys ("row names"). Subsequent column(s) are values. Rows may have different datatypes. Column headers ignored]
- `col` [2D mixed data. First row is the keys (column names). Subsequent rows(s) are values. Columns may have different datatypes.]
- `both` [2D matrix data. Quadrats in rows, species / observations in columns. All one datatype.]

## Implicit zeros
Some data have implicit zeros. In previous versions of takiwaR, this was dealt with directly. Now this is handled generically with a `fill` option. If `fill` is not supplied, `NA` is assumed (the R default)

## General takiwaR Data Storage Objects (str_out)
- `takRrow` (takiwaR key value)
	- Data type: Mixed (input as text, coerced to mixed by `type.convert`)
	- Input data direction: `row`
	- Valid fill types: `NULL` (a ragged list)
	- R data type: `list`
- `takRcol`(takiwaR mixed data)
	- Data type: Mixed (input as text, coerced to mixed by `type.convert`)
	- Input data direction: `col`
	- Valid fill types: `NA` (default data.frame)
	- R data type: `data.frame`
- `takRwide` (takiwaR wide matrix)
	- Data type: Any single datatype
	- Input data direction: `mrow` OR `mcol`
	- Valid fill types: `NA` or any of the nominated datatype
	- R data type: `matrix`
	- NB: Data of this class **always** has quadrats in rows (i.e. the `mrow` direction). Data input as `mcol` are transposed in this class.
- `takRempty`
	- Data type: `missing`
	- NB: A single NA value. 

##  Specialist takiwaR Data (and Metadata) Storage Objects (str_out)
- `takRmeta` (takiwaR metadata)
	- Inherits from `takRrow`
- `takRperc` (takiwaR percent)
	- Inherits from: `takRwide`
	- Data type: `numeric`
	- Data direction: `mcol`
	- Fill: `0`
- `takRcount` (takiwaR count)
	- Inherits from: `takRwide`
	- Data type: `integer`
	- Input data direction: `mcol`
	- Fill: `0`
- `takRpa` (takiwaR presence-absence) **TODO** 
	- Inherits from: `takRwide`
	- Data type: `logical`
	- Input data direction: `mcol`
	- Fill: `FALSE`
- `takRsf` (takiwaR size-frequency)
	- Inherits from: `takRwide`
	- Data type: `numeric`
	- Input data direction: `mrow`
	- Fill: `NA`

## Collections
Collections are a concept that allow for grouping of data and metadata.

Collections may be any sensible grouping, for example, a transect.  Conceptually they are similar to a transect (`takRbt`) in takiwaR 1.0.  

- `takRcollection` (takiwaR collection)
	- **Must** contain a metadata block.  
	- **May** contain one or more data blocks.  
	- **May** contain one or more collection.  
	- Has `print`, `validate` methods. 

## Validation subsystem
- `takRvalidator`: noun (something that validates). 
	- An overloaded predicate function that produces a single predicate.
- `takRvalidators`: plural noun (things that validate). 
	- One or more validators
- `takRpredicate`: noun (the outcome of a logical argument, a declaration). 
	- The outcome of a validator
- `takRpredicates`: plural noun (multiple outcomes or declarations). 
	- One or more predicates.

## takRdef (takiwaR definition objects)
- `takRdef` A definition object
	- A named list. 
	- This is a "definition" object. Used for guiding other processes, generally. 
	- Currently this can only contain objects of class `takRblock`, which is used to guide reading a file with sections / blocks.  

### `takRblock` 
- `name` [The internal name of the block/section]
	- `text` [The search text for the block/section]
	- `str_in` [One of the takiwaR input directions]
		- `row` | `col` | `mrow` | `mcol`
	- `str_out` [One of the takR* data objects / classes]
		- `takRrow` | `takRcol` | `takRwide` | `takRempty`
		- `takRmeta` |  `takRperc` | `takRcount` | `takRsf` | `takRpa`
	- `fill` [Object to fill blank cells]. 
		- `NA`| `NULL` | Any single number (commonly 0)
	- `n` [Number of experimental units / quadrats / rows / cols]. **Required** for matrix data (`str_in` = `mrow` or `mcol`).  

These sections are not essential:
- `required` [NA | Character]. The names of the required fields. Required for metadata section.
	- required = c("site","date","depth","n_quad","quad_size","gps_lat","gps_long")
- `unit` [Optional | Character]