# scorecard 0.4.3.999

* the parameter missing_join supports to NULL in woebin function

# scorecard 0.4.3

* fixed multiple bugs 

# scorecard 0.4.2

* added a new param in the woebin function to specify the bin position of missing values
* fixed a bug in scorecard_ply function

# scorecard 0.4.1

* added correlation matrix to the report function.
* added a new function scorecard_pmml.
* adjusted var_filter and var_filter2 function.

# scorecard 0.4.0

* remove the |> operator to support R < 4.1. 
* modified var_filter and scorecard2 function. 
* update examples for the scorecard and perf functions.
 
# scorecard 0.3.9

* fixed a bug in woebin function.
* the var_filter function supports to filter variable via step and vif.

# scorecard 0.3.8

* fixed a bug in woebin function when breaks_list is provided.

# scorecard 0.3.7

* modified the example codes in the scorecard, scorecard2, scorecard_ply functions
* fixed multiple bugs 

# scorecard 0.3.6

* fixed multiple bugs 
* added test files

# scorecard 0.3.5

* set the default number of cores to 2 in woebin and woebin_ply function.

# scorecard 0.3.4

* fixed multiple bugs in functions, such as woebin_adj and describe.
* the functions of woebin_adj and report support the arguments that used in woebin and woebin_plot.


# scorecard 0.3.3

* the perf_psi function modified the results from psi to csi for variables.
* add the global argument `options(scorecard.bin_close_right  = TRUE)` to set the bins closed on the right.
* fixed a bug in describe function.

# scorecard 0.3.2

* added a function `describe` that calculatess statistic parameters for exploratory data analysis.
* replace good/bad with neg/pos.
* replace NaN with NA if it exists in the input data.
* added [] after the returned DataTable in gains_table and woebin function in order to directly display the datatable when printing.

# scorecard 0.3.1

* added a parameter in scorecard/scorecard2 function to round score
* added parameters show_varval/show_lineval in woebin_plot function to show bar value or line value
* added a parameter breaks_by in perf_psi/gains_table function to identify the data set to create breakpoints.
* modified the graphics format of perf_eva function

# scorecard 0.3.0

* modified the method to create initial fine binning breaks.
* fixed the bintxt to vector function
* report function supports no_cores argument 

# scorecard 0.2.9

* fixed a bug in perf_psi when label is null
* modified a parameter in split_df function
* adding test files
* added a warning message in scorecard2 function when there are na coefficients in lr model
* improve the performance of rep_blank_na function
* modified the method to create initial fine binning breaks.

# scorecard 0.2.8.1

* fixed a bug in woebin function cant handle special_values
* woebin_adj supports to return bins list.
* fixed a bug in report function

# scorecard 0.2.8

* fixed a bug in woebin using chimerge method
* gains_table supports setting of break width
* fixed a bug in germancredit data set
* scorecard2 supports to adjust oversampling 
* provides replace_na function to replace missing values
* a new function var_scale is provided for variable scaling 

# scorecard 0.2.7

* check var_kp in scorecard_ply function
* fixed a bug caused by data.table updating

# scorecard 0.2.6

* split_df function supports multiple data frames.
* added a cross validation function perf_cv
* modified the default value for no_cores in woebin and woebin_ply function.
* woebin_plot supports displaying woe values
* fixed a bug when Inf or NaN exist in input data for woebin and woebin_ply function

# scorecard 0.2.5

* fixed various bugs in woebin_adj that returns breaks_list including missing without quotes.
* fixed a bug in perf_eva when pred is score.
* fixed a bug in woebin function when y is not provided.
* remove the .export option in foreach loop, in order to suppress the warning of 'already exported variables ...'
* modified the calculation of identical rate in var_filter function
* using forking on non-windows os and psock on windows when makeCluster in parallel 

# scorecard 0.2.4

* fixed a bug in woebin when only NA and special values
* remove 'missing' value from breaks_list if it exists.
* woebin function donot require y if label column is not available for equal freq/width method. 
* woebin_ply supports converting data into bin value.
* fixed a bug in perf_eva when pred is score
* move the gains table to the last sheet in exported report excel, and only have one data set.
* fixed a bug in woebin_ply when only x variables in bins are available in input dataset
* fixed a bug in report function export test binning graphics only.
* fixed a bug in woebin function when there are only two unique values for tree and chimerge methods.

# scorecard 0.2.3

* add var_skip argument in woebin function, and var_kp argument in scorecard_ply function. Therefore, the id column can be handle during the development of scorecard model.
* fixed a typo in perf_eva function
* replace !isFalse(x) with isTRUE(x) & !is.null(x) in perf_eva function. The isFalse function is only available after R3.5. 

# scorecard 0.2.2

* fixed a bug in check_y function when the name of label column is 'y' in input data.
* fixed a bug in woebin_adj function when count_distr_limit is not default value in woebin function.

# scorecard 0.2.1

* revised one_hot function
* modified .export used in foreach loop
* add my name in license file

# scorecard 0.2.0

* fixed a bug is woebin function cant modify positive values
* pdo in scorecard function now supports negative value.
* split_df will not remove datetime and identical variables
* added a one-hot encoding function 
* added save_breaks_list argument in both woebin and woebin_adj function, which can save breaks_list as file in current working directory.
* revised perf_eva and perf_psi functions
* added a vif function
* added a report function to create report for scorecard modeling
* added a scorecard2 function, which donot requires a glm model object in inputs

# scorecard 0.1.9

* pdo in scorecard function now supports negative value. If pdo is positive, the larger score means the lower probability to be positive sample. If pdo is negative, the larger score means the higher probability to be positive sample.
* fixed a bug in woebin function using chimerge method, which is caused by initial breaks have out-range values.
* added a check function on the length of unique values in string columns, which might cause the binning process slow.
* fixed a bug in perf_eva function which is caused by the nrow of plot is set to 0 when the length of plot type is one.
* the ratio argument in split_df function supports to set ratios for both train and test.
* If the argument return_rm_reason is TRUE in var_filter function, the info_value, missing_rate and identical_rate are provided in the result.


# scorecard 0.1.8

* remove columns have only one unique values in input dataset
* modify the default values of x_limits in perf_psi
* fixed a bug in perf_psi when the label is factor
* display proc time in woebin
* fixed a bug in per_eva when estimating AUC
* fixed a bug in woebin_adj when special_values is provided

# scorecard 0.1.7

* added chimerge method for woebin function
* special_values option added in woebin function
* f1 curve added in perf_eva

# scorecard 0.1.6

* Fixed a bug in woebin_adj function when all_var == FALSE and the breaks of all variables are perfect. 
* Provide parallel computation (foreach with parallel backend) in the functions of woebin and woebin_ply.
* Modified scorecard_ply function.
* Fixed a bug in woebin when there are empty bins based on provided break points. 

# scorecard 0.1.5

* Fixed a bug in scorecard function when calculating the coefficients.
* Fixed a bug in perf_eva when type="lift". 
* Fixed a bug in functions of woebin and var_filter when removing Date columns. 

# scorecard 0.1.4

* perf_eva supports both predicted probability and score.
* Added the woebin_adj function which can interactively adjust the binning info from woebin.
* Reviewed woebin function.

# scorecard 0.1.3

* Modified the format of printing message and added condition functions.
* Added the split_df function which split a dataframe into two.
* Reorder the binning information. Move the missing to the first binning.

# scorecard 0.1.2

* fixed a bug in var_filter

# scorecard 0.1.1

* Specified some potential problems via conditions
* Modified examples for most functions

# scorecard 0.1.0

* Initial version



