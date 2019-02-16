# scorecard 0.2.3.999

* fixed a bug in woebin when only NA and special values
* remove 'missing' value from breaks_list if it exists.

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



