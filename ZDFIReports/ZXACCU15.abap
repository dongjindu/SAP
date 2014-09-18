*----------------------------------------------------------------------*
*   INCLUDE ZXACCU15                                                   *
*----------------------------------------------------------------------*


READ TABLE extension INDEX 1.
CHECK sy-subrc EQ 0.

CHECK extension-field1 EQ 'WT'.

*t_accwt-wt_key = '00001'.
t_accwt-witht = extension-field2.
t_accwt-wt_withcd = extension-field3.
APPEND t_accwt.
