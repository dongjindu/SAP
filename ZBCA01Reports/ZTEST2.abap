REPORT ZDELETE_T558B_T558C_T5U8C.
*----------------------------------------------------------------------
* Program Description: Delete entries for Personnel Numbers from T558B
*                      T558C and T5U8C.
* Developed By       : Mohan Gowda, BearingPoint
* Date               : Feb. 12, 2003
* Description        : During payroll initial data load test and in
*                      production run, data upload had to be perfomed
*                      one period at a time. In order to acheive this
*                      each time the data had to be deleted from tables
*                      T558B, T558C and T5U8C after each pay period data
*                      was loaded. This program is used to delete
*                      all entries by personnel number.
***************WARNING************************************************
***PLEASE MAKE A BACKUP OF ENTRIES IN TABLES T558B, T558C AND T5U8C****
***BEFORE EXECUTING THIS PROGRAM***************************************
*-----------------------------------------------------------------------
*  Date         Transport     Developer       Description
*-----------------------------------------------------------------------
*  02/12/2003   UD1K900352    Mohan Gowda     Initial Development
*
*
*-----------------------------------------------------------------------
* Program to delete entries in table T558B. T558C and T5U8C.

*<---- Table Declaration
Tables: Pa0001.
*<---- Data Declaration
data: text(60).
data: answer(1).
*<---- Selection-Options
select-options: pernr for pa0001-pernr.
*-----------------------------------------------------------------------
Start-of-selection.
*-----------------------------------------------------------------------
move 'Are you sure to delete T558B, T558C and T5U8C entries' to text.
CALL FUNCTION 'POPUP_TO_CONFIRM'
  EXPORTING
    TITLEBAR                    = 'Delete Data Load Tables'
    TEXT_QUESTION               = text
    TEXT_BUTTON_1               = 'YES'
    ICON_BUTTON_1               = 'YES'
    TEXT_BUTTON_2               = 'NO'
    ICON_BUTTON_2               = 'YES'
    DEFAULT_BUTTON              = 'NO'
    DISPLAY_CANCEL_BUTTON       = 'X'
 IMPORTING
   ANSWER                       = answer.

move 'Make backup of old entries in T558B, T558C and T5U8C' to text.

IF Answer = '1'.
 clear: answer.
CALL FUNCTION 'POPUP_TO_CONFIRM'
  EXPORTING
    TITLEBAR                    = 'Are you really sure'
    TEXT_QUESTION               = text
    TEXT_BUTTON_1               = 'YES'
    ICON_BUTTON_1               = 'YES'
    TEXT_BUTTON_2               = 'NO'
    ICON_BUTTON_2               = 'YES'
    DEFAULT_BUTTON              = 'NO'
    DISPLAY_CANCEL_BUTTON       = 'X'
 IMPORTING
   ANSWER                       = answer.
endif.
if answer = '1'.
delete from T5u8C where pernr in pernr.
if sy-subrc = 0.
   write: / 'All entries from table T5U8C successfully deleted'.
endif.
commit work.
*
delete from T558B where pernr in pernr.
if sy-subrc = 0.
   write: / 'All entries from table T558B successfully deleted'.
endif.
commit work.
*
delete from T558C where pernr in pernr.
if sy-subrc = 0.
   write: / 'All entries from table T558C successfully deleted'.
endif.
commit work.

endif.
commit work.
