*----------------------------------------------------------------------*
***INCLUDE MZEMMPM45E_MODULEI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE OK_CODE.
    WHEN 'EXEC'.
      CLEAR OK_CODE.
      PERFORM DATE_SELECTION.
    WHEN 'DEL'.
      PERFORM DELETE_TC_LINE.
    WHEN 'SAVE'.
      PERFORM DATA_SAVE_SCREEN_100.
*    WHEN OTHERS.
*      CLEAR OK_CODE.
*      MESSAGE S000(ZMMM) WITH 'Unknow command'.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BACK_EXIT INPUT.
  IF SY-DYNNR = '0100'.

    CALL FUNCTION 'DEQUEUE_EZ_ZTMM_MODULE'
         EXPORTING
              MODE_ZTMM_MODULE = 'X'
              MANDT            = SY-MANDT.
  ENDIF.

  SET SCREEN 0. LEAVE SCREEN.

ENDMODULE.                 " BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  material_convert  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MATERIAL_CONVERT INPUT.
  DATA : WA_MATNR LIKE MARA-MATNR.
  DATA : WA_CHANGE(2) TYPE C VALUE '* ' .
  MOVE IT_100-MATNR TO WA_MATNR .

  TRANSLATE WA_MATNR USING WA_CHANGE.
  CONDENSE WA_MATNR .
  CONCATENATE '***' WA_MATNR(2) '*********' INTO IT_100-MATNR.


ENDMODULE.                 " material_convert  INPUT
*&---------------------------------------------------------------------*
*&      Module  change_item  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHANGE_ITEM INPUT.

  MODIFY IT_100 INDEX TC_100-CURRENT_LINE.

  IF SY-SUBRC NE 0.
    IT_100-LPEINH = '1'.
    IT_100-KLVAR = 'ZMOD'.
    IT_100-TVERS = '1'.
    IT_100-TYPPS = 'V'.
    IT_100-MENGE = '1'.
    IT_100-LPEINH = '1'.
    IT_100-PUNIT = '1'.
    IT_100-MEINS = 'EA'.
    APPEND IT_100.
  ENDIF.


ENDMODULE.                 " change_item  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  CASE OK_CODE .
    WHEN 'COST'.
      CLEAR OK_CODE.
      PERFORM SAVE_AND_POSTING .
    WHEN 'EXCL'.
      CLEAR OK_CODE.
      PERFORM DOWN_LOAD_EXCEL.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_200'
                                            OK_CODE.
      CLEAR OK_CODE.

  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.

  PERFORM LOCK_OBJECT.
  CASE OK_CODE.
    WHEN 'INFO'.
      CLEAR OK_CODE.
      PERFORM GET_CHANGED_MODULE_PRICE.
      CALL SCREEN '0400'.
    WHEN 'CK40N'.
      CLEAR OK_CODE.
      CALL TRANSACTION 'CK40N'.
    WHEN 'ADD'.
      CLEAR OK_CODE.
      CLEAR: IT_100, IT_100[].
      PERFORM INITAL_VALUE_100.
      CALL SCREEN '0100'.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0400 INPUT.
  CASE OK_CODE .
    WHEN 'INFO'.
      CLEAR OK_CODE.
      PERFORM INFOR_UPDATE_BDC.
    WHEN 'LOG'.
      CLEAR OK_CODE.
      PERFORM MESSAGE_LOG.
    when 'EXCL'.
      CLEAR OK_CODE.
      PERFORM EXCEL_DOWNLOAD_400.
    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_400'
                                            OK_CODE.
      CLEAR OK_CODE.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*&      Module  LIST_MODY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE LIST_MODY INPUT.
  MODIFY IT_400 INDEX TC_400-CURRENT_LINE.


  select single * from T024 where EKGRP = it_400-ekgrp.
  if sy-subrc ne 0.
     message e755(me) with it_400-ekgrp.
  endif.
*
ENDMODULE.                 " LIST_MODY  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_input  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT INPUT.

  IF IT_400-EKGRP = '' AND IT_400-FLAG = 'N'.
    MESSAGE E009(ZMMM) WITH 'Input Purchasing group '.
  ENDIF.

ENDMODULE.                 " check_input  INPUT
