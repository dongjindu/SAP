FUNCTION zmmf_if_info.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_INFO_RECORD) LIKE  ZSMM_IF008 STRUCTURE  ZSMM_IF008
*"     VALUE(I_CHECK) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      E_RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
*-- i_check C: --> Create
*           R: --> Modify
*           D: --> Delete
  DATA: l_info_record LIKE zsmm_if008.

  CLEAR l_info_record.
  CLEAR: e_return, e_return[].
*--Modification : 11/05/2006
*-Convert net price to control decimal length.
DATA : l_length  LIKE dfies-decimals,
       l_cur_len TYPE i,
       l_square  TYPE i,
       l_decimal TYPE p DECIMALS 5,
       l_currdec LIKE tcurx-currkey.

DATA : it_tcurx TYPE TABLE OF tcurx WITH HEADER LINE.
*--ABAP Memory : called by re-proceesing program
DATA : L_FLAG.
 IMPORT L_FLAG FROM MEMORY ID 'FLAG'.
IF L_FLAG NE 'X'.
CALL FUNCTION 'SWA_DETERMINE_DECIMALS'
  EXPORTING
    expression       = i_info_record-znetpr
  IMPORTING
    decimals         = l_length.

  CLEAR it_tcurx. REFRESH it_tcurx.
  SELECT * INTO TABLE it_tcurx
                        FROM tcurx.

  READ TABLE it_tcurx WITH KEY currkey =  i_info_record-waers.
  IF sy-subrc = 0.
      IF it_tcurx-currdec = 0.
         l_cur_len = 2.
      ELSE.
         l_cur_len = l_currdec.
      ENDIF.
  ELSEIF sy-subrc NE 0.
         l_cur_len = 2.
  ENDIF.

   l_square  = l_length - l_cur_len.
   g_per     = l_square.

   IF l_square > 0.
    l_square  = 10 ** l_square.
    l_decimal = i_info_record-znetpr * l_square.
   ELSE.
    l_decimal = i_info_record-znetpr.
   ENDIF.

     CALL FUNCTION 'ROUND'
       EXPORTING
        decimals            = 2
        input               = l_decimal
        sign                = '+'
       IMPORTING
        output              = l_decimal .

    i_info_record-netpr = l_decimal.
ENDIF.
*--End Modification : 11/05/2006
*-----------------------------------------

  MOVE: i_info_record TO l_info_record.
*---//
  SELECT SINGLE * FROM eina WHERE lifnr = l_info_record-lifnr
                            AND   matnr = l_info_record-matnr.
*-- Create
  IF sy-subrc NE 0.
    CALL FUNCTION 'ZMMF_IF_CREATE_INFO_RECORD'
         EXPORTING
              i_info_record = l_info_record
         TABLES
              e_return      = e_return.
*-- Delete
  ELSE.
    IF i_check EQ 'D'.
      CALL FUNCTION 'ZMMF_IF_DELETE_INFO_RECORD'
           EXPORTING
                i_info_record = l_info_record
           TABLES
                e_return      = e_return.
*-- Change
    ELSE.
      CALL FUNCTION 'ZMMF_IF_CHANGE_INFO_RECORD'
           EXPORTING
                i_info_record = l_info_record
           TABLES
                e_return      = e_return.
    ENDIF.
  ENDIF.
ENDFUNCTION.
