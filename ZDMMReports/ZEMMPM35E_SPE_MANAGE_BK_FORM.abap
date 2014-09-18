************************************************************************
*
** Program Name      : ZEMMPM35E_SPE_MANAGE
** Created by        : Min-su Park
** Created on        : 2003.10.21.
** Pattern           :
** Description       :  Manage Error Standard Price for Purchase
*Material
**
** Modification Logs
** Date            Developer        RequestNo      Description
** 2003.10.22.     Min-su Park    UD1K901873     Initial Coding
************************************************************************
*
*
**----------------------------------------------------------------------
*
****INCLUDE ZMINSUFORM .
**----------------------------------------------------------------------
*
**&---------------------------------------------------------------------
*
**&      Form  PAGE_CONTROL
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM PAGE_CONTROL.
*  CALL FUNCTION 'SCROLLING_IN_TABLE'
*       EXPORTING
*            entry_act             = TC_SPE-TOP_LINE
*            entry_to              = TC_SPE-LINES
*            last_page_full        = ' '
*            loops                 = W_LOOPC
*            ok_code               = W_FCODE
*            overlapping           = 'X'
*       IMPORTING
*            entry_new             = TC_SPE-TOP_LINE
*       EXCEPTIONS
*            no_entry_or_page_act  = 1
*            no_entry_to           = 2
*            no_ok_code_or_page_go = 3
*            OTHERS                = 4.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*ENDFORM.                    " PAGE_CONTROL
**&---------------------------------------------------------------------
*
**&      Form  SELECT_ALL
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM SELECT_ALL.
*  IT_ZTMM_SPE-MARK = 'X'.
*  MODIFY IT_ZTMM_SPE TRANSPORTING MARK WHERE MARK <> 'X'
*                                         AND NETPR <> 0.
*ENDFORM.                    " SELECT_ALL
**&---------------------------------------------------------------------
*
**&      Form  DESELECT_ALL
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM DESELECT_ALL.
*  CLEAR IT_ZTMM_SPE-MARK.
*  MODIFY IT_ZTMM_SPE TRANSPORTING MARK WHERE MARK = 'X'.
*ENDFORM.                    " DESELECT_ALL
**&---------------------------------------------------------------------
*
**&      Form  SAVE
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM SAVE.
*  LOOP AT IT_ZTMM_SPE. " WHERE MARK = 'X'.
**    IF IT_ZTMM_SPE-NETPR IS INITIAL.
**      MESSAGE E016.
**    ENDIF.
**    PERFORM UPDATE_PRICE.
*     UPDATE ZTMM_SPE FROM IT_ZTMM_SPE.
*  ENDLOOP.
*ENDFORM.                    " SAVE
**&---------------------------------------------------------------------
*
**&      Form  UPDATE_PRICE
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM UPDATE_PRICE.
*  DATA: WA_HEAD       LIKE BAPIMATHEAD, "Header with control
*information
*        WA_PLANT      LIKE BAPI_MARC  , "plant-specific material DATA
*        WA_PLANTX     LIKE BAPI_MARCX ,
*        WA_MBEW       LIKE BAPI_MBEW  ,
*        WA_MBEWX      LIKE BAPI_MBEWX .
*  DATA: IT_BAPIRET2   LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.
*  IF IT_ZTMM_SPE-WERKS = SPACE.
*    SELECT SINGLE WERKS
*             INTO IT_ZTMM_SPE-WERKS
*             FROM MARC
*            WHERE MATNR = IT_ZTMM_SPE-MATNR.
*  ENDIF.
*
*  CASE IT_ZTMM_SPE-PTYPE.
*    WHEN 'B'.
*      WA_HEAD-MATERIAL     = IT_ZTMM_SPE-MATNR.
*      WA_MBEW-VAL_AREA     = IT_ZTMM_SPE-WERKS.
*      WA_MBEW-PLNDPRICE3   = IT_ZTMM_SPE-NETPR.
*      WA_MBEW-PLNDPRDATE3  = SY-DATUM.
*      WA_MBEWX-VAL_AREA    = IT_ZTMM_SPE-WERKS.
*      WA_MBEWX-PLNDPRICE3  = 'X'              .
*      WA_MBEWX-PLNDPRDATE3 = 'X'              .
*    WHEN 'P'.
*      WA_HEAD-MATERIAL     = IT_ZTMM_SPE-MATNR.
*      WA_MBEW-VAL_AREA     = IT_ZTMM_SPE-WERKS.
*      WA_MBEW-PLNDPRICE1   = IT_ZTMM_SPE-NETPR.
*      WA_MBEW-PLNDPRDATE1  = SY-DATUM.
*      WA_MBEWX-VAL_AREA    = IT_ZTMM_SPE-WERKS.
*      WA_MBEWX-PLNDPRICE1  = 'X'              .
*      WA_MBEWX-PLNDPRDATE1 = 'X'              .
*  ENDCASE.
*
*  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
*       EXPORTING
*            HEADDATA       = WA_HEAD
*            VALUATIONDATA  = WA_MBEW
*            VALUATIONDATAX = WA_MBEWX
*       TABLES
*            RETURNMESSAGES = IT_BAPIRET2.
*
*  READ TABLE IT_BAPIRET2 WITH KEY TYPE = 'E'.
*  IF SY-SUBRC = 0.  "Error Occurred !
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*  ELSE.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
**     EXPORTING
**       WAIT          =
**     IMPORTING
**       RETURN        =    .
*    DELETE IT_ZTMM_SPE.
*    DELETE ZTMM_SPE FROM IT_ZTMM_SPE.
*  ENDIF.
*ENDFORM.                    " UPDATE_PRICE
**&---------------------------------------------------------------------
*
**&      Form  BDC_PASS
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**      -->P_0010   text
**      -->P_0011   text
**      -->P_0012   text
**----------------------------------------------------------------------
*
*FORM BDC_PASS USING PAR1 PAR2 PAR3.
*  CLEAR IT_BDC.
*  IF PAR1 = 'X'.
*    IT_BDC-DYNBEGIN = 'X'.
*    IT_BDC-PROGRAM  = PAR2.
*    IT_BDC-DYNPRO   = PAR3.
*    APPEND IT_BDC.
*  ELSE.
*    IT_BDC-FNAM = PAR2.
*    IT_BDC-FVAL = PAR3.
*    APPEND IT_BDC.
*  ENDIF.
*ENDFORM.                    " BDC_PASS
**&---------------------------------------------------------------------
*
**&      Form  FIND_STRING
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM FIND_STRING.
***---
*  IF W_FCODE EQ 'FIND'.
*    MOVE : 1 TO W_POSITION.
*  ELSEIF W_FCODE EQ 'FIND+'.
*    W_POSITION = W_LOOP_FIRST + 1.
*  ENDIF.
***---
*  IF W_FCODE EQ 'FIND'.
*    PERFORM popup_get_value(sapfsfxx) USING    'FSTR' ' '
*                                      CHANGING rsdxx-findstr.
*  ENDIF.
***---
*  IF sy-ucomm NE 'CANC'.
**---
*    CLEAR : W_FOUND.
*    IF sy-dynnr EQ '0100'.
*      LOOP AT IT_ZTMM_SPE FROM W_POSITION.
*        IF IT_ZTMM_SPE CS rsdxx-findstr OR IT_ZTMM_SPE CP rsdxx-findstr
.
*          MOVE : 'X' TO W_FOUND.
*          MOVE : sy-tabix TO W_FIND_POS.
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*      IF W_FOUND NE space.
*        MOVE : 1          TO TC_SPE-CURRENT_LINE,
*               W_FIND_POS TO TC_SPE-TOP_LINE    ,
*               W_FIND_POS TO W_LOOP_FIRST       .
*      ELSE.
*        MESSAGE s042(e2) WITH rsdxx-findstr.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*ENDFORM.                    " FIND_STRING
