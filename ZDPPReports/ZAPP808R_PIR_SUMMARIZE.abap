************************************************************************
* Program Name      : ZAPP808R_PIR_SUMMARIZE
* Author            : BOBBY
* Creation Date     : 2003.01.26.
* Specifications By : B. Choi
* Pattern           : 2.1
* Development Request No : UD1K906372
* Addl Documentation:
* Description       : Daily PIR Summarize
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  zapp808r_pir_summarize    MESSAGE-ID zmpp.

TABLES : ztpp_pmt07jb_c,      " MRP PIR Source Table
         ztpp_pmt07jb_c1.     " MRP PIR Source Table(Back-Up)

*---------------------------------------------------------------------*
*  Internal Table Definition
*---------------------------------------------------------------------*
DATA: IT_DATA         LIKE TABLE OF ZTPP_PMT07JB_C     WITH HEADER LINE,
      IT_SAVE         LIKE TABLE OF ZTPP_PMT07JB_C     WITH HEADER LINE.

*---------------------------------------------------------------------*
*  Gloval Variables Definition
*---------------------------------------------------------------------*
DATA: WA_DATA         LIKE ZTPP_PMT07JB_C ,
      WA_COUNT        TYPE I.

*---------------------------------------------------------------------*
*  SELECTION-SCREEN Definition
*---------------------------------------------------------------------*
SELECTION-SCREEN  BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETERS: p_flg  AS CHECKBOX DEFAULT 'X' .
SELECTION-SCREEN  END OF BLOCK blk1.

*---------------------------------------------------------------------*
INITIALIZATION.
*---------------------------------------------------------------------*
  CLEAR: WA_COUNT.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
START-OF-SELECTION.
*---------------------------------------------------------------------*
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
    FROM ZTPP_PMT07JB_C .

  SORT IT_DATA BY WERKS MATNR PVER PDATU COGUB INEXC.

  IF P_FLG = 'X'.
    PERFORM DATA_SUMMARIZE.
  ELSE.
    PERFORM DATA_RECOVERY .
  ENDIF.

*---------------------------------------------------------------------*
END-OF-SELECTION.
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
TOP-OF-PAGE.
*---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  DATA_SUMMARIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_SUMMARIZE.
  data: L_dealer(2) type c.
  PERFORM SAVE_BACKUP.

  LOOP AT IT_DATA.
    IF WA_DATA-WERKS = IT_DATA-WERKS AND
       WA_DATA-MATNR = IT_DATA-MATNR AND
       WA_DATA-PVER  = IT_DATA-PVER  AND
       WA_DATA-PDATU = IT_DATA-PDATU AND
       WA_DATA-COGUB = IT_DATA-COGUB AND
       WA_DATA-INEXC = IT_DATA-INEXC .
       WA_DATA-PLNMG = WA_DATA-PLNMG + IT_DATA-PLNMG .
    ELSE.
       IF WA_DATA-MATNR IS INITIAL.
         WA_DATA = IT_DATA .
         CONTINUE          .
       ENDIF.
       IT_SAVE = WA_DATA .
** Changed by Furong
       if IT_SAVE-MATNR+13(1) = ' '.
          IT_SAVE-PBDNR = IT_SAVE-MATNR+1(5).
       else.
           CALL FUNCTION 'ZFEB_GET_OLD_DEALER_CODE'
           EXPORTING
                new_DEALER = IT_SAVE-MATNR+4(1)
           IMPORTING
                old_DEALER = L_dealer.
          concatenate IT_SAVE-MATNR+1(3) L_dealer into IT_SAVE-PBDNR.
      endif.
** end of change
       APPEND IT_SAVE    .
       WA_DATA = IT_DATA .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_DATA LINES WA_COUNT.
  IF WA_COUNT > 0 .
    IT_SAVE = WA_DATA .
** Changed by Furong
*    IT_SAVE-PBDNR = IT_SAVE-MATNR+1(5).
       if IT_SAVE-MATNR+13(1) = ' '.
          IT_SAVE-PBDNR = IT_SAVE-MATNR+1(5).
       else.
           CALL FUNCTION 'ZFEB_GET_OLD_DEALER_CODE'
           EXPORTING
                new_DEALER = IT_SAVE-MATNR+4(1)
           IMPORTING
                old_DEALER = L_dealer.
          concatenate IT_SAVE-MATNR+1(3) L_dealer into IT_SAVE-PBDNR.
      endif.
** end of change

    APPEND IT_SAVE    .
  ENDIF.

  DELETE FROM ZTPP_PMT07JB_C CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
  INSERT ZTPP_PMT07JB_C FROM TABLE IT_SAVE .

  IF sy-subrc <> 0.
    ROLLBACK WORK.
    MESSAGE e000 WITH text-002 .
  ELSE.
    COMMIT WORK.
    MESSAGE S001 WITH text-003 .
  ENDIF.
ENDFORM.                    " DATA_SUMMARIZE

*&---------------------------------------------------------------------*
*&      Form  DATA_RECOVERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_RECOVERY.
  DATA: LT_DATA          LIKE TABLE OF ZTPP_PMT07JB_C1 WITH HEADER LINE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_DATA
    FROM ZTPP_PMT07JB_C1  .

  DESCRIBE TABLE LT_DATA   LINES WA_COUNT.

  CHECK WA_COUNT > 0 .
  DELETE FROM ZTPP_PMT07JB_C CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
  INSERT ZTPP_PMT07JB_C  FROM TABLE LT_DATA.
ENDFORM.                    " DATA_RECOVERY

*&---------------------------------------------------------------------*
*&      Form  SAVE_BACKUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_BACKUP.
  SELECT COUNT( * ) INTO WA_COUNT
    FROM ZTPP_PMT07JB_C1  .

  CHECK WA_COUNT = 0 .
  INSERT ZTPP_PMT07JB_C1 FROM TABLE IT_DATA.
ENDFORM.                    " SAVE_BACKUP
