***********************************************************************
* Form routines to handle SAPSCRIPT in report RFKQSU40
***********************************************************************

*---------------------------------------------------------------------*
*       FORM FORM_AT_LAST                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*

FORM FORM_AT_LAST.
  CALL FUNCTION 'CLOSE_FORM'.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM FORM_AT_LIFNR                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*

FORM FORM_AT_LIFNR.

  CALL FUNCTION 'START_FORM'.

  LFB1-QSREC = KTAB-QSREC.
*Move the correct tax code into the form
  IF TIN_TYPE = 2.
     LFA1-STCD1 = LFA1-STCD1.
  ELSE.
    LFA1-STCD1 = LFA1-STCD2.
  ENDIF.
  SADR-LAND1 = COUNTRY.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM FORM_END_LIFNR                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*

FORM FORM_END_LIFNR.

  CALL FUNCTION 'END_FORM'.
* CALL FUNCTION 'CONTROL_FORM'
*      EXPORTING
*           COMMAND = 'NEW-PAGE'.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM FORM_INIT                                                *
*---------------------------------------------------------------------*
*       Opens the form                                                *
*---------------------------------------------------------------------*
*  -->  PRNTFORM is cleared if user cancels                           *
*---------------------------------------------------------------------*

FORM FORM_INIT USING PRNTFORM.
  PRINT-TDTITLE = TEXT-D01.
  PRINT-TDCOVTITLE = TEXT-D01.
  CALL FUNCTION 'OPEN_FORM'
       EXPORTING
            FORM     = SAPFORMS
            LANGUAGE = 'E'
            OPTIONS  = PRINT
       EXCEPTIONS
            FORM     = 1
            CANCELED = 2
            DEVICE   = 3.
  CASE SY-SUBRC.
    WHEN 1.
      MESSAGE E163 WITH SAPFORMS.
    WHEN 2.
      CLEAR PRNTFORM.
    WHEN 3.
      MESSAGE E600 WITH TEXT-D02.
      LEAVE.
  ENDCASE.
ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM FORM_INIT_AGENT                                          *
*---------------------------------------------------------------------*
*       Initializes all Agent Info used for the 1042 Form             *
*---------------------------------------------------------------------*

FORM FORM_INIT_AGENT.

  T001-LAND1 = COUNTRY.
*NOTE454478 BEGIN
IF T001Z-PAVAL+2(1) NE '-'.
  WRITE T001Z-PAVAL TO SADR-DBNAME USING EDIT MASK '__-_______'.
ELSE.
  SADR-DBNAME = T001Z-PAVAL.
ENDIF.
*NOTE454478 END
  KNA1-NAME1 = PY_NAME.

*Note 0386974 start**************
  if py_tin ne 0.
     write py_tin to kna1-name2 using edit mask '__-_______'.
  endif.
*Note 0386974 end***************

ENDFORM.

*eject
*---------------------------------------------------------------------*
*       FORM FORM_MAIN                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*

FORM FORM_MAIN.

*Note 593071 begin
if d_x = 0.
   lfb1-qsrec = ktab-qsrec.
   if tin_type NE 2.
      lfa1-stcd1 = lfa1-stcd2.
   endif.
endif.
*Note 593071 end

  BSAK-QSSKZ  = X059-QSCOD.
  T059F-QSATZ = X059-QSATZ.
  LFB1-QLAND  = KTAB-QLAND.
  BSAK-QSSHB  = RTAB-QSSHH.
  BSAK-QBSHB  = RTAB-QBSHH.
*note 808104
 perform round_amount using bsak-qsshb
                      changing rounded_amount.
 bsak-qsshb = rounded_amount.

 perform round_amount using bsak-qbshb
                    changing rounded_amount.
 bsak-qbshb = rounded_amount.
*note 808104
*Note 492783 begin
  IF BSAK-QSSKZ = '15' or
     bsak-qsskz = '16'.
     BSAK-QSFBT = RTAB-ALLOW.
     bsak-dmbtr = bsak-qsshb - bsak-qsfbt.
     w_bsak_qsfbt = bsak-qsfbt.
     w_bsak_dmbtr = bsak-dmbtr.

  ELSE.
     write space to w_bsak_qsfbt.
     write space to w_bsak_dmbtr.
  ENDIF.

*  BSAK-DMBTR = BSAK-QSSHB - BSAK-QSFBT.
*Note 492783 end
*Note 497879 begin
  IF BSAK-QBSHB = 0.
     t059f-qsatz = 0.
     LFB1-QSBGR = KTAB-QSBGR.
  ENDIF.
*Exemption code
   if  t059f-qsatz ge 31.
       lfbw-wt_wtexrs = ' '.
   elseif
      ( t059f-qsatz ge 1 ) and ( t059f-qsatz le 30 ).
      lfbw-wt_wtexrs  = '00'.
   endif.
*Country text                                   "Customized by IMG
  select single * from t005t into corresponding fields of t005t
    where spras eq 'EN'
      and land1 = lfa1-land1.
*Note 497879 end

* Note 454478 begin
 IF FLAG = 0.
    GD_ELEMENT = 'LINE'.
    GD_WINDOW = 'TOP1'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
*Note 492783 begin
*Example: ZipCode 11111-0000 should be 11111 in form 1042
    if lfa1-pstlz+6(4) = '0000'.
       lfa1-pstlz+5(5) = space.
    endif.
*Note 492783 end
    gd_element = 'ADRS1'.
    GD_WINDOW = 'ADDRESS'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'AID1'.
    GD_WINDOW = 'AID'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'W-EIN'.
    GD_WINDOW = 'W-EIN'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
*Note 492783 begin
*Example: ZipCode 11111-0000 should be 11111 in form 1042
     if sadr-pstlz+6(4) = '0000'.
       sadr-pstlz+5(5) = space.
     endif.
*Note 492783 end
    GD_ELEMENT = 'AADRS1'.
    GD_WINDOW = 'AGENT'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'KONTO1'.
    GD_WINDOW = 'KONTO'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'CODE'.
    GD_WINDOW = 'CODE'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'RID1'.
    GD_WINDOW = 'RID'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'RCOUNTRY1'.
    GD_WINDOW = 'RCOUNTRY'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'LAND1'.
    GD_WINDOW = 'LAND'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'PAYER1'.
    GD_WINDOW = 'PAYER'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
*
    IF TIN_TYPE = 2.
       GD_ELEMENT = 'R-SSN'.
       GD_WINDOW = 'R-SSN'.
       PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    ELSE.
       GD_ELEMENT = 'R-EIN'.
       GD_WINDOW = 'R-EIN'.
       PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    ENDIF.
* FLAG = FLAG + 1.
  FLAG = '0'.
ELSE.
  IF FLAG = 1.
*
  MOVE X059-QSCOD TO *BSAK-QSSKZ.
*Note 497879 begin
  move t059f-qsatz to *t059f-qsatz.
*  MOVE X059-QSATZ TO *T059F-QSATZ.
*Note 497879 end
  MOVE KTAB-QLAND TO *LFB1-QLAND.
  MOVE RTAB-QSSHH TO *BSAK-QSSHB.
  MOVE RTAB-QBSHH TO *BSAK-QBSHB.
*Note 808104
 perform round_amount using *bsak-qsshb
                      changing rounded_amount.
 *bsak-qsshb = rounded_amount.

 perform round_amount using *bsak-qbshb
                    changing rounded_amount.
 *bsak-qbshb = rounded_amount.
*Note 808104
*Note 492783 begin
  move w_bsak_qsfbt to *w_bsak_qsfbt.
  move w_bsak_dmbtr to *w_bsak_dmbtr.
*  MOVE BSAK-QSFBT TO *BSAK-QSFBT.
*  MOVE BSAK-DMBTR TO *BSAK-DMBTR.
*Note 492783 end
  MOVE LFB1-QSBGR TO *LFB1-QSBGR.
  MOVE LFB1-QSREC TO *LFB1-QSREC.
  MOVE LFA1 TO *LFA1.
*Note 492783 begin
  move kna1 to *kna1.
*Note 492783 end
  MOVE SADR TO *SADR.
  MOVE T005R TO *T005R.
  move t005t to *t005t.
    GD_ELEMENT = 'LINE2'.
    GD_WINDOW = 'TOP2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
*Note 492783 begin
*Example: ZipCode 11111-0000 should be 11111 in form 1042
    if *lfa1-pstlz+6(4) = '0000'.
       *lfa1-pstlz+5(5) = space.
    endif.
*Note 492783 end
    GD_ELEMENT = 'ADRS2'.
    GD_WINDOW = 'ADDRESS2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'AID2'.
    GD_WINDOW = 'AID2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'W-EIN2'.
    GD_WINDOW = 'W-EIN2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
*Note 492783 begin
*Example: ZipCode 11111-0000 should be 11111 in form 1042
     if *sadr-pstlz+6(4) = '0000'.
       *sadr-pstlz+5(5) = space.
     endif.
*Note 492783 end
    GD_ELEMENT = 'AADRS2'.
    GD_WINDOW = 'AGENT2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'KONTO2'.
    GD_WINDOW = 'KONTO2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'CODE2'.
    GD_WINDOW = 'CODE2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'RID2'.
    GD_WINDOW = 'RID2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'RCNTRY2'.
    GD_WINDOW = 'RCNTRY2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'LAND2'.
    GD_WINDOW = 'LAND2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'PAYER2'.
    GD_WINDOW = 'PAYER2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    IF TIN_TYPE = 2.
       GD_ELEMENT = 'R-SSN2'.
       GD_WINDOW = 'R-SSN2'.
       PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    ELSE.
       GD_ELEMENT = 'R-EIN2'.
       GD_WINDOW = 'R-EIN2'.
       PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    ENDIF.
         FLAG = 0.
  ENDIF.
 ENDIF.
* Note 454478 end
ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM FORM_TEST                                                *
*---------------------------------------------------------------------*
*       Prints test prints to adjust the printer                      *
*---------------------------------------------------------------------*
*  -->  TESTPRNT   Number of test prints                              *
*  -->  LAND       Country to determine address layout                *
*---------------------------------------------------------------------*

FORM FORM_TEST USING TESTPRNT LAND.

  CALL FUNCTION 'START_FORM'.
* NOTE 454478 begin
*   Window Top1
  BSAK-QSSKZ = '19'.
  BSAK-DMBTR = 9999999.
  BSAK-QSFBT = 999999.
  BSAK-QBSHB = 9999999.
  T059F-QSATZ = 100 / 8.
  LFB1-QSBGR = 'XX'.
  BSAK-QSSHB = 9999999.
*  lfb1-qland = 'FR'.
*   RID window (Recipient IDs)
  LFB1-QSREC = '99'.
  LFA1-STCD1 = '99-999-9999'.
  LFA1-LIFNR = 'XXXXXXXXXX'.
*   Window Top2
  *BSAK-QSSKZ = '19'.
  *BSAK-DMBTR = 9999999.
  *BSAK-QSFBT = 999999.
  *BSAK-QBSHB = 9999999.
  *T059F-QSATZ = 100 / 8.
  *LFB1-QSBGR = 'XX'.
  *BSAK-QSSHB = 9999999.
  *LFB1-QSREC = '99'.
  *LFA1-STCD1 = '99-999-9999'.
  *LFA1-LIFNR = 'XXXXXXXXXX'.
*   Address window
  LFA1-ANRED = 'SIRXXXXXXXXXXXX'.
  LFA1-NAME1 = 'NAMEXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
*  lfa1-name2 = 'NAME2XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
*  lfa1-name3 = 'NAME3XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
*  lfa1-name4 = 'NAME4XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
*  lfa1-ort01 = 'CITYXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
  LFA1-ORT01 = 'CITYXXXXXXXX'.
*  lfa1-stras = 'STREETXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
  LFA1-STRAS = 'STREETXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
  LFA1-PFACH = 'XXXXXXXXXX'.
  LFA1-PSTLZ = '99999-9999'.
  LFA1-REGIO = 'PA'.
  LFA1-LAND1 = LAND.
*  sadr-land1 = land.
*   Window with recipients tax country.
  T005R-QLTXT = 'XXXXXXXXXXXXXXX'.
*   Address window2
  *LFA1-ANRED = 'SIRXXXXXXXXXXXX'.
  *LFA1-NAME1 = 'NAMEXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
*  *lfa1-name2 = 'NAME2XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
*  *lfa1-name3 = 'NAME3XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
*  *lfa1-ort01 = 'CITYXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
  *LFA1-STRAS = 'STREETXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
  *LFA1-ORT01 = 'CITYXXXXXXX'.
  *LFA1-PSTLZ = '99999-9999'.
  *LFA1-REGIO = 'PA'.
  *LFA1-LAND1 = LAND.
  *T005R-QLTXT = 'XXXXXXXXXXXXXXX'.
*   Address window AGENT
  SADR-ANRED = 'SIRXXXXXXXXXXXX'.
  SADR-NAME1 = 'NAMEXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
*  sadr-name2 = 'NAME2XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
*  sadr-name3 = 'NAME3XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
*  sadr-name4 = 'NAME4XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
*  sadr-ort01 = 'CITYXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
  SADR-ORT01 = 'CITYXXXXXXXXX'.
  SADR-STRAS = 'STREETXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
  SADR-PSTLZ = '99999-9999'.
  SADR-REGIO = 'PA'.
  SADR-LAND1 = LAND.
*  t001-land1 = land.
*   Address window AGENT
  *SADR-ANRED = 'SIRXXXXXXXXXXXX'.
  *SADR-NAME1 = 'NAMEXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
*  *sadr-name2 = 'NAME2XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
*  *sadr-name3 = 'NAME3XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
*  *sadr-ort01 = 'CITYXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
  *SADR-ORT01 = 'CITYXXXXXXXX'.
  *SADR-STRAS = 'STREETXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
  *SADR-PSTLZ = '99999-9999'.
  *SADR-REGIO = 'PA'.
  *SADR-LAND1 = LAND.
*   Window with Agent IDs / Numbers
  SADR-DBNAME = '99-9999999'.
  KNA1-NAME1 = 'NAMEXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
  KNA1-NAME2 = '99-9999999'.
*   Window with Agent IDs / Numbers
  *SADR-DBNAME = '99-9999999'.
  *KNA1-NAME1 = 'NAMEXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
  *KNA1-NAME2 = '99-9999999'.

  TESTPRNT = TESTPRNT * 2.

  DO TESTPRNT TIMES.
    GD_ELEMENT = 'LINE'.
    GD_WINDOW = 'TOP1'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'ADRS1'.
    GD_WINDOW = 'ADDRESS'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'AID1'.
    GD_WINDOW = 'AID'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'W-EIN'.
    GD_WINDOW = 'W-EIN'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'AADRS1'.
    GD_WINDOW = 'AGENT'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'KONTO1'.
    GD_WINDOW = 'KONTO'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'CODE'.
    GD_WINDOW = 'CODE'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'RID1'.
    GD_WINDOW = 'RID'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'RCOUNTRY1'.
    GD_WINDOW = 'RCOUNTRY'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'LAND1'.
    GD_WINDOW = 'LAND'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'PAYER1'.
    GD_WINDOW = 'PAYER'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'R-EIN'.
    GD_WINDOW = 'R-EIN'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'LINE2'.
    GD_WINDOW = 'TOP2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'ADRS2'.
    GD_WINDOW = 'ADDRESS2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'AID2'.
    GD_WINDOW = 'AID2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'W-EIN2'.
    GD_WINDOW = 'W-EIN2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'AADRS2'.
    GD_WINDOW = 'AGENT2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'KONTO2'.
    GD_WINDOW = 'KONTO2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'CODE2'.
    GD_WINDOW = 'CODE2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'RID2'.
    GD_WINDOW = 'RID2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'RCNTRY2'.
    GD_WINDOW = 'RCNTRY2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'LAND2'.
    GD_WINDOW = 'LAND2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'PAYER2'.
    GD_WINDOW = 'PAYER2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'R-SSN2'.
    GD_WINDOW = 'R-SSN2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.
    GD_ELEMENT = 'R-EIN2'.
    GD_WINDOW = 'R-EIN2'.
    PERFORM F_FORMPRINT USING GD_ELEMENT GD_WINDOW.

*    call function 'WRITE_FORM'
*         exporting
*              window  = 'TOP1'
*              element = 'LINE'.
  ENDDO.

  CALL FUNCTION 'END_FORM'.
* CALL FUNCTION 'CONTROL_FORM'
*      EXPORTING
*           COMMAND = 'NEW-PAGE'.
CLEAR: BSAK-QSSKZ, BSAK-DMBTR, BSAK-QSFBT, BSAK-QBSHB, T059F-QSATZ,
      LFB1-QSBGR, BSAK-QSSHB, LFB1-QSREC, LFA1-STCD1, LFA1-LIFNR,
      *BSAK-QSSKZ, *BSAK-DMBTR, *BSAK-QSFBT, *BSAK-QBSHB, *T059F-QSATZ,
      *LFB1-QSBGR, *BSAK-QSSHB, *LFB1-QSREC, *LFA1-STCD1, *LFA1-LIFNR,
      LFA1-ANRED, LFA1-NAME1, LFA1-ORT01,LFA1-STRAS,LFA1-PSTLZ,
      LFA1-REGIO, LFA1-LAND1, T005R-QLTXT,
      *LFA1-ANRED, *LFA1-NAME1,*LFA1-STRAS, *LFA1-ORT01, *LFA1-PSTLZ,
      *LFA1-REGIO, *LFA1-LAND1, *T005R-QLTXT,
      SADR-ANRED, SADR-NAME1,SADR-ORT01, SADR-STRAS, SADR-PSTLZ,
      SADR-REGIO, SADR-LAND1,
      *SADR-ANRED, *SADR-NAME1,*SADR-ORT01, *SADR-STRAS, *SADR-PSTLZ,
      *SADR-REGIO, *SADR-LAND1, SADR-DBNAME, KNA1-NAME1, KNA1-NAME2,
      *SADR-DBNAME, *KNA1-NAME1, *KNA1-NAME2.
* Note 454478 end
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_formprint
*----------------------------------------------------------------------*
* Note 454478 begin
FORM F_FORMPRINT USING IELEMENT IWINDOW.
  DATA: BEGIN OF LTEXT_HEADER.
          INCLUDE STRUCTURE THEAD.
  DATA: END OF LTEXT_HEADER.

  DATA: BEGIN OF LFORM_LINES OCCURS 0.
          INCLUDE STRUCTURE TLINE.
  DATA: END OF LFORM_LINES.


  CALL FUNCTION 'READ_FORM_LINES'
       EXPORTING
            ELEMENT  = IELEMENT
            FORM     = SAPFORMS
            WINDOW   = IWINDOW
            LANGUAGE = 'E'
       TABLES
            LINES    = LFORM_LINES.


*   ltext_header-tdlinesize = 63.
   LTEXT_HEADER-TDLINESIZE = 132.
  CALL FUNCTION 'TEXT_SYMBOL_REPLACE'
       EXPORTING
            HEADER = LTEXT_HEADER
            INIT   = ' '
       TABLES
            LINES  = LFORM_LINES.


  CALL FUNCTION 'WRITE_FORM_LINES'
       EXPORTING
*           function                 = 'SET'
            HEADER                   = LTEXT_HEADER
*           type                     = 'BODY'
           WINDOW                   = IWINDOW
       TABLES
            LINES                    = LFORM_LINES
       EXCEPTIONS
            FUNCTION                 = 1
            TYPE                     = 2
            UNOPENED                 = 3
            UNSTARTED                = 4
            WINDOW                   = 5
            BAD_PAGEFORMAT_FOR_PRINT = 6.



ENDFORM.                               " F_formprint
* Note 454478 end
