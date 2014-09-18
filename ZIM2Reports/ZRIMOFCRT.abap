*&---------------------------------------------------------------------*
*& Report  ZRIMOFCRT                                                   *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZRIMOFCRT   MESSAGE-ID ZIM                  .

TABLES: ZTREQHD, ZTREQST, ZTMLCSG7O, LFA1, T005T.



DATA: FM_NAME TYPE RS38L_FNAM.
DATA: CONTROL_PARAMETERS TYPE SSFCTRLOP.
DATA: OUTPUT_OPTIONS TYPE SSFCOMPOP.
DATA: OTF_TAB LIKE ITCOO OCCURS 0 WITH HEADER LINE.
DATA: PDF_FSIZE TYPE I.
DATA: PDF_TABLE TYPE RCL_BAG_TLINE.
DATA: BEGIN OF PDF_LINE_TAB OCCURS 0,
        LINE(134) TYPE C,
      END OF PDF_LINE_TAB.
DATA: G_HTML_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_HTML_CONTROL TYPE REF TO CL_GUI_HTML_VIEWER.

DATA: P_REPID TYPE SY-REPID,
      P_DYNNR TYPE SY-DYNNR,
      URL(255),
      EXTENSION TYPE I.

DATA: BEGIN OF SM_REQHD OCCURS 0.
        INCLUDE STRUCTURE ZTREQHD.
DATA: END OF SM_REQHD.

DATA: BEGIN OF SM_REQIT OCCURS 0.
        INCLUDE STRUCTURE ZTREQIT.
DATA: END OF SM_REQIT.

DATA: TEST_MODE.
DATA  G_BENIF_NM        LIKE LFA1-NAME1.
DATA: G_ADVBK_NM        LIKE LFA1-NAME1.
DATA: G_REQNO           LIKE ZTREQHD-ZFREQNO.
DATA: W_ORIG            LIKE ZTMLCSG7O-ZFORIG.
DATA: G_ORNM            LIKE T005T-LANDX.
DATA: G_SPRT            LIKE ZTREQHD-ZFSPRT.
DATA: G_APRT            LIKE ZTREQHD-ZFAPRT.
DATA: G_PSYN(20)        TYPE C.
DATA: G_TSYN(20)        TYPE C.
DATA: G_REQSD           LIKE ZTREQHD-ZFREQSD.
DATA: G_REQED           LIKE ZTREQHD-ZFREQED.
DATA: G_EBELN           LIKE ZTREQHD-EBELN.
DATA: G_REQTY           LIKE ZTREQHD-ZFREQTY.
DATA: W_LFA1            LIKE LFA1.
DATA: W_ADRC            LIKE ADRC.
DATA: G_BENI_ADD(40)    TYPE C.
DATA: G_ADVBK_ADD(40)   TYPE C.
DATA: G_BENI_ADD1(40)   TYPE C.
DATA: G_ADVBK_ADD1(40)  TYPE C.
*data: w_zfbeni_nm(28)   type c.
*data: w_open_nm(28)     type c.
DATA : W_OK_CODE        LIKE   SY-UCOMM.
*-----------------------------------------------------------------------
* Selection 절.
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_REQNO LIKE ZTREQHD-ZFREQNO OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.

*-----------------------------------------------------------------------
* Event: Initialization.
*-----------------------------------------------------------------------
INITIALIZATION.
  SET TITLEBAR 'OFCRT'.

* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.         " 해더 출력...

*-----------------------------------------------------------------------
* Event: Start-Of-Selection.
*-----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM P1000_READ_DATA.
  PERFORM P3000_WRITE_DATA.
*-----------------------------------------------------------------------
* Event: At User-Command.
*-----------------------------------------------------------------------
AT USER-COMMAND.
  W_OK_CODE = SY-UCOMM.
  CASE SY-UCOMM.
    WHEN 'CROF'.
      PERFORM P2000_CREAT_SMARTFORM.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  p1000_read_data
*&---------------------------------------------------------------------*
FORM P1000_READ_DATA.
  DATA : L_NAME1(255),
         L_NAME2(255),
         L_NAME3(255),
         L_NAME4(255).

  CLEAR : W_LFA1.

  SELECT SINGLE *
           FROM ZTREQHD
          WHERE ZFREQNO EQ P_REQNO.

  MOVE P_REQNO        TO G_REQNO.
  MOVE ZTREQHD-ZFSPRT  TO G_SPRT.
  MOVE ZTREQHD-ZFAPRT  TO G_APRT.
  MOVE ZTREQHD-ZFREQSD TO G_REQSD.
  MOVE ZTREQHD-ZFREQED TO G_REQED.
  MOVE ZTREQHD-EBELN   TO G_EBELN.
  MOVE ZTREQHD-ZFREQTY TO G_REQTY.

  IF ZTREQHD-ZZPSHIP EQ 'X'.
    MOVE 'ALLOWED' TO G_PSYN.
  ELSE.
    MOVE 'NOT ALLOWED' TO G_PSYN.
  ENDIF.

  IF ZTREQHD-ZZTSHIP EQ 'X'.
    MOVE 'ALLOWED' TO G_TSYN.
  ELSE.
    MOVE 'NOT ALLOWED' TO G_TSYN.
  ENDIF.

  SELECT SINGLE ZFORIG INTO W_ORIG
           FROM ZTMLCSG7O
          WHERE ZFREQNO EQ P_REQNO.

  SELECT SINGLE LANDX INTO G_ORNM
           FROM T005T
          WHERE LAND1 = W_ORIG
            AND SPRAS = SY-LANGU.

  CALL FUNCTION 'ZIM_GET_VENDOR_ADDRESS_FORMAT'
       EXPORTING
            LIFNR     = ZTREQHD-ZFBENI
       IMPORTING
            NAME1     = L_NAME1
            NAME2     = L_NAME2
            NAME3     = L_NAME3
            NAME4     = L_NAME4
            P_LFA1    = W_LFA1
            P_ADRC    = W_ADRC
       EXCEPTIONS
            NO_INPUT  = 01
            NOT_FOUND = 03.

  CASE SY-SUBRC.
    WHEN 01.     MESSAGE I025.
    WHEN 03.     MESSAGE E020   WITH    ZTREQHD-ZFBENI.
  ENDCASE.

  TRANSLATE : L_NAME1 TO UPPER CASE,
              L_NAME2 TO UPPER CASE,
              L_NAME3 TO UPPER CASE,
              L_NAME4 TO UPPER CASE.

  MOVE : L_NAME1      TO W_LFA1-NAME1,
         L_NAME2      TO W_LFA1-NAME2,
         L_NAME3      TO W_LFA1-NAME3,
         L_NAME4      TO W_LFA1-NAME4.

  MOVE: W_LFA1-NAME1   TO   G_BENIF_NM.
  MOVE: W_LFA1-NAME2   TO   G_BENI_ADD.
  MOVE: W_LFA1-NAME3   TO   G_BENI_ADD1.

*  select single name1 into g_benif_nm
*           from lfa1
*          where lifnr = ztreqhd-zfbeni.

*  select single name1 into g_advbk_nm
*           from lfa1
*          where lifnr = ztreqhd-zfopbn.

  CLEAR : W_LFA1, W_ADRC.

  CALL FUNCTION 'ZIM_GET_VENDOR_ADDRESS_FORMAT'
       EXPORTING
            LIFNR     = ZTREQHD-ZFOPBN
       IMPORTING
            NAME1     = L_NAME1
            NAME2     = L_NAME2
            NAME3     = L_NAME3
            NAME4     = L_NAME4
            P_LFA1    = W_LFA1
            P_ADRC    = W_ADRC
       EXCEPTIONS
            NO_INPUT  = 01
            NOT_FOUND = 03.

  CASE SY-SUBRC.
    WHEN 01.     MESSAGE I025.
    WHEN 03.     MESSAGE E020   WITH    ZTREQHD-ZFOPBN.
  ENDCASE.

  TRANSLATE : L_NAME1 TO UPPER CASE,
              L_NAME2 TO UPPER CASE,
              L_NAME3 TO UPPER CASE,
              L_NAME4 TO UPPER CASE.

  MOVE : L_NAME1      TO W_LFA1-NAME1,
         L_NAME2      TO W_LFA1-NAME2,
         L_NAME3      TO W_LFA1-NAME3.

  MOVE W_LFA1-NAME1 TO G_ADVBK_NM.
  MOVE W_LFA1-NAME2 TO G_ADVBK_ADD.
  MOVE W_LFA1-NAME3 TO G_ADVBK_ADD1.

  SELECT * INTO TABLE SM_REQIT
           FROM ZTREQIT
          WHERE ZFREQNO = P_REQNO.

  SORT SM_REQIT BY ZFITMNO.

ENDFORM.                    " p1000_read_data
*&---------------------------------------------------------------------*
*&      Form  p2000_creat_smartform
*&---------------------------------------------------------------------*
FORM P2000_CREAT_SMARTFORM.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
       EXPORTING
            FORMNAME           = 'ZIM_SMART_OFFER'
       IMPORTING
            FM_NAME            = FM_NAME
       EXCEPTIONS
            NO_FORM            = 1
            NO_FUNCTION_MODULE = 2
            OTHERS             = 3.

*  control_parameters-no_dialog = 'X'.
*  control_parameters-getotf    = 'X'.
*  output_options-tdnoprev      = 'X'.
  CALL FUNCTION FM_NAME
       EXPORTING
            SM_REQHD         = SM_REQHD
            G_BENIF_NM       = G_BENIF_NM
            G_ADVBK_NM       = G_ADVBK_NM
            G_BENI_ADD       = G_BENI_ADD
            G_BENI_ADD1      = G_BENI_ADD1
            G_ADVBK_ADD      = G_ADVBK_ADD
            G_ADVBK_ADD1     = G_ADVBK_ADD1
            G_REQNO          = G_REQNO
            G_ORNM           = G_ORNM
            G_SPRT           = G_SPRT
            G_APRT           = G_APRT
            G_PSYN           = G_PSYN
            G_TSYN           = G_TSYN
            G_REQSD          = G_REQSD
            G_REQED          = G_REQED
            G_EBELN          = G_EBELN
            G_REQTY          = G_REQTY
       TABLES
            SM_REQIT         = SM_REQIT
       EXCEPTIONS
            FORMATTING_ERROR = 1
            INTERNAL_ERROR   = 2
            SEND_ERROR       = 3
            USER_CANCELED    = 4.
*            others             = 5.

ENDFORM.                    " p2000_creat_smartform
*&---------------------------------------------------------------------*
*&      Form  p3000_write_data
*&---------------------------------------------------------------------*
FORM P3000_WRITE_DATA.

  SET PF-STATUS 'OFCRT'.
  SET TITLEBAR 'OFCRT'.

  WRITE: P_REQNO.
ENDFORM.                    " p3000_write_data
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.

  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /55  '[ Offer Sheet 생성 대상 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE,
            '개설예정'    ,  SY-VLINE NO-GAP,
            'P/O Number'    NO-GAP,  SY-VLINE NO-GAP,
            'CUR. '         NO-GAP,  SY-VLINE NO-GAP,
 '    개설 금액     '       NO-GAP,  SY-VLINE NO-GAP,
            'Ty'            NO-GAP,  SY-VLINE NO-GAP,
            'Mat'           NO-GAP,  SY-VLINE NO-GAP,
            'Pay.'          NO-GAP,  SY-VLINE NO-GAP,
    '     선  적  지     '  NO-GAP,  SY-VLINE NO-GAP,
            'Inc'           NO-GAP,  SY-VLINE NO-GAP,
            'Vendor    '    NO-GAP,  SY-VLINE NO-GAP,
            'Name',              118 SY-VLINE NO-GAP,
            'D'             NO-GAP,  SY-VLINE NO-GAP,
            ' 차입기관  '   NO-GAP,  SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.



ENDFORM.                    " P3000_TITLE_WRITE
