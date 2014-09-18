*&---------------------------------------------------------------------*
* Program Name      : ZRHRY04_EPD                                      *
* Author            : Ho-Joong, Hwang                                  *
* Creation Date     : 2003.11.18                                       *
* Specifications By : Ho-Joong, Hwang                                  *
* Pattern           : Report 1-1                                       *
* Development Request No : 4.6C UD1K904287                             *
* Addl Documentation:                                                  *
* Description       : Employee Payroll Display                         *
*                                                                      *
* Modification Logs                                                    *
* Date       Developer    RequestNo    Description                     *
* 2004.05.11 jslee72      UD1K910246   Conversion error handling       *
*
*&---------------------------------------------------------------------*
REPORT ZRHRY04_EPD MESSAGE-ID ZMHR
                   LINE-SIZE 132 LINE-COUNT 65
                   NO STANDARD PAGE HEADING.
************************************************************************
*                          DATA SOURCES                                *
************************************************************************

TABLES: PA0000,
        PA0001,
        PA0002,
        PA0006,
        PA0207,
        PA0208,
        T500P,
        T502T,                " Marital Status Designators
        T512T,                " Wage Type Texts
        T549A,
        T549T,
        T5UTK.

************************************************************************
*                           VARIABLES                                  *
************************************************************************

*... internal tables
DATA: BEGIN OF CD-KEY,
      PERNR LIKE P0001-PERNR,               " key to cluster directory
      END OF CD-KEY.

DATA: BEGIN OF RGDIR OCCURS 100.            " Cluster Directory
        INCLUDE STRUCTURE PC261.
DATA: END OF RGDIR.

DATA: BEGIN OF RX-KEY.
        INCLUDE STRUCTURE PC200.              " Payroll Results Key
DATA: END OF RX-KEY.

DATA: BEGIN OF RT OCCURS 150.               " Payroll Results
        INCLUDE STRUCTURE PC207.
DATA: END OF RT.

DATA: BEGIN OF TAX OCCURS 0.                " Payroll Results: Tax (US)
        INCLUDE STRUCTURE PC22T.
DATA: END OF TAX.

DATA: BEGIN OF ADR.
        INCLUDE STRUCTURE PC22S.              " Address
DATA: END OF ADR.

DATA: BEGIN OF VERSC.
        INCLUDE STRUCTURE PC202.
DATA: END OF VERSC.

DATA: BEGIN OF PERM.                        " Personal Characteristics
        INCLUDE STRUCTURE PC22R.
DATA: END OF PERM.

DATA: BEGIN OF CRT OCCURS 0.
        INCLUDE STRUCTURE PC208.
DATA: END OF CRT.

DATA: BEGIN OF TMP OCCURS 10,
      NUM LIKE RGDIR-SEQNR,
      FLAG.
DATA: END OF TMP.

*// === 2011.09.16  change by yn.kim  for ECC6.0 =====//*
***DATA: BEGIN OF CD-VERSION.
***        INCLUDE STRUCTURE PC201.
***DATA: MOLGA  LIKE T001P-MOLGA,           "country identifier
***      END OF CD-VERSION.

DATA: BEGIN OF CD-VERSION.
    include structure pc2_cd.                            "XTWPH9K000955
data: end of cd-version.                                 "XTWPH9K000955
*---- Cluster directory fields
*// ========= chenge end ======== //*


DATA: BEGIN OF OKR-VERSION.
        INCLUDE STRUCTURE PC201.
DATA: END OF OKR-VERSION.

DATA: IT_FORM LIKE PC408 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_EARNS OCCURS 0,
      LGART    LIKE T512T-LGART,
      LGTXT    LIKE T512T-LGTXT,
      CURNT(12),
      YTDAM(12).
DATA: END OF IT_EARNS.

DATA: IT_TAXDN LIKE IT_EARNS OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_CRTXT OCCURS 1,
      LGART    LIKE T512T-LGART,
      LGTXT    LIKE T512T-LGTXT.
DATA: END OF IT_CRTXT.

DATA: BEGIN OF IT_COLET OCCURS 0,
      LGART    LIKE T512T-LGART,
      LGTXT    LIKE T512T-LGTXT,
      CURNT    TYPE P DECIMALS 2,
      YTDAM    TYPE P DECIMALS 2.
DATA: END OF IT_COLET.

DATA: BEGIN OF IT_PERID OCCURS 0,
      ABKRS    LIKE PA0001-ABKRS,
      PABRJ    LIKE T549Q-PABRJ,
      CURNT    LIKE T549Q-PABRP,
      MAXRP    LIKE T549Q-PABRP.
DATA: END OF IT_PERID.

*... variants
DATA: W_PERNR   LIKE PA0001-PERNR,
      W_PABRJ   LIKE T549Q-PABRJ,
      W_PABRP   LIKE T549Q-PABRP.

DATA: W_WIDTH   TYPE I,
      W_SUBRC   LIKE SY-SUBRC,
      W_LTEXT(20),
      W_ADRES(30).

DATA: W_ABKRS   LIKE PA0001-ABKRS,
      W_ATEXT   LIKE T549T-ATEXT,
      W_BEGDA   LIKE T549Q-BEGDA,
      W_ENDDA   LIKE T549Q-ENDDA,
      W_PERID   LIKE PA0002-PERID.

DATA: W_STXT1   TYPE SHORT_D,
      W_STXT2   TYPE STEXT.

DATA: W_TAIXL   LIKE SY-TABIX,
      W_TAIXH   LIKE SY-TABIX,
      W_LGART   LIKE T512T-LGART,
      W_LGTXT   LIKE T512T-LGTXT.

DATA: CD-NEXT_SEQ   TYPE I,             "Next available seq number
      CD-LAST_PAY   TYPE D,             "Last payroll run date
      W_FPPER   LIKE RGDIR-FPPER.

************************************************************************
*                           MAIN SOURCE                                *
************************************************************************

SET PF-STATUS 'PSHRY04'.
*
CLEAR W_SUBRC.
PERFORM GET_PAYROLL_AREA_LAST_VALUE.
PERFORM INIT_DISPLAY_SCREEN.

************************************************************************
*                          USER COMMNAD                                *
************************************************************************

AT USER-COMMAND.
  CLEAR W_SUBRC.
  CASE SY-UCOMM.
    WHEN 'EXEC' OR 'ENTR'.
      PERFORM READ_SELECT_OPTIONS.
    WHEN 'NEXT'.
      PERFORM READ_SELECT_OPTIONS.
      W_PERNR = W_PERNR + 1.
    WHEN 'PREV'.
      PERFORM READ_SELECT_OPTIONS.
      W_PERNR = W_PERNR - 1.
  ENDCASE.
  PERFORM GET_PAYROLL_PERIOD.
  PERFORM CHECK_PAYROLL_AREA.
* check w_subrc = 0.
  IF W_SUBRC = 0.
    PERFORM GET_PAYROLL_RESULT.
    PERFORM WRITE_RESULT.
  ELSE.
    SY-LSIND = 0.
    PERFORM INIT_DISPLAY_SCREEN.
  ENDIF.
  CLEAR SY-UCOMM.

*&---------------------------------------------------------------------*
*&      Form  init_display_screen
*&---------------------------------------------------------------------*
FORM INIT_DISPLAY_SCREEN.
  W_WIDTH = 121.
*
  ULINE AT (W_WIDTH).
  WRITE: / SY-VLINE NO-GAP.
  WRITE: (12) 'Employee # :',
         (8) W_PERNR INPUT ON USING EDIT MASK '==ALPHA',
             AT W_WIDTH SY-VLINE NO-GAP.
  WRITE: / SY-VLINE NO-GAP.
  WRITE: (12) 'Year       :',
         (4) W_PABRJ INPUT ON, AT W_WIDTH SY-VLINE NO-GAP.
  WRITE: / SY-VLINE NO-GAP.
  WRITE: (12) 'Period     :',
         (2) W_PABRP INPUT ON, AT W_WIDTH SY-VLINE NO-GAP.
  ULINE AT (W_WIDTH).
*
  WRITE: / SY-VLINE NO-GAP.
  WRITE: (31) 'EARNINGS' NO-GAP, (2) ' ' NO-GAP,
         (12) 'CURRENT' RIGHT-JUSTIFIED,
         (12) 'YTD' RIGHT-JUSTIFIED, SY-VLINE NO-GAP.
  WRITE: (31) 'TAX & DEDUCTION' NO-GAP, (2) ' ' NO-GAP,
         (12) 'CURRENT' RIGHT-JUSTIFIED,
         (12) 'YTD' RIGHT-JUSTIFIED, SY-VLINE NO-GAP.
  ULINE AT (W_WIDTH).
*
  DO 17 TIMES.
    WRITE: / SY-VLINE NO-GAP.
    WRITE: (59) ' ' NO-GAP, SY-VLINE NO-GAP,
           (59) ' ' NO-GAP, AT W_WIDTH SY-VLINE NO-GAP.
  ENDDO.
  ULINE AT (W_WIDTH).
*
  WRITE: / SY-VLINE NO-GAP.
  WRITE: (20) 'GROSS' RIGHT-JUSTIFIED, (6) '-' RIGHT-JUSTIFIED,
         (11) 'PRE TAXES' RIGHT-JUSTIFIED, (6) '=' RIGHT-JUSTIFIED,
         (11) 'TAXABLE' RIGHT-JUSTIFIED, (6) '-' RIGHT-JUSTIFIED,
         (11) 'TAXES' RIGHT-JUSTIFIED, (6) '-' RIGHT-JUSTIFIED,
         (11) 'POST TAX' RIGHT-JUSTIFIED, (6) '=' RIGHT-JUSTIFIED,
         (11) 'NET PAY' RIGHT-JUSTIFIED, AT W_WIDTH SY-VLINE NO-GAP.
  ULINE AT (W_WIDTH).
*
  WRITE: / SY-VLINE NO-GAP.
  WRITE: (7) 'Curr. :' , AT W_WIDTH SY-VLINE NO-GAP.
  WRITE: / SY-VLINE NO-GAP.
  WRITE: (7) 'YTD   :' , AT W_WIDTH SY-VLINE NO-GAP.
  ULINE AT (W_WIDTH).
ENDFORM.                    " init_display_screen
*&---------------------------------------------------------------------*
*&      Form  get_personel_info
*&---------------------------------------------------------------------*
FORM GET_PERSONEL_INFO.
  CHECK W_SUBRC = 0.
*... get personal name & personal area
  CLEAR PA0001.
  SELECT SINGLE BUKRS WERKS ENAME
    INTO (PA0001-BUKRS, PA0001-WERKS, PA0001-ENAME)
    FROM PA0001 WHERE PERNR = W_PERNR
                  AND ENDDA = '99991231'.

*... get personal area name
  CLEAR T500P.
  SELECT SINGLE NAME1 INTO T500P-NAME1
    FROM T500P WHERE PERSA = PA0001-WERKS
                 AND MOLGA = '10'
                 AND BUKRS = PA0001-BUKRS.

*... get
  CLEAR TAX.
  READ TABLE TAX INDEX 1.
  CLEAR T5UTK.
  SELECT SINGLE STEXT INTO T5UTK-STEXT
    FROM T5UTK WHERE TAXAU = TAX-TAXAU
                 AND TXSTA = TAX-TXSTA
                 AND ENDDA = '99991231'.

*... get residence tax area & work tax area
  CLEAR PA0207.
  SELECT SINGLE TAXAR INTO PA0207-TAXAR
    FROM PA0207 WHERE PERNR = W_PERNR
                  AND ENDDA = '99991231'.

  CLEAR PA0208.
  SELECT SINGLE WTART INTO PA0208-WTART
    FROM PA0208 WHERE PERNR = W_PERNR
                  AND ENDDA = '99991231'.
ENDFORM.                    " get_personel_info
*&---------------------------------------------------------------------*
*&      Form  get_payroll_result
*&---------------------------------------------------------------------*
FORM GET_PAYROLL_RESULT.
  PERFORM GET_SEQUENTIAL_NUMBER.
  CHECK W_SUBRC = 0.
  READ TABLE TMP INDEX 1.
  PERFORM GET_RESULT_TABLE.
  PERFORM GET_RESULT_DATA_BY_FUNCTION.
ENDFORM.                    " get_payroll_result
*&---------------------------------------------------------------------*
*&      Form  get_sequential_number
*&---------------------------------------------------------------------*
FORM GET_SEQUENTIAL_NUMBER.
  CONCATENATE W_PABRJ W_PABRP INTO W_FPPER.
*
  CLEAR: RGDIR, TMP.
  REFRESH: RGDIR, TMP.
*
  CD-KEY = W_PERNR.
  IMPORT CD-VERSION CD-NEXT_SEQ CD-LAST_PAY RGDIR
    FROM DATABASE PCL2(CU) CLIENT SY-MANDT ID CD-KEY.
*
  W_SUBRC = SY-SUBRC.
  IF W_SUBRC = 0.
    DELETE RGDIR WHERE FPPER <> W_FPPER.
    READ TABLE RGDIR WITH KEY ABKRS = W_ABKRS
                              FPPER = W_FPPER
                              FPBEG = W_BEGDA
                              FPEND = W_ENDDA
                              INPER = W_FPPER
                              IPEND = W_ENDDA.
    IF SY-SUBRC = 0.
      TMP-NUM = RGDIR-SEQNR.
      APPEND TMP. CLEAR TMP.
    ELSE.
      READ TABLE RGDIR WITH KEY ABKRS = W_ABKRS
                                FPPER = W_FPPER
                                FPEND = W_ENDDA
                                INPER = W_FPPER
                                IPEND = W_ENDDA.
      IF SY-SUBRC = 0.
        W_BEGDA = RGDIR-FPBEG.
        TMP-NUM = RGDIR-SEQNR.
        APPEND TMP. CLEAR TMP.
      ENDIF.
    ENDIF.
    W_SUBRC = SY-SUBRC.
  ENDIF.
ENDFORM.                    " get_sequential_number
*&---------------------------------------------------------------------*
*&      Form  get_result_table
*&---------------------------------------------------------------------*
FORM GET_RESULT_TABLE.
  CLEAR RT. REFRESH RT.
  CLEAR: TAX, TAX[], VERSC, PERM, ADR, CRT, CRT[].
*
  RX-KEY-PERNR = W_PERNR.
  RX-KEY-SEQNO = TMP-NUM.
*
  IMPORT KR-VERSION TO OKR-VERSION RT CRT
    FROM DATABASE PCL2(RU) ID RX-KEY.
*
  CLEAR IT_CRTXT. REFRESH IT_CRTXT.
*
  SORT CRT BY LGART.
  LOOP AT CRT.
    AT NEW LGART.
      IT_CRTXT-LGART = CRT-LGART.
      CLEAR T512T.
      SELECT SINGLE LGTXT INTO T512T-LGTXT
        FROM T512T WHERE SPRSL = SY-LANGU
                     AND MOLGA = '10'
                     AND LGART = CRT-LGART.
      IT_CRTXT-LGTXT = T512T-LGTXT.
      APPEND IT_CRTXT. CLEAR IT_CRTXT.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " get_result_table
*&---------------------------------------------------------------------*
*&      Form  write_result
*&---------------------------------------------------------------------*
FORM WRITE_RESULT.
  CASE W_SUBRC.
    WHEN 0.        PERFORM WRITE_TRUE_LIST.
    WHEN OTHERS.
      MESSAGE I010 WITH W_PERNR.
      PERFORM NEXT_NUMBER_SEARCH.
  ENDCASE.
ENDFORM.                    " write_result
*&---------------------------------------------------------------------*
*&      Form  write_true_list
*&---------------------------------------------------------------------*
FORM WRITE_TRUE_LIST.
  SY-LSIND = 0.
*
  CLEAR IT_FORM.
  READ TABLE IT_FORM INDEX 2.
  W_LTEXT = IT_FORM-LINDA+1(15).
*
  CLEAR IT_FORM.
  READ TABLE IT_FORM INDEX 6.
  ULINE AT (W_WIDTH).
  WRITE: / SY-VLINE NO-GAP.
  WRITE: (12) 'Employee # :',
         (8) W_PERNR INPUT ON USING EDIT MASK '==ALPHA'.
  WRITE: 24 '(' NO-GAP, W_LTEXT+(14) NO-GAP, ')'.
  WRITE: '(' NO-GAP, (10) W_ATEXT NO-GAP COLOR 6 INVERSE, ')' NO-GAP.
  WRITE: 55(25) IT_FORM-LINDA+28(25) NO-GAP,
         88 IT_FORM-LINDA+1(25) NO-GAP, AT W_WIDTH SY-VLINE NO-GAP.
*
  CLEAR IT_FORM.
  READ TABLE IT_FORM INDEX 7.
*
  WRITE: / SY-VLINE NO-GAP.
  WRITE: (12) 'Year       :', (4) W_PABRJ INPUT ON.
  WRITE: 24 '(' NO-GAP, IT_FORM-LINDA+64(14) NO-GAP, ')'.
  WRITE: '(' NO-GAP, (10) W_STXT1 NO-GAP COLOR 6 INVERSE, ')' NO-GAP.
  WRITE: 55(25) IT_FORM-LINDA+28(25) NO-GAP,
         88 IT_FORM-LINDA+1(25) NO-GAP, AT W_WIDTH SY-VLINE NO-GAP.
*
  CLEAR IT_FORM.
  READ TABLE IT_FORM INDEX 8.
*
  WRITE: / SY-VLINE NO-GAP.
  WRITE: (12) 'Period     :',
         (2) W_PABRP INPUT ON, 24 '(' NO-GAP, W_BEGDA,
         '-', W_ENDDA NO-GAP, ')' NO-GAP.
  WRITE: 55(25) IT_FORM-LINDA+28(25) NO-GAP,
         88 IT_FORM-LINDA+1(25) NO-GAP, AT W_WIDTH SY-VLINE NO-GAP.
*
  ULINE AT (W_WIDTH).
*
  WRITE: / SY-VLINE NO-GAP.
  WRITE: (31) 'EARNINGS' NO-GAP, (2) ' ' NO-GAP,
         (12) 'CURRENT' RIGHT-JUSTIFIED,
         (12) 'YTD' RIGHT-JUSTIFIED, SY-VLINE NO-GAP.
  WRITE: (31) 'TAX & DEDUCTION' NO-GAP, (2) ' ' NO-GAP,
         (12) 'CURRENT' RIGHT-JUSTIFIED,
         (12) 'YTD' RIGHT-JUSTIFIED, SY-VLINE NO-GAP.
  ULINE AT (W_WIDTH).
*
  PERFORM WRITE_LIST_BODY.
ENDFORM.                    " write_true_list
*&---------------------------------------------------------------------*
*&      Form  get_payroll_period
*&---------------------------------------------------------------------*
FORM GET_PAYROLL_PERIOD.
  CLEAR: W_ABKRS, W_BEGDA, W_ENDDA, W_STXT1, W_STXT2.
*... get payroll area
  CALL FUNCTION 'HR_MX_GET_PAYROLL_AREA'
       EXPORTING
            PERNR        = W_PERNR
            DATE         = SY-DATUM
       IMPORTING
            PAYROLL_AREA = W_ABKRS
       EXCEPTIONS
            IT1_ERROR    = 1
            OTHERS       = 2.

*... get payroll period
  CALL FUNCTION 'HR_GB_PERIOD_DATES'
       EXPORTING
            ABKRS            = W_ABKRS
            PABRJ            = W_PABRJ
            PABRP            = W_PABRP
       IMPORTING
            BEGDA            = W_BEGDA
            ENDDA            = W_ENDDA
       EXCEPTIONS
            PERIOD_NOT_FOUND = 1
            OTHERS           = 2.

*... get payroll area text
  CLEAR T549T.
  SELECT SINGLE ATEXT INTO T549T-ATEXT
    FROM T549T WHERE SPRSL = SY-LANGU
                 AND ABKRS = W_ABKRS.
  CLEAR W_ATEXT.
  W_ATEXT = T549T-ATEXT.

*... get job text
  CLEAR PA0001.
  SELECT SINGLE STELL INTO PA0001-STELL
    FROM PA0001 WHERE PERNR = W_PERNR
                  AND ENDDA = '99991231'.

  CALL FUNCTION 'HRWPC_RFC_STELL_TEXT_GET'
       EXPORTING
            STELL       = PA0001-STELL
            BEGDA       = SY-DATUM
            ENDDA       = SY-DATUM
            LANGU       = SY-LANGU
       IMPORTING
            STELL_TEXT1 = W_STXT1
            STELL_TEXT2 = W_STXT2.
ENDFORM.                    " get_payroll_period
*&---------------------------------------------------------------------*
*&      Form  get_result_data_by_function
*&---------------------------------------------------------------------*
FORM GET_RESULT_DATA_BY_FUNCTION.
  CLEAR IT_FORM.
  REFRESH IT_FORM.
*
  CALL FUNCTION 'GET_PAYSLIP'
       EXPORTING
            EMPLOYEE_NUMBER = W_PERNR
            SEQUENCE_NUMBER = TMP-NUM
            PAYSLIP_VARIANT = 'CUS&02'
       TABLES
            P_FORM          = IT_FORM.
*
  PERFORM GET_EARNINGS_TAXES_DEDUCTION.
  PERFORM COLLECT_EARNS.
  PERFORM MODIFY_TAXES_LGTXT.
ENDFORM.                    " get_result_data_by_function
*&---------------------------------------------------------------------*
*&      Form  write_list_body
*&---------------------------------------------------------------------*
FORM WRITE_LIST_BODY.
  DATA: L_TAXAB    TYPE P DECIMALS 2,
        L_TAXES    TYPE P DECIMALS 2,
        L_POSTT    TYPE P DECIMALS 2,
        L_NETPY    TYPE P DECIMALS 2,
        C_NETPY(15).
*
  W_TAIXL = 1.
*
  DO 17 TIMES.
    WRITE: / SY-VLINE NO-GAP.
    CLEAR IT_COLET.
    READ TABLE IT_COLET INDEX W_TAIXL.
    IF SY-SUBRC = 0.
      WRITE: (4) IT_COLET-LGART, IT_COLET-LGTXT NO-GAP.
      IF IT_COLET-CURNT = 0.
        WRITE: 35(12) ' '.
      ELSE.
        WRITE: 35(12) IT_COLET-CURNT RIGHT-JUSTIFIED.
      ENDIF.
      IF IT_COLET-YTDAM = 0.
        WRITE: (12) ' ', SY-VLINE NO-GAP.
      ELSE.
        WRITE: (12) IT_COLET-YTDAM RIGHT-JUSTIFIED, SY-VLINE NO-GAP.
      ENDIF.
    ELSE.
      WRITE: (4) ' ', 35(12) ' ', (12) ' ', SY-VLINE NO-GAP.
    ENDIF.
    CLEAR IT_TAXDN.
    READ TABLE IT_TAXDN INDEX W_TAIXL.
    WRITE: (4) IT_TAXDN-LGART, IT_TAXDN-LGTXT NO-GAP.
    WRITE: 95(12) IT_TAXDN-CURNT RIGHT-JUSTIFIED,
             (12) IT_TAXDN-YTDAM RIGHT-JUSTIFIED,
                  AT W_WIDTH SY-VLINE NO-GAP.
    W_TAIXL = W_TAIXL + 1.
  ENDDO.
*
  ULINE AT (W_WIDTH).
  CLEAR IT_FORM.
  READ TABLE IT_FORM WITH KEY LINDA+1(14) = '         GROSS'.
  IF SY-SUBRC = 0.
    W_TAIXL = SY-TABIX.
    WRITE: / SY-VLINE NO-GAP.
    WRITE: (20) IT_FORM-LINDA+1(14) RIGHT-JUSTIFIED,
           (6) '-' RIGHT-JUSTIFIED,
           (11) IT_FORM-LINDA+22(7) RIGHT-JUSTIFIED,
           (6) '=' RIGHT-JUSTIFIED,
           (11) IT_FORM-LINDA+34(7) RIGHT-JUSTIFIED,
           (6) '-' RIGHT-JUSTIFIED,
           (11) IT_FORM-LINDA+47(5) RIGHT-JUSTIFIED,
           (6) '-' RIGHT-JUSTIFIED,
           (11) IT_FORM-LINDA+58(8) RIGHT-JUSTIFIED,
           (6) '=' RIGHT-JUSTIFIED,
           (11) IT_FORM-LINDA+68(11) RIGHT-JUSTIFIED,
           AT W_WIDTH SY-VLINE NO-GAP.
  ENDIF.
*
  W_TAIXL = W_TAIXL + 1.
  READ TABLE IT_FORM INDEX W_TAIXL.
*
  ULINE AT (W_WIDTH).
  WRITE: / SY-VLINE NO-GAP.
  WRITE: (8) IT_FORM-LINDA+1(6),
         IT_FORM-LINDA+7(11) RIGHT-JUSTIFIED, (6) '-' RIGHT-JUSTIFIED,
         IT_FORM-LINDA+18(11) RIGHT-JUSTIFIED, (6) '=' RIGHT-JUSTIFIED,
         IT_FORM-LINDA+31(11) RIGHT-JUSTIFIED, (6) '-' RIGHT-JUSTIFIED,
         IT_FORM-LINDA+44(11) RIGHT-JUSTIFIED, (6) '-' RIGHT-JUSTIFIED,
         IT_FORM-LINDA+56(11) RIGHT-JUSTIFIED, (6) '=' RIGHT-JUSTIFIED,
         IT_FORM-LINDA+68(11) RIGHT-JUSTIFIED,
         AT W_WIDTH SY-VLINE NO-GAP.
*
  W_TAIXL = W_TAIXL + 1.
  READ TABLE IT_FORM INDEX W_TAIXL.
*
  CLEAR: L_TAXAB, L_TAXES, L_POSTT, L_NETPY.
  PERFORM PACK_CTYPE_TO_PTYPE USING IT_FORM-LINDA+31(11).
  PERFORM PACK_CTYPE_TO_PTYPE USING IT_FORM-LINDA+44(11).
  PERFORM PACK_CTYPE_TO_PTYPE USING IT_FORM-LINDA+56(11).
  L_TAXAB = IT_FORM-LINDA+31(11).
  L_TAXES = IT_FORM-LINDA+44(11).
  L_POSTT = IT_FORM-LINDA+56(11).
  L_NETPY = L_TAXAB - L_TAXES - L_POSTT.
  C_NETPY = L_NETPY.
*
  WRITE: / SY-VLINE NO-GAP.
  WRITE: (8) IT_FORM-LINDA+1(6),
         IT_FORM-LINDA+7(11) RIGHT-JUSTIFIED, (6) '-' RIGHT-JUSTIFIED,
         IT_FORM-LINDA+18(11) RIGHT-JUSTIFIED, (6) '=' RIGHT-JUSTIFIED,
         IT_FORM-LINDA+31(11) RIGHT-JUSTIFIED, (6) '-' RIGHT-JUSTIFIED,
         IT_FORM-LINDA+44(11) RIGHT-JUSTIFIED, (6) '-' RIGHT-JUSTIFIED,
         IT_FORM-LINDA+56(11) RIGHT-JUSTIFIED, (6) '=' RIGHT-JUSTIFIED,
         (11) C_NETPY RIGHT-JUSTIFIED,
         AT W_WIDTH SY-VLINE NO-GAP.
  ULINE AT (W_WIDTH).
*
  CLEAR IT_FORM.
  READ TABLE IT_FORM INDEX 2.
  WRITE: /2 IT_FORM-LINDA+40(7), IT_FORM-LINDA+50(15).
*
  CLEAR IT_FORM.
  READ TABLE IT_FORM INDEX 3.
  WRITE: 30 IT_FORM-LINDA+40(30), 88 SY-UNAME, SY-DATUM,
            SY-UZEIT USING EDIT MASK '__:__:__'.
ENDFORM.                    " write_list_body
*&---------------------------------------------------------------------*
*&      Form  search_wage_type
*&---------------------------------------------------------------------*
FORM SEARCH_WAGE_TYPE USING P_LGTXT.
  DATA: L_LGTXT(20).
  CLEAR: W_LGART, W_LGTXT.
*
  CHECK P_LGTXT NE SPACE.
  LOOP AT IT_CRTXT WHERE LGTXT CS P_LGTXT.
    W_LGART = IT_CRTXT-LGART.
    W_LGTXT = IT_CRTXT-LGTXT.
  ENDLOOP.
*
  CHECK SY-SUBRC <> 0.
*
  CHECK P_LGTXT NE SPACE.
  CONCATENATE P_LGTXT '%' INTO L_LGTXT.
*
  CLEAR T512T.
  SELECT SINGLE LGART LGTXT INTO (T512T-LGART, T512T-LGTXT)
    FROM T512T WHERE SPRSL = SY-LANGU
                 AND MOLGA = '10'
                 AND LGTXT LIKE L_LGTXT.
*
  W_LGART = T512T-LGART.
  W_LGTXT = T512T-LGTXT.
*
  CASE W_LGART.
    WHEN '1205'.   W_LGART = '1206'.
    WHEN '/115'.   W_LGART = '1053'.
    WHEN OTHERS.   EXIT.
  ENDCASE.

  CLEAR T512T.
  SELECT SINGLE LGTXT INTO T512T-LGTXT
    FROM T512T WHERE SPRSL = SY-LANGU
                 AND MOLGA = '10'
                 AND LGART = W_LGART.
  W_LGTXT = T512T-LGTXT.
ENDFORM.                    " search_wage_type
*&---------------------------------------------------------------------*
*&      Form  get_earnings_taxes_deduction
*&---------------------------------------------------------------------*
FORM GET_EARNINGS_TAXES_DEDUCTION.
  CLEAR: IT_EARNS, IT_TAXDN, IT_EARNS[], IT_TAXDN[].
*
  CLEAR IT_FORM.
  READ TABLE IT_FORM WITH KEY LINDA+1(8) = 'EARNINGS'.
  W_TAIXL = SY-TABIX.

  CLEAR IT_FORM.
  READ TABLE IT_FORM WITH KEY LINDA+1(16) = 'Total Earnings :'.
  IF SY-SUBRC <> 0.
    CLEAR IT_FORM.
    READ TABLE IT_FORM WITH KEY LINDA+1(16) = 'TOTAL WAGES:    '.
  ENDIF.
  W_TAIXH = SY-TABIX.

  IF W_TAIXH > 64.
    W_TAIXH = 27.
  ENDIF.
*
  LOOP AT IT_FORM.
    IF SY-TABIX <= W_TAIXL.
      CONTINUE.
    ELSEIF SY-TABIX >= W_TAIXH.
      CONTINUE.
    ELSE.
      PERFORM SEARCH_WAGE_TYPE USING IT_FORM-LINDA+1(16).
      IF W_LGART NE SPACE AND W_LGTXT NE SPACE.
        IT_EARNS-LGART = W_LGART.
        IT_EARNS-LGTXT = W_LGTXT.
        IT_EARNS-CURNT = IT_FORM-LINDA+17(10).
        IT_EARNS-YTDAM = IT_FORM-LINDA+29(12).
        APPEND IT_EARNS. CLEAR IT_EARNS.
      ENDIF.
    ENDIF.
  ENDLOOP.
*
  DESCRIBE TABLE IT_FORM LINES SY-TFILL.
  IF SY-TFILL > 100.
    W_TAIXL = 80.
    CLEAR IT_FORM.
    READ TABLE IT_FORM WITH KEY LINDA+1(16) = 'Total Earnings :'.
    W_TAIXH = SY-TABIX.
    LOOP AT IT_FORM.
      IF SY-TABIX <= W_TAIXL.
        CONTINUE.
      ELSEIF SY-TABIX >= W_TAIXH.
        CONTINUE.
      ELSE.
        PERFORM SEARCH_WAGE_TYPE USING IT_FORM-LINDA+1(16).
        IF W_LGART NE SPACE AND W_LGTXT NE SPACE.
          IT_EARNS-LGART = W_LGART.
          IT_EARNS-LGTXT = W_LGTXT.
          IT_EARNS-CURNT = IT_FORM-LINDA+17(10).
          IT_EARNS-YTDAM = IT_FORM-LINDA+29(12).
          APPEND IT_EARNS. CLEAR IT_EARNS.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
*
  CLEAR IT_FORM.
  READ TABLE IT_FORM WITH KEY LINDA+1(8) = 'TAXES   '.
  W_TAIXL = SY-TABIX.

  CLEAR IT_FORM.
  READ TABLE IT_FORM WITH KEY LINDA+1(16) = 'Total Taxes : Cu'.
  W_TAIXH = SY-TABIX.
*
  LOOP AT IT_FORM.
    IF SY-TABIX <= W_TAIXL.
      CONTINUE.
    ELSEIF SY-TABIX >= W_TAIXH.
      CONTINUE.
    ELSE.
      PERFORM SEARCH_WAGE_TYPE USING IT_FORM-LINDA+1(16).
      IF W_LGART NE SPACE AND W_LGTXT NE SPACE.
        IT_TAXDN-LGART = W_LGART.
        IT_TAXDN-LGTXT = W_LGTXT.
        IT_TAXDN-CURNT = IT_FORM-LINDA+17(10).
        IT_TAXDN-YTDAM = IT_FORM-LINDA+29(12).
        APPEND IT_TAXDN. CLEAR IT_TAXDN.
      ELSE.
        IT_TAXDN-LGART = SPACE.
        IT_TAXDN-LGTXT = IT_FORM-LINDA+1(16).
        IT_TAXDN-CURNT = IT_TAXDN-YTDAM = SPACE.
        APPEND IT_TAXDN.
      ENDIF.
    ENDIF.
  ENDLOOP.
*
  DELETE IT_TAXDN WHERE LGART = SPACE
                    AND LGTXT = SPACE.
*
  CLEAR IT_FORM.
  READ TABLE IT_FORM WITH KEY LINDA+42(16) = 'Post-Tax Deducti'.
  W_TAIXH = SY-TABIX.
*
  LOOP AT IT_FORM.
    IF SY-TABIX <= W_TAIXL.
      CONTINUE.
    ELSEIF SY-TABIX >= W_TAIXH.
      CONTINUE.
    ELSE.
      PERFORM SEARCH_WAGE_TYPE USING IT_FORM-LINDA+42(16).
      IF W_LGART NE SPACE AND W_LGTXT NE SPACE.
        IT_TAXDN-LGART = W_LGART.
        IT_TAXDN-LGTXT = W_LGTXT.
        IT_TAXDN-CURNT = IT_FORM-LINDA+58(11).
        IT_TAXDN-YTDAM = IT_FORM-LINDA+70(11).
        APPEND IT_TAXDN. CLEAR IT_TAXDN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " get_earnings_taxes_deduction
*&---------------------------------------------------------------------*
*&      Form  read_select_options
*&---------------------------------------------------------------------*
FORM READ_SELECT_OPTIONS.
  DO.
    READ LINE SY-INDEX FIELD VALUE:
      W_PERNR, W_PABRJ, W_PABRP.
    IF SY-SUBRC <> 0. EXIT. ENDIF.
  ENDDO.
ENDFORM.                    " read_select_options
*&---------------------------------------------------------------------*
*&      Form  pack_ctype_to_ptype
*&---------------------------------------------------------------------*
FORM PACK_CTYPE_TO_PTYPE USING P_LINDA.
  DATA: L_TEXT1(3),
        L_TEXT2(8).
*
  SEARCH P_LINDA FOR ','.
  IF SY-SUBRC = 0.
    SPLIT P_LINDA AT ',' INTO L_TEXT1 L_TEXT2.
    CLEAR P_LINDA.
    CONCATENATE L_TEXT1 L_TEXT2 INTO P_LINDA.
  ENDIF.
ENDFORM.                    " pack_ctype_to_ptype
*&---------------------------------------------------------------------*
*&      Form  modify_taxes_lgtxt
*&---------------------------------------------------------------------*
FORM MODIFY_TAXES_LGTXT.
  DATA: L_TEXT1(10),
        L_TEXT2(10),
        L_TABIX      LIKE SY-TABIX.
  DATA: TMP_TAXDN LIKE IT_TAXDN OCCURS 0 WITH HEADER LINE.
*
  CLEAR IT_TAXDN.
  LOOP AT IT_TAXDN WHERE LGART = SPACE.
    CLEAR: L_TEXT1, L_TEXT2.
    L_TEXT1 = IT_TAXDN-LGTXT+1(9).
    L_TEXT2 = IT_TAXDN-LGTXT+11(10).
    CLEAR IT_TAXDN-LGTXT.
    IF L_TEXT1 = SPACE.
      IT_TAXDN-LGTXT = L_TEXT2.
    ELSE.
      CONCATENATE L_TEXT1 L_TEXT2 INTO IT_TAXDN-LGTXT
        SEPARATED BY SPACE.
    ENDIF.
    MODIFY IT_TAXDN.
  ENDLOOP.
*
  CLEAR: TMP_TAXDN, TMP_TAXDN[].
  TMP_TAXDN[] = IT_TAXDN[].
*
  LOOP AT TMP_TAXDN WHERE LGART = SPACE.
    L_TABIX = SY-TABIX + 1.
    CLEAR IT_TAXDN.
    READ TABLE IT_TAXDN INDEX L_TABIX.
    CONCATENATE TMP_TAXDN-LGTXT IT_TAXDN-LGTXT
           INTO IT_TAXDN-LGTXT SEPARATED BY SPACE.
    MODIFY IT_TAXDN INDEX L_TABIX TRANSPORTING LGTXT.
  ENDLOOP.
*
  DELETE IT_TAXDN WHERE LGART = SPACE.
ENDFORM.                    " modify_taxes_lgtxt
*&---------------------------------------------------------------------*
*&      Form  next_number_search
*&---------------------------------------------------------------------*
FORM NEXT_NUMBER_SEARCH.
  CHECK SY-UCOMM = 'NEXT' OR SY-UCOMM = 'PREV'.
*
  DO.
    CASE SY-UCOMM.
      WHEN 'NEXT'. W_PERNR = W_PERNR + 1.
      WHEN 'PREV'. W_PERNR = W_PERNR - 1.
    ENDCASE.
    CLEAR PA0001.
    SELECT SINGLE BEGDA INTO PA0001-BEGDA
      FROM PA0001 WHERE PERNR = W_PERNR
                    AND ENDDA = '99991231'.
    IF SY-SUBRC <> 0.
      W_SUBRC = SY-SUBRC.
      EXIT.
    ENDIF.

    PERFORM GET_PAYROLL_PERIOD.
    PERFORM CHECK_PAYROLL_AREA.
    PERFORM GET_SEQUENTIAL_NUMBER.
    IF W_SUBRC = 0.
      EXIT.
*   else.
*     message i010 with w_pernr.
    ENDIF.
  ENDDO.
*
  CHECK W_SUBRC = 0.
  READ TABLE TMP INDEX 1.
  PERFORM GET_RESULT_TABLE.
  PERFORM GET_RESULT_DATA_BY_FUNCTION.
  PERFORM WRITE_TRUE_LIST.
ENDFORM.                    " next_number_search
*&---------------------------------------------------------------------*
*&      Form  get_payroll_area_last_value
*&---------------------------------------------------------------------*
FORM GET_PAYROLL_AREA_LAST_VALUE.
  CLEAR IT_PERID. REFRESH IT_PERID.
*
  CLEAR T549A.
  SELECT ABKRS INTO T549A-ABKRS FROM T549A WHERE CALCR = 'X'.
    IT_PERID-ABKRS = T549A-ABKRS.
    CALL FUNCTION 'PA03_PERIODDATES_GET'
         EXPORTING
              F_ABKRS               = IT_PERID-ABKRS
         CHANGING
              F_CURRENT_PERIOD      = IT_PERID-MAXRP
              F_CURRENT_YEAR        = IT_PERID-PABRJ
         EXCEPTIONS
              PCR_DOES_NOT_EXITS    = 1
              ABKRS_DOES_NOT_EXIST  = 2
              PERIOD_DOES_NOT_EXITS = 3
              OTHERS                = 4.
    IT_PERID-CURNT = IT_PERID-MAXRP.
    APPEND IT_PERID. CLEAR IT_PERID.
  ENDSELECT.
ENDFORM.                    " get_payroll_area_last_value
*&---------------------------------------------------------------------*
*&      Form  check_payroll_area
*&---------------------------------------------------------------------*
FORM CHECK_PAYROLL_AREA.
  CASE SY-UCOMM.
    WHEN 'EXEC' OR 'ENTR'.
      IT_PERID-CURNT = W_PABRP.
      MODIFY IT_PERID TRANSPORTING CURNT WHERE ABKRS = W_ABKRS.
    WHEN 'NEXT' OR 'PREV'.
      READ TABLE IT_PERID WITH KEY ABKRS = W_ABKRS.
      W_PABRJ = IT_PERID-PABRJ.
      W_PABRP = IT_PERID-CURNT.
      PERFORM GET_PAYROLL_PERIOD.
  ENDCASE.
*
*  read table it_perid with key abkrs = w_abkrs
*                               pabrj = w_pabrj.
*  if sy-subrc = 0.
*    if it_perid-curnt > 0 and it_perid-curnt = w_pabrp.
*    elseif w_pabrp > it_perid-maxrp and it_perid-curnt = 0.
*      w_subrc = 4.
*      message s011 with it_perid-pabrj it_perid-maxrp.
*    else.
*      if it_perid-curnt > 0.
*        case sy-ucomm.
*          when 'EXEC' or 'ENTR'.
*            it_perid-curnt = w_pabrp.
*            modify it_perid transporting curnt where abkrs = w_abkrs.
*          when 'NEXT' or 'PREV'.
*            w_pabrp = it_perid-curnt.
*        endcase.
*        perform get_payroll_period.
*      else.
*        it_perid-curnt = w_pabrp.
*        modify it_perid transporting curnt where abkrs = w_abkrs.
*      endif.
*    endif.
*  else.
*    it_perid-pabrj = w_pabrj.
*    it_perid-curnt = w_pabrp.
*    modify it_perid transporting pabrj curnt where abkrs = w_abkrs.
*  endif.
ENDFORM.                    " check_payroll_area
*&---------------------------------------------------------------------*
*&      Form  collect_earns
*&---------------------------------------------------------------------*
FORM COLLECT_EARNS.
  DATA: CDAT1(12), CDAT2(12).
*
  CLEAR IT_COLET. REFRESH IT_COLET.
  LOOP AT IT_EARNS.
    IT_COLET-LGART = IT_EARNS-LGART.
    IT_COLET-LGTXT = IT_EARNS-LGTXT.
    IF IT_EARNS-CURNT CS '-'.
*      REPLACE '--' WITH SPACE INTO IT_EARNS-CURNT.
      DO 12 TIMES .
        REPLACE '-' WITH SPACE INTO IT_EARNS-CURNT.
      ENDDO.
    ENDIF.
    IF IT_EARNS-YTDAM CS '-'.
*      REPLACE '------------' WITH SPACE INTO IT_EARNS-YTDAM.
      DO 12 TIMES .
        REPLACE '-' WITH SPACE INTO IT_EARNS-YTDAM.
      ENDDO.
    ENDIF.
    SEARCH IT_EARNS-CURNT FOR ','.
    IF SY-SUBRC = 0.
      CLEAR: CDAT1, CDAT2.
      SPLIT IT_EARNS-CURNT AT ',' INTO CDAT1 CDAT2.
      CLEAR IT_EARNS-CURNT.
      CONCATENATE CDAT1 CDAT2 INTO IT_EARNS-CURNT.
    ENDIF.
    SEARCH IT_EARNS-YTDAM FOR ','.
    IF SY-SUBRC = 0.
      CLEAR: CDAT1, CDAT2.
      SPLIT IT_EARNS-YTDAM AT ',' INTO CDAT1 CDAT2.
      CLEAR IT_EARNS-YTDAM.
      CONCATENATE CDAT1 CDAT2 INTO IT_EARNS-YTDAM.
    ENDIF.

* changed by jslee 13/05/2004
*       PACK IT_EARNS-CURNT TO IT_COLET-CURNT.
*       PACK IT_EARNS-YTDAM TO IT_COLET-YTDAM.

    CATCH SYSTEM-EXCEPTIONS CONVERSION_ERRORS = 1.
      PACK IT_EARNS-CURNT TO IT_COLET-CURNT.
    ENDCATCH.
    IF SY-SUBRC EQ 1.
      MESSAGE I001(ZMHR) WITH 'Error: CONVT_NO_NUMBER '.
    ENDIF.
    CATCH SYSTEM-EXCEPTIONS CONVERSION_ERRORS = 1.
      PACK IT_EARNS-YTDAM TO IT_COLET-YTDAM.
    ENDCATCH.

    IF SY-SUBRC EQ 1.
      MESSAGE I001(ZMHR) WITH 'Error: CONVT_NO_NUMBER '.
    ENDIF.
* end of changed.

    COLLECT IT_COLET. CLEAR IT_COLET.
  ENDLOOP.
*
* it_earns-curnt = it_earns-ytdam = 0.
* modify it_earns transporting curnt ytdam where lgart ne space.
*
* sort it_earns by lgart.
* delete adjacent duplicates from it_earns.
*
* loop at it_colet.
*   it_earns-curnt = it_colet-curnt.
*   it_earns-ytdam = it_colet-ytdam.
*   modify it_earns transporting curnt ytdam
*                   where lgart = it_colet-lgart
*                     and lgtxt = it_colet-lgtxt.
* endloop.
ENDFORM.                    " collect_earns
