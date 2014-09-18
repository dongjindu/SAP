************************************************************************
* Provided by Andy Choi
* - 4.6c
* - 2003.3
************************************************************************
REPORT ZR01FIR_TB NO STANDARD PAGE HEADING  MESSAGE-ID AT
                                    LINE-SIZE 120           " Size 120
                                    LINE-COUNT 90.          " 너비 90

TABLES: glfunct,
        glt0,
        VWBEKI,
        T001,                                         " 회사코

        T077Z,
        SKA1, SKAT.

* 임시 Structure
DATA: BEGIN OF s_itab,
        BUKRS        LIKE   glfunct-RBUKRS,              " 회사코드
        RACCT1(04)   TYPE   C,                       " 계정1레벨
        RACCT2(04)   TYPE   C,                       " 계정2레벨
        RACCT3(04)   TYPE   C,                       " 계정3레벨
        RACCT4(04)   TYPE   C,                       " 계정5레벨
        RACCT        LIKE   glfunct-RACCT,              " 계정과목
        GVTYP        LIKE   SKA1-GVTYP,              " P&L?
        txt50        LIKE   SKAT-TXT50,
        TSL_T_I      TYPE   glt0-TSLVT,              " C/F
        TSL_D        TYPE   glt0-TSLVT,              " Debit
        TSL_H        TYPE   glt0-TSLVT,              " Credit
        TSL_D_J      TYPE   glt0-TSLVT,              " YTD Debit
        TSL_H_J      TYPE   glt0-TSLVT,              " YTD Credit
      END   OF s_itab.

* 전계정(Internal Table)
DATA  BEGIN OF itab OCCURS 05.
        INCLUDE STRUCTURE s_itab.
DATA  END   OF itab.

DATA: D-CHK        TYPE  C,
      P_H(25)      TYPE  C,
      P_H_J(25)    TYPE  C,
      P_D(25)      TYPE  C,
      P_D_J(25)    TYPE  C,
      TOTAL_SUM_D_J      TYPE   glt0-TSLVT,
      TOTAL_SUM_H_J      TYPE   glt0-TSLVT,

      D-RPMAX(02)        TYPE  P,
      D_BUTXT            LIKE  T001-BUTXT,
      GS_HAP(30)      TYPE  C,
      GS_TOT(30)      TYPE  C,
      CHK_TITLE          TYPE  C,
      LINE_RACCT(30)     TYPE  C,
      HEADER_LINE(120)   TYPE  C,
      G_C  LIKE T001-WAERS.

data: iska1 like ska1 occurs 0 with header line.

DATA: FNAME(30).
DATA: ZTXT30(30)     TYPE  C.

*SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF   BLOCK BL1
                            WITH  FRAME
                            TITLE TEXT-001.
*SELECTION-SCREEN   SKIP 1.
PARAMETERS:        BUKRS         LIKE  glfunct-RBUKRS  MEMORY ID BUK.
*SELECTION-SCREEN   SKIP 1.
PARAMETERS:        RYEAR         LIKE  glfunct-RYEAR  memory id gjr.
*SELECT-OPTIONS:    S_MON         FOR   glfunct-RPMAX  memory id per.
PARAMETERS:        RPMAX         LIKE  glfunct-RPMAX  memory id per.
*select-options:    zracct         for  glfunct-racct.
SELECTION-SCREEN END   OF   BLOCK BL1.
parameters:  p_bs as checkbox  default ' ',
             p_pl as checkbox  default 'X'.
select-options: s_fa for glfunct-RFAREA.

PARAMETERS : p_file LIKE RLGRAP-FILENAME DEFAULT
  'c:\temp\tb.xls'.

*---------------------------------------------------------------------
*  S T A R T - O F - S E L E C T I O N
*---------------------------------------------------------------------
START-OF-SELECTION.
  SELECT SINGLE * FROM T001
        WHERE   BUKRS  =  BUKRS.
  check sy-subrc = 0.
  D_BUTXT =  T001-BUTXT.
  G_C     =  t001-waers.

  if p_bs = 'X'.
    select * from ska1 into table iska1
      where KTOPL = t001-ktopl
        and XBILK = 'X'.
  endif.
  if p_pl = 'X'.
    select * from ska1 appending table iska1
      where KTOPL = t001-ktopl
        and XBILK = space.

  endif.
  PERFORM glfunct_PROCESS.

END-OF-SELECTION.
  PERFORM OUTPUT_PROCESS.

************************************************************************
***  (SHIFT+PF1) Execute Download
************************************************************************
AT pf13.
  PERFORM data_download.

TOP-OF-PAGE.
  IF  CHK_TITLE = 'A'.
    PERFORM TOP_PROCESS.
  ELSE.
    PERFORM NEXT_PROCESS.
  ENDIF.


TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM NEXT_PROCESS.

AT LINE-SELECTION.
  GET CURSOR FIELD FNAME.
  CASE FNAME.
    WHEN 'LINE_RACCT'.
      PERFORM  SANGSE_PROCESS.
    WHEN 'ZTXT30'.
      SET PARAMETER ID 'SAK' FIELD itab-RACCT.
      SET PARAMETER ID 'BUK' FIELD itab-BUKRS.
      SET PARAMETER ID 'GJR' FIELD RYEAR.
      CALL TRANSACTION 'FS10N'
           AND SKIP first SCREEN.
    WHEN OTHERS.
      MESSAGE I159 WITH '적절한 기능을 선택하세요'.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  glfunct_PROCESS
*&---------------------------------------------------------------------*
FORM glfunct_PROCESS.
  DATA: DTOT     LIKE  glfunct-HSL03.

  REFRESH itab.
  CLEAR   itab.

*  SELECT * FROM glfunct
*    WHERE  BUKRS  =  BUKRS                       " 회사코드
*    AND    RYEAR  =  RYEAR.                      " 회계년도
*    and    racct in zracct.
*    read table iska1 with key saknr = glfunct-racct.
*    check sy-subrc = 0.
*
*    PERFORM SORT_PROCESS.                        " 계산한다.
*  ENDSELECT.

  SELECT * FROM GLFUNCT
    WHERE  RBUKRS  =  BUKRS
    AND    RYEAR   =  RYEAR
    AND    RFAREA  in s_fa.

    read table iska1 with key saknr = glfunct-racct.
    check sy-subrc = 0.

    CLEAR   itab.

    itab-BUKRS  = GLFUNCT-RBUKRS.
    itab-RACCT  = GLFUNCT-RACCT.

    PERFORM SORT_PROCESS using glfunct-racct.
  ENDSELECT.

  LOOP AT itab.
    IF SY-SUBRC <> 0. EXIT. ENDIF.
    CLEAR: itab-TSL_D_J, itab-TSL_H_J.

    IF  itab-TSL_T_I  > 0.
      itab-TSL_D = itab-TSL_D + itab-TSL_T_I.
    ELSE.
      itab-TSL_H = itab-TSL_H + itab-TSL_T_I.
    ENDIF.

    DTOT = itab-TSL_D + itab-TSL_H.
    IF   DTOT > 0.
      itab-TSL_D_J = DTOT.
    ELSE.
      itab-TSL_H_J = ABS( DTOT ).
    ENDIF.

    itab-TSL_H = itab-TSL_H * -1.

    Perform GET_GL_NAME.
    MODIFY itab.
  ENDLOOP.

ENDFORM.                    " glfunct_PROCESS

*&---------------------------------------------------------------------*
*&      Form  OUTPUT_PROCESS
*&---------------------------------------------------------------------*
FORM OUTPUT_PROCESS.
  CLEAR: TOTAL_SUM_D_J, TOTAL_SUM_H_J.

  CHK_TITLE = 'A'.
  SORT itab BY RACCT1 RACCT2 RACCT3 RACCT4 RACCT.

  LOOP AT itab.
    IF SY-SUBRC <> 0. EXIT. ENDIF.

    AT END OF RACCT3.
      SUM.
      PERFORM LINE_PROCESS.
    ENDAT.

    AT END OF RACCT2.
      SUM.
      PERFORM HAP_PROCESS.
    ENDAT.

    AT END OF RACCT1.
      SUM.
      WRITE:/ SY-ULINE(120).
      PERFORM TOT_PROCESS.
      WRITE:/ SY-ULINE(120).
      SKIP.
    ENDAT.

    AT LAST.
      SUM.
      SKIP 2.
      WRITE:/ SY-ULINE(120).
      PERFORM GRAND_TOTAL.
      WRITE:/ SY-ULINE(120).
    ENDAT.

  ENDLOOP.


ENDFORM.                    " OUTPUT_PROCESS

*&---------------------------------------------------------------------*
*&      Form  LINE_PROCESS
*&---------------------------------------------------------------------*
FORM LINE_PROCESS.
  DATA: DTOT     LIKE  glfunct-TSLVT.
  CLEAR: itab-TSL_D_J, itab-TSL_H_J.

  FORMAT RESET.
  FORMAT   INTENSIFIED  OFF.

  PERFORM  GET_LINE_NAME USING itab-RACCT3 line_racct.

  DTOT   =  itab-TSL_D - itab-TSL_H.
  IF     DTOT > 0.
    itab-TSL_D_J  = DTOT.
  ELSE.
    itab-TSL_H_J  = ABS( DTOT ).
  ENDIF.

  WRITE:/ LINE_RACCT     LEFT-JUSTIFIED  HOTSPOT COLOR COL_KEY,
          itab-TSL_D    CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_NORMAL,
          itab-TSL_H    CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_NORMAL,
          itab-TSL_D_J  CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_NORMAL,
          itab-TSL_H_J  CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_NORMAL.
  HIDE:  itab-RACCT3.
  Clear: itab-RACCT3.
  CLEAR SY-SUBRC.
ENDFORM.                    " LINE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SANGSE_PROCESS
*&---------------------------------------------------------------------*
FORM SANGSE_PROCESS.
  DATA: DTOT     LIKE  glfunct-TSLVT.
  CHK_TITLE = 'Z'.
  SORT itab BY RACCT3.

  LOOP AT itab WHERE RACCT3 = itab-RACCT3.
    IF SY-SUBRC <> 0. EXIT. ENDIF.

    FORMAT RESET.
    ZTXT30 = itab-txt50.

    WRITE:/ ZTXT30         CURRENCY g_c
                           LEFT-JUSTIFIED  HOTSPOT COLOR COL_KEY,
            itab-TSL_D    CURRENCY g_c
                           RIGHT-JUSTIFIED COLOR COL_NORMAL,
            itab-TSL_H    CURRENCY g_c
                           RIGHT-JUSTIFIED COLOR COL_NORMAL,
            itab-TSL_D_J  CURRENCY g_c
                           RIGHT-JUSTIFIED COLOR COL_NORMAL,
            itab-TSL_H_J  CURRENCY g_c
                           RIGHT-JUSTIFIED COLOR COL_NORMAL.
    HIDE:  itab-RACCT, itab-TSL_D, itab-TSL_H, itab-BUKRS.
    CLEAR SY-SUBRC.

    AT END OF RACCT3.
      SUM.
      PERFORM SANGSE_SUM_PROCESS.
    ENDAT.

  ENDLOOP.

ENDFORM.                    " SANGSE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  SANGSE_SUM_PROCESS
*&---------------------------------------------------------------------*
FORM SANGSE_SUM_PROCESS.
  DATA: DTOT     LIKE  glfunct-TSLVT.

  FORMAT RESET.

  WRITE:/ 'S U M                         '
                         LEFT-JUSTIFIED  COLOR COL_TOTAL,
          itab-TSL_D    CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_TOTAL,
          itab-TSL_H    CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_TOTAL,
          itab-TSL_D_J  CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_TOTAL,
          itab-TSL_H_J  CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_TOTAL.
  CLEAR SY-SUBRC.

ENDFORM.                    " SANGSE_SUM_PROCESS
*&---------------------------------------------------------------------*
*&      Form  HAP_PROCESS
*&---------------------------------------------------------------------*
FORM HAP_PROCESS.
  DATA: DTOT     LIKE  glfunct-TSLVT.
  CLEAR: itab-TSL_D_J, itab-TSL_H_J.

  FORMAT RESET.
  PERFORM GET_LINE_NAME  USING itab-RACCT2 GS_HAP.

  DTOT   =  itab-TSL_D - itab-TSL_H.
  IF     DTOT > 0.
    itab-TSL_D_J  = DTOT.
  ELSE.
    itab-TSL_H_J  = ABS( DTOT ).
  ENDIF.

  WRITE:/ GS_HAP      CURRENCY g_c
                         LEFT-JUSTIFIED  COLOR COL_TOTAL,
          itab-TSL_D    CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_TOTAL,
          itab-TSL_H    CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_TOTAL,
          itab-TSL_D_J  CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_TOTAL,
          itab-TSL_H_J  CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_TOTAL.
  CLEAR SY-SUBRC.

ENDFORM.                    " HAP_PROCESS

*&---------------------------------------------------------------------*
*&      Form  TOT_PROCESS
*&---------------------------------------------------------------------*
FORM TOT_PROCESS.
  DATA: DTOT     LIKE  glfunct-TSLVT.
  CLEAR: itab-TSL_D_J, itab-TSL_H_J.

  FORMAT RESET.
  PERFORM GET_LINE_NAME  USING itab-RACCT1 GS_TOT.

  DTOT   =  itab-TSL_D - itab-TSL_H.
  IF     DTOT > 0.
    itab-TSL_D_J  = DTOT.
  ELSE.
    itab-TSL_H_J  = ABS( DTOT ).
  ENDIF.

  WRITE:/ GS_TOT      CURRENCY g_c
                         LEFT-JUSTIFIED  COLOR COL_GROUP,
          itab-TSL_D    CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_GROUP,
          itab-TSL_H    CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_GROUP,
          itab-TSL_D_J  CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_GROUP,
          itab-TSL_H_J  CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_GROUP.
  CLEAR SY-SUBRC.

ENDFORM.                    " TOT_PROCESS
*&---------------------------------------------------------------------*
*&      Form  TOP_PROCESS
*&---------------------------------------------------------------------*
FORM TOP_PROCESS.
  FORMAT   INTENSIFIED  OFF.
  SKIP 2.
  WRITE:   'Account Balance'            TO  HEADER_LINE.
  WRITE:/  HEADER_LINE CENTERED.
  WRITE:   '========================'  TO  HEADER_LINE.
  WRITE:/  HEADER_LINE CENTERED.

  SKIP 1.
  D-RPMAX = RPMAX.
  WRITE:/   'Year:', RYEAR.
  WRITE: 52 'Period:' NO-GAP, D-RPMAX NO-GAP, TEXT-007,
        100 'User:', sy-uname.

  WRITE:/  'Date:', SY-DATUM  DD/MM/YYYY,
        52 'Company:', BUKRS, D_BUTXT,
       100 'Time:', SY-UZEIT  USING EDIT MASK '__:__:__'.


  WRITE:/  SY-ULINE(120).              " 밑줄짝..
  WRITE:/
           'Account                       ',
           'Debit                '  RIGHT-JUSTIFIED,
           'Credit               '  RIGHT-JUSTIFIED,
           'Debit Total          '  RIGHT-JUSTIFIED,
           'Credit Total         '  RIGHT-JUSTIFIED.
  WRITE:/  SY-ULINE(120).              " 밑줄짝..



ENDFORM.                    " TOP_PROCESS

*&---------------------------------------------------------------------*
*&      Form  NEXT_PROCESS
*&---------------------------------------------------------------------*
FORM NEXT_PROCESS.
  FORMAT   INTENSIFIED  OFF.
  WRITE:/  SY-ULINE(120).              " 밑줄짝..
  WRITE:/  'Account                       ',
           'Debit                '  RIGHT-JUSTIFIED,
           'Credit               '  RIGHT-JUSTIFIED,
           'Debit Total          '  RIGHT-JUSTIFIED,
           'Credit Total         '  RIGHT-JUSTIFIED.


  WRITE:/  SY-ULINE(120).              " 밑줄짝..

ENDFORM.                    " NEXT_PROCESS
*&---------------------------------------------------------------------*
*&      Form  GRAND_TOTAL
*&---------------------------------------------------------------------*
FORM GRAND_TOTAL.
  DATA: DTOT     LIKE  glfunct-TSLVT,
        RXT30    LIKE  T077Z-TXT30,
        RRR(04)  TYPE  C.

  MOVE   'S U M'               TO  RXT30.

  FORMAT RESET.


  WRITE:/ RXT30          CURRENCY g_c
                         CENTERED        COLOR COL_KEY,
          itab-TSL_D    CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_KEY,
          itab-TSL_H    CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_KEY,
          TOTAL_SUM_D_J  CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_KEY,
          TOTAL_SUM_H_J  CURRENCY g_c
                         RIGHT-JUSTIFIED COLOR COL_KEY.
  CLEAR SY-SUBRC.

ENDFORM.                    " GRAND_TOTAL
*&---------------------------------------------------------------------*
*&      Form  data_download
*&---------------------------------------------------------------------*
FORM data_download.
* EDIT_TABLE_WITH_EXCEL
* SEND_TABLE_TO_EXCEL

  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename = p_file
            filetype = 'WK1'
       TABLES
            data_tab = itab.

  write:/ p_file, ' is created...'.
ENDFORM.                    " data_download

*&---------------------------------------------------------------------*
*&      Form  GET_GL_NAME
*&---------------------------------------------------------------------*
FORM GET_GL_NAME.
  SELECT SINGLE * FROM SKAT
    WHERE SPRAS   =  sy-langu
    AND   KTOPL   =  t001-ktopl
    AND   SAKNR   =  itab-RACCT.
  IF SY-SUBRC = 0.
    MOVE SKAT-TXT50  TO  itab-txt50.
  ELSE.
    MOVE '*'         TO  itab-txt50.
  ENDIF.
ENDFORM.                    " GET_GL_NAME
*&---------------------------------------------------------------------*
*&      Form  calc_period_amount
*&---------------------------------------------------------------------*
FORM calc_period_amount.

* P&L
  if iska1-GVTYP = 'X'.
    if glfunct-DRCRK = 'S'.
      case rpmax.
        WHEN  1.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL01.
        WHEN  2.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL02.
        WHEN  3.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL03.
        WHEN  4.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL04.
        WHEN  5.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL05.
        WHEN  6.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL06.
        WHEN  7.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL07.
        WHEN  8.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL08.
        WHEN  9.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL09.
        WHEN 10.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL10.
        WHEN 11.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL11.
        WHEN 12.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL12.
      endcase.
    else.
      case rpmax.
        WHEN  1.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL01.
        WHEN  2.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL02.
        WHEN  3.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL03.
        WHEN  4.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL04.
        WHEN  5.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL05.
        WHEN  6.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL06.
        WHEN  7.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL07.
        WHEN  8.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL08.
        WHEN  9.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL09.
        WHEN 10.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL10.
        WHEN 11.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL11.
        WHEN 12.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL12.
      ENDCASE.
    endif.
* B/S
  else.
    DO  12 TIMES.
      CHECK  SY-INDEX  <=  RPMAX.        " 속하는지를 체크
*      CHECK  SY-INDEX  <=  S_MON-HIGH.

* if balance item, collect from c/f
* else, just month...

      CASE  glfunct-DRCRK.                    " 차/대지시자기
        WHEN  'S'.                         " 차변
*     itab-TSL_T_I = itab-TSL_T_I + glfunct-HSLVT.
          itab-TSL_T_I = glfunct-HSLVT.
          CASE SY-INDEX.
            WHEN  1.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL01.
            WHEN  2.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL02.
            WHEN  3.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL03.
            WHEN  4.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL04.
            WHEN  5.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL05.
            WHEN  6.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL06.
            WHEN  7.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL07.
            WHEN  8.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL08.
            WHEN  9.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL09.
            WHEN 10.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL10.
            WHEN 11.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL11.
            WHEN 12.  itab-TSL_D   =  itab-TSL_D   +  glfunct-HSL12.
          ENDCASE.
        WHEN  'H'.                         " 대변
*     itab-tsl_t_i = itab-tsl_t_i + glfunct-hslvt.
          itab-TSL_T_I = glfunct-HSLVT.
          CASE SY-INDEX.
            WHEN  1.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL01.
            WHEN  2.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL02.
            WHEN  3.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL03.
            WHEN  4.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL04.
            WHEN  5.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL05.
            WHEN  6.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL06.
            WHEN  7.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL07.
            WHEN  8.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL08.
            WHEN  9.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL09.
            WHEN 10.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL10.
            WHEN 11.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL11.
            WHEN 12.  itab-TSL_H   =  itab-TSL_H   + glfunct-HSL12.
          ENDCASE.
      ENDCASE.
    ENDDO.
  endif.

ENDFORM.                    " calc_period_amount
*&---------------------------------------------------------------------*
*&      Form  SORT_PROCESS
*&---------------------------------------------------------------------*
FORM SORT_PROCESS  using f_acct.
  DATA: DTOT     LIKE  glfunct-HSL03,
        l_acct(10) type c.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = f_acct
       IMPORTING
            output = l_ACCT
       EXCEPTIONS
            OTHERS = 1.

*  itab-RACCT1  =  L_ACCT(01).              " 계정과목-1레벨
*  itab-RACCT2  =  L_ACCT(02).              " 계정과목-2레벨
*  itab-RACCT3  =  L_ACCT(03).              " 계정과목-3레벨
  itab-RACCT4  =  L_ACCT(04).              " 계정과목-5레벨


  case itab-RACCT4.
    when '5406'. itab-RACCT4 = '541'. "Subcontract
  endcase.

  itab-racct3 = itab-racct4(3).
  case itab-RACCT3.
    when '114'. itab-RACCT3 = '113'. "Cash in bank
    when '115'. itab-RACCT3 = '113'. "Cash in bank
    when '116'. itab-RACCT3 = '113'. "Cash in bank
    when '117'. itab-RACCT3 = '113'. "Cash in bank
    when '118'. itab-RACCT3 = '113'. "Cash in bank
    when '119'. itab-RACCT3 = '113'. "Cash in bank

    when '123'. itab-RACCT3 = '122'. "Cust.Recv.
    when '127'. itab-RACCT3 = '126'. "OtherRecv.

    when '132'. itab-RACCT3 = '131'. "Inventory
    when '133'. itab-RACCT3 = '131'. "Inventory
    when '134'. itab-RACCT3 = '131'. "Inventory
    when '135'. itab-RACCT3 = '131'. "Inventory
    when '136'. itab-RACCT3 = '131'. "Inventory
    when '137'. itab-RACCT3 = '131'. "Inventory

    when '212'. itab-RACCT3 = '211'. "Trade Payable

    when '222'. itab-RACCT3 = '221'. "TradePay-long
    when '223'. itab-RACCT3 = '221'. "TradePay-long

    when '510'. itab-RACCT3 = '451'. "cogs-domestic
    when '520'. itab-RACCT3 = '452'. "cogs-foreign
    when '530'. itab-RACCT3 = '459'. "cogs-variance
*    when '520'. itab-RACCT3 = '990'. "Inventory

    when '590'. itab-RACCT3 = '890'. "factory out
    when '690'. itab-RACCT3 = '990'. "FICO Recon
*   when '901'. itab-RACCT3 = '900'. "Zero
  endcase.

  itab-racct2 = itab-racct3(2).
* Reclass
  case itab-RACCT2.
    when '15 '. itab-RACCT2 = '14 '. "other.asset

    when '41 '. itab-RACCT2 = '40 '. "sales
    when '42 '. itab-RACCT2 = '40 '. "sales

*   when '61 '. itab-RACCT2 = '60 '. "sales expense
    when '62 '. itab-RACCT2 = '61 '. "sales expense

    when '71 '. itab-RACCT2 = '70 '. "other.inc/exp
    when '75 '. itab-RACCT2 = '70 '. "other.inc/exp
  endcase.


  itab-racct1 = itab-racct2(1).
  perform calc_period_amount.

  COLLECT itab.
ENDFORM.                    " SORT_PROCESS
*&---------------------------------------------------------------------*
*&      Form  GET_LINE_NAME
*&---------------------------------------------------------------------*
FORM GET_LINE_NAME USING L_RACCT L_LINE.
  CASE L_RACCT.
    when  '1  '.  l_line = 'Asset                '.
    when  '11 '.  l_line = 'Cash & Equiv         '.
    when  '111'.  l_line = 'Petty Cash           '.
    when  '113'.  l_line = 'Cash in bank         '.         "113~119
    when  '118'.  l_line = 'Marketable Securities'.
    when  '12 '.  l_line = 'Cust.Receiv          '.
    when  '121'.  l_line = 'Cust.Receiv-Domestic '.         "122,123
    when  '122'.  l_line = 'Cust.Receiv-Foreign  '.         "122,123
    when  '124'.  l_line = 'Allw for doubtful acc'.
    when  '112'.  l_line = 'Loans                '.
    when  '126'.  l_line = 'Other Receivables    '.  "127
    when  '13 '.  l_line = 'Inventory            '.
    when  '131'.  l_line = 'Inventory            '.         "131~137
    when  '138'.  l_line = 'Material in Transit  '.
    when  '139'.  l_line = 'Machinery in Transit '.
    when  '14 '.  l_line = 'Other Current Asset  '.         "14-15
    when  '141'.  l_line = 'Accrued Income       '.
    when  '151'.  l_line = 'Down Payment         '.
    when  '152'.  l_line = 'PrePayments          '.
    when  '153'.  l_line = 'Advances             '.
    when  '16 '.  l_line = 'Property & Plant     '.
    when  '160'.  l_line = 'Property & Plant     '.
    when  '169'.  l_line = 'AuC                  '.
    when  '17 '.  l_line = 'Acc.Dep on Property  '.
    when  '170'.  l_line = 'Acc.Dep on Property  '.
    when  '18 '.  l_line = 'Investments          '.
    when  '180'.  l_line = 'Investments          '.
    when  '19 '.  l_line = 'Other Longterm Asset '.
    when  '190'.  l_line = 'Long term receiv     '.
    when  '191'.  l_line = 'Long term loans      '.
    when  '192'.  l_line = 'Long term other rec. '.
    when  '193'.  l_line = 'Intangible asset     '.
    when  '194'.  l_line = 'Amort.intngbl asset  '.
    when  '195'.  l_line = 'Deffered Income Tax  '.
    when  '2  '.  l_line = 'Liabilities          '.
    when  '21 '.  l_line = 'Current Liabilities  '.
    when  '211'.  l_line = 'Trade Payable        '.         "211,212
    when  '213'.  l_line = 'Intercompany Payable '.
    when  '214'.  l_line = 'Loans                '.
    when  '215'.  l_line = 'Other Payables       '.
    when  '216'.  l_line = 'Benefit Accrued      '.
    when  '217'.  l_line = 'Accrued liability    '.
    when  '218'.  l_line = 'Allowance            '.
    when  '22 '.  l_line = 'Non Current liability'.
    when  '221'.  l_line = 'Trade payable - long '.         "221-223
    when  '224'.  l_line = 'Other Payable - long '.
    when  '225'.  l_line = 'Loans - long         '.
    when  '226'.  l_line = 'Deferred liability   '.
    when  '227'.  l_line = 'Deferred tax liab.   '.
    when  '229'.  l_line = 'Other liability      '.
    when  '3  '.  l_line = 'Equity               '.
    when  '31 '.  l_line = 'Capital              '.
    when  '310'.  l_line = 'Capital              '.
    when  '32 '.  l_line = 'Retained Earnings    '.
    when  '320'.  l_line = 'Retained Earnings    '.
    when  '4  '.  l_line = 'Sales Margin         '.
    when  '40 '.  l_line = 'Revenue              '.
    when  '410'.  l_line = 'Domestic Sales       '.
    when  '420'.  l_line = 'Foreign Sales        '.
    when  '45 '.  l_line = 'Cost of Goods Sold   '.
    when  '451'.  l_line = 'COGS-Domestic Sales  '.
    when  '452'.  l_line = 'COGS-Foreign Sales   '.
    when  '459'.  l_line = 'COGS-Variance        '.

    when  '5  '.  l_line = 'Material Expense     '.
    when  '54 '.  l_line = 'Material Expense     '.
    when  '540'.  l_line = 'Material Expense     '.
    when  '541'.  l_line = 'Subcontract Expense  '.

    when  '6  '.  l_line = 'Expense    '.
    when  '60 '.  l_line = 'Operating Expense    '.
    when  '601'.  l_line = 'Salary & Wages       '.
    when  '602'.  l_line = 'Insurance & Tax      '.
    when  '603'.  l_line = 'General Expense      '.
    when  '604'.  l_line = 'Consulting Fee       '.
    when  '605'.  l_line = 'Traning & Develop    '.
    when  '606'.  l_line = 'Logistics            '.
    when  '607'.  l_line = 'Repair & Maintenance '.
    when  '608'.  l_line = 'Production Expense   '.
    when  '609'.  l_line = 'Depreciation         '.

    when  '61 '.  l_line = 'Sales Expense        '.
    when  '610'.  l_line = 'Bad debit expense    '.
    when  '620'.  l_line = 'AD & Promotion       '.
    when  '7  '.  l_line = 'Other Income,Expense '.
    when  '70 '.  l_line = 'Other Income,Expense '.
    when  '711'.  l_line = 'Other Income         '.
    when  '712'.  l_line = 'Other Expense        '.
    when  '750'.  l_line = 'Corporate Tax        '.

    when  '8  '.  l_line = 'Manufacturing Cost   '.
    when  '890'.  l_line = 'Inventory Changes    '.

    when  '9  '.  l_line = 'Zero, AuC Clearing   '.
    when  '90 '.  l_line = 'Zero, AuC Clearing   '.
    when  '900'.  l_line = 'Zero-Balance         '.
    when  '901'.  l_line = 'AuC Clearing         '.
    when  others. l_line = '*'.
  ENDCASE.
ENDFORM.
