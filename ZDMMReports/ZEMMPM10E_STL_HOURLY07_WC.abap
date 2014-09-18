************************************************************************
* Program Name      : ZEMMPM10E_STL_06
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.04.23.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K909855
* Addl Documentation:
* Description       : Supply to Line - RP06
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.04.23.     Sung-Tae Lim     UD1K909855     Initial Coding
*
*
************************************************************************

REPORT ZEMMPM10E_STL_06 NO STANDARD PAGE HEADING
                        LINE-SIZE 400
                        MESSAGE-ID ZMMM.

**---
INCLUDE : ZRMMPMXXR_INCL.

**---
DATA : W_SUBRC   LIKE SY-SUBRC.

*--- Working time
* Global variable
DATA:  W_KALID   LIKE KAKO-KALID,                  "Calender ID
       W_MOSID   LIKE KAKO-MOSID,                  "Schedule group
       W_KAPID   LIKE KAKO-KAPID.                  "Capacity ID

* Internal tables
DATA: BEGIN OF IT_WORK_DATE OCCURS 0,                "Work day
        DATE   TYPE   D,
        DAYNR  LIKE   HRVSCHED-DAYNR,
      END   OF IT_WORK_DATE.

DATA: IT_BREAK LIKE TC37P OCCURS 0 WITH HEADER LINE. "Break time

* Global variables
DATA: WA_DATUM LIKE SY-DATUM.


* Ranges
RANGES: RG_PAPLAN FOR TC37A-PAPLAN.                 "Break ID

*--- For BDC message
DATA : IT_BDCMSGCOLL LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       WA_BDCMSGCOLL LIKE LINE OF IT_BDCMSGCOLL.

* Variable for date & time
DATA : W_DAYTIME(14).
DATA : W_CURRENT_DATE   TYPE SY-DATUM.
DATA : W_CURRENT_TIME   TYPE SY-UZEIT.
DATA : W_SCREEN_DATE    TYPE SY-DATUM.     " TO Start Date
DATA : W_SCREEN_TIME    TYPE SY-UZEIT.
DATA : W_DUMMY_DATE     TYPE SY-DATUM.  "Dummy date for temp use
DATA : W_DUMMY_MINUTES  TYPE NUM03.  "Dummy minutes for Cal of Start D&T

* Constants
CONSTANTS: C_ARBPL   LIKE CRHD-ARBPL VALUE 'T',     "Trim line shop name
           C_PADAUER LIKE TC37P-PADAUER VALUE '1200'. "break criterion
**---
* Itab & WA for Create TO(/nLT01)
DATA : BEGIN OF WA_TOLINE,
         MATNR        LIKE ZTMM_MAST-MATNR,
         BDMNG        LIKE RESB-BDMNG,
         MEINS        LIKE RESB-MEINS,
         FEEDR        LIKE ZTMM_MAST-FEEDR,
         WORKS        LIKE ZTMM_MAST-WORKS,
         RH_LH        LIKE ZTMM_MAST-RH_LH,
         SDATE        TYPE D,        "Start Date
         FEEDING_TIME TYPE SY-UZEIT, "Start Time
         EDATE        TYPE D,        "End Date
         ETIME        TYPE T,        "End Time
         STOCK_CHECK  LIKE ZTMM_MAST-STOCK_CHECK,
         FEED_CYCLE   LIKE ZTMM_MAST-FEED_CYCLE,
         ZTIME        LIKE ZTMM_MAST-ZTIME, "Time for STL
         VERME        LIKE LQUA-VERME,      "Available stock
         LPMIN        LIKE ZTMM_MAST-LPMIN,     " safety stock
         OPEN_TO      LIKE LTAP-VSOLA,      "Open TO
         BFERRORQTY   LIKE AFFW-ERFMG,      "Backkflush Error Qty
         BF_TO_QTY    LIKE PPC1_ALL-KOMP_QUANT,   "to Backflush Qty.
         RDMNG        LIKE MLGT-RDMNG,      "Rounding Qty
         SRC_LGTYP    LIKE MLGT-LGTYP,      "Source Storage type
         SRC_LGPLA    LIKE MLGT-LGPLA,      "Source Storage bin
         DES_LGTYP    LIKE PKHD-LGTYP,      "Destination Storage type
         DES_LGPLA    LIKE PKHD-LGPLA,      "Destination Storage bin
         TQTY         LIKE RESB-BDMNG,      "Target Qty
         DISPO        LIKE ZTMM_MAST-DISPO,
         ZMNMX        LIKE ZTMM_MAST-ZMNMX, " Min or Max
       END OF WA_TOLINE.

DATA : IT_TOLINE LIKE WA_TOLINE OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF IT_ITAB OCCURS 0.
        INCLUDE STRUCTURE IT_TOLINE.
DATA :   TANUM LIKE LTAP-TANUM,     " TO number
         W_DOCNO TYPE NUM10,
         LINECOLOR(4),     " ALV Color
         MESSA(80),
         MSGTY LIKE ZTMM_STL_LOG-MSGTY,
         MSGID LIKE ZTMM_STL_LOG-MSGID,
         MSGNR LIKE ZTMM_STL_LOG-MSGNR,
         NSOLA LIKE LTAP-NSOLA,
         STATS LIKE ZTMM_STL_LOG-STATS,
       END OF IT_ITAB.


* Odd Even Flag
DATA : W_ODD_EVEN TYPE I.

**---
**** Constants&Vars for Number range object ****************************
CONSTANTS : C_NRO_NR_00   VALUE '00' LIKE INRI-NRRANGENR, "Header Part
            C_NRO_NR_01   VALUE '01' LIKE INRI-NRRANGENR, "Item Part
            C_NRO_NR_09   VALUE '09' LIKE INRI-NRRANGENR, "App. Doc no.
            C_ONEHOUR   TYPE T VALUE '010000'.

*--- Number range object
DATA : W_NRO_OBJECT  VALUE 'ZMMNRO0002' LIKE INRI-OBJECT. "NRO for ZTLOG

*--- Number_Get_Next
DATA :      W_NRO_NUMBER  TYPE NUM10.      " Same type of w_nro_object

DATA : W_ZDOCNO TYPE NUM10.       "App. Doc. No.

*--- scheduled backflush quantity
DATA : BEGIN OF IT_BFQTY_TEMP OCCURS 0,
         MATNR LIKE ZTMM_PPC1_ALL-MATNR,
         WERKS LIKE ZTMM_PPC1_ALL-WERKS,
         LGORT LIKE ZTMM_PPC1_ALL-LGORT,
         KOMP_QUANT LIKE ZTMM_PPC1_ALL-KOMP_QUANT,
         FLG_ASYNCH LIKE ZTMM_PPC1_ALL-FLG_ASYNCH,
         SYNC_IND LIKE ZTMM_PPC1_ALL-SYNC_IND,
       END OF IT_BFQTY_TEMP.

DATA : IT_BACK LIKE IT_BFQTY_TEMP OCCURS 0 WITH HEADER LINE.

*--- current stock
DATA : BEGIN OF IT_STOCK_TEMP OCCURS 0,
         MATNR LIKE LQUA-MATNR,
         GESME LIKE LQUA-GESME,
         LGTYP LIKE PKHD-LGTYP,
         LGPLA LIKE PKHD-LGPLA,
       END OF IT_STOCK_TEMP.

DATA : IT_STOCK LIKE IT_STOCK_TEMP OCCURS 0 WITH HEADER LINE.

*--- open TO quantity
DATA : BEGIN OF IT_OPEN_TEMP OCCURS 0,
         MATNR LIKE LTAP-MATNR,
         VSOLA LIKE LTAP-VSOLA,
       END OF IT_OPEN_TEMP.

DATA : IT_OPEN LIKE IT_OPEN_TEMP OCCURS 0 WITH HEADER LINE.

*--- backflush error quantity
DATA : BEGIN OF IT_BFERROR_TEMP OCCURS 0,
         MATNR LIKE AFFW-MATNR,
         ERFMG LIKE AFFW-ERFMG,
         BWART LIKE AFFW-BWART,
         WERKS LIKE AFFW-WERKS,
         LGORT LIKE AFFW-LGORT,
       END OF IT_BFERROR_TEMP.

DATA : IT_BFERROR LIKE IT_BFERROR_TEMP OCCURS 0 WITH HEADER LINE.

*--- rounding quantity
DATA : BEGIN OF IT_ROUND_TEMP OCCURS 0,
         MATNR LIKE MLGT-MATNR,
         RDMNG LIKE MLGT-RDMNG,
       END OF IT_ROUND_TEMP.

DATA : IT_ROUND LIKE IT_ROUND_TEMP OCCURS 0 WITH HEADER LINE.

*--- source storage bin/type
DATA : BEGIN OF IT_SOURCE OCCURS 0,
         MATNR LIKE MLGT-MATNR,
         LGTYP LIKE MLGT-LGTYP,
         LGPLA LIKE MLGT-LGPLA,
       END OF IT_SOURCE.

*--- destination storage bin/type
DATA : BEGIN OF IT_DEST OCCURS 0,
         MATNR LIKE PKHD-MATNR,
         LGTYP LIKE PKHD-LGTYP,
         LGPLA LIKE PKHD-LGPLA,
       END OF IT_DEST.

*--- Standard Work time
DATA: BEGIN OF IT_WORK_TIME OCCURS 0,
        KAPID   LIKE   KAPA-KAPID,
        VERSN   LIKE   KAPA-VERSN,
        TAGNR   LIKE   KAPA-TAGNR,            "Day no
        SCHNR   LIKE   KAPA-SCHNR,            "Shift No
        DATUB   LIKE   KAPA-DATUB,            "Valid from
        DATUM   LIKE   SY-DATUM,              "Working date
        TPROG   LIKE   KAPA-TPROG,            "Shift
        PAPLAN  LIKE   TC37A-PAPLAN,          "Break schedule
        BEGZT   LIKE   TC37A-BEGZT,           "Start time
        ENDZT   LIKE   TC37A-ENDZT,           "Finish time
      END   OF IT_WORK_TIME.

*----- Work time(1T info)

*--- insert by stlim (2004/07/28)
DATA : IT_1T LIKE ZSMM_WORKING_TIME_FOR_1T OCCURS 0 WITH HEADER LINE.
DATA : IT_DUMMY LIKE ZSMM_WORKING_TIME OCCURS 0 WITH HEADER LINE.

CONSTANTS : C_DAY TYPE I VALUE 10.
*--- end of insert

*--- blocked by stlim (2004/07/28)
*DATA: BEGIN OF it_1t OCCURS 0,
*        datum   LIKE   sy-datum,              "Working date
*        index   TYPE   i,                     "index
*        tprog   LIKE   kapa-tprog,            "Shift
*        begzt(14),                            "Start time
*        endzt(14),                            "Finish time
*        second  TYPE   i,                     "Second
*        fdate   LIKE   sy-datum,              "Feed date
*        timef(14),                            "Feed time from
*        timet(14),                            "Feed time to
*        flag,
*      END   OF it_1t.
*--- end of block


*--- MRP controller
DATA : BEGIN OF IT_DISPO OCCURS 0,
         DISPO LIKE MARC-DISPO,
       END OF IT_DISPO.

DATA : W_MINUS_STOCK_CHECK(1).

DATA : IT_ZTMM_STL_TIME LIKE ZTMM_STL_TIME OCCURS 0 WITH HEADER LINE,
       W_TABIX LIKE SY-TABIX,
       W_LINES TYPE I.


**---
CONSTANTS : C_TIME_073000 TYPE T VALUE '073000',
            C_TIME_083000 TYPE T VALUE '083000',
            C_TIME_093000 TYPE T VALUE '093000',
            C_TIME_111500 TYPE T VALUE '111500',
            C_TIME_121500 TYPE T VALUE '121500',
            C_TIME_131500 TYPE T VALUE '131500',
            C_TIME_141500 TYPE T VALUE '141500',
            C_TIME_151500 TYPE T VALUE '151500',
            C_TIME_161500 TYPE T VALUE '161500',
            C_TIME_171500 TYPE T VALUE '171500',
            C_TIME_181500 TYPE T VALUE '181500',
            C_TIME_191500 TYPE T VALUE '191500',
            C_TIME_201500 TYPE T VALUE '201500',
            C_TIME_220000 TYPE T VALUE '220000',
            C_TIME_230000 TYPE T VALUE '230000',
            C_TIME_000000 TYPE T VALUE '000000',
            C_TIME_010000 TYPE T VALUE '010000',
            C_TIME_020000 TYPE T VALUE '020000',
            C_TIME_030000 TYPE T VALUE '030000',
            C_TIME_040000 TYPE T VALUE '040000',
            C_TIME_063000 TYPE T VALUE '063000'.

DATA : W_MAX_EXEC_DATETIME LIKE ZTMM_STL_EXEC-DTIME,
       W_MAX_EXEC_TIME TYPE T.
DATA : W_STARTING_TIME LIKE ZTMM_STL_TIME-STIME,
       W_ENDING_TIME   LIKE ZTMM_STL_TIME-ETIME,
       W_ENDING_DATE   LIKE SY-DATUM.
DATA : W_1SHIFT_OVERTIME(1),
       W_2SHIFT_OVERTIME(1).

DATA : IT_WORKTIME LIKE ZSMM_WORKTIME OCCURS 0 WITH HEADER LINE.

DATA : W_PRE_S_DATE TYPE D,
       W_PRE_S_TIME TYPE T,
       W_PRE_E_DATE TYPE D,
       W_PRE_E_TIME TYPE T,
       W_NSOLA LIKE LTAP-NSOLA.

CONSTANTS : C_MIN LIKE ZTMM_MAST-ZMNMX VALUE 'MIN',
            C_MZX LIKE ZTMM_MAST-ZMNMX VALUE 'MAX'.

**--- Macro
DEFINE APPEND_FIELDCAT.
  &1 = &1 + 1.
  W_FIELDCAT-COL_POS    = &1.
  W_FIELDCAT-FIELDNAME  = &2.
  W_FIELDCAT-OUTPUTLEN  = &3.
  W_FIELDCAT-SELTEXT_L  = &4.
  W_FIELDCAT-SELTEXT_M  = &4.
  W_FIELDCAT-SELTEXT_S  = &4.
  W_FIELDCAT-DATATYPE   = &5.
  W_FIELDCAT-KEY        = &6.
  W_FIELDCAT-QFIELDNAME = &7.
  W_FIELDCAT-CFIELDNAME = &8.
  APPEND W_FIELDCAT.
  CLEAR : W_FIELDCAT.
END-OF-DEFINITION.

DEFINE APPEND_SORTCAT.
  W_SORTCAT-SPOS      = &1.
  W_SORTCAT-FIELDNAME = &2.
  W_SORTCAT-TABNAME   = &3.
  W_SORTCAT-UP        = &4.
  W_SORTCAT-SUBTOT    = &5.
  APPEND W_SORTCAT.
  CLEAR : W_SORTCAT.
END-OF-DEFINITION.


**---
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-003.
PARAMETERS : P_WERKS LIKE T001W-WERKS OBLIGATORY DEFAULT 'P001',
             P_CDATE LIKE SY-DATUM OBLIGATORY,     "Current date
             P_CTIME LIKE SY-UZEIT OBLIGATORY.     "Current time
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-004.
PARAMETERS : P_ARBPL LIKE CRHD-ARBPL OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BLOCK2.

SELECT-OPTIONS : S_MATNR FOR MARA-MATNR.


**---
INITIALIZATION.
*  p_cdate = sy-datum.
*  p_ctime = sy-uzeit.
  PERFORM EVENT_BUILD USING W_EVENTCAT[].


**---
TOP-OF-PAGE.
  PERFORM TOP_OF_PAGE.
*  PERFORM make_col_heading.

AT SELECTION-SCREEN.
  PERFORM GET_EXEC_TIME.

**---
START-OF-SELECTION.
  PERFORM GET_DATA.

**---
END-OF-SELECTION.
  IF IT_TOLINE[] IS INITIAL.
    MESSAGE S999(ZMMM) WITH 'There is no data!'(001).
    EXIT.
  ELSE.
    PERFORM EXECUTE_PROCESS.
    PERFORM UPDATE_TABLE.
    PERFORM COMMENT_BUILD.     " USING w_top_of_page[].
    PERFORM MAKE_ALV_GRID.
*    PERFORM send_mail.
  ENDIF.








*&----------------------------------------------------------------------
FORM TIME_CALCULATION USING    VALUE(IM_DATE) TYPE D
                               VALUE(IM_TIME) TYPE T
                               VALUE(IM_MINUTES)
                      CHANGING VALUE(EX_DATE) TYPE D
                               VALUE(EX_TIME) TYPE T.
*---
  CLEAR : EX_DATE, EX_TIME.
  DATA : LV_TIME    TYPE T.
  DATA : LV_HOURSUM TYPE P.

  PERFORM GET_TIME_FROM_MINUTES USING    IM_MINUTES
                                CHANGING LV_TIME.

  LV_HOURSUM = IM_TIME(2) + LV_TIME(2).
  EX_DATE = IM_DATE.
  EX_TIME = IM_TIME + LV_TIME.
  IF LV_HOURSUM >= 24.
    EX_DATE = EX_DATE + 1.
  ENDIF.

**---
  MOVE : '00' TO EX_TIME+4(2).
ENDFORM.                    " time_calculation

*&---------------------------------------------------------------------*
*&      Form  get_verme_lpmin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_VERME_LPMIN.
*---
  CLEAR : IT_STOCK_TEMP, IT_STOCK_TEMP[], IT_STOCK, IT_STOCK[].

  SELECT A~PKNUM
         B~LGNUM
         B~LQNUM
         A~MATNR
         GESME     " quantity
         A~LGTYP
         A~LGPLA
               INTO CORRESPONDING FIELDS OF TABLE IT_STOCK_TEMP
               FROM PKHD AS A INNER JOIN LQUA AS B
                 ON A~MANDT EQ B~MANDT
                AND A~MATNR EQ B~MATNR
                AND A~LGTYP EQ B~LGTYP
                AND A~LGPLA EQ B~LGPLA
                AND B~BESTQ EQ SPACE
                FOR ALL ENTRIES IN IT_TOLINE
              WHERE A~MATNR EQ IT_TOLINE-MATNR.

  LOOP AT IT_STOCK_TEMP.
    MOVE : IT_STOCK_TEMP-MATNR TO IT_STOCK-MATNR,
           IT_STOCK_TEMP-GESME TO IT_STOCK-GESME.
    COLLECT IT_STOCK.
    CLEAR : IT_STOCK_TEMP, IT_STOCK.
  ENDLOOP.
ENDFORM.                    " get_verme_lpmin

*&---------------------------------------------------------------------*
*&      Form  get_open_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_OPEN_TO.
*---
  CLEAR : IT_OPEN_TEMP, IT_OPEN_TEMP[], IT_OPEN, IT_OPEN[].

  SELECT A~LGNUM
         A~TANUM
         A~TAPOS
         A~MATNR
         VSOLA     "Source target quantity in alternate unit
               INTO CORRESPONDING FIELDS OF TABLE IT_OPEN_TEMP
               FROM LTAP AS A
               INNER JOIN LTAK AS B
               ON A~LGNUM = B~LGNUM
               AND A~TANUM = B~TANUM
                FOR ALL ENTRIES IN IT_TOLINE
              WHERE A~MATNR EQ IT_TOLINE-MATNR
                AND A~PQUIT EQ SPACE
                AND A~LGNUM EQ 'P01'
                AND B~BWLVS = '850'.
  "Open TO(Indicator: confirmation complete)

  LOOP AT IT_OPEN_TEMP.
    MOVE-CORRESPONDING IT_OPEN_TEMP TO IT_OPEN.
    COLLECT IT_OPEN.
    CLEAR : IT_OPEN_TEMP, IT_OPEN.
  ENDLOOP.
ENDFORM.                    " get_open_to

*&---------------------------------------------------------------------*
*&      Form  get_bf_error_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BF_ERROR_QTY.
*---
  RANGES: RT_MATNR FOR MARA-MATNR.

  CLEAR : IT_BFERROR_TEMP, IT_BFERROR_TEMP[], IT_BFERROR, IT_BFERROR[].

  CLEAR : AFFW.
*----- changed by bsbae. changed on 20040706
  LOOP AT IT_TOLINE.
    MOVE: 'I'  TO RT_MATNR-SIGN,
          'EQ' TO RT_MATNR-OPTION,
          IT_TOLINE-MATNR TO RT_MATNR-LOW.
    APPEND RT_MATNR.
  ENDLOOP.
  IF SY-SUBRC NE 0.
    MOVE: 'I'  TO RT_MATNR-SIGN,
          'EQ' TO RT_MATNR-OPTION,
          '  ' TO RT_MATNR-LOW.
    APPEND RT_MATNR.
  ENDIF.

  SELECT MATNR
         BWART
         WERKS
         LGORT
         ERFMG
               INTO CORRESPONDING FIELDS OF TABLE IT_BFERROR_TEMP
               FROM AFFW
*                FOR ALL ENTRIES IN it_toline
*              WHERE matnr EQ it_toline-matnr
              WHERE MATNR IN RT_MATNR
*                AND werks EQ 'P001'
*                AND lgort EQ 'P400'
                AND BWART IN ('261', '262').
*----- changed by bsbae. changed on 20040706

  DELETE IT_BFERROR_TEMP WHERE NOT ( WERKS EQ 'P001'
                                 AND LGORT EQ 'P400' ).

  LOOP AT IT_BFERROR_TEMP.
    IF IT_BFERROR_TEMP-BWART EQ '262'.
      IT_BFERROR_TEMP-BWART = IT_BFERROR_TEMP-BWART * -1.
    ENDIF.

    MOVE : IT_BFERROR_TEMP-MATNR TO IT_BFERROR-MATNR,
           IT_BFERROR_TEMP-ERFMG TO IT_BFERROR-ERFMG.
    COLLECT IT_BFERROR.
    CLEAR : IT_BFERROR_TEMP, IT_BFERROR.
  ENDLOOP.
ENDFORM.                    " get_bf_error_qty

*&---------------------------------------------------------------------*
*&      Form  get_rdmng
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_RDMNG.
*---
  CLEAR : IT_ROUND, IT_ROUND[], IT_ROUND_TEMP, IT_ROUND_TEMP[].

  SELECT LGNUM
         LGTYP
         MATNR
         RDMNG
               INTO CORRESPONDING FIELDS OF TABLE IT_ROUND_TEMP
               FROM MLGT
                FOR ALL ENTRIES IN IT_TOLINE
              WHERE MATNR EQ IT_TOLINE-MATNR
** added by Furong on 07/29/2005
                AND LGTYP EQ IT_TOLINE-SRC_LGTYP
** end of addition
                AND LVORM EQ SPACE.

  LOOP AT IT_ROUND_TEMP.
    MOVE : IT_ROUND_TEMP-MATNR TO IT_ROUND-MATNR,
           IT_ROUND_TEMP-RDMNG TO IT_ROUND-RDMNG.
    COLLECT IT_ROUND.
    CLEAR : IT_ROUND_TEMP, IT_ROUND.
  ENDLOOP.
ENDFORM.                    " get_rdmng

*&---------------------------------------------------------------------*
*&      Form  get_tqty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_TQTY.
**--- logic which get quantity :
*     if supply to line master table 'STOCK_CHECK' = 'X'.
*  TO Qty. =    RESB-BDMNG(requirement qty)
*     1.      - LQUA-VERME(current stock(GESME))
*     2.      + Safety Stock(Supply to Line master table field)
*     3.      - open quantity
*     4.      + backflush error quantity
*     5.      + scheduled backflush quantity
**---

*---
  DATA : LV_TQTY LIKE RESB-BDMNG.  "Target Quantity.

  LV_TQTY = IT_TOLINE-BDMNG.

*--- check current stock     1.
  IF IT_TOLINE-STOCK_CHECK = 'X'.
    CLEAR : IT_STOCK.
    READ TABLE IT_STOCK WITH KEY MATNR = IT_TOLINE-MATNR.
    MOVE : IT_STOCK-GESME TO IT_TOLINE-VERME.
    LV_TQTY = LV_TQTY - IT_TOLINE-VERME.
  ENDIF.

*--- check safety stock     2.
  IF IT_TOLINE-STOCK_CHECK = 'X'.
    LV_TQTY = LV_TQTY + IT_TOLINE-LPMIN.
  ENDIF.

*--- check Open TO quantity     3.
  IF IT_TOLINE-STOCK_CHECK = 'X'.
    CLEAR : IT_OPEN.
    READ TABLE IT_OPEN WITH KEY MATNR = IT_TOLINE-MATNR.
    MOVE : IT_OPEN-VSOLA TO IT_TOLINE-OPEN_TO.
    LV_TQTY = LV_TQTY - IT_TOLINE-OPEN_TO.
  ENDIF.

*--- check backflush error quantity     4.
  IF IT_TOLINE-STOCK_CHECK = 'X'.
    CLEAR : IT_BFERROR.
    READ TABLE IT_BFERROR WITH KEY MATNR = IT_TOLINE-MATNR.
    MOVE : IT_BFERROR-ERFMG TO IT_TOLINE-BFERRORQTY.
    LV_TQTY = LV_TQTY + IT_TOLINE-BFERRORQTY.
  ENDIF.

*--- check scheduled backflush quantity     5.
  IF IT_TOLINE-STOCK_CHECK = 'X'.
    CLEAR : IT_BACK.
    READ TABLE IT_BACK WITH KEY MATNR = IT_TOLINE-MATNR.
    MOVE : IT_BACK-KOMP_QUANT TO IT_TOLINE-BF_TO_QTY.
    LV_TQTY = LV_TQTY + IT_TOLINE-BF_TO_QTY.
  ENDIF.

  IF LV_TQTY LT 1.
    MOVE : 'X' TO W_MINUS_STOCK_CHECK.
  ENDIF.


  IF W_MINUS_STOCK_CHECK EQ SPACE.
*--- rounding value check
*A. Get Remainder  :mod
*B. Get quotient   :div
    DATA : LV_TQTYMODRDMNG TYPE P,
           LV_TQTYDIVRDMNG TYPE P.

    CLEAR : IT_ROUND.
    READ TABLE IT_ROUND WITH KEY MATNR = IT_TOLINE-MATNR.
    MOVE : IT_ROUND-RDMNG TO IT_TOLINE-RDMNG.

    IF NOT IT_TOLINE-RDMNG IS INITIAL.
      LV_TQTYMODRDMNG = LV_TQTY MOD IT_TOLINE-RDMNG.
      LV_TQTYDIVRDMNG = LV_TQTY DIV IT_TOLINE-RDMNG.
    ENDIF.

    IF NOT LV_TQTYMODRDMNG IS INITIAL.
      LV_TQTYDIVRDMNG = LV_TQTYDIVRDMNG + 1.
      LV_TQTY = LV_TQTYDIVRDMNG * IT_TOLINE-RDMNG.
    ENDIF.
    IT_TOLINE-TQTY = LV_TQTY.   "Target Qty
  ELSE.
    IT_TOLINE-TQTY = 0.
  ENDIF.
ENDFORM.                    " get_tqty

*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_NRO_NR_09  text
*      -->P_W_NRO_OBJECT  text
*      <--P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM NUMBER_GET_NEXT
        USING    VALUE(P_W_NRO_INTERVAL) LIKE INRI-NRRANGENR
                 VALUE(P_W_NRO_OBJECT)   LIKE INRI-OBJECT
        CHANGING VALUE(P_W_NRO_NEXT).
*---
  CLEAR: P_W_NRO_NEXT.

  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            NR_RANGE_NR             = P_W_NRO_INTERVAL
            OBJECT                  = P_W_NRO_OBJECT
       IMPORTING
            NUMBER                  = P_W_NRO_NEXT
       EXCEPTIONS
            INTERVAL_NOT_FOUND      = 1
            NUMBER_RANGE_NOT_INTERN = 2
            OBJECT_NOT_FOUND        = 3
            QUANTITY_IS_0           = 4
            QUANTITY_IS_NOT_1       = 5
            INTERVAL_OVERFLOW       = 6
            OTHERS                  = 7.
ENDFORM.                    " number_get_next

*&---------------------------------------------------------------------*
*&      Form  get_sorce_storage_type_bin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SORCE_STORAGE_TYPE_BIN.
*---
  CLEAR : IT_SOURCE, IT_SOURCE[].

  SELECT MATNR
         LGTYP
         LGPLA
               INTO CORRESPONDING FIELDS OF TABLE IT_SOURCE
               FROM MLGT
                FOR ALL ENTRIES IN IT_TOLINE
              WHERE MATNR EQ IT_TOLINE-MATNR
** added by Furong on 07/29/2005
                AND LGTYP EQ IT_TOLINE-SRC_LGTYP
** end of addition

                AND LVORM EQ SPACE.
ENDFORM.                    " get_sorce_storage_type_bin

*&---------------------------------------------------------------------*
*&      Form  get_des_storage_type_bin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DES_STORAGE_TYPE_BIN.
*---
  CLEAR : IT_DEST, IT_DEST[].

  SELECT MATNR
         LGTYP
         LGPLA
               INTO CORRESPONDING FIELDS OF TABLE IT_DEST
               FROM PKHD
                FOR ALL ENTRIES IN IT_TOLINE
              WHERE MATNR EQ IT_TOLINE-MATNR.
ENDFORM.                    " get_des_storage_type_bin

*&---------------------------------------------------------------------*
*&      Form  bdc_processing_lt01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      -->P_W_ZDOCNO  text
*      <--P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM BDC_PROCESSING_LT01 TABLES   EXT_BDCMSGCOLL STRUCTURE BDCMSGCOLL
                         USING    VALUE(P_ZDOCNO)
                         CHANGING VALUE(P_SUBRC).
*---
  CLEAR : EXT_BDCMSGCOLL, EXT_BDCMSGCOLL[], P_SUBRC.

  DATA : LV_BWLVS_002     TYPE BDCDATA-FVAL,   "Movement type
         LV_MATNR_003     TYPE BDCDATA-FVAL,
         LV_ANFME_004     TYPE BDCDATA-FVAL,
         LV_ANFME_007     TYPE BDCDATA-FVAL,
         LV_ALTME_008     TYPE BDCDATA-FVAL,
         LV_VLTYP_009     TYPE BDCDATA-FVAL,
         LV_VLPLA_010     TYPE BDCDATA-FVAL,
         LV_NLTYP_011     TYPE BDCDATA-FVAL,
         LV_NLPLA_012     TYPE BDCDATA-FVAL,
         LV_REFNR_013     TYPE BDCDATA-FVAL.   "Group(Feeder)
** changed on 07/27/2006 by Furong
  IF IT_TOLINE-FEEDR = 'JIS'.
    LV_BWLVS_002 = '977'.
  ELSE.
    LV_BWLVS_002 = '850'.  "Movement type
  ENDIF.
** end of change
  LV_REFNR_013 = IT_TOLINE-FEEDR. "Group(Feeder)
  LV_MATNR_003  = IT_TOLINE-MATNR. "Material '327003K100'
  LV_ANFME_004  = IT_TOLINE-TQTY.
  LV_ANFME_007  = IT_TOLINE-TQTY.
  LV_ALTME_008  = IT_TOLINE-MEINS.
  LV_VLTYP_009  = IT_TOLINE-SRC_LGTYP.  "Src Storage Type '434'
  LV_VLPLA_010  = IT_TOLINE-SRC_LGPLA.  "Src Storage Bin  'AA-01-11'
  LV_NLTYP_011  = IT_TOLINE-DES_LGTYP.  "Des Storage Type '443'
  LV_NLPLA_012  = IT_TOLINE-DES_LGPLA.  "Des Storage Bin  'TS-01'

  CONDENSE : LV_BWLVS_002,  "Movement type
             LV_MATNR_003,
             LV_ANFME_004,
             LV_ANFME_007,
             LV_ALTME_008,
             LV_VLTYP_009,
             LV_VLPLA_010,
             LV_NLTYP_011,
             LV_NLPLA_012,
             LV_REFNR_013.

*--- BDC for LT01(Create TO)
  CALL FUNCTION 'Z_FMM_6012_01'
       EXPORTING
            LGNUM_001 = 'P01'  "Warehouse number
            REFNR_013 = LV_REFNR_013  "Group(Feeder)
            BWLVS_002 = LV_BWLVS_002  "Movement type '850'
            MATNR_003 = LV_MATNR_003  "Material '327003K100'
            ANFME_004 = LV_ANFME_004
            WERKS_005 = 'P001'  "Plant
            LGORT_006 = 'P400'  "Storage Location
            ANFME_007 = LV_ANFME_007
            ALTME_008 = LV_ALTME_008
            VLTYP_009 = LV_VLTYP_009  "Src Storage Type '434'
            VLPLA_010 = LV_VLPLA_010  "Src Storage Bin 'AA-01-11'
            NLTYP_011 = LV_NLTYP_011  "Des Storage Type '443'
            NLPLA_012 = LV_NLPLA_012  "Des Storage Bin 'TS-01'
       IMPORTING
            SUBRC     = P_SUBRC
       TABLES
            MESSTAB   = EXT_BDCMSGCOLL[].

*--- One More Try
  IF P_SUBRC NE 0.
    CALL FUNCTION 'Z_FMM_6012_01'
         EXPORTING
              LGNUM_001 = 'P01'  "Warehouse number
              REFNR_013 = LV_REFNR_013  "Group(Feeder)
              BWLVS_002 = LV_BWLVS_002  "Movement type '850'
              MATNR_003 = LV_MATNR_003  "Material '327003K100'
              ANFME_004 = LV_ANFME_004  " Quantity
              WERKS_005 = 'P001'  "Plant
              LGORT_006 = 'P400'  "Storage Location
              ANFME_007 = LV_ANFME_007  " Quantity
              ALTME_008 = LV_ALTME_008  " Unit of Measure
              VLTYP_009 = LV_VLTYP_009  "Src Storage Type '434'
              VLPLA_010 = LV_VLPLA_010  "Src Storage Bin 'AA-01-11'
              NLTYP_011 = LV_NLTYP_011  "Des Storage Type '443'
              NLPLA_012 = LV_NLPLA_012  "Des Storage Bin 'TS-01'
         IMPORTING
              SUBRC     = P_SUBRC
         TABLES
              MESSTAB   = EXT_BDCMSGCOLL[].
  ENDIF.
ENDFORM.                    " bdc_processing_lt01

*&---------------------------------------------------------------------*
*&      Form  bdc_processing_lta1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      -->P_W_ZDOCNO  text
*      -->P_WA_BDCMSGCOLL_MSGV1  text
*      <--P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM BDC_PROCESSING_LTA1 TABLES   EXT_BDCMSGCOLL STRUCTURE BDCMSGCOLL
                         USING    VALUE(P_ZDOCNO)
                                  VALUE(P_MSGV1)
                         CHANGING VALUE(P_SUBRC).
*---
  CLEAR : EXT_BDCMSGCOLL, EXT_BDCMSGCOLL[], P_SUBRC.

  DATA : LV_TANUM_001     TYPE BDCDATA-FVAL,  "TO number
         LV_LGNUM_002     TYPE BDCDATA-FVAL,  "Warehouse number
         LV_STDAT_003     TYPE BDCDATA-FVAL,  "Start date
         LV_STUZT_004     TYPE BDCDATA-FVAL,  "Start time
         LV_ENDAT_005     TYPE BDCDATA-FVAL,  "End date
         LV_ENUZT_006     TYPE BDCDATA-FVAL.  "End time

  DATA : LV_DATE(8).

  CLEAR : LV_DATE.

  PERFORM USER_DATE_FORMAT USING    SY-UNAME
                                    IT_TOLINE-SDATE
                           CHANGING LV_DATE.

  LV_STDAT_003 = LV_DATE.        "Start date
  LV_TANUM_001 = P_MSGV1.                              "TO number  '813'
  LV_LGNUM_002 = 'P01'.                                "Warehouse number
  LV_STUZT_004 = IT_TOLINE-FEEDING_TIME.             "Start time

*  MOVE : w_ending_date TO it_toline-edate.
*  MOVE : w_ending_time TO it_toline-etime.

*  PERFORM time_calculation USING    it_toline-sdate
*                                    it_toline-feeding_time
*                                    60   "Minutes
*                           CHANGING it_toline-edate
*                                    it_toline-etime.

*  MOVE : w_ending_time TO it_toline-etime.

  LV_ENUZT_006 = IT_TOLINE-ETIME. "End time

  CLEAR : LV_DATE.
  PERFORM USER_DATE_FORMAT USING    SY-UNAME
                                    IT_TOLINE-EDATE
                           CHANGING LV_DATE.

  LV_ENDAT_005 = LV_DATE.      "End date

  CONDENSE : LV_TANUM_001,
             LV_LGNUM_002,
             LV_STDAT_003,
             LV_STUZT_004,
             LV_ENDAT_005,
             LV_ENUZT_006.

*--- BDC for LTA1(Change TO Header)
  CALL FUNCTION 'Z_FMM_6012_02'
       EXPORTING
            TANUM_001 = LV_TANUM_001  " TO number '813'
            LGNUM_002 = LV_LGNUM_002
            STDAT_003 = LV_STDAT_003  "Start date
            STUZT_004 = LV_STUZT_004  "Start time "'10:36:48'
            ENDAT_005 = LV_ENDAT_005  "End date
            ENUZT_006 = LV_ENUZT_006  "End time
       IMPORTING
            SUBRC     = P_SUBRC
       TABLES
            MESSTAB   = EXT_BDCMSGCOLL[].

*--- BDC Log to the table ZTLOG
  IF EXT_BDCMSGCOLL[] IS INITIAL.  "SUCCESS
    CLEAR : WA_BDCMSGCOLL.
    WA_BDCMSGCOLL-TCODE   = 'LT1A'.
    WA_BDCMSGCOLL-MSGTYP  = 'S'.  "SUCCESS
    WA_BDCMSGCOLL-MSGSPRA = 'E'.
    WA_BDCMSGCOLL-MSGID   = 'ZMMM'.
    WA_BDCMSGCOLL-MSGNR   = '999'.
    WA_BDCMSGCOLL-MSGV1   = 'Transfer order'.
    WA_BDCMSGCOLL-MSGV2   = LV_TANUM_001.
    WA_BDCMSGCOLL-MSGV3   = 'Start/End Date/Time'.
    WA_BDCMSGCOLL-MSGV4   = 'is changed.'.
    APPEND WA_BDCMSGCOLL TO EXT_BDCMSGCOLL[].
  ENDIF.
ENDFORM.                    " bdc_processing_lta1

*&---------------------------------------------------------------------*
*&      Form  write_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_LIST.
**---
*  WRITE : /1(18)  <fs_toline>-matnr RIGHT-JUSTIFIED,
*          20(10)  <fs_toline>-bdmng UNIT <fs_toline>-meins,
*          31      <fs_toline>-meins,
*          36      <fs_toline>-works,
*          50      <fs_toline>-rh_lh,
*          60      <fs_toline>-feeding_time,  "Start time(Feeding Time)
*          75      <fs_toline>-stock_check,
*          90      <fs_toline>-feed_cycle,
*         115      <fs_toline>-ztime,
*         155      <fs_toline>-verme UNIT <fs_toline>-meins,
*         180      <fs_toline>-lpmin UNIT <fs_toline>-meins,
*         220      <fs_toline>-open_to UNIT <fs_toline>-meins,
*         240      <fs_toline>-bferrorqty UNIT <fs_toline>-meins,
*         270      <fs_toline>-bf_to_qty UNIT <fs_toline>-meins,
*         300      <fs_toline>-rdmng UNIT <fs_toline>-meins,
*         325      <fs_toline>-tqty UNIT <fs_toline>-meins,
*         355      <fs_toline>-feedr,      "Feeder
*         365      wa_bdcmsgcoll-msgv1 COLOR COL_POSITIVE.
ENDFORM.                    " write_list

*&---------------------------------------------------------------------*
*&      Form  make_col_heading
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_COL_HEADING.
*---
  WRITE: /1   'Material',
          20(9) 'Quantity' RIGHT-JUSTIFIED,
          31  'Unit',
          36  'Workstation',
          50  'RH/LH',
          60  'Start Time',  "Feeding Time
          75  'Stock Check',
          90  'Feed Cycle',
          115  'Time for STL',
          155 'Available stock(GESME)',
          180 'Safety Stock(LPMIN)',
*         155  'Available stock(VERME)',
*         180 'Minimum storage bin quantity(LPMIN)',
          220 'Open TO',
          240 'Backflush Error Check',
          270 'Scheduled Backflush Qty',
          300 'Rounding Quantity Check',
          325 'Target Quantity(TO)',
          355 'Feeder'.
ENDFORM.                    " make_col_heading

*&---------------------------------------------------------------------*
*&      Form  get_time_from_minutes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IM_MINUTES  text
*      <--P_LV_TIME  text
*----------------------------------------------------------------------*
FORM GET_TIME_FROM_MINUTES USING    VALUE(IM_MINUTES)
                           CHANGING VALUE(EX_TIME) TYPE T.
*---
  CLEAR : EX_TIME.
  DATA : BEGIN OF LS_TIME,
           HOUR(2) TYPE N,
           MINUTE(2) TYPE N,
           SECOND(2) TYPE N,
         END OF LS_TIME.

  LS_TIME-MINUTE = IM_MINUTES MOD 60.
  LS_TIME-HOUR   = IM_MINUTES DIV 60.

  MOVE LS_TIME TO EX_TIME.
ENDFORM.                    " get_time_from_minutes

*&---------------------------------------------------------------------*
*&      Form  user_date_format
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UNAME  text
*      -->P_<FS_TOLINE>_SDATE  text
*      <--P_LV_DATE  text
*----------------------------------------------------------------------*
FORM USER_DATE_FORMAT USING    VALUE(P_USER)     LIKE SY-UNAME
                               VALUE(P_DATE)     LIKE SY-DATUM
                      CHANGING VALUE(P_USERDATE) TYPE CHAR8.
*---
  CLEAR : P_USERDATE.

  DATA : YYYY(4).  "year
  DATA : MM(2).    "day
  DATA : DD(2).    "month
  DATA : DATFM LIKE USR01-DATFM.  "date format

  SELECT SINGLE DATFM INTO DATFM
                      FROM USR01
                     WHERE BNAME = P_USER.

** datfm
*1 DD.MM.YYYY
*2 MM/DD/YYYY
*3 MM-DD-YYYY
*4 YYYY.MM.DD
*5 YYYY/MM/DD
*6 YYYY-MM-DD
  YYYY = P_DATE+0(4).
  MM   = P_DATE+4(2).
  DD   = P_DATE+6(2).

  CASE DATFM.
    WHEN 1.
      P_USERDATE+0(2) = DD.
      P_USERDATE+2(2) = MM.
      P_USERDATE+4(4) = YYYY.
    WHEN 2 OR 3.
      P_USERDATE+0(2) = MM.
      P_USERDATE+2(2) = DD.
      P_USERDATE+4(4) = YYYY.
    WHEN 4 OR 5 OR 6.
      P_USERDATE+0(4) = YYYY.
      P_USERDATE+4(2) = MM.
      P_USERDATE+6(2) = DD.
  ENDCASE.
ENDFORM.                    " user_date_format

*&---------------------------------------------------------------------*
*&      Form  get_scheduled_bf_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SCHEDULED_BF_QTY.
*---
  CLEAR : IT_BFQTY_TEMP, IT_BFQTY_TEMP[], IT_BACK, IT_BACK[].

  SELECT MATNR
         WERKS
         LGORT
         KOMP_QUANT     " quantity
         FLG_ASYNCH
         SYNC_IND
                    INTO CORRESPONDING FIELDS OF TABLE IT_BFQTY_TEMP
                    FROM ZTMM_PPC1_ALL
                   WHERE FLG_ASYNCH NE 'X'
                  %_HINTS ORACLE 'FIRST_ROWS(10)'.

*---
  DELETE IT_BFQTY_TEMP WHERE SYNC_IND EQ 'X'.

*---
  LOOP AT IT_BFQTY_TEMP.
    MOVE-CORRESPONDING IT_BFQTY_TEMP TO IT_BACK.
    COLLECT IT_BACK.
    CLEAR : IT_BFQTY_TEMP, IT_BACK.
  ENDLOOP.
ENDFORM.                    " get_scheduled_bf_qty

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
*---
  CLEAR : W_SCREEN_DATE, W_SCREEN_TIME, W_CURRENT_DATE, W_CURRENT_TIME.


*---
  PERFORM DECIDE_TIMES.

*---
  PERFORM GET_VEHICLE_MASTER.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  get_vehicle_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_VEHICLE_MASTER.
  DATA: LW_DATE_F(14),
        LW_DATE_T(14).

*---
  DATA : LV_RP06_TIMESTAMP(11).      "'YYYYMMDDHHMISSX'

  DATA : BEGIN OF LS_RSNUM,
           RSNUM LIKE ZTPP_DVRT1-RSNUM,  "Reservation number
         END OF LS_RSNUM.

  DATA : LT_RSNUM LIKE LS_RSNUM OCCURS 0 WITH HEADER LINE.

  DATA : LT_SUPPLY_INFO LIKE ZSPP_VIN_INFO_FOR_STL OCCURS 0
                                                   WITH HEADER LINE.

*---
  CLEAR : LT_SUPPLY_INFO, LT_SUPPLY_INFO[].

  CONCATENATE: W_PRE_S_DATE W_PRE_S_TIME INTO LW_DATE_F,
               W_PRE_E_DATE W_PRE_E_TIME INTO LW_DATE_T.

  CALL FUNCTION 'Z_FPP_GET_SUPPLY_TO_LINE'
       EXPORTING
            I_DATE_F                 = LW_DATE_F
            I_DATE_T                 = LW_DATE_T
*            i_date                   = w_current_date
*            i_time                   = w_current_time
            I_RP                     = '07'
       TABLES
            T_SUPPLY_INFO            = LT_SUPPLY_INFO
       EXCEPTIONS
            NO_DATA_FOUNDED          = 1
            LINE_INFO_DOES_NOT_EXIST = 2
            ETC_EXCEPTION            = 3
            OTHERS                   = 4.

  LOOP AT LT_SUPPLY_INFO.
    MOVE : LT_SUPPLY_INFO-RSNUM TO LS_RSNUM.
    APPEND LS_RSNUM TO LT_RSNUM.
    CLEAR : LT_SUPPLY_INFO, LS_RSNUM, LT_RSNUM.
  ENDLOOP.

* Unique reservation numbers
  DELETE LT_RSNUM WHERE RSNUM = '0000000000'.

  DELETE ADJACENT DUPLICATES FROM LT_RSNUM
                             COMPARING RSNUM.

*/ Get Quantity Raw Data(LT_TOLINE) from RESB and ZTMM_MAST
  DATA : BEGIN OF LS_TOLINE.
          INCLUDE STRUCTURE WA_TOLINE.
  DATA :   RSNUM LIKE RESB-RSNUM,
           RSPOS LIKE RESB-RSPOS,
         END OF LS_TOLINE.

  DATA : LT_TOLINE LIKE LS_TOLINE OCCURS 0 WITH HEADER LINE.

  CHECK NOT LT_RSNUM[] IS INITIAL.

  SELECT A~RSNUM            "Reservation number
         A~RSPOS            "Item number of reservation
         A~MATNR            "Material
         A~BDMNG            "Quantity
         A~MEINS            "Unit
         B~FEEDR            "Feeder
         B~WORKS            "Workstation
         B~RH_LH            "RH/LH
         B~STOCK_CHECK      "STOCK CHECK
         B~FEED_CYCLE       "FEEDING CYCLE
         B~ZTIME            "TIME
         B~LPMIN            " safety stock
         B~DISPO            " person of contact
         B~ZMNMX            " Min or Max
                 INTO CORRESPONDING FIELDS OF TABLE LT_TOLINE
                 FROM RESB AS A INNER JOIN ZTMM_MAST AS B
                   ON A~MANDT EQ B~MANDT
                  AND A~MATNR EQ B~MATNR
                  FOR ALL ENTRIES IN LT_RSNUM
                WHERE A~RSNUM EQ LT_RSNUM-RSNUM    "Reservation number
                  AND B~WERKS EQ 'P001'
                  AND B~SPPTL EQ 'S'
                  AND ZLINE IN ('F1', 'F2', 'F3')
                  AND B~MATNR IN S_MATNR.

*/Get draft data(IT_TOLINE) for TO Creation
  DATA : LV_BDMNG     LIKE WA_TOLINE-BDMNG.  "For Sum of qty

  SORT LT_TOLINE BY MATNR.

  LOOP AT LT_TOLINE.
    LV_BDMNG = LV_BDMNG + LT_TOLINE-BDMNG.
    MOVE-CORRESPONDING LT_TOLINE TO WA_TOLINE.
    AT END OF MATNR.
      MOVE : LV_BDMNG TO WA_TOLINE-BDMNG.
      APPEND WA_TOLINE TO IT_TOLINE.
      CLEAR : LV_BDMNG.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " get_vehicle_master

*&---------------------------------------------------------------------*
*&      Form  execute_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXECUTE_PROCESS.
*--- get scheduled backflush quantity
  PERFORM GET_SCHEDULED_BF_QTY.
*--- get current stock
  PERFORM GET_VERME_LPMIN.
*--- get open TO quantity
  PERFORM GET_OPEN_TO.
*--- get BackFlush error quantity(/nCOGI)
  PERFORM GET_BF_ERROR_QTY.
*--- get rounding value

*** added by furong on 01/08/2005, checking rounding qty by storage type
  PERFORM GET_SOURCE_TYPE.
*** end of addition
*--- get rounding value
  PERFORM GET_RDMNG.

*--- get source storage type/bin
*** changed by furong on 01/08/2005
*  PERFORM get_sorce_storage_type_bin.
*--- get destination storage type/bin
*** end of change

  PERFORM GET_DES_STORAGE_TYPE_BIN.

*---
  PERFORM CREATE_TRANSFER_ORDER.
ENDFORM.                    " execute_process

*&---------------------------------------------------------------------*
*&      Form  create_transfer_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_TRANSFER_ORDER.
*---
  DATA : L_TABIX LIKE SY-TABIX,
         L_MESSA(80).
  DATA : LW_BEGZT(14).
  DATA : L_DATUM TYPE D.
  DATA : L_STATS LIKE ZTMM_STL_LOG-STATS.

  CLEAR : IT_ITAB, IT_ITAB[], IT_DISPO, IT_DISPO[].

  LOOP AT IT_TOLINE.
    CLEAR : L_STATS.
    MOVE : SY-TABIX TO L_TABIX.
*--- TO GET Start Date(Feeding Date) and Start Time
    MOVE : W_SCREEN_DATE   TO IT_TOLINE-SDATE,
           W_STARTING_TIME TO IT_TOLINE-FEEDING_TIME,
           W_ENDING_DATE   TO IT_TOLINE-EDATE,
           W_ENDING_TIME   TO IT_TOLINE-ETIME.
*    PERFORM time_calculation USING    w_screen_date
*                                      w_screen_time
*                                      0
*                             CHANGING it_toline-sdate
*                                      it_toline-feeding_time.

*--- consider feeding cycle
    IF IT_TOLINE-FEED_CYCLE EQ '0120'.
      CLEAR : W_ODD_EVEN, IT_1T.
      CONCATENATE W_SCREEN_DATE W_STARTING_TIME INTO LW_BEGZT.
*      CONCATENATE w_current_date w_current_time INTO lw_begzt.
      READ TABLE IT_1T WITH KEY BEGZT = LW_BEGZT.
      IF SY-SUBRC NE 0.
        MESSAGE E000(ZZ) WITH TEXT-M12.
      ENDIF.
      W_ODD_EVEN = IT_1T-INDEX MOD 2.
*      READ TABLE it_ztmm_stl_time WITH KEY rtime = w_current_time.
*      w_odd_even = it_ztmm_stl_time-zindx MOD 2.
*      w_odd_even = it_toline-feeding_time(2) MOD 2.
      IF W_ODD_EVEN EQ 0.
*      IF w_odd_even EQ 0.
*      IF w_odd_even NE 1.
        CLEAR : IT_1T, W_TABIX, L_DATUM.
        READ TABLE IT_1T WITH KEY BEGZT = LW_BEGZT.
        IF SY-SUBRC NE 0.
          MESSAGE E000(ZZ) WITH TEXT-M12.
        ENDIF.
*        READ TABLE it_ztmm_stl_time WITH KEY rtime = w_current_time.
        MOVE : SY-TABIX TO W_TABIX,
               IT_1T-DATUM TO L_DATUM.
        W_TABIX = W_TABIX + 1.
        CLEAR : IT_1T.
        READ TABLE IT_1T INDEX W_TABIX.
*--- if it is the last time-zone of the day, no shift.
        IF IT_1T-DATUM NE L_DATUM.
          W_TABIX = W_TABIX - 1.
          CLEAR : IT_1T.
          READ TABLE IT_1T INDEX W_TABIX.
          MOVE : IT_1T-BEGZT+8(6) TO IT_TOLINE-FEEDING_TIME,
                 IT_1T-ENDZT(8)   TO IT_TOLINE-EDATE,
                 IT_1T-ENDZT+8(6) TO IT_TOLINE-ETIME.
        ELSE.
          MOVE : IT_1T-BEGZT+8(6) TO IT_TOLINE-FEEDING_TIME,
                 IT_1T-ENDZT(8)   TO IT_TOLINE-EDATE,
                 IT_1T-ENDZT+8(6) TO IT_TOLINE-ETIME.
        ENDIF.
*        READ TABLE it_ztmm_stl_time INDEX w_tabix.
*        MOVE : it_ztmm_stl_time-stime TO it_toline-feeding_time.
      ENDIF.
    ENDIF.
*---

    CLEAR : W_MINUS_STOCK_CHECK.

*--- calculate quantity
*    PERFORM get_tqty.
    PERFORM GET_TQTY_NEW.

*--- App Doc No
    PERFORM NUMBER_GET_NEXT USING    C_NRO_NR_09     "NRO Interval
                                     W_NRO_OBJECT    "NRO Object
                            CHANGING W_ZDOCNO.     "App Doc No
    COMMIT WORK.

    IF W_MINUS_STOCK_CHECK EQ SPACE.
*--- Get Source Storage type/bin
*      CLEAR : it_source.
*      READ TABLE it_source WITH KEY matnr = it_toline-matnr.
*      MOVE : it_source-lgtyp TO it_toline-src_lgtyp,
*             it_source-lgpla TO it_toline-src_lgpla.
*
*--- Get Destination Storage type/bin
      CLEAR : IT_DEST.
      READ TABLE IT_DEST WITH KEY MATNR = IT_TOLINE-MATNR.
      IF SY-SUBRC EQ 0.
        MOVE : IT_DEST-LGTYP TO IT_TOLINE-DES_LGTYP,
               IT_DEST-LGPLA TO IT_TOLINE-DES_LGPLA.
      ELSE.
        MOVE : 'XXX'         TO IT_TOLINE-DES_LGTYP,
               'XXXXXXXXXX'  TO IT_TOLINE-DES_LGPLA.
      ENDIF.

*--- BDC Processing of /nLT01
      PERFORM BDC_PROCESSING_LT01 TABLES   IT_BDCMSGCOLL
                                  USING    W_ZDOCNO
                                  CHANGING W_SUBRC.

      IF W_SUBRC EQ 0.
*--- Change TO Header (/nLT1A)
        CLEAR : WA_BDCMSGCOLL.
        READ TABLE IT_BDCMSGCOLL INTO WA_BDCMSGCOLL
                                 WITH KEY MSGTYP = 'S'.
        CHECK SY-SUBRC = 0.
        PERFORM BDC_PROCESSING_LTA1 TABLES   IT_BDCMSGCOLL
                                    USING    W_ZDOCNO
                                             WA_BDCMSGCOLL-MSGV1
                                    CHANGING W_SUBRC.
        IF W_SUBRC EQ 0.
          MOVE : 'C' TO L_STATS.
        ELSE.
          MOVE : 'H' TO L_STATS.
        ENDIF.
      ENDIF.
    ENDIF.

    MODIFY IT_TOLINE INDEX L_TABIX.

    MOVE-CORRESPONDING IT_TOLINE TO IT_ITAB.
    MOVE : W_ZDOCNO              TO IT_ITAB-W_DOCNO.
    MOVE : W_NSOLA               TO IT_ITAB-NSOLA.
    MOVE : L_STATS               TO IT_ITAB-STATS.

    IF W_MINUS_STOCK_CHECK EQ SPACE.
      IF W_SUBRC EQ 0.
        MOVE : C_GREEN             TO IT_ITAB-LINECOLOR,
               'S'                 TO IT_ITAB-MSGTY.
        CLEAR : IT_BDCMSGCOLL.
        READ TABLE IT_BDCMSGCOLL INDEX 1.
        MOVE : IT_BDCMSGCOLL-MSGV2 TO IT_ITAB-TANUM.
      ELSE.
        MOVE : C_RED               TO IT_ITAB-LINECOLOR,
               'E'                 TO IT_ITAB-MSGTY.
      ENDIF.
    ELSE.
      CLEAR : IT_BDCMSGCOLL, IT_BDCMSGCOLL[].
    ENDIF.
*--- message
    CLEAR : IT_BDCMSGCOLL, L_MESSA.
    READ TABLE IT_BDCMSGCOLL WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC EQ 0.
      PERFORM GET_MESSAGE USING    IT_BDCMSGCOLL-MSGID
                                   IT_BDCMSGCOLL-MSGNR
                                   IT_BDCMSGCOLL-MSGV1
                                   IT_BDCMSGCOLL-MSGV2
                                   IT_BDCMSGCOLL-MSGV3
                                   IT_BDCMSGCOLL-MSGV4
                          CHANGING L_MESSA.
    ELSE.
      READ TABLE IT_BDCMSGCOLL WITH KEY MSGTYP = 'S'.
      IF SY-SUBRC EQ 0.
        PERFORM GET_MESSAGE USING    IT_BDCMSGCOLL-MSGID
                                     IT_BDCMSGCOLL-MSGNR
                                     IT_BDCMSGCOLL-MSGV1
                                     IT_BDCMSGCOLL-MSGV2
                                     IT_BDCMSGCOLL-MSGV3
                                     IT_BDCMSGCOLL-MSGV4
                            CHANGING L_MESSA.
      ENDIF.
    ENDIF.
    MOVE : L_MESSA         TO IT_ITAB-MESSA.
    MOVE : IT_BDCMSGCOLL-MSGID TO IT_ITAB-MSGID,
           IT_BDCMSGCOLL-MSGNR TO IT_ITAB-MSGNR.
    APPEND IT_ITAB.
*---
    MOVE : IT_TOLINE-DISPO TO IT_DISPO-DISPO.
    COLLECT IT_DISPO.
    CLEAR : IT_TOLINE, IT_ITAB, IT_DISPO.
  ENDLOOP.
ENDFORM.                    " create_transfer_order

*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_TABLE.
*---
  DATA : IT_ZTMM_STL_LOG LIKE ZTMM_STL_LOG OCCURS 0 WITH HEADER LINE.

  CLEAR : IT_ZTMM_STL_LOG, IT_ZTMM_STL_LOG[].

  LOOP AT IT_ITAB.
    CLEAR : ZTMM_STL_LOG.
    MOVE-CORRESPONDING IT_ITAB TO IT_ZTMM_STL_LOG.
    MOVE : IT_ITAB-W_DOCNO     TO IT_ZTMM_STL_LOG-LOGNO_H,
           SY-TCODE            TO IT_ZTMM_STL_LOG-ZTCODE,
           SY-REPID            TO IT_ZTMM_STL_LOG-ZPROGRAMM.
    IT_ZTMM_STL_LOG-ERNAM = IT_ZTMM_STL_LOG-AENAM = SY-UNAME.
    IT_ZTMM_STL_LOG-ERDAT = IT_ZTMM_STL_LOG-AEDAT = SY-DATUM.
    IT_ZTMM_STL_LOG-ERZET = IT_ZTMM_STL_LOG-AEZET = SY-UZEIT.
    APPEND IT_ZTMM_STL_LOG.
  ENDLOOP.

  MODIFY ZTMM_STL_LOG FROM TABLE IT_ZTMM_STL_LOG.
ENDFORM.                    " update_table

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COMMENT_BUILD.
*---
  CLEAR : W_LINE.
  W_LINE-TYP  = 'H'.
  W_LINE-INFO = TEXT-006.
  APPEND W_LINE TO W_TOP_OF_PAGE.

  CLEAR : W_LINE.
  APPEND INITIAL LINE TO W_TOP_OF_PAGE.
ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_ALV_GRID.
*---
  MOVE : 'LINECOLOR' TO W_LAYOUT-INFO_FIELDNAME.
  W_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

  PERFORM BUILD_FIELDCAT.
  PERFORM BUILD_SORTCAT.

  CLEAR : W_PROGRAM.

  MOVE : SY-REPID TO W_PROGRAM.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM = W_PROGRAM
            IS_LAYOUT          = W_LAYOUT
            IT_FIELDCAT        = W_FIELDCAT[]
            IT_EVENTS          = W_EVENTCAT[]
            IT_SORT            = W_SORTCAT[]
            I_SAVE             = 'A'
       TABLES
            T_OUTTAB           = IT_ITAB
       EXCEPTIONS
            PROGRAM_ERROR      = 1
            OTHERS             = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : color
  APPEND_FIELDCAT :
    W_COL_POS 'MATNR'     18 'Material'       'CHAR' 'X' ''      '',
    W_COL_POS 'BDMNG'     12 'Quantity'       'QUAN' ''  'MEINS' '',
    W_COL_POS 'MEINS'     03 'UoM'            'UNIT' ''  ''      '',
    W_COL_POS 'WORKS'     05 'Workstation'    'CHAR' ''  ''      '',
    W_COL_POS 'RH_LH'     02 'RH/LH'          'CHAR' ''  ''      '',
    W_COL_POS 'FEEDING_TIME' 10 'Starting Time'
                                              'TIMS' ''  ''      '',
    W_COL_POS 'STOCK_CHECK'  01 'Stock Check'
                                              'CHAR' ''  ''      '',
    W_COL_POS 'FEED_CYCLE'   04 'Feed Cycle'
                                              'NUMC' ''  'MEINS' '',
    W_COL_POS 'ZTIME'     03 'Time for STL'   'NUMC' ''  ''      '',
    W_COL_POS 'VERME'     12 'Current Stock'  'QUAN' ''  'MEINS' '',
    W_COL_POS 'LPMIN'     12 'Safety Stock'   'QUAN' ''  'MEINS' '',
    W_COL_POS 'OPEN_TO'   12 'Open TO'        'QUAN' ''  'MEINS' '',
    W_COL_POS 'BFERRORQTY' 12 'B/F Error'     'QUAN' ''  'MEINS' '',
    W_COL_POS 'BF_TO_QTY' 12 'to B/F Qty'     'QUAN' ''  'MEINS' '',
    W_COL_POS 'NSOLA'     12 'Prev Qty'       'QUAN' ''  'MEINS' '',
    W_COL_POS 'RDMNG'     12 'Rounding Qty'   'QUAN' ''  'MEINS' '',
    W_COL_POS 'TQTY'      12 'TO Qty'         'QUAN' ''  'MEINS' '',
    W_COL_POS 'FEEDR'     05 'Feeder'         'CHAR' ''  ''      '',
    W_COL_POS 'TANUM'     10 'TO Number'      'CHAR' ''  ''      '',
    W_COL_POS 'SRC_LGTYP' 03 'Src S/Type'     'CHAR' ''  ''      '',
    W_COL_POS 'SRC_LGPLA' 10 'Src S/Bin'      'CHAR' ''  ''      '',
    W_COL_POS 'DES_LGTYP' 03 'Des S/Type'     'CHAR' ''  ''      '',
    W_COL_POS 'DES_LGPLA' 10 'Des S/Bin'      'CHAR' ''  ''      '',
    W_COL_POS 'MESSA'     80 'Message'        'CHAR' ''  ''      ''.
ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_SORTCAT.
**--- &1 : position     &2 : field name     &3 : tab name
**--- &4 : up           &5 : sub total
  APPEND_SORTCAT : '1' 'MATNR' 'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  get_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL_MSGID  text
*      -->P_IT_BDCMSGCOLL_MSGNR  text
*      -->P_IT_BDCMSGCOLL_MSGV1  text
*      -->P_IT_BDCMSGCOLL_MSGV2  text
*      -->P_IT_BDCMSGCOLL_MSGV3  text
*      -->P_IT_BDCMSGCOLL_MSGV4  text
*      <--P_L_MESSA  text
*----------------------------------------------------------------------*
FORM GET_MESSAGE USING    P_MSGID
                          P_MSGNR
                          P_MSGV1
                          P_MSGV2
                          P_MSGV3
                          P_MSGV4
                 CHANGING P_L_MESSA.
*---
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            MSGID               = P_MSGID
            MSGNR               = P_MSGNR
            MSGV1               = P_MSGV1
            MSGV2               = P_MSGV2
            MSGV3               = P_MSGV3
            MSGV4               = P_MSGV4
       IMPORTING
            MESSAGE_TEXT_OUTPUT = P_L_MESSA.
ENDFORM.                    " get_message

*&---------------------------------------------------------------------*
*&      Form  send_mail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_MAIL.
*---
  DATA : BEGIN OF AS_ABAPLIST OCCURS 0.
          INCLUDE STRUCTURE ABAPLIST.
  DATA : END OF AS_ABAPLIST.

  DATA : BEGIN OF AS_ALI_CONT OCCURS 0,
           LINE(255),
         END OF AS_ALI_CONT.

  DATA : BEGIN OF MS_MAILOBJECT_CONT OCCURS 0.
          INCLUDE STRUCTURE MCMAILOBJ.
  DATA : END OF MS_MAILOBJECT_CONT.

  DATA : L_OKCODE LIKE SY-UCOMM.

  CLEAR : AS_ABAPLIST, AS_ALI_CONT, MS_MAILOBJECT_CONT.
  REFRESH : AS_ABAPLIST, AS_ALI_CONT, MS_MAILOBJECT_CONT.

  CALL FUNCTION 'SAVE_LIST'
       EXPORTING
            LIST_INDEX         = '0'
       TABLES
            LISTOBJECT         = AS_ABAPLIST
       EXCEPTIONS
            LIST_INDEX_INVALID = 1
            OTHERS             = 2.

  CALL FUNCTION 'TABLE_COMPRESS'
       TABLES
            IN             = AS_ABAPLIST
            OUT            = AS_ALI_CONT
       EXCEPTIONS
            COMPRESS_ERROR = 1
            OTHERS         = 2.

  LOOP AT AS_ALI_CONT.
    MOVE : '1' TO MS_MAILOBJECT_CONT-OBJNR,
           '1' TO MS_MAILOBJECT_CONT-OBJLEVEL,
           'ALI' TO MS_MAILOBJECT_CONT-OBJTYPE,
           'Supply to Line' TO MS_MAILOBJECT_CONT-OBJNAM,
           'Supply to Line' TO MS_MAILOBJECT_CONT-OBJDES,
           AS_ALI_CONT-LINE TO MS_MAILOBJECT_CONT-OBJLINE.
    APPEND MS_MAILOBJECT_CONT.
  ENDLOOP.

  CALL FUNCTION 'MC_SEND_MAIL'
       EXPORTING
            MS_MAIL_SENDMODE          = 'O'
            MS_MAIL_TITLE             = 'Notification'
            MS_MAIL_DESCRIPTION       = 'Supply to Line Result'
            MS_MAIL_RECEIVER          = 'STLIMSIS'
*           MS_MAIL_EXPRESS           =
*           MS_MAIL_DLINAME           =
*           MS_MAIL_LANGU             = SY-LANGU
*           MS_MAIL_FUNKOBJ_TYP       = 'R'
*           MS_MAIL_FUNKOBJ_NAME      = 'RMCSMAIL'
       IMPORTING
            MS_OK_CODE                = L_OKCODE
       TABLES
            MS_MAIL_CONT              = MS_MAILOBJECT_CONT
*           MS_MAIL_FUNKOBJ_PARM      =
*           MS_MAIL_SETGET_PARM       =
*           MS_MAIL_RECEIVERS         =
       EXCEPTIONS
            SEND_ERROR                = 1
            CANCELED                  = 2
            NO_TITLE                  = 3
            NO_DESCRIPTION            = 4
            NO_RECEIVER_OR_DLI        = 5
            INVALID_SENDMODE          = 6
            NO_CONTENT                = 7
            INVALID_FUNCTIONAL_PARAMS = 8
            OTHERS                    = 9.
ENDFORM.                    " send_mail

*&---------------------------------------------------------------------*
*&      Form  get_working_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_WORKING_TIME CHANGING P_1SHIFT_OVERTIME
                               P_2SHIFT_OVERTIME.
*---
  DATA : L_DAYNR LIKE HRVSCHED-DAYNR,
         L_DAYTXT LIKE HRVSCHED-DAYTXT,
         L_DAYFREE LIKE HRVSCHED-NODAY,
         L_CURRENT_DATE TYPE D.

  CONSTANTS : C_UZEIT_000000 TYPE T VALUE '000000',
              C_UZEIT_035959 TYPE T VALUE '035959'.

  CLEAR : CRHD, KAKO, KAZY, IT_WORKTIME, IT_WORKTIME[], L_DAYNR,
          L_DAYTXT, L_DAYFREE.

*---
  IF SY-UZEIT GE C_UZEIT_000000 AND SY-UZEIT LE C_UZEIT_035959.
    L_CURRENT_DATE = SY-DATUM - 1.
  ELSE.
    L_CURRENT_DATE = SY-DATUM.
  ENDIF.

  SELECT SINGLE KAPID INTO CRHD-KAPID
                      FROM CRHD
                     WHERE OBJTY EQ 'A'
*                       AND arbpl EQ 'T'
                       AND ARBPL EQ P_ARBPL
                       AND WERKS EQ 'P001'.

  SELECT SINGLE MOSID INTO KAKO-MOSID
                      FROM KAKO
                     WHERE KAPID EQ CRHD-KAPID.

  SELECT SINGLE VERSN INTO KAZY-VERSN
                      FROM KAZY
                     WHERE KAPID EQ CRHD-KAPID
                       AND DATUB GE L_CURRENT_DATE.

  CALL FUNCTION 'RH_GET_DATE_DAYNAME'
       EXPORTING
            LANGU               = SY-LANGU
            DATE                = L_CURRENT_DATE
       IMPORTING
            DAYNR               = L_DAYNR
            DAYTXT              = L_DAYTXT
            DAYFREE             = L_DAYFREE
       EXCEPTIONS
            NO_LANGU            = 1
            NO_DATE             = 2
            NO_DAYTXT_FOR_LANGU = 3
            INVALID_DATE        = 4
            OTHERS              = 5.

  SELECT TAGNR
         SCHNR
         KAPTPROG
         B~BEGDA
         B~ENDDA
         B~BEGZT
         B~ENDZT
               INTO CORRESPONDING FIELDS OF TABLE IT_WORKTIME
               FROM KAPA AS A INNER JOIN TC37A AS B
                 ON A~MANDT EQ B~MANDT
                AND A~TPROG EQ B~KAPTPROG
              WHERE SCHGRUP EQ KAKO-MOSID
                AND KAPID   EQ CRHD-KAPID
                AND VERSN   EQ KAZY-VERSN
                AND BEGDA   LE L_CURRENT_DATE
                AND ENDDA   GE L_CURRENT_DATE
                AND TAGNR   EQ L_DAYNR.

  SORT IT_WORKTIME BY TAGNR SCHNR.

*---
  DATA : L_ENDZT LIKE IT_WORKTIME-ENDZT,
         L_BEGZT LIKE IT_WORKTIME-BEGZT.

  CLEAR : IT_WORKTIME, L_ENDZT.
  READ TABLE IT_WORKTIME WITH KEY SCHNR = 1.
  MOVE : IT_WORKTIME-ENDZT TO L_ENDZT.

  CLEAR : IT_WORKTIME, L_BEGZT.
  READ TABLE IT_WORKTIME WITH KEY SCHNR = 2.
  MOVE : IT_WORKTIME-BEGZT TO L_BEGZT.

  CLEAR : P_1SHIFT_OVERTIME, P_2SHIFT_OVERTIME.

*--- 1 shift overtime
  IF L_ENDZT EQ L_BEGZT.
    MOVE : 'X' TO P_1SHIFT_OVERTIME.
  ENDIF.

  CLEAR : IT_WORKTIME, L_ENDZT.
  READ TABLE IT_WORKTIME WITH KEY SCHNR = 2.
  MOVE : IT_WORKTIME-ENDZT TO L_ENDZT.

*--- 2 shift overtime
  IF L_ENDZT NE C_TIME_020000.
    MOVE : 'X' TO P_2SHIFT_OVERTIME.
  ENDIF.
ENDFORM.                    " get_working_time

*&---------------------------------------------------------------------*
*&      Form  decide_times
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DECIDE_TIMES.
*---

*--- insert by stlim (2004/07/28)
  PERFORM CALL_WORKING_TIME_FUNCTION.
*--- end of insert

*--- blocked by stlim (2004/07/28)
*  PERFORM get_calendar_id.
*  PERFORM get_working_day.
*  PERFORM get_work_time_per_day.
*  PERFORM get_break_time_per_day.
*  PERFORM get_1t_time_info.
*--- end of block

  PERFORM GET_TIME.


*  PERFORM save_running_time.
*  PERFORM read_feeding_start_time.

*  DATA : it_1shift LIKE it_ztmm_stl_time OCCURS 0 WITH HEADER LINE,
*         it_2shift LIKE it_ztmm_stl_time OCCURS 0 WITH HEADER LINE.
*
*  CLEAR : ztmm_stl_exec, ztmm_stl_time, it_ztmm_stl_time, w_tabix,
*          it_ztmm_stl_time[], w_max_exec_datetime, w_max_exec_time,
*          w_lines,
*          it_1shift, it_1shift[], it_2shift, it_2shift[].
*
*  SELECT MAX( dtime ) INTO w_max_exec_datetime
*                      FROM ztmm_stl_exec
*                     WHERE repid EQ sy-repid.
*
*  IF sy-subrc NE 0.
*    MESSAGE e999 WITH text-m01.
*  ENDIF.
*
*  MOVE : w_max_exec_datetime+8(6) TO w_max_exec_time.
*
**  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztmm_stl_time
**           FROM ztmm_stl_time.
**
**  IF sy-subrc NE 0.
**    MESSAGE e999 WITH text-m02.
**  ENDIF.
*
*  LOOP AT it_ztmm_stl_time.
*    MOVE-CORRESPONDING it_ztmm_stl_time TO it_1shift.
*    MOVE-CORRESPONDING it_ztmm_stl_time TO it_2shift.
*    IF it_ztmm_stl_time-overt EQ '1'.
*      MOVE : '0' TO it_1shift-overt.
*      APPEND it_1shift.
*    ELSEIF it_ztmm_stl_time-overt EQ '2'.
*      MOVE : '0' TO it_2shift-overt.
*      APPEND it_2shift.
*    ENDIF.
*    CLEAR : it_ztmm_stl_time, it_1shift, it_2shift.
*  ENDLOOP.
*
*  SORT it_ztmm_stl_time BY zindx rtime.
*
**--- overtime check
*  CLEAR : it_worktime, it_worktime[].
*
*  CALL FUNCTION 'Z_FMM_STL_OVERTIME'
*       EXPORTING
*            i_datum           = sy-datum
*            i_uzeit           = sy-uzeit
*       TABLES
*            t_worktime        = it_worktime
*       CHANGING
*            e_1shift_overtime = w_1shift_overtime
*            e_2shift_overtime = w_2shift_overtime.
*
*  IF w_1shift_overtime EQ space.     " no overtime
*    DELETE it_ztmm_stl_time WHERE overt EQ '1'.
*  ELSE.     " 1 shift overtime
*    LOOP AT it_ztmm_stl_time.
*      READ TABLE it_1shift WITH KEY zindx = it_ztmm_stl_time-zindx
*                                    rtime = it_ztmm_stl_time-rtime
*                                    overt = it_ztmm_stl_time-overt.
*      IF sy-subrc EQ 0.
*        DELETE it_ztmm_stl_time.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*
*  IF w_2shift_overtime EQ space.     " no overtime
*    DELETE it_ztmm_stl_time WHERE overt EQ '2'.
*  ELSE.     " 2 shift overtime
*    LOOP AT it_ztmm_stl_time.
*      READ TABLE it_2shift WITH KEY zindx = it_ztmm_stl_time-zindx
*                                    rtime = it_ztmm_stl_time-rtime
*                                    overt = it_ztmm_stl_time-overt.
*      IF sy-subrc EQ 0.
*        DELETE it_ztmm_stl_time.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*
*  DESCRIBE TABLE it_ztmm_stl_time LINES w_lines.
*
*  READ TABLE it_ztmm_stl_time WITH KEY rtime = w_max_exec_time.
*  MOVE : sy-tabix TO w_tabix.
*
*  w_tabix = w_tabix + 1.
*
*  IF w_tabix GT w_lines.
*    w_tabix = 1.
*  ENDIF.
*
*  READ TABLE it_ztmm_stl_time INDEX w_tabix.
*
*  CLEAR : ztmm_stl_exec, w_starting_time, w_ending_time.
**  MOVE : it_ztmm_stl_time-stime TO w_starting_time,
**         it_ztmm_stl_time-etime TO w_ending_time.
*
*  MOVE : sy-repid TO ztmm_stl_exec-repid.
*  CONCATENATE w_current_date it_ztmm_stl_time-rtime
*                                              INTO ztmm_stl_exec-dtime.
*  ztmm_stl_exec-erdat = ztmm_stl_exec-aedat = sy-datum.
*  ztmm_stl_exec-erzet = ztmm_stl_exec-aezet = sy-uzeit.
*  ztmm_stl_exec-ernam = ztmm_stl_exec-aenam = sy-uname.
*
**  INSERT ztmm_stl_exec.
*
**---
**  MOVE : w_current_time TO w_max_exec_time.
**  MOVE : it_ztmm_stl_time-rtime TO w_current_time.
*
**--- overtime check
**  PERFORM get_working_time CHANGING w_1shift_overtime
**                                    w_2shift_overtime.
ENDFORM.                    " decide_times
*&---------------------------------------------------------------------*
*&      Form  get_calendar_id
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_CALENDAR_ID.
*----- Read Shop Calendar ID of Trim line
  SELECT SINGLE C~KALID C~MOSID B~KAPID
    INTO (W_KALID, W_MOSID, W_KAPID)
    FROM CRHD AS A INNER JOIN CRCA AS B
                      ON A~OBJTY = B~OBJTY
                     AND A~OBJID = B~OBJID
                   INNER JOIN KAKO AS C
                      ON B~KAPID = C~KAPID
   WHERE A~WERKS =  P_WERKS
*     AND a~arbpl =  c_arbpl
     AND A~ARBPL =  P_ARBPL
     AND B~FORK2 =  'SAP006'.
ENDFORM.                    " get_calendar_id
*&---------------------------------------------------------------------*
*&      Form  get_next_working_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_WORKING_DAY.
*----- Read working day
*----- Previous : freeday => except
*----- Today    : freeday, not freeday => append
*----- Next     : not freeday => append

  DATA: LW_DATE  TYPE D,
        LW_LINES TYPE I,
        LW_DAYNR LIKE HRVSCHED-DAYNR,
        LW_DAYFREE LIKE HRVSCHED-NODAY.

  CLEAR: IT_WORK_DATE, IT_WORK_DATE[].
  LW_DATE = WA_DATUM - 1.

  DO.
    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
         EXPORTING
              LANGU               = SY-LANGU
              DATE                = LW_DATE
              CALID               = W_KALID
         IMPORTING
              DAYNR               = LW_DAYNR
              DAYFREE             = LW_DAYFREE
         EXCEPTIONS
              NO_LANGU            = 1
              NO_DATE             = 2
              NO_DAYTXT_FOR_LANGU = 3
              INVALID_DATE        = 4
              OTHERS              = 5.
    IF SY-SUBRC <> 0.
      MESSAGE E000(ZZ) WITH TEXT-M03.
    ENDIF.

    IF LW_DAYFREE EQ 'X' AND
       SY-INDEX NE 2.
      LW_DATE = LW_DATE + 1.
      CONTINUE.
    ENDIF.

    MOVE: LW_DATE  TO IT_WORK_DATE-DATE,
          LW_DAYNR TO IT_WORK_DATE-DAYNR.
    APPEND IT_WORK_DATE.

    LW_DATE = LW_DATE + 1.

    IF SY-INDEX >= 3.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " get_next_working_day
*&---------------------------------------------------------------------*
*&      Form  get_work_time_per_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_WORK_TIME_PER_DAY.
  DATA: LT_WORK_TIME LIKE IT_WORK_TIME OCCURS 0 WITH HEADER LINE.

  LOOP AT IT_WORK_DATE.
    DATA: LW_DATUB TYPE D.

    CLEAR: LT_WORK_TIME, LT_WORK_TIME[].

    SELECT MIN( DATUB ) INTO LW_DATUB
      FROM KAPA
     WHERE KAPID =  W_KAPID
       AND VERSN =  '01'
       AND DATUB => IT_WORK_DATE-DATE.

    SELECT KAPID VERSN DATUB TAGNR SCHNR
      INTO CORRESPONDING FIELDS OF TABLE LT_WORK_TIME
      FROM KAPA
     WHERE KAPID =  W_KAPID
       AND VERSN =  '01'
       AND DATUB =  LW_DATUB
       AND TAGNR = IT_WORK_DATE-DAYNR
     GROUP BY KAPID VERSN DATUB TAGNR SCHNR.

    LOOP AT LT_WORK_TIME.
      SELECT SINGLE * FROM KAPA WHERE KAPID = LT_WORK_TIME-KAPID
                                  AND VERSN = LT_WORK_TIME-VERSN
                                  AND DATUB = LT_WORK_TIME-DATUB
                                  AND TAGNR = LT_WORK_TIME-TAGNR
                                  AND SCHNR = LT_WORK_TIME-SCHNR.
      IF SY-SUBRC NE 0.
        MESSAGE E000(ZZ) WITH TEXT-M04.
      ENDIF.

      CLEAR: IT_WORK_TIME.

      IF KAPA-TPROG IS INITIAL.
        MOVE-CORRESPONDING LT_WORK_TIME TO IT_WORK_TIME.
        MOVE: IT_WORK_DATE-DATE TO IT_WORK_TIME-DATUM,
              KAPA-TPROG        TO IT_WORK_TIME-TPROG,
              KAPA-BEGZT        TO IT_WORK_TIME-BEGZT,
              KAPA-ENDZT        TO IT_WORK_TIME-ENDZT.
      ELSE.
        MOVE-CORRESPONDING LT_WORK_TIME TO IT_WORK_TIME.
        MOVE: IT_WORK_DATE-DATE TO IT_WORK_TIME-DATUM,
              KAPA-TPROG TO IT_WORK_TIME-TPROG.
        PERFORM READ_WORK_TIME_FROM_TC37A.
      ENDIF.

      APPEND IT_WORK_TIME.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " get_work_time_per_day
*&---------------------------------------------------------------------*
*&      Form  read_work_time_from_tc37a
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_WORK_TIME_FROM_TC37A.
  SELECT SINGLE BEGZT ENDZT PAPLAN PAPLAN
    INTO (IT_WORK_TIME-BEGZT,  IT_WORK_TIME-ENDZT,
          IT_WORK_TIME-PAPLAN, RG_PAPLAN-LOW)
    FROM TC37A
   WHERE SCHGRUP  =  W_MOSID
     AND KAPTPROG =  IT_WORK_TIME-TPROG
     AND ENDDA    => IT_WORK_DATE-DATE
     AND BEGDA    <= IT_WORK_DATE-DATE.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M05.
  ENDIF.

  MOVE: 'I'  TO RG_PAPLAN-SIGN,
        'EQ' TO RG_PAPLAN-OPTION.
  COLLECT RG_PAPLAN.
ENDFORM.                    " read_work_time_from_tc37a
*&---------------------------------------------------------------------*
*&      Form  set_it_day_work_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_IT_DAY_WORK_TIME.

ENDFORM.                    " set_it_day_work_time
*&---------------------------------------------------------------------*
*&      Form  get_break_time_per_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BREAK_TIME_PER_DAY.
*  DATA: lw_begzt LIKE sy-uzeit,   "Break time from
*        lw_endzt LIKE sy-uzeit,   "Break time to
*        lw_begtm LIKE sy-uzeit,   "Working time zone from
*        lw_endtm LIKE sy-uzeit,   "Working time zone to
*        lw_begac LIKE sy-uzeit,   "Break time in Working time zone from
*        lw_endac LIKE sy-uzeit.   "Break time in Working time zone to

  READ TABLE RG_PAPLAN INDEX 1.
  CHECK SY-SUBRC EQ 0.

  SELECT * INTO TABLE IT_BREAK
    FROM TC37P
   WHERE SCHGRUP =  W_MOSID
     AND PAPLAN  IN RG_PAPLAN
     AND PADAUER >  C_PADAUER.


*  LOOP AT lt_tc37p.
*    MOVE: lt_tc37p-paubeg TO lw_begzt,
*          lt_tc37p-pauend TO lw_endzt.
*    DO.
*      CLEAR: it_break_time.
*      IF lw_begzt(2) EQ lw_endzt(2).
*        CONCATENATE lw_begzt(2) '0000' INTO lw_begtm.
*        lw_endtm = lw_begtm + 3600.
*
*        lw_begac = lw_begzt.
*        lw_endac = lw_endzt.
*
*        READ TABLE it_break_time WITH KEY paplan = lt_tc37p-paplan
*                                          paubeg = lw_begtm
*                                          pauend = lw_endtm.
*        IF sy-subrc EQ 0.
*          it_break_time-padauer = it_break_time-padauer +
*                                  ABS( lw_endac - lw_begac ).
*          MODIFY it_break_time INDEX sy-tabix.
*        ELSE.
*          MOVE: lt_tc37p-paplan TO it_break_time-paplan,
*                lw_begtm        TO it_break_time-paubeg,
*                lw_endtm        TO it_break_time-pauend.
*          it_break_time-padauer = abs( lw_endac - lw_begac ).
*
*          APPEND it_break_time.
*        ENDIF.
*
*        EXIT.
*      ELSE.
*        CONCATENATE lw_begzt(2) '0000' INTO lw_begtm.
*        lw_endtm = lw_begtm + 3600.
*
*        IF sy-index EQ 1.
*          lw_begac = lw_begzt.
*          lw_endac = lw_endtm.
*        ELSE.
*          lw_begac = lw_begtm.
*          IF lw_endtm = '000000'.
*            lw_endac = '240000'.
*          ELSE.
*            lw_endac = lw_endtm.
*          ENDIF.
*        ENDIF.
*
*        READ TABLE it_break_time WITH KEY paplan = lt_tc37p-paplan
*                                          paubeg = lw_begtm
*                                          pauend = lw_endtm.
*        IF sy-subrc EQ 0.
*          it_break_time-padauer = it_break_time-padauer +
*                                  ABS( lw_endac - lw_begac ).
*          MODIFY it_break_time INDEX sy-tabix.
*        ELSE.
*          MOVE: lt_tc37p-paplan TO it_break_time-paplan,
*                lw_begtm        TO it_break_time-paubeg,
*                lw_endtm        TO it_break_time-pauend.
*          it_break_time-padauer = abs( lw_endac - lw_begac ).
*
*          APPEND it_break_time.
*        ENDIF.
*
*        IF lw_endac EQ lw_endzt.
*          EXIT.
*        ELSE.
*          lw_begzt = lw_endtm.
*        ENDIF.
*      ENDIF.
*    ENDDO.
*  ENDLOOP.
ENDFORM.                    " get_break_time_per_day
*&---------------------------------------------------------------------*
*&      Form  get_1t_time_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_1T_TIME_INFO.
  DATA: LW_DATUM LIKE   SY-DATUM,         "date
        LW_UZEIT LIKE   SY-UZEIT,         " time
        LW_BEGZT(14),      "start time
        LW_ENDZT(14),      "finish time
        LW_DTTMF(14),                     "Date/Time from
        LW_DTTMT(14),                     "Date/Time to
        LW_DATUMF_1T LIKE SY-DATUM,       "from 1T date
        LW_DATUMT_1T LIKE SY-DATUM,                         "to 1T date
        LW_BEGZT_1T LIKE SY-UZEIT,        "from 1T time
        LW_ENDZT_1T LIKE SY-UZEIT.        "to IT time

  LOOP AT IT_WORK_DATE.
    LOOP AT IT_WORK_TIME WHERE DATUM = IT_WORK_DATE-DATE.
      SORT IT_1T BY DATUM TPROG BEGZT.
      CLEAR: IT_1T.
      MOVE: IT_WORK_TIME-DATUM TO IT_1T-DATUM,
            IT_WORK_TIME-TPROG TO IT_1T-TPROG.
*            it_break-paplan    TO it_1t-paplan.

      CONCATENATE IT_WORK_TIME-DATUM IT_WORK_TIME-BEGZT
             INTO IT_1T-BEGZT.
      IF IT_WORK_TIME-BEGZT > IT_WORK_TIME-ENDZT.
        LW_DATUM = IT_WORK_TIME-DATUM + 1.
      ELSE.
        LW_DATUM = IT_WORK_TIME-DATUM.
      ENDIF.
      CONCATENATE LW_DATUM IT_WORK_TIME-ENDZT
             INTO IT_1T-ENDZT.

      APPEND IT_1T.

      LOOP AT IT_BREAK WHERE PAPLAN = IT_WORK_TIME-PAPLAN.
        CLEAR: IT_1T.
        CONCATENATE IT_WORK_TIME-DATUM IT_BREAK-PAUBEG
               INTO LW_BEGZT.
        IF IT_BREAK-PAUBEG > IT_BREAK-PAUEND.
          LW_DATUM = IT_WORK_TIME-DATUM.
          LW_DATUM = LW_DATUM + 1.
        ELSE.
          LW_DATUM = IT_WORK_TIME-DATUM.
        ENDIF.
        CONCATENATE LW_DATUM IT_BREAK-PAUEND
               INTO LW_ENDZT.

        LOOP AT IT_1T WHERE DATUM = IT_WORK_TIME-DATUM
                        AND TPROG = IT_WORK_TIME-TPROG
                        AND BEGZT <= LW_BEGZT
                        AND ENDZT >= LW_ENDZT.

          MOVE: IT_1T-ENDZT TO LW_ENDZT.

          IF IT_1T-BEGZT+8(6) > IT_BREAK-PAUBEG.
            LW_DATUM = IT_1T-BEGZT(8).
            LW_DATUM = LW_DATUM + 1.
          ELSE.
            LW_DATUM = IT_1T-BEGZT(8).
          ENDIF.
          CONCATENATE LW_DATUM IT_BREAK-PAUBEG
                 INTO IT_1T-ENDZT.

          MODIFY IT_1T.
        ENDLOOP.

        IF SY-SUBRC EQ 0.
          CLEAR: IT_1T.
          MOVE: IT_WORK_TIME-DATUM TO IT_1T-DATUM,
                IT_WORK_TIME-TPROG TO IT_1T-TPROG.
          CONCATENATE LW_DATUM IT_BREAK-PAUBEG
                 INTO IT_1T-BEGZT.
          IF IT_BREAK-PAUBEG > IT_BREAK-PAUEND.
            LW_DATUM = LW_DATUM + 1.
          ENDIF.
          CONCATENATE LW_DATUM IT_BREAK-PAUEND
                 INTO IT_1T-ENDZT.
          MOVE: 'B' TO IT_1T-FLAG.

          APPEND IT_1T.

          CLEAR: IT_1T.
          MOVE: IT_WORK_TIME-DATUM TO IT_1T-DATUM,
                IT_WORK_TIME-TPROG TO IT_1T-TPROG.
*                it_break-paplan    TO it_1t-paplan.
          CONCATENATE LW_DATUM IT_BREAK-PAUEND INTO IT_1T-BEGZT.
          IF IT_1T-BEGZT+8(6) > LW_ENDZT.
            LW_DATUM = LW_DATUM + 1.
          ENDIF.
          MOVE: LW_ENDZT TO IT_1T-ENDZT.

          APPEND IT_1T.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  SORT IT_1T BY DATUM TPROG BEGZT.
  PERFORM CACULATE_SECOND.

  PERFORM GET_FINALLY_FEED_TIME.
  PERFORM SET_TIME_ZONE_INDEX.
ENDFORM.                    " get_1t_time_info
*&---------------------------------------------------------------------*
*&      Form  get_1t_time_info_break
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_1T_TIME_INFO_BREAK.

ENDFORM.                    " get_1t_time_info_break
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_RTN.
ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  caculate_second
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CACULATE_SECOND.
  DATA: LW_DATUM LIKE SY-DATUM,
        LW_BEGZT LIKE SY-UZEIT,
        LW_ENDZT LIKE SY-UZEIT,
        LW_TIME  LIKE SY-UZEIT.

  LOOP AT IT_1T.
    LW_DATUM = IT_1T-BEGZT(8).
    LW_BEGZT = IT_1T-BEGZT+8(6).
    LW_ENDZT = IT_1T-ENDZT+8(6).

    LW_BEGZT = LW_BEGZT + 1.
    IF LW_BEGZT EQ '000001'.
      LW_DATUM = LW_DATUM + 1.
    ENDIF.

    CONCATENATE LW_DATUM LW_BEGZT INTO IT_1T-BEGZT.

    IF LW_BEGZT <= LW_ENDZT.
      IT_1T-SECOND = ABS( LW_ENDZT - LW_BEGZT ).
    ELSE.
      LW_TIME      = LW_BEGZT.
      IT_1T-SECOND = LW_ENDZT.
      IT_1T-SECOND = IT_1T-SECOND + 86400 - LW_TIME.
    ENDIF.

    MODIFY IT_1T.
  ENDLOOP.
ENDFORM.                    " caculate_second
*&---------------------------------------------------------------------*
*&      Form  get_finally_feed_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_FINALLY_FEED_TIME.
  DATA: LW_DATEF LIKE SY-DATUM,
        LW_DATET LIKE SY-DATUM,
        LW_BEGZT LIKE TC37A-BEGZT,
        LW_ENDZT LIKE TC37A-ENDZT,
        LW_LINES TYPE I.

  DATA: LT_1T LIKE IT_1T OCCURS 0 WITH HEADER LINE.

  LT_1T[] = IT_1T[].
  CLEAR: IT_1T, IT_1T[].

  SORT LT_1T BY DATUM TPROG BEGZT.
  LOOP AT LT_1T WHERE FLAG <> 'B'.
    DO.
      CLEAR: IT_1T.

      IF     LT_1T-SECOND <  1799.
        DESCRIBE TABLE IT_1T LINES LW_LINES.

        READ TABLE IT_1T INDEX LW_LINES.

        MOVE: IT_1T-ENDZT(8)   TO LW_DATET,
              IT_1T-ENDZT+8(6) TO LW_ENDZT.

        LW_ENDZT = LW_ENDZT + LT_1T-SECOND.
        IF LW_ENDZT(2) EQ '00'.
          LW_DATEF = LW_DATEF + 1.
        ENDIF.

        CONCATENATE LW_DATET LW_ENDZT INTO IT_1T-ENDZT.
        APPEND IT_1T.

        EXIT.
      ELSEIF LT_1T-SECOND >= 1799 AND LT_1T-SECOND <= 3599.
        MOVE: LT_1T TO IT_1T.
        APPEND IT_1T.
        EXIT.
      ELSEIF LT_1T-SECOND >  3599.
        MOVE LT_1T TO IT_1T.

        MOVE: IT_1T-BEGZT(8)   TO LW_DATEF,
              IT_1T-BEGZT+8(6) TO LW_BEGZT,
              IT_1T-ENDZT(8)   TO LW_DATET,
              IT_1T-ENDZT+8(6) TO LW_ENDZT.

        LW_ENDZT = LW_BEGZT + 3600 - 1.
        IF LW_ENDZT(2) EQ '00'.
          LW_DATET = LW_DATEF + 1.
        ELSE.
          LW_DATET = LW_DATEF.
        ENDIF.

        CONCATENATE LW_DATET LW_ENDZT INTO IT_1T-ENDZT.
        APPEND IT_1T.

        LT_1T-SECOND = LT_1T-SECOND - 3600.
        LT_1T-BEGZT  = IT_1T-ENDZT + 1.
      ENDIF.
    ENDDO.
  ENDLOOP.
ENDFORM.                    " get_finally_feed_time
*&---------------------------------------------------------------------*
*&      Form  set_time_zone_index
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_TIME_ZONE_INDEX.
  DATA: LW_INDEX TYPE I.

  LOOP AT IT_1T.
    AT NEW DATUM.
      CLEAR: LW_INDEX.
    ENDAT.

    LW_INDEX = LW_INDEX + 1.

    MOVE LW_INDEX TO IT_1T-INDEX.
    MODIFY IT_1T.
  ENDLOOP.
ENDFORM.                    " set_time_zone_index
*&---------------------------------------------------------------------*
*&      Form  read_feeding_start_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_FEEDING_START_TIME.
*----- read feeding start time

  DATA: LW_INDEX TYPE I,
        W_1T LIKE IT_1T.

  READ TABLE IT_1T INTO W_1T WITH KEY BEGZT = IT_1T-BEGZT.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M04.
  ENDIF.

  MOVE: SY-TABIX TO LW_INDEX.
  LW_INDEX = LW_INDEX + 1.

  READ TABLE IT_1T INDEX LW_INDEX.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M06.
  ENDIF.

  IF IT_1T-DATUM EQ W_1T-DATUM AND
     IT_1T-TPROG EQ W_1T-TPROG.
    MOVE: W_1T-BEGZT+8(6) TO W_STARTING_TIME,
          W_1T-ENDZT+8(6) TO W_ENDING_TIME,
          W_1T-BEGZT(8)   TO W_SCREEN_DATE,
          W_1T-ENDZT(8)   TO W_ENDING_DATE.
  ELSE.
    MOVE: IT_1T-BEGZT+8(6) TO W_STARTING_TIME,
          IT_1T-ENDZT+8(6) TO W_ENDING_TIME,
          IT_1T-BEGZT(8)   TO W_SCREEN_DATE,
          IT_1T-ENDZT(8)   TO W_ENDING_DATE.
  ENDIF.

*--- get previous worktime time
  LW_INDEX = LW_INDEX - 1.

  CLEAR : IT_1T.

  READ TABLE IT_1T INDEX LW_INDEX.

  IF SY-SUBRC EQ 0.
    MOVE : IT_1T-BEGZT(8)   TO W_PRE_S_DATE,
           IT_1T-BEGZT+8(6) TO W_PRE_S_TIME,
           IT_1T-ENDZT(8)   TO W_PRE_E_DATE,
           IT_1T-ENDZT+8(6) TO W_PRE_E_TIME.
  ENDIF.
ENDFORM.                    " read_feeding_start_time
*&---------------------------------------------------------------------*
*&      Form  get_exec_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_EXEC_TIME.
*---
  MOVE : P_CDATE TO WA_DATUM.

*  CLEAR: w_max_exec_datetime.
*
*  SELECT MAX( dtime ) INTO w_max_exec_datetime
*                      FROM ztmm_stl_exec
*                     WHERE repid EQ sy-repid.
*  IF w_max_exec_datetime IS INITIAL.
*    wa_datum = sy-datum.
*  ELSE.
*    wa_datum = w_max_exec_datetime(8).
*  ENDIF.
ENDFORM.                    " get_exec_time
*&---------------------------------------------------------------------*
*&      Form  save_running_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_RUNNING_TIME.
*---
  CLEAR : W_MAX_EXEC_DATETIME.

  CONCATENATE : P_CDATE P_CTIME INTO W_MAX_EXEC_DATETIME.

*  CONCATENATE : p_cdate sy-uzeit INTO w_max_exec_datetime.

*  SELECT MAX( dtime ) INTO w_max_exec_datetime
*                      FROM ztmm_stl_exec
*                     WHERE repid EQ sy-repid.
  IF W_MAX_EXEC_DATETIME IS INITIAL.
    LOOP AT IT_1T WHERE BEGZT <= W_DAYTIME
                    AND ENDZT >= W_DAYTIME.
    ENDLOOP.
  ELSE.
    LOOP AT IT_1T WHERE BEGZT <= W_MAX_EXEC_DATETIME
                    AND ENDZT >= W_MAX_EXEC_DATETIME.

    ENDLOOP.

    READ TABLE IT_1T WITH KEY BEGZT = IT_1T-BEGZT
                              ENDZT = IT_1T-ENDZT.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH TEXT-M06.
    ENDIF.

    DATA: LW_INDEX LIKE SY-INDEX.
    LW_INDEX = SY-TABIX + 1.

    READ TABLE IT_1T INDEX LW_INDEX.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH TEXT-M06.
    ENDIF.

    MOVE: IT_1T-BEGZT(8)   TO W_CURRENT_DATE,
          IT_1T-BEGZT+8(6) TO W_CURRENT_TIME.

    CLEAR: ZTMM_STL_EXEC.
    MOVE: SY-REPID    TO ZTMM_STL_EXEC-REPID,
          IT_1T-BEGZT TO ZTMM_STL_EXEC-DTIME,
          SY-UNAME    TO ZTMM_STL_EXEC-ERNAM,
          SY-UZEIT    TO ZTMM_STL_EXEC-ERZET,
          SY-DATUM    TO ZTMM_STL_EXEC-ERDAT,
          SY-UNAME    TO ZTMM_STL_EXEC-AENAM,
          SY-UZEIT    TO ZTMM_STL_EXEC-AEZET,
          SY-DATUM    TO ZTMM_STL_EXEC-AEDAT.
    INSERT ZTMM_STL_EXEC.
  ENDIF.
ENDFORM.                    " save_running_time

*&---------------------------------------------------------------------*
*&      Form  get_tqty_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_TQTY_NEW.
**--- logic which get quantity :
*     if supply to line master table 'STOCK_CHECK' = 'X'.
*       if ZTMM_MAST-ZMNMX = 'MIN'.
*         TO Qty. =    RESB-BDMNG(requirement qty)
*            1.      - LQUA-VERME(current stock(GESME))
*            2.      + Safety Stock(Supply to Line master table field)
*            3.      - open quantity
*            4.      + backflush error quantity
*            5.      + scheduled backflush quantity
*            6.      + Previous TO Qty.
*       else.
*         TO Qty. =
*            1.      - LQUA-VERME(current stock(GESME))
*            2.      + Safety Stock(Supply to Line master table field)
*            3.      - open quantity
*            4.      + backflush error quantity
*            5.      + scheduled backflush quantity
*       endif.

*---
  DATA : LV_TQTY LIKE RESB-BDMNG.  "Target Quantity.

*--- requirement quantity
  IF IT_TOLINE-STOCK_CHECK = 'X'.
    IF IT_TOLINE-ZMNMX EQ C_MIN.
      LV_TQTY = IT_TOLINE-BDMNG.
*      PERFORM get_previous_qty.
    ENDIF.
  ELSE.
    LV_TQTY = IT_TOLINE-BDMNG.
  ENDIF.

*--- check current stock     1.
  IF IT_TOLINE-STOCK_CHECK = 'X'.
    CLEAR : IT_STOCK.
    READ TABLE IT_STOCK WITH KEY MATNR = IT_TOLINE-MATNR.
    MOVE : IT_STOCK-GESME TO IT_TOLINE-VERME.
    LV_TQTY = LV_TQTY - IT_TOLINE-VERME.
  ENDIF.

*--- check safety stock     2.
  IF IT_TOLINE-STOCK_CHECK = 'X'.
    LV_TQTY = LV_TQTY + IT_TOLINE-LPMIN.
  ENDIF.

*--- check Open TO quantity     3.
  IF IT_TOLINE-STOCK_CHECK = 'X'.
    CLEAR : IT_OPEN.
    READ TABLE IT_OPEN WITH KEY MATNR = IT_TOLINE-MATNR.
    MOVE : IT_OPEN-VSOLA TO IT_TOLINE-OPEN_TO.
    LV_TQTY = LV_TQTY - IT_TOLINE-OPEN_TO.
  ENDIF.

*--- check backflush error quantity     4.
  IF IT_TOLINE-STOCK_CHECK = 'X'.
    CLEAR : IT_BFERROR.
    READ TABLE IT_BFERROR WITH KEY MATNR = IT_TOLINE-MATNR.
    MOVE : IT_BFERROR-ERFMG TO IT_TOLINE-BFERRORQTY.
    LV_TQTY = LV_TQTY + IT_TOLINE-BFERRORQTY.
  ENDIF.

*--- check scheduled backflush quantity     5.
  IF IT_TOLINE-STOCK_CHECK = 'X'.
    CLEAR : IT_BACK.
    READ TABLE IT_BACK WITH KEY MATNR = IT_TOLINE-MATNR.
    MOVE : IT_BACK-KOMP_QUANT TO IT_TOLINE-BF_TO_QTY.
    LV_TQTY = LV_TQTY + IT_TOLINE-BF_TO_QTY.
  ENDIF.

*--- check previous TO quantity     6.
  IF IT_TOLINE-STOCK_CHECK = 'X'.
    IF IT_TOLINE-ZMNMX EQ C_MIN.
      LV_TQTY = LV_TQTY + W_NSOLA.
    ENDIF.
  ENDIF.

*---
  IF LV_TQTY LT 1.
    MOVE : 'X' TO W_MINUS_STOCK_CHECK.
  ENDIF.


  IF W_MINUS_STOCK_CHECK EQ SPACE.
*--- rounding value check
*A. Get Remainder  :mod
*B. Get quotient   :div
    DATA : LV_TQTYMODRDMNG TYPE P,
           LV_TQTYDIVRDMNG TYPE P.

    CLEAR : IT_ROUND.
    READ TABLE IT_ROUND WITH KEY MATNR = IT_TOLINE-MATNR.
    MOVE : IT_ROUND-RDMNG TO IT_TOLINE-RDMNG.

    IF NOT IT_TOLINE-RDMNG IS INITIAL.
      LV_TQTYMODRDMNG = LV_TQTY MOD IT_TOLINE-RDMNG.
      LV_TQTYDIVRDMNG = LV_TQTY DIV IT_TOLINE-RDMNG.
    ENDIF.

    IF NOT LV_TQTYMODRDMNG IS INITIAL.
      LV_TQTYDIVRDMNG = LV_TQTYDIVRDMNG + 1.
      LV_TQTY = LV_TQTYDIVRDMNG * IT_TOLINE-RDMNG.
    ENDIF.
    IT_TOLINE-TQTY = LV_TQTY.   "Target Qty
  ELSE.
    IT_TOLINE-TQTY = 0.
  ENDIF.
ENDFORM.                    " get_tqty_new

*&---------------------------------------------------------------------*
*&      Form  get_previous_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_PREVIOUS_QTY.
*---
  CLEAR : LTAK, LTAP, W_NSOLA.

  SELECT SUM( NSOLA ) INTO W_NSOLA
                      FROM LTAK AS A INNER JOIN LTAP AS B
                        ON A~MANDT EQ B~MANDT
                       AND A~LGNUM EQ B~LGNUM
                       AND A~TANUM EQ B~TANUM
                     WHERE A~STDAT EQ W_PRE_S_DATE
                       AND A~ENDAT EQ W_PRE_E_DATE
                       AND A~STUZT EQ W_PRE_S_TIME
                       AND A~ENUZT EQ W_PRE_E_TIME
                       AND B~MATNR EQ IT_TOLINE-MATNR
*--- except cancel(delete)
                       AND NOT
                           ( ( B~PQUIT EQ 'X'  OR B~PVQUI EQ 'X' )
                         AND ( B~VORGA EQ 'ST' OR B~VORGA EQ 'SL' ) ).
ENDFORM.                    " get_previous_qty
*&---------------------------------------------------------------------*
*&      Form  get_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_TIME.
  DATA: LW_CURRENT(14),
        LW_INDEX LIKE SY-INDEX.

  CONCATENATE P_CDATE P_CTIME INTO LW_CURRENT.

  LOOP AT IT_1T WHERE BEGZT <= LW_CURRENT
                  AND ENDZT >= LW_CURRENT.

  ENDLOOP.
  IF SY-SUBRC NE 0.
    MESSAGE S000(ZZ) WITH TEXT-M10.
    LEAVE TO SCREEN 0.
  ENDIF.

  READ TABLE IT_1T WITH KEY BEGZT = IT_1T-BEGZT
                            ENDZT = IT_1T-ENDZT.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M06.
    LEAVE TO SCREEN 0.
  ENDIF.

*----- Set current working time zone
  MOVE: IT_1T-BEGZT+8(6) TO W_STARTING_TIME,
        IT_1T-ENDZT+8(6) TO W_ENDING_TIME,
        IT_1T-BEGZT(8)   TO W_SCREEN_DATE,
        IT_1T-ENDZT(8)   TO W_ENDING_DATE.

  DATA: LW_TIME LIKE SY-UZEIT.

  W_PRE_E_DATE = W_SCREEN_DATE.
  LW_TIME = IT_1T-BEGZT+8(6).
  W_PRE_E_TIME =  LW_TIME - 1.

*----- Set Previous working time zone
  LW_INDEX = SY-TABIX - 1.
  CLEAR : IT_1T.
  READ TABLE IT_1T INDEX LW_INDEX.
  IF SY-SUBRC NE 0.
    MESSAGE S000(ZZ) WITH TEXT-M11.
    LEAVE TO SCREEN 0.
  ENDIF.

  MOVE : IT_1T-BEGZT(8)   TO W_PRE_S_DATE,
         IT_1T-BEGZT+8(6) TO W_PRE_S_TIME.

*----- Check lunch time
*  DATA: lw_time LIKE sy-uzeit.
*
*  lw_time = w_starting_time - 1.
*
*  IF lw_time NE w_pre_e_time.
*    IF lw_time EQ '000000'.
*      w_pre_e_date = w_screen_date - 1.
*      w_pre_e_time = lw_time.
*    ELSE.
*      w_pre_e_date = w_screen_date.
*      w_pre_e_time = lw_time.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " get_time

*&---------------------------------------------------------------------*
*&      Form  call_working_time_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_WORKING_TIME_FUNCTION.
*---
  DATA : L_DATUM TYPE D.

  CLEAR : IT_DUMMY, IT_DUMMY[], IT_1T, IT_1T[], L_DATUM.

  PERFORM GET_PREVIOUS_DATE USING L_DATUM.

  CALL FUNCTION 'Z_FMM_GET_WORKING_TIME'
       EXPORTING
            I_DATUM              = L_DATUM
            I_DAY                = C_DAY
*            i_arbpl              = c_arbpl
            I_ARBPL              = P_ARBPL
       TABLES
            T_WORKING_TIME       = IT_DUMMY
            T_1T                 = IT_1T
       EXCEPTIONS
            CANNOT_READ_DAYNAME  = 1
            INCORRECT_SHIFT_INFO = 2
            INCORRECT_CAPA_INFO  = 3
            OTHERS               = 4.

  IF SY-SUBRC NE 0.
    CASE SY-SUBRC.
      WHEN 1.
        MESSAGE E999 WITH TEXT-M21.
      WHEN 2.
        MESSAGE E999 WITH TEXT-M22.
      WHEN 3.
        MESSAGE E999 WITH TEXT-M23.
      WHEN 4.
        MESSAGE E999 WITH TEXT-M24.
    ENDCASE.
  ENDIF.
ENDFORM.                    " call_working_time_function
*&---------------------------------------------------------------------*
*&      Form  get_previous_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATUM  text
*----------------------------------------------------------------------*
FORM GET_PREVIOUS_DATE USING    PW_DATUM.
  DATA: LW_KALID   LIKE KAKO-KALID,
        LW_DAYFREE LIKE HRVSCHED-NODAY.

*----- Read Shop Calendar ID, Capacity ID
  SELECT SINGLE B~KALID INTO LW_KALID
    FROM CRHD AS A INNER JOIN KAKO AS B
      ON A~KAPID = B~KAPID
   WHERE A~WERKS = P_WERKS
*     AND a~arbpl = c_arbpl
     AND A~ARBPL = P_ARBPL
     AND A~LVORM = ' '
     AND B~KAPAR = '001'.

  PW_DATUM = WA_DATUM.

  DO.
    PW_DATUM = PW_DATUM - 1.

    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
         EXPORTING
              LANGU               = SY-LANGU
              DATE                = PW_DATUM
              CALID               = LW_KALID
         IMPORTING
              DAYFREE             = LW_DAYFREE
         EXCEPTIONS
              NO_LANGU            = 1
              NO_DATE             = 2
              NO_DAYTXT_FOR_LANGU = 3
              INVALID_DATE        = 4
              OTHERS              = 5.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

    IF LW_DAYFREE EQ SPACE.
      EXIT.
    ENDIF.

    IF SY-INDEX EQ 200.
      PW_DATUM = WA_DATUM.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " get_previous_date
*&---------------------------------------------------------------------*
*&      Form  get_source_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SOURCE_TYPE.
  DATA: M_LGTYP LIKE MLGT-LGTYP,
        M_LGPLA LIKE MLGT-LGPLA.
  DATA: M_RESULT TYPE ZRESULT.
  LOOP AT IT_TOLINE.
    CLEAR: M_LGTYP, M_LGPLA, M_RESULT.

    IF IT_TOLINE-SRC_LGTYP IS INITIAL.

      CALL FUNCTION 'Z_MM_LT01_SOURCE_CHECK'
        EXPORTING
          P_MATNR       = IT_TOLINE-MATNR
*           P_TOQTY       = 0
      IMPORTING
*            E_MESS        =
          ZRESULT       = M_RESULT
          P_LGTYP       = M_LGTYP
          P_LGPLA       = M_LGPLA
                .
      IF M_RESULT EQ '0'.
        IT_TOLINE-SRC_LGTYP = M_LGTYP.
        IT_TOLINE-SRC_LGPLA = M_LGPLA.
        MODIFY IT_TOLINE TRANSPORTING SRC_LGTYP SRC_LGPLA.
        CLEAR IT_TOLINE.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " get_source_type
