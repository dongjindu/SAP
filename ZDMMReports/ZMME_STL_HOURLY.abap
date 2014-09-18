************************************************************************
* Program Name      : ZMME_STL_HOURLY
* Author            : Furong
* Creation Date     : 03/11
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       : Supply to Line - Main Program
*                     Change TO to Reservation
* Modification Logs
* Date            Developer        RequestNo      Description
************************************************************************

REPORT zmme_stl_hourly NO STANDARD PAGE HEADING
                        LINE-SIZE 400
                        MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

**---
DATA : w_subrc   LIKE sy-subrc.

*--- Working time
* Global variable
DATA:  w_kalid   LIKE kako-kalid,                  "Calender ID
       w_mosid   LIKE kako-mosid,                  "Schedule group
       w_kapid   LIKE kako-kapid.                  "Capacity ID

* Internal tables
DATA: BEGIN OF it_work_date OCCURS 0,                "Work day
        date   TYPE   d,
        daynr  LIKE   hrvsched-daynr,
      END   OF it_work_date.

DATA: it_break LIKE tc37p OCCURS 0 WITH HEADER LINE. "Break time

* Global variables
DATA: wa_datum LIKE sy-datum,
      wa_pre_datum LIKE sy-datum.

* Ranges
RANGES: rg_paplan FOR tc37a-paplan.                 "Break ID

*--- For BDC message
DATA : it_bdcmsgcoll LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
       wa_bdcmsgcoll LIKE LINE OF it_bdcmsgcoll.

* Variable for date & time
DATA : w_daytime(14).
DATA : w_current_date   TYPE sy-datum.
DATA : w_current_time   TYPE sy-uzeit.
DATA : w_screen_date    TYPE sy-datum.     " TO Start Date
DATA : w_screen_time    TYPE sy-uzeit.
DATA : w_dummy_date     TYPE sy-datum.  "Dummy date for temp use
DATA : w_dummy_minutes  TYPE num03.  "Dummy minutes for Cal of Start D&T

* Constants
*CONSTANTS: c_arbpl   LIKE crhd-arbpl VALUE 'S2L'. "Trim line shop name
*CONSTANTS: c_padauer LIKE tc37p-padauer VALUE '1200'. "break criterion
**---
* Itab & WA for Create TO(/nLT01)
*DATA : BEGIN OF WA_TOLINE,
*         MATNR        LIKE ZTMM_MAST-MATNR,
*         BDMNG        LIKE RESB-BDMNG,
*         MEINS        LIKE RESB-MEINS,
*         FEEDR        LIKE ZTMM_MAST-FEEDR,
*         WORKS        LIKE ZTMM_MAST-WORKS,
*         RH_LH        LIKE ZTMM_MAST-RH_LH,
*         SDATE        TYPE D,        "Start Date
*         FEEDING_TIME TYPE SY-UZEIT, "Start Time
*         EDATE        TYPE D,        "End Date
*         ETIME        TYPE T,        "End Time
*         STOCK_CHECK  LIKE ZTMM_MAST-STOCK_CHECK,
*         FEED_CYCLE   LIKE ZTMM_MAST-FEED_CYCLE,
*         ZTIME        LIKE ZTMM_MAST-ZTIME, "Time for STL
*         VERME        LIKE LQUA-VERME,      "Available stock
*         LPMIN        LIKE ZTMM_MAST-LPMIN,     " safety stock
*         OPEN_TO      LIKE LTAP-VSOLA,      "Open TO
*         BFERRORQTY   LIKE AFFW-ERFMG,      "Backkflush Error Qty
*         BF_TO_QTY    LIKE PPC1_ALL-KOMP_QUANT,   "to Backflush Qty.
*         RDMNG        LIKE MLGT-RDMNG,      "Rounding Qty
*         SRC_LGTYP    LIKE MLGT-LGTYP,      "Source Storage type
*         SRC_LGPLA    LIKE MLGT-LGPLA,      "Source Storage bin
*         DES_LGTYP    LIKE PKHD-LGTYP,      "Destination Storage type
*         DES_LGPLA    LIKE PKHD-LGPLA,      "Destination Storage bin
*         TQTY         LIKE RESB-BDMNG,      "Target Qty
*         DISPO        LIKE ZTMM_MAST-DISPO,
*         ZMNMX        LIKE ZTMM_MAST-ZMNMX, " Min or Max
*       END OF WA_TOLINE.

DATA : BEGIN OF wa_toline,
         matnr        LIKE pkhd-matnr,
         bdmng        LIKE resb-bdmng,
         meins        LIKE resb-meins,
         zfeeder      LIKE pkhd-zfeeder,
         zrhlh        LIKE pkhd-zrhlh,
         sdate        TYPE d,        "Start Date
         feeding_time TYPE sy-uzeit, "Start Time
         edate        TYPE d,        "End Date
         etime        TYPE t,        "End Time
         stock_check(1),
         labst        LIKE mard-labst,      "Available stock
         lpmin        LIKE pkhd-zzeisbe,     " safety stock
         open_qty     LIKE resb-bdmng,      "Open TO
         bferrorqty   LIKE affw-erfmg,      "Backkflush Error Qty
         bf_to_qty    LIKE ppc1_all-komp_quant,   "to Backflush Qty.
         rdmng        LIKE pkhd-behmg,      "Rounding Qty
         des_lgort    LIKE pvbe-lgort,
         src_sloc     LIKE pkhd-umlgo,
         des_prvbe    LIKE pkhd-prvbe,
         des_lgpla    LIKE pkhd-ablad,
         tqty         LIKE resb-bdmng,      "Target Qty
         dispo        LIKE marc-dispo,
         zmnmx(3), " Min or Max
       END OF wa_toline.


DATA : it_toline LIKE wa_toline OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_itab OCCURS 0.
        INCLUDE STRUCTURE it_toline.
DATA :   tanum LIKE ltap-tanum,     " TO number
         rsnum LIKE resb-rsnum,
         w_docno TYPE num10,
         linecolor(4),     " ALV Color
         messa(80),
         msgty LIKE ztmm_stl_log-msgty,
         msgid LIKE ztmm_stl_log-msgid,
         msgnr LIKE ztmm_stl_log-msgnr,
         nsola LIKE ltap-nsola,
         stats LIKE ztmm_stl_log-stats,
       END OF it_itab.

**REQUESTED_QTY LIKE  BAPI1172_REQUESTED_QTY
**DELIVERYTIME  LIKE  BAPI1172_DELVRYTIME
**STATUSCHANGERESULT  LIKE  BAPI1075_3

* Odd Even Flag
DATA : w_odd_even TYPE i.

**---
**** Constants&Vars for Number range object ****************************
CONSTANTS : c_nro_nr_00   VALUE '00' LIKE inri-nrrangenr, "Header Part
            c_nro_nr_01   VALUE '01' LIKE inri-nrrangenr, "Item Part
            c_nro_nr_09   VALUE '09' LIKE inri-nrrangenr, "App. Doc no.
            c_onehour   TYPE t VALUE '010000'.

*--- Number range object
DATA : w_nro_object  VALUE 'ZMMNRO0002' LIKE inri-object. "NRO for ZTLOG

*--- Number_Get_Next
DATA :      w_nro_number  TYPE num10.      " Same type of w_nro_object

DATA : w_zdocno TYPE num10.       "App. Doc. No.

*--- scheduled backflush quantity
DATA : BEGIN OF it_bfqty_temp OCCURS 0,
         matnr LIKE ztmm_ppc1_all-matnr,
         werks LIKE ztmm_ppc1_all-werks,
         lgort LIKE ztmm_ppc1_all-lgort,
         flg_asynch LIKE ztmm_ppc1_all-flg_asynch,
         sync_ind LIKE ztmm_ppc1_all-sync_ind,
         komp_quant LIKE ztmm_ppc1_all-komp_quant,
       END OF it_bfqty_temp.

DATA : it_back LIKE it_bfqty_temp OCCURS 0 WITH HEADER LINE.

*--- current stock
*DATA : BEGIN OF it_stock_temp OCCURS 0,
*         matnr LIKE lqua-matnr,
*         gesme LIKE lqua-gesme,
*         lgtyp LIKE pkhd-lgtyp,
*         lgpla LIKE pkhd-lgpla,
*       END OF it_stock_temp.
DATA : BEGIN OF it_stock_temp OCCURS 0,
         matnr LIKE lqua-matnr,
         werks LIKE mard-werks,
         lgort LIKE mard-lgort,
         labst LIKE lqua-gesme,
        END OF it_stock_temp.

DATA : it_stock LIKE it_stock_temp OCCURS 0 WITH HEADER LINE.

*--- open reservation quantity
DATA : BEGIN OF it_open_temp OCCURS 0,
         matnr LIKE resb-matnr,
         bdmng LIKE resb-bdmng,
       END OF it_open_temp.

DATA : it_open LIKE it_open_temp OCCURS 0 WITH HEADER LINE.

*--- backflush error quantity
DATA : BEGIN OF it_bferror_temp OCCURS 0,
         matnr LIKE affw-matnr,
         erfmg LIKE affw-erfmg,
         bwart LIKE affw-bwart,
         werks LIKE affw-werks,
         lgort LIKE affw-lgort,
       END OF it_bferror_temp.

DATA : it_bferror LIKE it_bferror_temp OCCURS 0 WITH HEADER LINE.

*--- rounding quantity
*DATA : BEGIN OF IT_ROUND_TEMP OCCURS 0,
*         MATNR LIKE MLGT-MATNR,
*         RDMNG LIKE MLGT-RDMNG,
*       END OF IT_ROUND_TEMP.

DATA : BEGIN OF it_round_temp OCCURS 0,
         matnr LIKE pkhd-matnr,
         meins LIKE pkhd-meins,
         behmg LIKE pkhd-behmg,
       END OF it_round_temp.

DATA : it_round LIKE it_round_temp OCCURS 0 WITH HEADER LINE.


DATA : BEGIN OF it_cc OCCURS 0,
         matnr LIKE pkhd-matnr,
         umlgo LIKE pkhd-umlgo,
         zfeeder LIKE pkhd-zfeeder,
         ablad LIKE pkhd-ablad,
         prvbe LIKE pkhd-prvbe,
         pknum LIKE pkhd-pknum,
       END OF it_cc.

*--- source storage bin/type
DATA : BEGIN OF it_source OCCURS 0,
         matnr LIKE mlgt-matnr,
         lgtyp LIKE mlgt-lgtyp,
         lgpla LIKE mlgt-lgpla,
       END OF it_source.

*--- destination storage bin/type
DATA : BEGIN OF it_dest OCCURS 0,
         matnr LIKE pkhd-matnr,
*A__ BY PAUL
*         LGTYP LIKE PKHD-LGTYP,
*         LGPLA LIKE PKHD-LGPLA,
         lgort LIKE pvbe-lgort,
         dispo LIKE marc-dispo,
*
       END OF it_dest.

*--- Standard Work time
DATA: BEGIN OF it_work_time OCCURS 0,
        kapid   LIKE   kapa-kapid,
        versn   LIKE   kapa-versn,
        tagnr   LIKE   kapa-tagnr,            "Day no
        schnr   LIKE   kapa-schnr,            "Shift No
        datub   LIKE   kapa-datub,            "Valid from
        datum   LIKE   sy-datum,              "Working date
        tprog   LIKE   kapa-tprog,            "Shift
        paplan  LIKE   tc37a-paplan,          "Break schedule
        begzt   LIKE   tc37a-begzt,           "Start time
        endzt   LIKE   tc37a-endzt,           "Finish time
      END   OF it_work_time.

*----- Work time(1T info)

*--- insert by stlim (2004/07/28)
DATA : it_1t LIKE zsmm_working_time_for_1t OCCURS 0 WITH HEADER LINE.
DATA : it_dummy LIKE zsmm_working_time OCCURS 0 WITH HEADER LINE.

CONSTANTS : c_day   TYPE i VALUE 10.
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
DATA : BEGIN OF it_dispo OCCURS 0,
         dispo LIKE marc-dispo,
       END OF it_dispo.

DATA : w_minus_stock_check(1).

DATA : it_ztmm_stl_time LIKE ztmm_stl_time OCCURS 0 WITH HEADER LINE,
       w_tabix LIKE sy-tabix,
       w_lines TYPE i.


**---
CONSTANTS : c_time_073000 TYPE t VALUE '073000',
            c_time_083000 TYPE t VALUE '083000',
            c_time_093000 TYPE t VALUE '093000',
            c_time_111500 TYPE t VALUE '111500',
            c_time_121500 TYPE t VALUE '121500',
            c_time_131500 TYPE t VALUE '131500',
            c_time_141500 TYPE t VALUE '141500',
            c_time_151500 TYPE t VALUE '151500',
            c_time_161500 TYPE t VALUE '161500',
            c_time_171500 TYPE t VALUE '171500',
            c_time_181500 TYPE t VALUE '181500',
            c_time_191500 TYPE t VALUE '191500',
            c_time_201500 TYPE t VALUE '201500',
            c_time_220000 TYPE t VALUE '220000',
            c_time_230000 TYPE t VALUE '230000',
            c_time_000000 TYPE t VALUE '000000',
            c_time_010000 TYPE t VALUE '010000',
            c_time_020000 TYPE t VALUE '020000',
            c_time_030000 TYPE t VALUE '030000',
            c_time_040000 TYPE t VALUE '040000',
            c_time_063000 TYPE t VALUE '063000'.

DATA : w_max_exec_datetime LIKE ztmm_stl_exec-dtime,
       w_max_exec_time TYPE t.
DATA : w_starting_time LIKE ztmm_stl_time-stime,
       w_ending_time   LIKE ztmm_stl_time-etime,
       w_ending_date   LIKE sy-datum.
DATA : w_1shift_overtime(1),
       w_2shift_overtime(1).

DATA : it_worktime LIKE zsmm_worktime OCCURS 0 WITH HEADER LINE.

DATA : w_pre_s_date TYPE d,
       w_pre_s_time TYPE t,
       w_pre_e_date TYPE d,
       w_pre_e_time TYPE t,
       w_nsola LIKE ltap-nsola.

CONSTANTS : c_min LIKE ztmm_mast-zmnmx VALUE 'MIN',
            c_mzx LIKE ztmm_mast-zmnmx VALUE 'MAX'.

**--- Macro
DEFINE append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-outputlen  = &3.
  w_fieldcat-seltext_l  = &4.
  w_fieldcat-seltext_m  = &4.
  w_fieldcat-seltext_s  = &4.
  w_fieldcat-datatype   = &5.
  w_fieldcat-key        = &6.
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

DEFINE append_sortcat.
  w_sortcat-spos      = &1.
  w_sortcat-fieldname = &2.
  w_sortcat-tabname   = &3.
  w_sortcat-up        = &4.
  w_sortcat-subtot    = &5.
  append w_sortcat.
  clear : w_sortcat.
END-OF-DEFINITION.

**---
DATA: d_ucomm LIKE sy-ucomm.
RANGES: r_zline FOR ztmm_mast-zline.

**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-003.
PARAMETERS : p_werks LIKE t001w-werks OBLIGATORY DEFAULT 'P001',
             p_lgort LIKE mard-lgort OBLIGATORY DEFAULT 'P400',
             p_cdate LIKE sy-datum OBLIGATORY,     "Current date
             p_ctime LIKE sy-uzeit OBLIGATORY,     "Current time
** Furong on 07/19/12 3 shift
             p_ddate LIKE sy-datum OBLIGATORY,     "Delivery date
** End
             p_dtime LIKE sy-uzeit OBLIGATORY.     "Delivery time
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-004.
PARAMETERS : p_arbpl LIKE crhd-arbpl OBLIGATORY,
             p_rp(2) TYPE c DEFAULT '06'.
SELECTION-SCREEN END OF BLOCK block2.

SELECT-OPTIONS : s_matnr FOR mara-matnr.

PARAMETERS : p_sql  TYPE c MODIF ID his,
             p_pada LIKE tc37p-padauer DEFAULT '1200' MODIF ID his.
PARAMETERS : p_test AS CHECKBOX.

**---
INITIALIZATION.
*  p_cdate = sy-datum.
*  p_ctime = sy-uzeit.
  PERFORM event_build USING w_eventcat[].


**---
TOP-OF-PAGE.
  PERFORM top_of_page.
*  PERFORM make_col_heading.

AT SELECTION-SCREEN.
  d_ucomm = sy-ucomm.
  PERFORM get_exec_time.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK screen-group1 = 'his' OR screen-group1 = 'HIS'.
    IF d_ucomm = 'HISNA'.
      screen-active = '1'.
    ELSE.
      screen-active = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

**---
START-OF-SELECTION.
  PERFORM get_data.

**---
END-OF-SELECTION.
  IF it_toline[] IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
    EXIT.
  ELSE.
    PERFORM execute_process.
    IF p_test = space.
      PERFORM update_table.
    ENDIF.

    PERFORM comment_build.     " USING w_top_of_page[].
    PERFORM make_alv_grid.
*    PERFORM send_mail.
  ENDIF.








*&----------------------------------------------------------------------
*FORM TIME_CALCULATION USING    VALUE(IM_DATE) TYPE D
*                               VALUE(IM_TIME) TYPE T
*                               VALUE(IM_MINUTES)
*                      CHANGING VALUE(EX_DATE) TYPE D
*                               VALUE(EX_TIME) TYPE T.
**---
*  CLEAR : EX_DATE, EX_TIME.
*  DATA : LV_TIME    TYPE T.
*  DATA : LV_HOURSUM TYPE P.
*
*  PERFORM GET_TIME_FROM_MINUTES USING    IM_MINUTES
*                                CHANGING LV_TIME.
*
*  LV_HOURSUM = IM_TIME(2) + LV_TIME(2).
*  EX_DATE = IM_DATE.
*  EX_TIME = IM_TIME + LV_TIME.
*  IF LV_HOURSUM >= 24.
*    EX_DATE = EX_DATE + 1.
*  ENDIF.
*
***---
*  MOVE : '00' TO EX_TIME+4(2).
*ENDFORM.                    " time_calculation

*&---------------------------------------------------------------------*
*&      Form  get_verme_lpmin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM get_verme_lpmin.
**---
*  CLEAR : it_stock_temp, it_stock_temp[], it_stock, it_stock[].
*
*  SELECT a~pknum
*         b~lgnum
*         b~lqnum
*         a~matnr
*         gesme     " quantity
*         a~lgtyp
*         a~lgpla
*               INTO CORRESPONDING FIELDS OF TABLE it_stock_temp
*               FROM pkhd AS a INNER JOIN lqua AS b
*                 ON a~mandt EQ b~mandt
*                AND a~matnr EQ b~matnr
*                AND a~lgtyp EQ b~lgtyp
*                AND a~lgpla EQ b~lgpla
*                AND b~bestq EQ space
*                FOR ALL ENTRIES IN it_toline
*              WHERE a~matnr EQ it_toline-matnr.
*
*  LOOP AT it_stock_temp.
*    MOVE : it_stock_temp-matnr TO it_stock-matnr,
*           it_stock_temp-gesme TO it_stock-gesme.
*    COLLECT it_stock.
*    CLEAR : it_stock_temp, it_stock.
*  ENDLOOP.
*ENDFORM.                    " get_verme_lpmin
*
*&---------------------------------------------------------------------*
*&      Form  get_open_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM GET_OPEN_TO.
**---
*  CLEAR : IT_OPEN_TEMP, IT_OPEN_TEMP[], IT_OPEN, IT_OPEN[].
*
*  SELECT A~LGNUM
*         A~TANUM
*         A~TAPOS
*         A~MATNR
*         A~VSOLA     "Source target quantity in alternate unit
*               INTO CORRESPONDING FIELDS OF TABLE IT_OPEN_TEMP
*               FROM LTAP AS A
*               INNER JOIN LTAK AS B
*               ON A~LGNUM = B~LGNUM
*               AND A~TANUM = B~TANUM
*                FOR ALL ENTRIES IN IT_TOLINE
*              WHERE A~MATNR EQ IT_TOLINE-MATNR
*                AND A~PQUIT EQ SPACE
*                AND A~LGNUM EQ 'P01'
*                AND B~BWLVS = '850'.
*  "Open TO(Indicator: confirmation complete)
*
*  LOOP AT IT_OPEN_TEMP.
*    MOVE-CORRESPONDING IT_OPEN_TEMP TO IT_OPEN.
*    COLLECT IT_OPEN.
*    CLEAR : IT_OPEN_TEMP, IT_OPEN.
*  ENDLOOP.
*ENDFORM.                    " get_open_to

*&---------------------------------------------------------------------*
*&      Form  get_bf_error_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bf_error_qty.
*---
  RANGES: rt_matnr FOR mara-matnr.

  CLEAR : it_bferror_temp, it_bferror_temp[], it_bferror, it_bferror[].

  CLEAR : affw.

*----- changed by bsbae. changed on 20040706
  LOOP AT it_toline.
    MOVE: 'I'  TO rt_matnr-sign,
          'EQ' TO rt_matnr-option,
          it_toline-matnr TO rt_matnr-low.
    APPEND rt_matnr.
  ENDLOOP.
  IF sy-subrc NE 0.
    MOVE: 'I'  TO rt_matnr-sign,
          'EQ' TO rt_matnr-option,
          '  ' TO rt_matnr-low.
    APPEND rt_matnr.
  ENDIF.

  SELECT matnr
         bwart
         werks
         lgort
         erfmg
               INTO CORRESPONDING FIELDS OF TABLE it_bferror_temp
               FROM affw
*                FOR ALL ENTRIES IN it_toline
*              WHERE matnr EQ it_toline-matnr
              WHERE matnr IN rt_matnr
*                AND werks EQ 'P001'
*                AND lgort EQ 'P400'
                AND bwart IN ('261', '262').
*----- changed by bsbae. changed on 20040706

  DELETE it_bferror_temp WHERE NOT ( werks EQ 'P001'
                                 AND lgort EQ 'P400' ).

  LOOP AT it_bferror_temp.
    IF it_bferror_temp-bwart EQ '262'.
      it_bferror_temp-bwart = it_bferror_temp-bwart * -1.
    ENDIF.

    MOVE : it_bferror_temp-matnr TO it_bferror-matnr,
           it_bferror_temp-erfmg TO it_bferror-erfmg.
    COLLECT it_bferror.
    CLEAR : it_bferror_temp, it_bferror.
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
FORM get_rdmng.
*---
  CLEAR : it_round, it_round[], it_round_temp, it_round_temp[].

  SELECT a~matnr
         b~meins
         b~behmg

    INTO CORRESPONDING FIELDS OF TABLE it_round_temp
    FROM marc AS a
   INNER JOIN pkhd AS b
      ON a~matnr = b~matnr
     AND a~werks = b~werks
**C__ Paul : Remove Condition 06/15/11
**     AND A~VSPVB = B~PRVBE

     FOR ALL ENTRIES IN it_toline

   WHERE a~matnr EQ it_toline-matnr
     AND a~werks EQ p_werks
     AND a~lvorm EQ space.

  LOOP AT it_round_temp.
    MOVE : it_round_temp-matnr TO it_round-matnr,
           it_round_temp-behmg TO it_round-behmg.
    COLLECT it_round.
    CLEAR : it_round_temp, it_round.
  ENDLOOP.



*  SELECT MATNR
*         LGNUM
*         LGTYP
*         RDMNG
*               INTO CORRESPONDING FIELDS OF TABLE IT_ROUND_TEMP
*               FROM MLGT
*                FOR ALL ENTRIES IN IT_TOLINE
*              WHERE MATNR EQ IT_TOLINE-MATNR
*** added by Furong on 07/29/2005
*                AND LGTYP EQ IT_TOLINE-SRC_LGTYP
*** end of addition
*                AND LVORM EQ SPACE.
*
*  LOOP AT IT_ROUND_TEMP.
*    MOVE : IT_ROUND_TEMP-MATNR TO IT_ROUND-MATNR,
*           IT_ROUND_TEMP-RDMNG TO IT_ROUND-RDMNG.
*    COLLECT IT_ROUND.
*    CLEAR : IT_ROUND_TEMP, IT_ROUND.
*  ENDLOOP.
ENDFORM.                    " get_rdmng

*&---------------------------------------------------------------------*
*&      Form  get_tqty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM GET_TQTY.
***--- logic which get quantity :
**     if supply to line master table 'STOCK_CHECK' = 'X'.
**  TO Qty. =    RESB-BDMNG(requirement qty)
**     1.      - LQUA-VERME(current stock(GESME))
**     2.      + Safety Stock(Supply to Line master table field)
**     3.      - open quantity
**     4.      + backflush error quantity
**     5.      + scheduled backflush quantity
***---
*
**---
*  DATA : LV_TQTY LIKE RESB-BDMNG.  "Target Quantity.
*
*  LV_TQTY = IT_TOLINE-BDMNG.
*
**--- check current stock     1.
*  IF IT_TOLINE-STOCK_CHECK = 'X'.
*    CLEAR : IT_STOCK.
*    READ TABLE IT_STOCK WITH KEY MATNR = IT_TOLINE-MATNR.
*    MOVE : IT_STOCK-LABST TO IT_TOLINE-LABST.
*    LV_TQTY = LV_TQTY - IT_TOLINE-LABST.
*  ENDIF.
*
**--- check safety stock     2.
*  IF IT_TOLINE-STOCK_CHECK = 'X'.
*    LV_TQTY = LV_TQTY + IT_TOLINE-LPMIN.
*  ENDIF.
*
**--- check Open TO quantity     3.
*  IF IT_TOLINE-STOCK_CHECK = 'X'.
*    CLEAR : IT_OPEN.
*    READ TABLE IT_OPEN WITH KEY MATNR = IT_TOLINE-MATNR.
*    MOVE : IT_OPEN-BDMNG TO IT_TOLINE-OPEN_QTY.
*    LV_TQTY = LV_TQTY - IT_TOLINE-OPEN_QTY.
*  ENDIF.
*
**--- check backflush error quantity     4.
*  IF IT_TOLINE-STOCK_CHECK = 'X'.
*    CLEAR : IT_BFERROR.
*    READ TABLE IT_BFERROR WITH KEY MATNR = IT_TOLINE-MATNR.
*    MOVE : IT_BFERROR-ERFMG TO IT_TOLINE-BFERRORQTY.
*    LV_TQTY = LV_TQTY + IT_TOLINE-BFERRORQTY.
*  ENDIF.
*
**--- check scheduled backflush quantity     5.
*  IF IT_TOLINE-STOCK_CHECK = 'X'.
*    CLEAR : IT_BACK.
*    READ TABLE IT_BACK WITH KEY MATNR = IT_TOLINE-MATNR
*         BINARY SEARCH.
*    IF SY-SUBRC = 0.
*      MOVE : IT_BACK-KOMP_QUANT TO IT_TOLINE-BF_TO_QTY.
*      LV_TQTY = LV_TQTY + IT_TOLINE-BF_TO_QTY.
*    ENDIF.
*  ENDIF.
*
*  IF LV_TQTY LT 1.
*    MOVE : 'X' TO W_MINUS_STOCK_CHECK.
*  ENDIF.
*
*
*  IF W_MINUS_STOCK_CHECK EQ SPACE.
**--- rounding value check
**A. Get Remainder  :mod
**B. Get quotient   :div
*    DATA : LV_TQTYMODRDMNG TYPE P,
*           LV_TQTYDIVRDMNG TYPE P.
*
*    CLEAR : IT_ROUND.
*    READ TABLE IT_ROUND WITH KEY MATNR = IT_TOLINE-MATNR.
*    MOVE : IT_ROUND-BEHMG TO IT_TOLINE-RDMNG.
*
*    IF NOT IT_TOLINE-RDMNG IS INITIAL.
*      LV_TQTYMODRDMNG = LV_TQTY MOD IT_TOLINE-RDMNG.
*      LV_TQTYDIVRDMNG = LV_TQTY DIV IT_TOLINE-RDMNG.
*    ENDIF.
*
*    IF NOT LV_TQTYMODRDMNG IS INITIAL.
*      LV_TQTYDIVRDMNG = LV_TQTYDIVRDMNG + 1.
*      LV_TQTY = LV_TQTYDIVRDMNG * IT_TOLINE-RDMNG.
*    ENDIF.
*    IT_TOLINE-TQTY = LV_TQTY.   "Target Qty
*  ELSE.
*    IT_TOLINE-TQTY = 0.
*  ENDIF.
*ENDFORM.                    " get_tqty

*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_NRO_NR_09  text
*      -->P_W_NRO_OBJECT  text
*      <--P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM number_get_next
        USING    value(p_w_nro_interval) LIKE inri-nrrangenr
                 value(p_w_nro_object)   LIKE inri-object
        CHANGING value(p_w_nro_next).
*---
  CLEAR: p_w_nro_next.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = p_w_nro_interval
      object                  = p_w_nro_object
    IMPORTING
      number                  = p_w_nro_next
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
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
*FORM GET_SORCE_STORAGE_TYPE_BIN.
**---
*  CLEAR : IT_SOURCE, IT_SOURCE[].
*
*  SELECT MATNR
*         LGTYP
*         LGPLA
*               INTO CORRESPONDING FIELDS OF TABLE IT_SOURCE
*               FROM MLGT
*                FOR ALL ENTRIES IN IT_TOLINE
*              WHERE MATNR EQ IT_TOLINE-MATNR
*** added by Furong on 07/29/2005
*                AND LGTYP EQ IT_TOLINE-SRC_LGTYP
*** end of addition
*                AND LVORM EQ SPACE.
*ENDFORM.                    " get_sorce_storage_type_bin

*&---------------------------------------------------------------------*
*&      Form  get_des_storage_type_bin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM GET_DES_STORAGE_TYPE_BIN.
**---
*  CLEAR : IT_DEST, IT_DEST[].
*
*  SELECT MATNR
*         LGTYP
*         LGPLA
*               INTO CORRESPONDING FIELDS OF TABLE IT_DEST
*               FROM PKHD
*                FOR ALL ENTRIES IN IT_TOLINE
*              WHERE MATNR EQ IT_TOLINE-MATNR.
*ENDFORM.                    " get_des_storage_type_bin

*&---------------------------------------------------------------------*
*&      Form  bdc_processing_lt01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      -->P_W_ZDOCNO  text
*      <--P_W_SUBRC  text
*----------------------------------------------------------------------*
*FORM BDC_PROCESSING_LT01 TABLES   EXT_BDCMSGCOLL STRUCTURE BDCMSGCOLL
*                         USING    VALUE(P_ZDOCNO)
*                         CHANGING VALUE(P_SUBRC).
**---
*  CLEAR : EXT_BDCMSGCOLL, EXT_BDCMSGCOLL[], P_SUBRC.
*
*  DATA : LV_BWLVS_002     TYPE BDCDATA-FVAL,   "Movement type
*         LV_MATNR_003     TYPE BDCDATA-FVAL,
*         LV_ANFME_004     TYPE BDCDATA-FVAL,
*         LV_ANFME_007     TYPE BDCDATA-FVAL,
*         LV_ALTME_008     TYPE BDCDATA-FVAL,
*         LV_VLTYP_009     TYPE BDCDATA-FVAL,
*         LV_VLPLA_010     TYPE BDCDATA-FVAL,
*         LV_NLTYP_011     TYPE BDCDATA-FVAL,
*         LV_NLPLA_012     TYPE BDCDATA-FVAL,
*         LV_REFNR_013     TYPE BDCDATA-FVAL.   "Group(Feeder)
*** CHANGE NOT USE FEEDER NO MRE PAUL 06/10/11
*** changed on 07/27/2006 by Furong
***  IF IT_TOLINE-FEEDR = 'JIS'.
***    LV_BWLVS_002 = '977'.
***  ELSE.
***    LV_BWLVS_002 = '850'.  "Movement type
***  ENDIF.
***** end of change
***  LV_REFNR_013 = IT_TOLINE-FEEDR. "Group(Feeder)
**E_<
*  LV_MATNR_003  = IT_TOLINE-MATNR. "Material '327003K100'
*  LV_ANFME_004  = IT_TOLINE-TQTY.
*  LV_ANFME_007  = IT_TOLINE-TQTY.
*  LV_ALTME_008  = IT_TOLINE-MEINS.
**  LV_VLTYP_009  = IT_TOLINE-SRC_LGTYP.  "Src Storage Type '434'
**  LV_VLPLA_010  = IT_TOLINE-SRC_LGPLA.  "Src Storage Bin  'AA-01-11'
**  LV_NLTYP_011  = IT_TOLINE-DES_LGTYP.  "Des Storage Type '443'
**  LV_NLPLA_012  = IT_TOLINE-DES_LGPLA.  "Des Storage Bin  'TS-01'
**
*  CONDENSE : LV_BWLVS_002,  "Movement type
*             LV_MATNR_003,
*             LV_ANFME_004,
*             LV_ANFME_007,
*             LV_ALTME_008,
*             LV_VLTYP_009,
*             LV_VLPLA_010,
*             LV_NLTYP_011,
*             LV_NLPLA_012,
*             LV_REFNR_013.
*
**--- BDC for LT01(Create TO)
*  CALL FUNCTION 'Z_FMM_6012_01'
*       EXPORTING
*            LGNUM_001 = 'P01'  "Warehouse number
*            REFNR_013 = LV_REFNR_013  "Group(Feeder)
*            BWLVS_002 = LV_BWLVS_002  "Movement type '850'
*            MATNR_003 = LV_MATNR_003  "Material '327003K100'
*            ANFME_004 = LV_ANFME_004
*            WERKS_005 = 'P001'  "Plant
*            LGORT_006 = 'P400'  "Storage Location
*            ANFME_007 = LV_ANFME_007
*            ALTME_008 = LV_ALTME_008
*            VLTYP_009 = LV_VLTYP_009  "Src Storage Type '434'
*            VLPLA_010 = LV_VLPLA_010  "Src Storage Bin 'AA-01-11'
*            NLTYP_011 = LV_NLTYP_011  "Des Storage Type '443'
*            NLPLA_012 = LV_NLPLA_012  "Des Storage Bin 'TS-01'
*       IMPORTING
*            SUBRC     = P_SUBRC
*       TABLES
*            MESSTAB   = EXT_BDCMSGCOLL[].
*
**--- One More Try
*  IF P_SUBRC NE 0.
*    CALL FUNCTION 'Z_FMM_6012_01'
*         EXPORTING
*              LGNUM_001 = 'P01'  "Warehouse number
*              REFNR_013 = LV_REFNR_013  "Group(Feeder)
*              BWLVS_002 = LV_BWLVS_002  "Movement type '850'
*              MATNR_003 = LV_MATNR_003  "Material '327003K100'
*              ANFME_004 = LV_ANFME_004  " Quantity
*              WERKS_005 = 'P001'  "Plant
*              LGORT_006 = 'P400'  "Storage Location
*              ANFME_007 = LV_ANFME_007  " Quantity
*              ALTME_008 = LV_ALTME_008  " Unit of Measure
*              VLTYP_009 = LV_VLTYP_009  "Src Storage Type '434'
*              VLPLA_010 = LV_VLPLA_010  "Src Storage Bin 'AA-01-11'
*              NLTYP_011 = LV_NLTYP_011  "Des Storage Type '443'
*              NLPLA_012 = LV_NLPLA_012  "Des Storage Bin 'TS-01'
*         IMPORTING
*              SUBRC     = P_SUBRC
*         TABLES
*              MESSTAB   = EXT_BDCMSGCOLL[].
*  ENDIF.
*ENDFORM.                    " bdc_processing_lt01

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
*FORM BDC_PROCESSING_LTA1 TABLES   EXT_BDCMSGCOLL STRUCTURE BDCMSGCOLL
*                         USING    VALUE(P_ZDOCNO)
*                                  VALUE(P_MSGV1)
*                         CHANGING VALUE(P_SUBRC).
**---
*  CLEAR : EXT_BDCMSGCOLL, EXT_BDCMSGCOLL[], P_SUBRC.
*
*  DATA : LV_TANUM_001     TYPE BDCDATA-FVAL,  "TO number
*         LV_LGNUM_002     TYPE BDCDATA-FVAL,  "Warehouse number
*         LV_STDAT_003     TYPE BDCDATA-FVAL,  "Start date
*         LV_STUZT_004     TYPE BDCDATA-FVAL,  "Start time
*         LV_ENDAT_005     TYPE BDCDATA-FVAL,  "End date
*         LV_ENUZT_006     TYPE BDCDATA-FVAL.  "End time
*
*  DATA : LV_DATE(8).
*
*  CLEAR : LV_DATE.
*
*  PERFORM USER_DATE_FORMAT USING    SY-UNAME
*                                    IT_TOLINE-SDATE
*                           CHANGING LV_DATE.
*
*  LV_STDAT_003 = LV_DATE.        "Start date
*  LV_TANUM_001 = P_MSGV1.                              "TO number
*'813'
*  LV_LGNUM_002 = 'P01'.                                "Warehouse
*number
*  LV_STUZT_004 = IT_TOLINE-FEEDING_TIME.             "Start time
*
**  MOVE : w_ending_date TO it_toline-edate.
**  MOVE : w_ending_time TO it_toline-etime.
*
**  PERFORM time_calculation USING    it_toline-sdate
**                                    it_toline-feeding_time
**                                    60   "Minutes
**                           CHANGING it_toline-edate
**                                    it_toline-etime.
*
**  MOVE : w_ending_time TO it_toline-etime.
*
*  LV_ENUZT_006 = IT_TOLINE-ETIME. "End time
*
*  CLEAR : LV_DATE.
*  PERFORM USER_DATE_FORMAT USING    SY-UNAME
*                                    IT_TOLINE-EDATE
*                           CHANGING LV_DATE.
*
*  LV_ENDAT_005 = LV_DATE.      "End date
*
*  CONDENSE : LV_TANUM_001,
*             LV_LGNUM_002,
*             LV_STDAT_003,
*             LV_STUZT_004,
*             LV_ENDAT_005,
*             LV_ENUZT_006.
*
**--- BDC for LTA1(Change TO Header)
*  CALL FUNCTION 'Z_FMM_6012_02'
*       EXPORTING
*            TANUM_001 = LV_TANUM_001  " TO number '813'
*            LGNUM_002 = LV_LGNUM_002
*            STDAT_003 = LV_STDAT_003  "Start date
*            STUZT_004 = LV_STUZT_004  "Start time "'10:36:48'
*            ENDAT_005 = LV_ENDAT_005  "End date
*            ENUZT_006 = LV_ENUZT_006  "End time
*       IMPORTING
*            SUBRC     = P_SUBRC
*       TABLES
*            MESSTAB   = EXT_BDCMSGCOLL[].
*
**--- BDC Log to the table ZTLOG
*  IF EXT_BDCMSGCOLL[] IS INITIAL.  "SUCCESS
*    CLEAR : WA_BDCMSGCOLL.
*    WA_BDCMSGCOLL-TCODE   = 'LT1A'.
*    WA_BDCMSGCOLL-MSGTYP  = 'S'.  "SUCCESS
*    WA_BDCMSGCOLL-MSGSPRA = 'E'.
*    WA_BDCMSGCOLL-MSGID   = 'ZMMM'.
*    WA_BDCMSGCOLL-MSGNR   = '999'.
*    WA_BDCMSGCOLL-MSGV1   = 'Transfer order'.
*    WA_BDCMSGCOLL-MSGV2   = LV_TANUM_001.
*    WA_BDCMSGCOLL-MSGV3   = 'Start/End Date/Time'.
*    WA_BDCMSGCOLL-MSGV4   = 'is changed.'.
*    APPEND WA_BDCMSGCOLL TO EXT_BDCMSGCOLL[].
*  ENDIF.
*ENDFORM.                    " bdc_processing_lta1

*&---------------------------------------------------------------------*
*&      Form  write_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM WRITE_LIST.
***---
**  WRITE : /1(18)  <fs_toline>-matnr RIGHT-JUSTIFIED,
**          20(10)  <fs_toline>-bdmng UNIT <fs_toline>-meins,
**          31      <fs_toline>-meins,
**          36      <fs_toline>-works,
**          50      <fs_toline>-rh_lh,
**          60      <fs_toline>-feeding_time,  "Start time(Feeding Time)
**          75      <fs_toline>-stock_check,
**          90      <fs_toline>-feed_cycle,
**         115      <fs_toline>-ztime,
**         155      <fs_toline>-verme UNIT <fs_toline>-meins,
**         180      <fs_toline>-lpmin UNIT <fs_toline>-meins,
**         220      <fs_toline>-open_to UNIT <fs_toline>-meins,
**         240      <fs_toline>-bferrorqty UNIT <fs_toline>-meins,
**         270      <fs_toline>-bf_to_qty UNIT <fs_toline>-meins,
**         300      <fs_toline>-rdmng UNIT <fs_toline>-meins,
**         325      <fs_toline>-tqty UNIT <fs_toline>-meins,
**         355      <fs_toline>-feedr,      "Feeder
**         365      wa_bdcmsgcoll-msgv1 COLOR COL_POSITIVE.
*ENDFORM.                    " write_list

*&---------------------------------------------------------------------*
*&      Form  make_col_heading
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM MAKE_COL_HEADING.
**---
*  WRITE: /1   'Material',
*          20(9) 'Quantity' RIGHT-JUSTIFIED,
*          31  'Unit',
*          36  'Workstation',
*          50  'RH/LH',
*          60  'Start Time',  "Feeding Time
*          75  'Stock Check',
*          90  'Feed Cycle',
*          115  'Time for STL',
*          155 'Available stock(GESME)',
*          180 'Safety Stock(LPMIN)',
**         155  'Available stock(VERME)',
**         180 'Minimum storage bin quantity(LPMIN)',
*          220 'Open TO',
*          240 'Backflush Error Check',
*          270 'Scheduled Backflush Qty',
*          300 'Rounding Quantity Check',
*          325 'Target Quantity(TO)',
*          355 'Feeder'.
*ENDFORM.                    " make_col_heading

*&---------------------------------------------------------------------*
*&      Form  get_time_from_minutes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IM_MINUTES  text
*      <--P_LV_TIME  text
*----------------------------------------------------------------------*
*FORM GET_TIME_FROM_MINUTES USING    VALUE(IM_MINUTES)
*                           CHANGING VALUE(EX_TIME) TYPE T.
**---
*  CLEAR : EX_TIME.
*  DATA : BEGIN OF LS_TIME,
*           HOUR(2) TYPE N,
*           MINUTE(2) TYPE N,
*           SECOND(2) TYPE N,
*         END OF LS_TIME.
*
*  LS_TIME-MINUTE = IM_MINUTES MOD 60.
*  LS_TIME-HOUR   = IM_MINUTES DIV 60.
*
*  MOVE LS_TIME TO EX_TIME.
*ENDFORM.                    " get_time_from_minutes

*&---------------------------------------------------------------------*
*&      Form  user_date_format
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UNAME  text
*      -->P_<FS_TOLINE>_SDATE  text
*      <--P_LV_DATE  text
*----------------------------------------------------------------------*
FORM user_date_format USING    value(p_user)     LIKE sy-uname
                               value(p_date)     LIKE sy-datum
                      CHANGING value(p_userdate) TYPE char8.
*---
  CLEAR : p_userdate.

  DATA : yyyy(4).  "year
  DATA : mm(2).    "day
  DATA : dd(2).    "month
  DATA : datfm LIKE usr01-datfm.  "date format

  SELECT SINGLE datfm INTO datfm
                      FROM usr01
                     WHERE bname = p_user.

** datfm
*1 DD.MM.YYYY
*2 MM/DD/YYYY
*3 MM-DD-YYYY
*4 YYYY.MM.DD
*5 YYYY/MM/DD
*6 YYYY-MM-DD
  yyyy = p_date+0(4).
  mm   = p_date+4(2).
  dd   = p_date+6(2).

  CASE datfm.
    WHEN 1.
      p_userdate+0(2) = dd.
      p_userdate+2(2) = mm.
      p_userdate+4(4) = yyyy.
    WHEN 2 OR 3.
      p_userdate+0(2) = mm.
      p_userdate+2(2) = dd.
      p_userdate+4(4) = yyyy.
    WHEN 4 OR 5 OR 6.
      p_userdate+0(4) = yyyy.
      p_userdate+4(2) = mm.
      p_userdate+6(2) = dd.
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
FORM get_scheduled_bf_qty.
*---
  CLEAR : it_bfqty_temp, it_bfqty_temp[], it_back, it_back[].

  DATA: BEGIN OF lt_ppc OCCURS 100,
          headid LIKE ppc_head-headid,
        END OF lt_ppc.

  DATA $it_back LIKE it_back OCCURS 0 WITH HEADER LINE.

  IF p_sql = 'E' OR p_sql = space.
    SELECT headid
         INTO TABLE lt_ppc
         FROM ppc_head
         WHERE ( flg_asynch EQ ' ' OR  flg_asynch EQ 'B' )
*          AND postdate BETWEEN wa_pre_datum AND wa_datum
         %_HINTS ORACLE 'FIRST_ROWS(10) INDEX("PPC_HEAD" "PPC_HEAD~ASY")'.

    LOOP AT lt_ppc.
      SELECT matnr
             werks
             lgort
             flg_asynch
             sync_ind
             SUM( komp_quant )    " quantity
           APPENDING TABLE $it_back
           FROM ztmm_ppc1_all
           WHERE headid = lt_ppc-headid
             AND ( postdate BETWEEN wa_pre_datum AND wa_datum )
             AND werks  = p_werks
             AND lgort  = p_lgort
           GROUP BY matnr werks lgort flg_asynch sync_ind.

    ENDLOOP.

    LOOP AT $it_back.
      it_back = $it_back.
      COLLECT it_back.
    ENDLOOP.


*    SELECT matnr
*           werks
*           lgort
*           flg_asynch
*           sync_ind
*           SUM( komp_quant )    " quantity
*         INTO TABLE it_back
*         FROM ztmm_ppc1_all
*         WHERE ( flg_asynch eq ' ' or  flg_asynch eq 'B' )
*          AND postdate BETWEEN wa_pre_datum AND wa_datum
*           AND werks = p_werks
*           AND lgort = p_lgort
*         GROUP by matnr werks lgort flg_asynch sync_ind
*         %_HINTS ORACLE 'FIRST_ROWS(10)
*         INDEX("PPC_HEAD" "PPC_HEAD~ASY")'.

  ELSEIF p_sql = 'D'.
    SELECT matnr
           werks
           lgort
           komp_quant     " quantity
           flg_asynch
           sync_ind
                     INTO CORRESPONDING FIELDS OF TABLE it_bfqty_temp
                      FROM ztmm_ppc1_all
                     WHERE flg_asynch NE 'X'
                      AND postdate BETWEEN wa_pre_datum AND wa_datum
                      AND werks = p_werks
                      AND lgort = p_lgort
                     %_HINTS ORACLE 'FIRST_ROWS(10)'.
*---
    DELETE it_bfqty_temp WHERE sync_ind EQ 'X'.
*---
    LOOP AT it_bfqty_temp.
      MOVE-CORRESPONDING it_bfqty_temp TO it_back.
      COLLECT it_back.
      CLEAR : it_bfqty_temp, it_back.
    ENDLOOP.


  ELSEIF p_sql = 'O'.
    SELECT matnr
           werks
           lgort
           komp_quant     " quantity
           flg_asynch
           sync_ind
                     INTO CORRESPONDING FIELDS OF TABLE it_bfqty_temp
                      FROM ztmm_ppc1_all
                     WHERE flg_asynch NE 'X'
*                     and POSTDATE BETWEEN WA_PRE_DATUM AND WA_DATUM
                      AND werks = p_werks
                      AND lgort = p_lgort
                     %_HINTS ORACLE 'FIRST_ROWS(10)'.
*---
    DELETE it_bfqty_temp WHERE sync_ind EQ 'X'.
*---
    LOOP AT it_bfqty_temp.
      MOVE-CORRESPONDING it_bfqty_temp TO it_back.
      COLLECT it_back.
      CLEAR : it_bfqty_temp, it_back.
    ENDLOOP.

  ENDIF.

  SORT it_back BY matnr.

ENDFORM.                    " get_scheduled_bf_qty

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*---
  CLEAR : w_screen_date, w_screen_time, w_current_date, w_current_time.
  CLEAR : r_zline.

  r_zline-sign = 'I'.
  r_zline-option = 'EQ'.

  CASE p_rp.
    WHEN '06'.
      r_zline-low = 'T1'. APPEND r_zline.
      r_zline-low = 'T2'. APPEND r_zline.
      r_zline-low = 'T3'. APPEND r_zline.
      r_zline-low = 'C1'. APPEND r_zline.
      r_zline-low = 'C2'. APPEND r_zline.

* by ig.moon { 6/24/2013
      r_zline-low = 'T2-ONEKIT'. APPEND r_zline.
      r_zline-low = 'T3-ONEKIT'. APPEND r_zline.
* }

    WHEN '07'.
      r_zline-low = 'F1'. APPEND r_zline.
      r_zline-low = 'F2'. APPEND r_zline.
      r_zline-low = 'F3'. APPEND r_zline.
** On 05/31/13
      r_zline-low = 'DL-ONEKIT'. APPEND r_zline.
      r_zline-low = 'DR-ONEKIT'. APPEND r_zline.
** End on 05/31/13
    WHEN '09'.
      r_zline-low = 'F4'. APPEND r_zline.
      r_zline-low = 'F5'. APPEND r_zline.
      r_zline-low = 'OK'. APPEND r_zline.
    WHEN OTHERS.

  ENDCASE.

*---
  PERFORM decide_times.

*---
  PERFORM get_vehicle_master.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  get_vehicle_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_vehicle_master.
  DATA: lw_date_f(14),
        lw_date_t(14).
*---
  DATA : lv_rp06_timestamp(11).      "'YYYYMMDDHHMISSX'

  DATA : BEGIN OF ls_rsnum,
           rsnum LIKE ztpp_dvrt1-rsnum,  "Reservation number
         END OF ls_rsnum.

  DATA : lt_rsnum LIKE ls_rsnum OCCURS 0 WITH HEADER LINE.

  DATA : lt_supply_info LIKE zspp_vin_info_for_stl OCCURS 0
                                                   WITH HEADER LINE.

*---
  CLEAR : lt_supply_info, lt_supply_info[].

  CONCATENATE: w_pre_s_date w_pre_s_time INTO lw_date_f,
               w_pre_e_date w_pre_e_time INTO lw_date_t.

  CALL FUNCTION 'Z_FPP_GET_SUPPLY_TO_LINE'
    EXPORTING
      i_date_f                 = lw_date_f
      i_date_t                 = lw_date_t
*     i_date                   = w_current_date
*     i_time                   = w_current_time
      i_rp                     = p_rp
    TABLES
      t_supply_info            = lt_supply_info
    EXCEPTIONS
      no_data_founded          = 1
      line_info_does_not_exist = 2
      etc_exception            = 3
      OTHERS                   = 4.

  LOOP AT lt_supply_info.
    MOVE : lt_supply_info-rsnum TO ls_rsnum.
    APPEND ls_rsnum TO lt_rsnum.
    CLEAR : lt_supply_info, ls_rsnum, lt_rsnum.
  ENDLOOP.

* Unique reservation numbers
  DELETE lt_rsnum WHERE rsnum = '0000000000'.

  DELETE ADJACENT DUPLICATES FROM lt_rsnum
                             COMPARING rsnum.

*/ Get Quantity Raw Data(LT_TOLINE) from RESB and ZTMM_MAST
  DATA : BEGIN OF ls_toline.
          INCLUDE STRUCTURE wa_toline.
  DATA :   rsnum LIKE resb-rsnum,
           rspos LIKE resb-rspos,
         END OF ls_toline.

  DATA : lt_toline LIKE ls_toline OCCURS 0 WITH HEADER LINE.

  CHECK NOT lt_rsnum[] IS INITIAL.


**S__ Query change by Paul 05/16/2011

** Changed by Furong on 06/14/11

*  SELECT A~RSNUM            "Reservation number
*         A~RSPOS            "Item number of reservation
*         A~MATNR            "Material
*         A~BDMNG            "Quantity
*         A~MEINS            "Unit
*         B~ZFEEDER           "Feeder
*         B~ABLAD AS DES_LGPLA "Workstation
*         B~ZZTIM            "Safety Time
*         B~ZZEISBE AS LPMIN "Safety Stock
*         B~ZRHLH
*         C~RGVER            "Person of Contact
*
*
*    INTO CORRESPONDING FIELDS OF TABLE LT_TOLINE
*    FROM RESB AS A
*   INNER JOIN PKHD AS B
*         ON   A~MANDT EQ B~MANDT
*         AND  A~MATNR EQ B~MATNR
*   INNER JOIN PVBE AS C
*         ON   B~WERKS EQ C~WERKS
*         AND  B~PRVBE EQ C~PRVBE
*   INNER JOIN MARA AS D
*         ON   B~MATNR EQ D~MATNR
*
*     FOR ALL ENTRIES IN LT_RSNUM
*
*   WHERE A~RSNUM EQ LT_RSNUM-RSNUM    "Reservation number
*     AND B~WERKS EQ 'P001'
*     AND B~RKSTA EQ 'I'
*     AND B~PKSTU EQ '0001'
**     AND B~SPPTL EQ 'S'
*     AND B~PRVBE IN R_ZLINE
*     AND B~MATNR IN S_MATNR
*     AND D~TEMPB EQ '3'.

  DATA: l_index LIKE sy-tabix.
  DATA: BEGIN OF lt_pkhd OCCURS 0,
        matnr LIKE pkhd-matnr,
        zfeeder LIKE pkhd-zfeeder,
        ablad LIKE pkhd-ablad,
        zztim LIKE pkhd-zztim,
        zzeisbe LIKE pkhd-zzeisbe,
        zrhlh LIKE pkhd-zrhlh,
        rgver LIKE pvbe-rgver,
        behmg LIKE pkhd-behmg,
        END OF lt_pkhd.

  SELECT a~rsnum            "Reservation number
         a~rspos            "Item number of reservation
         a~matnr            "Material
         a~bdmng            "Quantity
         a~meins            "Unit
    INTO CORRESPONDING FIELDS OF TABLE lt_toline
    FROM resb AS a
   INNER JOIN mara AS d
      ON a~matnr EQ d~matnr
     FOR ALL ENTRIES IN lt_rsnum
   WHERE a~rsnum EQ lt_rsnum-rsnum    "Reservation number
     AND a~matnr IN s_matnr
     AND d~tempb EQ '3'.

  SELECT b~matnr   b~zfeeder b~ablad b~zztim
         b~zzeisbe b~zrhlh c~rgver
         b~behmg
    INTO TABLE lt_pkhd
    FROM pkhd AS b
   INNER JOIN pvbe AS c
      ON b~werks EQ c~werks
     AND b~prvbe EQ c~prvbe
     FOR ALL ENTRIES IN lt_toline
   WHERE b~matnr = lt_toline-matnr
     AND b~werks EQ 'P001'
     AND b~prvbe IN r_zline
     AND b~rksta EQ 'I'
     AND b~pkstu EQ '0001'.

  SORT lt_pkhd BY matnr.

  LOOP AT lt_toline.
    l_index = sy-tabix.
    READ TABLE lt_pkhd WITH KEY matnr = lt_toline-matnr
                         BINARY SEARCH.
    IF sy-subrc = 0.
      lt_toline-zfeeder = lt_pkhd-zfeeder.
      lt_toline-des_lgpla = lt_pkhd-ablad.
      lt_toline-lpmin = lt_pkhd-zzeisbe.
      lt_toline-zrhlh = lt_pkhd-zrhlh.
      lt_toline-rdmng = lt_pkhd-behmg.

      MODIFY lt_toline INDEX l_index.
    ELSE.
      DELETE lt_toline INDEX l_index.
    ENDIF.
  ENDLOOP.
** End of change on 06/14/11


**  SELECT A~RSNUM            "Reservation number
**         A~RSPOS            "Item number of reservation
**         A~MATNR            "Material
**         A~BDMNG            "Quantity
**         A~MEINS            "Unit
**         B~FEEDR            "Feeder
**         B~WORKS            "Workstation
**         B~RH_LH            "RH/LH
**         B~STOCK_CHECK      "STOCK CHECK
**         B~FEED_CYCLE       "FEEDING CYCLE
**         B~ZTIME            "TIME
**         B~LPMIN            " safety stock
**         B~DISPO            " person of contact
**         B~ZMNMX            " Min or Max
**                 INTO CORRESPONDING FIELDS OF TABLE LT_TOLINE
**                 FROM RESB AS A INNER JOIN ZTMM_MAST AS B
**                   ON A~MANDT EQ B~MANDT
**                  AND A~MATNR EQ B~MATNR
**                  FOR ALL ENTRIES IN LT_RSNUM
**                WHERE A~RSNUM EQ LT_RSNUM-RSNUM    "Reservation number
**                  AND B~WERKS EQ 'P001'
**                  AND B~SPPTL EQ 'S'
**                  AND B~ZLINE IN R_ZLINE
***                  AND b~zline IN ('T1', 'T2', 'T3', 'C1', 'C2')
**                  AND B~MATNR IN S_MATNR.
**E__<
*/Get draft data(IT_TOLINE) for TO Creation
  DATA : lv_bdmng     LIKE wa_toline-bdmng.  "For Sum of qty

  SORT lt_toline BY matnr.

  LOOP AT lt_toline.
    lv_bdmng = lv_bdmng + lt_toline-bdmng.
    MOVE-CORRESPONDING lt_toline TO wa_toline.
    AT END OF matnr.
      MOVE : lv_bdmng TO wa_toline-bdmng.
      wa_toline-stock_check = 'X'.
      wa_toline-zmnmx = 'MIN'.
      APPEND wa_toline TO it_toline.
      CLEAR : lv_bdmng.
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
FORM execute_process.
*--- get scheduled backflush quantity
  PERFORM get_scheduled_bf_qty.

*--- get current stock
*  PERFORM get_verme_lpmin.
  PERFORM get_stock.

*--- get open TO quantity
*  PERFORM get_open_to.
  PERFORM get_open_reservation.

*--- get BackFlush error quantity(/nCOGI)
  PERFORM get_bf_error_qty.

*** checking rounding qty by storage type
*  PERFORM GET_SOURCE_TYPE.
*** end of addition

*--- get control cycle
  PERFORM get_control_cycle.

*--- get rounding value
*  PERFORM GET_RDMNG.
*--- get destination storage type/bin
*  PERFORM GET_DES_STORAGE_TYPE_BIN.
  PERFORM get_des_storage_loc.
*---
*  PERFORM CREATE_TRANSFER_ORDER.
*s__ PAUL
  PERFORM create_s2l_order.
*E__<

ENDFORM.                    " execute_process

*&---------------------------------------------------------------------*
*&      Form  create_transfer_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM CREATE_TRANSFER_ORDER.
**---
*  DATA : L_TABIX LIKE SY-TABIX,
*         L_MESSA(80).
*  DATA : LW_BEGZT(14).
*  DATA : L_DATUM TYPE D.
*  DATA : L_STATS LIKE ZTMM_STL_LOG-STATS.
*
*  CLEAR : IT_ITAB, IT_ITAB[], IT_DISPO, IT_DISPO[].
*
*  LOOP AT IT_TOLINE.
*    CLEAR : L_STATS.
*    MOVE : SY-TABIX TO L_TABIX.
**--- TO GET Start Date(Feeding Date) and Start Time
*    MOVE : W_SCREEN_DATE   TO IT_TOLINE-SDATE,
*           W_STARTING_TIME TO IT_TOLINE-FEEDING_TIME,
*           W_ENDING_DATE   TO IT_TOLINE-EDATE,
*           W_ENDING_TIME   TO IT_TOLINE-ETIME.
**    PERFORM time_calculation USING    w_screen_date
**                                      w_screen_time
**                                      0
**                             CHANGING it_toline-sdate
**                                      it_toline-feeding_time.
*** CHANGE NOT USE FEED_CYCLE NO MORE PAUL 06/10/11
**--- consider feeding cycle
**    IF IT_TOLINE-FEED_CYCLE EQ '0120'.
**E__<
*      CLEAR : W_ODD_EVEN, IT_1T.
*      CONCATENATE W_SCREEN_DATE W_STARTING_TIME INTO LW_BEGZT.
**      CONCATENATE w_current_date w_current_time INTO lw_begzt.
*      READ TABLE IT_1T WITH KEY BEGZT = LW_BEGZT.
*      IF SY-SUBRC NE 0.
*        MESSAGE E000(ZZ) WITH TEXT-M12.
*      ENDIF.
*      W_ODD_EVEN = IT_1T-INDEX MOD 2.
**      READ TABLE it_ztmm_stl_time WITH KEY rtime = w_current_time.
**      w_odd_even = it_ztmm_stl_time-zindx MOD 2.
**      w_odd_even = it_toline-feeding_time(2) MOD 2.
*      IF W_ODD_EVEN EQ 0.
**      IF w_odd_even EQ 0.
**      IF w_odd_even NE 1.
*        CLEAR : IT_1T, W_TABIX, L_DATUM.
*        READ TABLE IT_1T WITH KEY BEGZT = LW_BEGZT.
*        IF SY-SUBRC NE 0.
*          MESSAGE E000(ZZ) WITH TEXT-M12.
*        ENDIF.
**        READ TABLE it_ztmm_stl_time WITH KEY rtime = w_current_time.
*        MOVE : SY-TABIX TO W_TABIX,
*               IT_1T-DATUM TO L_DATUM.
*        W_TABIX = W_TABIX + 1.
*        CLEAR : IT_1T.
*        READ TABLE IT_1T INDEX W_TABIX.
*        IF IT_1T-DATUM NE L_DATUM.
*          W_TABIX = W_TABIX - 1.
*          CLEAR : IT_1T.
*          READ TABLE IT_1T INDEX W_TABIX.
*          MOVE : IT_1T-BEGZT+8(6) TO IT_TOLINE-FEEDING_TIME,
*                 IT_1T-ENDZT(8)   TO IT_TOLINE-EDATE,
*                 IT_1T-ENDZT+8(6) TO IT_TOLINE-ETIME.
*        ELSE.
*          MOVE : IT_1T-BEGZT+8(6) TO IT_TOLINE-FEEDING_TIME,
*                 IT_1T-ENDZT(8)   TO IT_TOLINE-EDATE,
*                 IT_1T-ENDZT+8(6) TO IT_TOLINE-ETIME.
*        ENDIF.
**        READ TABLE it_ztmm_stl_time INDEX w_tabix.
**        MOVE : it_ztmm_stl_time-stime TO it_toline-feeding_time.
*      ENDIF.
**    ENDIF.
**---
*
*    CLEAR : W_MINUS_STOCK_CHECK.
*
**--- calculate quantity
**    PERFORM get_tqty.
*    PERFORM GET_TQTY_NEW.
*
**--- App Doc No
*    PERFORM NUMBER_GET_NEXT USING    C_NRO_NR_09     "NRO Interval
*                                     W_NRO_OBJECT    "NRO Object
*                            CHANGING W_ZDOCNO.     "App Doc No
*    COMMIT WORK.
*
*    IF W_MINUS_STOCK_CHECK EQ SPACE.
*
**** commented by Furong on 07/29/2005
**--- Get Source Storage type/bin
**      CLEAR : it_source.
**      READ TABLE it_source WITH KEY matnr = it_toline-matnr.
**      MOVE : it_source-lgtyp TO it_toline-src_lgtyp,
**             it_source-lgpla TO it_toline-src_lgpla.
**** end of comment
*
**--- Get Destination Storage type/bin
*      CLEAR : IT_DEST.
*      READ TABLE IT_DEST WITH KEY MATNR = IT_TOLINE-MATNR.
**      IF SY-SUBRC EQ 0.
**        MOVE : IT_DEST-LGTYP TO IT_TOLINE-DES_LGTYP,
**               IT_DEST-LGPLA TO IT_TOLINE-DES_LGPLA.
**      ELSE.
**        MOVE : 'XXX'         TO IT_TOLINE-DES_LGTYP,
**               'XXXXXXXXXX'  TO IT_TOLINE-DES_LGPLA.
**      ENDIF.
*
**--- BDC Processing of /nLT01
*      PERFORM BDC_PROCESSING_LT01 TABLES   IT_BDCMSGCOLL
*                                  USING    W_ZDOCNO
*                                  CHANGING W_SUBRC.
*
*      IF W_SUBRC EQ 0.
**--- Change TO Header (/nLT1A)
*        CLEAR : WA_BDCMSGCOLL.
*        READ TABLE IT_BDCMSGCOLL INTO WA_BDCMSGCOLL
*                                 WITH KEY MSGTYP = 'S'.
*        CHECK SY-SUBRC = 0.
*        PERFORM BDC_PROCESSING_LTA1 TABLES   IT_BDCMSGCOLL
*                                    USING    W_ZDOCNO
*                                             WA_BDCMSGCOLL-MSGV1
*                                    CHANGING W_SUBRC.
*        IF W_SUBRC EQ 0.
*          MOVE : 'C' TO L_STATS.
*        ELSE.
*          MOVE : 'H' TO L_STATS.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    MODIFY IT_TOLINE INDEX L_TABIX.
*
*    MOVE-CORRESPONDING IT_TOLINE TO IT_ITAB.
*    MOVE : W_ZDOCNO              TO IT_ITAB-W_DOCNO.
*    MOVE : W_NSOLA               TO IT_ITAB-NSOLA.
*    MOVE : L_STATS               TO IT_ITAB-STATS.
*
*    IF W_MINUS_STOCK_CHECK EQ SPACE.
*      IF W_SUBRC EQ 0.
*        MOVE : C_GREEN             TO IT_ITAB-LINECOLOR,
*               'S'                 TO IT_ITAB-MSGTY.
*        CLEAR : IT_BDCMSGCOLL.
*        READ TABLE IT_BDCMSGCOLL INDEX 1.
*        MOVE : IT_BDCMSGCOLL-MSGV2 TO IT_ITAB-TANUM.
*      ELSE.
*        MOVE : C_RED               TO IT_ITAB-LINECOLOR,
*               'E'                 TO IT_ITAB-MSGTY.
*      ENDIF.
*    ELSE.
*      CLEAR : IT_BDCMSGCOLL, IT_BDCMSGCOLL[].
*    ENDIF.
**--- message
*    CLEAR : IT_BDCMSGCOLL, L_MESSA.
*    READ TABLE IT_BDCMSGCOLL WITH KEY MSGTYP = 'E'.
*    IF SY-SUBRC EQ 0.
*      PERFORM GET_MESSAGE USING    IT_BDCMSGCOLL-MSGID
*                                   IT_BDCMSGCOLL-MSGNR
*                                   IT_BDCMSGCOLL-MSGV1
*                                   IT_BDCMSGCOLL-MSGV2
*                                   IT_BDCMSGCOLL-MSGV3
*                                   IT_BDCMSGCOLL-MSGV4
*                          CHANGING L_MESSA.
*    ELSE.
*      READ TABLE IT_BDCMSGCOLL WITH KEY MSGTYP = 'S'.
*      IF SY-SUBRC EQ 0.
*        PERFORM GET_MESSAGE USING    IT_BDCMSGCOLL-MSGID
*                                     IT_BDCMSGCOLL-MSGNR
*                                     IT_BDCMSGCOLL-MSGV1
*                                     IT_BDCMSGCOLL-MSGV2
*                                     IT_BDCMSGCOLL-MSGV3
*                                     IT_BDCMSGCOLL-MSGV4
*                            CHANGING L_MESSA.
*      ENDIF.
*    ENDIF.
*    MOVE : L_MESSA         TO IT_ITAB-MESSA.
*    MOVE : IT_BDCMSGCOLL-MSGID TO IT_ITAB-MSGID,
*           IT_BDCMSGCOLL-MSGNR TO IT_ITAB-MSGNR.
*    APPEND IT_ITAB.
**---
*    MOVE : IT_TOLINE-DISPO TO IT_DISPO-DISPO.
*    COLLECT IT_DISPO.
*    CLEAR : IT_TOLINE, IT_ITAB, IT_DISPO.
*  ENDLOOP.
*ENDFORM.                    " create_transfer_order

*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
*---
  DATA : it_ztmm_stl_log LIKE ztmm_stl_log OCCURS 0 WITH HEADER LINE.

  CLEAR : it_ztmm_stl_log, it_ztmm_stl_log[].

  LOOP AT it_itab.
    CLEAR : ztmm_stl_log.
    MOVE-CORRESPONDING it_itab TO it_ztmm_stl_log.
    MOVE : it_itab-w_docno     TO it_ztmm_stl_log-logno_h,
           sy-tcode            TO it_ztmm_stl_log-ztcode,
           sy-repid            TO it_ztmm_stl_log-zprogramm,
**Begin modifications by Matthew Cupples 07/25/2011
*           IT_TOLINE-LABST    TO IT_ZTMM_STL_LOG-VERME,
*           IT_TOLINE-DES_LGPLA TO IT_ZTMM_STL_LOG-WORKS,
*           IT_TOLINE-ZFEEDER	 TO IT_ZTMM_STL_LOG-FEEDR.
           it_itab-zrhlh       TO it_ztmm_stl_log-rh_lh,
           it_itab-zfeeder     TO it_ztmm_stl_log-feedr,
           it_itab-labst       TO it_ztmm_stl_log-verme,
           it_itab-des_lgpla   TO it_ztmm_stl_log-works,
           it_itab-src_sloc    TO it_ztmm_stl_log-src_lgort,
           it_itab-open_qty    TO it_ztmm_stl_log-open_to.
**End modifications by Matthew Cupples 07/25/2011
    it_ztmm_stl_log-ernam = it_ztmm_stl_log-aenam = sy-uname.
    it_ztmm_stl_log-erdat = it_ztmm_stl_log-aedat = sy-datum.
    it_ztmm_stl_log-erzet = it_ztmm_stl_log-aezet = sy-uzeit.
    APPEND it_ztmm_stl_log.
  ENDLOOP.

  MODIFY ztmm_stl_log FROM TABLE it_ztmm_stl_log.
ENDFORM.                    " update_table

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM comment_build.
*---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-006.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.
ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_alv_grid.
*---
  MOVE : 'LINECOLOR' TO w_layout-info_fieldname.
  w_layout-colwidth_optimize = 'X'.

  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program = w_program
      is_layout          = w_layout
      it_fieldcat        = w_fieldcat[]
      it_events          = w_eventcat[]
      it_sort            = w_sortcat[]
      i_save             = 'A'
    TABLES
      t_outtab           = it_itab
    EXCEPTIONS
      program_error      = 1
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
FORM build_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : color
  append_fieldcat :
    w_col_pos 'MATNR'     18 'Material'       'CHAR' 'X' ''      '',
    w_col_pos 'BDMNG'     12 'Quantity'       'QUAN' ''  'MEINS' '',
    w_col_pos 'MEINS'     03 'UoM'            'UNIT' ''  ''      '',
    w_col_pos 'DES_LGPLA' 05 'Workstation'    'CHAR' ''  ''      '',
    w_col_pos 'ZRHLH'     02 'RH/LH'          'CHAR' ''  ''      '',
    w_col_pos 'FEEDING_TIME' 10 'Starting Time'
                                              'TIMS' ''  ''      '',
*    W_COL_POS 'STOCK_CHECK'  01 'Stock Check'
*                                              'CHAR' ''  ''      '',
*    W_COL_POS 'FEED_CYCLE'   04 'Feed Cycle'
*                                              'NUMC' ''  'MEINS' '',
*    W_COL_POS 'ZTIME'     03 'Time for STL'   'NUMC' ''  ''      '',
    w_col_pos 'LABST'     12 'Current Stock'  'QUAN' ''  'MEINS' '',
    w_col_pos 'LPMIN'     12 'Safety Stock'   'QUAN' ''  'MEINS' '',
    w_col_pos 'BFERRORQTY' 12 'B/F Error'     'QUAN' ''  'MEINS' '',
    w_col_pos 'BF_TO_QTY' 12 'to B/F Qty'     'QUAN' ''  'MEINS' '',
    w_col_pos 'NSOLA'     12 'Prev Qty'       'QUAN' ''  'MEINS' '',
    w_col_pos 'RDMNG'     12 'Rounding Qty'   'QUAN' ''  'MEINS' '',
    w_col_pos 'ZFEEDER'   05 'Feeder'         'CHAR' ''  ''      '',
*S__BY PAUL 05/19/11
*    W_COL_POS 'TANUM'     10 'TO Number'      'CHAR' ''  ''      '',
*    W_COL_POS 'SRC_LGTYP' 03 'Src S/Type'     'CHAR' ''  ''      '',
*    W_COL_POS 'SRC_LGPLA' 10 'Src S/Bin'      'CHAR' ''  ''      '',
*    W_COL_POS 'DES_LGTYP' 03 'Des S/Type'     'CHAR' ''  ''      '',
*    W_COL_POS 'DES_LGPLA' 10 'Des S/Bin'      'CHAR' ''  ''      '',
    w_col_pos 'OPEN_QTY'   12 'Open Res'       'QUAN' ''  'MEINS' '',
    w_col_pos 'TQTY'       12 'Res Qty'        'QUAN' ''  'MEINS' '',
    w_col_pos 'RSNUM'      10 'Res. Number'    'CHAR' ''  ''      '',
    w_col_pos 'SRC_SLOC'   04 'Src S/Loc'      'CHAR' ''  ''      '',
    w_col_pos 'DES_LGORT'  04 'Des S/Loc'      'CHAR' ''  ''      '',
*E__<
    w_col_pos 'MESSA'     80 'Message'        'CHAR' ''  ''      ''.
ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat.
**--- &1 : position     &2 : field name     &3 : tab name
**--- &4 : up           &5 : sub total
  append_sortcat : '1' 'MATNR' 'IT_ITAB' 'X' ''.
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
*FORM GET_MESSAGE USING    P_MSGID
*                          P_MSGNR
*                          P_MSGV1
*                          P_MSGV2
*                          P_MSGV3
*                          P_MSGV4
*                 CHANGING P_L_MESSA.
**---
*  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*       EXPORTING
*            MSGID               = P_MSGID
*            MSGNR               = P_MSGNR
*            MSGV1               = P_MSGV1
*            MSGV2               = P_MSGV2
*            MSGV3               = P_MSGV3
*            MSGV4               = P_MSGV4
*       IMPORTING
*            MESSAGE_TEXT_OUTPUT = P_L_MESSA.
*ENDFORM.                    " get_message

*&---------------------------------------------------------------------*
*&      Form  send_mail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM SEND_MAIL.
**---
*  DATA : BEGIN OF AS_ABAPLIST OCCURS 0.
*          INCLUDE STRUCTURE ABAPLIST.
*  DATA : END OF AS_ABAPLIST.
*
*  DATA : BEGIN OF AS_ALI_CONT OCCURS 0,
*           LINE(255),
*         END OF AS_ALI_CONT.
*
*  DATA : BEGIN OF MS_MAILOBJECT_CONT OCCURS 0.
*          INCLUDE STRUCTURE MCMAILOBJ.
*  DATA : END OF MS_MAILOBJECT_CONT.
*
*  DATA : L_OKCODE LIKE SY-UCOMM.
*
*  CLEAR : AS_ABAPLIST, AS_ALI_CONT, MS_MAILOBJECT_CONT.
*  REFRESH : AS_ABAPLIST, AS_ALI_CONT, MS_MAILOBJECT_CONT.
*
*  CALL FUNCTION 'SAVE_LIST'
*       EXPORTING
*            LIST_INDEX         = '0'
*       TABLES
*            LISTOBJECT         = AS_ABAPLIST
*       EXCEPTIONS
*            LIST_INDEX_INVALID = 1
*            OTHERS             = 2.
*
*  CALL FUNCTION 'TABLE_COMPRESS'
*       TABLES
*            IN             = AS_ABAPLIST
*            OUT            = AS_ALI_CONT
*       EXCEPTIONS
*            COMPRESS_ERROR = 1
*            OTHERS         = 2.
*
*  LOOP AT AS_ALI_CONT.
*    MOVE : '1' TO MS_MAILOBJECT_CONT-OBJNR,
*           '1' TO MS_MAILOBJECT_CONT-OBJLEVEL,
*           'ALI' TO MS_MAILOBJECT_CONT-OBJTYPE,
*           'Supply to Line' TO MS_MAILOBJECT_CONT-OBJNAM,
*           'Supply to Line' TO MS_MAILOBJECT_CONT-OBJDES,
*           AS_ALI_CONT-LINE TO MS_MAILOBJECT_CONT-OBJLINE.
*    APPEND MS_MAILOBJECT_CONT.
*  ENDLOOP.
*
*  CALL FUNCTION 'MC_SEND_MAIL'
*       EXPORTING
*            MS_MAIL_SENDMODE          = 'O'
*            MS_MAIL_TITLE             = 'Notification'
*            MS_MAIL_DESCRIPTION       = 'Supply to Line Result'
*            MS_MAIL_RECEIVER          = 'STLIMSIS'
**           MS_MAIL_EXPRESS           =
**           MS_MAIL_DLINAME           =
**           MS_MAIL_LANGU             = SY-LANGU
**           MS_MAIL_FUNKOBJ_TYP       = 'R'
**           MS_MAIL_FUNKOBJ_NAME      = 'RMCSMAIL'
*       IMPORTING
*            MS_OK_CODE                = L_OKCODE
*       TABLES
*            MS_MAIL_CONT              = MS_MAILOBJECT_CONT
**           MS_MAIL_FUNKOBJ_PARM      =
**           MS_MAIL_SETGET_PARM       =
**           MS_MAIL_RECEIVERS         =
*       EXCEPTIONS
*            SEND_ERROR                = 1
*            CANCELED                  = 2
*            NO_TITLE                  = 3
*            NO_DESCRIPTION            = 4
*            NO_RECEIVER_OR_DLI        = 5
*            INVALID_SENDMODE          = 6
*            NO_CONTENT                = 7
*            INVALID_FUNCTIONAL_PARAMS = 8
*            OTHERS                    = 9.
*ENDFORM.                    " send_mail

*&---------------------------------------------------------------------*
*&      Form  get_working_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM GET_WORKING_TIME CHANGING P_1SHIFT_OVERTIME
*                               P_2SHIFT_OVERTIME.
**---
*  DATA : L_DAYNR LIKE HRVSCHED-DAYNR,
*         L_DAYTXT LIKE HRVSCHED-DAYTXT,
*         L_DAYFREE LIKE HRVSCHED-NODAY,
*         L_CURRENT_DATE TYPE D.
*
*  CONSTANTS : C_UZEIT_000000 TYPE T VALUE '000000',
*              C_UZEIT_035959 TYPE T VALUE '035959'.
*
*  CLEAR : CRHD, KAKO, KAZY, IT_WORKTIME, IT_WORKTIME[], L_DAYNR,
*          L_DAYTXT, L_DAYFREE.
*
**---
*  IF SY-UZEIT GE C_UZEIT_000000 AND SY-UZEIT LE C_UZEIT_035959.
*    L_CURRENT_DATE = SY-DATUM - 1.
*  ELSE.
*    L_CURRENT_DATE = SY-DATUM.
*  ENDIF.
*
*  SELECT SINGLE KAPID INTO CRHD-KAPID
*                      FROM CRHD
*                     WHERE OBJTY EQ 'A'
**                       AND arbpl EQ 'S2L'
*                      AND ARBPL EQ P_ARBPL
*                       AND WERKS EQ 'P001'.
*
*  SELECT SINGLE MOSID INTO KAKO-MOSID
*                      FROM KAKO
*                     WHERE KAPID EQ CRHD-KAPID.
*
*  SELECT SINGLE VERSN INTO KAZY-VERSN
*                      FROM KAZY
*                     WHERE KAPID EQ CRHD-KAPID
*                       AND DATUB GE L_CURRENT_DATE.
*
*  CALL FUNCTION 'RH_GET_DATE_DAYNAME'
*       EXPORTING
*            LANGU               = SY-LANGU
*            DATE                = L_CURRENT_DATE
*       IMPORTING
*            DAYNR               = L_DAYNR
*            DAYTXT              = L_DAYTXT
*            DAYFREE             = L_DAYFREE
*       EXCEPTIONS
*            NO_LANGU            = 1
*            NO_DATE             = 2
*            NO_DAYTXT_FOR_LANGU = 3
*            INVALID_DATE        = 4
*            OTHERS              = 5.
*
*  SELECT TAGNR
*         SCHNR
*         KAPTPROG
*         B~BEGDA
*         B~ENDDA
*         B~BEGZT
*         B~ENDZT
*               INTO CORRESPONDING FIELDS OF TABLE IT_WORKTIME
*               FROM KAPA AS A INNER JOIN TC37A AS B
*                 ON A~MANDT EQ B~MANDT
*                AND A~TPROG EQ B~KAPTPROG
*              WHERE SCHGRUP EQ KAKO-MOSID
*                AND KAPID   EQ CRHD-KAPID
*                AND VERSN   EQ KAZY-VERSN
*                AND BEGDA   LE L_CURRENT_DATE
*                AND ENDDA   GE L_CURRENT_DATE
*                AND TAGNR   EQ L_DAYNR.
*
*  SORT IT_WORKTIME BY TAGNR SCHNR.
*
**---
*  DATA : L_ENDZT LIKE IT_WORKTIME-ENDZT,
*         L_BEGZT LIKE IT_WORKTIME-BEGZT.
*
*  CLEAR : IT_WORKTIME, L_ENDZT.
*  READ TABLE IT_WORKTIME WITH KEY SCHNR = 1.
*  MOVE : IT_WORKTIME-ENDZT TO L_ENDZT.
*
*  CLEAR : IT_WORKTIME, L_BEGZT.
*  READ TABLE IT_WORKTIME WITH KEY SCHNR = 2.
*  MOVE : IT_WORKTIME-BEGZT TO L_BEGZT.
*
*  CLEAR : P_1SHIFT_OVERTIME, P_2SHIFT_OVERTIME.
*
**--- 1 shift overtime
*  IF L_ENDZT EQ L_BEGZT.
*    MOVE : 'X' TO P_1SHIFT_OVERTIME.
*  ENDIF.
*
*  CLEAR : IT_WORKTIME, L_ENDZT.
*  READ TABLE IT_WORKTIME WITH KEY SCHNR = 2.
*  MOVE : IT_WORKTIME-ENDZT TO L_ENDZT.
*
**--- 2 shift overtime
*  IF L_ENDZT NE C_TIME_020000.
*    MOVE : 'X' TO P_2SHIFT_OVERTIME.
*  ENDIF.
*ENDFORM.                    " get_working_time

*&---------------------------------------------------------------------*
*&      Form  decide_times
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM decide_times.
*---

*--- insert by stlim (2004/07/28)
  PERFORM call_working_time_function.
*--- end of insert

*--- blocked by stlim (2004/07/28)
*  PERFORM get_calendar_id.
*  PERFORM get_working_day.
*  PERFORM get_work_time_per_day.
*  PERFORM get_break_time_per_day.
*  PERFORM get_1t_time_info.
*--- end of block

  PERFORM get_time.


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
*FORM GET_CALENDAR_ID.
**----- Read Shop Calendar ID of Trim line
*  SELECT SINGLE C~KALID C~MOSID B~KAPID
*    INTO (W_KALID, W_MOSID, W_KAPID)
*    FROM CRHD AS A INNER JOIN CRCA AS B
*                      ON A~OBJTY = B~OBJTY
*                     AND A~OBJID = B~OBJID
*                   INNER JOIN KAKO AS C
*                      ON B~KAPID = C~KAPID
*   WHERE A~WERKS =  P_WERKS
**     AND a~arbpl =  c_arbpl
*          AND A~ARBPL =  P_ARBPL
*     AND B~FORK2 =  'SAP006'.
*ENDFORM.                    " get_calendar_id
*&---------------------------------------------------------------------*
*&      Form  get_next_working_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM GET_WORKING_DAY.
**----- Read working day
**----- Previous : freeday => except
**----- Today    : freeday, not freeday => append
**----- Next     : not freeday => append
*
*  DATA: LW_DATE  TYPE D,
*        LW_LINES TYPE I,
*        LW_DAYNR LIKE HRVSCHED-DAYNR,
*        LW_DAYFREE LIKE HRVSCHED-NODAY.
*
*  CLEAR: IT_WORK_DATE, IT_WORK_DATE[].
*  LW_DATE = WA_DATUM - 1.
*
*  DO.
*    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
*         EXPORTING
*              LANGU               = SY-LANGU
*              DATE                = LW_DATE
*              CALID               = W_KALID
*         IMPORTING
*              DAYNR               = LW_DAYNR
*              DAYFREE             = LW_DAYFREE
*         EXCEPTIONS
*              NO_LANGU            = 1
*              NO_DATE             = 2
*              NO_DAYTXT_FOR_LANGU = 3
*              INVALID_DATE        = 4
*              OTHERS              = 5.
*    IF SY-SUBRC <> 0.
*      MESSAGE E000(ZZ) WITH TEXT-M03.
*    ENDIF.
*
*    IF LW_DAYFREE EQ 'X' AND
*       SY-INDEX NE 2.
*      LW_DATE = LW_DATE + 1.
*      CONTINUE.
*    ENDIF.
*
*    MOVE: LW_DATE  TO IT_WORK_DATE-DATE,
*          LW_DAYNR TO IT_WORK_DATE-DAYNR.
*    APPEND IT_WORK_DATE.
*
*    LW_DATE = LW_DATE + 1.
*
*    IF SY-INDEX >= 3.
*      EXIT.
*    ENDIF.
*  ENDDO.
*ENDFORM.                    " get_next_working_day
*&---------------------------------------------------------------------*
*&      Form  get_work_time_per_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM GET_WORK_TIME_PER_DAY.
*  DATA: LT_WORK_TIME LIKE IT_WORK_TIME OCCURS 0 WITH HEADER LINE.
*
*  LOOP AT IT_WORK_DATE.
*    DATA: LW_DATUB TYPE D.
*
*    CLEAR: LT_WORK_TIME, LT_WORK_TIME[].
*
*    SELECT MIN( DATUB ) INTO LW_DATUB
*      FROM KAPA
*     WHERE KAPID =  W_KAPID
*       AND VERSN =  '01'
*       AND DATUB => IT_WORK_DATE-DATE.
*
*    SELECT KAPID VERSN DATUB TAGNR SCHNR
*      INTO CORRESPONDING FIELDS OF TABLE LT_WORK_TIME
*      FROM KAPA
*     WHERE KAPID =  W_KAPID
*       AND VERSN =  '01'
*       AND DATUB =  LW_DATUB
*       AND TAGNR = IT_WORK_DATE-DAYNR
*     GROUP BY KAPID VERSN DATUB TAGNR SCHNR.
*
*    LOOP AT LT_WORK_TIME.
*      SELECT SINGLE * FROM KAPA WHERE KAPID = LT_WORK_TIME-KAPID
*                                  AND VERSN = LT_WORK_TIME-VERSN
*                                  AND DATUB = LT_WORK_TIME-DATUB
*                                  AND TAGNR = LT_WORK_TIME-TAGNR
*                                  AND SCHNR = LT_WORK_TIME-SCHNR.
*      IF SY-SUBRC NE 0.
*        MESSAGE E000(ZZ) WITH TEXT-M04.
*      ENDIF.
*
*      CLEAR: IT_WORK_TIME.
*
*      IF KAPA-TPROG IS INITIAL.
*        MOVE-CORRESPONDING LT_WORK_TIME TO IT_WORK_TIME.
*        MOVE: IT_WORK_DATE-DATE TO IT_WORK_TIME-DATUM,
*              KAPA-TPROG        TO IT_WORK_TIME-TPROG,
*              KAPA-BEGZT        TO IT_WORK_TIME-BEGZT,
*              KAPA-ENDZT        TO IT_WORK_TIME-ENDZT.
*      ELSE.
*        MOVE-CORRESPONDING LT_WORK_TIME TO IT_WORK_TIME.
*        MOVE: IT_WORK_DATE-DATE TO IT_WORK_TIME-DATUM,
*              KAPA-TPROG TO IT_WORK_TIME-TPROG.
*        PERFORM READ_WORK_TIME_FROM_TC37A.
*      ENDIF.
*
*      APPEND IT_WORK_TIME.
*    ENDLOOP.
*  ENDLOOP.
*ENDFORM.                    " get_work_time_per_day
*&---------------------------------------------------------------------*
*&      Form  read_work_time_from_tc37a
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM READ_WORK_TIME_FROM_TC37A.
*  SELECT SINGLE BEGZT ENDZT PAPLAN PAPLAN
*    INTO (IT_WORK_TIME-BEGZT,  IT_WORK_TIME-ENDZT,
*          IT_WORK_TIME-PAPLAN, RG_PAPLAN-LOW)
*    FROM TC37A
*   WHERE SCHGRUP  =  W_MOSID
*     AND KAPTPROG =  IT_WORK_TIME-TPROG
*     AND ENDDA    => IT_WORK_DATE-DATE
*     AND BEGDA    <= IT_WORK_DATE-DATE.
*  IF SY-SUBRC NE 0.
*    MESSAGE E000(ZZ) WITH TEXT-M05.
*  ENDIF.
*
*  MOVE: 'I'  TO RG_PAPLAN-SIGN,
*        'EQ' TO RG_PAPLAN-OPTION.
*  COLLECT RG_PAPLAN.
*ENDFORM.                    " read_work_time_from_tc37a
*&---------------------------------------------------------------------*
*&      Form  set_it_day_work_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM SET_IT_DAY_WORK_TIME.
*
*ENDFORM.                    " set_it_day_work_time
*&---------------------------------------------------------------------*
*&      Form  get_break_time_per_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM GET_BREAK_TIME_PER_DAY.
**  DATA: lw_begzt LIKE sy-uzeit,   "Break time from
**        lw_endzt LIKE sy-uzeit,   "Break time to
**        lw_begtm LIKE sy-uzeit,   "Working time zone from
**        lw_endtm LIKE sy-uzeit,   "Working time zone to
**        lw_begac LIKE sy-uzeit,   "Break time in Working time zone
*from
**        lw_endac LIKE sy-uzeit.   "Break time in Working time zone to
*
*  READ TABLE RG_PAPLAN INDEX 1.
*  CHECK SY-SUBRC EQ 0.
*
*  SELECT * INTO TABLE IT_BREAK
*    FROM TC37P
*   WHERE SCHGRUP =  W_MOSID
*     AND PAPLAN  IN RG_PAPLAN
*     AND PADAUER >  P_PADA.
*
*
**  LOOP AT lt_tc37p.
**    MOVE: lt_tc37p-paubeg TO lw_begzt,
**          lt_tc37p-pauend TO lw_endzt.
**    DO.
**      CLEAR: it_break_time.
**      IF lw_begzt(2) EQ lw_endzt(2).
**        CONCATENATE lw_begzt(2) '0000' INTO lw_begtm.
**        lw_endtm = lw_begtm + 3600.
**
**        lw_begac = lw_begzt.
**        lw_endac = lw_endzt.
**
**        READ TABLE it_break_time WITH KEY paplan = lt_tc37p-paplan
**                                          paubeg = lw_begtm
**                                          pauend = lw_endtm.
**        IF sy-subrc EQ 0.
**          it_break_time-padauer = it_break_time-padauer +
**                                  ABS( lw_endac - lw_begac ).
**          MODIFY it_break_time INDEX sy-tabix.
**        ELSE.
**          MOVE: lt_tc37p-paplan TO it_break_time-paplan,
**                lw_begtm        TO it_break_time-paubeg,
**                lw_endtm        TO it_break_time-pauend.
**          it_break_time-padauer = abs( lw_endac - lw_begac ).
**
**          APPEND it_break_time.
**        ENDIF.
**
**        EXIT.
**      ELSE.
**        CONCATENATE lw_begzt(2) '0000' INTO lw_begtm.
**        lw_endtm = lw_begtm + 3600.
**
**        IF sy-index EQ 1.
**          lw_begac = lw_begzt.
**          lw_endac = lw_endtm.
**        ELSE.
**          lw_begac = lw_begtm.
**          IF lw_endtm = '000000'.
**            lw_endac = '240000'.
**          ELSE.
**            lw_endac = lw_endtm.
**          ENDIF.
**        ENDIF.
**
**        READ TABLE it_break_time WITH KEY paplan = lt_tc37p-paplan
**                                          paubeg = lw_begtm
**                                          pauend = lw_endtm.
**        IF sy-subrc EQ 0.
**          it_break_time-padauer = it_break_time-padauer +
**                                  ABS( lw_endac - lw_begac ).
**          MODIFY it_break_time INDEX sy-tabix.
**        ELSE.
**          MOVE: lt_tc37p-paplan TO it_break_time-paplan,
**                lw_begtm        TO it_break_time-paubeg,
**                lw_endtm        TO it_break_time-pauend.
**          it_break_time-padauer = abs( lw_endac - lw_begac ).
**
**          APPEND it_break_time.
**        ENDIF.
**
**        IF lw_endac EQ lw_endzt.
**          EXIT.
**        ELSE.
**          lw_begzt = lw_endtm.
**        ENDIF.
**      ENDIF.
**    ENDDO.
**  ENDLOOP.
*ENDFORM.                    " get_break_time_per_day
*&---------------------------------------------------------------------*
*&      Form  get_1t_time_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM GET_1T_TIME_INFO.
*  DATA: LW_DATUM LIKE   SY-DATUM,         "date
*        LW_UZEIT LIKE   SY-UZEIT,         " time
*        LW_BEGZT(14),      "start time
*        LW_ENDZT(14),      "finish time
*        LW_DTTMF(14),                     "Date/Time from
*        LW_DTTMT(14),                     "Date/Time to
*        LW_DATUMF_1T LIKE SY-DATUM,       "from 1T date
*        LW_DATUMT_1T LIKE SY-DATUM,                         "to 1T date
*        LW_BEGZT_1T LIKE SY-UZEIT,        "from 1T time
*        LW_ENDZT_1T LIKE SY-UZEIT.        "to IT time
*
*  LOOP AT IT_WORK_DATE.
*    LOOP AT IT_WORK_TIME WHERE DATUM = IT_WORK_DATE-DATE.
*      SORT IT_1T BY DATUM TPROG BEGZT.
*      CLEAR: IT_1T.
*      MOVE: IT_WORK_TIME-DATUM TO IT_1T-DATUM,
*            IT_WORK_TIME-TPROG TO IT_1T-TPROG.
**            it_break-paplan    TO it_1t-paplan.
*
*      CONCATENATE IT_WORK_TIME-DATUM IT_WORK_TIME-BEGZT
*             INTO IT_1T-BEGZT.
*      IF IT_WORK_TIME-BEGZT > IT_WORK_TIME-ENDZT.
*        LW_DATUM = IT_WORK_TIME-DATUM + 1.
*      ELSE.
*        LW_DATUM = IT_WORK_TIME-DATUM.
*      ENDIF.
*      CONCATENATE LW_DATUM IT_WORK_TIME-ENDZT
*             INTO IT_1T-ENDZT.
*
*      APPEND IT_1T.
*
*      LOOP AT IT_BREAK WHERE PAPLAN = IT_WORK_TIME-PAPLAN.
*        CLEAR: IT_1T.
*        CONCATENATE IT_WORK_TIME-DATUM IT_BREAK-PAUBEG
*               INTO LW_BEGZT.
*        IF IT_BREAK-PAUBEG > IT_BREAK-PAUEND.
*          LW_DATUM = IT_WORK_TIME-DATUM.
*          LW_DATUM = LW_DATUM + 1.
*        ELSE.
*          LW_DATUM = IT_WORK_TIME-DATUM.
*        ENDIF.
*        CONCATENATE LW_DATUM IT_BREAK-PAUEND
*               INTO LW_ENDZT.
*
*        LOOP AT IT_1T WHERE DATUM = IT_WORK_TIME-DATUM
*                        AND TPROG = IT_WORK_TIME-TPROG
*                        AND BEGZT <= LW_BEGZT
*                        AND ENDZT >= LW_ENDZT.
*
*          MOVE: IT_1T-ENDZT TO LW_ENDZT.
*
*          IF IT_1T-BEGZT+8(6) > IT_BREAK-PAUBEG.
*            LW_DATUM = IT_1T-BEGZT(8).
*            LW_DATUM = LW_DATUM + 1.
*          ELSE.
*            LW_DATUM = IT_1T-BEGZT(8).
*          ENDIF.
*          CONCATENATE LW_DATUM IT_BREAK-PAUBEG
*                 INTO IT_1T-ENDZT.
*
*          MODIFY IT_1T.
*        ENDLOOP.
*
*        IF SY-SUBRC EQ 0.
*          CLEAR: IT_1T.
*          MOVE: IT_WORK_TIME-DATUM TO IT_1T-DATUM,
*                IT_WORK_TIME-TPROG TO IT_1T-TPROG.
*          CONCATENATE LW_DATUM IT_BREAK-PAUBEG
*                 INTO IT_1T-BEGZT.
*          IF IT_BREAK-PAUBEG > IT_BREAK-PAUEND.
*            LW_DATUM = LW_DATUM + 1.
*          ENDIF.
*          CONCATENATE LW_DATUM IT_BREAK-PAUEND
*                 INTO IT_1T-ENDZT.
*          MOVE: 'B' TO IT_1T-FLAG.
*
*          APPEND IT_1T.
*
*          CLEAR: IT_1T.
*          MOVE: IT_WORK_TIME-DATUM TO IT_1T-DATUM,
*                IT_WORK_TIME-TPROG TO IT_1T-TPROG.
**                it_break-paplan    TO it_1t-paplan.
*          CONCATENATE LW_DATUM IT_BREAK-PAUEND INTO IT_1T-BEGZT.
*          IF IT_1T-BEGZT+8(6) > LW_ENDZT.
*            LW_DATUM = LW_DATUM + 1.
*          ENDIF.
*          MOVE: LW_ENDZT TO IT_1T-ENDZT.
*
*          APPEND IT_1T.
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.
*  ENDLOOP.
*
*  SORT IT_1T BY DATUM TPROG BEGZT.
*  PERFORM CACULATE_SECOND.
*
*  PERFORM GET_FINALLY_FEED_TIME.
*  PERFORM SET_TIME_ZONE_INDEX.
*ENDFORM.                    " get_1t_time_info
*&---------------------------------------------------------------------*
*&      Form  get_1t_time_info_break
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM GET_1T_TIME_INFO_BREAK.
*
*ENDFORM.                    " get_1t_time_info_break
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM CHECK_RTN.
*ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  caculate_second
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM CACULATE_SECOND.
*  DATA: LW_DATUM LIKE SY-DATUM,
*        LW_BEGZT LIKE SY-UZEIT,
*        LW_ENDZT LIKE SY-UZEIT,
*        LW_TIME  LIKE SY-UZEIT.
*
*  LOOP AT IT_1T.
*    LW_DATUM = IT_1T-BEGZT(8).
*    LW_BEGZT = IT_1T-BEGZT+8(6).
*    LW_ENDZT = IT_1T-ENDZT+8(6).
*
*    LW_BEGZT = LW_BEGZT + 1.
*    IF LW_BEGZT EQ '000001'.
*      LW_DATUM = LW_DATUM + 1.
*    ENDIF.
*
*    CONCATENATE LW_DATUM LW_BEGZT INTO IT_1T-BEGZT.
*
*    IF LW_BEGZT <= LW_ENDZT.
*      IT_1T-SECOND = ABS( LW_ENDZT - LW_BEGZT ).
*    ELSE.
*      LW_TIME      = LW_BEGZT.
*      IT_1T-SECOND = LW_ENDZT.
*      IT_1T-SECOND = IT_1T-SECOND + 86400 - LW_TIME.
*    ENDIF.
*
*    MODIFY IT_1T.
*  ENDLOOP.
*ENDFORM.                    " caculate_second
*&---------------------------------------------------------------------*
*&      Form  get_finally_feed_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM GET_FINALLY_FEED_TIME.
*  DATA: LW_DATEF LIKE SY-DATUM,
*        LW_DATET LIKE SY-DATUM,
*        LW_BEGZT LIKE TC37A-BEGZT,
*        LW_ENDZT LIKE TC37A-ENDZT,
*        LW_LINES TYPE I.
*
*  DATA: LT_1T LIKE IT_1T OCCURS 0 WITH HEADER LINE.
*
*  LT_1T[] = IT_1T[].
*  CLEAR: IT_1T, IT_1T[].
*
*  SORT LT_1T BY DATUM TPROG BEGZT.
*  LOOP AT LT_1T WHERE FLAG <> 'B'.
*    DO.
*      CLEAR: IT_1T.
*
*      IF     LT_1T-SECOND <  1799.
*        DESCRIBE TABLE IT_1T LINES LW_LINES.
*
*        READ TABLE IT_1T INDEX LW_LINES.
*
*        MOVE: IT_1T-ENDZT(8)   TO LW_DATET,
*              IT_1T-ENDZT+8(6) TO LW_ENDZT.
*
*        LW_ENDZT = LW_ENDZT + LT_1T-SECOND.
*        IF LW_ENDZT(2) EQ '00'.
*          LW_DATEF = LW_DATEF + 1.
*        ENDIF.
*
*        CONCATENATE LW_DATET LW_ENDZT INTO IT_1T-ENDZT.
*        APPEND IT_1T.
*
*        EXIT.
*      ELSEIF LT_1T-SECOND >= 1799 AND LT_1T-SECOND <= 3599.
*        MOVE: LT_1T TO IT_1T.
*        APPEND IT_1T.
*        EXIT.
*      ELSEIF LT_1T-SECOND >  3599.
*        MOVE LT_1T TO IT_1T.
*
*        MOVE: IT_1T-BEGZT(8)   TO LW_DATEF,
*              IT_1T-BEGZT+8(6) TO LW_BEGZT,
*              IT_1T-ENDZT(8)   TO LW_DATET,
*              IT_1T-ENDZT+8(6) TO LW_ENDZT.
*
*        LW_ENDZT = LW_BEGZT + 3600 - 1.
*        IF LW_ENDZT(2) EQ '00'.
*          LW_DATET = LW_DATEF + 1.
*        ELSE.
*          LW_DATET = LW_DATEF.
*        ENDIF.
*
*        CONCATENATE LW_DATET LW_ENDZT INTO IT_1T-ENDZT.
*        APPEND IT_1T.
*
*        LT_1T-SECOND = LT_1T-SECOND - 3600.
*        LT_1T-BEGZT  = IT_1T-ENDZT + 1.
*      ENDIF.
*    ENDDO.
*  ENDLOOP.
*ENDFORM.                    " get_finally_feed_time
*&---------------------------------------------------------------------*
*&      Form  set_time_zone_index
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM SET_TIME_ZONE_INDEX.
*  DATA: LW_INDEX TYPE I.
*
*  LOOP AT IT_1T.
*    AT NEW DATUM.
*      CLEAR: LW_INDEX.
*    ENDAT.
*
*    LW_INDEX = LW_INDEX + 1.
*
*    MOVE LW_INDEX TO IT_1T-INDEX.
*    MODIFY IT_1T.
*  ENDLOOP.
*ENDFORM.                    " set_time_zone_index
*&---------------------------------------------------------------------*
*&      Form  read_feeding_start_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM READ_FEEDING_START_TIME.
**----- read feeding start time
*
*  DATA: LW_INDEX TYPE I,
*        W_1T LIKE IT_1T.
*
*  READ TABLE IT_1T INTO W_1T WITH KEY BEGZT = IT_1T-BEGZT.
*  IF SY-SUBRC NE 0.
*    MESSAGE E000(ZZ) WITH TEXT-M04.
*  ENDIF.
*
*  MOVE: SY-TABIX TO LW_INDEX.
*  LW_INDEX = LW_INDEX + 1.
*
*  READ TABLE IT_1T INDEX LW_INDEX.
*  IF SY-SUBRC NE 0.
*    MESSAGE E000(ZZ) WITH TEXT-M06.
*  ENDIF.
*
*  IF IT_1T-DATUM EQ W_1T-DATUM AND
*     IT_1T-TPROG EQ W_1T-TPROG.
*    MOVE: W_1T-BEGZT+8(6) TO W_STARTING_TIME,
*          W_1T-ENDZT+8(6) TO W_ENDING_TIME,
*          W_1T-BEGZT(8)   TO W_SCREEN_DATE,
*          W_1T-ENDZT(8)   TO W_ENDING_DATE.
*  ELSE.
*    MOVE: IT_1T-BEGZT+8(6) TO W_STARTING_TIME,
*          IT_1T-ENDZT+8(6) TO W_ENDING_TIME,
*          IT_1T-BEGZT(8)   TO W_SCREEN_DATE,
*          IT_1T-ENDZT(8)   TO W_ENDING_DATE.
*  ENDIF.
*
**--- get previous worktime time
*  LW_INDEX = LW_INDEX - 1.
*
*  CLEAR : IT_1T.
*
*  READ TABLE IT_1T INDEX LW_INDEX.
*
*  IF SY-SUBRC EQ 0.
*    MOVE : IT_1T-BEGZT(8)   TO W_PRE_S_DATE,
*           IT_1T-BEGZT+8(6) TO W_PRE_S_TIME,
*           IT_1T-ENDZT(8)   TO W_PRE_E_DATE,
*           IT_1T-ENDZT+8(6) TO W_PRE_E_TIME.
*  ENDIF.
*ENDFORM.                    " read_feeding_start_time
*&---------------------------------------------------------------------*
*&      Form  get_exec_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_exec_time.
*---
  MOVE : p_cdate TO wa_datum.

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
*FORM SAVE_RUNNING_TIME.
**---
*  CLEAR : W_MAX_EXEC_DATETIME.
*
*  CONCATENATE : P_CDATE P_CTIME INTO W_MAX_EXEC_DATETIME.
*
**  CONCATENATE : p_cdate sy-uzeit INTO w_max_exec_datetime.
*
**  SELECT MAX( dtime ) INTO w_max_exec_datetime
**                      FROM ztmm_stl_exec
**                     WHERE repid EQ sy-repid.
*  IF W_MAX_EXEC_DATETIME IS INITIAL.
**    LOOP AT IT_1T WHERE BEGZT <= W_DAYTIME
**                    AND ENDZT >= W_DAYTIME.
**    ENDLOOP.
*  ELSE.
**    LOOP AT IT_1T WHERE BEGZT <= W_MAX_EXEC_DATETIME
**                    AND ENDZT >= W_MAX_EXEC_DATETIME.
**
**    ENDLOOP.
*
*    READ TABLE IT_1T WITH KEY BEGZT = IT_1T-BEGZT
*                              ENDZT = IT_1T-ENDZT.
*    IF SY-SUBRC NE 0.
*      MESSAGE E000(ZZ) WITH TEXT-M06.
*    ENDIF.
*
*    DATA: LW_INDEX LIKE SY-INDEX.
*    LW_INDEX = SY-TABIX + 1.
*
*    READ TABLE IT_1T INDEX LW_INDEX.
*    IF SY-SUBRC NE 0.
*      MESSAGE E000(ZZ) WITH TEXT-M06.
*    ENDIF.
*
*    MOVE: IT_1T-BEGZT(8)   TO W_CURRENT_DATE,
*          IT_1T-BEGZT+8(6) TO W_CURRENT_TIME.
*
*    CLEAR: ZTMM_STL_EXEC.
*    MOVE: SY-REPID    TO ZTMM_STL_EXEC-REPID,
*          IT_1T-BEGZT TO ZTMM_STL_EXEC-DTIME,
*          SY-UNAME    TO ZTMM_STL_EXEC-ERNAM,
*          SY-UZEIT    TO ZTMM_STL_EXEC-ERZET,
*          SY-DATUM    TO ZTMM_STL_EXEC-ERDAT,
*          SY-UNAME    TO ZTMM_STL_EXEC-AENAM,
*          SY-UZEIT    TO ZTMM_STL_EXEC-AEZET,
*          SY-DATUM    TO ZTMM_STL_EXEC-AEDAT.
*    INSERT ZTMM_STL_EXEC.
*  ENDIF.
*ENDFORM.                    " save_running_time

*&---------------------------------------------------------------------*
*&      Form  get_tqty_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_tqty_new.
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
  DATA : lv_tqty LIKE resb-bdmng.  "Target Quantity.

*--- requirement quantity
  IF it_toline-stock_check = 'X'.
    IF it_toline-zmnmx EQ c_min.
      lv_tqty = it_toline-bdmng.
*      PERFORM get_previous_qty.
    ENDIF.
  ELSE.
    lv_tqty = it_toline-bdmng.
  ENDIF.

*--- check current stock     1.
  IF it_toline-stock_check = 'X'.
    CLEAR : it_stock.
    READ TABLE it_stock WITH KEY matnr = it_toline-matnr.
    MOVE : it_stock-labst TO it_toline-labst.
    lv_tqty = lv_tqty - it_toline-labst.
  ENDIF.

*--- check safety stock     2.
  IF it_toline-stock_check = 'X'.
    lv_tqty = lv_tqty + it_toline-lpmin.
  ENDIF.

*--- check Open TO quantity     3.
  IF it_toline-stock_check = 'X'.
    CLEAR : it_open.
    READ TABLE it_open WITH KEY matnr = it_toline-matnr.
    MOVE : it_open-bdmng TO it_toline-open_qty.
    lv_tqty = lv_tqty - it_toline-open_qty.
  ENDIF.

*--- check backflush error quantity     4.
  IF it_toline-stock_check = 'X'.
    CLEAR : it_bferror.
    READ TABLE it_bferror WITH KEY matnr = it_toline-matnr.
    MOVE : it_bferror-erfmg TO it_toline-bferrorqty.
    lv_tqty = lv_tqty + it_toline-bferrorqty.
  ENDIF.

*--- check scheduled backflush quantity     5.
  IF it_toline-stock_check = 'X'.
    CLEAR : it_back.
    READ TABLE it_back WITH KEY matnr = it_toline-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE : it_back-komp_quant TO it_toline-bf_to_qty.
      lv_tqty = lv_tqty + it_toline-bf_to_qty.
    ENDIF.
  ENDIF.

*--- check previous TO quantity     6.
  IF it_toline-stock_check = 'X'.
    IF it_toline-zmnmx EQ c_min.
      lv_tqty = lv_tqty + w_nsola.
    ENDIF.
  ENDIF.

*---
  IF lv_tqty LT 1.
    MOVE : 'X' TO w_minus_stock_check.
  ENDIF.

**C__ by paul
**  IF W_MINUS_STOCK_CHECK EQ SPACE.
***--- rounding value check
***A. Get Remainder  :mod
***B. Get quotient   :div
**    DATA : LV_TQTYMODRDMNG TYPE P,
**           LV_TQTYDIVRDMNG TYPE P.
**
**    CLEAR : IT_ROUND.
**    READ TABLE IT_ROUND WITH KEY MATNR = IT_TOLINE-MATNR.
**    MOVE : IT_ROUND-BEHMG TO IT_TOLINE-RDMNG.
**
**    IF NOT IT_TOLINE-RDMNG IS INITIAL.
**      LV_TQTYMODRDMNG = LV_TQTY MOD IT_TOLINE-RDMNG.
**      LV_TQTYDIVRDMNG = LV_TQTY DIV IT_TOLINE-RDMNG.
**    ENDIF.
**
**    IF NOT LV_TQTYMODRDMNG IS INITIAL.
**      LV_TQTYDIVRDMNG = LV_TQTYDIVRDMNG + 1.
**      LV_TQTY = LV_TQTYDIVRDMNG * IT_TOLINE-RDMNG.
**    ENDIF.
**    IT_TOLINE-TQTY = LV_TQTY.   "Target Qty
**  ELSE.
**    IT_TOLINE-TQTY = 0.
**  ENDIF.
  it_toline-tqty = lv_tqty.   "Target Qty
ENDFORM.                    " get_tqty_new

*&---------------------------------------------------------------------*
*&      Form  get_previous_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM GET_PREVIOUS_QTY.
**---
*  CLEAR : LTAK, LTAP, W_NSOLA.
*
*  SELECT SUM( NSOLA ) INTO W_NSOLA
*                      FROM LTAK AS A INNER JOIN LTAP AS B
*                        ON A~MANDT EQ B~MANDT
*                       AND A~LGNUM EQ B~LGNUM
*                       AND A~TANUM EQ B~TANUM
*                     WHERE A~STDAT EQ W_PRE_S_DATE
*                       AND A~ENDAT EQ W_PRE_E_DATE
*                       AND A~STUZT EQ W_PRE_S_TIME
*                       AND A~ENUZT EQ W_PRE_E_TIME
*                       AND B~MATNR EQ IT_TOLINE-MATNR
**--- except cancel(delete)
*                       AND NOT
*                           ( ( B~PQUIT EQ 'X'  OR B~PVQUI EQ 'X' )
*                         AND ( B~VORGA EQ 'ST' OR B~VORGA EQ 'SL' ) ).
*ENDFORM.                    " get_previous_qty
*&---------------------------------------------------------------------*
*&      Form  get_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_time.
  DATA: lw_current(14),
        lw_index LIKE sy-index.

  DATA: lw_time LIKE sy-uzeit.

  CONCATENATE p_cdate p_ctime INTO lw_current.

************** { by IG.MOON 8/6/2012

  LOOP AT it_1t WHERE begzt <= lw_current
                  AND endzt >= lw_current.
  ENDLOOP.

*  IF SY-SUBRC NE 0.
*    MESSAGE S000(ZZ) WITH TEXT-M10.
*    LEAVE TO SCREEN 0.
*  ENDIF.

  READ TABLE it_1t WITH KEY begzt = it_1t-begzt
                            endzt = it_1t-endzt.
*  IF SY-SUBRC NE 0.
*    MESSAGE E000(ZZ) WITH TEXT-M06.
*    LEAVE TO SCREEN 0.
*  ENDIF.

  IF sy-subrc EQ 0.

*  ----- Set current working time zone
    MOVE: it_1t-begzt+8(6) TO w_starting_time,
          it_1t-endzt+8(6) TO w_ending_time,
          it_1t-begzt(8)   TO w_screen_date,
          it_1t-endzt(8)   TO w_ending_date.

    w_pre_e_date = w_screen_date.
    lw_time = it_1t-begzt+8(6).
    w_pre_e_time =  lw_time - 1.

*----- Set Previous working time zone
    lw_index = sy-tabix - 1.
    CLEAR : it_1t.
    READ TABLE it_1t INDEX lw_index.
    IF sy-subrc NE 0.
      MESSAGE s000(zz) WITH text-m11.
      LEAVE TO SCREEN 0.
    ENDIF.

    MOVE : it_1t-begzt(8)   TO w_pre_s_date,
           it_1t-begzt+8(6) TO w_pre_s_time.

  ELSE.

    MOVE: lw_current+8(6) TO w_starting_time,
          lw_current(8)   TO w_screen_date.

    PERFORM time_calculation USING    w_screen_date
                                      w_starting_time
                                      60   "Minutes
                             CHANGING w_ending_date
                                      w_ending_time.

    w_pre_e_date = w_screen_date.
    lw_time = w_starting_time.

    w_pre_e_time =  lw_time - 1.
    PERFORM time_calculation USING    w_screen_date
                                      w_starting_time
                                      -60   "Minutes
                             CHANGING w_pre_s_date
                                      w_pre_s_time.

  ENDIF.
*************** }

*----- Check lunch time
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
*&      Form  get_time_from_minutes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE         text
*      -->(IM_MINUTES)  text
*      -->VALUE         text
*      -->(EX_TIME)     text
*----------------------------------------------------------------------*
FORM get_time_from_minutes USING    value(im_minutes)
                           CHANGING value(ex_time) TYPE t.
*---
  CLEAR : ex_time.
  DATA : BEGIN OF ls_time,
           hour(2) TYPE n,
           minute(2) TYPE n,
           second(2) TYPE n,
         END OF ls_time.

  ls_time-minute = im_minutes MOD 60.
  ls_time-hour   = im_minutes DIV 60.

  MOVE ls_time TO ex_time.
ENDFORM.                    " get_time_from_minutes

*&---------------------------------------------------------------------*
*&      Form  TIME_CALCULATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE         text
*      -->(IM_DATE)     text
*      -->VALUE         text
*      -->(IM_TIME)     text
*      -->VALUE         text
*      -->(IM_MINUTES)  text
*      -->VALUE         text
*      -->(EX_DATE)     text
*      -->VALUE         text
*      -->(EX_TIME)     text
*----------------------------------------------------------------------*
FORM time_calculation USING    value(im_date) TYPE d
                               value(im_time) TYPE t
                               value(im_minutes)
                      CHANGING value(ex_date) TYPE d
                               value(ex_time) TYPE t.
*---
  CLEAR : ex_date, ex_time.
  DATA : lv_time    TYPE t.
  DATA : lv_hoursum TYPE p.

  PERFORM get_time_from_minutes USING    im_minutes
                                CHANGING lv_time.

  lv_hoursum = im_time(2) + lv_time(2).

  ex_date = im_date.

  IF im_minutes < 0.
    ex_time = im_time - lv_time.
  ELSE.
    ex_time = im_time + lv_time.
    MOVE '00' TO ex_time+4(2).
  ENDIF.

  IF lv_hoursum >= 24.
    ex_date = ex_date + 1.
  ENDIF.

ENDFORM.                    " time_calculation

*&---------------------------------------------------------------------*
*&      Form  call_working_time_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_working_time_function.
*---
  DATA : l_datum TYPE d.

  CLEAR : it_dummy, it_dummy[], it_1t, it_1t[], l_datum.

  PERFORM get_previous_date USING l_datum.
  wa_pre_datum = l_datum.

  CALL FUNCTION 'Z_FMM_GET_WORKING_TIME'
    EXPORTING
      i_datum              = l_datum
      i_day                = c_day
      i_arbpl              = p_arbpl
    TABLES
      t_working_time       = it_dummy
      t_1t                 = it_1t
    EXCEPTIONS
      cannot_read_dayname  = 1
      incorrect_shift_info = 2
      incorrect_capa_info  = 3
      OTHERS               = 4.

  IF sy-subrc NE 0.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE e999 WITH text-m21.
      WHEN 2.
        MESSAGE e999 WITH text-m22.
      WHEN 3.
        MESSAGE e999 WITH text-m23.
      WHEN 4.
        MESSAGE e999 WITH text-m24.
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
FORM get_previous_date USING    pw_datum.
  DATA: lw_kalid   LIKE kako-kalid,
        lw_dayfree LIKE hrvsched-noday.

*----- Read Shop Calendar ID, Capacity ID
  SELECT SINGLE b~kalid INTO lw_kalid
    FROM crhd AS a INNER JOIN kako AS b
      ON a~kapid = b~kapid
   WHERE a~werks = p_werks
*     AND a~arbpl = c_arbpl
     AND a~arbpl = p_arbpl
     AND a~lvorm = ' '
     AND b~kapar = '001'.

  pw_datum = wa_datum.

  DO.
    pw_datum = pw_datum - 1.

    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
      EXPORTING
        langu               = sy-langu
        date                = pw_datum
        calid               = lw_kalid
      IMPORTING
        dayfree             = lw_dayfree
      EXCEPTIONS
        no_langu            = 1
        no_date             = 2
        no_daytxt_for_langu = 3
        invalid_date        = 4
        OTHERS              = 5.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    IF lw_dayfree EQ space.
      EXIT.
    ENDIF.

    IF sy-index EQ 200.
      pw_datum = wa_datum.
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
*FORM GET_SOURCE_TYPE.
*  DATA: M_LGTYP LIKE MLGT-LGTYP,
*        M_LGPLA LIKE MLGT-LGPLA.
*  LOOP AT IT_TOLINE.
*    IF IT_TOLINE-SRC_LGTYP IS INITIAL.
*
*      CALL FUNCTION 'Z_MM_LT01_SOURCE_CHECK'
*        EXPORTING
*          P_MATNR       = IT_TOLINE-MATNR
**           P_TOQTY       = 0
*      IMPORTING
**           E_MESS        =
**           ZRESULT       =
*          P_LGTYP       = M_LGTYP
*          P_LGPLA       = M_LGPLA
*                .
*      IT_TOLINE-SRC_LGTYP = M_LGTYP.
*      IT_TOLINE-SRC_LGPLA = M_LGPLA.
*      MODIFY IT_TOLINE.  " transporting src_lgtyp src_lgpla.
*    ENDIF.
*  ENDLOOP.
*ENDFORM.                    " get_source_type
*&---------------------------------------------------------------------*
*&      Form  get_stock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_stock.
  CLEAR : it_stock_temp, it_stock_temp[], it_stock, it_stock[].

  SELECT matnr werks lgort labst
               INTO CORRESPONDING FIELDS OF TABLE it_stock_temp
               FROM mard
                FOR ALL ENTRIES IN it_toline
              WHERE matnr EQ it_toline-matnr
                AND werks = p_werks
                AND lvorm = ' '
**A__PAUL 05/12/2011
                AND lgort = 'P400'.

  LOOP AT it_stock_temp.
    MOVE : it_stock_temp-matnr TO it_stock-matnr,
           it_stock_temp-labst TO it_stock-labst.
    COLLECT it_stock.
    CLEAR : it_stock_temp, it_stock.
  ENDLOOP.

ENDFORM.                    " get_stock
*&---------------------------------------------------------------------*
*&      Form  get_open_reservation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_open_reservation.
*---
  DATA:         l_line TYPE i,
  l_count TYPE i.
  RANGES: rt_matnr FOR mara-matnr.
  CLEAR : it_open_temp, it_open_temp[], it_open, it_open[].
*
*  SELECT MATNR BDMNG
*    INTO CORRESPONDING FIELDS OF TABLE IT_OPEN_TEMP
*    FROM RESB
*     FOR ALL ENTRIES IN IT_TOLINE
*   WHERE MATNR EQ IT_TOLINE-MATNR
*     AND WERKS = P_WERKS
*     AND XLOEK = ''
*     AND KZEAR = ''
*     AND LGORT = 'G100'
*     AND BWART = '311'
*     AND UMLGO = 'P400'.
*
*  LOOP AT IT_OPEN_TEMP.
*    MOVE-CORRESPONDING IT_OPEN_TEMP TO IT_OPEN.
*    COLLECT IT_OPEN.
*    CLEAR : IT_OPEN_TEMP, IT_OPEN.
*  ENDLOOP.
**Made by IG ask to him 06/09/11

*  LOOP AT IT_TOLINE.
*
*    SELECT MATNR SUM( BDMNG ) INTO (IT_OPEN-MATNR,IT_OPEN-BDMNG)
*            FROM RESB
*           WHERE MATNR EQ IT_TOLINE-MATNR
*             AND WERKS = P_WERKS
*             AND XLOEK = ''
*             AND KZEAR = ''
*             AND LGORT = 'G100'
*             AND BWART = '311'
*             AND UMLGO = 'P400'
*             GROUP BY MATNR WERKS XLOEK KZEAR LGORT BWART UMLGO
*             %_HINTS ORACLE 'INDEX("RESB" "RESB~Z02")'.
*      APPEND IT_OPEN.
*    ENDSELECT.
*
*  ENDLOOP.

* Tuned by IG Moon 11/2/2011

  LOOP AT it_toline.

    SELECT matnr SUM( bdmng ) INTO (it_open-matnr,it_open-bdmng)
            FROM resb
           WHERE bdart = 'MR'
             AND matnr EQ it_toline-matnr
             AND werks = p_werks
             AND xloek = ''
             AND kzear = ''
             AND umlgo = 'P400'
             AND lgort = 'G100'
             AND bwart = '311'
             GROUP BY matnr
             %_HINTS ORACLE 'INDEX("RESB" "RESB~Z08")'.
      APPEND it_open.
    ENDSELECT.

  ENDLOOP.

ENDFORM.                    " get_open_reservation
*&---------------------------------------------------------------------*
*&      Form  GET_CONTROL_CYCLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_control_cycle.
  CLEAR : it_cc, it_cc[].

  SELECT a~matnr
         b~umlgo
         b~zfeeder
         b~ablad
         b~prvbe
         b~pknum
    INTO CORRESPONDING FIELDS OF TABLE it_cc
    FROM marc AS a
   INNER JOIN pkhd AS b
      ON a~matnr = b~matnr
     AND a~werks = b~werks
**C__ PAUL : CHANGE 06/15/11
**     AND A~VSPVB =  B~PRVBE

     FOR ALL ENTRIES IN it_toline

   WHERE a~matnr EQ it_toline-matnr
     AND a~werks EQ p_werks
     AND lvorm EQ space.

ENDFORM.                    " GET_CONTROL_CYCLE
*&---------------------------------------------------------------------*
*&      Form  GET_DES_STORAGE_LOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_LGORT  text
*----------------------------------------------------------------------*
FORM get_des_storage_loc.
  CLEAR : it_dest, it_dest[].

  SELECT a~lgort
         b~matnr
**S__Paul ADD
         b~dispo
**E<06/17/11
    INTO CORRESPONDING FIELDS OF TABLE it_dest
    FROM pvbe AS a
   INNER JOIN marc AS b
      ON b~vspvb EQ a~prvbe
     AND b~werks EQ a~werks

     FOR ALL ENTRIES IN it_toline

   WHERE b~matnr EQ it_toline-matnr
     AND b~werks EQ p_werks
     AND b~lvorm EQ space.

ENDFORM.                    " GET_DES_STORAGE_LOC
*&---------------------------------------------------------------------*
*&      Form  CREATE_S2L_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_s2l_order.
*---
  DATA : l_tabix LIKE sy-tabix,
         l_messa(80).
  DATA : lw_begzt(14).
  DATA : l_datum TYPE d,
         lv_date(8).
  DATA : l_stats LIKE ztmm_stl_log-stats.
*BAPI
  DATA: ls_packing LIKE bapi1172_packing_instruction,
        lf_resno   TYPE bapi2093_res_key-reserv_no,
        ls_header  LIKE bapi2093_res_head,
        lt_items   TYPE TABLE OF bapi2093_res_item WITH HEADER LINE,
        lt_segment TYPE TABLE OF bapi_profitability_segment
        WITH HEADER LINE,
        lt_parex   TYPE TABLE OF bapiparex WITH HEADER LINE,
        ls_requested LIKE bapi1172_requested_qty,
        ls_deliverytime LIKE bapi1172_delvrytime,
        lt_return  TYPE TABLE OF bapiret2 WITH HEADER LINE,
        lt_result LIKE bapi1075_3 OCCURS 0 WITH HEADER LINE,
        ls_return LIKE bapiret2.

  DATA : it_m039 LIKE zmmt0039 OCCURS 0 WITH HEADER LINE.

  CLEAR : it_itab, it_itab[], it_dispo, it_dispo[],
          it_m039, it_m039[].

  LOOP AT it_toline.
    CLEAR : l_stats.
    MOVE : sy-tabix TO l_tabix.

*--- TO GET Start Date(Feeding Date) and Start Time
    MOVE : w_screen_date   TO it_toline-sdate,
           w_starting_time TO it_toline-feeding_time,
           w_ending_date   TO it_toline-edate,
           w_ending_time   TO it_toline-etime.
    CLEAR : w_minus_stock_check.

*--- calculate quantity
    PERFORM get_tqty_new.

*--- App Doc No
*    PERFORM NUMBER_GET_NEXT USING    C_NRO_NR_09     "NRO Interval
*                                     W_NRO_OBJECT    "NRO Object
*                            CHANGING W_ZDOCNO.     "App Doc No
*    COMMIT WORK.

    IF w_minus_stock_check EQ space.

*--- Get Destination Storage location
      CLEAR : it_dest.
      READ TABLE it_dest WITH KEY matnr = it_toline-matnr.

      CLEAR : it_cc.
      READ TABLE it_cc WITH KEY matnr = it_toline-matnr.

      IF p_test = 'X'.
        PERFORM fill_m039.
        CONTINUE.
      ENDIF.

*--- BAPI to create reservation
      PERFORM user_date_format USING sy-uname it_toline-edate
               CHANGING lv_date.

      ls_header-move_type  = '311'.
** Furong on 07/19/12
      ls_header-res_date   = p_ddate.
*      LS_HEADER-RES_DATE   = SY-DATUM.
** end on 07/19/12
      ls_header-created_by = sy-uname.
      ls_header-move_plant = p_werks.
      ls_header-move_stloc = it_dest-lgort.

      CLEAR lt_items.

      lt_items-plant       = p_werks.
      lt_items-material    = it_toline-matnr.
      lt_items-stge_loc    = it_cc-umlgo.
      lt_items-unload_pt   = it_cc-ablad.
      lt_items-entry_qnt   = it_toline-tqty.
      lt_items-item_text   = it_toline-feeding_time.

*S__Change by Paul
*      LT_ITEMS-REQ_DATE    = LV_DATE.
      lt_items-req_date    = it_toline-edate.
*E__<
      lt_items-movement    = 'X'.
      lt_items-gr_rcpt     = it_cc-zfeeder.

      APPEND lt_items.

      CLEAR lt_parex.
      lt_parex-structure   = 'RESB'.
      lt_parex-valuepart1  = it_cc-ablad.
      lt_parex-valuepart2  = it_toline-feeding_time.
      APPEND lt_parex.

*-CREATE RESERVATION
*    CALL FUNCTION 'BAPI_RESERVATION_CREATE1'
*    	EXPORTING
*       RESERVATIONHEADER    = LS_HEADER
*    	IMPORTING
*       RESERVATION          = LF_RESNO
*    	TABLES
*       RESERVATIONITEMS     = LT_ITEMS
*       PROFITABILITYSEGMENT = LT_SEGMENT
*       RETURN               = LT_RETURN
*       EXTENSIONIN          = LT_PAREX.
*      READ TABLE LT_RETURN WITH KEY TYPE = 'E'.
*      IF LT_RETURN-TYPE EQ 'E'.
*        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*      ELSE.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*             EXPORTING
*                  	WAIT = 'X'.
*
*        IT_M039-PKKEY  = '998'.
*        IT_M039-RSNUM  = LF_RESNO.     "Reservation Number
*        IT_M039-RSPOS  = '0001'.       "Reservation Item
*        IT_M039-BUDAT  = IT_TOLINE-EDATE.        "Posting Date
*        IT_M039-PKTIM  = IT_TOLINE-FEEDING_TIME. "Request Time
*        IT_M039-MATNR  = IT_TOLINE-MATNR.          "Material Number
*        IT_M039-PRVBE  = IT_CC-PRVBE.              "Supply Area
*        IT_M039-ABLAD  = IT_CC-ABLAD.              "Workstation
*       IT_M039-PKBMG  = IT_TOLINE-TQTY.           "Requirements
*Quantity
*        IT_M039-MEINS  = IT_TOLINE-MEINS.          "Unit of Measure
*        IT_M039-WERKS  = P_WERKS.                  "Plant
*        IT_M039-REVERSED = "".                     "Reversed
*        IT_M039-LGORT  = LS_HEADER-MOVE_STLOC.     "Storage Location
*        IT_M039-LGPRO  = IT_DEST-LGORT.       "Issue Storage Location
*      IT_M039-ZFEEDER  = IT_CC-ZFEEDER.       "Goods Recipient
*(ZFEEDER)
*
*        APPEND IT_M039.
**-- SEND FEEDING ORDER TO GCS INTERFACE (END)
*      ENDIF.
**--- BAPI TO CREATE RESERVATION (END)
*    ENDIF.

      ls_requested-requested_qty = it_toline-tqty.
      ls_requested-base_uom      = it_toline-meins.
**Furong on 07/19/12 for 3 shift
      ls_deliverytime-delvrydate = p_ddate.
*      LS_DELIVERYTIME-DELVRYDATE = SY-DATUM.
** End
**Paul change delivery time instead parameter
      ls_deliverytime-delvrytime = p_dtime.

      CALL FUNCTION 'BAPI_KANBANCC_ADDEVENTDRKANBAN'
          EXPORTING
          kanbancontrolcycle = it_cc-pknum
        requested_qty = ls_requested
        deliverytime = ls_deliverytime
        IMPORTING
          return             = ls_return
        TABLES
          statuschangeresult = lt_result
        EXCEPTIONS
          OTHERS             = 1.

      IF ls_return-type EQ 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        LOOP AT lt_result.

          it_m039-pkkey  = lt_result-kanban_id.
          it_m039-rsnum  = lt_result-reserv_no.
          it_m039-rspos  = '0001'.                 "Reservation Item
          it_m039-budat  = it_toline-edate.        "Posting Date
          it_m039-pktim  = it_toline-feeding_time. "Request Time
          it_m039-matnr  = lt_result-material.     "Material Number
          it_m039-prvbe  = lt_result-supplyarea.   "Supply Area
          it_m039-ablad  = it_cc-ablad.            "Workstation
          it_m039-pkbmg  = lt_result-kanban_qty.  "Requirements
*Quantity
          it_m039-meins  = lt_result-base_uom.     "Unit of Measure
          it_m039-werks  = lt_result-plant.        "Plant
          it_m039-reversed = "".                   "Reversed
          it_m039-lgort  = ls_header-move_stloc.   "Storage Location
          it_m039-lgpro  = it_dest-lgort.       "Issue Storage Location
          it_m039-zfeeder  = it_cc-zfeeder.          "Goods Recipient

          APPEND it_m039.
*I__PAUL Change IT_M039 TO IT_ITAB
          MOVE-CORRESPONDING it_toline TO it_itab.
*          MOVE-CORRESPONDING IT_M039 TO IT_ITAB.

          it_itab-tqty  = it_m039-pkbmg.
          it_itab-rdmng = it_m039-pkbmg.
          it_itab-zrhlh = it_toline-zrhlh.

*--- App Doc No
          PERFORM number_get_next USING    c_nro_nr_09     "NRO Interval
                                           w_nro_object    "NRO Object
                                  CHANGING w_zdocno.     "App Doc No

          MOVE : it_dest-lgort         TO it_itab-des_lgort.
          MOVE : it_cc-umlgo           TO it_itab-src_sloc.
          MOVE : w_zdocno              TO it_itab-w_docno.
          MOVE : w_nsola               TO it_itab-nsola.
          MOVE : l_stats               TO it_itab-stats.

          CASE ls_return-type.
            WHEN 'E'.
              MOVE : c_red TO it_itab-linecolor,
                'E'    TO it_itab-msgty.
*--- message
              MOVE : ls_return-message TO it_itab-messa.
              MOVE : ls_return-id      TO it_itab-msgid,
                 ls_return-number  TO it_itab-msgnr.
            WHEN 'S' OR ''.
              MOVE : c_green  TO it_itab-linecolor,
                 'S'      TO it_itab-msgty.
              MOVE : lt_result-reserv_no TO it_itab-rsnum.
            WHEN OTHERS.

          ENDCASE.
*          ENDIF.

          APPEND it_itab.
          CLEAR : it_itab, it_m039.

        ENDLOOP.
*-- SEND FEEDING ORDER TO GCS INTERFACE (END)
      ENDIF.
*--- BAPI TO CREATE RESERVATION (END)

**A__Paul Add itab postion change & use else (not use bapi data display)
*    ENDIF.
    ELSE.

      MODIFY it_toline INDEX l_tabix.

      MOVE-CORRESPONDING it_toline TO it_itab.

*--- App Doc No
      PERFORM number_get_next USING    c_nro_nr_09     "NRO Interval
                                       w_nro_object    "NRO Object
                              CHANGING w_zdocno.     "App Doc No

      MOVE : it_dest-lgort         TO it_itab-des_lgort.
      MOVE : it_cc-umlgo           TO it_itab-src_sloc.
      MOVE : w_zdocno              TO it_itab-w_docno.
      MOVE : w_nsola               TO it_itab-nsola.
      MOVE : l_stats               TO it_itab-stats.

***      IF W_MINUS_STOCK_CHECK EQ SPACE.
****        IF W_SUBRC EQ 0.
***        READ TABLE LT_RETURN WITH KEY TYPE = 'S'.
***        IF SY-SUBRC = 0.
***          	MOVE : C_GREEN TO IT_ITAB-LINECOLOR,
***              'S'      TO IT_ITAB-MSGTY.
***          	MOVE : LF_RESNO TO IT_ITAB-RSNUM.
***        ELSE.
***          READ TABLE LT_RETURN WITH KEY TYPE = 'E'.
***          IF SY-SUBRC = 0.
***            	MOVE : C_RED TO IT_ITAB-LINECOLOR,
***               'E'    TO IT_ITAB-MSGTY.
***          ENDIF.
***        ENDIF.
***      ENDIF.
***
****--- message
***      MOVE : LT_RETURN-MESSAGE TO IT_ITAB-MESSA.
***      MOVE : LT_RETURN-ID      TO IT_ITAB-MSGID,
***  	     LT_RETURN-NUMBER  TO IT_ITAB-MSGNR.
      APPEND it_itab.
    ENDIF.
**E__06/08/11
    MOVE : it_toline-dispo TO it_dispo-dispo.
    COLLECT it_dispo.
    CLEAR : it_toline, it_itab, it_dispo, it_m039,
            ls_requested, ls_deliverytime, ls_return,
            lt_return[], lt_return, lt_result, lt_result[].
  ENDLOOP.

*  IF NOT IT_M039[] IS INITIAL.
*    MODIFY ZMMT0039 FROM TABLE IT_M039.
*  ENDIF.

ENDFORM.                    " CREATE_S2L_ORDER
*&---------------------------------------------------------------------*
*&      Form  fill_m039
*&---------------------------------------------------------------------*
FORM fill_m039.

  MOVE-CORRESPONDING it_toline TO it_itab.
  it_itab-tqty  = it_toline-tqty.
  it_itab-rdmng = it_toline-tqty.
  it_itab-zrhlh = it_toline-zrhlh.
  it_itab-des_lgort = it_dest-lgort.
  it_itab-src_sloc  = it_cc-umlgo.
  APPEND it_itab.

ENDFORM.                                                    " fill_m039
