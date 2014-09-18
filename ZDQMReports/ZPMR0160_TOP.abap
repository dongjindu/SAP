*&---------------------------------------------------------------------*
*&  Include           ZLESINFOT01
*&---------------------------------------------------------------------*

TABLES: mard, mara, eban, ebkn, ekpo, eket, ekbe, marc.
TABLES sscrfields.

INCLUDE <icon>.
INCLUDE <symbol>.
INCLUDE ole2incl.

TYPE-POOLS:slis.
TYPE-POOLS kcde .

DATA : c_st_nm    LIKE dd02l-tabname VALUE 'ZSPM0019',
       c_st_nm_2  LIKE dd02l-tabname VALUE 'ZSPM0020'.

DATA : row TYPE i,
       rrow TYPE i,
       column LIKE sy-index,
       ccolumn LIKE sy-index.
DATA : excel TYPE ole2_object,
       books TYPE ole2_object,
       book  TYPE ole2_object,
       range TYPE ole2_object,
       borders TYPE ole2_object,
       cell TYPE ole2_object,
       grade TYPE i.

DATA: w_cell TYPE i VALUE '11',     "셀
      w_text(60),                   "텍스트
      w_cell_index LIKE sy-index.   "셀 인덱스

CONSTANTS: c_num0 VALUE '0',
           c_num1 VALUE '1',
           c_num2 VALUE '2',
           c_num3 VALUE '3',
           c_num4 VALUE '4',
           c_num5 VALUE '5',
           c_num6 VALUE '6',
           c_num7 VALUE '7'.

DATA  :g_colps    TYPE i,
       g_line     TYPE i,
       g_check,
       err_flag,
       sy_subrc   TYPE sy-subrc,
       old_mblnr  LIKE mseg-mblnr,
       old_mblnr1 LIKE mseg-mblnr,
       upmode LIKE ctu_params-updmode VALUE 'S',
*                              "S: synchronously
*                              "A: asynchronously
*                              "L: local

       dismode LIKE ctu_params-dismode VALUE 'N'.
*"A: show all dynpros
*"E: show dynpro on error only
*"N: do not display dynpro

*     BDC관련
DATA: it_bdc LIKE bdcdata OCCURS 0 WITH HEADER LINE,    "BDC DATA
      it_msg LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE. "MESSAGE.

*     컨버젼
DATA: c_zkey(20),
      c_posnr(6) TYPE n,
      c_matnr LIKE mara-matnr,
      c_kunnr LIKE kna1-kunnr,
      g_tabix LIKE sy-tabix.

DATA : it_xls TYPE STANDARD TABLE OF kcde_cells
                                    WITH HEADER LINE.

* BDC 관련
DATA : bdcdata LIKE bdcdata OCCURS 0 WITH HEADER LINE,
       messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

* Log List
DATA: BEGIN OF it_log OCCURS 0,
        vkorg LIKE vbak-vkorg,
        audat LIKE vbak-audat.
        INCLUDE STRUCTURE bapiret2.
DATA: END OF it_log.

* DATA
DATA: g_skip.   "플랜트 선택 후 첫 화면 건너 뛰기

*----------------------------------------------------------------------*
* SMART FORM PRINT DECLARATION
*----------------------------------------------------------------------*
DATA : v_fname TYPE rs38l_fnam.
DATA: ssfctrlop TYPE ssfctrlop,
      ssfcompop TYPE ssfcompop,
      ssfcresop TYPE ssfcresop.


DATA: BEGIN OF it_sto_zero OCCURS 0,
        ebeln LIKE eket-ebeln,
        ebelp LIKE eket-ebelp,
     END OF it_sto_zero.


*----------------------------------------------------------------------*
*  CONSTANTS                                                           *
*----------------------------------------------------------------------*
CONSTANTS : c_status_set       TYPE slis_formname VALUE 'PF_STATUS_SET',
            c_user_command     TYPE slis_formname VALUE 'USER_COMMAND',
            c_top_of_page      TYPE slis_formname VALUE 'TOP_OF_PAGE',
            c_top_of_list      TYPE slis_formname VALUE 'TOP_OF_LIST',
            c_end_of_list      TYPE slis_formname VALUE 'END_OF_LIST'.

*----------------------------------------------------------------------*
*                    변수 선언                                         *
*----------------------------------------------------------------------*
DATA : gv_date  LIKE sy-datum,
       gv_sdate LIKE sy-datum,
       gv_ldate LIKE sy-datum,
       gv_cdate LIKE sy-datum,
       g_last_date LIKE sy-datum,
       gv_tabix LIKE sy-tabix,
*       g_ymon01 LIKE s510-spmon,
*       gv_vrsio LIKE s510-vrsio,
*       pr_spmon LIKE s510-spmon,
       g_flg,
       g_answer.

DATA: c_second TYPE i,   "컨버젼 00->INT
      v_field(50),       "필드명
      v_field1(50),
      v_dum,
      f_end,             "종료플래그
      v_scr TYPE i,      "스크린 레블
      v_chk,             "체크박스
      v_asel,            "전체선택
      v_dsel,            "전체해제
      f_err,             "DB UPDATE오류
      old_ttype(2) TYPE n,
      old_ttype1(2) TYPE n,
      v_mesg(50),
      o_cpage LIKE sy-cpage,
      o_staro LIKE sy-staro,
      o_staco LIKE sy-staco,
      v_ans.
*----------------------------------------------------------------------*
* DECLARATION FOR SEARCH HELP
*----------------------------------------------------------------------*
DATA DYNPREAD LIKE DYNPREAD OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF VALUETAB OCCURS 0,
          VALUE(80).
DATA: END OF VALUETAB.

DATA: BEGIN OF FIELDS OCCURS 0.
        INCLUDE STRUCTURE HELP_VALUE.
DATA: END OF FIELDS.

DATA: BEGIN OF DYNPFIELDS  OCCURS 0.
        INCLUDE STRUCTURE DYNPREAD.
DATA: END OF DYNPFIELDS.

DATA  SELECT_INDEX LIKE SY-TABIX.

DATA: BEGIN OF SELECT_VALUES OCCURS 0.
        INCLUDE STRUCTURE HELP_VTAB.
DATA: END OF SELECT_VALUES.
