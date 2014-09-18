*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0010TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& # DATA
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  TABLES
*&---------------------------------------------------------------------*
TABLES : equi, mara, mard, mbew, marc , T001W .
TABLES : USR01 .
*&---------------------------------------------------------------------*
*&  TYPE-POOL
*&---------------------------------------------------------------------*
TYPE-POOLS : slis .
TYPE-POOLS : kcde  .
TYPE-POOLS : truxs .
TYPE-POOLS : sydes .

*&---------------------------------------------------------------------*
*&  CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS : c_a TYPE char01 VALUE 'A' ,
            c_b TYPE char01 VALUE 'B' ,
            c_c TYPE char01 VALUE 'C' ,
            c_d TYPE char01 VALUE 'D' ,
            c_e TYPE char01 VALUE 'E' ,
            c_f TYPE char01 VALUE 'F' ,
            c_h TYPE char01 VALUE 'H' ,
            c_i TYPE char01 VALUE 'I' ,
            c_n TYPE char01 VALUE 'N' ,
            c_p TYPE char01 VALUE 'P' ,
            c_s TYPE char01 VALUE 'S' ,
            c_x TYPE char01 VALUE 'X' .

CONSTANTS : c_01 TYPE char02 VALUE '01' ,
            c_02 TYPE char02 VALUE '02' ,
            c_03 TYPE char02 VALUE '03' ,
            c_04 TYPE char02 VALUE '04' ,
            c_05 TYPE char02 VALUE '05' ,
            c_06 TYPE char02 VALUE '06' ,
            c_07 TYPE char02 VALUE '07' ,
            c_08 TYPE char02 VALUE '08' ,
            c_09 TYPE char02 VALUE '09' ,
            c_10 TYPE char02 VALUE '10' ,
            c_99 TYPE char02 VALUE '99' .

*&---------------------------------------------------------------------*
*&  GLOBAL VARIABLES
*&---------------------------------------------------------------------*

* Job_start, Job_end.
DATA: g_job_start_date LIKE sy-datum,
      g_job_start_time TYPE syuzeit,
      g_job_end_date   LIKE sy-datum,
      g_job_end_time   TYPE syuzeit.

* Result count
DATA : g_total   LIKE sy-tabix ,
       g_success LIKE sy-tabix ,
       g_error   LIKE sy-tabix .

DATA : g_time_stamp  TYPE timestampl.

*. Destination
DATA: G_DEST   TYPE RFCDEST VALUE 'EAIPM'.
*&---------------------------------------------------------------------*
*&  INTERNAL TABLES
*&---------------------------------------------------------------------*

DATA : BEGIN OF gt_data OCCURS 0 ,
        mark ,
        icon(4) ,
        FERTH like mara-FERTH ,
        MFRPN like mara-MFRPN .
        INCLUDE STRUCTURE zhkpmt0001 .
DATA   END OF gt_data .

*&---------------------------------------------------------------------*
*&  ALV
*&---------------------------------------------------------------------*
DATA : gt_fieldcat    TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       gt_fieldcat1   TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       gs_fieldcat    TYPE slis_fieldcat_alv,
       gt_event       TYPE slis_t_event,
       gs_event       TYPE slis_alv_event,
       gt_event_exit  TYPE slis_t_event_exit,
       gs_event_exit  TYPE slis_event_exit,
       gt_listheader  TYPE slis_t_listheader, "TOP-OF-PAGE
       gs_listheader  TYPE slis_listheader,
       gt_sortcat     TYPE slis_t_sortinfo_alv,
       gt_sortcat1    TYPE slis_t_sortinfo_alv,
       gs_sortcat     TYPE slis_sortinfo_alv,
       gs_layout      TYPE slis_layout_alv,
       g_variant      LIKE disvariant,
       g_repid        LIKE sy-repid,
       g_slis_print   TYPE slis_print_alv,
       g_save.

DATA: gt_colinfo_table   TYPE slis_t_specialcol_alv WITH HEADER LINE,
      gs_celltab         TYPE lvc_s_styl,
      gt_celltab         TYPE lvc_t_styl.

DATA: g_ucomm      TYPE sy-ucomm,
      g_ok         TYPE c,
      ok_code      TYPE sy-ucomm.

DATA: g_write_gubun(2).

DATA: g_name(50)   TYPE c.

FIELD-SYMBOLS: <fs_tab>  TYPE STANDARD TABLE,
               <fs_tabe> TYPE STANDARD TABLE,
               <fs_tab1> TYPE STANDARD TABLE.

CONSTANTS : c_status_set       TYPE slis_formname VALUE 'PF_STATUS_SET',
            c_user_command     TYPE slis_formname VALUE 'USER_COMMAND',
            c_top_of_page      TYPE slis_formname VALUE 'TOP_OF_PAGE',
            c_top_of_list      TYPE slis_formname VALUE 'TOP_OF_LIST',
            c_end_of_list      TYPE slis_formname VALUE 'END_OF_LIST',
            c_data_changed     TYPE slis_formname VALUE 'DATA_CHANGED'.

*&---------------------------------------------------------------------*
*& # Selection Screen
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_matnr FOR mara-matnr ,
                 s_werks FOR T001W-werks ,
                 s_lgort FOR mard-lgort ,
                 s_mtart FOR mara-mtart OBLIGATORY DEFAULT 'ERSA' ,
                 s_date  FOR mara-ersda .

PARAMETERS : p_dest   TYPE rfcdest .
SELECTION-SCREEN END OF BLOCK b1.
