************************************************************************
* Program Name      : YIPP102I_PCC_CHECK
* Author            : Bobby
* Creation Date     : 2004.02.08.
* Specifications By : Bobby
* Pattern           : 5.2.2
* Development Request No :
* Addl Documentation:
* Description       : PCC Check for the Sequenced data
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  yipp102i_pcc_check  MESSAGE-ID zmpp  .

*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
TABLES: ztpp_common_vals,
        equi ,
        ausp .

************* DO NOT USE!!!!! *****************************************
DATA: wa_filename             LIKE  rlgrap-filename,
      wa_filetype             LIKE  rlgrap-filetype VALUE 'DAT',
      wa_bdcgroup             LIKE  sy-uname,
      p_tcode                 LIKE  tstc-tcode                ,
      p_cmode                 TYPE  c                         ,
      it_rec                  LIKE TABLE OF mara       WITH HEADER LINE.
********************************************************************

*----------------------------------------------------------------------*
* INTERNAL TABLES DECLARATION
*----------------------------------------------------------------------*
DATA: BEGIN OF it_vin         OCCURS 0                        .
        INCLUDE STRUCTURE     ztpp_pmt07jb_b .
DATA:   matnr                 LIKE mara-matnr,
      END OF it_vin                          ,
      it_7jb              LIKE TABLE OF it_vin         WITH HEADER LINE,
      it_msg              LIKE TABLE OF bdcmsgcoll     WITH HEADER LINE,
      it_bdcdata          LIKE TABLE OF bdcdata        WITH HEADER LINE,
      it_vmaster          LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

*----------------------------------------------------------------------*
* WORKING-AREA VARIABLES DECLARATION
*----------------------------------------------------------------------*
DATA: wa_material             LIKE mara-matnr                 ,
      wa_number               LIKE ztpp_pp_log_head-logkey    ,
      wa_7jb                  LIKE it_vin                     ,
      wa_7jb_log              LIKE it_vin                     ,
      wa_maxday               LIKE sy-datum                   ,
      wa_minday               LIKE sy-datum                   ,
      wa_vin                  LIKE mara-matnr                 ,
      wa_lines                TYPE i                          ,
      wa_msg(70)              TYPE c                          ,
      wa_mng                  TYPE i                          ,
      wa_snd_jobs             TYPE i                          ,
      wa_rcv_jobs             TYPE i                          ,
      wa_taskname(4)          TYPE n VALUE '0001'             ,
      wa_excp_flag            TYPE c                          ,
      wa_error                TYPE c                          ,
      wa_flag                 TYPE c                          ,
      wa_err_hd               TYPE c                          ,
      wa_mode                 TYPE c   VALUE   'N'            ,
      wa_subrc                LIKE sy-subrc                   ,
      sv_log_color            LIKE mara-matnr                 ,
      jobc                    LIKE tbtcjob-jobcount           ,
      jobn                    LIKE  tbtcjob-jobname           ,
      immediate               LIKE btch0000-char1  VALUE  'X' ,
      wa_count(4)             TYPE n                          ,
      c_prog                  LIKE sy-repid                   .


*----------------------------------------------------------------------*
* Field-Symbols VARIABLES DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS   p_run      AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (55) text-102 FOR FIELD p_run  .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
TOP-OF-PAGE.
*----------------------------------------------------------------------

*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM get_data.
  PERFORM check_pcc_routine.


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_7jb
    FROM ztpp_pmt07jb_b .
ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  CREATE_PCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_pcc USING pa_matnr  pa_werks  pa_text  pa_verid  pa_flag .
  DATA: l_matnr         LIKE  bdcdata-fval ,
        l_werks         LIKE  bdcdata-fval ,
        l_text          LIKE  bdcdata-fval ,
        l_verid         LIKE  bdcdata-fval .

  CLEAR: wa_subrc, it_msg, it_msg[].
  l_matnr = pa_matnr.      l_werks = pa_werks .
  l_text  = pa_text .      l_verid = pa_verid .

  CALL FUNCTION 'Z_FCO_PCC_ORDER_CRE_WITH_PDV'
       EXPORTING
            matnr_001 = l_matnr
            werks_002 = l_werks
            ktext_004 = l_text
            verid_007 = l_verid
            p_first   = pa_flag
       IMPORTING
            subrc     = wa_subrc
       TABLES
            messtab   = it_msg.

  LOOP AT it_msg WHERE msgtyp = 'E'.
    WRITE: / it_msg                .
  ENDLOOP.
ENDFORM.                    " CREATE_PCC

*&---------------------------------------------------------------------*
*&      Form  CHECK_PCC_ROUTINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_pcc_routine.
  DATA: lt_7jb               LIKE TABLE OF it_7jb      WITH HEADER LINE,
        l_verid              LIKE mkal-verid ,
        l_text               LIKE makt-maktx ,
        l_flag               TYPE c          .

  lt_7jb[] = it_7jb[]       .
  LOOP AT lt_7jb.
    CONCATENATE lt_7jb-moye  lt_7jb-dist lt_7jb-bmdl INTO lt_7jb-matnr .
    CONCATENATE lt_7jb-matnr lt_7jb-ocnn             INTO lt_7jb-matnr
      SEPARATED BY space.
    MODIFY lt_7jb .
  ENDLOOP.

  SORT it_7jb BY matnr vers .
  DELETE ADJACENT DUPLICATES FROM lt_7jb COMPARING matnr vers .

  LOOP AT lt_7jb            .
    " Function Call for the PCC check...
    CLEAR: l_flag.
    l_verid = lt_7jb-vers+1(2) .
    PERFORM check_pcc_function   USING  lt_7jb-matnr   l_verid  l_flag .
    CHECK l_flag = space OR l_flag = 'X' .
    CLEAR: l_text .
    CONCATENATE lt_7jb-matnr l_verid                 INTO l_text .
    PERFORM create_pcc USING lt_7jb-matnr   'P001'
                             l_text   l_verid  l_flag .
  ENDLOOP.
ENDFORM.                    " CHECK_PCC_ROUTINE

*&---------------------------------------------------------------------*
*&      Form  CHECK_PCC_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_7JB_MATNR  text
*----------------------------------------------------------------------*
FORM check_pcc_function USING    pa_matnr  pa_verid  pa_flag .
  DATA: lp_procnr        LIKE aufk-procnr,
        lp_verid         LIKE afpo-verid ,
        lp_stlan         LIKE mkal-stlan ,
        lp_stlal         LIKE mkal-stlal ,
        lp_plnty         LIKE plko-plnty ,
        lp_plnnr         LIKE plko-plnnr ,
        lp_plnal         LIKE plko-plnal ,
        lp_aufnr         LIKE aufk-aufnr ,
        lw_keko          LIKE keko       ,
        lt_afko          LIKE TABLE OF afko            WITH HEADER LINE,
        lt_vkks0         LIKE TABLE OF vkks0           WITH HEADER LINE,
        lt_pkosa         LIKE TABLE OF pkosa           WITH HEADER LINE.

  pa_flag = 'X'.
  CALL FUNCTION 'KK_F_PKOSA_FIND'
       EXPORTING
            i_matnr               = pa_matnr
            i_werks               = 'P001'
            i_pwerk               = 'P001'
            i_verid               = pa_verid
       IMPORTING
            e_procnr              = lp_procnr
            e_verid               = lp_verid
            e_stlan               = lp_stlan
            e_stlal               = lp_stlal
            e_plnty               = lp_plnty
            e_plnnr               = lp_plnnr
            e_plnal               = lp_plnal
            e_aufnr               = lp_aufnr
       TABLES
            e_vkks0               = lt_vkks0
            e_pkosa               = lt_pkosa
       EXCEPTIONS
            none_found            = 1
            wrong_input           = 2
            none_picked           = 3
            wrong_rule            = 4
            rsh_not_valid         = 5
            wrong_characteristics = 6
            no_rule               = 7
            version_not_valid     = 8
            OTHERS                = 9.

  CASE sy-subrc  .
    WHEN 0.
      pa_flag = 'S' .     " Can not call the PCC Function..
    WHEN 1.
      " Check the Standard Costing Estimate REsult...
      " If the Data is make without error... Continue...
      " Else Error...
      SELECT SINGLE * INTO lw_keko
        FROM keko
       WHERE matnr = pa_matnr
         AND tvers = '01'
         AND werks = 'P001'
         AND kokrs = 'H201'
         AND feh_sta = 'FR'
         AND klvar = 'PPC1'
         AND kadat <= sy-datum
         AND bidat >= sy-datum .

      IF sy-subrc NE 0.
        " Display the Message...
        WRITE: / pa_matnr, '(', pa_verid, ')',  text-016.
        pa_flag = 'S'.  EXIT.
      ENDIF.
    WHEN 8.
      pa_flag = 'E' .     " Error - Un-Respected Scenario.
    WHEN OTHERS.
      pa_flag = 'E' .     " Error - Un-Respected Scenario.
  ENDCASE.
ENDFORM.                    " CHECK_PCC_FUNCTION
