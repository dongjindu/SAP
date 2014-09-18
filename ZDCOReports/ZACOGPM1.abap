REPORT zacogpm1 .

TABLES: ztco_mha, ztco_gp1.

PARAMETERS: p_kokrs LIKE csks-kokrs MEMORY ID cac OBLIGATORY,
            p_gjahr LIKE anlp-gjahr MEMORY   ID gjr OBLIGATORY.
PARAMETERS: p_ncoal LIKE grpdynp-name_coall   OBLIGATORY
                                              DEFAULT 'HMMA1'.

PARAMETERS: p_perid LIKE ztco_mha-perid,
            p_bus0(2) TYPE c DEFAULT '05',
            p_loct(1) TYPE c default '1'.
PARAMETERS: p_save AS CHECKBOX.

SELECT-OPTIONS: s_kostl FOR ztco_mha-kostl,
                s_PERNR for ztco_mha-PERNR.

DATA: i_mha LIKE ztco_mha OCCURS 0 WITH HEADER LINE.
DATA: i_mh1 LIKE ztco_gp1 OCCURS 0 WITH HEADER LINE.

DATA: i_cc LIKE STANDARD TABLE OF bapi0012_cclist
                WITH HEADER LINE.
DATA: g_yymm(6) TYPE n.
*--- ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

DATA: g_repid     LIKE sy-repid,
      gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv.
*---- ALV

CONCATENATE  p_gjahr p_perid+1(2) INTO g_yymm.
PERFORM read_cctr.

PERFORM read_mh.

*ERFORM collect_hc.

PERFORM change_to_gpms.

PERFORM display_out.

IF p_save = 'X'.
  DELETE FROM ztco_gp1 WHERE yymm = g_yymm. COMMIT WORK.
  MODIFY ztco_gp1 FROM TABLE i_mh1.
  WRITE:/ 'Updated...'.
ENDIF.
*&---------------------------------------------------------------------*
*&      Form  READ_CCTR
*&---------------------------------------------------------------------*
FORM read_cctr.
  DATA : it_return         LIKE STANDARD TABLE OF bapiret2
                           WITH HEADER LINE.

* Making an internal table for CCtr to select data
  DATA : lv_datum LIKE sy-datum.
  CONCATENATE p_gjahr p_perid '01' INTO lv_datum.

  CLEAR : i_cc,      i_cc[],
          it_return, it_return[].

  CALL FUNCTION 'BAPI_COSTCENTER_GETLIST1'
       EXPORTING
            controllingarea = p_kokrs
            date_from       = lv_datum
            costcentergroup = p_ncoal
       TABLES
            costcenterlist  = i_cc
            return          = it_return.

ENDFORM.                    " READ_CCTR
*&---------------------------------------------------------------------*
*&      Form  display_out
*&---------------------------------------------------------------------*
FORM display_out.
  PERFORM field_setting TABLES gt_fieldcat USING :
   'CNT0'    'CNT0'            '10' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'PRGB'    'PRGB'            '01' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'GTCC'    'GTCC'            '02' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'GTWI'    'GTWI'            '01' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'PC00'    'PC00'            ' 8' 'X' 'R'  ' '  ' '  ' '  ' '  'X',
   'PC01'    'PC01'            ' 8' 'X' 'R'  ' '  ' '  ' '  ' '  'X',
   'DPC0'    'DPC0'            ' 8' 'X' 'R'  ' '  ' '  ' '  ' '  'X',
   'DPC1'    'DPC1'            ' 8' 'X' 'R'  ' '  ' '  ' '  ' '  'X',
   'PC02'    'PC02'            ' 8' 'X' 'R'  ' '  ' '  ' '  ' '  'X',
   'PC03'    'PC03'            ' 8' 'X' 'R'  ' '  ' '  ' '  ' '  'X',
   'DPC2'    'DPC2'            ' 8' 'X' 'R'  ' '  ' '  ' '  ' '  'X',
   'DPC3'    'DPC3'            ' 8' 'X' 'R'  ' '  ' '  ' '  ' '  'X',
   'PC04'    'PC04'            ' 8' 'X' 'R'  ' '  ' '  ' '  ' '  'X',
   'PC05'    'PC05'            ' 8' 'X' 'R'  ' '  ' '  ' '  ' '  'X'.



  g_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = g_repid
            it_fieldcat        = gt_fieldcat
            i_save             = 'A'
       TABLES
            t_outtab           = i_mh1
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.

ENDFORM.                    " display_out
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES p_fieldcat_t LIKE gt_fieldcat USING
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO gt_fieldcat.

ENDFORM.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  read_mh
*&---------------------------------------------------------------------*
FORM read_mh.

  SELECT * INTO TABLE i_mha FROM ztco_mha
    WHERE gjahr = p_gjahr
      AND perid = p_perid
      AND kostl IN s_kostl
      and PERNR in s_PERNR.

ENDFORM.                    " read_mh
*&---------------------------------------------------------------------*
*&      Form  collect_hc
*&---------------------------------------------------------------------*
FORM collect_hc.

*AVG HC
*  IF i_mha-kostl+5(1) = '6'.  "Sales
*    IF i_mha-jobcd(1) = 'P'.
*      i_mh1-dpc1 = i_mha-emp_cnt.
*      i_mh1-dpc3 = i_mha-emp_avg.
*    ELSE.
*      i_mh1-dpc0 = i_mha-emp_cnt.
*      i_mh1-dpc2 = i_mha-emp_avg.
*    ENDIF.
*  ELSE.
    IF i_mha-jobcd(1) = 'P'.
      i_mh1-pc01 = i_mha-emp_cnt.
      i_mh1-pc03 = i_mha-emp_avg.
    ELSE.
      i_mh1-pc00 = i_mha-emp_cnt.
      i_mh1-pc02 = i_mha-emp_avg.
    ENDIF.
*  ENDIF.

* headcount (ending)
  IF i_mha-jobcd(1) = 'P'.
    i_mh1-pc05 = i_mha-emp_cnt.
  ELSE.
    i_mh1-pc04 = i_mha-emp_cnt.
  ENDIF.

  COLLECT i_mh1.
  CLEAR: i_mh1-pc00, i_mh1-pc01, i_mh1-pc02, i_mh1-pc03.
  CLEAR: i_mh1-dpc0, i_mh1-dpc1, i_mh1-dpc2, i_mh1-dpc3.
  CLEAR: i_mh1-pc04, i_mh1-pc05.


ENDFORM.                    " collect_hc
*&---------------------------------------------------------------------*
*&      Form  change_to_gpms
*&---------------------------------------------------------------------*
FORM change_to_gpms.
*regular working
  LOOP AT i_mha.
    CLEAR i_mh1.

    i_mh1-bus0 = p_bus0.
    i_mh1-loct = p_loct.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
         EXPORTING
              input  = i_mha-kostl
         IMPORTING
              output = i_mh1-cnt0.

* W/C -> SHOP
    if i_mh1-cnt0(1) = 'M'.
      i_mh1-cnt0 = i_mh1-cnt0(4).
    endif.

    i_mh1-yymm = g_yymm.

    CASE i_mha-empct.
      WHEN 'A'.  i_mh1-prgb = '1'.
      WHEN 'B'.  i_mh1-prgb = '2'.
      WHEN 'C'.  i_mh1-prgb = '9'.
      WHEN 'K'.  i_mh1-prgb = '1'.  "4
    ENDCASE.

*1.TOTAL
    i_mh1-gtcc = '#1'.
    i_mh1-gtwi = i_mha-dtype.
    PERFORM collect_hc.


*NOT REQUIRED...PENDING...ANDY
**2.collect by working type
*    i_mh1-gtwi = i_mha-dtype.
**..weekday
*    CLEAR i_mh1-gtcc.
*    IF i_mha-dtype = '1' AND i_mha-lgart = '1'.
*      i_mh1-gtcc = 'A1'.
*      PERFORM collect_hc.
*    ELSEIF i_mha-lgart = '2'.
*      i_mh1-gtcc = 'D1'.
*      PERFORM collect_hc.
*    ENDIF.


  ENDLOOP.

ENDFORM.                    " change_to_gpms
