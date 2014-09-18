************************************************************************
* Program Name      : ZCPP001R_CU60
* Author            : BOBBY (T.011-9531-4002)
* Creation Date     : 2003/08/16.
* Specifications By : Bobby
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Master Upload(BDC) --> Table Contents Maintain
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZCPP001R_CU60 MESSAGE-ID zmpp
                     NO STANDARD PAGE HEADING LINE-SIZE 255.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS:
      p_hd_lin                TYPE  i   DEFAULT 2   NO-DISPLAY,
                                             " HEAD LINE FOR EXCEL FILE.
      p_tcode                 LIKE  tstc-tcode      NO-DISPLAY,
      p_cmode                 TYPE  c               NO-DISPLAY,
      p_pmode                 TYPE  c   DEFAULT 'N'           .
SELECTION-SCREEN END OF BLOCK b1.

DATA: BEGIN OF it_rec OCCURS 0,
        table(030),                       " TABLE NAME
*       CHRTS(030),                       " CHARACTERISTIC
        field000(30) ,                     " RESULT FIELD
        field001(30) ,                     " KEY FIELD          .
        field002(30) ,                     " KEY FIELD          .
        field003(30) ,                     " KEY FIELD          .
        field004(30) ,                     " KEY FIELD          .
        field005(30) ,                     " KEY FIELD          .
        field006(30) ,                     " KEY FIELD          .
        field007(30) ,                     " KEY FIELD          .
        field008(30) ,                     " KEY FIELD          .
        field009(30) ,                     " KEY FIELD          .
        field010(30) ,                     " KEY FIELD          .
        field011(30) ,                     " KEY FIELD          .
        field012(30) ,                     " KEY FIELD          .
        field013(30) ,                     " KEY FIELD          .
        field014(30) ,                     " KEY FIELD          .
        field015(30) ,                     " KEY FIELD          .
        field016(30) ,                     " KEY FIELD          .
        field017(30) ,                     " KEY FIELD          .
        field018(30) ,                     " KEY FIELD          .
        field019(30) ,                     " KEY FIELD          .
        field020(30) ,                     " KEY FIELD          .
        field021(30) ,                     " KEY FIELD          .
      END OF it_rec.

DATA: wa_filename             LIKE  rlgrap-filename,
      wa_filetype             LIKE  rlgrap-filetype VALUE 'DAT',
      wa_bdcgroup             LIKE  sy-uname.          " APQI-GROUPID

DATA: it_vtable               LIKE TABLE OF vtentries  WITH HEADER LINE,
      it_msg                  LIKE TABLE OF bdcmsgcoll WITH HEADER LINE,
      it_bdcdata              LIKE TABLE OF bdcdata    WITH HEADER LINE.

DATA: wa_date                 type d,
      wa_tmp_count(3)         TYPE n,
      wa_tmp_flg              TYPE c,
      wa_tmp_text(225)        TYPE c,
      wa_table                TYPE tablstruct-var_tab,
      wa_vtint                LIKE cuvtab-vtint,
      wa_tableid              LIKE vtentries-vtlineno,
      wa_charact(40)          TYPE c,
      wa_fieldname(40)        TYPE c,
      wa_tmp_date(10)         TYPE c,
      wa_exist                TYPE c,
      wa_tmp_total(06)        TYPE n,
      wa_tmp_process(06)      TYPE n.

FIELD-SYMBOLS: <field_vals> .

INITIALIZATION.
  wa_bdcgroup = sy-uname.
  p_tcode  = 'CU60' .
  IF sy-uname = 'BOBBY'.
    p_pmode = 'A'.
  ENDIF.

START-OF-SELECTION.

  PERFORM bdc_upload_data.

  DELETE it_rec INDEX 1 .
  DELETE it_rec INDEX 1 .

  LOOP AT it_rec .
    wa_table = it_rec-table  .
    IF it_rec-field001  NE space .
      CALL FUNCTION 'CONVERSION_EXIT_DATEX_INPUT'
           EXPORTING
                input  = it_rec-field001(10)
           IMPORTING
                output = wa_date .
      it_rec-field001  = wa_date .
    ENDIF.
    PERFORM check_vtint  USING wa_vtint  it_rec-table wa_tableid .
    PERFORM append_vtable    .
    PERFORM call_function    .
    CLEAR: it_vtable[].
  ENDLOOP.

* PERFORM call_function    .

END-OF-SELECTION.

  INCLUDE zcpp103_common_routine .


*&---------------------------------------------------------------------*
*&      Form  APPEND_VTABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_vtable.
  DATA: l_count(02)          TYPE n,
        l_pos(0003)          TYPE n,
        l_length(3)          TYPE n,
        l_fieldpos(3)        TYPE n,
        l_date_conv          TYPE d,
        l_date(10)           TYPE c.

  CLEAR: it_vtable, l_count.
  PERFORM get_fieldname USING wa_vtint  '001'  wa_charact  l_length .
  it_vtable-vtlineno   = wa_tableid   .      " LENGTH = 5.
  it_vtable-vtcharact  = wa_charact.
  it_vtable-vtchardscr = ' ' .
  it_vtable-vtlinnoint = ' ' .
  it_vtable-vtvalue    = it_rec-field000(l_length) .
  it_vtable-vtvaldescr = ' ' .
  APPEND it_vtable           .

  l_pos = 2 .
  l_fieldpos = 1 .
  IF it_rec-field001 IS INITIAL.
    CLEAR: l_date      .
  ELSE.
    l_date_conv = it_rec-field001(8) .
    WRITE l_date_conv TO l_date      .
  ENDIF.

  it_vtable-vtlineno   = wa_tableid   .
  it_vtable-vtcharact  = 'P_ALC_DATE' .
  it_vtable-vtchardscr = ' ' .
  it_vtable-vtlinnoint = ' ' .
  it_vtable-vtvalue    = l_date .
  it_vtable-vtvaldescr = ' ' .
  APPEND it_vtable           .

  PERFORM get_countsize  USING wa_vtint  l_count.

  DO l_count TIMES.
    l_pos = l_pos + 1 .
    l_fieldpos = l_fieldpos + 1.

    PERFORM get_fieldname USING wa_vtint  l_pos  wa_charact  l_length .
    CONCATENATE 'IT_REC-FIELD' l_fieldpos INTO wa_fieldname.
    ASSIGN (wa_fieldname)                 TO   <field_vals>.

    IF <field_vals> IS INITIAL.
      <field_vals> = '-'     .
    ENDIF.

    it_vtable-vtlineno   = wa_tableid.
    it_vtable-vtcharact  = wa_charact.
    it_vtable-vtchardscr = ' ' .
    it_vtable-vtlinnoint = ' ' .
    it_vtable-vtvalue    = <field_vals>(l_length).
    it_vtable-vtvaldescr = ' ' .
    APPEND it_vtable           .
  ENDDO.
ENDFORM.                    " APPEND_VTABLE

*&---------------------------------------------------------------------*
*&      Form  CHECK_VTINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SV_VTINT  text
*      -->P_IT_REC_TABLE  text
*----------------------------------------------------------------------*
FORM check_vtint USING    p_vtint   p_table  p_tableid.
  SELECT SINGLE vtint INTO p_vtint
    FROM cuvtab
   WHERE vtnam = p_table .

  p_tableid = p_vtint+5(5).
ENDFORM.                    " CHECK_VTINT

*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_function.
  CALL FUNCTION 'CAMA_TABLE_MAINTAIN_ENTRIES'
       EXPORTING
            var_table       = wa_table
       TABLES
            var_tab_entries = it_vtable
       EXCEPTIONS
            error           = 1
            OTHERS          = 2.

  IF sy-subrc = 0.
    WRITE AT: /001(40)  wa_table ,
               041(04)  '===>'   ,
               045(10)  ' OK    '.
  ELSE.
    WRITE AT: /001(40)  wa_table ,
               041(04)  '===>'   ,
               045(42)  ' FAIL!!*****************************'.
  ENDIF.
ENDFORM.                    " CALL_FUNCTION

*&---------------------------------------------------------------------*
*&      Form  GET_FIELDNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_VTINT  text
*      -->P_0263   text
*----------------------------------------------------------------------*
FORM get_fieldname USING    p_vtint  p_pos  p_charact  p_length .
  DATA: l_anzst               LIKE cabn-anzst.

  SELECT SINGLE n~atnam n~anzst INTO  (p_charact, l_anzst)
    FROM cuvtab_fld AS f INNER JOIN cabn AS n
      ON f~atinn = n~atinn
   WHERE f~vtint = p_vtint
     AND f~vtpos = p_pos   .

  p_length = l_anzst .
ENDFORM.                    " GET_FIELDNAME

*&---------------------------------------------------------------------*
*&      Form  GET_COUNTSIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_VTINT  text
*      -->P_L_COUNT  text
*----------------------------------------------------------------------*
FORM get_countsize USING    p_vtint p_count.
  SELECT COUNT( * ) INTO p_count
    FROM cuvtab_fld
   WHERE vtint = p_vtint  .

  p_count = p_count - 2   .
ENDFORM.                    " GET_COUNTSIZE
