REPORT ZZUREP11 .

tables: glu1, t800a.

parameters: p_tab like t800a-tab default 'GLFUNCT' obligatory.
selection-screen skip 1.

SELECTION-SCREEN BEGIN OF BLOCK addsel WITH FRAME TITLE text-001.
select-options: p_rldnr for glu1-rldnr,
                p_rrcty for glu1-rrcty,
                p_rvers for glu1-rvers,
                p_ryear for glu1-ryear,
                p_rcomp for glu1-rcomp,
                p_rbukrs for glu1-rbukrs,
                p_racct for glu1-racct,
                p_rbusa for glu1-rbusa,
                p_rfarea for glu1-rfarea,
                p_rcntr for glu1-rcntr,
                p_rprctr for glu1-rprctr.
SELECTION-SCREEN END OF block addsel.

data: t_sumfields like dfies occurs 0 with header line,
      t_h_sumfields like dfies occurs 0 with header line,
      t_ob1fields like dfies occurs 0 with header line,
      t_ob2fields like dfies occurs 0 with header line,
      t_obj_fields_sum like dfies occurs 0 with header line,
      t_obj_fields_sum_exc like dfies occurs 0 with header line,
      t_obj_fields_obj like dfies occurs 0 with header line.
data: h_fieldname like dfies-fieldname.

field-symbols: <table>,
               <field>.

DATA: WORK(200).                       "allg. Arbeitsfeld
data: BEGIN OF CODE OCCURS 250,
        LINE(72),                      "Select-ABAP Code-Tabelle
      END OF CODE,
      OFFSET_CODE TYPE I.              "Linker Rand im Coding

AT SELECTION-SCREEN.
*Check, if totals table has been entered
  SELECT SINGLE * FROM T800A WHERE TAB = P_TAB.
  IF SY-SUBRC NE 0
  OR T800A-TTYPE NE 'TT'
  or t800a-objtable = space
  OR T800A-INACTIVE NE SPACE.
    MESSAGE E171(GU) WITH P_TAB.
  ENDIF.

END-OF-SELECTION.

*Get the fields of all tables
  perform get_fields.

*Generate the coding in ZZUREP12
  perform generate_data_declaration.

*Generate call of e01_check_objnrs
  perform generate_call_e01_check_objnrs.

*Generate e01_check_objnrs
  perform generate_e01_check_objnrs.

*Generate list output
  perform generate_list.

*Generate subroutine print_list.
  perform generate_print_list.


*Insert the generated report into ZZUREP12
  insert report 'ZZUREP12' from code.
  commit work.

*External call of check routine in generated RGUREP12
  perform e01_check_objnrs(ZZUREP12) tables p_rldnr
                                            p_rrcty
                                            p_rvers
                                            p_ryear
                                            p_rcomp
                                            p_rbukrs
                                            p_racct
                                            p_rbusa
                                            p_rfarea
                                            p_rcntr
                                            p_rprctr.

*---------------------------------------------------------------------*
*  FORM get_fields
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
form get_fields.
*Get key fields (with object numbers) of totals table
  perform get_table_fields tables t_sumfields
                            using t800a-tab
                                  'K'
                                  'X'.

*Get key fields (without object numbers) of totals table
  perform get_table_fields tables t_h_sumfields
                            using t800a-tab
                                  'K'
                                  ' '.

*Append logical key fields at the end of t_sumtab
  loop at t_h_sumfields.
    read table t_sumfields with key fieldname = t_h_sumfields-fieldname.
    if sy-subrc ne 0.
      append t_h_sumfields to t_sumfields.
    endif.
  endloop.
  delete t_sumfields where fieldname = 'RCLNT'.
*Get fields of object table 1
  if t800a-objtable ne space.
    perform get_table_fields tables t_ob1fields
                            using t800a-objtable
                                  'A'
                                  'X'.


    delete t_ob1fields where fieldname = 'MANDT'
                          or fieldname = 'DATIV'
                          or fieldname = 'DATIB'
                          or fieldname = 'DATPV'
                          or fieldname = 'DATPB'.
  endif.
*Get fields of object table 2
  if t800a-objtable2 ne space.
    perform get_table_fields tables t_ob2fields
                            using t800a-objtable2
                                  'A'
                                  'X'.


    delete t_ob2fields where fieldname = 'MANDT'
                          or fieldname = 'DATIV'
                          or fieldname = 'DATIB'
                          or fieldname = 'DATPV'
                          or fieldname = 'DATPB'.
  endif.
endform.                    "get_fields

form get_table_fields tables gtf_t_fields structure dfies
                      using  gtf_table like t800a-tab
                             gtf_FTYPE
                             gtf_nO_GLX_OBJ_PROCESSING.
data: l_subrc like sy-subrc.

CALL FUNCTION 'G_FIELD_SET'
       EXPORTING
            FTYPE                 = gtf_FTYPE
            NO_GLX_OBJ_PROCESSING = gtf_NO_GLX_OBJ_PROCESSING
            TABLE                 = gtf_TABLE
       EXCEPTIONS
            NOT_FOUND             = 1.
  IF SY-SUBRC EQ 1.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  REFRESH gtf_t_fields.
  DO.
    CALL FUNCTION 'G_FIELD_GET'
         IMPORTING
              FIELD_ATTR    = gtf_t_fields
              SUBRC         = L_SUBRC.
    IF L_SUBRC NE 0.
      EXIT.
    ELSE.
      APPEND gtf_t_fields.
    ENDIF.
  ENDDO.
  CLEAR gtf_t_fields.
endform.


*&---------------------------------------------------------------------*
*&      Form  generate_data_declaration
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_data_declaration .

  PERFORM APPEND_CODE USING  0 0 'report rgurep12.' SPACE.

  perform APPEND_CODE USING  0 0 space space.

  MOVE 'TABLES GLU1.' TO WORK.
  REPLACE '$' INTO WORK WITH ''.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  MOVE 'TABLES: $.' TO WORK.
  REPLACE '$' INTO WORK WITH t800a-tab.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  if t800a-objtable ne space.
    MOVE 'TABLES: $.' TO WORK.
    REPLACE '$' INTO WORK WITH t800a-objtable.
    PERFORM APPEND_CODE USING  0 0 WORK SPACE.
  endif.

  if t800a-objtable2 ne space.
    MOVE 'TABLES: $.' TO WORK.
    REPLACE '$' INTO WORK WITH t800a-objtable2.
    PERFORM APPEND_CODE USING  0 0 WORK SPACE.
  endif.

  perform APPEND_CODE USING  0 0 space space.

  perform APPEND_CODE USING  0 0:
    'select-options:' space,
    'p_rldnr for glu1-rldnr,' space,
    'p_rrcty for glu1-rrcty,' space,
    'p_rvers for glu1-rvers,' space,
    'p_ryear for glu1-ryear,' space,
    'p_rcomp for glu1-rcomp,' space,
    'p_rbukrs for glu1-rbukrs,' space,
    'p_racct for glu1-racct,' space,
    'p_rbusa for glu1-rbusa,' space,
    'p_rfarea for glu1-rfarea,' space,
    'p_rcntr for glu1-rcntr,' space,
    'p_rprctr for glu1-rprctr.' space.

  perform APPEND_CODE USING  0 0 space space.

  perform APPEND_CODE USING  0 8:
   'data: begin of t_sumtab occurs 0,' space.

  loop at t_sumfields.
    sy-fdpos = strlen( t800a-tab ).
    assign t800a-tab(sy-fdpos) to <table>.
    MOVE '$ like $-$,' TO WORK.
    REPLACE '$' INTO WORK WITH: t_sumfields-fieldname,
                                <table>,
                                t_sumfields-fieldname.
    PERFORM APPEND_CODE USING 0 0 WORK SPACE.
  endloop.

  perform APPEND_CODE USING -8 0:
   '      end of t_sumtab.' space.

  perform APPEND_CODE USING  0 0 space space.

  perform APPEND_CODE USING:
    0 8 'data: begin of t_errtab occurs 0.' space,
    0 0 'include structure t_sumtab.' space,
   -8 0 'data:   err_flag(3).' space,
    0 0 'data: end of t_errtab.' space.

  perform APPEND_CODE USING  0 0 space space.

  perform APPEND_CODE USING  0 8:
   'data: begin of t_ob1tab occurs 0,' space.

  loop at t_ob1fields.
    sy-fdpos = strlen( t800a-objtable ).
    assign t800a-objtable(sy-fdpos) to <table>.
    MOVE '$ like $-$,' TO WORK.
    REPLACE '$' INTO WORK WITH: t_ob1fields-fieldname,
                                <table>,
                                t_ob1fields-fieldname.
    PERFORM APPEND_CODE USING 0 0 WORK SPACE.
  endloop.

  perform APPEND_CODE USING -8 0:
   '      end of t_ob1tab.' space.

  perform APPEND_CODE USING  0 0 space space.

  if t800a-objtable2 ne space.
    perform APPEND_CODE USING  0 8:
     'data: begin of t_ob2tab occurs 0,' space.

    loop at t_ob2fields.
      sy-fdpos = strlen( t800a-objtable2 ).
      assign t800a-objtable2(sy-fdpos) to <table>.
      MOVE '$ like $-$,' TO WORK.
      REPLACE '$' INTO WORK WITH: t_ob2fields-fieldname,
                                  <table>,
                                  t_ob2fields-fieldname.
      PERFORM APPEND_CODE USING 0 0 WORK SPACE.
    endloop.

    perform APPEND_CODE USING -8 0:
     '      end of t_ob2tab.' space.
  endif.

  perform APPEND_CODE USING  0 0 space space.

ENDFORM.                    " generate_data_declaration
*&---------------------------------------------------------------------*
*&      Form  generate_call_e01_check_objnrs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_call_e01_check_objnrs .

  perform APPEND_CODE USING:
    0 2 'perform e01_check_objnrs tables' space,
    0 0 'p_rldnr' space,
    0 0 'p_rrcty' space,
    0 0 'p_rvers' space,
    0 0 'p_ryear' space,
    0 0 'p_rcomp' space,
    0 0 'p_rbukrs' space,
    0 0 'p_racct' space,
    0 0 'p_rbusa' space,
    0 0 'p_rfarea' space,
    0 0 'p_rcntr' space,
    0 0 'p_rprctr.' space,
   -2 0 space space.

ENDFORM.                    " generate_call_e01_check_objnrs
*&---------------------------------------------------------------------*
*&      Form  generate_e01_check_objnrs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_e01_check_objnrs .

  perform APPEND_CODE USING:
    0 2 'form e01_check_objnrs tables' space,
    0 0 't_p_rldnr structure p_rldnr' space,
    0 0 't_p_rrcty structure p_rrcty' space,
    0 0 't_p_rvers structure p_rvers' space,
    0 0 't_p_ryear structure p_ryear' space,
    0 0 't_p_rcomp structure p_rcomp' space,
    0 0 't_p_rbukrs structure p_rbukrs' space,
    0 0 't_p_racct structure p_racct' space,
    0 0 't_p_rbusa structure p_rbusa' space,
    0 0 't_p_rfarea structure p_rfarea' space,
    0 0 't_p_rcntr structure p_rcntr' space,
    0 0 't_p_rprctr structure p_rprctr.' space,
    0 0 space space.

*Generate select-statement
  work = 'select * from $ into corresponding fields of table t_sumtab'.
  REPLACE '$' INTO WORK WITH t800a-tab.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  PERFORM APPEND_CODE
    USING  0 0 'where rldnr in t_p_rldnr' SPACE.

  perform create_in_statement using 'RRCTY'.
  perform create_in_statement using 'RVERS'.
  perform create_in_statement using 'RYEAR'.
  perform create_in_statement using 'RCOMP'.
  perform create_in_statement using 'RBUKRS'.
  perform create_in_statement using 'RACCT'.
  perform create_in_statement using 'RBUSA'.
  perform create_in_statement using 'RFAREA'.
  perform create_in_statement using 'RCNTR'.
  perform create_in_statement using 'RPRCTR'.
  PERFORM APPEND_CODE USING  0 0 '.' SPACE.
  PERFORM APPEND_CODE USING  0 0 space SPACE.

*Generate loop on t_sumtab
  PERFORM APPEND_CODE USING
    0 2 'loop at t_sumtab.' SPACE.

  if not t800a-objtable is initial.
*Generate check 1) Receiver <-> object table 1
    clear t_obj_fields_sum. refresh t_obj_fields_sum.
    clear t_obj_fields_sum_exc. refresh t_obj_fields_sum_exc.
    clear t_obj_fields_obj. refresh t_obj_fields_obj.
    loop at t_ob1fields where fieldname ne 'OBJNR'.
      h_fieldname = 'R'.
      h_fieldname+1 = t_ob1fields-fieldname.
      CALL FUNCTION 'G_FIELD_READ'
        EXPORTING
          TABLE     = t800a-tab
          FIELDNAME = h_fieldname
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.
      if sy-subrc = 0.
        t_obj_fields_sum = t_ob1fields.
        t_obj_fields_sum-fieldname = h_fieldname.
        append t_obj_fields_sum.
        t_obj_fields_obj = t_ob1fields.
        append t_obj_fields_obj.
      endif.
    endloop.
    perform generate_check_objnr using 'ROBJNR'
                                       t800a-objtable
                                       't_ob1tab'
                                       '1'.

*Generate check 2) Sender <-> object table 1
    clear t_obj_fields_sum. refresh t_obj_fields_sum.
    clear t_obj_fields_sum_exc. refresh t_obj_fields_sum_exc.
    clear t_obj_fields_obj. refresh t_obj_fields_obj.
    loop at t_ob1fields where fieldname ne 'OBJNR'.
      h_fieldname = 'S'.
      h_fieldname+1 = t_ob1fields-fieldname.
      CALL FUNCTION 'G_FIELD_READ'
        EXPORTING
          TABLE     = t800a-tab
          FIELDNAME = h_fieldname
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.
      if sy-subrc = 0.
        t_obj_fields_sum = t_ob1fields.
        t_obj_fields_sum-fieldname = h_fieldname.
        append t_obj_fields_sum.
        t_obj_fields_obj = t_ob1fields.
        append t_obj_fields_obj.
      else.
        t_obj_fields_sum_exc = t_ob1fields.
        append t_obj_fields_sum_exc.
      endif.
    endloop.
    if not t_obj_fields_sum[] is initial.
      perform generate_check_objnr using 'SOBJNR'
                                         t800a-objtable
                                         't_ob1tab'
                                         '2'.
    endif.
  endif.

  if not t800a-objtable2 is initial.
*Generate check 3) C-Fields <-> object table 2
    clear t_obj_fields_sum. refresh t_obj_fields_sum.
    clear t_obj_fields_sum_exc. refresh t_obj_fields_sum_exc.
    clear t_obj_fields_obj. refresh t_obj_fields_obj.
    loop at t_ob2fields where fieldname ne 'OBJNR'.
      h_fieldname = t_ob2fields-fieldname.
      CALL FUNCTION 'G_FIELD_READ'
        EXPORTING
          TABLE     = t800a-tab
          FIELDNAME = h_fieldname
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.
      if sy-subrc = 0.
        t_obj_fields_sum = t_ob2fields.
        t_obj_fields_sum-fieldname = h_fieldname.
        append t_obj_fields_sum.
        t_obj_fields_obj = t_ob2fields.
        append t_obj_fields_obj.
      endif.
    endloop.
    perform generate_check_objnr using 'COBJNR'
                                       t800a-objtable2
                                       't_ob2tab'
                                       '3'.
  endif.

  PERFORM APPEND_CODE USING -2 0 'ENDLOOP.' SPACE.

  PERFORM APPEND_CODE USING  0 0 space SPACE.

ENDFORM.                    " generate_e01_check_objnrs
*&---------------------------------------------------------------------*
*&      Form  create_in_statement
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1053   text
*----------------------------------------------------------------------*
FORM create_in_statement  USING  cis_field like dfies-fieldname.
  CALL FUNCTION 'G_FIELD_READ'
    EXPORTING
      TABLE     = t800a-tab
      FIELDNAME = cis_field
    EXCEPTIONS
      NOT_FOUND = 1
      OTHERS    = 2.
  if sy-subrc = 0.
    work = ' and $ in t_p_$'.
    rePLACE '$' INTO WORK WITH: cis_field,
                                cis_field.
    PERFORM APPEND_CODE USING  0 0 WORK SPACE.
  endif.

ENDFORM.                    " create_in_statement

*---------------------------------------------------------------------*
*  FORM generate_check_objnr
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
*  -->  GCO_OBJNR
*  -->  GCO_OBJTABLE
*  -->  GCO_OBXTAB
*  -->  GCO_NUMBER
*---------------------------------------------------------------------*
form generate_check_objnr using gco_objnr
                                gco_objtable
                                gco_obxtab
                                gco_number.

*A-Check
  case gco_number.
    when '1'.
      PERFORM APPEND_CODE USING:
      0 0 space '1) Receiver <-> object table 1',
      0 0 space 'a) Receiver OBJECT NUMBER with Object table 1'.
    when '2'.
      PERFORM APPEND_CODE USING:
      0 0 space '2) Sender <-> object table 1',
      0 0 space 'a) Sender OBJECT NUMBER with Object table 1'.
    when '3'.
      PERFORM APPEND_CODE USING:
      0 0 space '3) Constant fields <-> object table 2',
      0 0 space 'a) C OBJECT NUMBER with Object table 2'.
  endcase.

  work = 'clear $. refresh $.'.
  REPLACE '$' INTO WORK WITH: gco_obxtab, gco_obxtab.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  work = 'if t_sumtab-$ ne ''000000000000000000''.'.
  REPLACE '$' INTO WORK WITH: gco_objnr.
  PERFORM APPEND_CODE USING  0 2 WORK SPACE.

  work =
  'select * from $ into corresponding fields of table $'.
  REPLACE '$' INTO WORK WITH: gco_objtable,
                              gco_obxtab.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  work = 'where objnr = t_sumtab-$.'.
  REPLACE '$' INTO WORK WITH: gco_objnr.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  work = 'describe table $ lines sy-tfill.'.
  REPLACE '$' INTO WORK WITH: gco_obxtab.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  perform create_fill_t_errtab_tfill using gco_number
                                           'A1'
                                           'A2'.

  work = 'read table $ index 1.'.
  REPLACE '$' INTO WORK WITH: gco_obxtab.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  work = 'IF'.
  loop at t_obj_fields_sum.
    read table t_obj_fields_obj index sy-tabix.

    work+3 = 't_sumtab-$ ne $-$'.
    REPLACE '$' INTO WORK WITH: t_obj_fields_sum-fieldname,
                                gco_obxtab,
                                t_obj_fields_obj-fieldname.
    PERFORM APPEND_CODE USING  0 0 WORK SPACE.

    work = 'OR'.
  endloop.
  PERFORM APPEND_CODE USING  0 2 '.' SPACE.

  perform create_fill_t_errtab_simple using gco_number
                                            'A3'.

  PERFORM APPEND_CODE USING -2 0 'ENDIF.' SPACE.
  PERFORM APPEND_CODE USING -2 0 'ENDIF.' SPACE.
  PERFORM APPEND_CODE USING -2 2 'ELSE.' SPACE.

  work = 'IF NOT'.
  loop at t_obj_fields_sum.

    work+8 = 't_sumtab-$ is initial'.
    REPLACE '$' INTO WORK WITH: t_obj_fields_sum-fieldname.
    PERFORM APPEND_CODE USING  0 0 WORK SPACE.

    work = 'OR NOT'.
  endloop.
  PERFORM APPEND_CODE USING  0 2 '.' SPACE.

  perform create_fill_t_errtab_simple using gco_number
                                            'A4'.

  PERFORM APPEND_CODE USING -2 0 'ENDIF.' SPACE.
  PERFORM APPEND_CODE USING -2 0 'ENDIF.' SPACE.

*B-Check
  case gco_number.
    when '1'.
      PERFORM APPEND_CODE USING:
      0 0 space 'b) Receiver FIELDS with Object table 1'.
    when '2'.
      PERFORM APPEND_CODE USING:
      0 0 space 'b) Sender FIELDS with Object table 1'.
    when '3'.
      PERFORM APPEND_CODE USING:
      0 0 space 'b) C-FIELDS with Object table 2'.
  endcase.

  work = 'clear $. refresh $.'.
  REPLACE '$' INTO WORK WITH: gco_obxtab, gco_obxtab.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  work = 'IF NOT'.
  loop at t_obj_fields_sum.

    work+8 = 't_sumtab-$ is initial'.
    REPLACE '$' INTO WORK WITH: t_obj_fields_sum-fieldname.
    PERFORM APPEND_CODE USING  0 0 WORK SPACE.

    work = 'OR NOT'.
  endloop.
  PERFORM APPEND_CODE USING  0 2 '.' SPACE.

  work =
  'select * from $ into corresponding fields of table $'.
  REPLACE '$' INTO WORK WITH: gco_objtable,
                              gco_obxtab.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  work = 'where'.
  loop at t_obj_fields_sum.
    read table t_obj_fields_obj index sy-tabix.

    work+6 = '$ = t_sumtab-$'.
    REPLACE '$' INTO WORK WITH: t_obj_fields_obj-fieldname,
                                t_obj_fields_sum-fieldname.
    PERFORM APPEND_CODE USING  0 0 WORK SPACE.

    work = 'AND'.
  endloop.
  loop at t_obj_fields_sum_exc.
    work+6 = '$ = space'.
    REPLACE '$' INTO WORK WITH: t_obj_fields_sum_exc-fieldname.
    PERFORM APPEND_CODE USING  0 0 WORK SPACE.

    work = 'AND'.
  endloop.

  PERFORM APPEND_CODE USING  0 0 '.' SPACE.

  work = 'describe table $ lines sy-tfill.'.
  REPLACE '$' INTO WORK WITH: gco_obxtab.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  perform create_fill_t_errtab_tfill using gco_number
                                           'B1'
                                           'B2'.

  work = 'read table $ index 1.'.
  REPLACE '$' INTO WORK WITH: gco_obxtab.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.


  work = 'if t_sumtab-$ ne $-objnr.'.
  REPLACE '$' INTO WORK WITH: gco_objnr,
                              gco_obxtab.
  PERFORM APPEND_CODE USING  0 2 WORK SPACE.

  perform create_fill_t_errtab_simple using gco_number
                                            'B3'.

  PERFORM APPEND_CODE USING -2 0 'ENDIF.' SPACE.

  PERFORM APPEND_CODE USING -2 0 'ENDIF.' SPACE.

  PERFORM APPEND_CODE USING -2 2 'ELSE.' SPACE.


  work = 'if t_sumtab-$ ne ''000000000000000000''.'.
  REPLACE '$' INTO WORK WITH: gco_objnr.
  PERFORM APPEND_CODE USING  0 2 WORK SPACE.

  perform create_fill_t_errtab_simple using gco_number
                                            'B4'.

  PERFORM APPEND_CODE USING -2 0 'ENDIF.' SPACE.

  PERFORM APPEND_CODE USING -2 0 'ENDIF.' SPACE.

endform.                    "generate_check_objnr

*---------------------------------------------------------------------*
*  FORM create_fill_t_errtab_tfill
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
*  -->  CFT_NUMBER
*  -->  CFT_VAR1
*  -->  CFT_VAR2
*---------------------------------------------------------------------*
form create_fill_t_errtab_tfill using cft_number
                                      cft_var1
                                      cft_var2.

  PERFORM APPEND_CODE USING:
    0 2 'if sy-tfill ne 1.' SPACE,
    0 0 't_errtab = t_sumtab.' SPACE,
    0 2 'if sy-tfill = 0.' SPACE.

  work = 't_errtab-err_flag = ''$$''.'.
  REPLACE '$' INTO WORK WITH: cft_number,
                              cft_var1.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  PERFORM APPEND_CODE USING:
    -2 2 'else.' SPACE.

  work = 't_errtab-err_flag = ''$$''.'.
  REPLACE '$' INTO WORK WITH: cft_number,
                              cft_var2.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  PERFORM APPEND_CODE USING:
    -2 0 'endif.' SPACE,
     0 0 'append t_errtab.' space,
    -2 2 'else.' SPACE.
endform.                    "create_fill_t_errtab_tfill

*---------------------------------------------------------------------*
*  FORM create_fill_t_errtab_simple
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
*  -->  CFT_NUMBER
*  -->  CFT_VAR1
*  -->  CFT_VAR2
*---------------------------------------------------------------------*
form create_fill_t_errtab_simple using cft_number
                                      cft_var.

  PERFORM APPEND_CODE USING:
    0 0 't_errtab = t_sumtab.' SPACE.

  work = 't_errtab-err_flag = ''$$''.'.
  REPLACE '$' INTO WORK WITH: cft_number,
                              cft_var.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  PERFORM APPEND_CODE USING:
    0 0 'append t_errtab.' space.
endform.                    "create_fill_t_errtab_simple

*&---------------------------------------------------------------------*
*&      Form  generate_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_list .

  PERFORM APPEND_CODE USING  0 0 SPACE 'Print result list'.
  PERFORM APPEND_CODE USING:
    0 0 'sort t_errtab by err_flag.' SPACE,
    0 0 'read table t_errtab index 1.' space,
    0 2 'if sy-subrc = 0.' space.

  work = 'message I016(GU) with ''Inconsistencies found''.'.
  PERFORM APPEND_CODE USING 0 0 work space.

  PERFORM APPEND_CODE USING:
    0 0 'perform print_list using ''1A1'' text-1A1.' space,
    0 0 'perform print_list using ''1A2'' text-1A2.' space,
    0 0 'perform print_list using ''1A3'' text-1A3.' space,
    0 0 'perform print_list using ''1A4'' text-1A4.' space,
    0 0 'perform print_list using ''1B1'' text-1B1.' space,
    0 0 'perform print_list using ''1B2'' text-1B2.' space,
    0 0 'perform print_list using ''1B3'' text-1B3.' space,
    0 0 'perform print_list using ''1B4'' text-1B4.' space,
    0 0 'perform print_list using ''2A1'' text-2A1.' space,
    0 0 'perform print_list using ''2A2'' text-2A2.' space,
    0 0 'perform print_list using ''2A3'' text-2A3.' space,
    0 0 'perform print_list using ''2A4'' text-2A4.' space,
    0 0 'perform print_list using ''2B1'' text-2B1.' space,
    0 0 'perform print_list using ''2B2'' text-2B2.' space,
    0 0 'perform print_list using ''2B3'' text-2B3.' space,
    0 0 'perform print_list using ''2B4'' text-2B4.' space,
    0 0 'perform print_list using ''3A1'' text-3A1.' space,
    0 0 'perform print_list using ''3A2'' text-3A2.' space,
    0 0 'perform print_list using ''3A3'' text-3A3.' space,
    0 0 'perform print_list using ''3A4'' text-3A4.' space,
    0 0 'perform print_list using ''3B1'' text-3B1.' space,
    0 0 'perform print_list using ''3B2'' text-3B2.' space,
    0 0 'perform print_list using ''3B3'' text-3B3.' space,
    0 0 'perform print_list using ''3B4'' text-3B4.' space,
   -2 2 'else.' space.

  work = 'message I016(GU) with ''No inconsistencies found''.'.
  PERFORM APPEND_CODE USING 0 0 work space.

  PERFORM APPEND_CODE USING:
   -2 0 'endif.' space,
   -2 0 'endform.' space.

  PERFORM APPEND_CODE USING  0 0 space SPACE.

ENDFORM.                    " generate_list

*&---------------------------------------------------------------------*
*&      Form  generate_print_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_print_list .
  PERFORM APPEND_code USING:
    0 2 'FORM print_list USING pl_err_flag pl_text.' SPACE,
    0 2 'loop at t_errtab where err_flag = pl_err_flag.' space,
    0 0 'exit.' space,
   -2 0 'endloop.' space,
    0 2 'if sy-subrc = 0.' space,
    0 0 'write / pl_text color col_negative.' space,
   -2 0 'endif.' space,
    0 2 'loop at t_errtab where err_flag = pl_err_flag.' space,
    0 0 'write: /' space.

  loop at t_sumfields.
    work = 't_errtab-$, ''!'','.
    REPLACE '$' INTO WORK WITH: t_sumfields-fieldname.
    PERFORM APPEND_CODE USING  0 0 work SPACE.
  endloop.
  PERFORM APPEND_CODE USING  0 0 ''' ''.' SPACE.
  PERFORM APPEND_CODE USING -2 0 'endloop.' SPACE.
  PERFORM APPEND_CODE USING -2 0 'endform.' SPACE.
ENDFORM.                    " generate_print_list

*---------------------------------------------------------------------*
*       FORM APPEND_CODE                                              *
*---------------------------------------------------------------------*
*       ABAP/4-Coding-Zeilen in Tabelle CODE anhängen                 *
*---------------------------------------------------------------------*
*  -->  APP_OFF_BEF  Differenzabstand zum linken Rand vor APPEND      *
*  -->  APP_OFF_AFT  Differenzabstand zum linken Rand nach APPEND     *
*  -->  APP_CODE     Code-Zeile                                       *
*  -->  APP_COMMENT  Kommentar                                        *
*---------------------------------------------------------------------*
FORM APPEND_CODE USING APP_OFF_BEF APP_OFF_AFT APP_CODE APP_COMMENT.
  DATA: BEGIN OF SPLITTED_CODE OCCURS 1,
          LINE(72),
        END OF SPLITTED_CODE.
  DATA: UNSPLITTED_LINE(200).

  CLEAR UNSPLITTED_LINE.
  CLEAR SPLITTED_CODE. REFRESH SPLITTED_CODE.
  CLEAR CODE-LINE.

  OFFSET_CODE = OFFSET_CODE + APP_OFF_BEF.
*Kein Coding
  IF APP_CODE = SPACE.
*Kommentar
    IF APP_COMMENT NE SPACE.
      CODE = '*'.
      CODE+1 = APP_COMMENT.
      IF CODE+1(2) = SPACE OR
         CODE+1(2) = '--'.
        CODE+70 = '* '.
      ENDIF.
    ENDIF.
    APPEND CODE.
  ELSE.
*Evtl. Zeilensplit
    WRITE APP_CODE TO UNSPLITTED_LINE+OFFSET_CODE.
    CALL FUNCTION 'G_SPLIT_LINE'
      EXPORTING
        INPUT_LINE   = UNSPLITTED_LINE
      TABLES
        EXPORT_LINES = SPLITTED_CODE
      EXCEPTIONS
        OTHERS       = 1.
*Interne Tabelle Splitted_code abarbeiten
    LOOP AT SPLITTED_CODE.
      CODE-LINE = SPLITTED_CODE-LINE.
      IF APP_COMMENT NE SPACE AND
      CODE+39 = SPACE.          "Coding nie mit Kommentar überschreiben
        CODE+39(1) = '"'.
        CODE+40 = APP_COMMENT.
      ENDIF.
      APPEND CODE.                     "Zeile anhängen
    ENDLOOP.
  ENDIF.
  OFFSET_CODE = OFFSET_CODE + APP_OFF_AFT.
ENDFORM.                    "APPEND_CODE
