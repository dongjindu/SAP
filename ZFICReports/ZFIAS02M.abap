report ZFIAS02M
       no standard page heading line-size 255.

include bdcrecx1.

*replace
*parameters: dataset(132) lower case.
PARAMETERS : LOCFILE LIKE RLGRAP-FILENAME DEFAULT
   'c:\temp\as02.txt'.
***    DO NOT CHANGE - the generated data section - DO NOT CHANGE    ***
*
*   If it is nessesary to change the data section use the rules:
*   1.) Each definition of a field exists of two lines
*   2.) The first line shows exactly the comment
*       '* data element: ' followed with the data element
*       which describes the field.
*       If you don't have a data element use the
*       comment without a data element name
*   3.) The second line shows the fieldname of the
*       structure, the fieldname must consist of
*       a fieldname and optional the character '_' and
*       three numbers and the field length in brackets
*   4.) Each field must be type C.
*
*** Generated data section with specific formatting - DO NOT CHANGE  ***
parameters: P_BUKRS like anla-bukrs memory id BUK OBLIGATORY .

data: begin of record occurs 0,
* data element: ANLN1
        ANLN1_001(012),
* data element: ANLN2
        ANLN2_002(004),
* data element: Date
        BDATU(010),
* data element: KOSTL
        KOSTL(010),
* data element: KOSTLV
        KOSTLV(010),
* data element: WERKS_D : Plant
        WERKS(004),
* data element: STORT   : Location
        STORT(010),
* data element: ORD41   : eval.group 1
        EVAL1(004),
* data element: IZWEK   : Reason for investment
        IZWEK_013(002),
* data element: AM_UMWKZ: Reason for environmental investment
        UMWKZ_014(005),
* data element: AM_AUFNR: order number
        EAUFN_017(012),
* data element: VMGLI   : Property classification key
        VMGLI_022(004),
* data element: EIGKZ   : Property indicator
        EIGKZ_023(001),

* data element: AFASL
* data element: NDJAR
        AFASL_01(004),
        NDJAR_01(003),
        AFASL_10(004),
        NDJAR_10(003),
        AFASL_20(004),
        NDJAR_20(003),
        AFASL_30(004),
        NDJAR_30(003),
        AFASL_40(004),
        NDJAR_40(003),
      end of record.

*** End generated data section ***
tables: anlz, anla, ANLB.

start-of-selection.

*add source
  PERFORM UPLOAD_PC_FILE.

  perform open_group.

* replace
  LOOP AT RECORD.
    perform get_default_value.

    perform bdc_dynpro  using 'SAPLAIST' '0100'.
    perform bdc_fld     using ' ' 'BDC_CURSOR'        'ANLA-ANLN1'.
    perform bdc_fld     using ' ' 'BDC_OKCODE'        '/00'.
    perform bdc_fld     using ' ' 'ANLA-ANLN1'        record-ANLN1_001.
    perform bdc_fld     using ' ' 'ANLA-ANLN2'        record-ANLN2_002.
    perform bdc_fld     using ' ' 'ANLA-BUKRS'        p_bukrs.

    perform bdc_dynpro  using 'SAPLAIST' '1000'.
    perform bdc_fld     using ' ' 'BDC_OKCODE'        '=TAB02'.

* Organization --> All required field...

    perform bdc_dynpro  using 'SAPLAIST' '1000'.
    perform bdc_fld     using ' ' 'BDC_OKCODE'        '=TAB03'.
    perform bdc_fld     using ' ' 'BDC_CURSOR'        'ANLZ-KOSTLV'.
    perform bdc_fld     using 'O' 'ANLZ-KOSTL'        record-KOSTL.
    perform bdc_fld     using 'O' 'ANLZ-KOSTLV'       record-KOSTLV.

    perform bdc_fld     using 'O' 'ANLZ-WERKS'        record-WERKS.
    perform bdc_fld     using 'O' 'ANLZ-STORT'        record-STORT.
* Allocation (Eval.grp1, Invest reason, Env Reason)
    perform bdc_dynpro  using 'SAPLAIST' '1000'.
    perform bdc_fld     using ' ' 'BDC_OKCODE'        '=TAB04'.
    perform bdc_fld     using ' ' 'BDC_CURSOR'        'ANLA-UMWKZ'.
    perform bdc_fld     using 'O' 'ANLA-ORD41'        record-EVAL1.
    perform bdc_fld     using 'O' 'ANLA-IZWEK'        record-IZWEK_013.
    perform bdc_fld     using 'O' 'ANLA-UMWKZ'        record-UMWKZ_014.
* Order
    perform bdc_dynpro  using 'SAPLAIST' '1000'.
    perform bdc_fld     using ' ' 'BDC_OKCODE'        '=TAB05'.
    perform bdc_fld     using ' ' 'BDC_CURSOR'        'ANLA-EAUFN'.
    perform bdc_fld     using 'O' 'ANLA-EAUFN'        record-EAUFN_017.
* Tax Classification
    perform bdc_dynpro  using 'SAPLAIST' '1000'.
    perform bdc_fld     using ' ' 'BDC_OKCODE'        '=TAB08'.
    perform bdc_fld     using ' ' 'BDC_CURSOR'        'ANLA-EIGKZ'.
    perform bdc_fld     using 'X' 'ANLA-VMGLI'        record-VMGLI_022.
    perform bdc_fld     using 'X' 'ANLA-EIGKZ'        record-EIGKZ_023.
* Depreciation
    perform bdc_dynpro  using 'SAPLAIST' '1000'.
    perform bdc_fld     using ' ' 'BDC_OKCODE'        '=BUCH'.
    perform bdc_fld     using ' ' 'BDC_CURSOR'        'ANLB-NDJAR(01)'.
    perform bdc_fld     using 'O' 'ANLB-AFASL(01)'    record-AFASL_01.
    perform bdc_fld     using 'O' 'ANLB-NDJAR(01)'    record-NDJAR_01.
    perform bdc_fld     using 'O' 'ANLB-AFASL(02)'    record-AFASL_10.
    perform bdc_fld     using 'O' 'ANLB-NDJAR(02)'    record-NDJAR_10.
    perform bdc_fld     using 'O' 'ANLB-AFASL(03)'    record-AFASL_20.
    perform bdc_fld     using 'O' 'ANLB-NDJAR(03)'    record-NDJAR_20.
    perform bdc_fld     using 'O' 'ANLB-AFASL(04)'    record-AFASL_30.
    perform bdc_fld     using 'O' 'ANLB-NDJAR(04)'    record-NDJAR_30.
    perform bdc_fld     using 'O' 'ANLB-AFASL(05)'    record-AFASL_40.
    perform bdc_fld     using 'O' 'ANLB-NDJAR(05)'    record-NDJAR_40.

    perform bdc_transaction using 'AS02'.

*replace source
  ENDLOOP.

  perform close_group.
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PC_FILE
*&---------------------------------------------------------------------*
FORM UPLOAD_PC_FILE.
  CALL FUNCTION 'UPLOAD'
       EXPORTING
            FILENAME            = LOCFILE
            FILETYPE            = 'DAT'
       TABLES
            DATA_TAB            = RECORD
       EXCEPTIONS
            CONVERSION_ERROR    = 1
            FILE_OPEN_ERROR     = 2
            FILE_READ_ERROR     = 3
            INVALID_TABLE_WIDTH = 4
            INVALID_TYPE        = 5
            NO_BATCH            = 6
            UNKNOWN_ERROR       = 7
            OTHERS              = 8.
ENDFORM.                    " UPLOAD_PC_FILE
*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FLD USING F_REQ FNAM FVAL.
  case f_req.
    when ' '.
      IF FVAL <> NODATA.
        CLEAR BDCDATA.
        BDCDATA-FNAM = FNAM.
        BDCDATA-FVAL = FVAL.
        APPEND BDCDATA.
      endif.
    when 'O'.  "if value exist, ...
      IF FVAL <> NODATA or FVAL <> SPACE.
        CLEAR BDCDATA.
        BDCDATA-FNAM = FNAM.
        BDCDATA-FVAL = FVAL.
        APPEND BDCDATA.
      endif.
    when 'X'.  "overwrite, space, ...ok.
      CLEAR BDCDATA.
      BDCDATA-FNAM = FNAM.
      BDCDATA-FVAL = FVAL.
      APPEND BDCDATA.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_default_value
*&---------------------------------------------------------------------*
FORM get_default_value.
  data: l_anln1 like anlz-anln1,
        l_anln2 like anlz-anln2.

  if record-bdatu is initial.
    record-bdatu = sy-datum.
  endif.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = record-anln1_001
       IMPORTING
            OUTPUT = l_anln1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = record-anln2_002
       IMPORTING
            OUTPUT = l_anln2.

  select single * from anla
     where BUKRS  = p_bukrs
       and ANLN1  = l_anln1
       and ANLN2  = l_anln2.
  if record-eval1 = space.
    record-eval1 = anla-ORD41.
  endif.
  if record-IZWEK_013 = space.
    record-IZWEK_013 = anla-IZWEK.
  endif.
  if record-UMWKZ_014 = space.
    record-UMWKZ_014 = anla-UMWKZ.
  endif.
  if record-EAUFN_017 = space.
    record-EAUFN_017 = anla-EAUFN.
  endif.

* Tax classification.
  if record-VMGLI_022 = space.
    record-VMGLI_022 = anla-VMGLI.
  endif.
  if record-EIGKZ_023 = space.
    record-EIGKZ_023 = anla-EIGKZ.
  endif.

* Dep.key
  select single * from anlb
     where BUKRS  = p_bukrs
       and ANLN1  = l_anln1
       and ANLN2  = l_anln2
       and AFABE  = '01'.
  if record-AFASL_01 = space.
    record-AFASL_01 = anlb-afasl.
  endif.
  if record-NDJAR_01 = space.
    record-NDJAR_01 = anlb-ndjar.
  endif.

  select single * from anlb
     where BUKRS  = p_bukrs
       and ANLN1  = l_anln1
       and ANLN2  = l_anln2
       and AFABE  = '10'.
  if record-AFASL_10 = space.
    record-AFASL_10 = anlb-afasl.
  endif.
  if record-NDJAR_10 = space.
    record-NDJAR_10 = anlb-ndjar.
  endif.

  select single * from anlb
     where BUKRS  = p_bukrs
       and ANLN1  = l_anln1
       and ANLN2  = l_anln2
       and AFABE  = '20'.
  if record-AFASL_20 = space.
    record-AFASL_20 = anlb-afasl.
  endif.
  if record-NDJAR_20 = space.
    record-NDJAR_20 = anlb-ndjar.
  endif.

  select single * from anlb
     where BUKRS  = p_bukrs
       and ANLN1  = l_anln1
       and ANLN2  = l_anln2
       and AFABE  = '30'.
  if record-AFASL_30 = space.
    record-AFASL_30 = anlb-afasl.
  endif.
  if record-NDJAR_30 = space.
    record-NDJAR_30 = anlb-ndjar.
  endif.

  select single * from anlb
     where BUKRS  = p_bukrs
       and ANLN1  = l_anln1
       and ANLN2  = l_anln2
       and AFABE  = '40'.
  if record-AFASL_40 = space.
    record-AFASL_40 = anlb-afasl.
  endif.
  if record-NDJAR_40 = space.
    record-NDJAR_40 = anlb-ndjar.
  endif.

* Org
  select single * from anlz
     where BUKRS  = p_bukrs
       and ANLN1  = l_anln1
       and ANLN2  = l_anln2
       and bdatu >= record-bdatu.

  if record-kostl = space.
    record-kostl = anlz-kostl.
  endif.
  if record-kostlv = space.
    record-kostlv = anlz-kostlv.
  endif.
  if record-werks = space.
    record-werks = anlz-werks.
  endif.
  if record-stort = space.
    record-stort = anlz-stort.
  endif.

ENDFORM.                    " get_default_value
