************************************************************************
* Program Name      : ZCPP103I_CU61.
* Author            : Bobby
* Creation Date     : 2003.08.18.
* Specifications By : Bobby
* Pattern           : 1.1
* Development Request No : UD1K901939
* Addl Documentation:
* Description       : [BDC] VARIANT TABLE CREATE (CU61)
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  zcpp103i_cu61   NO STANDARD PAGE HEADING
                        LINE-SIZE 120
                        MESSAGE-ID zmpp.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS:
      p_hd_lin                TYPE  i   DEFAULT 2   NO-DISPLAY,
                                             " HEAD LINE FOR EXCEL FILE.
      p_tcode                 LIKE  tstc-tcode                ,
      p_cmode                 TYPE  c                         ,
      p_pmode                 TYPE  c   DEFAULT 'N'           .
SELECTION-SCREEN END OF BLOCK b1.

DATA: wa_value(40),
      wa_tmp_flag,
      wa_field_name(30),
      wa_tmp_text(225) ,
      wa_tmp_total(06)        TYPE n,
      wa_tmp_process(06)      TYPE n,
      it_msg                  LIKE TABLE OF bdcmsgcoll WITH HEADER LINE,
      it_bdcdata              LIKE TABLE OF bdcdata    WITH HEADER LINE.

DATA: wa_filename             LIKE  rlgrap-filename,
      wa_filetype             LIKE  rlgrap-filetype VALUE 'DAT',
      wa_bdcgroup             LIKE  sy-uname.          " APQI-GROUPID

DATA: BEGIN OF it_rec OCCURS 0,
        vtnam(040),                        " TABLE NAME
        vttxt(040),                        " TABLE DESCRIBTION
        field000(40) ,                     " RESULT FIELD
        field001(40) ,                     " KEY FIELD          .
        field002(40) ,                     " KEY FIELD          .
        field003(40) ,                     " KEY FIELD          .
        field004(40) ,                     " KEY FIELD          .
        field005(40) ,                     " KEY FIELD          .
        field006(40) ,                     " KEY FIELD          .
        field007(40) ,                     " KEY FIELD          .
        field008(40) ,                     " KEY FIELD          .
        field009(40) ,                     " KEY FIELD          .
        field010(40) ,                     " KEY FIELD          .
        field011(40) ,                     " KEY FIELD          .
        field012(40) ,                     " KEY FIELD          .
        field013(40) ,                     " KEY FIELD          .
        field014(40) ,                     " KEY FIELD          .
        field015(40) ,                     " KEY FIELD          .
        field016(40) ,                     " KEY FIELD          .
        field017(40) ,                     " KEY FIELD          .
        field018(40) ,                     " KEY FIELD          .
        field019(40) ,                     " KEY FIELD          .
        field020(40) ,                     " KEY FIELD          .
        field021(40) ,                     " KEY FIELD          .
*        field022(40) ,                     " KEY FIELD          .
*        field023(40) ,                     " KEY FIELD          .
*        field024(40) ,                     " KEY FIELD          .
*        field025(40) ,                     " KEY FIELD          .
*        field026(40) ,                     " KEY FIELD          .
*        field027(40) ,                     " KEY FIELD          .
*        field028(40) ,                     " KEY FIELD          .
*        field029(40) ,                     " KEY FIELD          .
*        field030(40) ,                     " KEY FIELD          .
*        field031(40) ,                     " KEY FIELD          .
*        field032(40) ,                     " KEY FIELD          .
*        field033(40) ,                     " KEY FIELD          .
*        field034(40) ,                     " KEY FIELD          .
*        field035(40) ,                     " KEY FIELD          .
*        field036(40) ,                     " KEY FIELD          .
*        field037(40) ,                     " KEY FIELD
*        field038(40) ,                     " KEY FIELD
*        field039(40) ,                     " KEY FIELD
*        field040(40) ,                     " KEY FIELD
*        field041(40) ,                     " KEY FIELD
*        field042(40) ,                     " KEY FIELD
*        field043(40) ,                     " KEY FIELD
*        field044(40) ,                     " KEY FIELD
*        field045(40) ,                     " KEY FIELD
*        field046(40) ,                     " KEY FIELD
*        field047(40) ,                     " KEY FIELD
*        field048(40) ,                     " KEY FIELD
*        field049(40) ,                     " KEY FIELD
*        field050(40) ,                     " KEY FIELD
*        field051(40) ,                     " KEY FIELD
*        field052(40) ,                     " KEY FIELD
*        field053(40) ,                     " KEY FIELD
*        field054(40) ,                     " KEY FIELD
*        field055(40) ,                     " KEY FIELD
*        field056(40) ,                     " KEY FIELD
*        field057(40) ,                     " KEY FIELD
*        field058(40) ,                     " KEY FIELD
*        field059(40) ,                     " KEY FIELD
*        field060(40) ,                     " KEY FIELD
*        field061(40) ,                     " KEY FIELD
*        field062(40) ,                     " KEY FIELD
*        field063(40) ,                     " KEY FIELD
*        field064(40) ,                     " KEY FIELD
*        field065(40) ,                     " KEY FIELD
*        field066(40) ,                     " KEY FIELD
*        field067(40) ,                     " KEY FIELD
*        field068(40) ,                     " KEY FIELD
*        field069(40) ,                     " KEY FIELD
*        field070(40) ,                     " KEY FIELD
*        field071(40) ,                     " KEY FIELD
*        field072(40) ,                     " KEY FIELD
*        field073(40) ,                     " KEY FIELD
*        field074(40) ,                     " KEY FIELD
*        field075(40) ,                     " KEY FIELD
*        field076(40) ,                     " KEY FIELD
*        field077(40) ,                     " KEY FIELD
*        field078(40) ,                     " KEY FIELD
*        field079(40) ,                     " KEY FIELD
*        field080(40) ,                     " KEY FIELD
*        field081(40) ,                     " KEY FIELD
*        field082(40) ,                     " KEY FIELD
*        field083(40) ,                     " KEY FIELD
*        field084(40) ,                     " KEY FIELD
*        field085(40) ,                     " KEY FIELD
*        field086(40) ,                     " KEY FIELD
*        field087(40) ,                     " KEY FIELD
*        field088(40) ,                     " KEY FIELD
*        field089(40) ,                     " KEY FIELD
*        field090(40) ,                     " KEY FIELD
*        field091(40) ,                     " KEY FIELD
*        field092(40) ,                     " KEY FIELD
*        field093(40) ,                     " KEY FIELD
*        field094(40) ,                     " KEY FIELD
*        field095(40) ,                     " KEY FIELD
*        field096(40) ,                     " KEY FIELD
*        field097(40) ,                     " KEY FIELD
*        field098(40) ,                     " KEY FIELD
*        field099(40) ,                     " KEY FIELD
*        field100(40) ,                     " KEY FIELD
      END OF it_rec.


INITIALIZATION.
  wa_bdcgroup = sy-uname.
  p_tcode  = 'CU61' .
  IF sy-uname = 'BOBBY'.
    p_pmode = 'A'.
  ENDIF.

*------> START-OF-SELECTION.
START-OF-SELECTION.
  PERFORM bdc_upload_data.
  IF p_cmode = 'X'.
    PERFORM bdc_open_group.
  ENDIF.

  DELETE it_rec INDEX 1 .
  READ TABLE it_rec INDEX 1.
  wa_value = it_rec-vtnam+7(33) .

  DELETE it_rec INDEX 1 .
  DELETE it_rec INDEX 1 .
  DELETE it_rec INDEX 1 .
  DELETE it_rec INDEX 1 .

  PERFORM execute_process.

*------> END-OF-SELECTION.
END-OF-SELECTION.

  INCLUDE zcpp103_common_routine .

*&---------------------------------------------------------------------*
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM execute_process.
  LOOP AT it_rec  .
    CLEAR: wa_tmp_flag.
    PERFORM check_exist_data  USING wa_tmp_flag .
    CHECK wa_tmp_flag IS INITIAL.
    PERFORM generate_bdc_data.
    PERFORM call_transaction.
  ENDLOOP.
ENDFORM.                    " EXECUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0016   text
*----------------------------------------------------------------------*
FORM generate_bdc_data.
  FIELD-SYMBOLS <field>.
  DATA: l_cn(3)               TYPE n                   .

  PERFORM bdc_dynpro_processing USING:
                     'X' 'SAPLCUTU'             '0100',
                     ' ' 'BDC_OKCODE'           '=BASD',
                     ' ' 'RCUTU-VTNAM'          it_rec-vtnam,
*                      ' ' 'RCUTU-DATUM'          IT_REC-DATUM,

                     'X' 'SAPLCUTU'             '0200',
                     ' ' 'BDC_OKCODE'           '=PARM',
                     ' ' 'RCUTU-VTTXT'          it_rec-vttxt,
                     ' ' 'RCUTU-VTSTA'          '1'    ,
*                    ' ' 'RCUTU-VTGRU'          sv_group,
                     ' ' 'RCUTU-VTDCT'          'X'    ,

                     'X' 'SAPLCUTU'             '0300',
                     ' ' 'BDC_OKCODE'           '/00' ,
                     ' ' 'RCUTU-ATNAM(01)'      it_rec-field000,
                     ' ' 'RCUTU-ATNAM(02)'      it_rec-field001,

                     'X' 'SAPLCUTU'             '0300',
                     ' ' 'BDC_OKCODE'           '/00' ,
                     ' ' 'RCUTU-ATKEY(02)'      ' '   .

  l_cn = 1 .

  DO 20 TIMES.
    l_cn = l_cn + 1 .
*    IF <field> NE SPACE  .
*    UNASSIGN <FIELD>.
*    ENDIF.

    CONCATENATE 'IT_REC-FIELD'  l_cn  INTO wa_field_name .
    ASSIGN (wa_field_name) TO <field> .
    IF <field> IS INITIAL. CONTINUE.  ENDIF.

    PERFORM bdc_dynpro_processing USING:
                    'X' 'SAPLCUTU'             '0300',
                    ' ' 'BDC_OKCODE'           '=NEWF',

                    'X' 'SAPLCUTU'             '0300',
                    ' ' 'BDC_OKCODE'           '/00' ,
                    ' ' 'RCUTU-ATNAM(02)'      <field>,

                    'X' 'SAPLCUTU'             '0300',
                    ' ' 'BDC_OKCODE'           '/00' ,
                    ' ' 'RCUTU-ATKEY(02)'      'X'   .
  ENDDO.

  PERFORM bdc_dynpro_processing USING:
                    'X' 'SAPLCUTU'             '0300',
                    ' ' 'BDC_OKCODE'           '=SAVE'.
ENDFORM.                    " GENERATE_BDC_DATA

*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_transaction.
  IF p_cmode = space .
    CALL TRANSACTION 'CU61' USING it_bdcdata MODE p_pmode
                            MESSAGES INTO it_msg    .

    LOOP AT it_msg .
      CLEAR: wa_tmp_text .
      PERFORM create_message    USING  wa_tmp_text   it_msg-msgid
                                       it_msg-msgnr  it_msg-msgv1
                                       it_msg-msgv2  it_msg-msgv3
                                       it_msg-msgv4  .
      PERFORM display_message   USING  wa_tmp_text   it_msg-msgtyp
                                       wa_tmp_total  wa_tmp_process  .
    ENDLOOP.

  ELSE.
    p_tcode = 'CU61' .
    PERFORM bdc_insert_transaction .
  ENDIF.
  REFRESH it_bdcdata.
ENDFORM.                    " CALL_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  CHECK_EXIST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TMP_FLAG  text
*----------------------------------------------------------------------*
FORM check_exist_data USING    p_flag.
  SELECT SINGLE vtnam INTO it_rec-vtnam
    FROM cuvtab
   WHERE vtnam = it_rec-vtnam .

  IF sy-subrc = 0.
*   p_flag = 'X'.
    PERFORM bdc_dynpro_processing USING:
                       'X' 'SAPLCUTU'             '0100',
                       ' ' 'BDC_OKCODE'           '=DELX',
                       ' ' 'RCUTU-VTNAM'          it_rec-vtnam,

                       'X' 'SAPLSPO1'             '0100',
                       ' ' 'BDC_OKCODE'           '=YES' .

    IF p_cmode = space .
      CALL TRANSACTION 'CU62' USING it_bdcdata MODE p_pmode
                              MESSAGES INTO it_msg    .

      LOOP AT it_msg .
        CLEAR: wa_tmp_text .
        PERFORM create_message    USING  wa_tmp_text   it_msg-msgid
                                         it_msg-msgnr  it_msg-msgv1
                                         it_msg-msgv2  it_msg-msgv3
                                         it_msg-msgv4  .
        PERFORM display_message   USING  wa_tmp_text   it_msg-msgtyp
                                         wa_tmp_total  wa_tmp_process  .
      ENDLOOP.
    ELSE.
      p_tcode = 'CU62' .
      PERFORM bdc_insert_transaction .
    ENDIF.
    REFRESH it_bdcdata.
    clear: it_msg, it_msg[], it_bdcdata, it_bdcdata[].
  ENDIF.
ENDFORM.                    " CHECK_EXIST_DATA
