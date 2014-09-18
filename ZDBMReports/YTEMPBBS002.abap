REPORT ytempbbs001.
TABLES: marc.

DATA: BEGIN OF it_itab OCCURS 0,
        matnr   LIKE   mara-matnr,
        kzkfg   LIKE   mara-kzkfg,
        msg(100),
      END   OF it_itab.

DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

DATA: BEGIN OF wa_opt OCCURS 0.
        INCLUDE STRUCTURE ctu_params.
DATA: END OF wa_opt.

SELECT-OPTIONS: s_matnr FOR marc-matnr.

AT SELECTION-SCREEN.
*---// BDC MODE, DEFAULT SIZE, UPDATE MODE
  wa_opt-defsize = 'X'.
  wa_opt-dismode = 'N'.
  wa_opt-updmode = 'S'.

  SELECT matnr kzkfg
    INTO CORRESPONDING FIELDS OF TABLE it_itab
    FROM mara
   WHERE matnr IN s_matnr
     AND kzkfg EQ space.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

START-OF-SELECTION.

  LOOP AT it_itab.
    PERFORM execute_bdc.
    MODIFY it_itab.
  ENDLOOP.


  LOOP AT it_itab WHERE msg NE space.
    WRITE:/ it_itab-matnr,
            it_itab-kzkfg,
            it_itab-msg.
  ENDLOOP.
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM execute_bdc.
  REFRESH: it_bdc.

  PERFORM dynpro USING:
     'X' 'SAPLMGMM'            '0060',
     ' ' 'RMMG1-MATNR'         it_itab-matnr,
     ' ' 'BDC_OKCODE'          '=AUSW',

     'X' 'SAPLMGMM'            '0070',
     ' ' 'MSICHTAUSW-KZSEL(2)' 'X',
     ' ' 'BDC_OKCODE'          '/00',


     'X' 'SAPLMGMM'            '5004',
     ' ' 'MARA-KZKFG'          'X',
     ' ' 'BDC_OKCODE'          '=BU'.

  CALL TRANSACTION 'MM02'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '801'.
    PERFORM get_err_msg USING it_itab-msg.
  ENDIF.
ENDFORM.                    " EXECUTE_BDC
*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3789   text
*      -->P_3790   text
*      -->P_3791   text
*----------------------------------------------------------------------*
FORM dynpro USING dynbegin name value.
  IF dynbegin = 'X'.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-program,
          value TO it_bdc-dynpro,
          dynbegin TO it_bdc-dynbegin.
    APPEND it_bdc.
  ELSE.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-fnam,
          value TO it_bdc-fval.
    APPEND it_bdc.
  ENDIF.
ENDFORM.                    " dynpro
*&---------------------------------------------------------------------*
*&      Form  get_err_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_9001_ZMSG  text
*----------------------------------------------------------------------*
FORM get_err_msg USING pw_msg.
  DATA: lw_msg LIKE cfgnl-msglin.

  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            id      = sy-msgid
            mtype   = sy-msgty
            number  = sy-msgno
            par1    = sy-msgv1
            par2    = sy-msgv2
            par3    = sy-msgv3
            par4    = sy-msgv4
       IMPORTING
            msg_lin = lw_msg.

  MOVE: lw_msg TO pw_msg.
ENDFORM.                    " get_err_msg
