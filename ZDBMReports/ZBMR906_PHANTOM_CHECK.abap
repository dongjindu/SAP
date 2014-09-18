REPORT zbmr904_bom_multi_implosion MESSAGE-ID zmpp
                   NO STANDARD PAGE HEADING LINE-SIZE 130 .

* Development Request No :UD1K913289
* Addl Documentation:
* Description       : Check the phantom materials and update
*                     the special procurment type if it's not '50'
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
*****************************************************************
*GLOBAL DATA
*****************************************************************
TABLES: mara, marc.


DATA: c_roh(4) TYPE c VALUE 'ROH',
      c_roh1(4) TYPE c VALUE 'ROH1',
      c_halb(4) TYPE c VALUE 'HALB',
      c_alt(2)  TYPE c VALUE '01'.
DATA: g_message(50) TYPE c.
DATA: s_no_exist(1).
****************************************************************
*INTERNAL TABLES
****************************************************************
DATA: BEGIN OF it_material OCCURS 0,
        matnr LIKE mara-matnr,
        maktx LIKE makt-maktx,
        mtart LIKE mara-mtart,
        werks LIKE marc-werks,
        beskz LIKE marc-beskz,  "PROCUREMENT TYPE
        sobsl LIKE marc-sobsl,  "SPECIAL PROCUREMNT TYPE
        ncost LIKE marc-ncost,  "NO COSTING
        ersda LIKE mara-ersda,   "CREATE DATE
        laeda LIKE mara-laeda,  "CHANGE DATE
        updat(8) TYPE c,       " 'FAIL' or 'SUCCESS'
      END OF it_material.
DATA: it_checked LIKE it_material OCCURS 0 WITH HEADER LINE.

*FOR BDC
DATA: BEGIN OF bdcdata OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.

DATA: BEGIN OF it_message OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_message.
DATA: BEGIN OF it_mess OCCURS 0,
        msgty LIKE sy-msgty,
        msgtx(100) TYPE c,
      END OF it_mess.


*****************************************************************
*END OF  DATA DECLARATION
*****************************************************************


*****************************************************************
*SELECTION-SCREEN
*****************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_case1 RADIOBUTTON  GROUP rg USER-COMMAND ucom    .
SELECTION-SCREEN COMMENT  (40) text-002 FOR FIELD p_case1  .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  10(10) text-003 FOR FIELD p_matnr .
SELECT-OPTIONS: p_matnr  FOR  mara-matnr.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_case2         RADIOBUTTON  GROUP rg    .
SELECTION-SCREEN COMMENT  (40) text-004 FOR FIELD p_case2       .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  10(10) text-003 FOR FIELD p_date     .
SELECT-OPTIONS: p_date  FOR sy-datum DEFAULT sy-datum.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(18) text-005 FOR FIELD p_plant.
SELECT-OPTIONS:   p_plant  FOR  marc-werks DEFAULT 'P001'.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.
**********************************************************************
*END OF SELECTION SCREEN
**********************************************************************



*********************************************************************
***EVENTS
*********************************************************************
AT SELECTION-SCREEN.


START-OF-SELECTION.
  PERFORM read_materials.
  PERFORM check_phantom.
  PERFORM update_master.


END-OF-SELECTION.
  PERFORM write_output.

************************************************************************
*     FORMS                                                            *
***********************************************************************
*&---------------------------------------------------------------------*
*&      Form  READ_MATERIALS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_materials.

  CASE 'X'.
    WHEN p_case1.
      SELECT a~matnr c~maktx a~mtart b~werks
              b~beskz b~sobsl b~ncost a~ersda
             a~laeda
      INTO TABLE it_material
      FROM ( mara AS a INNER JOIN marc AS b ON
             a~matnr = b~matnr )
            INNER JOIN makt AS c ON
             a~matnr = c~matnr
      WHERE a~matnr IN p_matnr   AND
            b~werks IN p_plant   AND
            ( a~mtart = c_halb OR
              a~mtart = c_roh  OR
              a~mtart = c_roh1 ).

      IF sy-subrc NE 0.
        s_no_exist = 'X'.
        g_message = 'No Material Exist!'.
      ENDIF.
    WHEN p_case2.
      SELECT a~matnr c~maktx a~mtart b~werks
              b~beskz b~sobsl b~ncost a~ersda
              a~laeda
      INTO TABLE it_material
      FROM ( mara AS a INNER JOIN marc AS b ON
             a~matnr = b~matnr )
            INNER JOIN makt AS c ON
             a~matnr = c~matnr
      WHERE ( a~ersda IN p_date OR
              a~laeda IN p_date ) and
            B~WERKS in P_PLANT    AND
            ( a~mtart = c_halb OR
             a~mtart = c_roh  OR
              a~mtart = c_roh1 ).
      IF sy-subrc NE 0.
        s_no_exist = 'X'.
        g_message = 'No Material Exist!'.
      ENDIF.

  ENDCASE.
ENDFORM.                    " READ_MATERIALS
*&---------------------------------------------------------------------*
*&      Form  CHECK_PHANTOM
*&---------------------------------------------------------------------*
*       CHeck the procurement type and keep the
*       wrong records for update
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_phantom.
  DATA: l_tabix LIKE sy-tabix.
  DATA: i_lines TYPE i.
  IF s_no_exist = 'X'.
    EXIT.
  ENDIF.
  LOOP AT it_material.
    l_tabix = sy-tabix.
    IF ( ( it_material-mtart = c_roh OR
       it_material-mtart = c_roh1 ) AND
       ( it_material-sobsl = '50' OR
         it_material-ncost = 'X' ) )    OR
       ( it_material-mtart = c_halb AND
         it_material-beskz = 'E'    AND
        ( it_material-sobsl = '50' OR
         it_material-ncost = 'X' ) )    OR
       ( it_material-mtart = c_halb AND
         it_material-beskz NE 'E'    AND
        ( it_material-sobsl NE '50' OR
         it_material-ncost NE 'X' ) ).
    ELSE.
      DELETE it_material INDEX l_tabix.
    ENDIF.
  ENDLOOP.
  SORT it_material BY matnr.
  DESCRIBE TABLE it_material LINES i_lines.
  IF i_lines = 0.
    s_no_exist = 'N'.
  ENDIF.
ENDFORM.                    " CHECK_PHANTOM
*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_data.
  DATA: i_count TYPE i.
  DATA: i_mod TYPE i.

  SORT it_material BY matnr.
  LOOP AT it_material.
    WRITE :/ sy-vline NO-GAP, 2(18) it_material-matnr  NO-GAP,
            sy-vline NO-GAP, 21(6) it_material-mtart  NO-GAP,
            sy-vline NO-GAP, 28(35) it_material-maktx  NO-GAP,
            sy-vline NO-GAP, 64(6) it_material-werks  NO-GAP,
            sy-vline NO-GAP, 71(10) it_material-beskz  NO-GAP,
            sy-vline NO-GAP, 82(7) it_material-sobsl  NO-GAP,
            sy-vline NO-GAP, 90(7) it_material-ncost  NO-GAP,
            sy-vline NO-GAP, 98(11) it_material-ersda  NO-GAP,
            sy-vline NO-GAP, 110(11) it_material-laeda  NO-GAP,
            sy-vline NO-GAP, 122(8) it_material-updat  NO-GAP,
            sy-vline NO-GAP.
    i_count = i_count + 1.
    i_mod = i_count MOD 5.
    IF i_mod = 0.
      ULINE.
    ENDIF.
  ENDLOOP.
  IF i_mod NE 0.
    ULINE.
  ENDIF.
* WRITE THE MESSAGE
  DESCRIBE TABLE it_mess LINES i_count.
  IF i_count NE 0.
    SKIP.
    ULINE.
    WRITE: / 'The Following Error Occurs: '.
    ULINE.
    i_count = 1.
    LOOP AT it_mess.
      WRITE: / i_count, it_mess-msgtx.
      i_count = i_count + 1.
    ENDLOOP.
    ULINE.
  ENDIF.

ENDFORM.                    " write_data
*&---------------------------------------------------------------------*
*&      Form  WRITE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_header.
  ULINE.
  FORMAT COLOR COL_HEADING.
  WRITE :/ sy-vline NO-GAP, text-010  NO-GAP,
           sy-vline NO-GAP, text-011  NO-GAP,
           sy-vline NO-GAP, text-012  NO-GAP,
           sy-vline NO-GAP, text-013  NO-GAP,
           sy-vline NO-GAP, text-014  NO-GAP,
           sy-vline NO-GAP, text-015  NO-GAP,
           sy-vline NO-GAP, text-016  NO-GAP,
           sy-vline NO-GAP, text-017  NO-GAP,
           sy-vline NO-GAP, text-018  NO-GAP,
           sy-vline NO-GAP, text-019 NO-GAP,
           sy-vline NO-GAP.
  FORMAT COLOR OFF.
  ULINE.

ENDFORM.                    " WRITE_HEADER
*&---------------------------------------------------------------------*
*&      Form  WRITE_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_output.


  PERFORM write_header.

  IF s_no_exist = 'X' OR
     s_no_exist = 'N'.
    WRITE: / g_message.
    EXIT.
  ENDIF.

  PERFORM write_data.

ENDFORM.                    " WRITE_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_master.
  DATA: l_ncost LIKE marc-ncost.
  DATA: l_sobsl LIKE marc-sobsl.
  IF s_no_exist = 'X' OR
     s_no_exist = 'N'.
    EXIT.
  ENDIF.
  LOOP AT it_material.
    CLEAR bdcdata.
    REFRESH bdcdata.
    PERFORM update_bdc USING it_material.
  ENDLOOP.
ENDFORM.                    " UPDATE_MASTER
*&---------------------------------------------------------------------*
*&      Form  UPDATE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MATERIAL  text
*----------------------------------------------------------------------*
FORM update_bdc USING p_material LIKE it_material.
  DATA: l_update TYPE c.
  PERFORM fill_bdcdata USING p_material .
  PERFORM call_transaction USING 'MM02' l_update.
  IF l_update = 'S'.
    p_material-updat = 'SUCCESS'.
    MODIFY it_material FROM p_material TRANSPORTING updat.
  ELSE.
    p_material-updat = 'FAIL'.
    MODIFY it_material FROM p_material TRANSPORTING updat.
  ENDIF.
ENDFORM.                    " UPDATE_BDC

*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TCODE  text
*----------------------------------------------------------------------*
FORM call_transaction USING  p_tcode p_update.
  DATA: l_msgstr(100) TYPE c.

  CALL TRANSACTION p_tcode
           USING bdcdata
           MODE 'N'
           UPDATE 'S'
           MESSAGES INTO it_message.
* ckeck the massge
  IF sy-subrc = 0.
    p_update = 'S'.
  ELSE.
    p_update = 'F'.

    PERFORM rkc_msg_string USING l_msgstr.

    it_mess-msgty = sy-msgty.
    it_mess-msgtx = l_msgstr.
    APPEND it_mess.

  ENDIF.

ENDFORM.                    " CALL_TRANSACTION

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_BDCDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MATERIAL  text
*----------------------------------------------------------------------*
FORM fill_bdcdata USING    p_material LIKE it_material .
  DATA: l_ncost LIKE marc-ncost.
  DATA: l_sobsl LIKE marc-sobsl.

*   CHECK THE VALUE AND SET THE TO BE UPDATED VALUE
  IF ( ( p_material-mtart = c_roh OR
       p_material-mtart = c_roh1 ) AND
       ( p_material-sobsl = '50' OR
         p_material-ncost = 'X' ) )    OR
       ( p_material-mtart = c_halb AND
         p_material-beskz = 'E'    AND
        ( p_material-sobsl = '50' OR
         p_material-ncost = 'X' ) ).
    l_ncost = space.
    l_sobsl = space.
  ENDIF.

  IF   p_material-mtart = c_halb AND
         p_material-beskz NE 'E'    AND
        ( p_material-sobsl NE '50' OR
         p_material-ncost NE 'X' ) .
    l_ncost = 'X'.
    l_sobsl = '50'.
  ENDIF.


  PERFORM bdc_dynpro      USING 'SAPLMGMM' '0060'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RMMG1-MATNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=AUSW'.
  PERFORM bdc_field       USING 'RMMG1-MATNR'
                                p_material-matnr.

  PERFORM bdc_dynpro      USING 'SAPLMGMM' '0070'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'MSICHTAUSW-DYTXT(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ENTR'.
  PERFORM bdc_field       USING 'MSICHTAUSW-KZSEL(01)'
                                'X'.

  PERFORM bdc_dynpro      USING 'SAPLMGMM' '5004'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.

  PERFORM bdc_dynpro      USING 'SAPLMGMM' '5004'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SP13'.

  PERFORM bdc_dynpro      USING 'SAPLMGMM' '0081'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RMMG1-WERKS'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ENTR'.
  PERFORM bdc_field       USING 'RMMG1-WERKS'
                                p_material-werks.

  PERFORM bdc_dynpro      USING 'SAPLMGMM' '5000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SP26'.

  PERFORM bdc_field       USING 'MARC-SOBSL'
                                l_sobsl.

  PERFORM bdc_dynpro      USING 'SAPLMGMM' '5000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.

  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'MARC-NCOST'.
  PERFORM bdc_field       USING 'MARC-NCOST'
                                l_ncost.
ENDFORM.                    " FILL_BDCDATA
*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
*       SERACH THE MESSAGE OF BDC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM rkc_msg_string CHANGING p_msg.
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
            msg_lin = lw_msg
       EXCEPTIONS
            OTHERS  = 1.

  MOVE: lw_msg TO p_msg.
ENDFORM.                    " RKC_MSG_STRING
