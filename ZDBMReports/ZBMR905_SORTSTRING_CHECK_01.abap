************************************************************************
* Program Name      : ZBMR905_SORTSTRING_CHECK_01
* Author            : WSKIM
* Creation Date     : 2004.12.23.
* Specifications By : I.Y Choi
* Pattern           :
* Development Request No:
* Addl Documentation:
* Description       : Check the bom component sort string and make
*                     make correction for error & Update
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT zbmr905_sortstring_check_01 MESSAGE-ID zmpp
       NO STANDARD PAGE HEADING LINE-SIZE 127 LINE-COUNT 100.
*****************************************************************
*GLOBAL DATA
*****************************************************************
TABLES: mara, marc, stko, stpo, stas, mast.


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
        matnr LIKE mast-matnr,  "MATERIA NO
        werks LIKE mast-werks,  "PLANT
        stlan LIKE mast-stlan,  "BOM USAGE
        stlnr LIKE mast-stlnr,  "BILL OF BOM
        stlal LIKE mast-stlal,  "ALTERNATIVE BOM
*        ANDAT LIKE MAST-ANDAT,  "CREATION DATE
*        AEDAT LIKE MAST-AEDAT,  "CHANGE DATE
        mtart LIKE mara-mtart,  "MATERIA TYPE
        posnr LIKE stpo-posnr,  "ITEM NO
        idnrk LIKE stpo-idnrk,  "COMPONENT
        suff  LIKE stpo-suff,   "SUFFIX
        sortf LIKE stpo-sortf,  "SORT STRING
        menge LIKE stpo-menge,  "QUANTITY
        datuv LIKE stpo-datuv,  "VALID-FROM DATE
        datub LIKE rc29p-datub, "VALID-TO DATE
        aennr LIKE stpo-aennr,  "CHANGE NUMBER
        stlkn LIKE stpo-stlkn,
        stpoz LIKE stpo-stpoz.
DATA: END OF it_material.
DATA : it_head LIKE it_material OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* BDC-DATA
*----------------------------------------------------------------------*
DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.
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
DATA: BEGIN OF wa_opt OCCURS 0.
        INCLUDE STRUCTURE ctu_params.
DATA: END OF wa_opt.
DATA : mode TYPE c VALUE 'N'..
DATA  write_message.
DATA : it_bomgroup LIKE bapi1080_bgr_c OCCURS 0 WITH HEADER LINE,
       it_variants LIKE bapi1080_bom_c OCCURS 0 WITH HEADER LINE,
       it_items LIKE bapi1080_itm_c OCCURS 0 WITH HEADER LINE,
       it_materialrelations LIKE bapi1080_mbm_c
         OCCURS 0 WITH HEADER LINE,
       it_itemassignments LIKE bapi1080_rel_itm_bom_c
         OCCURS 0 WITH HEADER LINE,
       it_texts LIKE bapi1080_txt_c  OCCURS 0 WITH HEADER LINE,
       it_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.


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
SELECT-OPTIONS:   p_plant  FOR  marc-werks DEFAULT 'P001' OBLIGATORY.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(21) text-006 FOR FIELD p_usage.
PARAMETERS:   p_usage  LIKE mast-stlan DEFAULT '1' OBLIGATORY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(21) text-007 FOR FIELD p_valid.
PARAMETERS:   p_valid  LIKE stko-datuv DEFAULT sy-datum OBLIGATORY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.
**********************************************************************
*END OF SELECTION SCREEN
**********************************************************************



*********************************************************************
***EVENTS
*********************************************************************
AT SELECTION-SCREEN.

AT USER-COMMAND.
  PERFORM update_sortstring.

START-OF-SELECTION.
  SET PF-STATUS '100'.

  PERFORM read_materials.
  PERFORM check_sortstring.
*  FIELD EXIT PREVET FROM UPDATING,
*  PERFORM update_sortstring.


END-OF-SELECTION.
  PERFORM write_output.

************************************************************************
*     FORMS                                                            *
************************************************************************

*&---------------------------------------------------------------------*
*&      Form  READ_MATERIALS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_materials.
  DATA: lt_material LIKE it_material OCCURS 0 WITH HEADER LINE.
  DATA: wa_material LIKE lt_material.
  DATA: i_count TYPE i.
  DATA: l_mtart LIKE mara-mtart.
  DATA: i_deleted TYPE i.


  CASE 'X'.
    WHEN p_case1.
*     READ THE COMPONENTS FROM COMPONENT TABLES
      SELECT a~idnrk a~stlkn a~stlnr a~posnr a~suff
             a~sortf a~datuv a~menge a~stpoz b~stlal
             c~matnr c~stlan c~werks d~mtart a~aennr
      INTO CORRESPONDING FIELDS OF TABLE it_material
      FROM ( ( stpo AS a
           INNER JOIN stas AS b
             ON a~stlty = b~stlty AND
                a~stlnr = b~stlnr AND
                a~stlkn = b~stlkn  )
           INNER JOIN mast AS c
             ON b~stlal = c~stlal AND
                b~stlnr = c~stlnr  )
           INNER JOIN mara AS d
             ON a~idnrk = d~matnr
      WHERE a~idnrk IN p_matnr AND
            a~datuv LE p_valid AND
            a~stlty = 'M'      AND
            b~stlal = c_alt    AND
            b~lkenz NE 'X'     AND
            c~werks IN p_plant AND
            c~stlan = p_usage .

      IF sy-subrc NE 0.
        s_no_exist = 'X'.
        EXIT.
      ENDIF.
*    REDUCE THE DELETED NODE
      PERFORM eleminae_deleted.

    WHEN p_case2.
*   READ THE MATERIALS BASED THE COMPONENT CREATION/CHANGE DATE
      SELECT a~idnrk a~stlkn a~stlnr a~posnr a~suff
                   a~sortf a~datuv a~menge b~stlal c~matnr
                   c~stlan c~werks d~mtart
            INTO CORRESPONDING FIELDS OF TABLE it_material
            FROM ( ( stpo AS a
                 INNER JOIN stas AS b
                   ON a~stlty = b~stlty AND
                      a~stlnr = b~stlnr AND
                      a~stlkn = b~stlkn  )
                 INNER JOIN mast AS c
                   ON b~stlal = c~stlal AND
                      b~stlnr = c~stlnr  )
                 INNER JOIN mara AS d
                   ON a~idnrk = d~matnr
            WHERE ( a~andat IN p_date OR
                    a~aedat IN p_date ) and
                  A~DATUV le P_VALID AND
                  a~stlty = 'M'      AND
                  b~stlal = c_alt    AND
                  b~lkenz NE 'X'     AND
                  c~werks IN p_plant AND
                  c~stlan = p_usage .

      IF sy-subrc NE 0.
        s_no_exist = 'X'.
        g_message = 'No Component for Selection Condition!'.
        EXIT.
      ENDIF.
*    REDUCE THE DELETED NODE
      PERFORM eleminae_deleted.


  ENDCASE.
ENDFORM.                    " READ_MATERIALS
*&---------------------------------------------------------------------*
*&      Form  ELEMINAE_DELETED
*&---------------------------------------------------------------------*
*       CHCEK THE DELETED ITEM AND DETERMINE THE VALID TO
*       DATE, IF THE VALID-TO DATE IS LESS THAN THE SELECTION
*       CONDITION VALID-FROM, DELETE THIS RECORD
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM eleminae_deleted.
  DATA: BEGIN OF lt_deleted OCCURS 0,
         stlty LIKE stas-stlty,
         stlnr LIKE stas-stlnr,
         stlkn LIKE stas-stlkn,
         datuv LIKE stas-datuv,
        END OF lt_deleted.
  DATA: l_tabix LIKE sy-tabix.
  DATA: i_count TYPE i.

  IF s_no_exist = 'X'.
    EXIT.
  ENDIF.
* SEARCH THE DELETED NODE. THE VALID-TO DATE IS DEPEND ON THE
* CHANGE NO THAT IS USED TO DELETE THE NODE.
  SELECT stlty stlnr stlkn datuv
       INTO TABLE lt_deleted
       FROM stas
       FOR ALL ENTRIES IN it_material
       WHERE stlty = 'M' AND
             stlnr = it_material-stlnr AND
             stlkn = it_material-stlkn AND
             lkenz = 'X'.
  SORT lt_deleted BY datuv.
  LOOP AT it_material.
    l_tabix = sy-tabix.
    READ TABLE lt_deleted WITH KEY stlnr = it_material-stlnr
                                  stlkn = it_material-stlkn.
    IF sy-subrc = 0.
*    CHECK THE DELETED NODE VALID-FROM FIELD VALUE, THIS VALUE IS THE
*    NODE VALID-TO DATE AND IF THIS DATE IS LESS THAN THE SELECTION
*    CONDITION VALID-FROM DATE, THIS NODE IS NOT VALID ON THE SPECIFIED
*    DATE, SO DELETE IT, OTHERWISE, KEEP IT AND UPDATE THE VALID-TO DATE
      IF lt_deleted-datuv LT p_valid.
        DELETE it_material INDEX l_tabix.
      ELSE.
        it_material-datub = lt_deleted-datuv.
        MODIFY it_material.
      ENDIF.
    ELSE.
*      IF THE NODE IS NOT DELETED, DEFAULT VALID-TO DATE IS 12/30/9999
      it_material-datub = '99991231'.
      MODIFY it_material.
    ENDIF.
  ENDLOOP.
  CLEAR it_material.
  DESCRIBE TABLE it_material LINES i_count.
  IF i_count = 0.
    s_no_exist = 'X'.
    g_message = 'No Valid Component Exists!'.
  ENDIF.
ENDFORM.                    " ELEMINAE_DELETED
*&---------------------------------------------------------------------*
*&      Form  CHECK_SORTSTRING
*&---------------------------------------------------------------------*
*       Check the sort string is correct or not. If not,
*       keep the record for update.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_sortstring.
  DATA: l_tabix LIKE sy-tabix.
  DATA: i_count TYPE i.

  IF s_no_exist = 'X'.
    EXIT.
  ENDIF.
  LOOP AT it_material.
    IF ( ( it_material-mtart = c_roh OR
       it_material-mtart = c_roh1 ) AND
       it_material-sortf IS INITIAL ) OR
       ( it_material-mtart = c_halb AND
         NOT it_material-sortf IS INITIAL ).
    ELSE.
      DELETE it_material.
    ENDIF.
  ENDLOOP.
  SORT it_material BY matnr stlnr stlkn idnrk.
  DELETE ADJACENT DUPLICATES FROM it_material
    COMPARING matnr stlnr stlkn idnrk.
  DESCRIBE TABLE it_material LINES i_count.
  IF i_count = 0.
    s_no_exist = 'N'.
    g_message = 'No Wrong Sort String Assignment Found!'.
  ENDIF.
ENDFORM.                    " CHECK_SORTSTRING
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SORTSTRING
*&---------------------------------------------------------------------*
*       Update the wrong sort string using BDC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_sortstring.
  DATA : it_plpo LIKE plpo,
         w_int TYPE i.
  REFRESH it_mess.
  IF s_no_exist = 'X' OR
     s_no_exist = 'N'.
    EXIT.
  ENDIF.
*for test
  DATA: i_count TYPE i.
  DESCRIBE TABLE it_material LINES w_int.
  IF w_int <> 0.
    SORT it_material BY matnr.

    LOOP AT it_material.

      SELECT SINGLE  *  FROM mara
              WHERE matnr EQ it_material-idnrk
                AND ( mtart EQ 'ROH' OR mtart EQ 'ROH1'
                     OR mtart EQ 'HALB' ).
      IF sy-subrc = 0.
        SELECT SINGLE * FROM marc
             WHERE matnr EQ it_material-idnrk
               AND werks EQ it_material-werks.

        IF mara-mtart EQ 'ROH1' AND marc-disgr EQ 'P010'.
          it_material-sortf  = '  '.
        ELSEIF  mara-mtart EQ 'HALB' AND It_material-idnrk(1) EQ 'B'.
          it_material-sortf  = ' '.
        ELSEIF marc-sobsl = '50' .
          it_material-sortf  = ' '.
        ELSE.
          IF marc-vspvb NE space.
*reference rate routing
            SELECT SINGLE plnkn usr01
                 INTO CORRESPONDING FIELDS OF it_plpo
                        FROM plpo
                           WHERE  plnty EQ 'M'
                              AND plnnr EQ 'RP'
                              AND werks EQ it_material-werks
                              AND usr00 EQ marc-vspvb.

            it_material-sortf  = it_plpo-usr01.
          ELSE.
            it_material-sortf  = '18'.
          ENDIF.
        ENDIF.
      ELSE.
        it_material-sortf  = ' '.
      ENDIF.
*      PERFORM fill_data USING it_material.
*      PERFORM call_transaction USING 'CS02' mode.
      MODIFY it_material FROM it_material.
      CLEAR: it_material, it_message,it_mess.
    ENDLOOP.

    CASE sy-ucomm.
      WHEN 'UPD'.
*Update
        REFRESH it_head.
        SORT it_material BY matnr.
        it_head[] = it_material[].
        DELETE ADJACENT DUPLICATES FROM it_head COMPARING matnr.
        LOOP AT it_head.
          PERFORM fill_data_header USING it_head.
          LOOP AT it_material WHERE matnr EQ it_head-matnr.
            PERFORM fill_data_item USING it_material.
          ENDLOOP.
          PERFORM fill_data_save USING it_material.
          PERFORM call_transaction USING 'CS02' mode.
        ENDLOOP.
      WHEN 'UPDI'.
        REFRESH it_head.
        SORT it_material BY matnr.
        it_head[] = it_material[].
        DELETE ADJACENT DUPLICATES FROM it_head COMPARING matnr.
        LOOP AT it_head.
          LOOP AT it_material WHERE matnr EQ it_head-matnr.
            PERFORM update_sortstring_di USING it_material.
          ENDLOOP.
        ENDLOOP.
    ENDCASE.
    sy-lsind = sy-lsind - 1.
    PERFORM write_output.
    PERFORM write_message.
  ENDIF.

ENDFORM.                    " UPDATE_SORTSTRING


*&---------------------------------------------------------------------*
*&      Form  UPDATE_SORTSTRING
*&---------------------------------------------------------------------*
*       Update the wrong sort string using BDC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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
ENDFORM.
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
*&---------------------------------------------------------------------*
*&      Form  FILL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_data USING pw_mat LIKE it_material.
  REFRESH bdcdata. CLEAR :bdcdata.
  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0100'.
  PERFORM bdc_field       USING 'RC29N-MATNR'
                              pw_mat-matnr.
  PERFORM bdc_field       USING 'RC29N-WERKS'
                              pw_mat-werks.
  PERFORM bdc_field       USING 'RC29N-STLAN'
                              pw_mat-stlan.
  PERFORM bdc_field       USING 'RC29N-STLAL'
                              '1'.
  PERFORM bdc_field       USING 'RC29N-AENNR'
                              pw_mat-aennr.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
*  PERFORM bdc_field       USING 'RC29N-DATUV'
*                              '12/03/2004'.
  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0150'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=SETP'.

  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0708'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=CLWI'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RC29P-SELPI'.
  PERFORM bdc_field       USING 'RC29P-SELPI'
                              pw_mat-stlkn.

  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0150'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RC29P-SORTF(01)'.
  PERFORM bdc_field       USING 'RC29P-SORTF(01)'
                              pw_mat-sortf.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=FCBU'.

  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0130'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.

  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0131'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.

  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0138'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.

  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0138'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                            '=EBACK'.

ENDFORM.                    " FILL_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TCODE  text
*----------------------------------------------------------------------*
FORM call_transaction USING    p_tcode mode.
  DATA: l_msgstr(100) TYPE c.
  REFRESH it_message.
  CLEAR it_message.
  CALL TRANSACTION p_tcode
           USING bdcdata
           MODE mode
           UPDATE 'S'
           MESSAGES INTO it_message.
* ckeck the massge
*  PERFORM rkc_msg_string USING l_msgstr.
  IF sy-msgty = 'E'.
    it_mess-msgty = 'E'.
    it_mess-msgtx = l_msgstr.
    APPEND it_mess.
  ELSE.
    COMMIT WORK.
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
           sy-vline NO-GAP, text-020  NO-GAP,
           sy-vline NO-GAP, text-016  NO-GAP,
           sy-vline NO-GAP, text-017  NO-GAP,
           sy-vline NO-GAP, text-018  NO-GAP,
           sy-vline NO-GAP, text-019  NO-GAP,
           sy-vline NO-GAP.
  FORMAT COLOR OFF.
  ULINE.

ENDFORM.                    " WRITE_HEADER
*&---------------------------------------------------------------------*
*&      Form  WRITE_DATA
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
            sy-vline NO-GAP, 21(6) it_material-werks  NO-GAP,
            sy-vline NO-GAP, 31(5) it_material-stlan  NO-GAP,
            sy-vline NO-GAP, 37(8) it_material-posnr  NO-GAP,
            sy-vline NO-GAP, 46(18) it_material-idnrk  NO-GAP,
            sy-vline NO-GAP, 65(6) it_material-suff  NO-GAP,
            sy-vline NO-GAP, 72(6) it_material-mtart  NO-GAP,
            sy-vline NO-GAP, 82(8) it_material-sortf  NO-GAP,
            sy-vline NO-GAP, 91(10) it_material-menge  NO-GAP,
            sy-vline NO-GAP, 102(12) it_material-datuv  NO-GAP,
            sy-vline NO-GAP, 115(12) it_material-datub  NO-GAP,
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
ENDFORM.                    " WRITE_DATA
*&---------------------------------------------------------------------*
*&      Form  WRITE_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_message.
  LOOP AT it_mess.
    WRITE : / it_mess-msgty, it_mess-msgtx.
  ENDLOOP.
ENDFORM.                    " WRITE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  fill_data_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MATERIAL  text
*----------------------------------------------------------------------*
FORM fill_data_header USING   pw_mat LIKE it_material.
  REFRESH bdcdata. CLEAR :bdcdata.
  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0100'.
  PERFORM bdc_field       USING 'RC29N-MATNR'
                              pw_mat-matnr.
  PERFORM bdc_field       USING 'RC29N-WERKS'
                              pw_mat-werks.
  PERFORM bdc_field       USING 'RC29N-STLAN'
                              pw_mat-stlan.
  PERFORM bdc_field       USING 'RC29N-STLAL'
                              '1'.
  PERFORM bdc_field       USING 'RC29N-AENNR'
                              pw_mat-aennr.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
*  PERFORM bdc_field       USING 'RC29N-DATUV'
*                              '12/03/2004'.

ENDFORM.                    " fill_data_HEADER
*&---------------------------------------------------------------------*
*&      Form  fill_data_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MATERIAL  text
*----------------------------------------------------------------------*
FORM fill_data_item USING pw_mat LIKE it_material.

  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0150'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=SETP'.

  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0708'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=CLWI'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RC29P-SELID'.
  PERFORM bdc_field       USING 'RC29P-SELID'
                              pw_mat-idnrk.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                              'RC29P-SELPI'.
*  PERFORM bdc_field       USING 'RC29P-SELPI'
*                              pw_mat-stlkn.

  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0152'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RC29P-SORTF(01)'.
  PERFORM bdc_field       USING 'RC29P-SORTF(01)'
                              pw_mat-sortf.

  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0130'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.

  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0131'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.

  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0138'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.


ENDFORM.                    " fill_data_ITEM
*&---------------------------------------------------------------------*
*&      Form  fill_data_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MATERIAL  text
*----------------------------------------------------------------------*
FORM fill_data_save USING   pw_mat LIKE it_material.
  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0708'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/EABBR'.

  PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=FCBU'.

ENDFORM.                    " fill_data_SAVE
*&---------------------------------------------------------------------*
*&      Form  update_sortstring_di
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MATERIAL  text
*----------------------------------------------------------------------*
FORM update_sortstring_di USING  pw_mat LIKE it_material.

  UPDATE stpo SET sortf = pw_mat-sortf
          WHERE stlty EQ 'M'
            AND stlnr EQ pw_mat-stlnr
            AND stlkn EQ pw_mat-stlkn
            AND stpoz EQ pw_mat-stpoz.

ENDFORM.                    " update_sortstring_di
