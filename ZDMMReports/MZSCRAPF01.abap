*----------------------------------------------------------------------*
***INCLUDE MZSCRAPF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  Display_search_help
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_KATALOGART  text
*----------------------------------------------------------------------*
FORM display_search_help USING    p_katalogart
                                  p_fieldname
                                  p_fieldname1.

  DATA:
       l_tq15t                 LIKE tq15t,
       l_qpk1gr                LIKE qpk1gr,
       l_repid                 LIKE d020s-prog,
       l_dynnr                 LIKE sy-dynnr,
       l_qmgrp                 LIKE qmgrp.

  DATA: i_katalogart TYPE qpgr-katalogart,
        l_qpk1cd          LIKE qpk1cd,
        i_codegruppe LIKE  qpgr-codegruppe,
        i_code LIKE  qpcd-code VALUE '*',
        w_ans(1) TYPE c.

  DATA : t_codegrptab LIKE qpk1codegrp OCCURS 0 WITH HEADER LINE.

  DATA : BEGIN OF l_dynfieldtab OCCURS 10.
          INCLUDE STRUCTURE dynpread.
  DATA : END   OF l_dynfieldtab.

  MOVE : sy-repid TO l_repid,
         sy-dynnr TO l_dynnr.



  IF p_fieldname = 'FEGRP'.


    MOVE 'QMGRP' TO l_dynfieldtab-fieldname.
    APPEND l_dynfieldtab.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname                         = l_repid
        dynumb                         = l_dynnr
*       TRANSLATE_TO_UPPER             = ' '
*       REQUEST                        = ' '
*       PERFORM_CONVERSION_EXITS       = ' '
*       PERFORM_INPUT_CONVERSION       = ' '
*       DETERMINE_LOOP_INDEX           = ' '
      TABLES
        dynpfields                     = l_dynfieldtab
*     EXCEPTIONS
*       INVALID_ABAPWORKAREA           = 1
*       INVALID_DYNPROFIELD            = 2
*       INVALID_DYNPRONAME             = 3
*       INVALID_DYNPRONUMMER           = 4
*       INVALID_REQUEST                = 5
*       NO_FIELDDESCRIPTION            = 6
*       INVALID_PARAMETER              = 7
*       UNDEFIND_ERROR                 = 8
*       DOUBLE_CONVERSION              = 9
*       STEPL_NOT_FOUND                = 10
*       OTHERS                         = 11
              .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


    READ TABLE l_dynfieldtab INDEX 1.
    l_qmgrp = l_dynfieldtab-fieldvalue.


    CLEAR: l_dynfieldtab, l_dynfieldtab[].
** Changed by Furong on 12/20/10
    IF riwo00-qmart = 'Q4'.
      t_codegrptab = '0'.
      APPEND t_codegrptab.
    ELSE.
** Changed by Furong on 08/10/10
** Changed by Furong on 12/13/11
*      IF MARC-WERKS = 'E001'.
      IF marc-werks = 'E001' OR marc-werks = 'E002'.
** End of change
        t_codegrptab = '0'.
        APPEND t_codegrptab.
        t_codegrptab = '5'.
        APPEND t_codegrptab.
        t_codegrptab = '7'.
        APPEND t_codegrptab.
        t_codegrptab = '9'.
        APPEND t_codegrptab.
      ELSE.
        IF l_qmgrp = 'MXTX10' OR l_qmgrp = 'MXTX11' OR
           l_qmgrp = 'MXTX12' OR l_qmgrp = 'MXTX13' OR
           l_qmgrp = 'MXTX15' OR l_qmgrp = 'MXTX19' OR
           l_qmgrp = 'MXTX51' OR l_qmgrp = 'MXTX53'.

          t_codegrptab = '0'.
          APPEND t_codegrptab.
          t_codegrptab = '4'.
          APPEND t_codegrptab.
          t_codegrptab = '9'.
          APPEND t_codegrptab.
** Chnaged by Furong on 10/19/09
        ELSEIF l_qmgrp+0(4) = 'MXPX'.
          t_codegrptab = '0'.
          APPEND t_codegrptab.
          t_codegrptab = '1'.
          APPEND t_codegrptab.
          t_codegrptab = '2'.
          APPEND t_codegrptab.
          t_codegrptab = '3'.
          APPEND t_codegrptab.
          t_codegrptab = '9'.
          APPEND t_codegrptab.
** End of change
        ELSE.

          t_codegrptab = '*'.
          APPEND t_codegrptab.

        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR : l_qmgrp.

  ELSE.

    t_codegrptab = '*'.
    APPEND t_codegrptab.

  ENDIF.

  i_katalogart = p_katalogart.

  CALL FUNCTION 'QPK1_GP_CODE_PICKUP'
    EXPORTING
      i_katalogart                 = i_katalogart
      i_codegruppe                 = i_codegruppe
      i_code                       = i_code
      i_sprache                    = sy-langu
      i_winx1                      = 10
      i_winx2                      = 68
      i_winy1                      = 5
      i_winy2                      = 27
*   I_DISPLAY_MODE               =
*   I_RETURN_IF_ONE              = 'X'
*   I_RETURN_IF_MANY             =
*   I_NO_USAGEINDICATION         =
*   I_NO_AUTHORITY_CHECK         =
    IMPORTING
      e_qpk1cd                     = l_qpk1cd
    TABLES
      t_codegrptab                 = t_codegrptab
* EXCEPTIONS
*   NO_MATCH_IN_RANGE            = 1
*   NO_USER_SELECTION            = 2
*   NO_AUTHORIZATION             = 3
*   NO_SELECTION_SPECIFIED       = 4
*   OBJECT_LOCKED                = 5
*   LOCK_ERROR                   = 6
*   OBJECT_MISSING               = 7
*   OTHERS                       = 8
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  MOVE p_fieldname TO l_dynfieldtab-fieldname.
  MOVE  l_qpk1cd-codegruppe TO l_dynfieldtab-fieldvalue.
  APPEND l_dynfieldtab.

  MOVE p_fieldname1 TO l_dynfieldtab-fieldname.
  MOVE  l_qpk1cd-code TO l_dynfieldtab-fieldvalue.
  APPEND l_dynfieldtab.

  IF p_fieldname = 'QMGRP'.

    MOVE  'OTGRP' TO l_dynfieldtab-fieldname.
    MOVE  l_qpk1cd-codegruppe TO l_dynfieldtab-fieldvalue.
    APPEND l_dynfieldtab.

    MOVE  'OTEIL' TO l_dynfieldtab-fieldname.
    MOVE  l_qpk1cd-code TO l_dynfieldtab-fieldvalue.
    APPEND l_dynfieldtab.

  ENDIF.

  IF p_fieldname = 'FEGRP' AND l_qpk1cd-codegruppe = '0'.

    MOVE 'URGRP' TO l_dynfieldtab-fieldname.
    MOVE  'CAUS' TO l_dynfieldtab-fieldvalue.
    APPEND l_dynfieldtab.

    MOVE 'URCOD' TO l_dynfieldtab-fieldname.
    MOVE  '04' TO l_dynfieldtab-fieldvalue.
    APPEND l_dynfieldtab.

    LOOP AT SCREEN.
      IF screen-name = 'URGRP'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSEIF p_fieldname = 'FEGRP' AND l_qpk1cd-codegruppe = '4'.

    MOVE 'URGRP' TO l_dynfieldtab-fieldname.
    MOVE  'CAUS' TO l_dynfieldtab-fieldvalue.
    APPEND l_dynfieldtab.

    MOVE 'URCOD' TO l_dynfieldtab-fieldname.
    MOVE  '02' TO l_dynfieldtab-fieldvalue.
    APPEND l_dynfieldtab.

    LOOP AT SCREEN.
      IF screen-name = 'URGRP'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSEIF p_fieldname = 'FEGRP' AND l_qpk1cd-codegruppe = '5'.

    MOVE 'URGRP' TO l_dynfieldtab-fieldname.
    MOVE  'CAUS' TO l_dynfieldtab-fieldvalue.
    APPEND l_dynfieldtab.

    MOVE 'URCOD' TO l_dynfieldtab-fieldname.
    MOVE  '02' TO l_dynfieldtab-fieldvalue.
    APPEND l_dynfieldtab.

    LOOP AT SCREEN.
      IF screen-name = 'URGRP'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = l_repid
      dynumb               = l_dynnr
    TABLES
      dynpfields           = l_dynfieldtab
    EXCEPTIONS
      invalid_abapworkarea = 01
      invalid_dynprofield  = 02
      invalid_dynproname   = 03
      invalid_dynpronummer = 04
      invalid_request      = 05
      no_fielddescription  = 06
      undefind_error       = 07.

  IF sy-subrc <> 0.
    MESSAGE s099(q3) .
    EXIT.
  ENDIF.

ENDFORM.                    " Display_search_help
*&---------------------------------------------------------------------*
*&      Form  process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process.

  CLEAR eflag.

  DATA: v_lgtyp LIKE pkhd-lgtyp,
        v_lgpla LIKE pkhd-lgpla,
        v_vspvb LIKE marc-vspvb,
        v_lgber LIKE lagp-lgber,
        v_verme LIKE lqua-verme,
        v_negat TYPE t331-negat,
        v_qmnum LIKE qmel-qmnum,
        v_ltkze LIKE mlgn-ltkze.

  DATA: v_sub(20) TYPE c.

  DATA: it_tline TYPE tline OCCURS 0 WITH HEADER LINE.

  CLEAR: it_tline, it_tline[].

  PERFORM code_check.

  IF w_call <> 'SSRW'.

    it_tline-tdformat = '*' .
    CONCATENATE 'Plant' marc-werks INTO it_tline-tdline.
    APPEND it_tline.
    CLEAR it_tline.

    it_tline-tdformat = '*' .
    CONCATENATE 'Storage Location' t001l-lgort INTO it_tline-tdline.
    APPEND it_tline.
    CLEAR it_tline.

    IF t001l-lgort = 'P400'.

      PERFORM creat_mvt TABLES it_tline.   " creation of movement


      SELECT SINGLE vspvb INTO v_vspvb FROM marc
                                       WHERE matnr = matnr
                                         AND werks = marc-werks.

      SELECT SINGLE lgtyp lgpla  INTO (v_lgtyp, v_lgpla) FROM pkhd
                                 WHERE matnr = matnr
                                   AND werks = marc-werks
                                   AND prvbe = v_vspvb.

      IF ( v_lgtyp IS INITIAL AND v_lgpla IS INITIAL ).
** Changed by Furong on 06/21/10
*      MOVE 'INV_COUNT' TO V_LGPLA.
        MOVE 'HMMA ADJ' TO v_lgpla.
** End of change
        MOVE '999' TO v_lgtyp.

      ELSE.

        SELECT SINGLE lgber INTO v_lgber FROM lagp
                                         WHERE lgnum = rmmg1-lgnum
                                           AND lgtyp = v_lgtyp
                                           AND lgpla = v_lgpla.


        SELECT SINGLE verme INTO v_verme FROM lqua
                              WHERE lgnum = rmmg1-lgnum
                                AND matnr = matnr
                                AND lgtyp = v_lgtyp
                                AND lgpla = v_lgpla.

        IF v_verme < rkmng.

          SELECT SINGLE negat FROM t331 INTO v_negat
                      WHERE lgnum = rmmg1-lgnum AND lgtyp = v_lgtyp.

          IF v_negat = 'X'.

          ELSE.

*          MOVE 'INV_COUNT' TO V_LGPLA.
            MOVE 'HMMA ADJ' TO v_lgpla.
            MOVE '999' TO v_lgtyp.
          ENDIF.

        ENDIF.

      ENDIF.

      IF eflag IS INITIAL.

*S__BY PAUL
*      PERFORM CREATE_TO TABLES IT_TLINE     " creation of transfer
*order
*                                  USING V_VSPVB V_LGTYP V_LGPLA V_LGBER
*.

      ENDIF.

** Changed by Furong on 10/19/09
*  ELSEIF T001L-LGORT = 'P500'.
    ELSE.
** End of change on 10/19/09

      PERFORM creat_mvt TABLES it_tline.     " creation of movement

    ENDIF.


    CLEAR eflag.

** changed on 12/13/11
*    IF MARC-WERKS <> 'E001'.
    IF marc-werks = 'P001'.
** End on 12/13/11
      IF lfa1-lifnr IS INITIAL.
        it_tline-tdline = 'Vendor Not available'.
        APPEND it_tline.
        CLEAR it_tline.
      ENDIF.
    ENDIF.

    PERFORM creat_notif.                " creation of notification

*  IF NOT LFA1-LIFNR IS INITIAL.
*
*    PERFORM CREAT_NOTIF.                  " creation of notification
*
*  ELSE.
*    IF MARC-WERKS <> 'E001'.
*      IT_TLINE-TDLINE = 'Vendor Not available'.
*      APPEND IT_TLINE.
*      CLEAR IT_TLINE.
*    ENDIF.
*    PERFORM CREAT_NOTIF1.                " creation of notification
*
*  ENDIF.


    READ TABLE it_qmnum INDEX 1.
    v_qmnum = it_qmnum-qmnum.

    PERFORM update_notif_text TABLES it_tline    " updating QN# long text
                                       USING v_qmnum.
    READ TABLE it_tline INDEX 3.
    CLEAR: w_print_error.
    IF it_tline-tdline+0(5) = 'ERROR'.
      PERFORM update_notif_status USING v_qmnum. " updating QN# status
      w_print_error = 'X'.
      READ TABLE it_qmnum INDEX 1.
      it_qmnum-flag = 'X'.
      MODIFY it_qmnum INDEX 1.
    ELSE.
      READ TABLE it_tline INDEX 4.
      IF it_tline-tdline+0(5) = 'ERROR'.
        PERFORM update_notif_status USING v_qmnum.  " updating QN# status
        w_print_error = 'X'.
        READ TABLE it_qmnum INDEX 1.
        it_qmnum-flag = 'X'.
        MODIFY it_qmnum INDEX 1.
      ELSE.
** Changed by Furong on 03/03/09
*        IF FEGRP = '4' AND URGRP <> '04'.
*
*          CLEAR: IT_TLINE, IT_TLINE[].
*          PERFORM UPDATE_NOPR_HMMA TABLES IT_TLINE
*                                   USING V_QMNUM.
*          WAIT UP TO 2 SECONDS.
*     PERFORM UPDATE_NOTIF_TEXT TABLES IT_TLINE   "updating QN# long text
*                                                          USING V_QMNUM.
*        ENDIF.
        IF marc-werks = 'P001'.
          IF fegrp = '4' AND urgrp <> '04'.

            CLEAR: it_tline, it_tline[].
            PERFORM update_nopr_hmma TABLES it_tline
                                     USING v_qmnum.
            WAIT UP TO 2 SECONDS.
            PERFORM update_notif_text TABLES it_tline   "updating QN# long text
                                       USING v_qmnum.
          ENDIF.
        ELSE.
          IF urgrp = '04'.
          ELSE.
** Furong on 08/13/12
            IF fecod+0(1) = '5' OR fecod+0(1) = '7'.
              IF fecod <> '5003' OR fecod = '7003'.

**           IF FECOD = '7002' OR
**            FECOD = '7004' OR
**            FECOD = '7005' OR
**            FECOD = '7006' OR
**            FECOD = '7007' OR
**            FECOD = '7008'.
** End on 08/13/12
                CLEAR: it_tline, it_tline[].
                PERFORM update_nopr_hmma TABLES it_tline
                                         USING v_qmnum.
                WAIT UP TO 2 SECONDS.
                PERFORM update_notif_text TABLES it_tline   "updating QN# long text
                 USING v_qmnum.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
** End of change
      ENDIF.
    ENDIF.

** Changed by Furong on 09/16/09
    IF w_ans = '1'.
      PERFORM set_reorder USING v_qmnum.
      CLEAR: w_ans.
    ENDIF.
** Changed by Furong on 11/04/09
*  QMNUM = V_QMNUM.
*  PERFORM UPDATE_CAUSE_TEXT.
** End of change on 11/04/09
** end of change

** END OF CHANGE ON 12/20/10
********  Printing Label ********
    PERFORM print_label.
  ELSE.
    PERFORM creat_notif.                " creation of notification
    READ TABLE it_qmnum INDEX 1.
    v_qmnum = it_qmnum-qmnum.
    mdoc = v_qmnum.
    p_qmnum = v_qmnum.
    PERFORM print_label_1.

  ENDIF.
*****   Send mail logic  ********

** Changed by Furong on 05/27/09

*  IF T001L-LGORT = 'P400'.
*
*    IF V_LGTYP = '422'.
*
*      SELECT SINGLE LTKZE FROM MLGN INTO V_LTKZE
*                        WHERE MATNR = MATNR AND LGNUM = RMMG1-LGNUM.
*      IF V_LTKZE = '003'.
*
*        IF QMGRP = 'MXTX53'.
*
*          V_SUB = 'PDI reorder'.
*
*          PERFORM SEND_MAIL USING V_SUB.
*
*        ELSE.
*
*          V_SUB = 'Line side Reorder'.
*
*          PERFORM SEND_MAIL USING V_SUB.
*        ENDIF.
*      ENDIF.
*
*    ENDIF.
*
*  ELSEIF T001L-LGORT = 'P500'.
*
*    IF QMGRP = 'MXTX53'.
*
*      V_SUB = 'PDI reorder'.
*
*      PERFORM SEND_MAIL USING V_SUB.
*
*    ELSE.
*
*      V_SUB = 'Line side Reorder'.
*
*      PERFORM SEND_MAIL USING V_SUB.
*    ENDIF.
*
*  ENDIF.
** End of change

  CLEAR: v_lgtyp, v_lgpla, v_vspvb, v_lgber, v_sub, v_verme,
         v_qmnum,v_ltkze,v_qmnum,v_negat,it_tline, it_tline[].

*  IF SY-UNAME <> '101457'  or SY-UNAME <> '100794'.
*    CALL 'SYST_LOGOFF'.
*  ELSE.
*    LEAVE PROGRAM.
*  ENDIF.
ENDFORM.                    " process
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0210   text
*      -->P_0211   text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING    program dynpro.

  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.

ENDFORM.                    " bdc_dynpro
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0305   text
*      -->P_0306   text
*----------------------------------------------------------------------*
FORM bdc_field USING    fnam fval.

  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.

ENDFORM.                    " bdc_field
*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0315   text
*----------------------------------------------------------------------*
FORM bdc_transaction TABLES  p_p_it_tline  STRUCTURE  tline
                     USING    tcode.

  DATA: l_subrc LIKE sy-subrc,
        msg(255).

* call transaction using

  REFRESH: messtab.
  CLEAR : msg,mdoc, myear.
  CALL TRANSACTION tcode USING bdcdata
                   MODE   ctumode
                   UPDATE cupdate
                   MESSAGES INTO messtab.
  l_subrc = sy-subrc.

  READ TABLE messtab WITH KEY msgtyp = 'E'.

  IF sy-subrc = 0.
    LOOP AT messtab.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH messtab-msgv1 messtab-msgv2 messtab-msgv3
                 messtab-msgv4.

      CONCATENATE msg messtab-msgv1 INTO msg.
    ENDLOOP.
    p_p_it_tline-tdformat = '*' .
    CONCATENATE 'ERROR: ' msg INTO p_p_it_tline-tdline.
    APPEND p_p_it_tline.
    CLEAR p_p_it_tline.
    eflag = 'X'.

  ELSE.
    LOOP AT messtab.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH messtab-msgv1 messtab-msgv2 messtab-msgv3
                 messtab-msgv4.

      CONCATENATE msg messtab-msgv1 INTO msg.
    ENDLOOP.
    REFRESH bdcdata.
** Changed by Furong on 03/06/09
    IF tcode <> 'QM02'.
      p_p_it_tline-tdformat = '*' .
      CONCATENATE 'TRORDER' msg INTO p_p_it_tline-tdline.
      APPEND p_p_it_tline.
      CLEAR p_p_it_tline.
    ENDIF.
  ENDIF.
** End of change
  REFRESH: bdcdata.
ENDFORM.                    " bdc_transaction
*&---------------------------------------------------------------------*
*&      Form  creat_mvt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM creat_mvt TABLES   p_it_tline STRUCTURE tline.


  DATA : it_header LIKE bapi2017_gm_head_01,
         code LIKE bapi2017_gm_code VALUE '04',
         v_uom LIKE mara-meins.

  DATA: it_item LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE,
        it_ret LIKE bapiret2 OCCURS 0 WITH HEADER LINE.


  SELECT SINGLE meins FROM mara INTO v_uom WHERE matnr = matnr.

  it_header-pstng_date  = sy-datum.
  it_header-doc_date     = sy-datum.
  it_item-material      = matnr.
  it_item-plant         = marc-werks.
  it_item-stge_loc      = t001l-lgort.
  it_item-move_type     = bwart.
  it_item-entry_qnt     = rkmng.
  it_item-entry_uom     = v_uom.
  it_item-move_plant    = marc-werks.
  it_item-move_stloc    = rmmg1-lgort.
  it_item-move_reas     = fecod. "03.27.2014 Victor added
  it_item-mvt_ind       = ' '.
** Changed by Furong on 10/19/09
  IF marc-xchar = 'X'.
    it_item-batch  = mseg-charg.
  ENDIF.
** End of change

  APPEND it_item.
  CLEAR it_item.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header       = it_header
      goodsmvt_code         = code
*     TESTRUN               = ' '
    IMPORTING
*     GOODSMVT_HEADRET      =
      materialdocument      = mdoc
      matdocumentyear       = myear
    TABLES
      goodsmvt_item         = it_item
*     GOODSMVT_SERIALNUMBER =
      return                = it_ret.


  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
             wait          = 'X'
*           IMPORTING
*             RETURN        =
            .


  IF mdoc IS INITIAL.
    p_it_tline-tdformat = '*' .
    CONCATENATE 'ERROR: ' it_ret-message INTO p_it_tline-tdline.
    APPEND p_it_tline.
    CLEAR p_it_tline.
    eflag = 'X'.
  ELSE.
    p_it_tline-tdformat = '*' .
    CONCATENATE '@1 Material DOC:' mdoc myear INTO p_it_tline-tdline
           SEPARATED BY space.
    APPEND p_it_tline.
    CLEAR p_it_tline.
  ENDIF.

  CLEAR v_uom.

ENDFORM.                    " creat_mvt
*&---------------------------------------------------------------------*
*&      Form  find_part
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_part.

  CLEAR lfa1-name1.
  DATA : v_vspvb1 LIKE marc-vspvb,
         v_vspvb2 LIKE marc-vspvb,
         v_ast(1) VALUE '*'.

  DATA : dyname LIKE d020s-prog , dynumb LIKE d020s-dnum .
  DATA: BEGIN OF it_dynpfields OCCURS 3.
          INCLUDE STRUCTURE dynpread.
  DATA: END OF it_dynpfields.


  DATA: ret_tab TYPE ddshretval OCCURS 0 WITH HEADER LINE,
        fld_tab TYPE dfies OCCURS 0 WITH HEADER LINE,
        it_dsel TYPE dselc OCCURS 0 WITH HEADER LINE.

  DATA: v_desc TYPE makt-maktg,
        v_lgpbe TYPE mard-lgpbe,
        v_werks TYPE marc-werks,
        v_lifnr TYPE eord-lifnr,
        v_mfrpn TYPE mara-mfrpn,
        v_vspvb TYPE marc-vspvb,
        v_name LIKE lfa1-name1,
        v_combd TYPE c,
        v_combp TYPE c.

  DATA: BEGIN OF it_marc OCCURS 0,
        matnr TYPE marc-matnr,
        werks TYPE marc-werks,
        vspvb TYPE marc-vspvb,
        END OF it_marc.

  DATA: BEGIN OF it_makt OCCURS 0,
        matnr TYPE mara-matnr,
        maktg TYPE makt-maktg,
        END OF it_makt.

  DATA: BEGIN OF it_mard OCCURS 0,
        matnr TYPE mard-matnr,
        werks TYPE mard-werks,
        lgpbe TYPE mard-lgpbe,
        END OF it_mard.

  DATA: BEGIN OF it_eord OCCURS 0,
        matnr TYPE eord-matnr,
        werks TYPE eord-werks,
        zeord TYPE eord-zeord,
        lifnr TYPE eord-lifnr,
        END OF it_eord.

  DATA: BEGIN OF it_mara OCCURS 0,
        matnr TYPE mara-matnr,
        mfrpn TYPE mara-mfrpn,
        END OF it_mara.

  DATA: it_output LIKE zscrap_str OCCURS 0 WITH HEADER LINE,
        val_tab LIKE zscrap_str OCCURS 0 WITH HEADER LINE.

  dyname = 'SAPMZSCRAP'.
  dynumb = sy-dynnr.

*  DYNUMB = '1001'.

  MOVE 'MARC-WERKS' TO it_dynpfields-fieldname.
  APPEND it_dynpfields.

  MOVE 'MATNR' TO it_dynpfields-fieldname.
  APPEND it_dynpfields.

  MOVE 'MAKT-MAKTG' TO it_dynpfields-fieldname.
  APPEND it_dynpfields.

  MOVE 'MARD-LGPBE' TO it_dynpfields-fieldname.
  APPEND it_dynpfields.

  MOVE 'LFA1-LIFNR' TO it_dynpfields-fieldname.
  APPEND it_dynpfields.

  MOVE 'MARA-MFRPN' TO it_dynpfields-fieldname.
  APPEND it_dynpfields.

  MOVE 'MARC-VSPVB' TO it_dynpfields-fieldname.
  APPEND it_dynpfields.


  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname                         = dyname
      dynumb                         = dynumb
    translate_to_upper               = 'X'
*   REQUEST                        = ' '
*   PERFORM_CONVERSION_EXITS       = ' '
*   PERFORM_INPUT_CONVERSION       = ' '
*   DETERMINE_LOOP_INDEX           = ' '
    TABLES
      dynpfields                     = it_dynpfields
* EXCEPTIONS
*   INVALID_ABAPWORKAREA           = 1
*   INVALID_DYNPROFIELD            = 2
*   INVALID_DYNPRONAME             = 3
*   INVALID_DYNPRONUMMER           = 4
*   INVALID_REQUEST                = 5
*   NO_FIELDDESCRIPTION            = 6
*   INVALID_PARAMETER              = 7
*   UNDEFIND_ERROR                 = 8
*   DOUBLE_CONVERSION              = 9
*   STEPL_NOT_FOUND                = 10
*   OTHERS                         = 11
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  LOOP AT it_dynpfields.

    IF it_dynpfields-fieldname = 'MARC-WERKS'.
      v_werks = it_dynpfields-fieldvalue.
    ELSEIF it_dynpfields-fieldname = 'MATNR'.
      v_matnr = it_dynpfields-fieldvalue.
    ELSEIF it_dynpfields-fieldname = 'MAKT-MAKTG'.
      v_desc = it_dynpfields-fieldvalue.
    ELSEIF it_dynpfields-fieldname = 'MARD-LGPBE'.
      v_lgpbe = it_dynpfields-fieldvalue.
    ELSEIF it_dynpfields-fieldname = 'LFA1-LIFNR'.
      v_lifnr = it_dynpfields-fieldvalue.
    ELSEIF it_dynpfields-fieldname = 'MARA-MFRPN'.
      v_mfrpn = it_dynpfields-fieldvalue.
    ELSEIF it_dynpfields-fieldname = 'MARC-VSPVB'.
      v_vspvb = it_dynpfields-fieldvalue.
    ENDIF.

  ENDLOOP.


*********** Finding part w.r.t Plant *******************

  IF NOT v_werks IS INITIAL.

** Changed by Furong on 10/26/09
*    IF MARA-MTART = 'HALB' OR MARA-MTART = 'ROH1'.
*      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_OUTPUT
*    FROM ZSCRAP_HALB AS A
*    INNER JOIN MARA AS B
*    ON A~MATNR = B~MATNR
*    WHERE WERKS = V_WERKS.
*    ELSE.
** End of change


** Changed by Furong on 07/14/11

    IF sy-uname+0(1) = 'P'.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_output
         FROM zscrap_halb AS a
         INNER JOIN mara AS b
         ON a~matnr = b~matnr
         WHERE werks = v_werks
          AND ( lgort = 'G100' OR lgort = 'G150' ).
    ELSE.
** Changed by Furong on 08/06/09
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_output
      FROM zscrap_halb AS a
      INNER JOIN mara AS b
      ON a~matnr = b~matnr
      WHERE werks = v_werks.

** Changed by Furong on 10/19/09
*       and ( LGORT = 'P400' OR LGORT = 'P500' )
*      AND B~MTART <> 'HALB'.
** end of change on 10/19/09
** end of change on 08/06/09
*    ENDIF.

      LOOP AT it_output.
        IF it_output-lgort = '9999' OR
           it_output-lgort = 'F001' OR
           it_output-lgort = 'P010' OR
           it_output-lgort = 'P499' OR
           it_output-lgort = 'P600' OR
           it_output-lgort = 'P610' OR
           it_output-lgort = 'P620' OR
           it_output-lgort = 'P630' OR
           it_output-lgort = 'P640' OR
           it_output-lgort = 'P690' OR
           it_output-lgort = 'P699' OR
           it_output-lgort = 'P800' OR
           it_output-lgort = 'V001' OR
           it_output-lgort = 'PK10' OR
           it_output-lgort = 'X551' OR
           it_output-lgort = 'X905' OR
           it_output-lgort = 'P999' OR
           it_output-lgort = 'XMIT' OR
           it_output-lgort = 'P998' OR
           it_output-lgort = 'G999' OR
           it_output-lgort = 'G998' OR
           it_output-lgort = 'G100' OR
           it_output-lgort = 'G150'.
          DELETE it_output.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT it_output.

    DELETE ADJACENT DUPLICATES FROM it_output.

************ Finding part w.r.t combination of Material, Description ***
*************                                    and production line ***
    IF  NOT v_matnr IS INITIAL.

      DO.

        REPLACE '*' WITH '%' INTO v_matnr.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

      ENDDO.
** Changed by Furong on 08/06/09
*      SELECT * FROM ZSCRAP INTO CORRESPONDING FIELDS OF TABLE IT_OUTPUT
*                               WHERE WERKS = MARC-WERKS
*                               AND MATNR LIKE V_MATNR
*                               AND ( LGORT = 'P400' OR LGORT = 'P500' )
*.
** Changed by Furong on 10/26/09
*    IF MARA-MTART = 'HALB' OR MARA-MTART = 'ROH1'.
*        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_OUTPUT
*          FROM ZSCRAP_halb AS A
*          INNER JOIN MARA AS B
*            ON A~MATNR = B~MATNR
*            WHERE WERKS = MARC-WERKS
*              AND A~MATNR LIKE V_MATNR.
*     else.
** End of change on 10/26/09

      CASE sy-uname+0(1).
        WHEN 'P' OR 'p'.
          SELECT * INTO CORRESPONDING FIELDS OF TABLE it_output
                FROM zscrap_halb AS a
                INNER JOIN mara AS b
                  ON a~matnr = b~matnr
                  WHERE werks = marc-werks
                    AND a~matnr LIKE v_matnr
                    AND ( lgort = 'G100' OR lgort = 'G150' ).

        WHEN OTHERS.
          SELECT * INTO CORRESPONDING FIELDS OF TABLE it_output
              FROM zscrap_halb AS a
              INNER JOIN mara AS b
                ON a~matnr = b~matnr
                WHERE werks = marc-werks
                  AND a~matnr LIKE v_matnr.
*              AND ( LGORT = 'P400' OR LGORT = 'P500' )
*              AND B~MTART <> 'HALB' .
** end of change
*      endif.
          DELETE ADJACENT DUPLICATES FROM it_output.
** Changed by Furong on 08/11/10
*      IF SY-TCODE = 'ZSCRAP1_ENG'.

** On 09/20/13 by Furong
          LOOP AT it_output.
            val_tab = it_output.
            APPEND val_tab.
          ENDLOOP.


*          CASE marc-werks.
*            WHEN 'E001'.
*              LOOP AT it_output.
*
*                IF it_output-lgort = 'E010' OR
*                        it_output-lgort = 'E110' OR
*                        it_output-lgort = 'E115' OR
*                        it_output-lgort = 'E119' OR
*                        it_output-lgort = 'E120' OR
*                        it_output-lgort = 'E125' OR
*                        it_output-lgort = 'E128' OR
*                        it_output-lgort = 'E301' OR
*                        it_output-lgort = 'E302' OR
*                        it_output-lgort = 'E303' OR
*                        it_output-lgort = 'E999' OR
*                        it_output-lgort = 'E998' OR
*                        it_output-lgort = 'EL50' OR
*                        it_output-lgort = 'X551' OR
*                        it_output-lgort = 'E499'.
*                ELSE.
*                  val_tab = it_output.
*                  SHIFT val_tab-lgpbe UP TO '*' .
*                  SHIFT val_tab-lgpbe.
*                  APPEND val_tab.
*                  CLEAR val_tab.
*                ENDIF.
*              ENDLOOP.
*            WHEN 'E002'.
*
*              LOOP AT it_output.
*
*                IF it_output-lgort = 'N010' OR
*                        it_output-lgort = 'N110' OR
*                        it_output-lgort = 'N115' OR
*                        it_output-lgort = 'N119' OR
*                        it_output-lgort = 'N120' OR
*                        it_output-lgort = 'N125' OR
*                        it_output-lgort = 'N128' OR
*                        it_output-lgort = 'N301' OR
*                        it_output-lgort = 'N302' OR
*                        it_output-lgort = 'N303' OR
*                        it_output-lgort = 'N999' OR
*                        it_output-lgort = 'N998' OR
*                        it_output-lgort = 'NL50' OR
*                        it_output-lgort = 'N551' OR
*** On 03/05/13
*                        it_output-lgort = 'X551' OR
*** end on 03/05/13
*                        it_output-lgort = 'N499'.
*                ELSE.
*                  val_tab = it_output.
*                  SHIFT val_tab-lgpbe UP TO '*' .
*                  SHIFT val_tab-lgpbe.
*                  APPEND val_tab.
*                  CLEAR val_tab.
*                ENDIF.
*              ENDLOOP.
*
*            WHEN 'P001'.
*              LOOP AT it_output.
*                IF it_output-lgort = '9999' OR
*                   it_output-lgort = 'F001' OR
*                   it_output-lgort = 'P010' OR
*                   it_output-lgort = 'P499' OR
*                   it_output-lgort = 'P600' OR
*                   it_output-lgort = 'P610' OR
*                   it_output-lgort = 'P620' OR
*                   it_output-lgort = 'P630' OR
*                   it_output-lgort = 'P640' OR
*                   it_output-lgort = 'P690' OR
*                   it_output-lgort = 'P699' OR
*                   it_output-lgort = 'P800' OR
*                   it_output-lgort = 'V001' OR
*                   it_output-lgort = 'PK10' OR
*                   it_output-lgort = 'X551' OR
*                   it_output-lgort = 'X905' OR
*                   it_output-lgort = 'P128' OR
*                   it_output-lgort = 'P230' OR
*                   it_output-lgort = 'P999' OR
*                   it_output-lgort = 'P998' OR
*                   it_output-lgort = 'G999' OR
*                   it_output-lgort = 'G998' OR
*                   it_output-lgort = 'G100' OR
*                   it_output-lgort = 'G150' OR
*                   it_output-lgort = 'XMIT'.
*                ELSE.
*                  val_tab = it_output.
*                  SHIFT val_tab-lgpbe UP TO '*' .
*                  SHIFT val_tab-lgpbe.
*                  APPEND val_tab.
*                  CLEAR val_tab.
*                ENDIF.
*              ENDLOOP.
*
*          ENDCASE.
** End on 09/20/13
      ENDCASE.

      IF  NOT v_desc IS INITIAL.    " Description available

        v_combd = 'X' .

** Changed by Furong on 01/07/09
*        DO.
*
*          REPLACE '*' WITH '%' INTO v_desc.
*          IF sy-subrc <> 0.
*            EXIT.
*          ENDIF.
*
*        ENDDO.

        DO.

          REPLACE '*' WITH '' INTO v_desc.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

        ENDDO.
        CONCATENATE '%'  v_desc '%' INTO  v_desc.
** End of change

** Furong on 10/28/11 Performance {
        REFRESH: it_makt.
        IF NOT it_output[] IS INITIAL.
** } on 10/28/11
          SELECT matnr maktg INTO CORRESPONDING FIELDS OF TABLE it_makt
                                  FROM makt
                                  FOR ALL ENTRIES IN it_output
                                  WHERE matnr = it_output-matnr
                                    AND maktg LIKE v_desc.
** Furong on 10/28/11 Performance {
        ENDIF.
** } on 10/28/11
        IF NOT val_tab[] IS INITIAL.

          LOOP AT it_makt.
            READ TABLE it_output WITH KEY matnr = it_makt-matnr
                                               BINARY SEARCH.
            IF sy-subrc = 0.
              CONTINUE.
            ELSE.
              DELETE val_tab WHERE matnr = it_makt-matnr.
            ENDIF.
          ENDLOOP.

        ELSE.
          LOOP AT it_makt.
            READ TABLE it_output WITH KEY matnr = it_makt-matnr
                                               BINARY SEARCH.
            IF sy-subrc = 0.
              val_tab = it_output.
              SHIFT val_tab-lgpbe UP TO '*' .
              SHIFT val_tab-lgpbe.
              APPEND val_tab.
              CLEAR val_tab.
            ENDIF.
          ENDLOOP.

*          LOOP AT it_output.
*            READ TABLE it_makt WITH KEY matnr = it_makt-matnr.
*            IF sy-subrc = 0.
*              val_tab = it_output.
*              SHIFT val_tab-lgpbe UP TO '*' .
*              SHIFT val_tab-lgpbe.
*              APPEND val_tab.
*              CLEAR val_tab.
*            ENDIF.
*          ENDLOOP.
        ENDIF.

      ENDIF.

      IF NOT v_vspvb IS INITIAL.      " Prdocution line available

        v_combp = 'X' .

        CLEAR: v_vspvb1, v_vspvb2.
        v_vspvb1 = v_vspvb2 = v_vspvb.

        SHIFT v_vspvb1 LEFT  DELETING LEADING  v_ast.
        SHIFT v_vspvb2 RIGHT DELETING TRAILING v_ast.

        IF NOT val_tab[] IS INITIAL.

          LOOP AT val_tab.
            IF val_tab-vspvb CP v_vspvb OR val_tab-vspvb CP v_vspvb1
               OR val_tab-vspvb CP v_vspvb2.
              CONTINUE.
            ELSE.
              DELETE val_tab.
            ENDIF.
          ENDLOOP.

        ELSE.
          LOOP AT it_output.
            IF it_output-vspvb CP v_vspvb OR
               it_output-vspvb CP v_vspvb1 OR
               it_output-vspvb CP v_vspvb2.
              MOVE-CORRESPONDING it_output TO val_tab.
              IF NOT val_tab-lgpbe IS INITIAL.
                SHIFT val_tab-lgpbe UP TO '*'.
                SHIFT val_tab-lgpbe.
              ENDIF.
              APPEND val_tab.
              CLEAR val_tab.
            ENDIF.
          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDIF.


*****************  Finding part w.r.t Material Description ************

    IF  NOT v_desc IS INITIAL.

      IF v_combd <> 'X'.

** Changed by Furong on 01/07/09
*        DO.
*
*          REPLACE '*' WITH '%' INTO v_desc.
*          IF sy-subrc <> 0.
*            EXIT.
*          ENDIF.
*
*        ENDDO.

        DO.

          REPLACE '*' WITH '' INTO v_desc.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

        ENDDO.
        CONCATENATE '%'  v_desc '%' INTO  v_desc.
** End of change
** Furong on 10/28/11 Performance {
        REFRESH: it_makt.
        IF it_output[] IS INITIAL.
        ELSE.
** } on 10/31/11

          SELECT matnr maktg INTO CORRESPONDING FIELDS OF TABLE it_makt
                                  FROM makt
                                  FOR ALL ENTRIES IN it_output
                                  WHERE matnr = it_output-matnr
                                    AND maktg LIKE v_desc.
        ENDIF.
*        LOOP AT it_makt.
*          READ TABLE it_output WITH KEY matnr = it_makt-matnr
*                                             BINARY SEARCH.
*          IF sy-subrc = 0.
*            val_tab = it_output.
*            SHIFT val_tab-lgpbe UP TO '*' .
*            SHIFT val_tab-lgpbe.
*            APPEND val_tab.
*            CLEAR val_tab.
*          ENDIF.
*        ENDLOOP.

        LOOP AT it_output.
          READ TABLE it_makt WITH KEY matnr = it_output-matnr.
          IF sy-subrc = 0.
            val_tab = it_output.
            SHIFT val_tab-lgpbe UP TO '*' .
            SHIFT val_tab-lgpbe.
            APPEND val_tab.
            CLEAR val_tab.
          ENDIF.
        ENDLOOP.

      ENDIF.

    ENDIF.

*****************  Finding part w.r.t Line station ************
    IF NOT v_lgpbe IS INITIAL.
** Furong on 10/31/11 Performance {
      REFRESH: it_mard.
      IF NOT it_output[] IS INITIAL.
** } on 10/31/11
        CONCATENATE '%' v_lgpbe '%' INTO v_lgpbe.
        SELECT matnr werks lgort lgpbe FROM mard
                            INTO CORRESPONDING FIELDS OF TABLE it_mard
                            FOR ALL ENTRIES IN it_output
                            WHERE matnr = it_output-matnr
                              AND werks = it_output-werks
** Changed by Furong on 10/19/09
*                            AND ( LGORT = 'P400'  OR LGORT = 'P500' )
** End on 10/19/09
                              AND lgpbe LIKE v_lgpbe.
      ENDIF.
      LOOP AT it_mard.

        READ TABLE it_output WITH KEY matnr = it_mard-matnr
                                      werks = it_mard-werks
                                      lgpbe = it_mard-lgpbe
                                           BINARY SEARCH.
        IF sy-subrc = 0.

          val_tab = it_output.
          SHIFT val_tab-lgpbe UP TO '*' .
          SHIFT val_tab-lgpbe.
          APPEND val_tab.
          CLEAR val_tab.
        ENDIF.

      ENDLOOP.

    ENDIF.

*****************  Finding part w.r.t Vendor number ************

    IF NOT v_lifnr IS INITIAL.

      DO.

        REPLACE '*' WITH '%' INTO v_lifnr.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

      ENDDO.

** Furong on 10/31/11 Performance {
      REFRESH: it_eord.
      IF NOT it_output[] IS INITIAL.
** End on 10/31/11
        SELECT matnr werks zeord lifnr FROM eord
                            INTO CORRESPONDING FIELDS OF TABLE it_eord
                            FOR ALL ENTRIES IN it_output
                            WHERE matnr = it_output-matnr
                              AND werks = it_output-werks
                              AND lifnr LIKE v_lifnr.
      ENDIF.
      LOOP AT it_eord.

        READ TABLE it_output WITH KEY matnr = it_eord-matnr
                                       werks = it_eord-werks
                                       lifnr = it_eord-lifnr
                                               BINARY SEARCH.
        IF sy-subrc = 0.
          val_tab = it_output.
          IF NOT val_tab-lgpbe IS INITIAL.
            SHIFT val_tab-lgpbe UP TO '*' .
            SHIFT val_tab-lgpbe.
          ENDIF.
          APPEND val_tab.
          CLEAR val_tab.
        ENDIF.
      ENDLOOP.
    ENDIF.

*****************  Finding part w.r.t ALC code ************

    IF NOT v_mfrpn IS INITIAL.

      DO.

        REPLACE '*' WITH '%' INTO v_mfrpn.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

      ENDDO.

** Furong on 10/31/11 Performance {
      REFRESH: it_mara.
      IF NOT it_output[] IS INITIAL.
** End on 10/31/11

        SELECT matnr mfrpn FROM mara INTO TABLE it_mara
                                     FOR ALL ENTRIES IN it_output
                                     WHERE matnr = it_output-matnr
                                       AND mfrpn LIKE v_mfrpn .
      ENDIF.
      LOOP AT it_mara.

        READ TABLE it_output WITH KEY matnr = it_mara-matnr
                                      mfrpn = it_mara-mfrpn
                                      BINARY SEARCH.
        IF sy-subrc = 0.

          val_tab = it_output.
          IF NOT val_tab-lgpbe IS INITIAL.
            SHIFT val_tab-lgpbe UP TO '*' .
            SHIFT val_tab-lgpbe.
          ENDIF.
          APPEND val_tab.
          CLEAR val_tab.
        ENDIF.

      ENDLOOP.

    ENDIF.

*****************  Finding part w.r.t Production line ************

    IF NOT v_vspvb IS INITIAL.

      IF v_combp <> 'X'.

        CLEAR: v_vspvb1, v_vspvb2.
        v_vspvb1 = v_vspvb2 = v_vspvb.

        SHIFT v_vspvb1 LEFT  DELETING LEADING  v_ast.
        SHIFT v_vspvb2 RIGHT DELETING TRAILING v_ast.

        LOOP AT it_output.
          IF it_output-vspvb CP v_vspvb OR it_output-vspvb CP v_vspvb1
              OR it_output-vspvb CP v_vspvb2.
            MOVE-CORRESPONDING it_output TO val_tab.
            IF NOT val_tab-lgpbe IS INITIAL.
              SHIFT val_tab-lgpbe UP TO '*'.
              SHIFT val_tab-lgpbe.
            ENDIF.
            APPEND val_tab.
            CLEAR val_tab.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDIF.


*** On 09/20/13 by Furong
  CASE marc-werks.
    WHEN 'E001'.
      LOOP AT val_tab.
        IF val_tab-lgort = 'E010' OR
                val_tab-lgort = 'E110' OR
                val_tab-lgort = 'E115' OR
                val_tab-lgort = 'E119' OR
                val_tab-lgort = 'E120' OR
                val_tab-lgort = 'E125' OR
                val_tab-lgort = 'E128' OR
                val_tab-lgort = 'E301' OR
                val_tab-lgort = 'E302' OR
                val_tab-lgort = 'E303' OR
                val_tab-lgort = 'E999' OR
                val_tab-lgort = 'E998' OR
                val_tab-lgort = 'EL50' OR
                val_tab-lgort = 'X551' OR
                val_tab-lgort = 'E499'.
          DELETE val_tab.
          CLEAR val_tab.
        ENDIF.
      ENDLOOP.
    WHEN 'E002'.

      LOOP AT val_tab.
        IF val_tab-lgort = 'N010' OR
                val_tab-lgort = 'N110' OR
                val_tab-lgort = 'N115' OR
               val_tab-lgort = 'N119' OR
                val_tab-lgort = 'N120' OR
                val_tab-lgort = 'N125' OR
                val_tab-lgort = 'N128' OR
               val_tab-lgort = 'N301' OR
                val_tab-lgort = 'N302' OR
                val_tab-lgort = 'N303' OR
                val_tab-lgort = 'N999' OR
                val_tab-lgort = 'N998' OR
                val_tab-lgort = 'NL50' OR
                val_tab-lgort = 'N551' OR
** On 03/05/13
                val_tab-lgort = 'X551' OR
** end on 03/05/13
                val_tab-lgort = 'N499'.
          DELETE val_tab.
          CLEAR val_tab.
        ENDIF.
      ENDLOOP.

    WHEN 'P001'.
      LOOP AT val_tab.
        IF val_tab-lgort = '9999' OR
           val_tab-lgort = 'F001' OR
           val_tab-lgort = 'P010' OR
           val_tab-lgort = 'P499' OR
          val_tab-lgort = 'P600' OR
           val_tab-lgort = 'P610' OR
          val_tab-lgort = 'P620' OR
          val_tab-lgort = 'P630' OR
           val_tab-lgort = 'P640' OR
           val_tab-lgort = 'P690' OR
          val_tab-lgort = 'P699' OR
          val_tab-lgort = 'P800' OR
         val_tab-lgort = 'V001' OR
           val_tab-lgort = 'PK10' OR
         val_tab-lgort = 'X551' OR
          val_tab-lgort = 'X905' OR
           val_tab-lgort = 'P128' OR
          val_tab-lgort = 'P230' OR
          val_tab-lgort = 'P999' OR
          val_tab-lgort = 'P998' OR
          val_tab-lgort = 'G999' OR
          val_tab-lgort = 'G998' OR
         val_tab-lgort = 'G100' OR
           val_tab-lgort = 'G150' OR
           val_tab-lgort = 'XMIT'.
          DELETE val_tab.
          CLEAR val_tab.
        ENDIF.
      ENDLOOP.

  ENDCASE.
** End on 09/20/13

*************  Logic for F4 Help   *************

  SORT val_tab.
  DELETE ADJACENT DUPLICATES FROM val_tab.

  it_dsel-fldname = 'WERKS'.
  it_dsel-dyfldname = 'MARC-WERKS'.
  APPEND it_dsel.

  it_dsel-fldname = 'MATNR'.
  it_dsel-dyfldname = 'MATNR'.
  APPEND it_dsel.

  it_dsel-fldname = 'MAKTG'.
  it_dsel-dyfldname = 'MAKT-MAKTG'.
  APPEND it_dsel.

  it_dsel-fldname = 'LGPBE'.
  it_dsel-dyfldname = 'MARD-LGPBE'.
  APPEND it_dsel.

  it_dsel-fldname = 'LIFNR'.
  it_dsel-dyfldname = 'LFA1-LIFNR'.
  APPEND it_dsel.

  it_dsel-fldname = 'MFRPN'.
  it_dsel-dyfldname = 'MARA-MFRPN'.
  APPEND it_dsel.

  it_dsel-fldname = 'VSPVB'.
  it_dsel-dyfldname = 'MARC-VSPVB'.
  APPEND it_dsel.

  it_dsel-fldname = 'LGORT'.
  it_dsel-dyfldname = 'LGORT'.
  APPEND it_dsel.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
EXPORTING
  ddic_structure        = 'ZSCRAP_STR'
  retfield               = 'MATNR'
*      PVALKEY                = ' '
  dynpprog               = dyname
  dynpnr                 = dynumb
*      DYNPROFIELD            = ' '
*      STEPL                  = 0
*      WINDOW_TITLE           =
*      VALUE                  = ' '
 value_org              = 'S'
*       MULTIPLE_CHOICE        = 'X'
*      DISPLAY                = ' '
*      CALLBACK_PROGRAM       = ' '
*      CALLBACK_FORM          = ' '
TABLES
  value_tab              = val_tab
  field_tab              = fld_tab
  return_tab             = ret_tab
  dynpfld_mapping        = it_dsel
*    EXCEPTIONS
*      PARAMETER_ERROR        = 1
*      NO_VALUES_FOUND        = 2
*      OTHERS                 = 3
       .

  IF ret_tab IS INITIAL.
    MESSAGE e026 WITH 'No data selected'.
    EXIT.
  ENDIF.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.

    CLEAR: it_dynpfields, it_dynpfields[].

    LOOP AT ret_tab.

      CASE ret_tab-retfield.

        WHEN 'WERKS'.
          marc-werks =  ret_tab-fieldval.

        WHEN 'MATNR'.

          matnr =  ret_tab-fieldval.

        WHEN 'MAKT-MAKTG'.


          makt-maktg = ret_tab-fieldval.

        WHEN 'MARD-LGPBE'.

          mard-lgpbe =  ret_tab-fieldval.

        WHEN 'LFA1-LIFNR'.

          lfa1-lifnr =  ret_tab-fieldval.

          CLEAR: v_name.

          SELECT SINGLE name1 INTO v_name FROM lfa1
                         WHERE lifnr = ret_tab-fieldval.

          lfa1-name1 = v_name.

        WHEN 'MARA-MFRPN'.

          mara-mfrpn =  ret_tab-fieldval.

        WHEN 'MARC-VSPVB'.

          marc-vspvb =  ret_tab-fieldval.

        WHEN 'LGORT'.

          lgort = ret_tab-fieldval.

      ENDCASE.

    ENDLOOP.

    IF lfa1-lifnr IS INITIAL.
      SELECT SINGLE lifnr INTO lfa1-lifnr
        FROM eord
        WHERE matnr = matnr
          AND werks = marc-werks.
    ENDIF.
*    P_UCOMM = 'X'.
  ENDIF.

  CLEAR : v_combd, v_combp.

ENDFORM.                    " find_part
*&---------------------------------------------------------------------*
*&      Form  update_values
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_values.


****** Displaying the all values for particular line selected by user
****** shown in the F4 window

  CLEAR lfa1-name1.

  DATA: val_tab1 LIKE zscrap_str OCCURS 0 WITH HEADER LINE.

  DATA: ret_tab1 TYPE ddshretval OCCURS 0 WITH HEADER LINE,
        fld_tab1 TYPE dfies OCCURS 0 WITH HEADER LINE,
        it_dsel1 TYPE dselc OCCURS 0 WITH HEADER LINE.

  DATA : dyname1 LIKE d020s-prog , dynumb1 LIKE d020s-dnum .

  DATA: v_name LIKE lfa1-name1.

  RANGES: r_lgort_eng FOR mard-lgort.

  dyname1 = 'SAPMZSCRAP' .
  dynumb1 = sy-dynnr.
*  DYNUMB1 = '1001'.

  it_dsel1-fldname = 'WERKS'.
  it_dsel1-dyfldname = 'MARC-WERKS'.
  APPEND it_dsel1.

  it_dsel1-fldname = 'MATNR'.
  it_dsel1-dyfldname = 'MATNR'.
  APPEND it_dsel1.

  it_dsel1-fldname = 'MAKTG'.
  it_dsel1-dyfldname = 'MAKT-MAKTG'.
  APPEND it_dsel1.

  it_dsel1-fldname = 'LGPBE'.
  it_dsel1-dyfldname = 'MARD-LGPBE'.
  APPEND it_dsel1.

  it_dsel1-fldname = 'LIFNR'.
  it_dsel1-dyfldname = 'LFA1-LIFNR'.
  APPEND it_dsel1.

  it_dsel1-fldname = 'MFRPN'.
  it_dsel1-dyfldname = 'MARA-MFRPN'.
  APPEND it_dsel1.

  it_dsel1-fldname = 'VSPVB'.
  it_dsel1-dyfldname = 'MARC-VSPVB'.
  APPEND it_dsel1.

  it_dsel1-fldname = 'LGORT'.
  it_dsel1-dyfldname = 'LGORT'.
  APPEND it_dsel1.


  IF NOT matnr IS INITIAL.
*    IF SY-TCODE = 'ZSCRAP1_ENG'.
*    IF MARC-WERKS = 'E001'.
    IF marc-werks+0(1) = 'E'.
** for engine
      IF marc-werks = 'E001'.
        IF mara-mtart = 'HALB'.
          SELECT * FROM zscrap_halb
            INTO CORRESPONDING FIELDS OF TABLE val_tab1
            WHERE matnr = matnr AND werks = marc-werks.
          LOOP AT val_tab1.
            IF val_tab1-lgort = 'E010' OR
               val_tab1-lgort = 'E110' OR
               val_tab1-lgort = 'E115' OR
               val_tab1-lgort = 'E119' OR
               val_tab1-lgort = 'E120' OR
               val_tab1-lgort = 'E125' OR
               val_tab1-lgort = 'E128' OR
               val_tab1-lgort = 'E301' OR
               val_tab1-lgort = 'E302' OR
               val_tab1-lgort = 'E303' OR
               val_tab1-lgort = 'E999' OR
               val_tab1-lgort = 'E998' OR
               val_tab1-lgort = 'EL50' OR
               val_tab1-lgort = 'X551' OR
               val_tab1-lgort = 'E499'.
              DELETE val_tab1.
            ENDIF.
          ENDLOOP.
        ELSE.
          REFRESH r_lgort_eng.
          r_lgort_eng-sign = 'I'.
          r_lgort_eng-option = 'EQ'.
          r_lgort_eng-low = 'E100'.
          APPEND r_lgort_eng.
          r_lgort_eng-low = 'E200'.
          APPEND r_lgort_eng.
          r_lgort_eng-low = 'E210'.
          APPEND r_lgort_eng.
          r_lgort_eng-low = 'E910'.
          APPEND r_lgort_eng.
          r_lgort_eng-low = 'E911'.
          APPEND r_lgort_eng.
          r_lgort_eng-low = 'E912'.
          APPEND r_lgort_eng.
          r_lgort_eng-low = 'E810'.
          APPEND r_lgort_eng.
          r_lgort_eng-low = 'E811'.
          APPEND r_lgort_eng.
          r_lgort_eng-low = 'E812'.
          APPEND r_lgort_eng.

          SELECT * FROM zscrap
             INTO CORRESPONDING FIELDS OF TABLE val_tab1
             WHERE matnr = matnr AND werks = marc-werks
               AND lgort IN r_lgort_eng.
        ENDIF.
** For E002
      ELSEIF marc-werks = 'E002'.
        IF mara-mtart = 'HALB'.
          SELECT * FROM zscrap_halb
            INTO CORRESPONDING FIELDS OF TABLE val_tab1
            WHERE matnr = matnr AND werks = marc-werks.
          LOOP AT val_tab1.
            IF val_tab1-lgort = 'N010' OR
               val_tab1-lgort = 'N110' OR
               val_tab1-lgort = 'N115' OR
               val_tab1-lgort = 'N119' OR
               val_tab1-lgort = 'N120' OR
               val_tab1-lgort = 'N125' OR
               val_tab1-lgort = 'N128' OR
               val_tab1-lgort = 'N301' OR
               val_tab1-lgort = 'N302' OR
               val_tab1-lgort = 'N303' OR
               val_tab1-lgort = 'N999' OR
               val_tab1-lgort = 'N998' OR
               val_tab1-lgort = 'NL50' OR
               val_tab1-lgort = 'N551' OR
               val_tab1-lgort = 'N499'.
              DELETE val_tab1.
            ENDIF.
          ENDLOOP.
        ELSE.
          REFRESH r_lgort_eng.
          r_lgort_eng-sign = 'I'.
          r_lgort_eng-option = 'EQ'.
          r_lgort_eng-low = 'N100'.
          APPEND r_lgort_eng.
          r_lgort_eng-low = 'N200'.
          APPEND r_lgort_eng.
          r_lgort_eng-low = 'N210'.
          APPEND r_lgort_eng.
          r_lgort_eng-low = 'N910'.
          APPEND r_lgort_eng.
          r_lgort_eng-low = 'N911'.
          APPEND r_lgort_eng.
          r_lgort_eng-low = 'N912'.
          APPEND r_lgort_eng.
          r_lgort_eng-low = 'N810'.
          APPEND r_lgort_eng.
          r_lgort_eng-low = 'N811'.
          APPEND r_lgort_eng.
          r_lgort_eng-low = 'N812'.
          APPEND r_lgort_eng.

          SELECT * FROM zscrap
             INTO CORRESPONDING FIELDS OF TABLE val_tab1
            WHERE matnr = matnr AND werks = marc-werks
              AND lgort IN r_lgort_eng.
        ENDIF.
** End for E002
      ENDIF.

    ELSE.
** for P001
      IF sy-uname+0(1) = 'P'.
        SELECT * FROM zscrap
            INTO CORRESPONDING FIELDS OF TABLE val_tab1
           WHERE matnr = matnr AND werks = marc-werks
             AND ( lgort = 'G100' OR lgort = 'G150' ).

      ELSE.
        IF mara-mtart = 'HALB' OR mara-mtart = 'ROH1'.
          SELECT * FROM zscrap_halb
            INTO CORRESPONDING FIELDS OF TABLE val_tab1
            WHERE matnr = matnr AND werks = marc-werks.
          LOOP AT val_tab1.
            IF val_tab1-lgort = '9999' OR
                val_tab1-lgort = 'F001' OR
                val_tab1-lgort = 'P010' OR
                val_tab1-lgort = 'P499' OR
                val_tab1-lgort = 'P600' OR
                val_tab1-lgort = 'P610' OR
                val_tab1-lgort = 'P620' OR
                val_tab1-lgort = 'P630' OR
               val_tab1-lgort = 'P640' OR
               val_tab1-lgort = 'P690' OR
                val_tab1-lgort = 'P699' OR
                val_tab1-lgort = 'P800' OR
                val_tab1-lgort = 'V001' OR
                val_tab1-lgort = 'PK10' OR
                val_tab1-lgort = 'X551' OR
                val_tab1-lgort = 'X905' OR
                val_tab1-lgort = 'P128' OR
                val_tab1-lgort = 'P230' OR
                val_tab1-lgort = 'P999' OR
                val_tab1-lgort = 'P998' OR
                val_tab1-lgort = 'G999' OR
                val_tab1-lgort = 'G998' OR
                val_tab1-lgort = 'G100' OR
                val_tab1-lgort = 'G150' OR
                val_tab1-lgort = 'XMIT'.
              DELETE val_tab1.
            ENDIF.
          ENDLOOP.
        ELSE.
          SELECT * FROM zscrap
              INTO CORRESPONDING FIELDS OF TABLE val_tab1
             WHERE matnr = matnr AND werks = marc-werks
               AND ( lgort = 'P400' OR lgort = 'P500' ).

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


  SORT val_tab1.

  DELETE ADJACENT DUPLICATES FROM val_tab1.

  LOOP AT val_tab1.
    SHIFT val_tab1-lgpbe UP TO '*'.
    SHIFT val_tab1-lgpbe.
    MODIFY val_tab1.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
 EXPORTING
    ddic_structure        = 'ZSCRAP_STR'
    retfield               = 'MATNR'
*      PVALKEY                = ' '
    dynpprog               = dyname1
    dynpnr                 = dynumb1
*      DYNPROFIELD            = ' '
*      STEPL                  = 0
*      WINDOW_TITLE           =
*      VALUE                  = ' '
   value_org              = 'S'
*       MULTIPLE_CHOICE        = 'X'
*      DISPLAY                = ' '
*      CALLBACK_PROGRAM       = ' '
*      CALLBACK_FORM          = ' '
 TABLES
    value_tab              = val_tab1
    field_tab              = fld_tab1
    return_tab             = ret_tab1
    dynpfld_mapping        = it_dsel1
*    EXCEPTIONS
*      PARAMETER_ERROR        = 1
*      NO_VALUES_FOUND        = 2
*      OTHERS                 = 3
         .

  IF ret_tab1 IS INITIAL.
    MESSAGE e026 WITH 'No data selected'.
    EXIT.
  ENDIF.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.


    LOOP AT ret_tab1.

      CASE ret_tab1-retfield.

        WHEN 'WERKS'.
          marc-werks =  ret_tab1-fieldval.

        WHEN 'MATNR'.

          matnr =  ret_tab1-fieldval.

        WHEN 'MAKT-MAKTG'.

          makt-maktg = ret_tab1-fieldval.

        WHEN 'MARD-LGPBE'.

          mard-lgpbe =  ret_tab1-fieldval.

        WHEN 'LFA1-LIFNR'.

          lfa1-lifnr =  ret_tab1-fieldval.

          SELECT SINGLE name1 INTO v_name FROM lfa1
                         WHERE lifnr = ret_tab1-fieldval.

          lfa1-name1 = v_name.


        WHEN 'MARA-MFRPN'.

          mara-mfrpn =  ret_tab1-fieldval.

        WHEN 'MARC-VSPVB'.

          marc-vspvb =  ret_tab1-fieldval.

        WHEN 'LGORT'.

          lgort =  ret_tab1-fieldval.

      ENDCASE.

    ENDLOOP.

*    IF LFA1-LIFNR IS INITIAL.
*      IF LGORT =  'P100' OR
*          LGORT =  'P110' OR
*          LGORT =  'P120'.
*        LFA1-LIFNR = 'AG7N'.
*        SELECT SINGLE NAME1 INTO V_NAME FROM LFA1
*              WHERE LIFNR = LFA1-LIFNR.
*
*        LFA1-NAME1 = V_NAME.
*
*
*      ENDIF.
*    ENDIF.

*    P_UCOMM = 'X'.
  ENDIF.

ENDFORM.                    " update_values
*&---------------------------------------------------------------------*
*&      Form  check_values
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_values.

** checking all validations for input fields  ***********

  SELECT SINGLE * FROM marc WHERE werks = marc-werks.
  IF sy-subrc NE 0.
    MESSAGE e006 WITH marc-werks.
  ENDIF.

  SELECT SINGLE * FROM marc WHERE matnr = matnr AND
                           werks = marc-werks.
  IF sy-subrc NE 0.
    CLEAR marc-vspvb.
    MESSAGE e007 WITH matnr marc-werks.
  ENDIF.

  IF NOT lfa1-lifnr IS INITIAL.

    SELECT SINGLE * FROM lfa1 WHERE lifnr = lfa1-lifnr.

    IF sy-subrc NE 0.
      MESSAGE e008 WITH lfa1-lifnr.
    ENDIF.
** Changed by Furong on 10/19/09
*  ELSE.
*    LFA1-LIFNR = 'AG7N'.
*    SELECT SINGLE NAME1 INTO LFA1-NAME1 FROM LFA1
*    WHERE LIFNR = LFA1-LIFNR.
** End of change
  ENDIF.

  SELECT SINGLE * FROM pvbe WHERE werks = marc-werks
                            AND prvbe = marc-vspvb.
** Changed by Furong on 10/21/09
*  IF SY-SUBRC NE 0.
*
*    MESSAGE E009 WITH MARC-VSPVB MARC-WERKS.
*
*  ENDIF.
** End of change

** Changed by Furong on 12/11/12
** Changed by Furong on 03/23/09
*  SELECT SINGLE * FROM MARA WHERE MATNR = MATNR.
*  IF MARA-MSTAE NE 12.
*    MESSAGE E027 WITH MARA.
*  ENDIF.
  SELECT SINGLE * FROM mara WHERE matnr = matnr.
  IF mara-mstae = 12 OR mara-mstae = 13.
  ELSE.
    MESSAGE e027 WITH mara.
  ENDIF.
** End on 12/0/12

** Added by Furong on 08/09/10
*  IF SY-TCODE = 'ZSCRAP1_ENG'.
  IF marc-werks = 'P001'.
** Added by Furong on 08/05/09
    IF mara-mtart = 'HALB'.
** Changed by Furong on 10/19/09
*      MESSAGE E029 WITH MARA.

      SELECT SINGLE * FROM mard WHERE matnr = matnr
                                  AND werks = marc-werks
                                AND lgort IN ('P100', 'P110', 'P120').
      IF sy-subrc NE 0.
        MESSAGE e029 WITH mara.
      ENDIF.
** End of change
    ENDIF.
  ENDIF.

** End of addition on 08/05/09
** Added by Furong on 10/19/09
  IF NOT mseg-charg IS INITIAL.
    SELECT SINGLE * FROM mchb WHERE matnr = matnr
                                AND werks = marc-werks
                                AND charg = mseg-charg.
    IF sy-subrc NE 0.

      MESSAGE e026 WITH 'Please enter correct batch number'.
    ENDIF.
  ENDIF.
** End of addition on 10/19/09
** End of change

** Added by Furong on 10/19/09
*  IF MARA-MATKL = 'AM' AND MARC-XCHAR = 'X' AND MCHB-CHARG IS INITIAL.
*    MESSAGE E026 WITH 'Please enter batch number'.
*  ENDIF.
** End of addition
ENDFORM.                    " check_values
*&---------------------------------------------------------------------*
*&      Form  creat_notif
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM creat_notif.
  DATA: l_date_c(8).
  REFRESH: bdcdata.
**************  Initial screen **************

  PERFORM bdc_dynpro      USING 'SAPLIQS0' '0200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RIWO00-QMART'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RIWO00-QMART'
                                 riwo00-qmart.

****   Second screen   ***********************

  PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=10\TAB02'.
  PERFORM bdc_field       USING 'VIQMEL-QMGRP'
                                 qmgrp.           " Author work centre
  PERFORM bdc_field       USING 'VIQMEL-QMCOD'
                                 qmcod.

  PERFORM bdc_field       USING 'RQM00-MATNR'     " material
                                 matnr.
  PERFORM bdc_field       USING 'RQM00-MAWERK'
                                 marc-werks.      " Plant
  PERFORM bdc_field       USING 'VIQMEL-RKMNG'
                                 rkmng.           " quantity


** Changed by Furong on 02/10/10
  IF NOT itobattr-equnr IS INITIAL.
    PERFORM bdc_field       USING 'VIQMEL-KDMAT'
                                   itobattr-equnr.    " BODY NO
  ENDIF.
** End of change
  PERFORM bdc_field       USING 'VIQMFE-OTGRP'
                                 otgrp.           " root cause
  PERFORM bdc_field       USING 'VIQMFE-OTEIL'
                                 oteil.
  PERFORM bdc_field       USING 'VIQMFE-FEGRP'
                                 fegrp.           " Defect type
  PERFORM bdc_field       USING 'VIQMFE-FECOD'
                                 fecod.
  PERFORM bdc_field       USING 'VIQMUR-URCOD'    " cause code
                                 urcod.
** Changed by Furong on 11/04/09
  PERFORM bdc_field       USING 'VIQMUR-URTXT'    " Cause text
                                cuasetxt.
** End of change
  PERFORM bdc_field       USING 'VIQMUR-URGRP'
                                 urgrp.
  IF marc-xchar = 'X'.
    PERFORM bdc_field       USING 'VIQMEL-CHARG'
                                   mseg-charg.
  ENDIF.

** Changed by Furong on 04/03/12
  IF riwo00-qmart = 'Q4'.
    PERFORM bdc_field       USING 'ZSQM_CI_QMEL-CODEGRP_VH'
                                   codegrp_vh.
    PERFORM bdc_field       USING 'ZSQM_CI_QMEL-CODE_VH'
                                   code_vh.
  ENDIF.
** End

** Changed by Furong on 07/29/14 (
  IF riwo00-qmart = 'Q3' OR  riwo00-qmart = 'Q4'.
   PERFORM bdc_field       USING 'ZSQM_CI_QMEL-QCODEGRP_LOC'
                                  ZLOCATION.
  ENDIF.
** )

  PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
** changed by Furong on 08/10/10
  IF NOT lfa1-lifnr IS INITIAL.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'IHPA-PARNR(02)'.
    PERFORM bdc_field       USING 'IHPA-PARVW(02)'
                                  'Z5'.
    PERFORM bdc_field       USING 'IHPA-PARNR(02)'
                                   lfa1-lifnr.
  ENDIF.
** End of change
** Changed by Furong on 02/10/10
  IF NOT qmel-erdat IS INITIAL.
    WRITE: qmel-erdat TO l_date_c MMDDYY.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'VIQMEL-AUSVN'.
    PERFORM bdc_field       USING 'VIQMEL-AUSVN'
                                  l_date_c.
  ENDIF.
** End of change
  PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BUCH'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'IHPA-PARVW(03)'.

  PERFORM bdc_transaction1 USING 'QM01'.



ENDFORM.                    " creat_notif
*&---------------------------------------------------------------------*
*&      Form  print_label
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_label.
  SET PARAMETER ID 'IQM' FIELD it_qmnum-qmnum.
  SET PARAMETER ID 'MAT' FIELD it_qmnum-matnr.
*it_qmnum-QMNUM = QMNUM.
*append it_qmnum.
  SORT it_qmnum BY qmnum.
  DELETE ADJACENT DUPLICATES FROM it_qmnum COMPARING qmnum.
  EXPORT it_qmnum TO MEMORY ID 'M1'.
  SUBMIT zqmscrap_label AND RETURN.
*****end code to pass variables

ENDFORM.                    " print_label
*&---------------------------------------------------------------------*
*&      Form  code_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM code_check.

  IF qmgrp IS INITIAL.
    MESSAGE e015.
  ENDIF.

  IF rkmng IS INITIAL.
    MESSAGE e016.
  ENDIF.

  IF otgrp IS INITIAL.
    MESSAGE e017.
  ENDIF.

  IF fegrp IS INITIAL.
    MESSAGE e018.
  ENDIF.

  IF urgrp IS INITIAL.
    MESSAGE e019.
  ENDIF.

  IF fegrp = '0'.
    IF urcod <> '04'.
      MESSAGE e013.
    ENDIF.
    IF fecod = '0015'.
      IF w_call = 'SSRW' OR
         w_call = 'SSSC'.
      ELSE.
        MESSAGE e031.
      ENDIF.
    ENDIF.

  ENDIF.

  IF urcod = '04'.
    IF fegrp <> '0'.
      MESSAGE e014.
    ENDIF.
  ENDIF.
  IF cuasetxt IS INITIAL.
    MESSAGE e030.
  ENDIF.

  IF riwo00-qmart IS INITIAL.
    MESSAGE e026 WITH 'Please enter Notification type'.
  ENDIF.

** Changed by Furong on 10/21/09
  IF marc-xchar = 'X' AND mseg-charg IS INITIAL.
    MESSAGE e026 WITH 'Please enter batch number'.
  ENDIF.
** End of change
** Added by Furong on 11/05/09
  IF NOT mseg-charg IS INITIAL.
    SELECT SINGLE * FROM mchb WHERE matnr = matnr
                                AND werks = marc-werks
                                AND charg = mseg-charg.
    IF sy-subrc NE 0.

      MESSAGE e026 WITH 'Please enter correct batch number'.
    ENDIF.
  ENDIF.
** End of addition on 11/05/09

  CLEAR: w_ans.
  IF NOT mara-blatt IS INITIAL.
** Added by Furong on 01/04/11
    IF qmgrp = 'MXGXS1'.
    ELSE.
** End of change on 01/04/11
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*     TITLEBAR                    = ' '
*     DIAGNOSE_OBJECT             = ' '
    text_question               = 'Does the part need to be reordered?'
         text_button_1               = 'Yes'
*     ICON_BUTTON_1               = ' '
         text_button_2               = 'No'
*     ICON_BUTTON_2               = ' '
*     DEFAULT_BUTTON              = '1'
        display_cancel_button       = ' '
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN                = 25
*     START_ROW                   = 6
*     POPUP_TYPE                  =
       IMPORTING
         answer                      = w_ans
*     TABLES
*       PARAMETER                   =
       EXCEPTIONS
         text_not_found              = 1
         OTHERS                      = 2
                .
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

*    CALL FUNCTION 'POPUP_TO_DECIDE'
*          EXPORTING
*        TEXTLINE1              = 'Does the part need to be reordered?'
*           TEXT_OPTION1            = 'Yes'
*           TEXT_OPTION2            = 'No'
*            TITEL                   = 'Warning'
**   START_COLUMN            = 25
**   START_ROW               = 6
**   CANCEL_DISPLAY          = 'X'
*            IMPORTING
*            ANSWER                  = W_ANS
      .
    ENDIF.
  ENDIF.
  IF itobattr-equnr IS INITIAL AND
     ( fecod = '4001' OR
       fecod = '4003' OR
       fecod = '4004' OR
       fecod = '4007' OR
       fecod = '4008' OR
       fecod = '4011').
    MESSAGE e026
     WITH 'Body No. is required for the Responsibility Code'.
  ENDIF.

  IF NOT itobattr-equnr IS INITIAL.
    l_equnr = itobattr-equnr.
    IF l_equnr+3(1) = ' '.
      CONCATENATE l_equnr+0(3) l_equnr+4(14) INTO l_equnr.
      itobattr-equnr = l_equnr.
    ENDIF.
    SELECT SINGLE equnr INTO l_equnr
    FROM equi
      WHERE equnr = l_equnr.
    IF sy-subrc = 0.
    ELSE.
      MESSAGE e026 WITH 'Invalid body number entered'.
    ENDIF.
  ENDIF.
** Changed by Furong on 08/18/10
  IF qmel-erdat IS INITIAL.
    MESSAGE e026 WITH 'Please enter malfunction start date'.
  ELSE.
    IF qmel-erdat > sy-datum.
      MESSAGE e026 WITH
      'Mal. start date cannot be greater than curr. date'.
    ENDIF.
  ENDIF.

** End of change

** Furong on 06/11/12 for if qty >= 100,
** and send the email to the manager
  IF rkmng >= 100.
    PERFORM check_limitation_100.
  ENDIF.
** End

ENDFORM.                    " code_check
*&---------------------------------------------------------------------*
*&      Form  display_var
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_var.

  DATA: val_tab2 LIKE zscrap_var OCCURS 0 WITH HEADER LINE.


  DATA: ret_tab2 TYPE ddshretval OCCURS 0 WITH HEADER LINE,
        fld_tab2 TYPE dfies OCCURS 0 WITH HEADER LINE,
        it_dsel2 TYPE dselc OCCURS 0 WITH HEADER LINE.

  DATA : dyname2 LIKE d020s-prog , dynumb2 LIKE d020s-dnum .

  dyname2 = 'SAPMZSCRAP' .
  dynumb = sy-dynnr.
*  DYNUMB2 = '1002'.


  it_dsel2-fldname = 'NAME'.
  it_dsel2-dyfldname = 'NAME'.
  APPEND it_dsel2.

  it_dsel2-fldname = 'LGNUM'.
  it_dsel2-dyfldname = 'LGNUM'.
  APPEND it_dsel2.

  it_dsel2-fldname = 'BWART'.
  it_dsel2-dyfldname = 'BWART'.
  APPEND it_dsel2.

  it_dsel2-fldname = 'LGORT'.
  it_dsel2-dyfldname = 'LGORT'.
  APPEND it_dsel2.

  it_dsel2-fldname = 'QMART'.
  it_dsel2-dyfldname = 'QMART'.
  APPEND it_dsel2.

  it_dsel2-fldname = 'QMGRP'.
  it_dsel2-dyfldname = 'QMGRP'.
  APPEND it_dsel2.

  it_dsel2-fldname = 'QMCOD'.
  it_dsel2-dyfldname = 'QMCOD'.
  APPEND it_dsel2.

  it_dsel2-fldname = 'OTGRP'.
  it_dsel2-dyfldname = 'OTGRP'.
  APPEND it_dsel2.

  it_dsel2-fldname = 'OTEIL'.
  it_dsel2-dyfldname = 'OTEIL'.
  APPEND it_dsel2.

  it_dsel2-fldname = 'FEGRP'.
  it_dsel2-dyfldname = 'FEGRP'.
  APPEND it_dsel2.

  it_dsel2-fldname = 'FECOD'.
  it_dsel2-dyfldname = 'FECOD'.
  APPEND it_dsel2.

  it_dsel2-fldname = 'URGRP'.
  it_dsel2-dyfldname = 'URGRP'.
  APPEND it_dsel2.

  it_dsel2-fldname = 'URCOD'.
  it_dsel2-dyfldname = 'URCOD'.
  APPEND it_dsel2.

** Changed by Furong on 08/09/10
*  SELECT * FROM ZSCRAP_VAR INTO CORRESPONDING FIELDS OF TABLE VAL_TAB2.
*  IF SY-TCODE = 'ZSCRAP1_ENG'.
  IF marc-werks = 'E001'.
    SELECT * FROM zscrap_var
             INTO CORRESPONDING FIELDS OF TABLE val_tab2
            WHERE lgort = 'E499'.
** For E002
  ELSEIF marc-werks = 'E002'.
    SELECT * FROM zscrap_var
             INTO CORRESPONDING FIELDS OF TABLE val_tab2
            WHERE lgort = 'N499'.
** END FOR E002
  ELSE.
    SELECT * FROM zscrap_var INTO CORRESPONDING FIELDS OF TABLE val_tab2
                             WHERE lgort = 'P499'.
  ENDIF.
** End of change on 08/09/10
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
 EXPORTING
    ddic_structure        = 'ZSCRAP_VAR'
    retfield               = 'NAME'
*      PVALKEY                = ' '
    dynpprog               = dyname2
    dynpnr                 = dynumb2
*      DYNPROFIELD            = ' '
*      STEPL                  = 0
*      WINDOW_TITLE           =
*      VALUE                  = ' '
   value_org              = 'S'
*       MULTIPLE_CHOICE        = 'X'
*      DISPLAY                = ' '
*      CALLBACK_PROGRAM       = ' '
*      CALLBACK_FORM          = ' '
 TABLES
    value_tab              = val_tab2
    field_tab              = fld_tab2
    return_tab             = ret_tab2
    dynpfld_mapping        = it_dsel2
*    EXCEPTIONS
*      PARAMETER_ERROR        = 1
*      NO_VALUES_FOUND        = 2
*      OTHERS                 = 3
         .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.


    LOOP AT ret_tab2.

      CASE ret_tab2-retfield.

        WHEN 'LGNUM'.

          rmmg1-lgnum = ret_tab2-fieldval.

        WHEN 'BWART'.

          bwart = ret_tab2-fieldval.

        WHEN 'LGORT'.

          rmmg1-lgort = ret_tab2-fieldval.

        WHEN 'QMART'.
          IF w_call = 'SSRW'.
          ELSE.
            riwo00-qmart = ret_tab2-fieldval.
          ENDIF.
        WHEN 'QMGRP'.

          qmgrp =  ret_tab2-fieldval.

        WHEN 'QMCOD'.

          qmcod = ret_tab2-fieldval.

        WHEN 'OTGRP'.

          otgrp =  ret_tab2-fieldval.

        WHEN 'OTEIL'.

          oteil =  ret_tab2-fieldval.

        WHEN 'FEGRP'.

          fegrp =  ret_tab2-fieldval.

        WHEN 'FECOD'.

          fecod =  ret_tab2-fieldval.

        WHEN 'URGRP'.

          urgrp =  ret_tab2-fieldval.

        WHEN 'URCOD'.

          urcod =  ret_tab2-fieldval.

      ENDCASE.

    ENDLOOP.

  ENDIF.


ENDFORM.                    " display_var
*&---------------------------------------------------------------------*
*&      Form  creat_notif1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM creat_notif1.
  DATA: l_date_c(8).
  REFRESH: bdcdata.
**************  Initial screen **************

  PERFORM bdc_dynpro      USING 'SAPLIQS0' '0200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RIWO00-QMART'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RIWO00-QMART'
                                 riwo00-qmart.

****   Second screen   ***********************

  PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BUCH'.
  PERFORM bdc_field       USING 'VIQMEL-QMGRP'
                                 qmgrp.           " Author work centre
  PERFORM bdc_field       USING 'VIQMEL-QMCOD'
                                 qmcod.

  PERFORM bdc_field       USING 'RQM00-MATNR'     " material
                                 matnr.
  PERFORM bdc_field       USING 'RQM00-MAWERK'
                                 marc-werks.      " Plant
  PERFORM bdc_field       USING 'VIQMEL-RKMNG'
                                 rkmng.           " quantity

** Changed by Furong on 02/10/10
  IF NOT itobattr-equnr IS INITIAL.
    PERFORM bdc_field       USING 'VIQMEL-KDMAT'
                                   itobattr-equnr.    " BODY NO
  ENDIF.
** End of change

  PERFORM bdc_field       USING 'VIQMFE-OTGRP'
                                 otgrp.           " root cause
  PERFORM bdc_field       USING 'VIQMFE-OTEIL'
                                 oteil.
  PERFORM bdc_field       USING 'VIQMFE-FEGRP'
                                 fegrp.           " Defect type
  PERFORM bdc_field       USING 'VIQMFE-FECOD'
                                 fecod.
  PERFORM bdc_field       USING 'VIQMUR-URCOD'    " cause code
                                 urcod.
** Changed by Furong on 11/04/09
  IF marc-xchar = 'X'.
    PERFORM bdc_field       USING 'VIQMEL-CHARG'
                                   mseg-charg.
  ENDIF.

  PERFORM bdc_field       USING 'VIQMUR-URTXT'    " Cause text
                                cuasetxt.
** End of change
  PERFORM bdc_field       USING 'VIQMUR-URGRP'
                                 urgrp.

  PERFORM bdc_transaction1 USING 'QM01'.


ENDFORM.                    " creat_notif1
*&---------------------------------------------------------------------*
*&      Form  create_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_VSPVB  text
*      -->P_V_LGTYP  text
*      -->P_V_LGPLA  text
*      -->P_V_LGBER  text
*----------------------------------------------------------------------*
FORM create_to TABLES   p_it_tline STRUCTURE tline
               USING    p_vspvb
                        p_lgtyp
                        p_lgpla
                        p_lgber.
  .
  DATA : v_uom LIKE mara-meins.

  SELECT SINGLE meins FROM mara INTO v_uom WHERE matnr = matnr.


  PERFORM bdc_dynpro      USING 'SAPML02B' '0203'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RL02B-MBLNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RL02B-MBLNR'
                                 mdoc.
  PERFORM bdc_field       USING 'RL02B-MJAHR'
                                 myear.
  PERFORM bdc_field       USING 'RL02B-LGNUM'
                                 rmmg1-lgnum.
  PERFORM bdc_field       USING 'RL02B-DUNKL'
                                'H'.
  PERFORM bdc_dynpro      USING 'SAPML03T' '0132'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BEST'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LTBP1-OFMEA(01)'.
  PERFORM bdc_dynpro      USING 'SAPML03T' '0106'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RL03T-AUSME'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=TAH2'.
  PERFORM bdc_dynpro      USING 'SAPML03T' '0102'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LTAP-VLPLA'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RL03T-ANFME'      " quantity
                                 rkmng.
  PERFORM bdc_field       USING 'LTAP-ALTME'
                                 v_uom.
  PERFORM bdc_field       USING 'RL03T-SQUIT'
                                'X'.
  PERFORM bdc_field       USING 'LTAP-VLTYP'       " Storage
                                 p_lgtyp.
  PERFORM bdc_field       USING 'LTAP-VLBER'
                                 p_lgber.
  PERFORM bdc_field       USING 'LTAP-VLPLA'       " storage Bin
                                p_lgpla.
  PERFORM bdc_dynpro      USING 'SAPML03T' '0105'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'T334T-LGTY0'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.
*perform bdc_field       using 'T334T-LGTY0'
*                              '444'.
  PERFORM bdc_transaction TABLES p_it_tline
                                USING 'LT06'.

  CLEAR v_uom.

ENDFORM.                    " create_to
*&---------------------------------------------------------------------*
*&      Form  send_mail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_SUB  text
*----------------------------------------------------------------------*
FORM send_mail USING    p_sub.

  DATA: t_objbin  LIKE solisti1  OCCURS 10 WITH HEADER LINE ,
        t_objpack LIKE sopcklsti1 OCCURS 1  WITH HEADER LINE,
        t_objhead LIKE solisti1   OCCURS 1  WITH HEADER LINE,
        t_objtxt  LIKE solisti1  OCCURS 10 WITH HEADER LINE ,
        t_reclist LIKE somlreci1 OCCURS 1  WITH HEADER LINE ,
        w_docdata   LIKE sodocchgi1.

  DATA: l_text(400) TYPE c,
        l_qty_char(13).

  DATA: t_compressed_list  LIKE soli OCCURS 0,
        t_listobject LIKE abaplist  OCCURS 1  WITH HEADER LINE.

*-Receiver List
  CLEAR t_reclist.
  REFRESH t_reclist.

*-External emails
**S> 08/04/11 Paul
  t_reclist-receiver = 'pcscrapreorder@hmmausa.COM'.
**E<
  t_reclist-com_type =  'INT'.
  t_reclist-rec_type = 'U'.
  t_reclist-express  = 'X'.
  APPEND t_reclist.
  CLEAR  t_reclist.

*-Message Body
  REFRESH t_objtxt.
  CLEAR   t_objtxt.

  READ TABLE it_qmnum INDEX 1.

  CONCATENATE 'Quality Notification #' it_qmnum-qmnum
                                       INTO t_objtxt-line.
  APPEND t_objtxt.

  CONCATENATE 'Material #' matnr INTO t_objtxt-line SEPARATED BY space.
  APPEND t_objtxt.

*  CONCATENATE  'Quantity' RKMNG INTO T_OBJTXT-LINE  SEPARATED BY SPACE.
  l_qty_char = rkmng.
  CONCATENATE  'Quantity' l_qty_char INTO t_objtxt-line
          SEPARATED BY space.
  APPEND t_objtxt.

  CONCATENATE 'Delivery Location' qmgrp INTO t_objtxt-line
               SEPARATED BY space.
  APPEND t_objtxt.


  APPEND t_objtxt.

  CLEAR t_objtxt.

  DATA: w_tab_lines  TYPE i.

*-Creation of document to be sent
  w_docdata-obj_name = 'EMAIL'.
  w_docdata-obj_descr = p_sub.  " Subject Line

*-Contents of Email
  CLEAR w_tab_lines.
  DESCRIBE TABLE t_objtxt LINES w_tab_lines.
  READ TABLE t_objtxt INDEX w_tab_lines.
  w_docdata-doc_size = ( w_tab_lines - 1 ) * 255 + strlen( t_objtxt ).

*-Creation of entry for compressed document
  CLEAR t_objpack-transf_bin.
  t_objpack-head_start = 1.
  t_objpack-head_num = 1.
  t_objpack-body_start = 1.
  t_objpack-body_num = w_tab_lines.
  t_objpack-doc_type = 'TXT'.
  APPEND t_objpack.
  CLEAR t_objpack.

*-Creation of document attachment
  CONCATENATE p_sub sy-datum INTO t_objhead.
  APPEND t_objhead.

  CLEAR w_tab_lines.
  DESCRIBE TABLE t_objbin LINES w_tab_lines.
  t_objpack-doc_size = w_tab_lines * 255.
  t_objpack-transf_bin = 'X'.
  t_objpack-head_start = 1.
  t_objpack-head_num = 1.
  t_objpack-body_start = 1.
  t_objpack-body_num = w_tab_lines.
  t_objpack-doc_type = 'TXT'.
  t_objpack-obj_name = 'ATTACHMENT'.
  t_objpack-obj_descr = p_sub.  " Attached file name
  APPEND t_objpack.
  CLEAR  t_objpack.


  DATA: sent_to_all LIKE sonv-flag,
        user_address LIKE sousradri1 OCCURS 1 WITH HEADER LINE.
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = w_docdata
      put_in_outbox              = 'X'
      commit_work                = 'X'
    IMPORTING
      sent_to_all                = sent_to_all
    TABLES
      packing_list               = t_objpack
      object_header              = t_objhead
      contents_bin               = t_objbin
      contents_txt               = t_objtxt
      receivers                  = t_reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  SKIP 1.
  CASE sy-subrc.
    WHEN 0.
      WRITE: / 'Result of the sendprocess:'.
    WHEN 1.
      WRITE: / 'No permission to sent to the specified  ',
                'amount of recipients !'.
    WHEN 2.
      WRITE: / 'Document could not be sent to any recipient !'.
    WHEN 4.
      WRITE: / 'No permission to send !'.
    WHEN OTHERS.
      WRITE: / 'Error while sending !'.
  ENDCASE.


ENDFORM.                    " send_mail

*---------------------------------------------------------------------*
*       FORM SEND_MAIL_1004                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_SUB                                                         *
*---------------------------------------------------------------------*
FORM send_mail_1004 USING    p_sub.

  DATA: t_objbin  LIKE solisti1  OCCURS 10 WITH HEADER LINE ,
        t_objpack LIKE sopcklsti1 OCCURS 1  WITH HEADER LINE,
        t_objhead LIKE solisti1   OCCURS 1  WITH HEADER LINE,
        t_objtxt  LIKE solisti1  OCCURS 10 WITH HEADER LINE ,
        t_reclist LIKE somlreci1 OCCURS 1  WITH HEADER LINE ,
        w_docdata   LIKE sodocchgi1.

  DATA: l_text(400) TYPE c.

  DATA: t_compressed_list  LIKE soli OCCURS 0,
        t_listobject LIKE abaplist  OCCURS 1  WITH HEADER LINE.

*-Receiver List
  CLEAR t_reclist.
  REFRESH t_reclist.

*-External emails
**S> 08/04/11 Paul
  t_reclist-receiver = 'pcscrapreorder@hmmausa.COM'.
**E<
  t_reclist-com_type =  'INT'.
  t_reclist-rec_type = 'U'.
  t_reclist-express  = 'X'.
  APPEND t_reclist.
  CLEAR  t_reclist.

*-Message Body
  REFRESH t_objtxt.
  CLEAR   t_objtxt.

  READ TABLE it_qmnum INDEX 1.

*  CONCATENATE 'Quality Notification #' IT_QMNUM-QMNUM
*                                       INTO T_OBJTXT-LINE.
  APPEND t_objtxt.

  CONCATENATE 'Material #' matnr INTO t_objtxt-line SEPARATED BY space.
  APPEND t_objtxt.

  CONCATENATE  'Quantity' rkmng INTO t_objtxt-line  SEPARATED BY space.
  APPEND t_objtxt.

  t_objtxt-line = 'Delivery Location: Manager Vehicle Repair'.
  APPEND t_objtxt.

  CONCATENATE 'Body #' itobattr-equnr INTO t_objtxt-line
              SEPARATED BY space.
  APPEND t_objtxt.

  CLEAR t_objtxt.

  DATA: w_tab_lines  TYPE i.

*-Creation of document to be sent
  w_docdata-obj_name = 'EMAIL'.
  w_docdata-obj_descr = p_sub.  " Subject Line

*-Contents of Email
  CLEAR w_tab_lines.
  DESCRIBE TABLE t_objtxt LINES w_tab_lines.
  READ TABLE t_objtxt INDEX w_tab_lines.
  w_docdata-doc_size = ( w_tab_lines - 1 ) * 255 + strlen( t_objtxt ).

*-Creation of entry for compressed document
  CLEAR t_objpack-transf_bin.
  t_objpack-head_start = 1.
  t_objpack-head_num = 1.
  t_objpack-body_start = 1.
  t_objpack-body_num = w_tab_lines.
  t_objpack-doc_type = 'TXT'.
  APPEND t_objpack.
  CLEAR t_objpack.

*-Creation of document attachment
  CONCATENATE p_sub sy-datum INTO t_objhead.
  APPEND t_objhead.

  CLEAR w_tab_lines.
  DESCRIBE TABLE t_objbin LINES w_tab_lines.
  t_objpack-doc_size = w_tab_lines * 255.
  t_objpack-transf_bin = 'X'.
  t_objpack-head_start = 1.
  t_objpack-head_num = 1.
  t_objpack-body_start = 1.
  t_objpack-body_num = w_tab_lines.
  t_objpack-doc_type = 'TXT'.
  t_objpack-obj_name = 'ATTACHMENT'.
  t_objpack-obj_descr = p_sub.  " Attached file name
  APPEND t_objpack.
  CLEAR  t_objpack.


  DATA: sent_to_all LIKE sonv-flag,
        user_address LIKE sousradri1 OCCURS 1 WITH HEADER LINE.
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = w_docdata
      put_in_outbox              = 'X'
      commit_work                = 'X'
    IMPORTING
      sent_to_all                = sent_to_all
    TABLES
      packing_list               = t_objpack
      object_header              = t_objhead
      contents_bin               = t_objbin
      contents_txt               = t_objtxt
      receivers                  = t_reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  SKIP 1.
  CASE sy-subrc.
    WHEN 0.
      WRITE: / 'Result of the sendprocess:'.
    WHEN 1.
      WRITE: / 'No permission to sent to the specified  ',
                'amount of recipients !'.
    WHEN 2.
      WRITE: / 'Document could not be sent to any recipient !'.
    WHEN 4.
      WRITE: / 'No permission to send !'.
    WHEN OTHERS.
      WRITE: / 'Error while sending !'.
  ENDCASE.


ENDFORM.                    " send_mail_1004

*&---------------------------------------------------------------------*
*&      Form  delete_var
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_var.


  DATA: val_tab3 LIKE zscrap_var OCCURS 0 WITH HEADER LINE.


  DATA: ret_tab3 TYPE ddshretval OCCURS 0 WITH HEADER LINE,
        fld_tab3 TYPE dfies OCCURS 0 WITH HEADER LINE,
        it_dsel3 TYPE dselc OCCURS 0 WITH HEADER LINE.

  DATA : dyname3 LIKE d020s-prog , dynumb3 LIKE d020s-dnum .

  DATA: l_name TYPE zscrap_var-name.

  dyname3 = 'SAPMZSCRAP' .
  dynumb3 = '1002'.


  it_dsel3-fldname = 'NAME'.
  it_dsel3-dyfldname = 'NAME'.
  APPEND it_dsel3.

  it_dsel3-fldname = 'QMGRP'.
  it_dsel3-dyfldname = 'QMGRP'.
  APPEND it_dsel3.

  it_dsel3-fldname = 'QMCOD'.
  it_dsel3-dyfldname = 'QMCOD'.
  APPEND it_dsel3.

  it_dsel3-fldname = 'OTGRP'.
  it_dsel3-dyfldname = 'OTGRP'.
  APPEND it_dsel3.

  it_dsel3-fldname = 'OTEIL'.
  it_dsel3-dyfldname = 'OTEIL'.
  APPEND it_dsel3.

  it_dsel3-fldname = 'FEGRP'.
  it_dsel3-dyfldname = 'FEGRP'.
  APPEND it_dsel3.

  it_dsel3-fldname = 'FECOD'.
  it_dsel3-dyfldname = 'FECOD'.
  APPEND it_dsel3.

  it_dsel3-fldname = 'URGRP'.
  it_dsel3-dyfldname = 'URGRP'.
  APPEND it_dsel3.

  it_dsel3-fldname = 'URCOD'.
  it_dsel3-dyfldname = 'URCOD'.
  APPEND it_dsel3.


  SELECT * FROM zscrap_var INTO CORRESPONDING FIELDS OF TABLE val_tab3.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
 EXPORTING
    ddic_structure        = 'ZSCRAP_VAR'
    retfield               = 'NAME'
*      PVALKEY                = ' '
    dynpprog               = dyname3
    dynpnr                 = dynumb3
*      DYNPROFIELD            = ' '
*      STEPL                  = 0
*      WINDOW_TITLE           =
*      VALUE                  = ' '
   value_org              = 'S'
       multiple_choice        = 'X'
*      DISPLAY                = ' '
*      CALLBACK_PROGRAM       = ' '
*      CALLBACK_FORM          = ' '
 TABLES
    value_tab              = val_tab3
    field_tab              = fld_tab3
    return_tab             = ret_tab3
    dynpfld_mapping        = it_dsel3
*    EXCEPTIONS
*      PARAMETER_ERROR        = 1
*      NO_VALUES_FOUND        = 2
*      OTHERS                 = 3
         .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.


    LOOP AT ret_tab3.

      IF ret_tab3-fieldname = 'NAME'.

        l_name = ret_tab3-fieldval.

      ENDIF.

      AT END OF recordpos.

        DELETE FROM zscrap_var WHERE name = l_name.

        CLEAR l_name.

        COMMIT WORK AND WAIT.

      ENDAT.

    ENDLOOP.

  ENDIF.



ENDFORM.                    " delete_var
*&---------------------------------------------------------------------*
*&      Form  change_var
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_var.



  DATA: val_tab4 LIKE zscrap_var OCCURS 0 WITH HEADER LINE.


  DATA: ret_tab4 TYPE ddshretval OCCURS 0 WITH HEADER LINE,
        fld_tab4 TYPE dfies OCCURS 0 WITH HEADER LINE,
        it_dsel4 TYPE dselc OCCURS 0 WITH HEADER LINE.

  DATA : dyname4 LIKE d020s-prog , dynumb4 LIKE d020s-dnum .

  dyname4 = 'SAPMZSCRAP' .
  dynumb4 = '1002'.


  it_dsel4-fldname = 'NAME'.
  it_dsel4-dyfldname = 'NAME'.
  APPEND it_dsel4.

  it_dsel4-fldname = 'LGNUM'.
  it_dsel4-dyfldname = 'LGNUM'.
  APPEND it_dsel4.

  it_dsel4-fldname = 'BWART'.
  it_dsel4-dyfldname = 'BWART'.
  APPEND it_dsel4.

  it_dsel4-fldname = 'LGORT'.
  it_dsel4-dyfldname = 'LGORT'.
  APPEND it_dsel4.

  it_dsel4-fldname = 'QMART'.
  it_dsel4-dyfldname = 'QMART'.
  APPEND it_dsel4.

  it_dsel4-fldname = 'QMGRP'.
  it_dsel4-dyfldname = 'QMGRP'.
  APPEND it_dsel4.

  it_dsel4-fldname = 'QMCOD'.
  it_dsel4-dyfldname = 'QMCOD'.
  APPEND it_dsel4.

  it_dsel4-fldname = 'OTGRP'.
  it_dsel4-dyfldname = 'OTGRP'.
  APPEND it_dsel4.

  it_dsel4-fldname = 'OTEIL'.
  it_dsel4-dyfldname = 'OTEIL'.
  APPEND it_dsel4.

  it_dsel4-fldname = 'FEGRP'.
  it_dsel4-dyfldname = 'FEGRP'.
  APPEND it_dsel4.

  it_dsel4-fldname = 'FECOD'.
  it_dsel4-dyfldname = 'FECOD'.
  APPEND it_dsel4.

  it_dsel4-fldname = 'URGRP'.
  it_dsel4-dyfldname = 'URGRP'.
  APPEND it_dsel4.

  it_dsel4-fldname = 'URCOD'.
  it_dsel4-dyfldname = 'URCOD'.
  APPEND it_dsel4.

*  IF SY-TCODE = 'ZSCRAP1_ENG'.
  IF marc-werks = 'E001'.
    SELECT * FROM zscrap_var INTO CORRESPONDING FIELDS OF TABLE val_tab4
                             WHERE lgort = 'E499'.
** changed on 12/13/11
  ELSEIF marc-werks = 'E002'.
    SELECT * FROM zscrap_var INTO CORRESPONDING FIELDS OF TABLE val_tab4
                             WHERE lgort = 'N499'.
** End on 12/13/11
  ELSE.
    SELECT * FROM zscrap_var INTO CORRESPONDING FIELDS OF TABLE val_tab4
                            WHERE lgort = 'P499'.
  ENDIF.

*  SELECT * FROM ZSCRAP_VAR INTO CORRESPONDING FIELDS OF TABLE VAL_TAB4.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
 EXPORTING
    ddic_structure        = 'ZSCRAP_VAR'
    retfield               = 'NAME'
*      PVALKEY                = ' '
    dynpprog               = dyname4
    dynpnr                 = dynumb4
*      DYNPROFIELD            = ' '
*      STEPL                  = 0
*      WINDOW_TITLE           =
*      VALUE                  = ' '
   value_org              = 'S'
*       MULTIPLE_CHOICE        = 'X'
*      DISPLAY                = ' '
*      CALLBACK_PROGRAM       = ' '
*      CALLBACK_FORM          = ' '
 TABLES
    value_tab              = val_tab4
    field_tab              = fld_tab4
    return_tab             = ret_tab4
    dynpfld_mapping        = it_dsel4
*    EXCEPTIONS
*      PARAMETER_ERROR        = 1
*      NO_VALUES_FOUND        = 2
*      OTHERS                 = 3
         .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.


    LOOP AT ret_tab4.

      CASE ret_tab4-retfield.

        WHEN 'LGNUM'.

          rmmg1-lgnum = ret_tab4-fieldval.

        WHEN 'BWART'.

          bwart = ret_tab4-fieldval.

        WHEN 'LGORT'.

          rmmg1-lgort = ret_tab4-fieldval.

        WHEN 'QMART'.

          riwo00-qmart = ret_tab4-fieldval.

        WHEN 'NAME'.

          name = ret_tab4-fieldval.

        WHEN 'QMGRP'.

          qmgrp =  ret_tab4-fieldval.

        WHEN 'QMCOD'.


          qmcod = ret_tab4-fieldval.

        WHEN 'OTGRP'.

          otgrp =  ret_tab4-fieldval.

        WHEN 'OTEIL'.

          oteil =  ret_tab4-fieldval.

        WHEN 'FEGRP'.

          fegrp =  ret_tab4-fieldval.

        WHEN 'FECOD'.

          fecod =  ret_tab4-fieldval.

        WHEN 'URGRP'.

          urgrp =  ret_tab4-fieldval.

        WHEN 'URCOD'.

          urcod =  ret_tab4-fieldval.

      ENDCASE.

    ENDLOOP.

  ENDIF.


  LEAVE TO SCREEN '1100' .

ENDFORM.                    " change_var
*&---------------------------------------------------------------------*
*&      Form  update_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_variant.

  DATA wa_var TYPE zscrap_var.



  wa_var-name = name.
  wa_var-lgnum = rmmg1-lgnum.
  wa_var-bwart = bwart.
  wa_var-lgort = rmmg1-lgort.
  wa_var-qmart = qmel-qmart.
  wa_var-qmgrp = qmgrp.
  wa_var-qmcod = qmcod.
  wa_var-otgrp = otgrp.
  wa_var-oteil = oteil.
  wa_var-fegrp = fegrp.
  wa_var-fecod = fecod.
  wa_var-urgrp = urgrp.
  wa_var-urcod = urcod.

  MODIFY zscrap_var FROM wa_var.

  IF sy-subrc <> 0.
  ENDIF.

  CLEAR wa_var.

  MESSAGE s020(zs).


ENDFORM.                    " update_variant
*&---------------------------------------------------------------------*
*&      Form  display_notif
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_notif.

  DATA : dyname LIKE d020s-prog , dynumb LIKE d020s-dnum .
  DATA: BEGIN OF it_dynpfields OCCURS 3.
          INCLUDE STRUCTURE dynpread.
  DATA: END OF it_dynpfields.

  DATA: BEGIN OF val_tab1 OCCURS 0,
        qmnum LIKE qmel-qmnum,
        matnr LIKE qmel-matnr,
        ernam LIKE qmel-ernam,
        erdat LIKE qmel-erdat,
        mzeit LIKE qmel-mzeit,
        objnr LIKE qmel-objnr,
        END OF val_tab1.


  DATA: val_tab LIKE zscrap_void_str OCCURS 0 WITH HEADER LINE,
        ret_tab LIKE ddshretval OCCURS 0 WITH HEADER LINE,
        it_dsel LIKE dselc OCCURS 0 WITH HEADER LINE.

  DATA: v_matnr TYPE qmel-matnr,
        v_qmnum TYPE qmel-qmnum,
        v_ernam TYPE qmel-ernam,
        v_erdat LIKE qmel-erdat,
        v_stsma TYPE jsto-stsma,
        v_stat  TYPE jest-stat,
        v_txt30 TYPE tj30t-txt30,
        v_sydatum(10).

  CLEAR : v_erdat, v_fecod.

  dyname = 'SAPMZSCRAP'.
  dynumb = '1003'.


  MOVE 'QMNUM' TO it_dynpfields-fieldname.
  APPEND it_dynpfields.

  MOVE 'VN_MATNR' TO it_dynpfields-fieldname.
  APPEND it_dynpfields.

  MOVE 'ERNAM' TO it_dynpfields-fieldname.
  APPEND it_dynpfields.

  MOVE 'QMEL-ERDAT' TO it_dynpfields-fieldname.
  APPEND it_dynpfields.


  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname                         = dyname
      dynumb                         = dynumb
    translate_to_upper               = 'X'
*   REQUEST                        = ' '
*   PERFORM_CONVERSION_EXITS       = ' '
*   PERFORM_INPUT_CONVERSION       = ' '
*   DETERMINE_LOOP_INDEX           = ' '
    TABLES
      dynpfields                     = it_dynpfields
* EXCEPTIONS
*   INVALID_ABAPWORKAREA           = 1
*   INVALID_DYNPROFIELD            = 2
*   INVALID_DYNPRONAME             = 3
*   INVALID_DYNPRONUMMER           = 4
*   INVALID_REQUEST                = 5
*   NO_FIELDDESCRIPTION            = 6
*   INVALID_PARAMETER              = 7
*   UNDEFIND_ERROR                 = 8
*   DOUBLE_CONVERSION              = 9
*   STEPL_NOT_FOUND                = 10
*   OTHERS                         = 11
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  LOOP AT it_dynpfields.

    IF it_dynpfields-fieldname = 'QMNUM'.
      v_qmnum = it_dynpfields-fieldvalue.
    ELSEIF it_dynpfields-fieldname = 'VN_MATNR'.
      v_matnr = it_dynpfields-fieldvalue.
    ELSEIF it_dynpfields-fieldname = 'ERNAM'.
      v_ernam = it_dynpfields-fieldvalue.
    ELSEIF it_dynpfields-fieldname = 'QMEL-ERDAT'.

      v_sydatum = it_dynpfields-fieldvalue.
      CONCATENATE v_sydatum+6(4) v_sydatum+0(2) v_sydatum+3(2)
                         INTO  v_erdat.
      CLEAR v_sydatum.
    ENDIF.

  ENDLOOP.

  IF NOT v_qmnum IS INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_qmnum
      IMPORTING
        output = v_qmnum.

    SELECT qmnum matnr ernam erdat mzeit objnr FROM qmel
                             INTO CORRESPONDING FIELDS OF TABLE val_tab1
                             WHERE qmnum = v_qmnum.

  ELSEIF NOT v_matnr IS INITIAL.
    SELECT qmnum matnr ernam erdat mzeit objnr FROM qmel
                            INTO CORRESPONDING FIELDS OF TABLE val_tab1
                            WHERE matnr = v_matnr.

  ELSEIF NOT v_ernam IS INITIAL.
    SELECT qmnum matnr ernam erdat mzeit objnr FROM qmel
                            INTO CORRESPONDING FIELDS OF TABLE val_tab1
                            WHERE ernam = v_ernam .
    IF NOT v_erdat IS INITIAL.
      DELETE val_tab1 WHERE erdat <> v_erdat.
    ENDIF.

  ELSEIF NOT v_erdat IS INITIAL.


    SELECT qmnum matnr ernam erdat mzeit objnr FROM qmel
                       INTO CORRESPONDING FIELDS OF TABLE val_tab1
                       WHERE erdat = v_erdat.

  ENDIF.

  LOOP AT val_tab1.
    MOVE-CORRESPONDING val_tab1 TO val_tab.

    SELECT SINGLE stat FROM jest INTO v_stat
                                 WHERE objnr = val_tab1-objnr
                                   AND inact = ' '.
    SELECT SINGLE stsma FROM jsto INTO v_stsma
                                      WHERE objnr = val_tab1-objnr.

    SELECT SINGLE txt30 FROM tj30t INTO v_txt30
                                    WHERE stsma = v_stsma
                                        AND estat = v_stat
                                        AND spras = 'E'.

    SELECT SINGLE fecod FROM qmfe INTO v_fecod
    WHERE qmnum = val_tab1-qmnum.

    val_tab-txt30 = v_txt30.
    APPEND val_tab.

    CLEAR: val_tab, v_stat, v_txt30, v_stsma.

  ENDLOOP.

  SORT val_tab.
  DELETE ADJACENT DUPLICATES FROM val_tab.


  it_dsel-fldname = 'QMNUM'.
  it_dsel-dyfldname = 'QMNUM'.
  APPEND it_dsel.

  it_dsel-fldname = 'MATNR'.
  it_dsel-dyfldname = 'VN_MATNR'.
  APPEND it_dsel.

  it_dsel-fldname = 'ERNAM'.
  it_dsel-dyfldname = 'ERNAM'.
  APPEND it_dsel.

  it_dsel-fldname = 'ERDAT'.
  it_dsel-dyfldname = 'QMEL-ERDAT'.
  APPEND it_dsel.

  it_dsel-fldname = 'MZEIT'.
  it_dsel-dyfldname = 'MZEIT'.
  APPEND it_dsel.

  it_dsel-fldname = 'TXT30'.
  it_dsel-dyfldname = 'TJ30T-TXT30'.
  APPEND it_dsel.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
   ddic_structure         = 'ZSCRAP_VOID_STR'
      retfield               = 'QMEL_QMNUM'
*   PVALKEY                = ' '
    dynpprog               = dyname
    dynpnr                 = dynumb
*   DYNPROFIELD            = ' '
*   STEPL                  = 0
*   WINDOW_TITLE           =
*   VALUE                  = ' '
    value_org              = 'S'
*   MULTIPLE_CHOICE        = ' '
*   DISPLAY                = ' '
*   CALLBACK_PROGRAM       = ' '
*   CALLBACK_FORM          = ' '
    TABLES
    value_tab              = val_tab
*    field_tab             = fld_tab
    return_tab             = ret_tab
    dynpfld_mapping        = it_dsel
* EXCEPTIONS
*   PARAMETER_ERROR        = 1
*   NO_VALUES_FOUND        = 2
*   OTHERS                 = 3
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  LOOP AT ret_tab.
    CASE ret_tab-fieldname.
      WHEN 'QMNUM'.
        qmnum = ret_tab-fieldval.
      WHEN 'MATNR'.
        vn_matnr = ret_tab-fieldval.
      WHEN 'ERNAM'.
        ernam = ret_tab-fieldval.
      WHEN 'ERDAT'.
        v_sydatum = ret_tab-fieldval.
        CONCATENATE v_sydatum+6(4) v_sydatum+0(2) v_sydatum+3(2)
                           INTO  qmel-erdat.
      WHEN 'MZEIT'.
        mzeit = ret_tab-fieldval.
      WHEN 'TXT30'.
        tj30t-txt30 = ret_tab-fieldval.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " display_notif
*&---------------------------------------------------------------------*
*&      Form  create_voidnotif
*&---------------------------------------------------------------------*
*  Creation of Void notification.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_voidnotif.

  DATA: v_objnr TYPE qmel-objnr,
        v_stsma TYPE jsto-stsma,
        v_stat TYPE  jest-stat,
        v_id TYPE thead-tdid,
        v_lang TYPE thead-tdspras,
        v_name TYPE thead-tdname,
        v_obj TYPE thead-tdobject.

  DATA: l_stat LIKE jest-stat.

  DATA : it_tlines LIKE tline OCCURS 0 WITH HEADER LINE,
         it_tline1 LIKE tline OCCURS 0 WITH HEADER LINE.



  PERFORM code_check1.

  SELECT SINGLE matnr objnr  charg  FROM qmel INTO
                             (vn_matnr, v_objnr, mseg-charg)
                                              WHERE qmnum = qmnum.

  IF NOT mseg-charg IS INITIAL.
    marc-xchar = 'X'.
  ENDIF.
  SELECT SINGLE stat FROM jest INTO v_stat WHERE objnr = v_objnr
                                             AND inact = ' '.

  SELECT SINGLE stat INTO l_stat
     FROM jest AS a
     INNER JOIN tj02t AS b
     ON a~stat = b~istat
     WHERE objnr = v_objnr
       AND inact = ' '
       AND spras = 'E'
       AND ( txt04 = 'OSNO' OR txt04 = 'NOPR' ).

  IF sy-subrc <> 0.
    MESSAGE e028(zs).
    EXIT.
  ENDIF.
** Cahnged by Furong on 03/18/09
*  IF V_STAT = 'E0001'.
*    PERFORM CHANGE_TO_VOID TABLES IT_TLINE1 USING QMNUM.
*    PERFORM RELEASE_LOCK USING QMNUM.
*    PERFORM UPDATE_NOTIF_TEXT TABLES IT_TLINE1
*                          USING QMNUM.
*    PERFORM RELEASE_LOCK USING QMNUM.
*    IF EFLAG <> 'X'.
*      PERFORM SET_DEL_NOTIF USING QMNUM.
*    ENDIF.
*    PERFORM GEN_MAIL USING QMNUM.
*
*  ELSEIF ( V_STAT = 'E0004' OR V_STAT = 'E0005' OR V_STAT = 'E0006' ).
*    MESSAGE E021(ZS).

  IF v_stat = 'E0001' OR v_stat = 'E0004' OR v_stat = 'E0005'.
    PERFORM change_to_void TABLES it_tline1 USING qmnum.
    PERFORM release_lock USING qmnum.
    PERFORM update_notif_text TABLES it_tline1
                          USING qmnum.
    PERFORM update_cause_text.
    PERFORM release_lock USING qmnum.
    IF eflag <> 'X'.
      PERFORM set_del_notif USING qmnum.
    ENDIF.
    PERFORM gen_mail USING qmnum.

  ELSEIF v_stat = 'E0006'.
    MESSAGE e021(zs).
** End of change on 03/18/09

  ELSEIF v_stat = 'E0003'.
    MESSAGE e023(zs).

  ELSEIF v_stat = 'E0002'.

    v_id = 'LTQM'.
    v_lang = 'E'.
    v_obj  = 'QMEL'.
    v_name = qmnum.

    PERFORM get_text TABLES it_tlines
                            USING v_id v_lang v_obj v_name.

    READ TABLE it_tlines INDEX 4.

    IF it_tlines-tdline+(7) = 'TRORDER'.
      PERFORM change_to_void TABLES it_tline1
                             USING qmnum.
      PERFORM update_notif_text TABLES it_tline1
                                USING qmnum.
      PERFORM update_cause_text.
      PERFORM release_lock USING qmnum.
      PERFORM set_del_notif USING qmnum.
      PERFORM gen_mail USING qmnum.

    ELSE.
      READ TABLE it_tlines INDEX 3.
      IF it_tlines-tdline CS 'ERROR'.

        PERFORM chg_void USING qmnum.

        PERFORM update_notif_text TABLES it_tline1
                              USING qmnum.
        PERFORM update_cause_text.
        PERFORM release_lock USING qmnum.
        PERFORM set_del_notif USING qmnum.
        PERFORM gen_mail USING qmnum.

      ELSE.

        PERFORM chg_void_with_mvt TABLES it_tline1
                                  USING qmnum.
        PERFORM update_notif_text TABLES it_tline1
                              USING qmnum.
        PERFORM update_cause_text.
        PERFORM release_lock USING qmnum.
        PERFORM set_del_notif USING qmnum.
        PERFORM gen_mail USING qmnum.

      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR: v_objnr,v_stsma,v_stat,v_id,v_lang,v_name,v_obj, eflag.

*  IF SY-UNAME <> '101457' or SY-UNAME <> '100794'.
*    CALL 'SYST_LOGOFF'.
*  ELSE.
*    LEAVE PROGRAM.
*  ENDIF.

ENDFORM.                    " create_voidnotif
*&---------------------------------------------------------------------*
*&      Form  change_to_void
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_QMNUM  text
*----------------------------------------------------------------------*
FORM change_to_void TABLES it_tline STRUCTURE tline
                  USING    p_qmnum.


  DATA: v_mawerk TYPE qmel-mawerk,
        v_lgort TYPE mard-lgort,
        v_bzmng TYPE qmel-bzmng,
        v_lgtyp LIKE pkhd-lgtyp,
        v_lgpla LIKE pkhd-lgpla,
        v_vspvb LIKE marc-vspvb,
        v_lgber LIKE lagp-lgber,
        v_verme LIKE lqua-verme,
        v_temp(10) TYPE n,
        v_id TYPE thead-tdid,
        v_lang TYPE thead-tdspras,
        v_name TYPE thead-tdname,
        v_obj TYPE thead-tdobject.



  DATA : it_tlines LIKE tline OCCURS 0 WITH HEADER LINE.


  SELECT SINGLE mawerk bzmng INTO (v_mawerk, v_bzmng)
                         FROM qmel WHERE qmnum = p_qmnum.

  v_id = 'LTQM'.
  v_lang = 'E'.
  v_obj  = 'QMEL'.
  v_name = qmnum.

  PERFORM get_text TABLES it_tlines
                          USING v_id v_lang v_obj v_name.

  READ TABLE it_tlines INDEX 2.

  v_lgort = it_tlines+18(4).

  CLEAR: it_tlines, it_tlines[].

  IF v_lgort = 'P400'.


    SELECT SINGLE vspvb INTO v_vspvb FROM marc
                                 WHERE matnr = vn_matnr
                                   AND werks = v_mawerk.

    SELECT SINGLE lgtyp lgpla  INTO (v_lgtyp, v_lgpla) FROM pkhd
                               WHERE matnr = vn_matnr
                                 AND werks = v_mawerk
                                 AND prvbe = v_vspvb.

    IF ( v_lgtyp IS INITIAL AND v_lgpla IS INITIAL ).

*      MOVE 'INV_COUNT' TO V_LGPLA.
      MOVE 'HMMA ADJ' TO v_lgpla.
      MOVE '999' TO v_lgtyp.


    ELSE.

      SELECT SINGLE lgber INTO v_lgber FROM lagp
                                       WHERE lgnum = rmmg1-lgnum
                                         AND lgtyp = v_lgtyp
                                         AND lgpla = v_lgpla.


      SELECT SINGLE verme INTO v_verme FROM lqua
                            WHERE lgnum = rmmg1-lgnum
                              AND matnr = vn_matnr
                              AND lgtyp = v_lgtyp
                              AND lgpla = v_lgpla.

      MOVE v_verme TO v_temp.

      IF v_temp < rkmng.
*        MOVE 'INV_COUNT' TO V_LGPLA.
        MOVE 'HMMA ADJ' TO v_lgpla.
        MOVE '999' TO v_lgtyp.
      ENDIF.

    ENDIF.

    PERFORM creat_rev_mvt TABLES it_tline
                          USING v_mawerk v_lgort v_bzmng.

** Changed by Furong on 07/19/11
*    PERFORM CREATE_REV_TO TABLES IT_TLINE USING V_LGTYP V_LGPLA V_LGBER
** End on 07/19/11

** Changed by Furong on 10/19/09
*  ELSEIF V_LGORT = 'P500' .
  ELSE.
** End of change
    PERFORM creat_rev_mvt TABLES it_tline
                          USING v_mawerk v_lgort v_bzmng.

  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_qmnum
    IMPORTING
      output = p_qmnum.

  IF eflag <> 'X'.

    CALL FUNCTION 'IQS0_CHANGE_NOTIF_USER_STATUS'
      EXPORTING
        i_qmnum                    = p_qmnum
        i_user_stat_intern         = 'E0003'
        i_user_stat_extern         = 'VOID'
        i_langu                    = sy-langu
* IMPORTING
*   E_USER_STAT_INTERN         =
*   E_USER_STAT_EXTERN         =
      EXCEPTIONS
        foreign_lock               = 1
*   SYST_FAILURE               = 2
        invalid_notification       = 3
*   OBJECT_NOT_FOUND           = 4
        status_inconsistant        = 5
        status_not_allowed         = 6
*   OTHERS                     = 7
              .
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.

      COMMIT WORK AND WAIT.

      MESSAGE i022(zs).

    ENDIF.

  ELSE.

    CALL FUNCTION 'IQS0_CHANGE_NOTIF_USER_STATUS'
    EXPORTING
      i_qmnum                    =  p_qmnum
      i_user_stat_intern         = 'E0002'
      i_user_stat_extern         = 'EROR'
      i_langu                    = sy-langu
* IMPORTING
*   E_USER_STAT_INTERN         =
*   E_USER_STAT_EXTERN         =
    EXCEPTIONS
      foreign_lock               = 1
*   SYST_FAILURE               = 2
      invalid_notification       = 3
*   OBJECT_NOT_FOUND           = 4
      status_inconsistant        = 5
      status_not_allowed         = 6
*   OTHERS                     = 7
            .
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.

      COMMIT WORK AND WAIT.

      MESSAGE i022(zs).

    ENDIF.

  ENDIF.


  CLEAR: v_mawerk,
      v_lgort,
      v_bzmng,
      v_lgtyp,
      v_lgpla,
      v_vspvb,
      v_lgber,
      v_verme,
      v_temp,
      v_id,
      v_lang,
      v_name,
      v_obj.



ENDFORM.                    " change_to_void

*&---------------------------------------------------------------------*
*&      Form  create_rev_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_LGTYP  text
*      -->P_V_LGPLA  text
*      -->P_V_LGBER  text
*----------------------------------------------------------------------*
FORM create_rev_to TABLES   p_it_tline
                   USING    p_lgtyp
                            p_lgpla
                            p_lgber.

  DATA : v_uom LIKE mara-meins.

  SELECT SINGLE meins FROM mara INTO v_uom WHERE matnr = vn_matnr.


  PERFORM bdc_dynpro      USING 'SAPML02B' '0203'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RL02B-MBLNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RL02B-MBLNR'
                                 mdoc.
  PERFORM bdc_field       USING 'RL02B-MJAHR'
                                 myear.
  PERFORM bdc_field       USING 'RL02B-LGNUM'
                                 'P01'.
  PERFORM bdc_field       USING 'RL02B-DUNKL'
                                'H'.
  PERFORM bdc_dynpro      USING 'SAPML03T' '0132'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=TTYP'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LTBP1-OFMEA(01)'.
  PERFORM bdc_dynpro      USING 'SAPML03T' '0105'.

  PERFORM bdc_field       USING 'BDC_CURSOR'
                              'LTAP-NLPLA'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'LTAP-NLTYP'       " Dest Storage
                                 p_lgtyp.
*  PERFORM bdc_field       USING 'LTAP-NLBER'
*                                 p_lgber.
  PERFORM bdc_field       USING 'LTAP-NLPLA'       " Dest storage Bin
                                p_lgpla.
  PERFORM bdc_dynpro      USING 'SAPML03T' '0105'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'T334T-LGTY0'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM bdc_transaction TABLES p_it_tline
                          USING 'LT06'.



ENDFORM.                    " create_rev_to
*&---------------------------------------------------------------------*
*&      Form  bdc_transaction1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2515   text
*----------------------------------------------------------------------*
FORM bdc_transaction1 USING tcode.

  DATA: l_subrc LIKE sy-subrc,
        msg(255).

* call transaction using

  REFRESH: messtab.
  CLEAR : msg,mdoc, myear.
  CALL TRANSACTION tcode USING bdcdata
                   MODE   ctumode
                   UPDATE cupdate
                   MESSAGES INTO messtab.
  l_subrc = sy-subrc.

  READ TABLE messtab WITH KEY msgtyp = 'E'.

  IF sy-subrc = 0.
    LOOP AT messtab.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH messtab-msgv1 messtab-msgv2 messtab-msgv3
                 messtab-msgv4.

      CONCATENATE msg messtab-msgv1 INTO msg.
    ENDLOOP.

  ELSE.
    READ TABLE messtab WITH KEY msgtyp = 'S'.
    IF sy-subrc = 0.
      msg = messtab-msgv1.
    ELSE.
      LOOP AT messtab.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH messtab-msgv1 messtab-msgv2 messtab-msgv3
                   messtab-msgv4.

        CONCATENATE msg messtab-msgv1 INTO msg.
      ENDLOOP.
    ENDIF.
    it_qmnum-qmnum = messtab-msgv1.
    it_qmnum-matnr = matnr.
    APPEND it_qmnum.
    CLEAR it_qmnum.
    REFRESH bdcdata.
    IF tcode = 'QM01'.
      MESSAGE i003(zs) WITH msg.
    ENDIF.
  ENDIF.
  REFRESH: bdcdata.
ENDFORM.                    " bdc_transaction1
*&---------------------------------------------------------------------*
*&      Form  update_notif_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE  text
*      -->P_V_QMNUM  text
*----------------------------------------------------------------------*
FORM update_notif_text TABLES   p_it_tline STRUCTURE tline
                       USING    p_v_qmnum.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_v_qmnum
    IMPORTING
      output = p_v_qmnum.


  CALL FUNCTION 'IQS0_ADD_NOTIFICATION_LONGTEXT'
    EXPORTING
      i_qmnum             = p_v_qmnum
      i_post              = 'X'
*   I_RESET             =
    TABLES
      t_inlines           = p_it_tline
* EXCEPTIONS
*   SHOW_MESSAGES       = 1
*   OTHERS              = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    COMMIT WORK AND WAIT.

  ENDIF.


ENDFORM.                    " update_notif_text
*&---------------------------------------------------------------------*
*&      Form  update_notif_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_QMNUM  text
*----------------------------------------------------------------------*
FORM update_notif_status USING    p_v_qmnum.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_v_qmnum
    IMPORTING
      output = p_v_qmnum.


  CALL FUNCTION 'IQS0_CHANGE_NOTIF_USER_STATUS'
    EXPORTING
      i_qmnum                    =  p_v_qmnum
      i_user_stat_intern         = 'E0002'
      i_user_stat_extern         = 'EROR'
      i_langu                    = sy-langu
* IMPORTING
*   E_USER_STAT_INTERN         =
*   E_USER_STAT_EXTERN         =
    EXCEPTIONS
      foreign_lock               = 1
*   SYST_FAILURE               = 2
      invalid_notification       = 3
*   OBJECT_NOT_FOUND           = 4
      status_inconsistant        = 5
      status_not_allowed         = 6
*   OTHERS                     = 7
            .
  IF sy-subrc <> 0.
    MESSAGE s000 WITH p_v_qmnum.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.

    COMMIT WORK AND WAIT.

  ENDIF.


ENDFORM.                    " update_notif_status
*&---------------------------------------------------------------------*
*&      Form  chg_void
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_QMNUM  text
*----------------------------------------------------------------------*
FORM chg_void USING    p_qmnum.


  CALL FUNCTION 'IQS0_CHANGE_NOTIF_USER_STATUS'
    EXPORTING
      i_qmnum                    = p_qmnum
      i_user_stat_intern         = 'E0003'
      i_user_stat_extern         = 'VOID'
      i_langu                    = sy-langu
* IMPORTING
*   E_USER_STAT_INTERN         =
*   E_USER_STAT_EXTERN         =
    EXCEPTIONS
      foreign_lock               = 1
*   SYST_FAILURE               = 2
      invalid_notification       = 3
*   OBJECT_NOT_FOUND           = 4
      status_inconsistant        = 5
      status_not_allowed         = 6
*   OTHERS                     = 7
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.

    COMMIT WORK AND WAIT.

    MESSAGE i022(zs).

  ENDIF.

ENDFORM.                    " chg_void
*&---------------------------------------------------------------------*
*&      Form  creat_rev_mvt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE  text
*      -->P_V_MAWERK  text
*      -->P_V_LGORT  text
*      -->P_V_BZMNG  text
*----------------------------------------------------------------------*
FORM creat_rev_mvt TABLES   p_it_tline STRUCTURE tline
                   USING    p_mawerk
                            p_lgort
                            p_bzmng.


  DATA : it_header LIKE bapi2017_gm_head_01,
         code LIKE bapi2017_gm_code VALUE '04',
         v_uom LIKE mara-meins.

  DATA: it_item LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE,
        it_ret LIKE bapiret2 OCCURS 0 WITH HEADER LINE.


  SELECT SINGLE meins FROM mara INTO v_uom WHERE matnr = matnr.

  it_header-pstng_date  = sy-datum.
  it_header-doc_date     = sy-datum.
  it_item-material      = vn_matnr.
  it_item-plant         = p_mawerk.

  IF p_mawerk = 'E001'.
    it_item-stge_loc      = 'E499'.
** changed on 12/13/11
  ELSEIF p_mawerk = 'E002'.
    it_item-stge_loc      = 'N499'.
** end on 12/13/11
  ELSE.
    it_item-stge_loc      = 'P499'.
  ENDIF.

  it_item-move_type     = '311'.
  it_item-entry_qnt     = p_bzmng.
  it_item-entry_uom     = v_uom.
  it_item-move_plant    = p_mawerk.
  it_item-move_stloc    = p_lgort.
  it_item-move_reas     = v_fecod. "03.28.2014 Victor
  it_item-mvt_ind       = ' '.
** Changed by Furong on 10/19/09
  IF marc-xchar = 'X'.
    it_item-batch  = mseg-charg.
  ENDIF.
** End of change
  APPEND it_item.
  CLEAR it_item.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header       = it_header
      goodsmvt_code         = code
*     TESTRUN               = ' '
    IMPORTING
*     GOODSMVT_HEADRET      =
      materialdocument      = mdoc
      matdocumentyear       = myear
    TABLES
      goodsmvt_item         = it_item
*     GOODSMVT_SERIALNUMBER =
      return                = it_ret.


  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
             wait          = 'X'
*           IMPORTING
*             RETURN        =
            .


  IF mdoc IS INITIAL.
    p_it_tline-tdformat = '*' .
    CONCATENATE 'ERROR: ' it_ret-message INTO p_it_tline-tdline.
    APPEND p_it_tline.
    CLEAR p_it_tline.
    eflag = 'X'.
  ELSE.

    p_it_tline-tdformat = '*' .
    CONCATENATE '@2 Material DOC:' mdoc myear INTO p_it_tline-tdline
           SEPARATED BY space.

*    CONCATENATE '@2 Material DOC:' MDOC INTO P_IT_TLINE-TDLINE.
    APPEND p_it_tline.
    CLEAR p_it_tline.

    COMMIT WORK.

  ENDIF.
ENDFORM.                    " creat_rev_mvt
*&---------------------------------------------------------------------*
*&      Form  get_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINES  text
*      -->P_V_ID  text
*      -->P_V_LANG  text
*      -->P_V_OBJ  text
*----------------------------------------------------------------------*
FORM get_text TABLES   p_it_tlines STRUCTURE tline
              USING    p_v_id
                       p_v_lang
                       p_v_obj
                       p_v_name.


*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*       EXPORTING
*            input  = p_v_name
*       IMPORTING
*            output = p_v_name.


  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*         CLIENT                        = SY-MANDT
      id                            = p_v_id
      language                      = p_v_lang
      name                          = p_v_name
      object                        = p_v_obj
*         ARCHIVE_HANDLE                = 0
*         LOCAL_CAT                     = ' '
*       IMPORTING
*         HEADER                        =
    TABLES
      lines                         = p_it_tlines
*       EXCEPTIONS
*         ID                            = 1
*         LANGUAGE                      = 2
*         NAME                          = 3
*         NOT_FOUND                     = 4
*         OBJECT                        = 5
*         REFERENCE_CHECK               = 6
*         WRONG_ACCESS_TO_ARCHIVE       = 7
*         OTHERS                        = 8
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " get_text
*&---------------------------------------------------------------------*
*&      Form  chg_void_with_mvt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE1  text
*      -->P_QMNUM  text
*----------------------------------------------------------------------*
FORM chg_void_with_mvt TABLES   p_it_tline1 STRUCTURE tline
                       USING    p_qmnum.

  DATA: v_mawerk TYPE qmel-mawerk,
          v_lgort TYPE mard-lgort,
          v_bzmng TYPE qmel-bzmng,
          v_lgtyp LIKE pkhd-lgtyp,
          v_lgpla LIKE pkhd-lgpla,
          v_vspvb LIKE marc-vspvb,
          v_lgber LIKE lagp-lgber,
          v_verme LIKE lqua-verme,
          v_temp(10) TYPE n,
          v_id TYPE thead-tdid,
          v_lang TYPE thead-tdspras,
          v_name TYPE thead-tdname,
          v_obj TYPE thead-tdobject.



  DATA : it_tlines LIKE tline OCCURS 0 WITH HEADER LINE.


  SELECT SINGLE mawerk bzmng INTO (v_mawerk, v_bzmng)
                         FROM qmel WHERE qmnum = p_qmnum.

  v_id = 'LTQM'.
  v_lang = 'E'.
  v_obj  = 'QMEL'.
  v_name = qmnum.

  PERFORM get_text TABLES it_tlines
                          USING v_id v_lang v_obj v_name.

  READ TABLE it_tlines INDEX 2.

  v_lgort = it_tlines+18(4).

  CLEAR: it_tlines, it_tlines[].


  SELECT SINGLE vspvb INTO v_vspvb FROM marc
                               WHERE matnr = vn_matnr
                                 AND werks = v_mawerk.

  SELECT SINGLE lgtyp lgpla  INTO (v_lgtyp, v_lgpla) FROM pkhd
                             WHERE matnr = vn_matnr
                               AND werks = v_mawerk
                               AND prvbe = v_vspvb.

  IF ( v_lgtyp IS INITIAL AND v_lgpla IS INITIAL ).

*    MOVE 'INV_COUNT' TO V_LGPLA.
    MOVE 'HMMA ADJ' TO v_lgpla.
    MOVE '999' TO v_lgtyp.


  ELSE.

    SELECT SINGLE lgber INTO v_lgber FROM lagp
                                     WHERE lgnum = rmmg1-lgnum
                                       AND lgtyp = v_lgtyp
                                       AND lgpla = v_lgpla.


    SELECT SINGLE verme INTO v_verme FROM lqua
                          WHERE lgnum = rmmg1-lgnum
                            AND matnr = vn_matnr
                            AND lgtyp = v_lgtyp
                            AND lgpla = v_lgpla.

    MOVE v_verme TO v_temp.

    IF v_temp < rkmng.
*      MOVE 'INV_COUNT' TO V_LGPLA.
      MOVE 'HMMA ADJ' TO v_lgpla.
      MOVE '999' TO v_lgtyp.
    ENDIF.

  ENDIF.

  PERFORM creat_rev_mvt TABLES p_it_tline1
                        USING v_mawerk v_lgort v_bzmng.


  IF eflag <> 'X'.

    CALL FUNCTION 'IQS0_CHANGE_NOTIF_USER_STATUS'
     EXPORTING
       i_qmnum                    = p_qmnum
       i_user_stat_intern         = 'E0003'
       i_user_stat_extern         = 'VOID'
       i_langu                    = sy-langu
* IMPORTING
*   E_USER_STAT_INTERN         =
*   E_USER_STAT_EXTERN         =
     EXCEPTIONS
       foreign_lock               = 1
*   SYST_FAILURE               = 2
       invalid_notification       = 3
*   OBJECT_NOT_FOUND           = 4
       status_inconsistant        = 5
       status_not_allowed         = 6
*   OTHERS                     = 7
             .
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.

      COMMIT WORK AND WAIT.

      MESSAGE i022(zs).

    ENDIF.

  ENDIF.

  CLEAR eflag.

ENDFORM.                    " chg_void_with_mvt
*&---------------------------------------------------------------------*
*&      Form  set_del_notif
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_QMNUM  text
*----------------------------------------------------------------------*
FORM set_del_notif USING    p_qmnum.


  PERFORM bdc_dynpro      USING 'SAPLIQS0' '0200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RIWO00-QMNUM'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RIWO00-QMNUM'
                                p_qmnum.
  PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=LVMS'.
  PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BUCH'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'VIQMEL-QMGRP'.

  PERFORM bdc_transaction1 USING 'QM02'.

ENDFORM.                    " set_del_notif
*&---------------------------------------------------------------------*
*&      Form  gen_mail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gen_mail USING    p_qmnum.


  DATA:  v_mawerk TYPE qmel-mawerk,
          v_lgort TYPE mard-lgort,
          v_bzmng TYPE qmel-bzmng,
          v_lgtyp LIKE pkhd-lgtyp,
          v_ltkze LIKE mlgn-ltkze,
          v_vspvb LIKE marc-vspvb,
          v_lgber LIKE lagp-lgber,
          v_sub(30) TYPE c,
          v_temp(10) TYPE n,
          v_id TYPE thead-tdid,
          v_lang TYPE thead-tdspras,
          v_name TYPE thead-tdname,
          v_obj TYPE thead-tdobject,
          v_qmgrp TYPE qmel-qmgrp,
          v_matnr TYPE qmel-matnr.



  DATA : it_tlines LIKE tline OCCURS 0 WITH HEADER LINE.

  SELECT SINGLE matnr mawerk bzmng qmgrp
                        INTO (v_matnr, v_mawerk, v_bzmng, v_qmgrp)
                         FROM qmel WHERE qmnum = p_qmnum.

  v_id = 'LTQM'.
  v_lang = 'E'.
  v_obj  = 'QMEL'.
  v_name = qmnum.

  PERFORM get_text TABLES it_tlines
                          USING v_id v_lang v_obj v_name.

  READ TABLE it_tlines INDEX 2.

  v_lgort = it_tlines+18(4).

  CLEAR: it_tlines, it_tlines[].


  IF v_lgort = 'P400'.

    SELECT SINGLE vspvb INTO v_vspvb FROM marc
                                 WHERE matnr = v_matnr
                                   AND werks = v_mawerk.

    SELECT SINGLE lgtyp INTO v_lgtyp FROM pkhd
                               WHERE matnr = v_matnr
                                 AND werks = v_mawerk
                                 AND prvbe = v_vspvb.

    IF v_lgtyp = '422'.

      SELECT SINGLE ltkze FROM mlgn INTO v_ltkze
                        WHERE matnr = v_matnr AND lgnum = 'P01'.
      IF v_ltkze = '003'.

        IF v_qmgrp = 'MXTX53'.

          v_sub = 'Cancellation of PDI reorder'.

          PERFORM send_mail1 USING v_sub p_qmnum
                                   v_matnr v_bzmng v_qmgrp.

        ELSE.

          v_sub = 'Cancellation of Line side Reorder'.

          PERFORM send_mail1 USING v_sub p_qmnum
                                   v_matnr v_bzmng v_qmgrp.
        ENDIF.
      ENDIF.

    ENDIF.

*  ELSEIF V_LGORT = 'P500'.
  ELSE.
    IF v_qmgrp = 'MXTX53'.

      v_sub = 'Cancellation of PDI reorder'.

      PERFORM send_mail1 USING v_sub p_qmnum
                               v_matnr v_bzmng v_qmgrp.

    ELSE.

      v_sub = 'Cancellation of Line side Reorder'.

      PERFORM send_mail1 USING v_sub p_qmnum
                               v_matnr v_bzmng v_qmgrp.
    ENDIF.

  ENDIF.


  CLEAR:  v_mawerk,
        v_lgort,
        v_bzmng,
        v_lgtyp,
        v_ltkze,
        v_vspvb,
        v_lgber,
        v_sub,
        v_temp,
        v_id,
        v_lang,
        v_name,
        v_obj,
        v_qmgrp,
        v_matnr.

ENDFORM.                    " gen_mail
*&---------------------------------------------------------------------*
*&      Form  send_mail1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_SUB  text
*      -->P_QMNUM  text
*      -->P_V_MATNR  text
*      -->P_V_BZMNG  text
*----------------------------------------------------------------------*
FORM send_mail1 USING    p_sub
                         p_qmnum
                         p_matnr
                         p_bzmng
                         p_qmgrp.

  DATA: t_objbin  LIKE solisti1  OCCURS 10 WITH HEADER LINE ,
          t_objpack LIKE sopcklsti1 OCCURS 1  WITH HEADER LINE,
          t_objhead LIKE solisti1   OCCURS 1  WITH HEADER LINE,
          t_objtxt  LIKE solisti1  OCCURS 10 WITH HEADER LINE ,
          t_reclist LIKE somlreci1 OCCURS 1  WITH HEADER LINE ,
          w_docdata   LIKE sodocchgi1.

  DATA: l_text(400) TYPE c,
        v_quan(10) TYPE c.

  DATA: t_compressed_list  LIKE soli OCCURS 0,
        t_listobject LIKE abaplist  OCCURS 1  WITH HEADER LINE.

*-Receiver List
  CLEAR t_reclist.
  REFRESH t_reclist.

*-External emails
**S> 08/04/11 Paul
  t_reclist-receiver = 'pcscrapreorder@hmmausa.COM'.
**E<
  t_reclist-com_type =  'INT'.
  t_reclist-rec_type = 'U'.
  t_reclist-express  = 'X'.
  APPEND t_reclist.
  CLEAR  t_reclist.

*-Message Body
  REFRESH t_objtxt.
  CLEAR   t_objtxt.


  CONCATENATE 'Quality Notification #' p_qmnum
                                       INTO t_objtxt-line.
  APPEND t_objtxt.

  CONCATENATE 'Material #' p_matnr INTO t_objtxt-line.
  APPEND t_objtxt.

  MOVE p_bzmng TO v_quan.

  CONCATENATE  'Quantity' v_quan INTO t_objtxt-line.
  APPEND t_objtxt.
  CLEAR t_objtxt.

  CONCATENATE  'Delivery Location ' p_qmgrp INTO t_objtxt-line.
  APPEND t_objtxt.
  CLEAR t_objtxt.

  DATA: w_tab_lines  TYPE i.

*-Creation of document to be sent
  w_docdata-obj_name = 'EMAIL'.
  w_docdata-obj_descr = p_sub.  " Subject Line

*-Contents of Email
  CLEAR w_tab_lines.
  DESCRIBE TABLE t_objtxt LINES w_tab_lines.
  READ TABLE t_objtxt INDEX w_tab_lines.
  w_docdata-doc_size = ( w_tab_lines - 1 ) * 255 + strlen( t_objtxt ).

*-Creation of entry for compressed document
  CLEAR t_objpack-transf_bin.
  t_objpack-head_start = 1.
  t_objpack-head_num = 1.
  t_objpack-body_start = 1.
  t_objpack-body_num = w_tab_lines.
  t_objpack-doc_type = 'TXT'.
  APPEND t_objpack.
  CLEAR t_objpack.

*-Creation of document attachment
  CONCATENATE p_sub sy-datum INTO t_objhead.
  APPEND t_objhead.

  CLEAR w_tab_lines.
  DESCRIBE TABLE t_objbin LINES w_tab_lines.
  t_objpack-doc_size = w_tab_lines * 255.
  t_objpack-transf_bin = 'X'.
  t_objpack-head_start = 1.
  t_objpack-head_num = 1.
  t_objpack-body_start = 1.
  t_objpack-body_num = w_tab_lines.
  t_objpack-doc_type = 'TXT'.
  t_objpack-obj_name = 'ATTACHMENT'.
  t_objpack-obj_descr = p_sub.  " Attached file name
  APPEND t_objpack.
  CLEAR  t_objpack.


  DATA: sent_to_all LIKE sonv-flag,
        user_address LIKE sousradri1 OCCURS 1 WITH HEADER LINE.
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = w_docdata
      put_in_outbox              = 'X'
      commit_work                = 'X'
    IMPORTING
      sent_to_all                = sent_to_all
    TABLES
      packing_list               = t_objpack
      object_header              = t_objhead
      contents_bin               = t_objbin
      contents_txt               = t_objtxt
      receivers                  = t_reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  SKIP 1.
  CASE sy-subrc.
    WHEN 0.
      WRITE: / 'Result of the sendprocess:'.
    WHEN 1.
      WRITE: / 'No permission to sent to the specified  ',
                'amount of recipients !'.
    WHEN 2.
      WRITE: / 'Document could not be sent to any recipient !'.
    WHEN 4.
      WRITE: / 'No permission to send !'.
    WHEN OTHERS.
      WRITE: / 'Error while sending !'.
  ENDCASE.

ENDFORM.                    " send_mail1
