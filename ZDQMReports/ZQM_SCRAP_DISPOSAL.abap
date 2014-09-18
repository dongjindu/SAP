************************************************************************
* Author            : Furong Wang
* Creation Date     : 08/2009
* Specifications By : Matthew Cupples
* Pattern           :
* Development Request
* Addl Documentation:
* Description       :
*
* Modification Logs
*
************************************************************************
REPORT zqm_scrap_disposal NO STANDARD PAGE HEADING
                          LINE-SIZE 132
                          LINE-COUNT 64(1)
                          MESSAGE-ID zmmm.

CONSTANTS: c_hmma(4) VALUE 'HMMA',
           c_vend(4) VALUE 'VEND',
           c_stck(4) VALUE 'STCK'.

DATA:  ctumode LIKE ctu_params-dismode VALUE 'N',
       cupdate LIKE ctu_params-updmode VALUE 'A',
       bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE,
       messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF lt_temp OCCURS 0,
      qmnum LIKE viqmel-qmnum,
      udate LIKE jcds-udate,
      utime LIKE jcds-utime,
      chind LIKE jcds-chind,
      END OF lt_temp.

DATA: BEGIN OF it_itab OCCURS 0,
      qmnum LIKE viqmel-qmnum,
      txt04 LIKE tj30t-txt04,
      END OF it_itab.
*
DATA: mdoc LIKE bapi2017_gm_head_ret-mat_doc,
      myear LIKE bapi2017_gm_head_ret-doc_year.

DATA: eflag,
      rflag,
      rmess(255).

DATA: wtab(72) OCCURS 100 WITH HEADER LINE,
      w_or(2),
      w_select(1).

DATA: BEGIN OF it_log OCCURS 0,
      qmnum LIKE viqmel-qmnum,
      result(1),
      mess(255),
      END OF it_log.

DATA: w_result(1),
      w_mess(255),
      w_interval LIKE sy-uzeit,
      w_date LIKE sy-datum,
      w_time LIKE sy-uzeit,
      w_xchar LIKE marc-xchar,
      w_charg LIKE qmel-charg..
DATA: l_hr TYPE i.

*SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-a01.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN  BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) text-u04 FOR FIELD p_all.
PARAMETERS: p_all AS CHECKBOX USER-COMMAND chal DEFAULT 'X'.
SELECTION-SCREEN  END OF LINE.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN  BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(6) text-u01 FOR FIELD p_hmma.
PARAMETERS: p_hmma AS CHECKBOX MODIF ID abc USER-COMMAND chot.
SELECTION-SCREEN COMMENT 15(6) text-u02 FOR FIELD p_vend.
PARAMETERS: p_vend AS CHECKBOX MODIF ID abc USER-COMMAND chot.
SELECTION-SCREEN COMMENT 30(6) text-u03 FOR FIELD p_stck.
PARAMETERS: p_stck AS CHECKBOX MODIF ID abc USER-COMMAND chot.
SELECTION-SCREEN  END OF LINE.
SELECTION-SCREEN END   OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-a02.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_date LIKE sy-datum DEFAULT '20090301'.
SELECTION-SCREEN END   OF BLOCK block2.

AT SELECTION-SCREEN OUTPUT.
*  PERFORM MODIFY_SCREEN.

AT SELECTION-SCREEN.
  IF sy-ucomm = 'CHAL'.
    PERFORM modify_screen_all.
  ENDIF.
  IF sy-ucomm = 'CHOT'.
    PERFORM modify_screen_other.
  ENDIF.

START-OF-SELECTION.
  w_or = 'OR'.
  CLEAR: w_select.

  IF p_all = 'X'.
    CONCATENATE 'TXT04 = ''' c_hmma '''' INTO wtab.
    APPEND wtab.
    CONCATENATE w_or ' TXT04 = ''' c_vend '''' INTO wtab.
    APPEND wtab.
    CONCATENATE w_or ' TXT04 = ''' c_stck '''' INTO wtab.
    APPEND wtab.
  ELSE.
    IF p_hmma = 'X'.
      CONCATENATE 'TXT04 = ''' c_hmma '''' INTO wtab.
      APPEND wtab.
      w_select = 'X'.
    ENDIF.
    IF p_vend = 'X'.
      IF  w_select IS INITIAL.
        CONCATENATE 'TXT04 = ''' c_vend '''' INTO wtab.
      ELSE.
        CONCATENATE w_or ' TXT04 = ''' c_vend '''' INTO wtab.
      ENDIF.
      APPEND wtab.
      w_select = 'X'.
    ENDIF.
    IF p_stck = 'X'.
      IF  w_select IS INITIAL.
        CONCATENATE 'TXT04 = ''' c_stck '''' INTO wtab.
      ELSE.
        CONCATENATE w_or ' TXT04 = ''' c_stck '''' INTO wtab.
      ENDIF.
      APPEND wtab.
    ENDIF.
  ENDIF.

  SELECT qmnum udate utime chind
    INTO CORRESPONDING FIELDS OF TABLE lt_temp
    FROM viqmel AS c
    INNER JOIN jest AS d
    ON c~objnr = d~objnr
     INNER JOIN jcds AS e
    ON d~objnr = e~objnr
   AND  d~stat = e~stat
     INNER JOIN tj02t AS f
      ON d~stat = f~istat
      WHERE qmart = 'Q3'
      AND erdat >= p_date
      AND txt04 = 'NOPR'
      AND d~inact = ' '
      AND chind = 'I'
      AND f~spras = 'EN'.

  IF lt_temp[] IS INITIAL.
    MESSAGE e009 WITH 'No Data Was Selected'.
  ENDIF.
  w_date = sy-datum.
  w_time = sy-uzeit.

  LOOP AT lt_temp.
    IF w_date > lt_temp-udate.
      CONTINUE.
    ENDIF.

    w_interval = w_time - lt_temp-utime.
    l_hr = w_interval+0(2).

    IF l_hr >= 1.
      CONTINUE.
    ENDIF.

*    IF W_INTERVAL >= 10000.
*      CONTINUE.
*    ENDIF.

    DELETE lt_temp.
  ENDLOOP.

  IF NOT lt_temp[] IS INITIAL.
    SELECT qmnum txt04 INTO TABLE it_itab
        FROM viqmel AS c
        INNER JOIN jest AS d
        ON c~objnr = d~objnr
          INNER JOIN tj30 AS e
          ON d~stat = e~estat
          INNER JOIN tj30t AS f
          ON e~stsma = f~stsma
          AND e~estat = f~estat
          FOR ALL ENTRIES IN lt_temp
          WHERE c~qmnum = lt_temp-qmnum
*        AND TXT04 IN ('HMMA', 'VEND', 'STCK')
          AND ( (wtab) )
          AND f~stsma = 'ZQNSCRP1'
          AND inact = ' '
          AND f~spras = 'EN'.

    LOOP AT it_itab.
      CLEAR: w_result, w_mess .

      CASE it_itab-txt04.
        WHEN 'VEND'.
          PERFORM process_vend USING it_itab-qmnum w_result w_mess.
        WHEN 'HMMA'.
          PERFORM process_hmma USING it_itab-qmnum w_result w_mess .
        WHEN 'STCK'.
          PERFORM process_stck USING it_itab-qmnum w_result w_mess.
        WHEN OTHERS.
          w_result = 'E'.
          w_mess =
          'Tag cannot be completed. Please notify PRODUCTION CONTROL'.
      ENDCASE.

      IF w_result = 'E'.
        it_log-qmnum = it_itab-qmnum.
        it_log-result = w_result.
        it_log-mess = w_mess.
        APPEND it_log.
        CLEAR: it_log.
      ENDIF.
    ENDLOOP.

    IF it_log[] IS INITIAL.
      WRITE: /5 'Data was successfully processed'.
    ELSE.
      LOOP AT it_log.
        WRITE: /1 it_log-qmnum,
                20 it_log-result,
                30 it_log-mess.
      ENDLOOP.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  PROCESS_VEND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_vend USING p_qmnum p_result p_mess.

*  DATA: MDOC LIKE BAPI2017_GM_HEAD_RET-MAT_DOC,
*        MYEAR LIKE BAPI2017_GM_HEAD_RET-DOC_YEAR.

*  DATA:  CTUMODE LIKE CTU_PARAMS-DISMODE VALUE 'N',
*        CUPDATE LIKE CTU_PARAMS-UPDMODE VALUE 'A',
*        BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE,
*        MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
  DATA: it_tline1 LIKE tline OCCURS 0 WITH HEADER LINE.
  DATA: l_profl LIKE mara-profl,
        l_mtart LIKE mara-mtart,
        l_qmnum(12).

  CLEAR: eflag, rflag, rmess.
  CLEAR: mdoc, myear.

  SELECT SINGLE profl mtart INTO (l_profl, l_mtart)
    FROM mara AS a
    INNER JOIN qmel AS b
    ON a~matnr = b~matnr
    WHERE qmnum = p_qmnum.

  IF l_mtart = 'HALB'.
** furong on 07/16/12
*    PERFORM PROCESS_HMMA USING P_QMNUM P_RESULT P_MESS .
** End
    EXIT.
  ENDIF.

  SELECT SINGLE qmnum INTO l_qmnum
        FROM viqmel AS c
        INNER JOIN jest AS d
        ON c~objnr = d~objnr
          INNER JOIN tj30 AS e
          ON d~stat = e~estat
         WHERE c~qmnum = p_qmnum
          AND  stsma = 'ZQNSCRP1'
          AND estat = 'E0009'
          AND inact = ' '.

  IF sy-subrc = 0.
    PERFORM return_to_glaccount TABLES it_tline1 USING p_qmnum
                                '123206'.

  ELSE.
    IF l_profl = 'K'.
      PERFORM return_to_glaccount TABLES it_tline1 USING p_qmnum
                                     '123200'.
    ELSE.
      PERFORM return_to_vendor TABLES it_tline1 USING p_qmnum.
    ENDIF.
  ENDIF.
** Changed on 09/30/09
*  IF RFLAG <> 'E'.
** End of change
  PERFORM release_lock USING p_qmnum.
  PERFORM update_notif_text TABLES it_tline1
                        USING p_qmnum.
  PERFORM release_lock USING p_qmnum.
  IF eflag <> 'X'.
    PERFORM set_comp_notif USING p_qmnum.
  ELSE.
    PERFORM update_notif_status USING p_qmnum.
  ENDIF.
*  ENDIF.
  p_result = rflag.
  p_mess = rmess.
ENDFORM.                    " PROCESS_VEND
*&---------------------------------------------------------------------*
*&      Form  PROCESS_HMMA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_hmma USING p_qmnum  p_result p_mess.
  CLEAR: eflag.
  DATA: it_tline1 LIKE tline OCCURS 0 WITH HEADER LINE.
  DATA: l_werks LIKE qmel-mawerk,
        l_matnr LIKE qmel-matnr,
        l_fevor LIKE marc-fevor.
** Chagned on 05/30/12 for engine 3c parts
*     PERFORM HMMA_SCRAP TABLES IT_TLINE1 USING P_QMNUM.
  SELECT SINGLE mawerk matnr INTO (l_werks, l_matnr)
            FROM qmel WHERE qmnum = p_qmnum.
  SELECT SINGLE fevor INTO l_fevor
            FROM marc WHERE matnr = l_matnr
                       AND werks = l_werks.

  IF l_werks+0(1) = 'E' AND l_fevor = 'SEC'.
    PERFORM hmma_scrap_311 TABLES it_tline1 USING p_qmnum.
  ELSE.
    PERFORM hmma_scrap_511 TABLES it_tline1 USING p_qmnum.
  ENDIF.
** End on 05/30/12
  PERFORM release_lock USING p_qmnum.
  IF it_tline1[] IS INITIAL.
  ELSE.
    PERFORM update_notif_text TABLES it_tline1
                          USING p_qmnum.
    PERFORM release_lock USING p_qmnum.
  ENDIF.
  IF eflag <> 'X'.
    PERFORM set_comp_notif USING p_qmnum.
  ELSE.
    PERFORM update_notif_status USING p_qmnum.
  ENDIF.
  p_result = rflag.
  p_mess = rmess.
ENDFORM.                    " PROCESS_HMMA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_STCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_stck USING p_qmnum p_result p_message.
  CLEAR: eflag.
  DATA: it_tline1 LIKE tline OCCURS 0 WITH HEADER LINE.
  PERFORM change_to_void TABLES it_tline1 USING p_qmnum.
  PERFORM release_lock USING p_qmnum.
  PERFORM update_notif_text TABLES it_tline1
                        USING p_qmnum.
  PERFORM release_lock USING p_qmnum.
  IF eflag <> 'X'.
    PERFORM set_comp_notif USING p_qmnum.
  ELSE.
    PERFORM update_notif_status USING p_qmnum.
  ENDIF.
  p_result = rflag.
  p_message = rmess.
ENDFORM.                    " PROCESS_STCK

*&---------------------------------------------------------------------*
*&      Form  change_to_void
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE1  text
*      -->P_QMNUM  text
*----------------------------------------------------------------------*
FORM change_to_void TABLES   it_tline STRUCTURE tline
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
         v_obj TYPE thead-tdobject,
         v_matnr LIKE qmel-matnr,
         v_rkmng LIKE qmel-rkmng.

  DATA : it_tlines LIKE tline OCCURS 0 WITH HEADER LINE.

  CLEAR: rflag, eflag, rmess.

  SELECT SINGLE mawerk bzmng matnr rkmng charg
              INTO (v_mawerk, v_bzmng, v_matnr, v_rkmng, w_charg)
              FROM qmel WHERE qmnum = p_qmnum.

  CLEAR: w_xchar.
  SELECT SINGLE xchar INTO w_xchar FROM marc
    WHERE matnr = v_matnr
      AND werks = v_mawerk.

  v_id = 'LTQM'.
  v_lang = 'E'.
  v_obj  = 'QMEL'.
  v_name = p_qmnum.

  PERFORM get_text TABLES it_tlines
                          USING v_id v_lang v_obj v_name.

  READ TABLE it_tlines INDEX 2.
  v_lgort = it_tlines+18(4).
  CLEAR: it_tlines, it_tlines[].
  IF v_lgort = 'P400'.
    SELECT SINGLE vspvb INTO v_vspvb FROM marc
                                 WHERE matnr = v_matnr
                                   AND werks = v_mawerk.

    SELECT SINGLE lgtyp lgpla  INTO (v_lgtyp, v_lgpla) FROM pkhd
                               WHERE matnr = v_matnr
                                 AND werks = v_mawerk
                                 AND prvbe = v_vspvb.

    IF ( v_lgtyp IS INITIAL AND v_lgpla IS INITIAL ).
** Changed by Furong pn 07/28/10
*      MOVE 'INV_COUNT' TO V_LGPLA.
      MOVE 'HMMA ADJ' TO v_lgpla.
** end of change
      MOVE '999' TO v_lgtyp.
    ELSE.
      SELECT SINGLE lgber INTO v_lgber FROM lagp
                                       WHERE lgnum = 'P01'
                                         AND lgtyp = v_lgtyp
                                         AND lgpla = v_lgpla.

      SELECT SINGLE verme INTO v_verme FROM lqua
                            WHERE lgnum = 'P01'
                              AND matnr = v_matnr
                              AND lgtyp = v_lgtyp
                              AND lgpla = v_lgpla.
      MOVE v_verme TO v_temp.
      IF v_temp < v_rkmng.
*        MOVE 'INV_COUNT' TO V_LGPLA.
        MOVE 'HMMA ADJ' TO v_lgpla.
        MOVE '999' TO v_lgtyp.
      ENDIF.
    ENDIF.

    PERFORM creat_rev_mvt TABLES it_tline
                          USING v_mawerk v_lgort v_bzmng
                                v_matnr p_qmnum.
    IF eflag <> 'X'.
      PERFORM create_rev_to TABLES it_tline
       USING v_lgtyp v_lgpla v_lgber v_matnr.
    ENDIF.

** Changed by Furong on 10/19/09
*  ELSEIF V_LGORT = 'P500' .
  ELSE.
** End of change on 10/19/09

    PERFORM creat_rev_mvt TABLES it_tline
                          USING v_mawerk v_lgort v_bzmng
                                v_matnr  p_qmnum.

  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_qmnum
    IMPORTING
      output = p_qmnum.

  IF eflag = 'X'.

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
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      COMMIT WORK AND WAIT.
      MESSAGE i028(zs).
      WAIT UP TO 5 SECONDS.
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
                            p_bzmng
                            p_matnr
                            p_qmnum.

  DATA : it_header LIKE bapi2017_gm_head_01,
         code LIKE bapi2017_gm_code VALUE '04',
         v_uom LIKE mara-meins,
         l_fecod LIKE qmfe-fecod.

  DATA: it_item LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE,
        it_ret LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  SELECT SINGLE meins FROM mara INTO v_uom WHERE matnr = p_matnr.

  SELECT SINGLE fecod  FROM qmfe INTO l_fecod
                       WHERE qmnum = p_qmnum.

  it_header-pstng_date  = sy-datum.
  it_header-doc_date    = sy-datum.
  it_item-material      = p_matnr.
  it_item-plant         = p_mawerk.
  it_item-move_reas =  l_fecod.

** Changed on 12/13/11
  CASE p_mawerk.
    WHEN 'E001'.
      it_item-stge_loc      = 'E499'.
    WHEN 'E002'.
      it_item-stge_loc      = 'N499'.
    WHEN 'P001'.
      it_item-stge_loc      = 'P499'.
  ENDCASE.

*  IF P_MAWERK = 'E001'.
*    IT_ITEM-STGE_LOC      = 'E499'.
*  ELSE.
*    IT_ITEM-STGE_LOC      = 'P499'.
*  ENDIF.
*  IT_ITEM-STGE_LOC      = 'P499'.
** end of change

  it_item-move_type     = '311'.
  it_item-entry_qnt     = p_bzmng.
  it_item-entry_uom     = v_uom.
  it_item-move_plant    = p_mawerk.
  it_item-move_stloc    = p_lgort.
  it_item-mvt_ind       = ' '.
** Changed by Furong on 10/19/09
  IF w_xchar = 'X'.
    it_item-batch  = w_charg.
  ENDIF.
** End of change

  APPEND it_item.

  CLEAR: it_item, mdoc, myear.

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
    CONCATENATE 'ERROR: '
         it_ret-message INTO p_it_tline-tdline.
    APPEND p_it_tline.
    rflag = 'E'.
    rmess = p_it_tline.
    CLEAR p_it_tline.
    eflag = 'X'.
  ELSE.
    p_it_tline-tdformat = '*' .
    CONCATENATE '@2 Material DOC:'
                 mdoc myear INTO p_it_tline-tdline  SEPARATED BY space.
    APPEND p_it_tline.
    rflag = 'S'.
    rmess = p_it_tline.
    CLEAR p_it_tline.
  ENDIF.

ENDFORM.                    " creat_rev_mvt
*&---------------------------------------------------------------------*
*&      Form  create_rev_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE  text
*      -->P_V_LGTYP  text
*      -->P_V_LGPLA  text
*      -->P_V_LGBER  text
*----------------------------------------------------------------------*
FORM create_rev_to TABLES   p_it_tline
                   USING    p_lgtyp
                            p_lgpla
                            p_lgber
                            p_matnr.

  DATA : v_uom LIKE mara-meins.

  REFRESH: bdcdata.
  SELECT SINGLE meins FROM mara INTO v_uom WHERE matnr = p_matnr.


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
**Paul Comment : 071111
**  PERFORM BDC_TRANSACTION TABLES P_IT_TLINE
**                          USING 'LT06'.
**E<
ENDFORM.                    " create_rev_to
*&---------------------------------------------------------------------*
*&      Form  release_lock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_QMNUM  text
*----------------------------------------------------------------------*
FORM release_lock USING    p_qmnum.
  IF NOT p_qmnum IS INITIAL.
    CALL FUNCTION 'DEQUEUE_EIQMEL'
      EXPORTING
*       MODE_QMEL = 'E'
*       MANDT     = SY-MANDT
        qmnum     = p_qmnum
*       X_QMNUM   = ' '
*       _SCOPE    = '3'
*       _SYNCHRON = ' '
*       _COLLECT  = ' '
      .

  ENDIF.

ENDFORM.                    " release_lock
*&---------------------------------------------------------------------*
*&      Form  update_notif_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE1  text
*      -->P_QMNUM  text
*----------------------------------------------------------------------*
FORM update_notif_text TABLES   p_it_tline STRUCTURE tline
                       USING    p_v_qmnum.
*
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_v_qmnum
    IMPORTING
      output = p_v_qmnum.

  CALL FUNCTION 'IQS0_ADD_NOTIFICATION_LONGTEXT'
    EXPORTING
      i_qmnum       = p_v_qmnum
      i_post        = 'X'
*     I_RESET       =
    TABLES
      t_inlines     = p_it_tline
    EXCEPTIONS
      show_messages = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.                    " update_notif_text
*&---------------------------------------------------------------------*
*&      Form  set_COMP_notif
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_QMNUM  text
*----------------------------------------------------------------------*
FORM set_comp_notif USING p_qmnum.
  REFRESH bdcdata.

  PERFORM bdc_dynpro      USING 'SAPLIQS0' '0200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RIWO00-QMNUM'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RIWO00-QMNUM'
                                p_qmnum.
  PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ARCH'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'VIQMEL-QMGRP'.
  PERFORM bdc_dynpro      USING 'SAPLQM02' '0200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'VIQMEL-QMDAB'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SCHL'.
  PERFORM bdc_transaction1 USING 'QM02'.
ENDFORM.                    " set_COMP_notif
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0210   text
*      -->P_0211   text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.

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
FORM bdc_transaction TABLES  p_p_it_tline STRUCTURE tline
                     USING    tcode.

  DATA: l_subrc LIKE sy-subrc,
        msg(255),
        l_year(4).

  REFRESH: messtab, p_p_it_tline.
  CLEAR : msg.

  CALL TRANSACTION tcode USING bdcdata
                   MODE   ctumode
                   UPDATE cupdate
                   MESSAGES INTO messtab.
  l_subrc = sy-subrc.

  READ TABLE messtab WITH KEY msgtyp = 'E'.

  IF sy-subrc = 0.
*    LOOP AT MESSTAB.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH MESSTAB-MSGV1 MESSTAB-MSGV2 MESSTAB-MSGV3
*                 MESSTAB-MSGV4.

    CONCATENATE messtab-msgv1 messtab-msgv2 messtab-msgv3
                messtab-msgv4 INTO msg SEPARATED BY space.
*    ENDLOOP.
    p_p_it_tline-tdformat = '*' .
    CONCATENATE 'ERROR: ' msg INTO p_p_it_tline-tdline.
    APPEND p_p_it_tline.
    CLEAR p_p_it_tline.
    eflag = 'X'.
  ELSE.

    READ TABLE messtab WITH KEY msgtyp = 'A'.

    IF sy-subrc = 0.
*      LOOP AT MESSTAB.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH MESSTAB-MSGV1 MESSTAB-MSGV2 MESSTAB-MSGV3
*                 MESSTAB-MSGV4.

*        CONCATENATE MSG MESSTAB-MSGV1 INTO MSG.
*      ENDLOOP.
      CONCATENATE messtab-msgv1 messtab-msgv2 messtab-msgv3
                 messtab-msgv4 INTO msg SEPARATED BY space.

      p_p_it_tline-tdformat = '*' .
      CONCATENATE 'ERROR: ' msg INTO p_p_it_tline-tdline.
      APPEND p_p_it_tline.
      CLEAR p_p_it_tline.
      eflag = 'X'.
    ELSE.

      l_year = sy-datum+0(4).
      CASE tcode.
        WHEN 'MIGO'.
          READ TABLE messtab WITH KEY msgtyp = 'S'.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH MESSTAB-MSGV1 MESSTAB-MSGV2 MESSTAB-MSGV3
*                  MESSTAB-MSGV4.

          CONCATENATE msg messtab-msgv1 INTO msg.
          mdoc = messtab-msgv1.
          REFRESH bdcdata.
          p_p_it_tline-tdformat = '*' .
          CONCATENATE 'Disposition - Return to vendor: '
              msg l_year INTO p_p_it_tline-tdline SEPARATED BY space.
          APPEND p_p_it_tline.
          CLEAR p_p_it_tline.
        WHEN 'MB1A'.
          READ TABLE messtab WITH KEY msgtyp = 'S'.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH MESSTAB-MSGV1 MESSTAB-MSGV2 MESSTAB-MSGV3
*                  MESSTAB-MSGV4.

          CONCATENATE msg messtab-msgv1 INTO msg.
          mdoc = messtab-msgv1.
          REFRESH bdcdata.
          p_p_it_tline-tdformat = '*' .
          CONCATENATE 'Disposition - Return to GL Account, @2 Material DOC:'
                     msg l_year INTO p_p_it_tline-tdline  SEPARATED BY space.

*        CONCATENATE 'Disposition - Return to GL Account, Material DOC:'
*            MSG L_YEAR INTO P_P_IT_TLINE-TDLINE  SEPARATED BY SPACE.
          APPEND p_p_it_tline.
          CLEAR p_p_it_tline.

        WHEN OTHERS.
          LOOP AT messtab.
*          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH MESSTAB-MSGV1 MESSTAB-MSGV2 MESSTAB-MSGV3
*                     MESSTAB-MSGV4.

            CONCATENATE msg messtab-msgv1 INTO msg.
          ENDLOOP.
          REFRESH bdcdata.
          p_p_it_tline-tdformat = '*' .
          CONCATENATE 'TRORDER' msg l_year INTO p_p_it_tline-tdline.
          APPEND p_p_it_tline.
          CLEAR p_p_it_tline.
      ENDCASE.
    ENDIF.
  ENDIF.
ENDFORM.                    " bdc_transaction

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

  REFRESH: messtab.
  CLEAR : msg.
  CALL TRANSACTION tcode USING bdcdata
                   MODE   ctumode
                   UPDATE cupdate
                   MESSAGES INTO messtab.
  l_subrc = sy-subrc.

  READ TABLE messtab WITH KEY msgtyp = 'E'.

  IF sy-subrc = 0.
    LOOP AT messtab.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH MESSTAB-MSGV1 MESSTAB-MSGV2 MESSTAB-MSGV3
*                 MESSTAB-MSGV4.
      rflag = 'E'.
      CONCATENATE msg messtab-msgv1 INTO rmess.
    ENDLOOP.

  ELSE.
    LOOP AT messtab.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH MESSTAB-MSGV1 MESSTAB-MSGV2 MESSTAB-MSGV3
*                 MESSTAB-MSGV4.
      IF sy-msgty = 'A'.
        rflag = 'E'.
        CONCATENATE msg messtab-msgv1 INTO rmess.
        EXIT.
      ELSE.
        rflag = 'S'.
      ENDIF.
*      CONCATENATE MSG MESSTAB-MSGV1 INTO RMESS.
    ENDLOOP.

    COMMIT WORK.
*    it_qmnum-qmnum = messtab-msgv1.
*    it_qmnum-matnr = matnr.
*    APPEND it_qmnum.
*    CLEAR it_qmnum.
    REFRESH bdcdata.
*    IF tcode = 'QM01'.
*      MESSAGE i003(zs) WITH msg.
*    ENDIF.
  ENDIF.

ENDFORM.                    " bdc_transaction1

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
       EXCEPTIONS
         id                            = 1
         language                      = 2
         name                          = 3
         not_found                     = 4
         object                        = 5
         reference_check               = 6
         wrong_access_to_archive       = 7
         OTHERS                        = 8
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " get_text

*&---------------------------------------------------------------------*
*&      Form  HMMA_SCRAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE1  text
*      -->P_P_QMNUM  text
*----------------------------------------------------------------------*
FORM hmma_scrap_511 TABLES p_it_tline STRUCTURE tline
                USING  p_qmnum.

  DATA : it_header LIKE bapi2017_gm_head_01,
           code LIKE bapi2017_gm_code VALUE '03',
           v_uom LIKE mara-meins.

  DATA: it_item LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE,
        it_ret LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  DATA: v_mawerk TYPE qmel-mawerk,
          v_lgort TYPE mard-lgort,
          v_bzmng TYPE qmel-bzmng,
          v_matnr LIKE qmel-matnr,
          v_rkmng LIKE qmel-rkmng,
          v_fecod LIKE viqmfe-fecod.

  CLEAR: rflag, eflag, rmess.

  SELECT SINGLE mawerk bzmng matnr rkmng charg
              INTO (v_mawerk, v_bzmng, v_matnr, v_rkmng, w_charg)
              FROM qmel WHERE qmnum = p_qmnum.


  CLEAR: w_xchar.
  SELECT SINGLE xchar INTO w_xchar FROM marc
    WHERE matnr = v_matnr
      AND werks = v_mawerk.

  SELECT SINGLE meins FROM mara INTO v_uom WHERE matnr = v_matnr.

  SELECT SINGLE fecod INTO v_fecod
    FROM viqmfe
    WHERE qmnum = p_qmnum.

  IF v_mawerk IS INITIAL.
    SELECT SINGLE werks INTO v_mawerk
    FROM marc
    WHERE matnr = v_matnr.
  ENDIF.
  CASE v_mawerk.
    WHEN 'P001'.
      it_item-orderid = 'CP001'.
    WHEN 'E001'.
      it_item-orderid = 'CE001'.
** Changed on 12/13/11
    WHEN 'E002'.
      it_item-orderid = 'CE002'.
** end of change
    WHEN OTHERS.
      rflag = 'E'.
      rmess = 'Cannot find Plant'.
      eflag = 'X'.
      EXIT.
  ENDCASE.

  CLEAR: mdoc, myear.

  it_header-pstng_date  = sy-datum.
  it_header-doc_date     = sy-datum.
  it_item-material      = v_matnr.
  it_item-plant = v_mawerk.

** Changed on 12/13/11
  CASE v_mawerk.
    WHEN 'E001'.
      it_item-stge_loc      = 'E499'.
    WHEN 'E002'.
      it_item-stge_loc      = 'N499'.
    WHEN 'P001'.
      it_item-stge_loc      = 'P499'.
  ENDCASE.

*  IF V_MAWERK = 'E001'.
*    IT_ITEM-STGE_LOC      = 'E499'.
*  ELSE.
*    IT_ITEM-STGE_LOC      = 'P499'.
*  ENDIF.

** end on 12/13/11

*  IT_ITEM-STGE_LOC      = 'P499'.
  it_item-move_type     = '551'.
  it_item-entry_qnt     = v_rkmng.
  it_item-entry_uom     = v_uom.
  it_item-move_plant    = v_mawerk.
*  IT_ITEM-MOVE_STLOC    = P_LGORT.
  it_item-move_reas =  v_fecod.
  it_item-mvt_ind       = ' '.

** Changed by Furong on 10/19/09
  IF w_xchar = 'X'.
    it_item-batch  = w_charg.
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
    CONCATENATE 'ERROR for HMMA Scrap :'
         it_ret-message INTO p_it_tline-tdline.
    APPEND p_it_tline.
    rflag = 'E'.
    rmess = p_it_tline-tdline.
    CLEAR p_it_tline.
    eflag = 'X'.
  ELSE.
    p_it_tline-tdformat = '*' .
    CONCATENATE 'HMMA Scrap - @2 Material DOC:'
                 mdoc myear INTO p_it_tline-tdline  SEPARATED BY space.
*    CONCATENATE 'Material Doc for HMMA Scrap :'
*                 MDOC MYEAR INTO P_IT_TLINE-TDLINE  SEPARATED BY SPACE.
    APPEND p_it_tline.
    rflag = 'S'.
    rmess = p_it_tline.
    CLEAR p_it_tline.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " HMMA_SCRAP_511
*&---------------------------------------------------------------------*
*&      Form  RETURN_TO_VENDOR_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  return_to_vendor_bapi TABLES p_it_tline STRUCTURE tline
                           USING p_ebeln p_ebelp
                                 p_mawerk
*                                 P_LGORT
                                 p_bzmng
                                 p_matnr
                                 p_fecod
                                 p_belen
                                 p_buzei
                                 p_gjahr.

  DATA : it_header LIKE bapi2017_gm_head_01,
          code LIKE bapi2017_gm_code VALUE '01',
          v_uom LIKE mara-meins.

  DATA: it_item LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE,
        it_ret LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  SELECT SINGLE meins FROM mara INTO v_uom WHERE matnr = p_matnr.


*           am07m-lifnr       TO it_goodsmvt_item-vendor,

*           it_itab-lfpos     TO it_goodsmvt_item-ref_doc_it.

  CLEAR: mdoc, myear, p_it_tline[].

  it_header-pstng_date  = sy-datum.
  it_header-doc_date     = sy-datum.

  it_item-material      = p_matnr.
  it_item-plant         = p_mawerk.
  it_item-po_number   = p_ebeln.
  it_item-po_item = p_ebelp.
  it_item-move_reas = p_fecod.
  it_item-ref_doc_yr = p_gjahr.
  it_item-ref_doc = p_belen.
  it_item-ref_doc_it = p_buzei.

** Changed on 12/13/11
  CASE p_mawerk.
    WHEN 'E001'.
      it_item-stge_loc      = 'E499'.
    WHEN 'E002'.
      it_item-stge_loc      = 'N499'.
    WHEN 'P001'.
      it_item-stge_loc      = 'P499'.
  ENDCASE.


*  IF P_MAWERK = 'E001'.
*    IT_ITEM-STGE_LOC      = 'E499'.
*  ELSE.
*    IT_ITEM-STGE_LOC      = 'P499'.
*  ENDIF.

** End on 12/13/11

*  IT_ITEM-STGE_LOC      = 'P499'.
  it_item-move_type     = '122'.
  it_item-entry_qnt     = p_bzmng.
  it_item-entry_uom     = v_uom.
  it_item-move_plant    = p_mawerk.
*  IT_ITEM-MOVE_STLOC    = P_LGORT.
  it_item-mvt_ind       = 'B'.
  IF w_xchar = 'X'.
    it_item-batch = w_charg.
  ENDIF.
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

  IF mdoc IS INITIAL.
    p_it_tline-tdformat = '*' .
    CONCATENATE 'ERROR: '
         it_ret-message INTO p_it_tline-tdline.
    APPEND p_it_tline.
    rflag = 'E'.
    rmess = p_it_tline.
    CLEAR p_it_tline.
    eflag = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
             wait          = 'X'
*           IMPORTING
*             RETURN        =
            .
    p_it_tline-tdformat = '*' .
    CONCATENATE '@2 Material DOC:'
                 mdoc myear INTO p_it_tline-tdline  SEPARATED BY space.
    APPEND p_it_tline.
    rflag = 'S'.
    rmess = p_it_tline.
    CLEAR p_it_tline.
    COMMIT WORK.
  ENDIF.


ENDFORM.                    " RETURN_TO_VENDOR_BAPI
*&---------------------------------------------------------------------*
*&      Form  RETURN_TO_glaccount
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE1  text
*      -->P_P_QMNUM  text
*----------------------------------------------------------------------*
FORM return_to_glaccount TABLES   p_it_tline STRUCTURE tline
                       USING   p_qmnum p_glacc.

  DATA: v_mawerk TYPE qmel-mawerk,
          v_lgort TYPE mard-lgort,
          v_bzmng TYPE qmel-bzmng,
          v_matnr LIKE qmel-matnr,
          v_rkmng LIKE qmel-rkmng.
*          V_FECOD LIKE VIQMFE-FECOD.

  DATA: l_date(8),
        l_qty(13),
        l_budat LIKE ekbe-budat.
  DATA: l_stge_loc LIKE rm07m-lgort.
  CLEAR: rflag, eflag, rmess.
  REFRESH: bdcdata.

  SELECT SINGLE mawerk bzmng matnr rkmng
              INTO (v_mawerk, v_bzmng, v_matnr, v_rkmng)
              FROM qmel WHERE qmnum = p_qmnum.

  WRITE: sy-datum TO  l_date.
  l_qty = v_rkmng.

** Changed on 12/13/11
  CASE v_mawerk.
    WHEN 'E001'.
      l_stge_loc      = 'E499'.
    WHEN 'E002'.
      l_stge_loc      = 'N499'.
    WHEN 'P001'.
      l_stge_loc      = 'P499'.
  ENDCASE.

*  IF V_MAWERK = 'E001'.
*    L_STGE_LOC      = 'E499'.
*  ELSE.
*    L_STGE_LOC      = 'P499'.
*  ENDIF.
** End on 12/13/11

  PERFORM bdc_dynpro      USING 'SAPMM07M' '0400'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'XFULL'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'MKPF-BLDAT'
                               l_date.
  PERFORM bdc_field       USING 'MKPF-BUDAT'
                                 l_date.
  PERFORM bdc_field       USING 'RM07M-BWARTWA'
                                 '201'.
  PERFORM bdc_field       USING 'RM07M-WERKS'
                                v_mawerk.
  PERFORM bdc_field       USING 'RM07M-LGORT'
                                  l_stge_loc.               " 'P499'.
  PERFORM bdc_field       USING 'XFULL'
                               ' '.
  PERFORM bdc_field       USING 'RM07M-WVERS2'
                                'X'.

  PERFORM bdc_dynpro      USING 'SAPMM07M' '0421'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'MSEG-ERFMG(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM bdc_field       USING 'MSEG-MATNR(01)'
                                 v_matnr.
  PERFORM bdc_field       USING 'MSEG-ERFMG(01)'
                                    l_qty.

  PERFORM bdc_field       USING 'MSEGK-KONTO'
                                 p_glacc.

  PERFORM bdc_transaction TABLES p_it_tline
                          USING 'MB1A'.

  IF mdoc IS INITIAL.
    rflag = 'E'.
    READ TABLE  p_it_tline INDEX 1.
    rmess = p_it_tline-tdline.
    CLEAR p_it_tline.
    eflag = 'X'.
  ELSE.
    rflag = 'S'.
    rmess = mdoc.
    CLEAR p_it_tline.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " RETURN_TO_glaccount
*&---------------------------------------------------------------------*
*&      Form  RETURN_TO_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE1  text
*      -->P_P_QMNUM  text
*----------------------------------------------------------------------*
FORM return_to_vendor TABLES   p_it_tline STRUCTURE tline
                       USING   p_qmnum.
  DATA : it_header LIKE bapi2017_gm_head_01,
          code LIKE bapi2017_gm_code VALUE '06',
          v_uom LIKE mara-meins.

  DATA: it_item LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE,
        it_ret LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  DATA: v_mawerk TYPE qmel-mawerk,
          v_lgort TYPE mard-lgort,
          v_bzmng TYPE qmel-bzmng,
          v_lifnum LIKE qmel-lifnum,
          v_matnr LIKE qmel-matnr,
          v_rkmng LIKE qmel-rkmng,
          v_fecod LIKE viqmfe-fecod.


  DATA: l_date(8),
        l_qty(13),
        l_belnr LIKE ekbe-belnr,
        l_menge  LIKE ekbe-menge,
        l_ebeln LIKE ekbe-ebeln,
        l_gjahr LIKE ekbe-gjahr,
        l_ebelp LIKE ekbe-ebelp,
        l_buzei LIKE ekbe-buzei,
        l_budat LIKE ekbe-budat.

  DATA: lt_return LIKE TABLE OF ekbe WITH HEADER LINE,
        l_rtn_qty LIKE qmel-rkmng.

  CLEAR: rflag, eflag, rmess.

  SELECT SINGLE mawerk bzmng matnr rkmng lifnum charg
              INTO (v_mawerk, v_bzmng, v_matnr,
                    v_rkmng, v_lifnum, w_charg)
              FROM qmel WHERE qmnum = p_qmnum.

  SELECT SINGLE meins FROM mara INTO v_uom WHERE matnr = v_matnr.

  SELECT SINGLE fecod INTO v_fecod
    FROM viqmfe
    WHERE qmnum = p_qmnum.

  CLEAR: w_xchar.
  SELECT SINGLE xchar INTO w_xchar FROM marc
    WHERE matnr = v_matnr
      AND werks = v_mawerk.

  IF w_xchar IS INITIAL.

    IF v_lifnum IS INITIAL.
      SELECT SINGLE ebeln ebelp INTO (l_ebeln, l_ebelp)
      FROM eord
      WHERE matnr =  v_matnr
         AND werks = v_mawerk
         AND vrtyp = 'L'
         AND bdatu >= sy-datum.
    ELSE.
      SELECT SINGLE ebeln ebelp INTO (l_ebeln, l_ebelp)
       FROM eord
       WHERE matnr =  v_matnr
          AND werks = v_mawerk
          AND lifnr = v_lifnum
          AND vrtyp = 'L'
          AND bdatu >= sy-datum.
    ENDIF.

    SELECT belnr buzei gjahr  budat menge
             INTO (l_belnr, l_buzei, l_gjahr, l_budat, l_menge)
**                                "GJAHR BELNR BWART menge
      FROM ekbe
      WHERE ebeln = l_ebeln
        AND bwart = '101'
        AND menge >= v_rkmng
        ORDER BY gjahr DESCENDING budat DESCENDING.
      IF sy-subrc = 0.
** Changed by Furong on 04/15/09
*      exit.
        SELECT * INTO TABLE lt_return
          FROM ekbe
          WHERE lfgja >= l_gjahr
            AND lfbnr = l_belnr
            AND lfpos = l_buzei
            AND ( bwart = '102' OR bwart = '122' ).
        IF sy-subrc = 0.
          CLEAR: l_rtn_qty.
          LOOP AT lt_return.
            l_rtn_qty = l_rtn_qty + lt_return-menge.
          ENDLOOP.
          l_rtn_qty = l_rtn_qty + v_rkmng.
          IF l_rtn_qty < l_menge.
            EXIT.
          ENDIF.
        ELSE.
          EXIT.
        ENDIF.
** End of change.
      ENDIF.
    ENDSELECT.

  ELSE.

    w_xchar = 'X'.
    SELECT SINGLE a~mblnr zeile gjahr budat menge ebeln ebelp
     INTO (l_belnr, l_buzei, l_gjahr, l_budat, l_menge, l_ebeln,
l_ebelp)
     FROM mseg AS a
     INNER JOIN mkpf AS b
     ON a~mblnr = b~mblnr
     WHERE matnr = v_matnr
       AND werks = v_mawerk
       AND bwart = '101'
       AND lifnr = v_lifnum
       AND charg = w_charg.

  ENDIF.

  IF l_belnr IS INITIAL.
    rflag = 'E'.
    rmess = 'No Material document found'.
    p_it_tline-tdformat = '*' .
    CONCATENATE 'ERROR: '
         rmess INTO p_it_tline-tdline.
    APPEND p_it_tline.
    CLEAR p_it_tline.
    eflag = 'X'.
    EXIT.
  ENDIF.

  WRITE: sy-datum TO  l_date.
  l_qty = v_rkmng.

  PERFORM return_to_vendor_bapi TABLES p_it_tline
                          USING l_ebeln l_ebelp
                          v_mawerk v_rkmng v_matnr v_fecod
                          l_belnr l_buzei l_gjahr.

  IF mdoc IS INITIAL.
    rflag = 'E'.
    READ TABLE  p_it_tline INDEX 1.
    rmess = p_it_tline-tdline.
    CLEAR p_it_tline.
    eflag = 'X'.
  ELSE.
    rflag = 'S'.
    rmess = mdoc.
*    READ TABLE  P_IT_TLINE INDEX 1.
*    RMESS = P_IT_TLINE.
    CLEAR p_it_tline.
    COMMIT WORK.
  ENDIF.
ENDFORM.                    " RETURN_TO_VENDOR
*&---------------------------------------------------------------------*
*&      Form  modify_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen_all.
  LOOP AT SCREEN .
    IF p_all = 'X' AND screen-group1 EQ 'ABC'.
      p_hmma = ' '.
      p_vend = ' '.
      p_stck = ' '.
*      SCREEN-INVISIBLE = 1.
      screen-active    = 0.
      screen-input     = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " modify_screen
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN_OTHER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen_other.
  LOOP AT SCREEN .
    IF p_hmma = 'X' OR p_vend = 'X' OR p_stck = 'X'.
      IF screen-name = 'P_ALL'.
        p_all = ' '.
*        SCREEN-INVISIBLE = 1.
        screen-active    = 0.
        screen-input     = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN_OTHER
*&---------------------------------------------------------------------*
*&      Form  UPDATE_NOTIF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_QMNUM  text
*----------------------------------------------------------------------*
FORM update_notif_status USING p_qmnum.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_qmnum
    IMPORTING
      output = p_qmnum.

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
*    MESSAGE S000 WITH P_QMNUM.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.                    " UPDATE_NOTIF_STATUS
*&---------------------------------------------------------------------*
*&      Form  HMMA_SCRAP_311
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE1  text
*      -->P_P_QMNUM  text
*----------------------------------------------------------------------*
FORM hmma_scrap_311 TABLES p_it_tline STRUCTURE tline
                USING  p_qmnum.

  DATA : it_header LIKE bapi2017_gm_head_01,
         code LIKE bapi2017_gm_code VALUE '04',
           v_uom LIKE mara-meins.

  DATA: it_item LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE,
        it_ret LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  DATA: v_mawerk TYPE qmel-mawerk,
          v_lgort TYPE mard-lgort,
          v_bzmng TYPE qmel-bzmng,
          v_matnr LIKE qmel-matnr,
          v_rkmng LIKE qmel-rkmng,
          v_fecod LIKE viqmfe-fecod.

  CLEAR: rflag, eflag, rmess.

  SELECT SINGLE mawerk bzmng matnr rkmng charg
              INTO (v_mawerk, v_bzmng, v_matnr, v_rkmng, w_charg)
              FROM qmel WHERE qmnum = p_qmnum.


  CLEAR: w_xchar.
  SELECT SINGLE xchar INTO w_xchar FROM marc
    WHERE matnr = v_matnr
      AND werks = v_mawerk.

  SELECT SINGLE meins FROM mara INTO v_uom WHERE matnr = v_matnr.

  SELECT SINGLE fecod INTO v_fecod
    FROM viqmfe
    WHERE qmnum = p_qmnum.

  IF v_mawerk IS INITIAL.
    SELECT SINGLE werks INTO v_mawerk
    FROM marc
    WHERE matnr = v_matnr.
  ENDIF.
*  CASE V_MAWERK.
*    WHEN 'P001'.
*      IT_ITEM-ORDERID = 'CP001'.
*    WHEN 'E001'.
*      IT_ITEM-ORDERID = 'CE001'.
*** Changed on 12/13/11
*    WHEN 'E002'.
*      IT_ITEM-ORDERID = 'CE002'.
*** end of change
*    WHEN OTHERS.
*      RFLAG = 'E'.
*      RMESS = 'Cannot find Plant'.
*      EFLAG = 'X'.
*      EXIT.
*  ENDCASE.

  CLEAR: mdoc, myear.

  it_header-pstng_date  = sy-datum.
  it_header-doc_date     = sy-datum.
  it_item-material      = v_matnr.
  it_item-plant = v_mawerk.

** Changed on 12/13/11
  CASE v_mawerk.
    WHEN 'E001'.
      it_item-stge_loc      = 'E499'.
    WHEN 'E002'.
      it_item-stge_loc      = 'N499'.
    WHEN 'P001'.
      it_item-stge_loc      = 'P499'.
  ENDCASE.

*  IF V_MAWERK = 'E001'.
*    IT_ITEM-STGE_LOC      = 'E499'.
*  ELSE.
*    IT_ITEM-STGE_LOC      = 'P499'.
*  ENDIF.

** end on 12/13/11

*  IT_ITEM-STGE_LOC      = 'P499'.
  it_item-move_type     = '311'.
  it_item-entry_qnt     = v_rkmng.
  it_item-entry_uom     = v_uom.
  it_item-move_plant    = v_mawerk.
  it_item-move_stloc    = 'X551'.
  it_item-move_reas     =  v_fecod. "Uncomment 03.28.2014 Victor
  it_item-mvt_ind       = ' '.

** Changed by Furong on 10/19/09
  IF w_xchar = 'X'.
    it_item-batch  = w_charg.
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
    CONCATENATE 'ERROR for HMMA Scrap :'
         it_ret-message INTO p_it_tline-tdline.
    APPEND p_it_tline.
    rflag = 'E'.
    rmess = p_it_tline-tdline.
    CLEAR p_it_tline.
    eflag = 'X'.
  ELSE.
    p_it_tline-tdformat = '*' .
    CONCATENATE 'Transfer Document - @2 Material DOC:'
                 mdoc myear INTO p_it_tline-tdline  SEPARATED BY space.
*    CONCATENATE 'Material Doc for HMMA Scrap :'
*                 MDOC MYEAR INTO P_IT_TLINE-TDLINE  SEPARATED BY SPACE.
    APPEND p_it_tline.
    rflag = 'S'.
    rmess = p_it_tline.
    CLEAR p_it_tline.
    COMMIT WORK.
  ENDIF.
ENDFORM.                    " HMMA_SCRAP_311
