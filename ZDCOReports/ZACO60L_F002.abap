*----------------------------------------------------------------------*
***INCLUDE ZACO05L_F002 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  POSTING_USING_PP_PGM
*&---------------------------------------------------------------------*
*       POSTING
*  Production order - Time Ticket         'CO11N'
*  REM backflush    - Activity  Backflush 'MFBF'
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_using_pp_pgm.

* Selecting
  PERFORM read_data_fr_ztco_nmhpcpost.

*// Mod. By Hyung Jin Youn 2004.02.14
* DI B/F nethod - "VEHI" Material - FSC
  PERFORM post_di_b_f.
*// End of Mod.

* Production Order method - Mostly "Press, Engine" Material
* MTO production method
  PERFORM post_mto_us_timeticket.

* REM B/F Method - "EGINE" Material
* MTS production method
  PERFORM post_mts_us_rem_act_bf.


ENDFORM.                    " POSTING_USING_PP_PGM

*&---------------------------------------------------------------------*
*&      Form  read_data_fr_ZTCO_nMHPCPOST
*&---------------------------------------------------------------------*
*       Clear data Container and refill data from Table
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data_fr_ztco_nmhpcpost.
* Renewal
  CLEAR : it_ztco_nmhpcpost, it_ztco_nmhpcpost[].
* Only for the records which were not posted before
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztco_nmhpcpost
           FROM ztco_nmhpcpost
          WHERE gjahr = p_gjahr
            AND perid BETWEEN p_frper AND p_toper
            AND rueck EQ space
            AND rmzhl EQ space
            AND reversed EQ space.

  IF it_ztco_nmhpcpost[] IS INITIAL.
    MESSAGE e046 WITH 'ZTCO_nMHPCPOST' p_gjahr p_frper p_toper.
  ENDIF.

*// Mod. By Hyung Jin Youn
* Check REM Profile
** REM Profile
*DATA : GV_REMPF_FSC      LIKE MARC-SFEPR VALUE 'VEHI'.
*DATA : GV_REMPF_ENG      LIKE MARC-SFEPR VALUE 'ENGI'.
*DATA : GV_REMPF_BLANK    LIKE MARC-SFEPR VALUE SPACE.

  CLEAR it_ztco_nmhpcpost.
  LOOP AT it_ztco_nmhpcpost WHERE sfepr NE gv_rempf_fsc
                             AND sfepr NE gv_rempf_eng
                             AND sfepr NE gv_rempf_blank.
  ENDLOOP.

  IF sy-subrc = 0.
    MESSAGE s000 WITH text-101.
    STOP.
  ENDIF.

*// End of Mod.

ENDFORM.                    " read_data_fr_ZTCO_nMHPCPOST

*&---------------------------------------------------------------------*
*&      Form  POST_MTO_US_TIMETICKET
*&---------------------------------------------------------------------*
*       POST - MTO / Production Order method
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_mto_us_timeticket.

  SORT it_ztco_nmhpcpost BY gjahr perid.

* Only RemFlg = ' ' <- NOT RemBF
  CLEAR : it_po_post, it_po_post[].

  LOOP AT it_ztco_nmhpcpost WHERE sauft EQ space.
* Transferring data
    MOVE-CORRESPONDING it_ztco_nmhpcpost TO it_po_post.

* Read production order / operation
    PERFORM read_po_and_opid.
* append
    APPEND it_po_post.
    CLEAR  it_po_post.
  ENDLOOP.

  CLEAR  it_po_post.

* POST using BAPI
  PERFORM post_mto_with_bapi_tt.

ENDFORM.                    " POST_MTO_US_TIMETICKET

*&---------------------------------------------------------------------*
*&      Form  SEL_DATE_RANGE
*&---------------------------------------------------------------------*
*       Convert Period to Date
*----------------------------------------------------------------------*
*      -->P_Perid   Period
*----------------------------------------------------------------------*
FORM sel_date_range  USING p_perid.
** Convert Periods to date Range
*  CLEAR : R_WORKDATE, R_WORKDATE[].
*  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
*       EXPORTING
*            I_GJAHR = P_GJAHR
*            I_PERIV = TKA01-LMONA
*            I_POPER = P_PERID
*       IMPORTING
*            E_DATE  = R_WORKDATE-LOW.
*
*  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
*       EXPORTING
*            I_GJAHR = P_GJAHR
*            I_PERIV = TKA01-LMONA
*            I_POPER = P_PERID
*       IMPORTING
*            E_DATE  = R_WORKDATE-HIGH.
*  R_WORKDATE-SIGN   = 'I'.
*  R_WORKDATE-OPTION = 'BT'.
*  APPEND R_WORKDATE.
ENDFORM.                    " SEL_DATE_RANGE

*&---------------------------------------------------------------------*
*&      Form  READ_PO_AND_OPID
*&---------------------------------------------------------------------*
*       Read production order / operation
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_po_and_opid.
* Read Production Order (The Latest date - Creation date )
  CLEAR aufk.
  SELECT SINGLE MAX( erdat ) aufnr
                   INTO (aufk-erdat, aufk-aufnr)
                   FROM aufk
                  WHERE pkosa = it_po_post-aufnr
                    AND autyp = '10' "<- Production order
                  GROUP BY aufnr.
*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_aufk CHANGING  aufk-erdat  aufk-aufnr.
  ENDIF.
*- U1 End

* Set Production order
  it_po_post-po_aufnr = aufk-aufnr.

* Read Operation with order
  DATA : it_l_vorg_tab  LIKE  STANDARD TABLE OF kpzp1
                          WITH HEADER LINE .

  CALL FUNCTION 'RM_OPERATION_READ_MULTI'
    EXPORTING
      aufnr_imp          = it_po_post-po_aufnr
      period_imp         = it_po_post-perid
      gjahr_imp          = it_po_post-gjahr
      matnr_imp          = it_po_post-matnr
*     APROZ_IMP          =
    IMPORTING
      plnty_exp          = it_po_post-plnty_exp
      plnnr_exp          = it_po_post-plnnr_exp
      plnal_exp          = it_po_post-plnal_exp
      plnme_exp          = it_po_post-plnme_exp
    TABLES
      vorg_tab           = it_l_vorg_tab
    EXCEPTIONS
      parallel_seq       = 1
      missing_parameters = 2
      no_sequence        = 3
      order_not_found    = 4
      false_fyear        = 5
      no_routing         = 6
      prog_err           = 7
      no_conversion      = 8
      OTHERS             = 9.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Operation ID
  SELECT SINGLE
                  plpo~vornr         plpo~arbid
                  crhd~arbpl

           INTO  (it_po_post-vornr, it_po_post-arbid,
                  it_po_post-arbpl)

           FROM  ( plko INNER JOIN plpo
             ON  plko~plnty = plpo~plnty
            AND  plko~plnnr = plpo~plnnr )
*            AND  PLKO~ZAEHL = PLPO~ZAEHL )
                                           INNER JOIN crhd
            ON
*            CRHD~OBJTY = PLPO~OBJTY  : OBJECT type = 'A' / Work Center
                 crhd~objid = plpo~arbid
           WHERE
                 plko~plnty = it_po_post-plnty_exp
             AND plko~plnnr = it_po_post-plnnr_exp
             AND plko~plnal = it_po_post-plnal_exp
             AND plko~verwe = '1'  "Usage 1 Production
             AND plpo~werks = it_po_post-werks
             AND plpo~loekz = space
             AND crhd~objty = 'A'
             AND crhd~arbpl = it_po_post-kostl.
*      Index : Table key (all)
  IF sy-subrc <> 0.
* SKIP Error : Let SAP program generate system errors during posting.
*    MESSAGE E049 WITH  IT_PO_POST-PLNTY_EXP
*                       IT_PO_POST-PLNNR_EXP
*                       IT_PO_POST-PLNAL_EXP
*                       IT_PO_POST-KOSTL.
  ENDIF.

ENDFORM.                    " READ_PO_AND_OPID

*&---------------------------------------------------------------------*
*&      Form  CHECK_NOT_POST_RC
*&---------------------------------------------------------------------*
*       Check Not-posted records
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_not_post_rc.

  CLEAR gv_new.
  CLEAR ztco_nmhpcpost.
* Only for the records which were not posted before
  SELECT SINGLE *
           FROM ztco_nmhpcpost
          WHERE gjahr = p_gjahr
            AND perid BETWEEN p_frper AND p_toper
            AND rueck EQ space
            AND rmzhl EQ space
            AND reversed = space.
  IF sy-subrc = 0.
    gv_new = space.
  ELSE.
    gv_new = 'X'. "<- No record found, New records should be created
  ENDIF.

  CHECK gv_new = 'X'.
  CLEAR ztco_nmhpcpost.
  SELECT SINGLE *
           FROM ztco_nmhpcpost
          WHERE gjahr = p_gjahr
            AND perid BETWEEN p_frper AND p_toper
            AND reversed = space.
  IF sy-subrc = 0.
    MESSAGE e053 WITH p_gjahr p_frper p_toper.
*   MESSAGE E052.
  ENDIF .

ENDFORM.                    " CHECK_NOT_POST_RC

*&---------------------------------------------------------------------*
*&      Form  POST_MTO_WITH_BAPI_TT
*&---------------------------------------------------------------------*
*       POST - MTO
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_mto_with_bapi_tt.

  DATA : it_l_timetickets	LIKE STANDARD TABLE OF bapi_pp_timeticket
                              WITH HEADER LINE .
  DATA : wa_l_return        LIKE bapiret1.
  DATA : it_l_detail_return
                              LIKE STANDARD TABLE OF bapi_coru_return
                              WITH HEADER LINE .
  DATA : lv_conf_text         LIKE it_l_timetickets-conf_text .
  DATA : lv_postg_date        LIKE it_l_timetickets-postg_date.

* Making Time Ticket Data
  SORT it_po_post BY gjahr perid.

  LOOP AT it_po_post.
* Clear
    CLEAR :  it_l_timetickets, it_l_timetickets[].
* Period
    it_l_timetickets-orderid   = it_po_post-po_aufnr.
    it_l_timetickets-operation = it_po_post-vornr.
* Final Confirmation
    it_l_timetickets-fin_conf  = 'X'.
* Plant/WC(CCtr)
    it_l_timetickets-plant     = it_po_post-werks.
    it_l_timetickets-work_cntr = it_po_post-kostl.
* Quantity / Unit
    it_l_timetickets-conf_acti_unit3 =  it_po_post-meinh.
    it_l_timetickets-conf_activity3  =  it_po_post-varquan.
* TEXT
    CLEAR lv_conf_text.
    CONCATENATE it_po_post-gjahr it_po_post-perid it_po_post-matnr
                sy-uname         sy-repid
           INTO lv_conf_text
           SEPARATED BY '/'.
    it_l_timetickets-conf_text = lv_conf_text.
* Posting Date : The Last day of period
    ON CHANGE OF it_po_post-gjahr
              OR it_po_post-perid.
      CLEAR : lv_postg_date.
      CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
        EXPORTING
          i_gjahr = it_po_post-gjahr
          i_periv = tka01-lmona
          i_poper = it_po_post-perid
        IMPORTING
          e_date  = lv_postg_date.
    ENDON.
    it_l_timetickets-postg_date  = lv_postg_date.
* IT_L_TIMETICKETS
    APPEND it_l_timetickets.
    CLEAR  it_l_timetickets.

* Call Posting FM without Commit Work
* Sinlge Line Posting <- To check Confirnation No
    CLEAR : it_l_detail_return, it_l_detail_return[].
    CALL FUNCTION 'BAPI_PRODORDCONF_CREATE_TT'
      EXPORTING
        post_wrong_entries = '0'
        testrun            = space
      IMPORTING
        return             = wa_l_return
      TABLES
        timetickets        = it_l_timetickets
*       GOODSMOVEMENTS     =
*       LINK_CONF_GOODSMOV =
        detail_return      = it_l_detail_return.

* Skip 'E' 'A' Error
* But Store Error Message .
    LOOP AT it_l_detail_return WHERE type CA 'EA'.
      it_po_post-message = it_l_detail_return-message.
    ENDLOOP.
    LOOP AT it_l_detail_return WHERE type CA 'SIW'.
      it_po_post-message = it_l_detail_return-message.
      it_po_post-rueck   = it_l_detail_return-conf_no.
      it_po_post-rmzhl   = it_l_detail_return-conf_cnt.
    ENDLOOP.
* Storing Message and Conf. No / Item
    MODIFY it_po_post.
* Result
    CLEAR ztco_nmhpcpost.
    MOVE-CORRESPONDING it_po_post TO ztco_nmhpcpost.

* Update   ZTCO_nMHPCPOST - Common Part
    PERFORM update_ztco_nmhpcpost_w_log.

    CLEAR  it_po_post.
  ENDLOOP.

  CLEAR  it_po_post.

ENDFORM.                    " POST_MTO_WITH_BAPI_TT

*&---------------------------------------------------------------------*
*&      Form  POST_MTS_US_REM_ACT_BF
*&---------------------------------------------------------------------*
*       REM Activity Backflush
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_mts_us_rem_act_bf.

  SORT it_ztco_nmhpcpost BY gjahr perid matnr werks aufnr.

* Using BDC T-CODE 'MFBF'.
* Only RemFlg = 'X' <- RemBF
  CLEAR : it_rem_post, it_rem_post[].

*// Mod. By Hyung Jin Youn 2004.02.14
  LOOP AT it_ztco_nmhpcpost WHERE sauft NE space         " 'X"
                             AND sfepr EQ gv_rempf_eng. " Engine
*// End. of Mod.

* Transferring data
    MOVE-CORRESPONDING it_ztco_nmhpcpost TO it_rem_post.
* Read Production Version / Planning Plant
    ON CHANGE
              OF it_ztco_nmhpcpost-gjahr
              OR it_ztco_nmhpcpost-perid
              OR it_ztco_nmhpcpost-matnr
              OR it_ztco_nmhpcpost-werks
              OR it_ztco_nmhpcpost-aufnr.

      PERFORM read_pv_pplant.
    ENDON.
* Additional DATA
    it_rem_post-verid     =   wa_rem_post-verid.
    it_rem_post-pwerk     =   wa_rem_post-pwerk.
    it_rem_post-plnty_exp =   wa_rem_post-plnty_exp .
    it_rem_post-plnnr_exp =   wa_rem_post-plnnr_exp .
    it_rem_post-plnal_exp =   wa_rem_post-plnal_exp .
* read operation no
    PERFORM read_opr_no .
* Appending
    APPEND it_rem_post.
    CLEAR  it_rem_post.
  ENDLOOP.

  CLEAR  it_rem_post.

* POST using MFBF
  PERFORM post_mts_with_mfbf.

ENDFORM.                    " POST_MTS_US_REM_ACT_BF

*&---------------------------------------------------------------------*
*&      Form  READ_PV_PPLANT
*&---------------------------------------------------------------------*
*       Read Production Version / Planning Plant
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_pv_pplant.

  CLEAR wa_rem_post.

**// Mod. By hyung Jin youn 2004.02.17
* About Production version
  wa_rem_post-werks = it_ztco_nmhpcpost-werks.
  wa_rem_post-matnr = it_ztco_nmhpcpost-matnr.
  wa_rem_post-verid = it_ztco_nmhpcpost-verid.
* Production Version and Planning Plant
  CLEAR blpk.

  SELECT SINGLE pwerk
         INTO wa_rem_post-pwerk
         FROM blpk
        WHERE werks = wa_rem_post-werks
          AND matnr = wa_rem_post-matnr
          AND verid = wa_rem_post-verid
          AND reptp = '01'   "<- REM B/F
        GROUP BY pwerk.
  IF sy-subrc <> 0.
    MESSAGE e073 WITH wa_rem_post-werks
                      wa_rem_post-matnr.
  ENDIF.
**// End of Mod.

* For Operation Nos.
  CLEAR mkal.

  SELECT SINGLE *  FROM mkal
                  WHERE matnr = wa_rem_post-matnr
                    AND werks = wa_rem_post-werks
                    AND verid = wa_rem_post-verid.
  IF sy-subrc <> 0.
    MESSAGE e051 WITH wa_rem_post-werks
                      wa_rem_post-matnr
                      wa_rem_post-verid.
  ENDIF.
*
  wa_rem_post-plnty_exp = mkal-pltyg .
  wa_rem_post-plnnr_exp = mkal-plnng .
  wa_rem_post-plnal_exp = mkal-alnag .

ENDFORM.                    " READ_PV_PPLANT

*&---------------------------------------------------------------------*
*&      Form  READ_OPR_NO
*&---------------------------------------------------------------------*
*       Read Operation Numbers
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_opr_no.
* Operation ID
  SELECT SINGLE
                  plpo~vornr         plpo~arbid
                  crhd~arbpl

           INTO  (it_rem_post-vornr, it_rem_post-arbid,
                  it_rem_post-arbpl)

           FROM  ( plko INNER JOIN plpo
             ON  plko~plnty = plpo~plnty
            AND  plko~plnnr = plpo~plnnr )
*            AND  PLKO~ZAEHL = PLPO~ZAEHL )
                                           INNER JOIN crhd
            ON
*            CRHD~OBJTY = PLPO~OBJTY  : OBJECT type = 'A' / Work Center
                 crhd~objid = plpo~arbid
           WHERE
                 plko~plnty = it_rem_post-plnty_exp
             AND plko~plnnr = it_rem_post-plnnr_exp
             AND plko~plnal = it_rem_post-plnal_exp
             AND plko~verwe = '1'  "Usage 1 Production
             AND plpo~werks = it_rem_post-werks
             AND plpo~loekz = space
             AND crhd~objty = 'A'
             AND crhd~arbpl = it_rem_post-kostl.
*      Index : Table key (all)
  IF sy-subrc <> 0.
* SKIP Error : Let SAP program generate system errors during posting.
*    MESSAGE E049 WITH  IT_REM_POST-PLNTY_EXP
*                       IT_REM_POST-PLNNR_EXP
*                       IT_REM_POST-PLNAL_EXP
*                       IT_REM_POST-KOSTL.
  ENDIF.

ENDFORM.                    " READ_OPR_NO

*&---------------------------------------------------------------------*
*&      Form  POST_MTS_WITH_MFBF
*&---------------------------------------------------------------------*
*       Using "MFBF' - REM BACKFLUSH
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_mts_with_mfbf.
*       BAPI for Confirmation Document dose not support Activity
*       BackFlush function
*       There is another method for activity backflush - Add-on Program
*       Please, Refer to Technical Spec. for detail


* MFBF dose not give the Confirmation Document NO
* So to catch the final document, it is neccessary to run BDC
* by each record.
  SORT it_rem_post BY gjahr perid matnr werks aufnr.

  LOOP AT it_rem_post.
* Posting Period
    ON CHANGE OF it_rem_post-gjahr
              OR it_rem_post-perid.
      PERFORM read_last_day_of_per.
    ENDON.
* Building BDC Data / CALL TR.
    PERFORM build_bdc_data.
    CLEAR it_rem_post.
  ENDLOOP.

ENDFORM.                    " POST_MTS_WITH_MFBF

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR it_bdcdata.
  it_bdcdata-program  = program.
  it_bdcdata-dynpro   = dynpro.
  it_bdcdata-dynbegin = 'X'.
  APPEND it_bdcdata.
ENDFORM.                    "bdc_dynpro

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR it_bdcdata.
  it_bdcdata-fnam = fnam.
  it_bdcdata-fval = fval.
  APPEND it_bdcdata.
ENDFORM.                    "bdc_field

*&---------------------------------------------------------------------*
*&      Form  BUILD_BDC_DATA
*&---------------------------------------------------------------------*
*       Building BDC Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_bdc_data.
* Clear BDC Container
  CLEAR : it_bdcdata, it_bdcdata[].

* 18 CHAR variable (for Qunatity Field in BDC )
  DATA : lv_18char(18).



**** Header DATA - Information " REM B/F
  PERFORM bdc_dynpro      USING 'SAPLBARM' '0800'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=RBTYP'.
* Acivity Backflush
  PERFORM bdc_field       USING 'RM61B-RB_LEIST'
                                'X'.

  PERFORM bdc_dynpro      USING 'SAPLBARM' '0800'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=RBREF'.
* Acivity Backflush
  PERFORM bdc_field       USING 'RM61B-RB_LEIST'
                                'X'.
* Posting Date / Document Date
  PERFORM bdc_field       USING 'RM61B-BUDAT'
                                 gv_postdate_bdc .
* Matnr
  PERFORM bdc_field       USING 'RM61B-MATNR'
                                it_rem_post-matnr.
* Plant
  PERFORM bdc_field       USING 'RM61B-WERKS'
                                it_rem_post-werks.
* Production Version
  PERFORM bdc_field       USING 'RM61B-VERID'
                                it_rem_post-verid.
* Planning Plant
  PERFORM bdc_field       USING 'RM61B-PLWERK'
                                it_rem_post-pwerk.
* No Planned activities from routing
  PERFORM bdc_field       USING 'RM61B-ROUT_OFF'
                                'X'.

**** Header DATA " REM B/F
  PERFORM bdc_dynpro      USING 'SAPLBARM' '0800'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ISTDA'.
* Acivity Backflush
  PERFORM bdc_field       USING 'RM61B-RB_LEIST'
                                'X'.
* Posting Date / Document Date
  PERFORM bdc_field       USING 'RM61B-BUDAT'
                                 gv_postdate_bdc .
* Matnr
  PERFORM bdc_field       USING 'RM61B-MATNR'
                                it_rem_post-matnr.
* Plant
  PERFORM bdc_field       USING 'RM61B-WERKS'
                                it_rem_post-werks.
* Production Version
  PERFORM bdc_field       USING 'RM61B-VERID'
                                it_rem_post-verid.
* Planning Plant
  PERFORM bdc_field       USING 'RM61B-PLWERK'
                                it_rem_post-pwerk.
* No Planned activities from routing
  PERFORM bdc_field       USING 'RM61B-ROUT_OFF'
                                'X'.

**** OPR View
  PERFORM bdc_dynpro      USING 'SAPLRMAA' '0320'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SELE'.
  PERFORM bdc_dynpro      USING 'SAPLRMAA' '0320'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=GOON'.

**** POST
  PERFORM bdc_dynpro      USING 'SAPLRMAA' '0300'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=POST'.
* IF only One OPR is assigned to routing,
* The OPR No. '10' is the first number.
* In 'MFBF' BDC, if only one operation number is assigned,
* system blocks BDC input in fields OPR no.
  IF it_rem_post-vornr <> '0010'.
    PERFORM bdc_field       USING 'RM61J-VORNR'
                                  it_rem_post-vornr.
  ENDIF.
* value
  CLEAR lv_18char.
  WRITE it_rem_post-varquan TO lv_18char.
  PERFORM bdc_field       USING 'RM61J-ISM03'
                                lv_18char.
  PERFORM bdc_field       USING 'RM61J-ILE03'
                                'HR'.
  PERFORM bdc_field       USING 'RM61J-LAR03'
                                it_rem_post-lstar.

**** Call transaction
* Call Transaction
  CLEAR   it_messtab.
  REFRESH it_messtab.
*  CALL TRANSACTION 'MFBF'
*                   USING  it_bdcdata
*                   MODE   p_mode
*                   UPDATE 'S'
*                   MESSAGES INTO it_messtab.

**** Check message
  CLEAR : it_return , it_return[].
  CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
    TABLES
      imt_bdcmsgcoll = it_messtab
      ext_return     = it_return.
* the type of all messages in MFBF is either 'S' or 'I'.
* Check Error with Message Numbers
  CLEAR : it_return.
  LOOP AT it_return WHERE  ( id = 'RU' AND number = '100' )
                       OR  ( id = 'RM' AND number = '186' ).
  ENDLOOP.
* Success
  IF sy-subrc = 0.
    CLEAR : it_return.
    READ TABLE  it_return WITH KEY id = 'RU' number = '100' .
    it_rem_post-message = it_return-message.
*   Searching Confirmation Document No.
*   MFBF tra. dose not send the generated Confirmation Document No
*   in message. so find it in table of AFKO. The table has the last
*   Conf. doc. no. and the counter no.
    CLEAR afko.
    SELECT SINGLE rueck rmzhl
      INTO (it_rem_post-rueck, it_rem_post-rmzhl)
      FROM afko
     WHERE aufnr = it_rem_post-aufnr.  "<- PCC order

*- U1 Start
    IF p_arch EQ 'X' AND sy-subrc <> 0.
      PERFORM archive_read_afko  CHANGING it_rem_post-rueck  it_rem_post-rmzhl.
    ENDIF.
*- U1 End

* Failure
  ELSE.
* Capturing the last Message
    CLEAR it_return.
    DESCRIBE TABLE it_return LINES sy-tfill.
    READ TABLE it_return INDEX sy-tfill.
    it_rem_post-message = it_return-message.
  ENDIF.

* Storing Message and Conf. No / Item
  MODIFY it_rem_post.
* Result
  CLEAR ztco_nmhpcpost.
  MOVE-CORRESPONDING it_rem_post TO ztco_nmhpcpost.

* Update   ZTCO_nMHPCPOST - Common Part
  PERFORM update_ztco_nmhpcpost_w_log.

ENDFORM.                    " BUILD_BDC_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_LAST_DAY_OF_PER
*&---------------------------------------------------------------------*
*       Get Last day of Period
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_last_day_of_per.

  CLEAR gv_postdate_bdc  .

  DATA : lv_date LIKE sy-datum.
  CLEAR : lv_date.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr = p_gjahr
      i_periv = tka01-lmona
      i_poper = it_rem_post-perid
    IMPORTING
      e_date  = lv_date.

* DATE CONVERSION
  DATA : lv_con_date(10).
  CLEAR lv_con_date.
  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      date_internal            = lv_date
    IMPORTING
      date_external            = gv_postdate_bdc
    EXCEPTIONS
      date_internal_is_invalid = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " READ_LAST_DAY_OF_PER

*&---------------------------------------------------------------------*
*&      Form  REVERSE_ACT_MTO_MTS
*&---------------------------------------------------------------------*
*       Call Reverse FM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reverse_act_mto_mts.


**************************************************
*** MTS - Reverse using DI-B/F
  PERFORM rev_mts_act_w_di_bf.

**************************************************
*** MTO - Reverse using Time Ticket
  PERFORM rev_mto_act_w_tt.

**************************************************
*** MTS - Reverse using REM-B/F
  PERFORM rev_mts_act_w_rembf.

**************************************************
*** Not Posted data
  PERFORM rev_res_not_posted.


ENDFORM.                    " REVERSE_ACT_MTO_MTS

*&---------------------------------------------------------------------*
*&      Form  REV_MTO_CONF
*&---------------------------------------------------------------------*
*       MTO reverse
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rev_mto_conf.

  DATA : wa_l_return  LIKE bapiret1.
  DATA : lv_locked    LIKE bapi_coru_param-locked.
  DATA : lv_conf_text LIKE bapi_pp_confirm-conf_text.

* TEXT
  CLEAR lv_conf_text.
  CONCATENATE it_ztco_nmhpcpost-gjahr it_ztco_nmhpcpost-perid
              it_ztco_nmhpcpost-matnr
              sy-uname         sy-repid
         INTO lv_conf_text
         SEPARATED BY '/'.

* Cancellation of Conf. Doc.
  CALL FUNCTION 'BAPI_PRODORDCONF_CANCEL'
    EXPORTING
      confirmation        = it_ztco_nmhpcpost-rueck
      confirmationcounter = it_ztco_nmhpcpost-rmzhl
      postg_date          = gv_rev_date
      conf_text           = lv_conf_text
    IMPORTING
      return              = wa_l_return
      locked              = lv_locked
      created_conf_no     = it_ztco_nmhpcpost-rev_rueck
      created_conf_count  = it_ztco_nmhpcpost-rev_rmzhl.

* Success
  IF     wa_l_return IS INITIAL
    AND  lv_locked   EQ space
    AND  NOT it_ztco_nmhpcpost-rev_rueck IS INITIAL
    AND  NOT it_ztco_nmhpcpost-rev_rmzhl IS INITIAL.
* Mark Reverse Ind.
    it_ztco_nmhpcpost-reversed = 'X'.
* Succ. Message
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = sy-msgid
        msgnr               = sy-msgno
        msgv1               = sy-msgv1
        msgv2               = sy-msgv2
        msgv3               = sy-msgv3
        msgv4               = sy-msgv4
      IMPORTING
        message_text_output = it_ztco_nmhpcpost-message.
* Failure
  ELSE.
    it_ztco_nmhpcpost-message = wa_l_return-message.
  ENDIF.

* Already Cancelled
* If the original document is not found,
* It is considered that the doc. was already cancelled by other reasons
  IF    wa_l_return-id     = 'RU'
    AND wa_l_return-number = '122'.
* Mark Reverse Ind.
    it_ztco_nmhpcpost-reversed = 'X'.
    it_ztco_nmhpcpost-message  = text-011.
  ENDIF.

* modify
  MODIFY it_ztco_nmhpcpost.

* Result Update
  CLEAR  ztco_nmhpcpost.
  MOVE-CORRESPONDING  it_ztco_nmhpcpost  TO  ztco_nmhpcpost.

* Update   ZTCO_nMHPCPOST - Common Part
  PERFORM update_ztco_nmhpcpost_w_log.

ENDFORM.                    " REV_MTO_CONF

*&---------------------------------------------------------------------*
*&      Form  GET_LAST_REV_POS_DATE
*&---------------------------------------------------------------------*
*       Get Posting date (Reverse)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_last_rev_pos_date.
* Posting Date : The last day of period
  CLEAR gv_rev_date .
  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr = p_gjahr
      i_periv = tka01-lmona
      i_poper = it_ztco_nmhpcpost-perid
    IMPORTING
      e_date  = gv_rev_date.
ENDFORM.                    " GET_LAST_REV_POS_DATE

*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTCO_nMHPCPOST_W_LOG
*&---------------------------------------------------------------------*
*       Update   ZTCO_nMHPCPOST - Common Part
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_ztco_nmhpcpost_w_log.
* Update LOG
  ztco_nmhpcpost-aedat = sy-datum.
  ztco_nmhpcpost-aezet = sy-uzeit.
  ztco_nmhpcpost-aenam = sy-uname.
  UPDATE ztco_nmhpcpost.
  IF sy-subrc <> 0.
    ROLLBACK WORK.
    MESSAGE e050 WITH ztco_nmhpcpost-gjahr
                      ztco_nmhpcpost-perid
                      ztco_nmhpcpost-matnr
                      ztco_nmhpcpost-kostl.
  ELSE.
* Commit Work
* Single Commit : For document links
* Commit Work only when successing
* both in BAPI FM (or BDC)  update and in DB UPDATE
    COMMIT WORK AND WAIT .
  ENDIF.
ENDFORM.                    " UPDATE_ZTCO_nMHPCPOST_W_LOG

*&---------------------------------------------------------------------*
*&      Form  RESULT_LIST
*&---------------------------------------------------------------------*
*       List (result)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM result_list.

* Local Data Definition
  DATA : lv_count TYPE i.
* Total Count
  DESCRIBE TABLE it_ztco_nmhpcpost LINES sy-tfill.
  WRITE : / 'The Number of Selected Records : ', sy-tfill.

  SKIP 1.
*  CASE p_revs.
*    WHEN space.
  WRITE : / 'Posting Type : Posting of Confirmation Doc'.
  CASE gv_new.
    WHEN 'X'.
      WRITE : / 'Running Type : New Running'.
    WHEN OTHERS.
      WRITE : / 'Running Type : Re-Running'.
  ENDCASE.
* Success Count
  CLEAR lv_count.
  LOOP AT it_ztco_nmhpcpost TRANSPORTING NO FIELDS
                           WHERE NOT rueck IS INITIAL
                             AND NOT rmzhl IS INITIAL
                             AND reversed IS INITIAL.
    ADD 1 TO lv_count.
  ENDLOOP.
  SKIP 1.
  WRITE : / 'Successfully posting : ', lv_count.
*    WHEN 'X'.
*      WRITE : / 'Posting Type : Cancellation of Confirmation Doc.'.
** Success Count
*      CLEAR lv_count.
*      LOOP AT it_ztco_nmhpcpost TRANSPORTING NO FIELDS
*                               WHERE reversed = 'X'.
*        ADD 1 TO lv_count.
*      ENDLOOP.
*      SKIP 1.
*      WRITE : / 'Successfully Cancelled : ', lv_count.
** Success Count
*      DATA : lv_count2 TYPE i.
*      CLEAR lv_count2.
*      LOOP AT it_ztco_nmhpcpost TRANSPORTING NO FIELDS
*                               WHERE rev_rueck IS initial
*                                 AND rev_rmzhl IS initial
*                                 AND reversed = 'X'.
*        ADD 1 TO lv_count2.
*      ENDLOOP.
*      SKIP 1.
*      WRITE : / 'Marked as reversed     : ', lv_count2, ' out of',
*                lv_count .
*  ENDCASE.
ENDFORM.                    " RESULT_LIST

*&---------------------------------------------------------------------*
*&      Form  REV_MTS_REM_CONF
*&---------------------------------------------------------------------*
*       MTS reverse
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rev_mts_rem_conf.

  DATA : lv_exp_prtnr LIKE blpp-prtnr.
  DATA : wa_l_return  LIKE  bapiret2.

** TEXT / No text can be input
*  CLEAR LV_CONF_TEXT.
*  CONCATENATE IT_ZTCO_nMHPCPOST-GJAHR IT_ZTCO_nMHPCPOST-PERID
*              IT_ZTCO_nMHPCPOST-MATNR
*              SY-UNAME         SY-REPID
*         INTO LV_CONF_TEXT
*         SEPARATED BY '/'.

  CLEAR blpp.
  SELECT SINGLE *
           FROM blpp
          WHERE rueck = it_ztco_nmhpcpost-rueck
            AND rmzhl = it_ztco_nmhpcpost-rmzhl.

** Cancellation of Conf. Doc.
  CLEAR lv_exp_prtnr.
  CLEAR wa_l_return.
  CALL FUNCTION 'BAPI_REPMANCONF1_CANCEL'
    EXPORTING
      confirmation     = blpp-prtnr
      postdate         = gv_rev_date
*     CANC_PDCOLLNR    =
    IMPORTING
      cancconfirmation = lv_exp_prtnr
      return           = wa_l_return.

* First Bapi Commit
  COMMIT WORK AND WAIT.

* Success
  IF     NOT lv_exp_prtnr IS INITIAL
     AND     wa_l_return  IS INITIAL .
* Mark Reverse Ind.
    it_ztco_nmhpcpost-reversed = 'X'.
* Message
    CONCATENATE
     'Confirmation of Doc log. ' lv_exp_prtnr ' is cancelled'
     INTO it_ztco_nmhpcpost-message.
* Reverse Confirmation No. / '0001'
    SELECT SINGLE rueck rmzhl
             INTO (it_ztco_nmhpcpost-rev_rueck,
                   it_ztco_nmhpcpost-rev_rmzhl)
             FROM blpp
            WHERE prtnr = lv_exp_prtnr
              AND prtps = '0001'.
* Failure
  ELSE.
    it_ztco_nmhpcpost-message = wa_l_return-message.
  ENDIF.

* Already Cancelled
* If the original document is not found,
* It is considered that the doc. was already cancelled by other reasons
  IF    wa_l_return-id     = 'RM'
    AND wa_l_return-number = '472'.
* Mark Reverse Ind.
    it_ztco_nmhpcpost-reversed = 'X'.
    it_ztco_nmhpcpost-message  = text-011.
  ENDIF.

* modify
  MODIFY it_ztco_nmhpcpost.

* Result Update
  CLEAR  ztco_nmhpcpost.
  MOVE-CORRESPONDING  it_ztco_nmhpcpost  TO  ztco_nmhpcpost.

* Update   ZTCO_nMHPCPOST - Common Part
  PERFORM update_ztco_nmhpcpost_w_log.

ENDFORM.                    " REV_MTS_REM_CONF

*&---------------------------------------------------------------------*
*&      Form  PUT_RESULT_INTO_IT_ZTCO_MHPCPO
*&---------------------------------------------------------------------*
*       Put Results into IT_ZTCO_nMHPCPOST from IT_PO_POST
*                                            & IT_REM_POST
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM put_result_into_it_ztco_mhpcpo.
  LOOP AT it_ztco_nmhpcpost.
    CLEAR it_po_post.
    READ TABLE it_po_post WITH KEY
                                  gjahr = it_ztco_nmhpcpost-gjahr
                                  perid = it_ztco_nmhpcpost-perid
                                  matnr = it_ztco_nmhpcpost-matnr
                                  werks = it_ztco_nmhpcpost-werks
                                  aufnr = it_ztco_nmhpcpost-aufnr
                                  kostl = it_ztco_nmhpcpost-kostl
                                  lstar = it_ztco_nmhpcpost-lstar
                                  mhdoc = it_ztco_nmhpcpost-mhdoc.
    IF sy-subrc = 0.
      it_ztco_nmhpcpost-rueck   = it_po_post-rueck   .
      it_ztco_nmhpcpost-rmzhl   = it_po_post-rmzhl   .
      it_ztco_nmhpcpost-message = it_po_post-message .
    ELSE.
      CLEAR it_rem_post.
      READ TABLE it_rem_post WITH KEY
                                    gjahr = it_ztco_nmhpcpost-gjahr
                                    perid = it_ztco_nmhpcpost-perid
                                    matnr = it_ztco_nmhpcpost-matnr
                                    werks = it_ztco_nmhpcpost-werks
                                    aufnr = it_ztco_nmhpcpost-aufnr
                                    kostl = it_ztco_nmhpcpost-kostl
                                    lstar = it_ztco_nmhpcpost-lstar
                                    mhdoc = it_ztco_nmhpcpost-mhdoc.
      IF sy-subrc = 0.
        it_ztco_nmhpcpost-rueck   = it_rem_post-rueck   .
        it_ztco_nmhpcpost-rmzhl   = it_rem_post-rmzhl   .
        it_ztco_nmhpcpost-message = it_rem_post-message .
      ELSE.
        CLEAR it_di_post.
        READ TABLE it_di_post WITH KEY
                                      gjahr = it_ztco_nmhpcpost-gjahr
                                      perid = it_ztco_nmhpcpost-perid
                                      matnr = it_ztco_nmhpcpost-matnr
                                      werks = it_ztco_nmhpcpost-werks
                                      aufnr = it_ztco_nmhpcpost-aufnr
                                      kostl = it_ztco_nmhpcpost-kostl
                                      lstar = it_ztco_nmhpcpost-lstar
                                      mhdoc = it_ztco_nmhpcpost-mhdoc.
        it_ztco_nmhpcpost-rueck   = it_di_post-rueck   .
        it_ztco_nmhpcpost-rmzhl   = it_di_post-rmzhl   .
        it_ztco_nmhpcpost-message = it_di_post-message .
      ENDIF.
    ENDIF.
    MODIFY it_ztco_nmhpcpost.
    CLEAR it_ztco_nmhpcpost.
  ENDLOOP.
ENDFORM.                    " PUT_RESULT_INTO_IT_ZTCO_MHPCPO

*&---------------------------------------------------------------------*
*&      Form  POST_DI_B_F
*&---------------------------------------------------------------------*
*       DI Activity B/F
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_di_b_f.

  SORT it_ztco_nmhpcpost BY matnr werks.

  CLEAR : it_di_post, it_di_post[].

  LOOP AT it_ztco_nmhpcpost WHERE sauft NE space         " 'X"
                             AND sfepr EQ gv_rempf_fsc. " FSC
* Transferring data
    MOVE-CORRESPONDING it_ztco_nmhpcpost TO it_di_post.
* Check Production Version
    CLEAR mkal.

    SELECT SINGLE *  FROM mkal
                    WHERE matnr = it_di_post-matnr
                      AND werks = it_di_post-werks
                      AND verid = it_di_post-verid.
    IF sy-subrc <> 0.
      MESSAGE s051 WITH it_di_post-werks
                        it_di_post-matnr
                        it_di_post-verid
                        it_di_post-aufnr.
      STOP.
    ENDIF.

* Reversal Flag
* FLG_REVERSAL
    IF it_di_post-varquan < 0.
      it_di_post-flg_reversal = 'X'.
    ENDIF.

* Unit Always 'STD'
* Refer to program ZACO03U_MHAM -> FORM ADD_UP_DATA.
* Unit conversion was made already.

* Append
    APPEND  it_di_post.
    CLEAR   it_di_post.

    CLEAR it_ztco_nmhpcpost .
  ENDLOOP.

  CLEAR  it_di_post.

* POST using PPCVAR -> Not exactly posting action
* PPCVAR just make data in DI-B/F queue and the data can be posted
* (Activity B/F) when the run of PPCGO is completed
* PPCGO uses the data in DI-B/F queue and posts the activity
* related data
* Then, PPCGO2 posts the components related data
* IN HMMA - Add-on BAckflush Program is using 2 step B/F
* This development part is to make data for DI-B/F queue.
* So actual posting is supposed to be done after running PPCGO
* The reflection of COST data is going to be done when the background
* job for PPCGO is finished - PP module is in charge of running PPCGO
  PERFORM post_di_act_ppcvar.

ENDFORM.                    " POST_DI_B_F

*&---------------------------------------------------------------------*
*&      Form  READ_RESOURCE_DATA
*&---------------------------------------------------------------------*
*       Read resource_information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_resource_data.

  CLEAR : it_resguid16,        it_resguid16[].
  CLEAR : if_modeid16.
  CLEAR : it_act_raw,          it_act_raw[].
  CLEAR : it_ppc_show_ext_act, it_ppc_show_ext_act[].

  CALL FUNCTION 'PPC1DC_ACTS_SELECT'
    EXPORTING
      it_resguids = it_resguid16
      if_modeguid = if_modeid16
    TABLES
*     ir_budat    = so_budat
*     ir_uname    = so_uname
*     ir_rptid    = so_rptid
*     ir_actid    = so_actid
      et_acts_ext = it_act_raw
*     ET_HEADIDS  =
    .

  CALL FUNCTION 'PPC1RT_ACT_RAW_CONVERT'
    TABLES
      it_act_raw  = it_act_raw
      et_acts_ext = it_ppc_show_ext_act.

* Delete redundant records
  DELETE it_ppc_show_ext_act WHERE cost_center EQ space
                                OR acttype     EQ space.

  SORT it_ppc_show_ext_act BY cost_center acttype.
  DELETE ADJACENT DUPLICATES FROM it_ppc_show_ext_act
                  COMPARING cost_center acttype.

* Clear IT_PPC_ACT_MOD
  CLEAR : it_ppc_act_mod, it_ppc_act_mod[].

  LOOP AT it_ppc_show_ext_act WHERE acttype = p_lstar.
    LOOP AT it_act_raw
                WHERE actid = it_ppc_show_ext_act-actid.
      MOVE-CORRESPONDING it_ppc_show_ext_act TO it_ppc_act_mod.
      MOVE-CORRESPONDING it_act_raw          TO it_ppc_act_mod.
      APPEND it_ppc_act_mod.
      CLEAR  it_ppc_act_mod.
      CLEAR  it_act_raw.
    ENDLOOP.
    CLEAR it_ppc_show_ext_act.
  ENDLOOP.
ENDFORM.                    " READ_RESOURCE_DATA

*&---------------------------------------------------------------------*
*&      Form  POST_DI_ACT_PPCVAR
*&---------------------------------------------------------------------*
*       POST using PPCVAR
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_di_act_ppcvar.

  CLEAR : it_apoheads,     it_apoheads[].
  CLEAR : it_apocomplists, it_apocomplists[].
  CLEAR : it_apoactlists,  it_apoactlists[].

  CLEAR it_ppc_act_mod.

  SORT it_di_post BY flg_reversal gjahr perid matnr werks aufnr.

* Check invalid resourse
  LOOP AT it_di_post.
    CLEAR it_ppc_act_mod .
    READ TABLE  it_ppc_act_mod WITH KEY cost_center = it_di_post-kostl
                                        acttype     = it_di_post-lstar.
    IF sy-subrc <> 0.
*      MESSAGE S074 WITH 'PPCSA' IT_DI_POST-KOSTL IT_DI_POST-LSTAR.
      it_di_post-wrong_ppc = 'X'. "Wrong Master
      MODIFY it_di_post.
      CLEAR it_di_post.
    ENDIF.
  ENDLOOP.

* Building Posting Tabs
  LOOP AT it_di_post WHERE wrong_ppc NE 'X'.
    ON CHANGE OF it_di_post-flg_reversal
              OR it_di_post-gjahr
              OR it_di_post-perid
              OR it_di_post-matnr
              OR it_di_post-werks
              OR it_di_post-aufnr.
* Creation of PPC Header
      PERFORM create_ppc_header.
    ENDON.

* Creation of PPC Item  - with WA_PPC_HEAD-HEADID
    CLEAR it_apoactlists.
    it_apoactlists-headid = wa_ppc_head-headid.

    CLEAR it_ppc_act_mod .
    READ TABLE  it_ppc_act_mod WITH KEY cost_center = it_di_post-kostl
                                        acttype     = it_di_post-lstar.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

* Resource Info
    it_apoactlists-resource_guid  = it_ppc_act_mod-resource_guid.
    it_apoactlists-mode_guid      = it_ppc_act_mod-mode_guid.
* Value
    it_apoactlists-duration_var = it_di_post-varquan .
* DURATION_VAR / Delta
    it_apoactlists-delta_duration_var = it_apoactlists-duration_var.
* DELTA_DURATION_FIX
    it_apoactlists-durunit = it_di_post-meinh.

    APPEND it_apoactlists.
    CLEAR  it_apoactlists.
    CLEAR it_di_post.
  ENDLOOP.


* Call posting FM
  CLEAR : it_return, it_return[].
  CALL FUNCTION 'BAPI_MNFCTCONFRCVR_RECEIVE'
    IMPORTING
      return          = it_return
    TABLES
      it_apoheads     = it_apoheads
*     IT_APOCOMPLISTS =
      it_apoactlists  = it_apoactlists.


* Result
  LOOP AT  it_di_post WHERE wrong_ppc NE 'X'.
* Skip 'E' 'A' Error
* But Store Error Message .
    LOOP AT it_return WHERE type CA 'EA'.
      it_di_post-message = it_return-message.
    ENDLOOP.
    LOOP AT it_return WHERE type CA 'SIW'.
* Result Message (DI B/F)
      PERFORM message_for_di_bf USING text-110.
    ENDLOOP.
    IF it_return[] IS INITIAL.
* Result Message (DI B/F)
      PERFORM message_for_di_bf USING text-110.
    ENDIF.
* Storing Message
    MODIFY it_di_post.
* Result
    CLEAR ztco_nmhpcpost.
    MOVE-CORRESPONDING it_di_post TO ztco_nmhpcpost.

* Update   ZTCO_nMHPCPOST - Common Part
    PERFORM update_ztco_nmhpcpost_w_log.

    CLEAR  it_di_post.
  ENDLOOP.

* Invalid Master
  LOOP AT  it_di_post WHERE wrong_ppc = 'X'.
* Store Error Message .
    MESSAGE s074 WITH 'PPCSA' it_di_post-kostl it_di_post-lstar
    INTO  it_di_post-message.
* Storing Message
    MODIFY it_di_post.
* Result
    CLEAR ztco_nmhpcpost.
    MOVE-CORRESPONDING it_di_post TO ztco_nmhpcpost.
* Update   ZTCO_nMHPCPOST - Common Part
    PERFORM update_ztco_nmhpcpost_w_log.
    CLEAR  it_di_post.
  ENDLOOP.

ENDFORM.                    " POST_DI_ACT_PPCVAR

*&---------------------------------------------------------------------*
*&      Form  CREATE_PPC_HEADER
*&---------------------------------------------------------------------*
*       Creation of PPC Header
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_ppc_header.

* Posting Date
  DATA : lv_date LIKE sy-datum.
  CLEAR : lv_date.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr = it_di_post-gjahr
      i_periv = tka01-lmona
      i_poper = it_di_post-perid
    IMPORTING
      e_date  = lv_date.

* Clear Header Information
  CLEAR   wa_ppc_head.
  CLEAR : it_ppc_heads, it_ppc_heads[].

* Header ID
  CALL FUNCTION 'GUID_CREATE'
    IMPORTING
      ev_guid_32 = wa_ppc_head-headid.

* ... fill additional information
  GET TIME STAMP FIELD wa_ppc_head-conf_time.
  MOVE lv_date  TO wa_ppc_head-pstng_date.
  MOVE sy-uname TO wa_ppc_head-conf_username.
* Posting Ind. (Reversal Ind.)
  wa_ppc_head-flg_reversal = it_di_post-flg_reversal.

  MOVE '3' TO wa_ppc_head-flg_info_dest.    "separate variances posting
  APPEND wa_ppc_head TO it_ppc_heads.

* Header Tab.
  LOOP AT it_ppc_heads .
    MOVE-CORRESPONDING it_ppc_heads TO it_apoheads .
* MAT Infor
    it_apoheads-head_matnr = it_di_post-matnr.
    it_apoheads-prodplant  = it_di_post-werks.
    it_apoheads-version    = it_di_post-verid.

    APPEND it_apoheads .
    CLEAR  it_apoheads .
    CLEAR  it_ppc_heads.
  ENDLOOP.

ENDFORM.                    " CREATE_PPC_HEADER

*&---------------------------------------------------------------------*
*&      Form  REV_MTO_ACT_W_TT
*&---------------------------------------------------------------------*
*       MTO - Reverse using Time Ticket
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rev_mto_act_w_tt.

  SORT it_ztco_nmhpcpost BY gjahr perid sauft sfepr.

  LOOP AT it_ztco_nmhpcpost WHERE sauft = space
                             AND NOT rueck IS INITIAL
                             AND NOT rmzhl IS INITIAL.
* Posting Period
    ON CHANGE OF it_ztco_nmhpcpost-gjahr
              OR it_ztco_nmhpcpost-perid.
      PERFORM get_last_rev_pos_date.
    ENDON.
* Reverse confirmation document with counter
    PERFORM rev_mto_conf.
    CLEAR it_ztco_nmhpcpost.
  ENDLOOP.
ENDFORM.                    " REV_MTO_ACT_W_TT

*&---------------------------------------------------------------------*
*&      Form  REV_MTS_ACT_W_REMBF
*&---------------------------------------------------------------------*
*       MTS - Reverse using REM-B/F
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rev_mts_act_w_rembf.
**// Mod. by Hyung Jin Youn 2004.02.17
  LOOP AT it_ztco_nmhpcpost WHERE sauft NE space         " 'X'
                             AND sfepr EQ gv_rempf_eng  " Engine
                             AND NOT rueck IS INITIAL
                             AND NOT rmzhl IS INITIAL.
**// End of Mod.

* Posting Period
    ON CHANGE OF it_ztco_nmhpcpost-gjahr
              OR it_ztco_nmhpcpost-perid.
      PERFORM get_last_rev_pos_date.
    ENDON.
* Reverse confirmation document with counter
    PERFORM rev_mts_rem_conf.
    CLEAR it_ztco_nmhpcpost.
  ENDLOOP.

ENDFORM.                    " REV_MTS_ACT_W_REMBF

*&---------------------------------------------------------------------*
*&      Form  REV_MTS_ACT_W_DI_BF
*&---------------------------------------------------------------------*
*       MTS - Reverse using DI-B/F
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rev_mts_act_w_di_bf.

  SORT it_ztco_nmhpcpost BY matnr werks.

  CLEAR : it_di_post, it_di_post[].

**// Mod. by Hyung Jin Youn 2004.02.17
  LOOP AT it_ztco_nmhpcpost WHERE sauft  NE space         " 'X'
                             AND sfepr EQ gv_rempf_fsc   " FSC
                             AND NOT rueck IS INITIAL
                             AND NOT rmzhl IS INITIAL.
**// End of Mod.

* Transferring data
    MOVE-CORRESPONDING it_ztco_nmhpcpost TO it_di_post.
* Check Production Version
    CLEAR mkal.

    SELECT SINGLE *  FROM mkal
                    WHERE matnr = it_di_post-matnr
                      AND werks = it_di_post-werks
                      AND verid = it_di_post-verid.
    IF sy-subrc <> 0.
      MESSAGE s051 WITH it_di_post-werks
                        it_di_post-matnr
                        it_di_post-verid
                        it_di_post-aufnr.
      STOP.
    ENDIF.

*** - > Reverse
* Reverse * (-1)
    it_di_post-varquan = it_di_post-varquan  * ( -1 ).

* Reversal Flag
* FLG_REVERSAL
    IF it_di_post-varquan < 0.
      it_di_post-flg_reversal = 'X'.
    ENDIF.

* Unit Always 'STD'
* Refer to program ZACO03U_MHAM -> FORM ADD_UP_DATA.
* Unit conversion was made already.

* Append
    APPEND  it_di_post.
    CLEAR   it_di_post.

    CLEAR it_ztco_nmhpcpost .
  ENDLOOP.

  CLEAR  it_di_post.


* Same rutine as normal posting
  PERFORM post_di_act_ppcvar.


* Saving result for report
  LOOP AT it_di_post .
    LOOP AT it_ztco_nmhpcpost WHERE
                                   gjahr = it_di_post-gjahr
                               AND perid = it_di_post-perid
                               AND matnr = it_di_post-matnr
                               AND werks = it_di_post-werks
                               AND aufnr = it_di_post-aufnr
                               AND kostl = it_di_post-kostl
                               AND lstar = it_di_post-lstar
                               AND mhdoc = it_di_post-mhdoc.

* Dummy Value - no confirmation document
      it_ztco_nmhpcpost-rev_rueck =  it_di_post-rev_rueck.
      it_ztco_nmhpcpost-rev_rmzhl =  it_di_post-rev_rmzhl.
      it_ztco_nmhpcpost-reversed  =  it_di_post-reversed .
      MODIFY  it_ztco_nmhpcpost.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " REV_MTS_ACT_W_DI_BF

*&---------------------------------------------------------------------*
*&      Form  REV_RES_NOT_POSTED
*&---------------------------------------------------------------------*
*       Not Posted data - Reverse
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rev_res_not_posted.
  LOOP AT it_ztco_nmhpcpost WHERE rueck IS INITIAL
                              AND rmzhl IS INITIAL.
* Set reverse mark without cancelled ref. doc.
* because those data were not posted
* Result Update
* Mark Reverse Ind.
    it_ztco_nmhpcpost-reversed = 'X'.
* Message
    it_ztco_nmhpcpost-message  = text-010.
* modify
    MODIFY it_ztco_nmhpcpost.
* Trans. data
    CLEAR  ztco_nmhpcpost.
    MOVE-CORRESPONDING  it_ztco_nmhpcpost  TO  ztco_nmhpcpost.

* Update   ZTCO_nMHPCPOST - Common Part
    PERFORM update_ztco_nmhpcpost_w_log.

    CLEAR it_ztco_nmhpcpost.
  ENDLOOP.

ENDFORM.                    " REV_RES_NOT_POSTED

*&---------------------------------------------------------------------*
*&      Form  MESSAGE_FOR_DI_BF
*&---------------------------------------------------------------------*
*       Result Message (DI B/F)
*----------------------------------------------------------------------*
*      -->P_IT_RETURN_MESSAGE  text
*----------------------------------------------------------------------*
FORM message_for_di_bf USING    p_message.
*  CASE p_revs.
*    WHEN space.
  it_di_post-message      = p_message.
* Dummy Value - no confirmation document
  it_di_post-rueck        = '9900000000'.
  it_di_post-rmzhl        = '99000000'.
*    WHEN 'X'.
** --> Reverse.
*      it_di_post-message      = text-111.
** Dummy Value - no confirmation document
*      it_di_post-rev_rueck    = '1100000000'.
*      it_di_post-rev_rmzhl    = '11000000'.
*      it_di_post-reversed     = 'X'.
*  ENDCASE.
ENDFORM.                    " MESSAGE_FOR_DI_BF
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_AUFK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_aufk  CHANGING  p_erdat  p_aufnr.

  TYPES: BEGIN OF ty_aufk,
         aufnr TYPE aufnr,
         autyp TYPE auftyp,
         erdat TYPE auferfdat,
         pkosa TYPE pkosa_d,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_aufk.

  DATA: l_handle    TYPE sytabix,
        lt_aufk     TYPE TABLE OF aufk WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_aufk TYPE TABLE OF ty_aufk,
        ls_inx_aufk TYPE ty_aufk.

  DATA: l_erdat LIKE aufk-erdat,
        l_aufnr LIKE aufk-aufnr.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZAUFK_002'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR: l_erdat, l_aufnr.
  SELECT SINGLE MAX( erdat ) aufnr INTO (l_erdat, l_aufnr)
    FROM (l_gentab)
   WHERE pkosa = it_po_post-aufnr
     AND autyp = '10' "<- Production order
   GROUP BY aufnr.

  CHECK sy-subrc = 0 AND NOT l_aufnr IS INITIAL.

  IF l_erdat > p_erdat.
    p_aufnr = l_aufnr.
  ENDIF.

ENDFORM.                    " ARCHIVE_READ_AUFK
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_AFKO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_afko  CHANGING  p_rueck   p_rmzhl.

  TYPES: BEGIN OF ty_afko,
         aufnr TYPE aufnr,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_afko.

  DATA: l_handle    TYPE sytabix,
        lt_afko     TYPE TABLE OF afko WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_afko TYPE TABLE OF ty_afko,
        ls_inx_afko TYPE ty_afko.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZAFKO_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_afko[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_afko
    FROM (l_gentab)
   WHERE aufnr = it_rem_post-aufnr.  "<- PCC order

  CHECK NOT lt_inx_afko[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_afko, gt_afko[].
  LOOP AT lt_inx_afko INTO ls_inx_afko.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'PP_ORDER'
        archivkey                 = ls_inx_afko-archivekey
        offset                    = ls_inx_afko-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_afko, lt_afko[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'AFKO'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_afko
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_afko[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_afko INTO TABLE gt_afko.
  ENDLOOP.

  SORT gt_afko.
  DELETE ADJACENT DUPLICATES FROM gt_afko COMPARING ALL FIELDS.

  CHECK NOT gt_afko[] IS INITIAL.

  READ TABLE gt_afko INDEX 1.

  p_rueck = gt_afko-rueck.
  p_rmzhl = gt_afko-rmzhl.

ENDFORM.                    " ARCHIVE_READ_AFKO
