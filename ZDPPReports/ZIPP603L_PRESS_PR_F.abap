*----------------------------------------------------------------------*
*   INCLUDE ZIPP503L_ENGIN_PR_F                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM read_process.

* by ig.moon 09/16/2009 {
*  IF r1 EQ 'X'.
*    SELECT * FROM ztpppr
*             INTO TABLE it_ztpppr
*             WHERE ( zresult EQ 'I'
*                  OR zresult EQ 'E'
*                  OR zresult EQ 'C' )
*               AND zslno IN s_zslno
*               AND zsdat IN s_zsdat.
*
*  ELSEIF r2 EQ 'X'.
*    SELECT * FROM ztpppr
*             INTO TABLE it_ztpppr
*             WHERE zresult EQ 'E'
*               AND zslno IN s_zslno
*               AND zsdat IN s_zsdat.
*
*  ELSEIF r3 EQ 'X'.
*    SELECT * FROM ztpppr
*             INTO TABLE it_ztpppr
**             WHERE ZRESULT EQ 'S'
*             WHERE zslno IN s_zslno
*               AND zsdat IN s_zsdat.
*  ENDIF.

  IF r1 EQ 'X'.
    SELECT * FROM ztpppr
             INTO TABLE it_ztpppr
             WHERE zsdat IN s_zsdat
               AND ( zresult EQ 'I'
                  OR zresult EQ 'E'
                  OR zresult EQ 'C' )
               AND zslno IN s_zslno
    %_HINTS oracle 'FIRST_ROWS(10) INDEX("ZTPPPR" "ZTPPPR~Z01")'.

  ELSEIF r2 EQ 'X'.
    SELECT * FROM ztpppr
             INTO TABLE it_ztpppr
             WHERE zsdat IN s_zsdat
               AND zresult EQ 'E'
               AND zslno IN s_zslno
    %_HINTS oracle 'FIRST_ROWS(10) INDEX("ZTPPPR" "ZTPPPR~Z01")'.


  ELSEIF r3 EQ 'X'.
    SELECT * FROM ztpppr
             INTO TABLE it_ztpppr
             WHERE zsdat IN s_zsdat
               AND zslno IN s_zslno
    %_HINTS oracle 'FIRST_ROWS(10) INDEX("ZTPPPR" "ZTPPPR~Z01")'.

  ENDIF.
* }

  SORT it_ztpppr BY tseq prpid rseq.

ENDFORM.                    " READ_PROCESS

*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM data_process.
  DATA: l_tabix   LIKE sy-tabix    ,
        l_msg     LIKE cfgnl-msglin.

*--->**ADDED BY YPL ON 10/11/2004*UD1K912811*****

  DATA: mt_ztpppr LIKE it_ztpppr OCCURS 1 WITH HEADER LINE.
  DATA: lt_ztpppr LIKE it_ztpppr OCCURS 1 WITH HEADER LINE.
  DATA: l_item1 LIKE it_ztpppr-pnlno,
        l_item2 LIKE it_ztpppr-pnlno,
        l_item3 LIKE it_ztpppr-pnlno.
  DATA: i_count TYPE i,
        i_lines TYPE i.
  DATA: lw_ztpppr LIKE it_ztpppr.
**changed by Furong on 10/18/2006
  LOOP AT it_ztpppr.

* UD1K941165 - by IG.MOON 7/31/2007 { added 'R04'
*    IF it_ztpppr-pprdno IS INITIAL AND ( it_ztpppr-prpid = 'R03'
*            OR it_ztpppr-prpid = 'R12' OR it_ztpppr-prpid = 'R13' ).

    IF it_ztpppr-pprdno IS INITIAL AND ( it_ztpppr-prpid = 'R03'
            OR it_ztpppr-prpid = 'R12' OR it_ztpppr-prpid = 'R13'
            OR it_ztpppr-prpid = 'R04' ).

* }
      lw_ztpppr = it_ztpppr.
      lw_ztpppr-zresult = 'L'.
      lw_ztpppr-zmsg = 'Production order missing'.
      lw_ztpppr-zbdat = sy-datum.  "SAP BDC EXECUTED DATE
      lw_ztpppr-zbtim = sy-uzeit.  "SAP BDC EXECUTED TIME
      lw_ztpppr-zbnam = sy-uname.
      UPDATE ztpppr FROM lw_ztpppr.
      DELETE it_ztpppr.
*      PERFORM generate_prodction_order USING lw_ztpppr.
*      IF lw_ztpppr-pprdno IS INITIAL.
*      ELSE.
*        MODIFY it_ztpppr FROM lw_ztpppr.
*        UPDATE ztpppr FROM lw_ztpppr.
      CLEAR:it_ztpppr, lw_ztpppr.
*      ENDIF.
    ENDIF.
  ENDLOOP.

** end of change

*--->RE-ARANGING THE SEQUENCE OF IT_STPPPR

  LOOP AT it_ztpppr.
    l_tabix = sy-tabix.
    LOOP AT mt_ztpppr .
      IF mt_ztpppr-pnlno <> l_item2 AND mt_ztpppr-pnlno <> l_item3
         AND mt_ztpppr-pnlno <> l_item1.
        APPEND mt_ztpppr TO lt_ztpppr.
        DELETE TABLE mt_ztpppr FROM mt_ztpppr.
        l_item1 = l_item2.
        l_item2 = l_item3.
        l_item3 = mt_ztpppr-pnlno .
      ENDIF.
    ENDLOOP.

    IF it_ztpppr-pnlno = l_item2 OR it_ztpppr-pnlno = l_item3
       OR it_ztpppr-pnlno = l_item1.
      APPEND it_ztpppr TO mt_ztpppr.
    ELSE.
      APPEND it_ztpppr TO lt_ztpppr.
      l_item1 = l_item2.
      l_item2 = l_item3.
      l_item3 = it_ztpppr-pnlno .
    ENDIF.
  ENDLOOP.

  LOOP AT mt_ztpppr.
    APPEND mt_ztpppr TO lt_ztpppr.
  ENDLOOP.

  CLEAR: it_ztpppr,it_ztpppr[].
  it_ztpppr[] = lt_ztpppr[] .
  CLEAR: lt_ztpppr, mt_ztpppr.
  REFRESH: lt_ztpppr, mt_ztpppr.
*--->**ADDED BY YPL ON 10/11/2004*UD1K912811****

  LOOP AT it_ztpppr.

*--->**ADDED BY YPL ON 10/11/2004*UD1K912811****
    i_count = i_count + 1.
    READ TABLE it_mat WITH KEY matnr = it_ztpppr-pnlno.
** changed by Furong on 08/11/2006
    IF wa_prpid = 'RUN' AND sy-subrc = 0.
** changed on 10/17/06 due to activity issue
*    IF sy-subrc = 0.
* end of change
      WAIT UP TO 3 SECONDS.
    ELSE.
      i_lines = i_count MOD 4.
      IF i_lines = 0.
        WAIT UP TO 3 SECONDS.
      ENDIF.
    ENDIF.
    it_mat-matnr = it_ztpppr-pnlno.
    APPEND it_mat TO it_mat.
    DESCRIBE TABLE it_mat LINES i_lines.
    IF i_lines > 3.
      DELETE it_mat INDEX 1.
    ENDIF.
*    IF wa_prpid = 'RUN' AND wa_matnr = it_ztpppr-pnlno.
*      WAIT UP TO 3 SECONDS .
*    ENDIF.
*--->**ADDED BY YPL ON 10/11/2004*UD1K912811*****

    l_tabix = sy-tabix.
    CLEAR: it_timetickets        , it_goodsmovements,
           it_timetickets[]      , it_goodsmovements[],
           it_link_conf_goodsmov , it_link_conf_goodsmov[] ,
           it_detail_return      , it_detail_return[] ,
           it_return             , it_return[] .
*----> Alpha Convert of Prod Order No
    CALL FUNCTION 'CONVERSION_EXIT_AUFNR_INPUT'
         EXPORTING
              input  = it_ztpppr-pprdno
         IMPORTING
              output = wa_pprdno.

*----> PROCESS Report Point
    CLEAR: wa_prpid .

*
*
    PERFORM process_report_point  .
*
*

    IF wa_return-type EQ space.
      PERFORM it_ztpppr_modify USING wa_return-message
                                     'S'
                                     l_tabix.
    ELSE.
      PERFORM it_ztpppr_modify USING wa_return-message
                                     wa_return-type
                                     l_tabix.
    ENDIF.
    MOVE-CORRESPONDING it_ztpppr TO wa_ztpppr.
    IF it_ztpppr-zresult EQ 'E' .
      MOVE-CORRESPONDING it_ztpppr TO it_error .
      APPEND it_error .
    ENDIF.
    UPDATE ztpppr FROM wa_ztpppr.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    ENDIF.

    wa_matnr  = it_ztpppr-pnlno.
    wa_aufnr  = it_ztpppr-pprdno.

    CLEAR: marc,
           wa_return   ,
           it_error    ,
           it_detail_return.

  ENDLOOP.

*----> GR Material Doc
  LOOP AT it_ztpppr WHERE zresult EQ 'S' .
    CLEAR wa_ztpppr .
    CASE it_ztpppr-prpid .

* UD1K941165 - by IG.MOON 7/31/2007 {
*     WHEN 'R03' OR 'R12' OR 'R13' OR 'R23'.
      WHEN 'R03' OR 'R12' OR 'R13' OR 'R23' OR 'R04'.
* }
*        PERFORM message_gr .
        MOVE-CORRESPONDING it_ztpppr TO wa_ztpppr.
        UPDATE ztpppr FROM wa_ztpppr.
    ENDCASE .

  ENDLOOP.
ENDFORM.                    " DATA_PROCESS

*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
FORM rkc_msg_string CHANGING p_msg.

  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            id      = wa_return-type
            mtype   = wa_return-id
            number  = wa_return-number
            par1    = wa_return-message_v1
            par2    = wa_return-message_v2
            par3    = wa_return-message_v3
            par4    = wa_return-message_v4
       IMPORTING
            msg_lin = p_msg
       EXCEPTIONS
            OTHERS  = 1.
ENDFORM.                    " RKC_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  IT_ZTPPPR_MODIFY
*&---------------------------------------------------------------------*
FORM it_ztpppr_modify USING p_msg
                            p_msgty
                            p_tabix.

  CASE p_msgty.
    WHEN 'S'.
      it_ztpppr-zbdat = sy-datum.  "SAP BDC EXECUTED DATE
      it_ztpppr-zbtim = sy-uzeit.  "SAP BDC EXECUTED TIME
      it_ztpppr-zbnam = sy-uname.  "BDC User ID
*     C: Create U: Update D: Delete
      it_ztpppr-zmode = 'C'.       "C:CREATE
*      it_ztpppr-zmsg    = p_msg.
      it_ztpppr-zmsg    = ''.
      it_ztpppr-zresult = p_msgty.

    WHEN 'I'.
      it_ztpppr-zbdat = sy-datum.  "SAP BDC EXECUTED DATE
      it_ztpppr-zbtim = sy-uzeit.  "SAP BDC EXECUTED TIME
      it_ztpppr-zbnam = sy-uname.  "BDC User ID
*     C: Create U: Update D: Delete
      it_ztpppr-zmode = 'C'.       "C:CREATE
      it_ztpppr-zresult = 'S' .
      it_ztpppr-zmsg    = 'Classification Update successfully!!'.

    WHEN 'E'.
      it_ztpppr-zbdat = sy-datum.  "SAP BDC EXECUTED DATE
      it_ztpppr-zbtim = sy-uzeit.  "SAP BDC EXECUTED TIME
      it_ztpppr-zbnam = sy-uname.  "BDC User ID
      it_ztpppr-zmode = 'C'.       "C:CREATE
      it_ztpppr-zmsg    = p_msg.
      it_ztpppr-zresult = p_msgty.

    WHEN OTHERS.
      it_ztpppr-zbdat = sy-datum.  "SAP BDC EXECUTED DATE
      it_ztpppr-zbtim = sy-uzeit.  "SAP BDC EXECUTED TIME
      it_ztpppr-zbnam = sy-uname.  "BDC User ID
      it_ztpppr-zmode = 'C'.       "C:CREATE
      it_ztpppr-zmsg    = p_msg.
      it_ztpppr-zresult = p_msgty.
  ENDCASE.

  MODIFY it_ztpppr INDEX p_tabix TRANSPORTING zbdat
                                              zbtim
                                              zbnam
                                              zmode
                                              zresult
                                              zmsg
                                              mblnr
                                              mjahr.

ENDFORM.                    " IT_ZTPPPR_MODIFY
*&---------------------------------------------------------------------*
*&      Form  PR_CID_R03
*&---------------------------------------------------------------------*
FORM pr_cid_r03.
  IF it_ztpppr-pqty  > 0.
*----> GetTimeTicketProposal
    PERFORM gettimeticket_proposal USING 'YIELD'.   "CHECK YIELD
  ENDIF.

  IF it_ztpppr-psqty > 0.
*----> GetTimeTicketProposal
    PERFORM gettimeticket_proposal USING 'SCRAP'.   "CHECK SCRAP
  ENDIF.

  IF it_ztpppr-prqty > 0                        .   "CHECK REWORK
*----> GetTimeTicketProposal
    PERFORM gettimeticket_proposal USING 'REWORK'.
  ENDIF.

  IF wa_return-type = 'E'  OR  wa_return-type = 'C'  .
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
  ENDIF.
ENDFORM.                    " PR_CID_R03

*&---------------------------------------------------------------------*
*&      Form  PR_CID_R12
*&---------------------------------------------------------------------*
FORM pr_cid_r12.

  IF it_ztpppr-pqty  > 0.
*----> GetTimeTicketProposal
    PERFORM gettimeticket_proposal USING 'YIELD'.   "CHECK YIELD

  ENDIF.

  IF it_ztpppr-psqty > 0.
*----> GetTimeTicketProposal
    PERFORM gettimeticket_proposal USING 'SCRAP'.   "CHECK SCRAP
  ENDIF.

  IF it_ztpppr-prqty > 0                        .   "CHECK REWORK
*----> GetTimeTicketProposal
    PERFORM gettimeticket_proposal USING 'REWORK'.
  ENDIF.

  IF wa_return-type = 'E'  OR  wa_return-type = 'C'  .
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
    PERFORM update_classification USING 'PRS_PNL_CQP'
                                        it_ztpppr-plqty
                                        it_ztpppr-pnlno.
  ENDIF.
ENDFORM.                    " PR_CID_R12

*&---------------------------------------------------------------------*
*&      Form  PR_CID_R23
*&---------------------------------------------------------------------*
FORM pr_cid_r23 USING p_prpid.
*Issue number : 20041105-002 Scrap Process at R14,R15
*Requested by Moon,changed by wskim,on 20041109
*Logic change
*-----Start
**  IF IT_ZTPPPR-PQTY  > 0.
***----> GetTimeTicketProposal
**    PERFORM GETTIMETICKET_PROPOSAL USING 'YIELD'.   "CHECK YIELD
**
**  ENDIF.
*
*  IF it_ztpppr-psqty > 0.
**----> GetTimeTicketProposal
*    PERFORM gettimeticket_proposal USING 'SCRAP'.   "CHECK SCRAP
*  ENDIF.
*
**  IF IT_ZTPPPR-PRQTY > 0                        .   "CHECK REWORK
***----> GetTimeTicketProposal
**    PERFORM GETTIMETICKET_PROPOSAL USING 'REWORK'.
**  ENDIF.
*------add
  IF it_ztpppr-psqty > 0.
    PERFORM mb1a_gi_r23 USING p_prpid.
*-----End
    IF wa_return-type = 'E'  OR  wa_return-type = 'C'  .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
      PERFORM update_classification USING 'PRS_PNL_CQP'
                                          it_ztpppr-plqty
                                          it_ztpppr-pnlno.
    ENDIF.
  ENDIF.
ENDFORM.                    " PR_CID_R23

*&---------------------------------------------------------------------*
*&      Form  PR_CID_R13
*&---------------------------------------------------------------------*
FORM pr_cid_r13.
  CLEAR: it_timetickets, it_goodsmovements, it_timetickets[].

  CALL FUNCTION 'CONVERSION_EXIT_AUFNR_INPUT'
       EXPORTING
            input  = it_ztpppr-pprdno
       IMPORTING
            output = wa_pprdno. "IT_ZTPPPR-PPRDNO.

  it_timetickets-orderid   = wa_pprdno .
  it_timetickets-operation = '0010'.
  it_timetickets-yield     = it_ztpppr-pqty.
  it_timetickets-scrap     = it_ztpppr-psqty.
  it_timetickets-conf_quan_unit = it_ztpppr-meins.
** added by Furong on 08/10/2006
** changed on 10/17/06 due to activity issue
  it_timetickets-postg_date = wa_datum.
** end of change
  it_timetickets-exec_start_date = it_ztpppr-rdate.
  it_timetickets-exec_start_time = it_ztpppr-rtime.
  APPEND it_timetickets.

  it_goodsmovements-batch      = it_ztpppr-cbno.
  it_goodsmovements-spec_stock = 'S'           .
  APPEND it_goodsmovements.
  CLEAR it_goodsmovements.

  CALL FUNCTION 'BAPI_PRODORDCONF_CREATE_TT'
       IMPORTING
            return         = return
       TABLES
            timetickets    = it_timetickets
            goodsmovements = it_goodsmovements
            detail_return  = it_detail_return.

  PERFORM check_return          .

* IF IT_DETAIL_RETURN-TYPE NE 'E'.
  IF wa_return-type =  'E' OR wa_return-type =  'C'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
  ELSE.
*----> CHECK REWORK
    IF it_ztpppr-prqty > 0.    "CHECK REWORK
      CLEAR: it_timetickets,   it_goodsmovements,
             return,           it_detail_return,
             it_timetickets[], it_goodsmovements[],
             it_detail_return[].
*----> GetTimeTicketProposal
      PERFORM gettimeticket_proposal USING 'REWORK'.
*----> CreateTimeTicketMultiple
      PERFORM createtimeticket_multiple.
      PERFORM check_return           .

      IF wa_return-type = 'E'  OR  wa_return-type = 'C'  .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
             EXPORTING
                  wait = 'X'.
      ENDIF.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.
  ENDIF.
ENDFORM.                    " PR_CID_R13

*&---------------------------------------------------------------------*
*&      Form  PR_CID_R14
*&---------------------------------------------------------------------*
FORM pr_cid_r14 USING p_prpid.
*Issue number : 20041105-002 Scrap Process at R14,R15
*Requested by Moon,changed by wskim,on 20041109
*Logic change
*-----Start
  IF it_ztpppr-pqty  > 0. " Yield
*-----End
    CLEAR : goodsmvt_header, it_goodsmvt_item,
            it_goodsmvt_item[].

*-----> GOODS MOVEMENT HEADER
    goodsmvt_header-pstng_date = wa_datum       .
    goodsmvt_header-header_txt = it_ztpppr-rtime.
*  GM_Code 01: Goods receipt for purchase order
*  GM_Code 02: Goods receipt for production order
*  GM_Code 03: Goods issue
*  GM_Code 04: Transfer posting
*  GM_Code 05: Other goods receipts
*  GM_Code 06: Reversal of goods movements
    goodsmvt_code = '04'.

    CLEAR : ztpp_mip_stk_tra, mkal, marc.

    SELECT SINGLE *
              FROM mkal
              WHERE werks EQ 'P001'
                AND matnr EQ it_ztpppr-pnlno
                AND verid EQ '01'.
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM mara WHERE matnr EQ it_ztpppr-pnlno.
      IF sy-subrc EQ 0.
        MOVE: 'E'      TO wa_return-type,
              text-m01 TO wa_return-message.
      ELSE.
        MOVE: 'E'      TO wa_return-type,
              text-m02 TO wa_return-message.
      ENDIF.

      EXIT.
    ENDIF.

    IF it_ztpppr-eusage EQ '01'.
      SELECT SINGLE *
             FROM ztpp_mip_stk_tra
             WHERE werks  EQ mkal-werks
               AND eusage EQ it_ztpppr-eusage.
      it_goodsmvt_item-move_type  = ztpp_mip_stk_tra-bwart. "MVT type
      it_goodsmvt_item-move_stloc = ztpp_mip_stk_tra-lgort. "A/S Stock

    ELSE.
      SELECT SINGLE *
                FROM marc
                WHERE werks EQ 'P001'
                  AND matnr EQ it_ztpppr-pnlno.
      it_goodsmvt_item-move_type  = '311'.                "
      it_goodsmvt_item-move_stloc = 'P230'.           "TO SLOC
    ENDIF.

    it_goodsmvt_item-material   = it_ztpppr-pnlno.        "MATERIAL
    it_goodsmvt_item-plant      = mkal-werks.             "PLANT
    it_goodsmvt_item-stge_loc   = 'P120'.             "FROM SLOC
    it_goodsmvt_item-entry_qnt  = it_ztpppr-pqty.         "QTY
    it_goodsmvt_item-entry_uom  = it_ztpppr-meins.        "UoM
*  IT_GOODSMVT_ITEM-MOVE_STLOC = ZTPP_MIP_STK_TRA-LGORT. "A/S Stock
    APPEND it_goodsmvt_item.

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header             = goodsmvt_header
        goodsmvt_code               = goodsmvt_code
      IMPORTING
        materialdocument            = it_ztpppr-mblnr
        matdocumentyear             = it_ztpppr-mjahr
      TABLES
        goodsmvt_item               = it_goodsmvt_item
*      GOODSMVT_SERIALNUMBER       =
        return                      = it_return.


    LOOP AT it_return.
      MOVE-CORRESPONDING it_return TO wa_return.
    ENDLOOP.
*Issue number : 20041105-002 Scrap Process at R14,R15
*Requested by Moon,changed by wskim,on 20041109
*Logic change
*-----Start
  ELSEIF it_ztpppr-psqty > 0. "scrap
    PERFORM mb1a_gi_r23 USING p_prpid.
*-----End
  ENDIF.

  IF wa_return-type NE 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
    PERFORM update_classification USING 'PRS_PNL_CQP'
                                        it_ztpppr-plqty
                                        it_ztpppr-pnlno.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
** added by Furong on 08/10/2006
    wa_return-type = 'P'.
** end of change
  ENDIF.

ENDFORM.                    " PR_CID_R14
*&---------------------------------------------------------------------*
*&      Form  PR_CID_R15
*&---------------------------------------------------------------------*
FORM pr_cid_r15 USING p_prpid.
*Issue number : 20041105-002 Scrap Process at R14,R15
*Requested by Moon,changed by wskim,on 20041109
*Logic change
*-----Start
  IF it_ztpppr-pqty  > 0. " Yield
*-----End
    CLEAR : goodsmvt_header, it_goodsmvt_item,
            it_goodsmvt_item[].

*-----> GOODS MOVEMENT HEADER
    goodsmvt_header-pstng_date = wa_datum       .
    goodsmvt_header-header_txt = it_ztpppr-rtime.
*  GM_Code 01: Goods receipt for purchase order
*  GM_Code 02: Goods receipt for production order
*  GM_Code 03: Goods issue
*  GM_Code 04: Transfer posting
*  GM_Code 05: Other goods receipts
*  GM_Code 06: Reversal of goods movements
    goodsmvt_code = '04'.

    CLEAR : ztpp_mip_stk_tra, mkal, marc.

    SELECT SINGLE *
              FROM mkal
              WHERE werks EQ 'P001'
                AND matnr EQ it_ztpppr-pnlno
                AND verid EQ '01'.
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM mara WHERE matnr EQ it_ztpppr-pnlno.
      IF sy-subrc EQ 0.
        MOVE: 'E'      TO wa_return-type,
              text-m01 TO wa_return-message.
      ELSE.
        MOVE: 'E'      TO wa_return-type,
              text-m02 TO wa_return-message.
      ENDIF.

      EXIT.
    ENDIF.

    IF it_ztpppr-eusage EQ '01'.
      SELECT SINGLE *
             FROM ztpp_mip_stk_tra
             WHERE werks  EQ mkal-werks
               AND eusage EQ it_ztpppr-eusage.
      it_goodsmvt_item-move_type  = ztpp_mip_stk_tra-bwart. "MVT type
      it_goodsmvt_item-move_stloc = ztpp_mip_stk_tra-lgort. "A/S Stock

    ELSE.
      SELECT SINGLE *
                FROM marc
                WHERE werks EQ 'P001'
                  AND matnr EQ it_ztpppr-pnlno.
      it_goodsmvt_item-move_type  = '311'.                "
      it_goodsmvt_item-move_stloc = 'P230'.           "TO SLOC
    ENDIF.

    it_goodsmvt_item-material   = it_ztpppr-pnlno.        "MATERIAL
    it_goodsmvt_item-plant      = mkal-werks.             "PLANT
*    it_goodsmvt_item-stge_loc   = 'P121'.             "FROM SLOC
    it_goodsmvt_item-stge_loc   = 'P120'.             "FROM SLOC
    it_goodsmvt_item-entry_qnt  = it_ztpppr-pqty.         "QTY
    it_goodsmvt_item-entry_uom  = it_ztpppr-meins.        "UoM
*  IT_GOODSMVT_ITEM-MOVE_STLOC = ZTPP_MIP_STK_TRA-LGORT. "A/S Stock
    APPEND it_goodsmvt_item.

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header             = goodsmvt_header
        goodsmvt_code               = goodsmvt_code
      IMPORTING
        materialdocument            = it_ztpppr-mblnr
        matdocumentyear             = it_ztpppr-mjahr
      TABLES
        goodsmvt_item               = it_goodsmvt_item
*      GOODSMVT_SERIALNUMBER       =
        return                      = it_return.

    LOOP AT it_return.
      MOVE-CORRESPONDING it_return TO wa_return.
    ENDLOOP.
*Issue number : 20041105-002 Scrap Process at R14,R15
*Requested by Moon,changed by wskim,on 20041109
*Logic change
*-----Start
  ELSEIF it_ztpppr-psqty > 0. "scrap
    PERFORM mb1a_gi_r23 USING p_prpid.
*-----End
  ENDIF.

  IF wa_return-type NE 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
    PERFORM update_classification USING 'PRS_PNL_CQP'
                                        it_ztpppr-plqty
                                        it_ztpppr-pnlno.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
** added by Furong on 08/10/2006
    wa_return-type = 'P'.
** end of change
  ENDIF.

ENDFORM.                    " PR_CID_R15


*FORM pr_cid_r15 USING p_prpid.
* goodsmvt_header-pstng_date = it_ztpppr-rdate.
*  goodsmvt_header-header_txt = it_ztpppr-rtime.

*  it_goodsmvt_item-plant      = 'P001'.
*  it_goodsmvt_item-stge_loc   = 'P121'.
*  it_goodsmvt_item-move_type  = '311'.
*  it_goodsmvt_item-entry_qnt  = it_ztpppr-pqty.
*  it_goodsmvt_item-entry_uom  = it_ztpppr-meins.

*  SELECT SINGLE * FROM marc
*         WHERE matnr EQ it_ztpppr-pnlno.

* it_goodsmvt_item-move_stloc = marc-lgpro.

*  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
*    EXPORTING
*      goodsmvt_header             = goodsmvt_header
*    IMPORTING
*      materialdocument            = it_ztpppr-mblnr
*      matdocumentyear             = it_ztpppr-mjahr
*    TABLES
*      goodsmvt_item               = it_goodsmvt_item
*      GOODSMVT_SERIALNUMBER       =
*      return                      = it_return.

*  LOOP AT it_return.
*    MOVE-CORRESPONDING it_return TO wa_return.
*  ENDLOOP.
*  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
*       EXPORTING
*            type   = wa_return-type
*            cl     = wa_return-id
*            number = wa_return-number
*            par1   = wa_return-message_v1
*            par2   = wa_return-message_v2
*            par3   = wa_return-message_v3
*            par4   = wa_return-message_v4
*       IMPORTING
*            return = wa_return.

*  IF it_detail_return-type NE 'E'.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*         EXPORTING
*              wait = 'X'.
*  ELSE.
*   ROLLBACK WORK.
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*  ENDIF.

*  PERFORM UPDATE_CLASSIFICATION USING 'PRS_PNL_CQP'
*                                      IT_ZTPPPR-PLQTY
*                                      IT_ZTPPPR-PNLNO.

*ENDFORM.                    " PR_CID_R15

*&---------------------------------------------------------------------*
*&      Form  CALL_MESSSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RETURN  text
*----------------------------------------------------------------------*
FORM call_messsage USING    pa_return LIKE bapiret1 .
  CALL FUNCTION 'BALW_BAPIRETURN_GET1'
       EXPORTING
            type       = pa_return-type
            cl         = pa_return-id
            number     = pa_return-number
            par1       = pa_return-message_v1
            par2       = pa_return-message_v2
            par3       = pa_return-message_v3
            par4       = pa_return-message_v4
       IMPORTING
            bapireturn = pa_return.

ENDFORM.                    " CALL_MESSSAGE

*&---------------------------------------------------------------------*
*&      Form  CALL_BAPI_PRODORDCONF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bapi_prodordconf.
*  IT_TIMETICKETS-ORDERID = IT_ZTPPPR-PPRDNO.         "Ordre No
  it_timetickets-orderid = wa_pprdno.         "Ordre No
  it_timetickets-operation = '0010'.                 "SEQ
  it_timetickets-yield      = it_ztpppr-pqty .       "Yield
  it_timetickets-scrap      = it_ztpppr-psqty.       "Scrap
  it_timetickets-conf_quan_unit  = it_ztpppr-meins.  "UoM
** added by Furong on 08/10/2006
** changed on 10/17/06 due to activity issue
  it_timetickets-postg_date = wa_datum.
** end of change
  it_timetickets-postg_date = it_ztpppr-rdate.       "Posting date
  it_timetickets-plant      = 'P001'.                "Plant
  APPEND it_timetickets.

*  SELECT SINGLE * FROM RESB
*         WHERE AUFNR EQ IT_ZTPPPR-PPRDNO
*           AND BAUGR EQ IT_ZTPPPR-PNLNO
*           AND WERKS EQ 'P001'.
*
*  IT_GOODSMOVEMENTS-BATCH      = IT_ZTPPPR-CBNO.
*  IT_GOODSMOVEMENTS-PLANT      = 'P001'.
**  IT_GOODSMOVEMENTS-STGE_LOC   =
*  IT_GOODSMOVEMENTS-MOVE_TYPE  = '261'.
*  IT_GOODSMOVEMENTS-MATERIAL   = RESB-MATNR  . "IT_ZTPPPR-PNLNO.

*  APPEND IT_GOODSMOVEMENTS.    CLEAR IT_GOODSMOVEMENTS.
*-----> Create Time Ticket Multiple
  CALL FUNCTION 'BAPI_PRODORDCONF_CREATE_TT'
       IMPORTING
            return         = return
       TABLES
            timetickets    = it_timetickets
            goodsmovements = it_goodsmovements
            detail_return  = it_detail_return.

ENDFORM.                    " CALL_BAPI_PRODORDCONF

*&---------------------------------------------------------------------*
*&      Form  CHECK_RETURN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_return.
  DATA: l_check         TYPE c.

*  PERFORM write_return.
*  PERFORM write_detret.

  CLEAR: wa_return-type.
  LOOP AT it_detail_return WHERE type = 'E' OR type = 'A'.
    MOVE-CORRESPONDING it_detail_return TO wa_return.
    wa_return-type =  'E'.
    l_check        =  'X'.
  ENDLOOP.
  PERFORM call_messsage     USING wa_return.
  IF l_check =  'X'  AND  it_detail_return-number = '469'.
    wa_return-type =  'C'.      " Locking. For the RE-Processing..
  ENDIF.
ENDFORM.                    " CHECK_RETURN
*&---------------------------------------------------------------------*
*&      Form  GETTIMETICKET_PROPOSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0601   text
*----------------------------------------------------------------------*
FORM gettimeticket_proposal USING  pa_ind.
  DATA: lt_timetickets LIKE TABLE OF it_timetickets WITH HEADER LINE.

  wa_prpid = 'RUN'  .
  CASE pa_ind.
    WHEN 'YIELD'.
      it_timetickets-yield      = it_ztpppr-pqty .   "Yield
    WHEN 'SCRAP'.
      it_timetickets-scrap      = it_ztpppr-psqty.   "Scrap
    WHEN 'REWORK'.
      it_timetickets-yield      = it_ztpppr-prqty .  "Rework
  ENDCASE.

  it_timetickets-orderid   = wa_pprdno.              "Order No
  it_timetickets-operation = '0010'.                 "SEQ
  it_timetickets-conf_quan_unit  = it_ztpppr-meins.  "UoM
  it_timetickets-plant     = 'P001'.                 "Plant
  it_timetickets-postg_date = wa_datum.              "Posting date

  APPEND it_timetickets.
** added by Furong on 08/10/2006
** changed on 10/17/06 due to activity issue
  lt_timetickets[] = it_timetickets[].
** end of change
*Issue number : 20041105-003
*Requested by Moon,changed by wskim,on 20041109
*Mssing input parameters : Don't be calculated activity
*-----Start
  wa_propose-activity = 'X'.
*-----End
  wa_propose-goodsmovement = 'X'.

*----> GetTimeTicketProposal
  CALL FUNCTION 'BAPI_PRODORDCONF_GET_TT_PROP'
       EXPORTING
            propose            = wa_propose
       IMPORTING
            return             = return
       TABLES
            timetickets        = it_timetickets
            goodsmovements     = it_goodsmovements
            link_conf_goodsmov = it_link_conf_goodsmov
            detail_return      = it_detail_return.

** added by Furong on 08/10/2006
** changed on 10/17/06 due to activity issue
*  it_timetickets[] = lt_timetickets[].
  LOOP AT it_timetickets.
    it_timetickets-postg_date = wa_datum.
    MODIFY it_timetickets TRANSPORTING postg_date.
  ENDLOOP.
** end of change
  DATA l_tabix   TYPE  sy-tabix.
  CASE pa_ind.
    WHEN 'YIELD' OR 'SCRAP'.
      LOOP AT it_goodsmovements WHERE move_type EQ '261'.
        l_tabix = sy-tabix.
        IF it_ztpppr-prpid EQ 'R03'.
          MOVE it_ztpppr-cbno  TO  it_goodsmovements-batch.
        ENDIF.
        MODIFY it_goodsmovements INDEX l_tabix .
      ENDLOOP.

    WHEN 'REWORK'.
*      DATA WA_GOODSMOVEMENTS  LIKE IT_GOODSMOVEMENTS.
      LOOP AT it_goodsmovements.
        l_tabix = sy-tabix .
        IF it_goodsmovements-move_type EQ '261'.
          IF it_ztpppr-prpid EQ 'R03' .
            MOVE it_ztpppr-cbno  TO  it_goodsmovements-batch.
          ENDIF.
        ELSEIF it_goodsmovements-move_type EQ '101'.
          MOVE 'P128'          TO  it_goodsmovements-stge_loc.
        ENDIF.
        MODIFY it_goodsmovements  INDEX l_tabix .
      ENDLOOP.
  ENDCASE.

*----> CreateTimeTicketMultiple
  PERFORM createtimeticket_multiple           .
  PERFORM check_return                        .

* Get Material Document created for confrimation

*select  * from   AFWI where RUECK = it_timetickets-CONF_NO  and
*                            RMZHL = ( select max( RMZHL ) from  AFWI
*                                       where RUECK eq
*                                       it_timetickets-CONF_NO ).
*exit.
*endselect.
*    it_ztpppr-mblnr = AFWI-MBLNR.
*    it_ztpppr-mjahr = AFWI-MJAHR.
  CLEAR :  it_ztpppr-mblnr, it_ztpppr-mjahr.
*  data : l_conf type BAPI_PP_CONF_KEY-CONF_CNT.
*  loop at it_link_conf_goodsmov.
*    l_conf = it_link_conf_goodsmov-INDEX_CONFIRM.
*
*    CALL FUNCTION 'BAPI_PRODORDCONF_GETDETAIL'
*      EXPORTING
*        CONFIRMATION              = it_timetickets-CONF_NO
*        CONFIRMATIONCOUNTER       = l_conf
*     IMPORTING
*        RETURN                    =  return
**   CONF_DETAIL               =
*     TABLES
*       GOODSMOVEMENTS             =  it_movt_type .
**   FAILEDGMOVES              =
*
*    read table it_movt_type index 1.
*    it_ztpppr-mblnr = it_movt_type-MAT_DOC.
*    it_ztpppr-mjahr = it_movt_type-DOC_YEAR.
*    exit.
*  endloop.

ENDFORM.                    " GETTIMETICKET_PROPOSAL
*&---------------------------------------------------------------------*
*&      Form  CREATETIMETICKET_MULTIPLE
*&---------------------------------------------------------------------*
FORM createtimeticket_multiple.
*----> CreateTimeTicketMultiple
  CALL FUNCTION 'BAPI_PRODORDCONF_CREATE_TT'
       IMPORTING
            return             = return
       TABLES
            timetickets        = it_timetickets
            goodsmovements     = it_goodsmovements
            link_conf_goodsmov = it_link_conf_goodsmov
            detail_return      = it_detail_return.
ENDFORM.                    " CREATETIMETICKET_MULTIPLE
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM at_selection_screen.
  DATA l_days    TYPE   sy-tabix .
*---> When displaying, only 1 day display is possible
  IF r3 EQ c_mark.
    IF s_zsdat[] IS INITIAL.
      MESSAGE e001 WITH text-101.
    ELSE.
      l_days = s_zsdat-high - s_zsdat-low .
      IF l_days GT 0.
        MESSAGE e001 WITH text-101.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " AT_SELECTION_SCREEN

*&---------------------------------------------------------------------*
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
FORM execute_process.
  CASE c_mark.
    WHEN r1 OR r2.
      PERFORM write_open.      "Write Header for Batch job
      PERFORM read_process.    "Read from ZTPPER Where condition
      PERFORM data_process.    "Data processing for Report Point
      PERFORM write_result.    "Write Result for Batch job
      PERFORM write_close.     "Write Botton for Batch job
    WHEN r3.
      PERFORM read_process.    "Read from ZTPPER Where condition
      CALL SCREEN 9000 .
  ENDCASE.
ENDFORM.                    " EXECUTE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_OPEN
*&---------------------------------------------------------------------*
FORM write_open.
  WRITE :/ '********** BEGIN OF PROCESS ***********'.
**S> 08/04/11 Paul
  WRITE AT: /001(030) 'Processing Time...(start)' ,
**E<
             031(010) sy-datum                    ,
             042(010) sy-uzeit                    .
  CASE c_mark.
    WHEN r1.
      WRITE :/ text-301.
    WHEN r2.
      WRITE :/ text-302.
  ENDCASE.
  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  SKIP 2.
ENDFORM.                    " WRITE_OPEN
*&---------------------------------------------------------------------*
*&      Form  WRITE_RESULT
*&---------------------------------------------------------------------*
FORM write_result.
  DATA : l_line          TYPE    sy-index ,
         l_ztpppr_ix     TYPE    sy-tabix ,
         l_error_ix      TYPE    sy-tabix ,
         l_success_ix    TYPE    sy-tabix .

  DESCRIBE TABLE it_ztpppr  LINES   l_ztpppr_ix .
  DESCRIBE TABLE it_error   LINES   l_error_ix  .
  l_success_ix   =  l_ztpppr_ix  -  l_error_ix  .
  WRITE:/ text-311 , l_ztpppr_ix  .
  WRITE:/ text-312 , l_success_ix .
  WRITE:/ text-313 , l_error_ix   .
  SKIP 2.

  LOOP AT it_error.
    AT FIRST.
      FORMAT RESET INTENSIFIED ON.
      WRITE :/ '********** BEGIN OF ERROR Detail List ***********'.
    ENDAT.
    l_line = sy-tabix MOD 2.
    IF l_line EQ 1.
      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    ELSE.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ENDIF.
    WRITE :/ it_error-tseq      COLOR COL_KEY ,
             it_error-prpid     COLOR COL_KEY ,
             it_error-rseq      COLOR COL_KEY .
*             it_error-eitem     COLOR COL_KEY ,
*             it_error-eassyid   COLOR COL_KEY ,
*             it_error-epono     COLOR COL_KEY ,

*             color COL_NEGATIVE,color COL_NORMAL.
    AT LAST.
      FORMAT RESET INTENSIFIED ON.
      WRITE :/ '********** END OF ERROR Detail List ***********'.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " WRITE_RESULT
*&---------------------------------------------------------------------*
*&      Form  WRITE_CLOSE
*&---------------------------------------------------------------------*
FORM write_close.
  SKIP 2.
  WRITE :/ '********** END OF PROCESS ***********'.
**S> 08/04/11 Paul
  WRITE AT: /001(030) 'Processing Time...(end)' ,
**E<
           031(010) sy-datum                    ,
           042(010) sy-uzeit                    .
  WRITE :/ '********** END OF PROCESS ***********'.
ENDFORM.                    " WRITE_CLOSE
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET TITLEBAR '9000'.
  SET PF-STATUS 'MAIN'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_grid OUTPUT.
  IF gs_custom_container IS INITIAL.
*-----> CREATE OBJECT
*    CREATE OBJECT GS_APPLICATION.

    CREATE OBJECT gs_custom_container
        EXPORTING container_name = wa_container.

    CREATE OBJECT alv_grid
        EXPORTING i_parent = gs_custom_container.

    PERFORM  build_variant.
    PERFORM  build_layout.
    PERFORM  build_fieldcat.

*-----> SET OBJECT
    CALL METHOD alv_grid->set_table_for_first_display
      EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
        i_structure_name              = 'ZTPPPR'
        is_variant                    = gs_variant
        i_save                        = 'A'
*        I_DEFAULT                     = 'X'
        is_layout                     = gs_layout
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
      CHANGING
        it_outtab                     = it_ztpppr[]
        it_fieldcatalog               = gt_fieldcat[]
*        IT_SORT                       =
*        IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4  .

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_VARIANT
*&---------------------------------------------------------------------*
FORM build_variant.
  gs_variant-report = sy-repid.
ENDFORM.                    " BUILD_VARIANT
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
FORM build_layout.
  gs_layout-zebra  = 'X'.       "ZEBRA
  gs_layout-cwidth_opt = 'X'.   "OPTIMIZE COLUMN WIDTH
  gs_layout-detailinit = 'X'.   "DISPLAY INITIAL VALUES ON DETAIL SCREEN
ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM build_fieldcat.
  DATA: l_struct    LIKE dd02l-tabname.

  DATA: zero_fname1(20),
        zero_fname2(20),
        zero_cnt TYPE i.

  l_struct = 'ZTPPPR'.
  CLEAR : wa_fieldcat, gt_fieldcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_buffer_active        = 'X'
            i_structure_name       = l_struct
       CHANGING
            ct_fieldcat            = gt_fieldcat[]
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.

*  DELETE GT_FIELDCAT  WHERE FIELDNAME = 'MANDT' OR
*                            FIELDNAME = 'ZUSER' OR
*                            FIELDNAME = 'ZSDAT' OR
*                            FIELDNAME = 'ZSTIM' OR
*                            FIELDNAME = '' OR
*                            FIELDNAME = ''.

  LOOP AT gt_fieldcat INTO wa_fieldcat.
    PERFORM set_field_info USING wa_fieldcat.
    MODIFY gt_fieldcat FROM wa_fieldcat.
    CLEAR wa_fieldcat.
  ENDLOOP.

ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_INFO
*&---------------------------------------------------------------------*
FORM set_field_info USING l_fieldcat STRUCTURE lvc_s_fcat.

*  CASE L_FIELDCAT-FIELDNAME.
*    WHEN 'FLG'.
*      SET_FIELDCAT  'FLG' 3.
*      L_FIELDCAT-KEY = 'X'.
*    WHEN 'MODL'.
*      SET_FIELDCAT 'MODEL' 3.
*      L_FIELDCAT-KEY = 'X'.
*    WHEN 'VHNO'.
*      SET_FIELDCAT 'BODY NO.' 7.
*      L_FIELDCAT-KEY = 'X'.
*    WHEN 'ORDR'.
*      SET_FIELDCAT 'WORK ORDER' 9.
*    WHEN 'DIST'.
*      SET_FIELDCAT 'DIST.' 5.
*    WHEN 'INTC'.
**      SET_FIELDCAT 'INT'  3.
*    WHEN 'EXTC'.
**      SET_FIELDCAT 'EXT'  3.
*    WHEN 'VINN'.
*      SET_FIELDCAT 'VIN' 17.
*    WHEN 'K01PNO'.
**      SET_FIELDCAT 'Packing No' 8.
*    WHEN 'PLNT'.
**      SET_FIELDCAT 'Plant' 2.
*    WHEN 'LINE'.
**      SET_FIELDCAT 'Line'  2.
*    WHEN 'SSR1'.
*      SET_FIELDCAT 'SEQ' 4.
*    WHEN 'P_EMMISSION'.
*      SET_FIELDCAT 'CAL.' 2.
*    WHEN 'EVL1'.
*      SET_FIELDCAT 'VAL1' 4.
*    WHEN 'EVL2'.
*      SET_FIELDCAT 'VAL2' 4.
*    WHEN 'EVL3'.
*      SET_FIELDCAT 'VAL3' 4.
*    WHEN 'EVL4'.
*      SET_FIELDCAT 'VAL4' 4.
*    WHEN 'EVL5'.
*      SET_FIELDCAT 'VAL5' 4.
**   WHEN 'P_SEQ_DATE'.                " Sequence Date
**     SET_FIELDCAT 'SEQ Date' 10.
*    WHEN 'P_ENGINE_NO'.
*      SET_FIELDCAT 'ENGINE ASSY ID' 15.
*    WHEN 'P_KEY_NO'.
*      SET_FIELDCAT 'KEY' 6.
*    WHEN 'P_TM_NO'.
*      SET_FIELDCAT 'KEY' 15.
*    WHEN 'CDAT'.
*      SET_FIELDCAT 'CREATE DATE' 10.
*    WHEN 'CTIM'.
*      SET_FIELDCAT 'CREATE TIME' 10.
*    WHEN 'ZEDAT'.
*      SET_FIELDCAT 'I/F DATE' 10.
*  ENDCASE.

ENDFORM.                    " SET_FIELD_INFO
*&---------------------------------------------------------------------*
*&      Module  SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_cursor_field OUTPUT.
  SET CURSOR FIELD wa_fname_tx LINE wa_saveline_ix.
ENDMODULE.                 " SET_CURSOR_FIELD  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_cursor_field INPUT.
  CLEAR: wa_fname_tx, wa_saveline_ix.
  GET CURSOR FIELD wa_fname_tx LINE wa_saveline_ix.
ENDMODULE.                 " GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION
*&---------------------------------------------------------------------*
FORM update_classification USING  p_psqty
                                  p_psqty1
                                  p_object.

  DATA : l_object   LIKE  equi-equnr ,
         l_qty(10)   .

  CLEAR: it_vmaster, it_vmaster[].
  WRITE p_psqty1  TO  l_qty   LEFT-JUSTIFIED NO-GROUPING .

  l_object          = p_object.
  it_vmaster-atnam  = p_psqty.
  it_vmaster-atwrt  = l_qty  .
  APPEND it_vmaster.  CLEAR it_vmaster .

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object     = l_object
            ctype      = '001'
            mode       = 'W'
       TABLES
            val_table  = it_vmaster
       EXCEPTIONS
            no_data    = 1
            error_mode = 2
            OTHERS     = 3.

ENDFORM.                    " UPDATE_CLASSIFICATION
*&---------------------------------------------------------------------*
*&      Form  PROCESS_REPORT_POINT
*&---------------------------------------------------------------------*
FORM process_report_point.
*----> Convert Actual Date
  CALL FUNCTION 'Z_FPP_CHANGE_DATE'
       EXPORTING
            iwerks                     = 'P001'
            idate                      = it_ztpppr-rdate
            itime                      = it_ztpppr-rtime
       IMPORTING
            odate                      = wa_datum
       EXCEPTIONS
            factory_calendar_not_found = 1
            holiday_calendar_not_found = 2
            date_has_invalid_format    = 3
            date_inconsistency         = 4
            error_time                 = 5
            OTHERS                     = 6.

  IF sy-subrc NE 0.
    DATA l_msg  LIKE cfgnl-msglin.
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
              msg_lin = l_msg
         EXCEPTIONS
              OTHERS  = 1.

    wa_return-type       = sy-msgty .
    wa_return-id         = sy-msgid .
    wa_return-number     = sy-msgno .
    wa_return-message_v1 = sy-msgv1 .
    wa_return-message_v2 = sy-msgv2 .
    wa_return-message_v3 = sy-msgv3 .
    wa_return-message_v4 = sy-msgv4 .
    wa_return-message    = l_msg    .
  ELSE.
    CASE it_ztpppr-prpid.
      WHEN 'R03'.    "Blank Storage In (AS/RS)
        PERFORM pr_cid_r03.

      WHEN 'R12'.    "Panel Stamping 1 out
        PERFORM pr_cid_r12.

      WHEN 'R13'.    "Panel Stamping 2 out
*        PERFORM PR_CID_R13.
        PERFORM pr_cid_r12.

      WHEN 'R14'.    "Panel Storage out (AS/RS)
        PERFORM pr_cid_r14 USING it_ztpppr-prpid.

      WHEN 'R15'.    "Panel Storage out (SOP)
*        PERFORM PR_CID_R15.
        PERFORM pr_cid_r15 USING it_ztpppr-prpid.
* Begin of changes - UD1K940429
      WHEN 'R16'.
* DO goods issue for Scrap 9 Movement type 551
        PERFORM pr_cid_r16 USING  it_ztpppr-prpid.
* End of changes - UD1K940429
      WHEN 'R23'.    "Body Storage (P230)
        PERFORM pr_cid_r23 USING it_ztpppr-prpid.   " Only Scrap

* UD1K941165 - by IG.MOON 7/31/2007 {
      WHEN 'R04'. " Posting OS&D after 'Blank Rework Scrap'
        PERFORM pr_cid_r04 USING  it_ztpppr-prpid.
* }
    ENDCASE.
  ENDIF.
ENDFORM.                    " PROCESS_REPORT_POINT
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_GR
*&---------------------------------------------------------------------*
FORM message_gr.

*  DATA : l_mlauf      LIKE   mlauf      ,
*         l_mblnr      TYPE   mseg-mblnr .
*
*  CLEAR wa_pprdno .
**----> Alpha Convert of Prod Order No
*  CALL FUNCTION 'CONVERSION_EXIT_AUFNR_INPUT'
*       EXPORTING
*            input  = it_ztpppr-pprdno
*       IMPORTING
*            output = wa_pprdno.
**
**  SELECT SINGLE mblnr
**                INTO l_mblnr
**                FROM mseg
**                WHERE aufnr EQ wa_pprdno
**                  AND werks EQ 'P001'
**                  AND matnr EQ it_ztpppr-pnlno
**                  AND bwart EQ '101'  .
*
*  SELECT *  INTO l_mlauf
*    FROM mlauf  UP TO 1 ROWS
*   WHERE budat = it_ztpppr-rdate
*     AND aufnr = wa_aufnr
*     AND cpudt = sy-datum
*     AND bwart = '101'
*   ORDER BY belnr DESCENDING   .
*  ENDSELECT.
*
*  CONCATENATE 'GR:' l_mlauf-belnr 'is the previous Material Doc-No.'
*         INTO wa_return-message SEPARATED BY ' '.

* by ig.moon 9/24/2009 {

  DATA : l_mlauf      LIKE   mlauf      ,
         l_mblnr      TYPE   mseg-mblnr .

  CLEAR wa_pprdno .
*----> Alpha Convert of Prod Order No
  CALL FUNCTION 'CONVERSION_EXIT_AUFNR_INPUT'
       EXPORTING
            input  = it_ztpppr-pprdno
       IMPORTING
            output = wa_pprdno.

  SELECT mblnr INTO l_mblnr
                FROM mseg UP TO 1 ROWS
                WHERE aufnr EQ wa_pprdno
                  AND bwart EQ '101'
                  AND werks EQ 'P001'
                  AND matnr EQ it_ztpppr-pnlno
                  AND zbudat EQ it_ztpppr-rdate
  ORDER BY mblnr DESCENDING
  %_HINTS ORACLE 'FIRST_ROWS(10) INDEX("MSEG" "MSEG~ZPP")'.
  ENDSELECT.

  CONCATENATE 'GR:' l_mblnr 'is the previous Material Doc-No.'
         INTO it_ztpppr-zmsg SEPARATED BY ' '.

* }


ENDFORM.                    " MESSAGE_GR

*&---------------------------------------------------------------------*
*&      Form  write_return
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_return.
  WRITE AT: /001(050) ' **** IMPORTING RETURN **** ' .

  WRITE AT: /001(004) return-type    ,
             006(030) return-id      ,
             037(006) return-number  ,
             044(156) return-message .
ENDFORM.                    " write_return

*&---------------------------------------------------------------------*
*&      Form  WRITE_DETRET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_detret.
  WRITE AT: /001(050) ' **** DETAIL RETURN **** ' .

  LOOP AT it_detail_return           .
    WRITE AT: /001(004) it_detail_return-type    ,
               006(030) it_detail_return-id      ,
               037(006) it_detail_return-number  ,
               044(156) it_detail_return-message ,
               202(010) it_detail_return-conf_no .
  ENDLOOP.
ENDFORM.                    " WRITE_DETRET
*&---------------------------------------------------------------------*
*&      Form  generate_prodction_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_ZTPPR  text
*----------------------------------------------------------------------*
FORM generate_prodction_order CHANGING p_ztpppr LIKE it_ztpppr.
  DATA: l_gamng LIKE it_ztpppr-pqty,
        l_gamng_char(14),
        l_date_char(10).

  l_gamng = p_ztpppr-pqty + p_ztpppr-prqty + p_ztpppr-psqty.
  WRITE: l_gamng TO l_gamng_char LEFT-JUSTIFIED.
  WRITE: p_ztpppr-rdate TO l_date_char.

  PERFORM bdc_dynpro      USING 'SAPLCOKO1' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'CAUFVD-MATNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'CAUFVD-MATNR'
                                p_ztpppr-pnlno.
  PERFORM bdc_field       USING 'CAUFVD-WERKS'
                                'P001'.
  PERFORM bdc_field       USING 'AUFPAR-PP_AUFART'
                                'PP01'.

  PERFORM bdc_dynpro      USING 'SAPLCOKO1' '0115'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=FREI'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'CAUFVD-GSTRP'.
  PERFORM bdc_field       USING 'CAUFVD-GAMNG'
                                l_gamng_char.
*  PERFORM bdc_field       USING 'CAUFVD-GMEIN'
*                                record-gmein_005.
  PERFORM bdc_field       USING 'CAUFVD-GLTRP'
                                l_date_char.
  PERFORM bdc_field       USING 'CAUFVD-GSTRP'
                                l_date_char.

*  PERFORM bdc_field       USING 'CAUFVD-TERKZ'
*                                record-terkz_008.
*  PERFORM bdc_field       USING 'CAUFVD-FHORI'
*                                record-fhori_009.
*
  PERFORM bdc_dynpro      USING 'SAPLCOKO1' '0115'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'CAUFVD-GAMNG'.

*  PERFORM bdc_field       USING 'CAUFVD-GAMNG'
*                                record-gamng_010.
*  PERFORM bdc_field       USING 'CAUFVD-GLTRP'
*                                record-gltrp_011.
*  PERFORM bdc_field       USING 'CAUFVD-GSTRP'
*                                record-gstrp_012.
*  PERFORM bdc_field       USING 'CAUFVD-TERKZ'
*                                record-terkz_013.
*  PERFORM bdc_field       USING 'CAUFVD-FHORI'
*                                record-fhori_014.
  PERFORM bdc_transaction USING 'CO01' CHANGING p_ztpppr-pprdno.
*  IF p_ztpppr-pprdno IS INITIAL.
*  ELSE.
*    MODIFY p_ztpppr.  "TRANSPORTING pprdno.
*  ENDIF.
ENDFORM.                    " generate_prodction_order
*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2464   text
*----------------------------------------------------------------------*
FORM bdc_transaction USING p_tcode
           CHANGING p_pprdno LIKE it_ztpppr-pprdno.
  DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
          l_aufnr  LIKE afko-aufnr.

  ctumode = 'N'.        "A: show all dynpros
  "E: show dynpro on error only
  "N: do not display dynpro
  REFRESH messtab.
  CALL TRANSACTION p_tcode USING bdcdata
                     MODE   ctumode
                     UPDATE 'L'
                     MESSAGES INTO messtab.
  IF sy-subrc = 0.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              input  = sy-msgv1
         IMPORTING
              output = l_aufnr.
    p_pprdno = l_aufnr.
  ENDIF.
** log the messasge
*    WRITE: / 'CALL_TRANSACTION',
*             tcode,
*             'returncode:'(i05),
*             'RECORD:',
*             sy-index.
*    LOOP AT messtab.
*      SELECT SINGLE * FROM t100 WHERE sprsl = messtab-msgspra
*                                AND   arbgb = messtab-msgid
*                                AND   msgnr = messtab-msgnr.
*      IF sy-subrc = 0.
*        l_mstring = t100-text.
*        IF l_mstring CS '&1'.
*          REPLACE '&1' WITH messtab-msgv1 INTO l_mstring.
*          REPLACE '&2' WITH messtab-msgv2 INTO l_mstring.
*          REPLACE '&3' WITH messtab-msgv3 INTO l_mstring.
*          REPLACE '&4' WITH messtab-msgv4 INTO l_mstring.
*        ELSE.
*          REPLACE '&' WITH messtab-msgv1 INTO l_mstring.
*          REPLACE '&' WITH messtab-msgv2 INTO l_mstring.
*          REPLACE '&' WITH messtab-msgv3 INTO l_mstring.
*          REPLACE '&' WITH messtab-msgv4 INTO l_mstring.
*        ENDIF.
*        CONDENSE l_mstring.
*        WRITE: / messtab-msgtyp, l_mstring(250).
*      ELSE.
*        WRITE: / messtab.
*      ENDIF.
*    ENDLOOP.
**
ENDFORM.                    " bdc_transaction

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

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  pr_cid_r16
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZTPPPR_PRPID  text
*----------------------------------------------------------------------*
FORM pr_cid_r16 USING   p_prpid.

  IF it_ztpppr-psqty  > 0. " Yield
    CLEAR : goodsmvt_header, it_goodsmvt_item,
            it_goodsmvt_item[].
*-----> GOODS MOVEMENT HEADER
*    goodsmvt_header-pstng_date = it_ztpppr-rdate. " Reporting date
*    goodsmvt_header-doc_date =   sy-datum.        " System Date

* UD1K941165 - by IG.MOON { according to Mr. Hong's request
    goodsmvt_header-pstng_date = wa_datum. " Reporting date
    goodsmvt_header-doc_date   =   wa_datum. " System Date
* }

    goodsmvt_header-header_txt = 'Rework Scrap'.  " Header text
*  GM_Code 01: Goods receipt for purchase order
*  GM_Code 02: Goods receipt for production order
*  GM_Code 03: Goods issue
*  GM_Code 04: Transfer posting
*  GM_Code 05: Other goods receipts
*  GM_Code 06: Reversal of goods movements
    goodsmvt_code = '03'.

    CLEAR : ztpp_mip_stk_tra, mkal, marc.

    SELECT SINGLE *
              FROM mkal
              WHERE werks EQ 'P001'
                AND matnr EQ it_ztpppr-pnlno
                AND verid EQ '01'.
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM mara WHERE matnr EQ it_ztpppr-pnlno.
      IF sy-subrc EQ 0.
        MOVE: 'E'      TO wa_return-type,
              text-m01 TO wa_return-message.
      ELSE.
        MOVE: 'E'      TO wa_return-type,
              text-m02 TO wa_return-message.
      ENDIF.

      EXIT.
    ENDIF.

    it_goodsmvt_item-move_type  = '551'.    "Movement Type
    it_goodsmvt_item-stge_loc   = 'P120'.   "TO SLOC
    it_goodsmvt_item-move_reas  = '9900'.   "Reason code

    it_goodsmvt_item-material   = it_ztpppr-pnlno.        "MATERIAL
    it_goodsmvt_item-plant      = mkal-werks.             "PLANT
*    it_goodsmvt_item-stge_loc   = 'P121'.             "FROM SLOC
    it_goodsmvt_item-entry_qnt  = it_ztpppr-psqty.       "QTY
    it_goodsmvt_item-entry_uom  = it_ztpppr-meins.       "UoM
    it_goodsmvt_item-orderid  = it_ztpppr-pprdno.        "UoM
    it_goodsmvt_item-withdrawn = 'Y'.
*  IT_GOODSMVT_ITEM-MOVE_STLOC = ZTPP_MIP_STK_TRA-LGORT. "A/S Stock
    APPEND it_goodsmvt_item.


    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header             = goodsmvt_header
        goodsmvt_code               = goodsmvt_code
      IMPORTING
        materialdocument            = it_ztpppr-mblnr
        matdocumentyear             = it_ztpppr-mjahr
      TABLES
        goodsmvt_item               = it_goodsmvt_item
*      GOODSMVT_SERIALNUMBER       =
        return                      = it_return.

    LOOP AT it_return.
      MOVE-CORRESPONDING it_return TO wa_return.
    ENDLOOP.

*-----Start
*  ELSEIF it_ztpppr-psqty > 0. "scrap
*    PERFORM mb1a_gi_r23 USING p_prpid.
*-----End
  ENDIF.

  IF wa_return-type NE 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    wa_return-type = 'E'.
  ENDIF.


ENDFORM.                    " pr_cid_r16
