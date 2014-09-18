*----------------------------------------------------------------------*
*   INCLUDE ZIPP503L_ENGIN_PR_F                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM read_process.
  CLEAR: it_ztpperm, it_error .

  REFRESH: it_ztpperm, it_error .
** Added by Furong on 01/11/10
  UPDATE ztpperm SET: zresult = 'S'
          zbdat = sy-datum
          zbtim = sy-uzeit
          zbnam = sy-uname
         WHERE ( erpid = 'E91'
           OR  erpid = 'E92'
           OR  erpid = 'E93' )
           AND zresult = 'I'.
  IF sy-subrc = 0.
    COMMIT WORK.
  ENDIF.
** End of change
** Added by Furong on 05/17/10
  UPDATE ztpperm SET: zresult = 'P'
          zbdat = sy-datum
          zbtim = sy-uzeit
          zbnam = sy-uname
        WHERE ( zresult = 'I' OR zresult = 'E' )
          AND eqty = 0
          AND rqty = 0
          AND sqty = 0.
  IF sy-subrc = 0.
    COMMIT WORK.
  ENDIF.
** End of change
  IF r1 EQ 'X'.     "PROCESSING
    SELECT * FROM ztpperm
             APPENDING TABLE it_ztpperm
** changed by Furong on 10/31/2006
*             WHERE ZRESULT EQ 'I'
             WHERE ( zresult EQ 'I'
                  OR zresult EQ 'E'
                  OR zresult EQ 'C' )
** end of change
*               AND ZSLNO   IS NOT NULL
               AND zslno   IN s_zslno
**changed by Furong on 09/10/2005
               AND rdate  IN s_zsdat.
  ELSEIF r2 EQ 'X'.  "REPROCESSING FOR ERROR
    SELECT * FROM ztpperm
             INTO TABLE it_ztpperm
             WHERE zresult EQ 'E'
               AND zslno IN s_zslno
               AND rdate IN s_zsdat.

  ELSEIF r3 EQ 'X'.  "DISPLAY
    SELECT * FROM ztpperm
             INTO TABLE it_ztpperm
*             WHERE ZRESULT EQ 'S'
             WHERE zslno IN s_zslno
               AND rdate IN s_zsdat.
** END OF CHANGE
  ENDIF.
** Changed by Furong on 12/17/09
*  SORT IT_ZTPPERM BY TSEQ ERPID RSEQ.
** Changed by Furong on 04/26/10
*  SORT it_ztpperm BY  rdate tseq erpid.
** Changed by Furong on 04/27/12
  SORT it_ztpperm BY rdate erpid.
** End on 04/27/12
*  SORT IT_ZTPPERM BY ERPID RDATE RSEQ.
** End of change on 04/26/10
** End of change
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM data_process.
  DATA: l_tabix LIKE sy-tabix,
        l_msg LIKE cfgnl-msglin,
        l_tseq LIKE ztpperm-tseq,
        l_erpid LIKE it_ztpperm-erpid..

  LOOP AT it_ztpperm.
** Added by Furong on 01/09/08 for database update delay
*    IF wa_erpid = 'RUN' AND wa_matnr = it_ztpperM-eitem.
*      WAIT UP TO 3 SECONDS .
*    ENDIF.
*    IF WA_MATNR = IT_ZTPPERM-EITEM.
*      IF IT_ZTPPERM-ERPID = 'E01' OR
*         IT_ZTPPERM-ERPID = 'E02' OR
*         IT_ZTPPERM-ERPID = 'E03' OR.
*      ELSE.
*        WAIT UP TO 3 SECONDS.
*      ENDIF.
    IF wa_matnr = it_ztpperm-eitem.
      IF it_ztpperm-erpid = 'E05' AND
         l_erpid = it_ztpperm-erpid..
        WAIT UP TO 3 SECONDS.
      ENDIF.
    ENDIF.
** End of addition
    l_tabix = sy-tabix.

    CLEAR : data_general  , data_specific  ,
            data_generalx , data_specificx ,
            data_install  , return         ,
            wa_datum .

*----> CHECK Material Master
    SELECT SINGLE *
               FROM mara
               WHERE matnr EQ it_ztpperm-eitem .
    IF sy-subrc EQ 0.
*----> Convert Actual Date
** Changed by Furong Wang on 06/19/08
*      IF  IT_ZTPPERM-PROD_DT IS INITIAL.
*        WA_FLAG  = 'E'.
*        WA_MSGID = 'SF'  .
*        WA_MSGTY = 'E'   .
*        WA_MSGNO = '999' .
*        WA_MSGV1 = TEXT-202.  "Posting date is invalid
*      ELSE.
*        WA_DATUM = IT_ZTPPERM-PROD_DT.
**----> PROCESS REPORT POINT
*           CLEAR: WA_ERPID .
*        PERFORM PROCESS_REPORT_POINT  .
*      ENDIF.
** End of change
** Changed on 07/17/08 by Furong: production date

*      CALL FUNCTION 'Z_FPP_CHANGE_DATE'
*           EXPORTING
*                IWERKS                     = 'E001'
*                IDATE                      = IT_ZTPPERM-RDATE
*                ITIME                      = IT_ZTPPERM-RTIME
*           IMPORTING
*                ODATE                      = WA_DATUM
*           EXCEPTIONS
*                FACTORY_CALENDAR_NOT_FOUND = 1
*                HOLIDAY_CALENDAR_NOT_FOUND = 2
*                DATE_HAS_INVALID_FORMAT    = 3
*                DATE_INCONSISTENCY         = 4
*                ERROR_TIME                 = 5
*                OTHERS                     = 6.
*
*      IF SY-SUBRC NE 0.
*        WA_FLAG  = 'E'.
*        WA_MSGID = 'SF'  .
*        WA_MSGTY = 'E'   .
*        WA_MSGNO = '999' .
*        WA_MSGV1 = TEXT-202.  "Posting date is invalid
*      ELSE.
      wa_datum = it_ztpperm-prod_dt.
*----> PROCESS Report Point
      CLEAR: wa_erpid .
      PERFORM process_report_point.
*      ENDIF.

** End of cahnge on 07/14/08
    ELSE.
      wa_flag  = 'E'.
      wa_msgid = 'SF'.
      wa_msgty = 'E'.
      wa_msgno = '999'.
      wa_msgv1 = text-201.   "Material Code is Invalid!!
    ENDIF.

    IF wa_flag EQ space .
      IF it_ztpperm-erpid = 'E51'.

      ELSE.
        IF return-type EQ space . "       AND RETURN-MESSAGE+0(1) EQ 'S'.
** Furong on 04/02/12
*          PERFORM it_ztpperm_modify USING l_msg
          PERFORM it_ztpperm_modify USING return-message
** end on 04/02/12
                                         'S'
                                         l_tabix.
        ELSE.
          PERFORM it_ztpperm_modify USING return-message
                                         return-type
                                         l_tabix.
        ENDIF.
      ENDIF.
    ELSE.
      PERFORM rkc_msg_string1 USING wa_msgid
                                    wa_msgty
                                    wa_msgno
                                    wa_msgv1
                                    wa_msgv2
                                    wa_msgv3
                                    wa_msgv4
                              CHANGING l_msg  .

      IF wa_msgty EQ space.  "AND RETURN-MESSAGE+0(1) EQ 'S'.
        PERFORM it_ztpperm_modify USING l_msg
                                       'S'
                                       l_tabix.
      ELSE.
        PERFORM it_ztpperm_modify USING l_msg
                                       wa_msgty
                                       l_tabix.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING it_ztpperm TO wa_ztpperm.
    IF it_ztpperm-zresult EQ 'E' .
      MOVE-CORRESPONDING it_ztpperm TO it_error .
      APPEND it_error .
    ENDIF.
** Furong on 09/18/12 for 3C not seting 'S'
    IF it_ztpperm-erpid = 'E01' or
       it_ztpperm-erpid = 'E02' or
       it_ztpperm-erpid = 'E03'.
    else.
** end on 09/18/12
      UPDATE ztpperm FROM wa_ztpperm.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      ENDIF.
    endif.

    CLEAR: wa_equnr   ,
           wa_ztpperm ,
           wa_flag    ,
           wa_msgid   ,
           wa_msgty   ,
           wa_msgno   ,
           wa_msgv1   ,
           wa_msgv2   ,
           wa_msgv3   ,
           wa_msgv4   ,
           it_error   ,
** Changed by Furong on 06/04/08
           l_msg.
** End of change
** Added by Furong on 01/09/08 for database update delay
    wa_matnr = it_ztpperm-eitem.
** Added by Furong on 05/10/10
    l_erpid = it_ztpperm-erpid.
**  End of addition on 05/10/10
** End of change
  ENDLOOP.

  PERFORM post_gr_c3.
ENDFORM.                    " DATA_PROCESS

*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
FORM rkc_msg_string CHANGING p_msg.
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
      msg_lin = p_msg
    EXCEPTIONS
      OTHERS  = 1.
ENDFORM.                    " RKC_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING1
*&---------------------------------------------------------------------*
FORM rkc_msg_string1 USING p_msgid
                           p_msgty
                           p_msgno
                           p_msgv1
                           p_msgv2
                           p_msgv3
                           p_msgv4
                     CHANGING p_msg.

  CALL FUNCTION 'RKC_MSG_STRING'
    EXPORTING
      id      = p_msgid
      mtype   = p_msgty
      number  = p_msgno
      par1    = p_msgv1
      par2    = p_msgv2
      par3    = p_msgv3
      par4    = p_msgv4
    IMPORTING
      msg_lin = p_msg
    EXCEPTIONS
      OTHERS  = 1.

ENDFORM.                    " RKC_MSG_STRING1
*&---------------------------------------------------------------------*
*&      Form  IT_ZTPPER_MODIFY
*&---------------------------------------------------------------------*
FORM it_ztpperm_modify USING p_msg
                            p_msgty
                            p_tabix.

  CASE p_msgty.
    WHEN 'S'.
      it_ztpperm-zbdat = sy-datum.  "SAP BDC EXECUTED DATE
      it_ztpperm-zbtim = sy-uzeit.  "SAP BDC EXECUTED TIME
      it_ztpperm-zbnam = sy-uname.  "BDC User ID
*     C: Create U: Update D: Delete
      it_ztpperm-zmode    = 'C'.       "C:CREATE
      it_ztpperm-zresult  = 'S'.       "SUCCESS
      it_ztpperm-zmsg     = p_msg.
      it_ztpperm-zresult  = p_msgty.

    WHEN 'I'.
      it_ztpperm-zbdat = sy-datum.  "SAP BDC EXECUTED DATE
      it_ztpperm-zbtim = sy-uzeit.  "SAP BDC EXECUTED TIME
      it_ztpperm-zbnam = sy-uname.  "BDC User ID
*     C: Create U: Update D: Delete
      it_ztpperm-zmode   = 'C'.       "C:CREATE
      it_ztpperm-zresult = 'S'.       "P_MSGTY.
      it_ztpperm-zmsg    = 'Classification Update successfully!!'.

    WHEN 'E'.
      it_ztpperm-zbdat = sy-datum.  "SAP BDC EXECUTED DATE
      it_ztpperm-zbtim = sy-uzeit.  "SAP BDC EXECUTED TIME
      it_ztpperm-zbnam = sy-uname.  "BDC User ID
      it_ztpperm-zmode = 'C'.       "C:CREATE
*      IT_ZTPPERM-FLAG  = 'E'.       "ERROR
      it_ztpperm-zmsg    = p_msg.
      it_ztpperm-zresult = 'E' .

    WHEN OTHERS.
      it_ztpperm-zbdat = sy-datum.  "SAP BDC EXECUTED DATE
      it_ztpperm-zbtim = sy-uzeit.  "SAP BDC EXECUTED TIME
      it_ztpperm-zbnam = sy-uname.  "BDC User ID
      it_ztpperm-zmode = 'C'.       "C:CREATE
      it_ztpperm-zmsg    = p_msg.
** Changed by Furong on 06/04/08
*      IT_ZTPPERM-ZRESULT = 'S'.
      it_ztpperm-zresult = p_msgty.
** End of change
  ENDCASE.

  MODIFY it_ztpperm INDEX p_tabix TRANSPORTING zbdat
                                              zbtim
                                              zbnam
                                              zmode
                                              zresult
                                              zmsg.

ENDFORM.                    " IT_ZTPPERM_MODIFY
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E00
*&---------------------------------------------------------------------*
FORM en_cid_e00.

*  check assy ID.
  SELECT SINGLE * FROM equi
         WHERE equnr EQ wa_equnr .  "IT_ZTPPERM-EASSYID.
  IF sy-subrc NE 0.
    CLEAR ztpper_mapping .
    SELECT SINGLE * FROM ztpper_mapping.

    external_number         = it_ztpperm-eassyid.      "Equipment Master
* General Tab
    data_general-objecttype = ztpper_mapping-eqart.	"Object Type
    data_general-descript   = ztpper_mapping-shtxt.	"Description

* Location Tab
    data_general-maintplant = ztpper_mapping-swerk.   "Maint Plant
    data_general-maintloc   = ztpper_mapping-stort.   "Maint Location
    data_general-maintroom  = ztpper_mapping-msgrp.	"Room
    data_general-plsectn    = ztpper_mapping-beber.	"Plant Section

*    DATA_GENERAL-PP_WKCTR   = ZTPPER_MAPPING-ARBPL.  "Object ID (W/C)
    SELECT SINGLE objid
           INTO data_general-pp_wkctr
           FROM crhd
           WHERE arbpl EQ ztpper_mapping-arbpl
** Furong 01/27/12
*             AND WERKS EQ 'E001' .  "ZTPPER_MAPPING-WERGW .
             AND werks EQ it_ztpperm-en_reserve_02.
** on 01/27/12

* Organization Tab
    data_general-comp_code  = ztpper_mapping-bukrs.	"Company Code
    data_general-costcenter = ztpper_mapping-kostl.	"Cost Center
    data_general-planplant  = ztpper_mapping-iwerk.	"Planning Plant
    data_general-plangroup  = ztpper_mapping-ingrp.   "Planner Group

*    DATA_GENERAL-WORK_CTR   = ZTPPER_MAPPING-ARBPL.  "Work Center
    SELECT SINGLE objid
           INTO data_general-work_ctr                "Work Center
           FROM crhd
           WHERE arbpl EQ ztpper_mapping-gewrk      "Work Center
** Furong 01/27/12
*             AND WERKS EQ 'E001' .  "ZTPPER_MAPPING-WERGW .
             AND werks EQ it_ztpperm-en_reserve_02.
** on 01/27/12

* Sales and Distribution Tab
    data_general-sales_org  = ztpper_mapping-vkorg.   "Sales Org
    data_general-distr_chan = ztpper_mapping-vtweg.   "Distribution
    data_general-division   = ztpper_mapping-spart.   "Division

* Ser Data Tab
    data_specific-material   = it_ztpperm-eitem.       "ITEM
    data_specific-equicatgry = ztpper_mapping-eqtyp.  "Equi Category 'E'

    CALL FUNCTION 'BAPI_EQUI_CREATE'
      EXPORTING
        external_number   = external_number
        data_general      = data_general
        data_specific     = data_specific
        valid_date        = wa_datum
        data_install      = data_install
      IMPORTING
        equipment         = external_number
        data_general_exp  = data_general
        data_specific_exp = data_specific
        return            = return.

*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*    WAIT UP TO 1 SECONDS.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = return-type
        cl     = return-id
        number = return-number
        par1   = return-message_v1
        par2   = return-message_v2
        par3   = return-message_v3
        par4   = return-message_v4
      IMPORTING
        return = return.

    IF return-type  = 'E'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
*----> Assign class to Engine Master
      PERFORM assign_class USING it_ztpperm-eassyid.
    ENDIF.
  ELSE.
** changed by Furong on 12/16/09
    PERFORM update_classification_e00 .
** end of change
    wa_flag = 'X'.
    wa_msgid = 'SF'.
** changed by Furong on 12/16/09
*    WA_MSGTY = 'E'.
    wa_msgty = 'S'.
** end of change
    wa_msgno = '999'.
    wa_msgv1 = text-203.
  ENDIF.

ENDFORM.                    " EN_CID_E00
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E04
*&---------------------------------------------------------------------*
FORM en_cid_e04.

  external_number           = it_ztpperm-eassyid.    "Equipment number
* General Tab
*  DATA_GENERAL-START_FROM   = IT_ZTPPERM-RDATE.      "Start-up date
*  DATA_GENERALX-START_FROM  = 'X'.
*  DATA_GENERAL-CONSTYEAR    = IT_ZTPPERM-RDATE+0(4). "Year construction
*  DATA_GENERALX-CONSTYEAR   = 'X'.
*  DATA_GENERAL-CONSTMONTH   = IT_ZTPPERM-RDATE+4(2). "Month constr.
*  DATA_GENERALX-CONSTMONTH  = 'X'.
  data_general-start_from   = wa_datum       .      "Start-up date
  data_generalx-start_from  = 'X'.
  data_general-constyear    = wa_datum+0(4)  .      "Year construction
  data_generalx-constyear   = 'X'.
  data_general-constmonth   = wa_datum+4(2)  .      "Month constr.
  data_generalx-constmonth  = 'X'.
* Specific
  data_specific-equicatgry  = 'E'.                  "Equipment category
  data_specificx-equicatgry = 'X'.

  CALL FUNCTION 'BAPI_EQUI_CHANGE'
    EXPORTING
      equipment         = external_number
      data_general      = data_general
      data_generalx     = data_generalx
      data_specific     = data_specific
      data_specificx    = data_specificx
    IMPORTING
      data_general_exp  = data_general
      data_specific_exp = data_specific
      return            = return
    EXCEPTIONS
      OTHERS            = 1.
*  ENDIF.

  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
    EXPORTING
      type   = return-type
      cl     = return-id
      number = return-number
      par1   = return-message_v1
      par2   = return-message_v2
      par3   = return-message_v3
      par4   = return-message_v4
    IMPORTING
      return = return.

  IF return-type  = 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    PERFORM update_classification_e04.
  ENDIF.

ENDFORM.                    " EN_CID_E04
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E05
*&---------------------------------------------------------------------*
FORM en_cid_e05.

** Changed by Furong on 06/12/08
  IF NOT it_ztpperm-eassyid IS INITIAL.
    SELECT SINGLE *
      FROM ztpperm
       WHERE erpid = it_ztpperm-erpid
         AND eassyid = it_ztpperm-eassyid
         AND zresult = 'S'.
    IF sy-subrc = 0.
      PERFORM update_classification_e06 USING 'KD' .
      IF wa_flag IS INITIAL.
        wa_flag  = 'X'.
        wa_msgid = 'SF'  .
        wa_msgty = 'S'   .
        wa_msgno = '999' .
        wa_msgv1 = text-801.  "Duplicated record
      ENDIF.
      EXIT.
    ENDIF.
  ENDIF.
** End of change

*  check assy ID.
  SELECT SINGLE * FROM equi
         WHERE equnr EQ wa_equnr .  "IT_ZTPPERM-EASSYID.
  IF sy-subrc EQ 0 .
*    PERFORM UPDATE_CLASSIFICATION_E06.
    PERFORM en_cid_e01 .
  ELSE.
    CLEAR ztpper_mapping .
    SELECT SINGLE * FROM ztpper_mapping.

    external_number         = it_ztpperm-eassyid.      "Equipment Master
* General Tab
    data_general-objecttype = ztpper_mapping-eqart.	"Object Type
    data_general-descript   = ztpper_mapping-shtxt.	"Description

* Location Tab
    data_general-maintplant = ztpper_mapping-swerk.   "Maint Plant
    data_general-maintloc   = ztpper_mapping-stort.   "Maint Location
    data_general-maintroom  = ztpper_mapping-msgrp.	"Room
    data_general-plsectn    = ztpper_mapping-beber.	"Plant Section

*    DATA_GENERAL-PP_WKCTR   = ZTPPER_MAPPING-ARBPL.  "Object ID (W/C)
    SELECT SINGLE objid
           INTO data_general-pp_wkctr
           FROM crhd
           WHERE arbpl EQ ztpper_mapping-arbpl
** Furong 01/27/12
*             AND WERKS EQ 'E001' .  "ZTPPER_MAPPING-WERGW .
             AND werks EQ it_ztpperm-en_reserve_02.
** on 01/27/12

* Organization Tab
    data_general-comp_code  = ztpper_mapping-bukrs.	"Company Code
    data_general-costcenter = ztpper_mapping-kostl.	"Cost Center
    data_general-planplant  = ztpper_mapping-iwerk.	"Planning Plant
    data_general-plangroup  = ztpper_mapping-ingrp.   "Planner Group

*    DATA_GENERAL-WORK_CTR   = ZTPPER_MAPPING-ARBPL.  "Work Center
    SELECT SINGLE objid
           INTO data_general-work_ctr                "Work Center
           FROM crhd
           WHERE arbpl EQ ztpper_mapping-gewrk       "Work Center
** Furong 01/27/12
*             AND WERKS EQ 'E001' .  "ZTPPER_MAPPING-WERGW .
             AND werks EQ it_ztpperm-en_reserve_02.
** on 01/27/12

* Sales and Distribution Tab
    data_general-sales_org  = ztpper_mapping-vkorg.   "Sales Org
    data_general-distr_chan = ztpper_mapping-vtweg.   "Distribution
    data_general-division   = ztpper_mapping-spart.   "Division

* Ser Data Tab
    data_specific-material   = it_ztpperm-eitem.       "ITEM
    data_specific-equicatgry = ztpper_mapping-eqtyp.  "Equi Category 'E'

    CALL FUNCTION 'BAPI_EQUI_CREATE'
      EXPORTING
        external_number   = external_number
        data_general      = data_general
        data_specific     = data_specific
        valid_date        = wa_datum
        data_install      = data_install
      IMPORTING
        equipment         = external_number
        data_general_exp  = data_general
        data_specific_exp = data_specific
        return            = return.

*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*    WAIT UP TO 1 SECONDS.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = return-type
        cl     = return-id
        number = return-number
        par1   = return-message_v1
        par2   = return-message_v2
        par3   = return-message_v3
        par4   = return-message_v4
      IMPORTING
        return = return.

    IF return-type  = 'E'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
*----> Assign class to Engine Master
      PERFORM assign_class USING it_ztpperm-eassyid.
** Furong on 08/31/12 for backflush and GR
      PERFORM en_cid_e01 .
** End on 08/31/12
    ENDIF.
  ENDIF.

ENDFORM.                    " EN_CID_E05
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E06
*&---------------------------------------------------------------------*
FORM en_cid_e06 USING p_cid .
*---> KD Engine Assy : P_CID = 'KD'
*---> not KD Engine Assy : P_CID = SPACE
  PERFORM update_classification_e06 USING p_cid .
ENDFORM.                    " EN_CID_E06

*&---------------------------------------------------------------------*
*&      Form  EN_CID_E07
*&---------------------------------------------------------------------*
FORM en_cid_e07.
  DATA: goodsmvt_header   LIKE  bapi2017_gm_head_01,
        goodsmvt_code     LIKE  bapi2017_gm_code,
        it_goodsmvt_item
        LIKE TABLE OF bapi2017_gm_item_create WITH HEADER LINE,
        it_return  LIKE TABLE OF bapiret2 WITH HEADER LINE.
  DATA: l_mblnr    TYPE  bapi2017_gm_head_ret-mat_doc,
        l_mjahr    TYPE  bapi2017_gm_head_ret-doc_year.

  IF it_ztpperm-eusage NE space.   "A/S Stock Transfer
    CLEAR : ztpp_mip_stk_tra.
    SELECT SINGLE *
           FROM ztpp_mip_stk_tra
** Furong 01/27/12
*            where WERKS EQ 'E001' .
             WHERE werks EQ it_ztpperm-en_reserve_02
** on 01/27/12
             AND eusage EQ it_ztpperm-eusage.

    IF sy-subrc EQ 0.
      goodsmvt_header-pstng_date = wa_datum.
      goodsmvt_header-doc_date   = it_ztpperm-rdate.
      goodsmvt_header-header_txt = it_ztpperm-rtime.
*  GM_Code 01: Goods receipt for purchase order
*  GM_Code 02: Goods receipt for production order
*  GM_Code 03: Goods issue
*  GM_Code 04: Transfer posting
*  GM_Code 05: Other goods receipts
*  GM_Code 06: Reversal of goods movements
      goodsmvt_code = '04'.
      it_goodsmvt_item-material  = it_ztpperm-eitem.   "MATERIAL
      it_goodsmvt_item-plant     = ztpp_mip_stk_tra-werks. "Engine PLANT
      CLEAR : marc.
      SELECT SINGLE * FROM marc
             WHERE matnr EQ it_ztpperm-eitem          "ENGINE MATERIAL
               AND werks EQ ztpp_mip_stk_tra-werks.  "ENGINE PLANT
      it_goodsmvt_item-stge_loc   = marc-lgpro.      "FROM
      it_goodsmvt_item-move_type  = '311'. "MVT type
      it_goodsmvt_item-entry_qnt  = it_ztpperm-eqty.
      it_goodsmvt_item-entry_uom  = it_ztpperm-meins.

      it_goodsmvt_item-move_stloc = ztpp_mip_stk_tra-lgort. "A/S Stock
      APPEND it_goodsmvt_item.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header       = goodsmvt_header
          goodsmvt_code         = goodsmvt_code
        IMPORTING
          materialdocument      = l_mblnr
          matdocumentyear       = l_mjahr
        TABLES
          goodsmvt_item         = it_goodsmvt_item
*         GOODSMVT_SERIALNUMBER =
          return                = it_return.

      READ TABLE it_return INDEX 1.
      MOVE-CORRESPONDING it_return TO return.
    ENDIF.
  ENDIF.

*-----> Change Engine Master
  CLEAR ztpper_mapping .
  SELECT SINGLE * FROM ztpper_mapping.

  external_number          = it_ztpperm-eassyid.    "Equipment number
  data_general-constyear   = wa_datum+0(4) .   "IT_ZTPPERM-RDATE+0(4).
  data_generalx-constyear  = 'X'.
  data_general-constmonth  = wa_datum+4(2) .   "IT_ZTPPERM-RDATE+4(2).
  data_generalx-constmonth = 'X'.
  data_general-sales_org  = ztpper_mapping-vkorg.
  data_generalx-sales_org  = 'X' .
  data_general-distr_chan = ztpper_mapping-vtweg.
  data_generalx-distr_chan = 'X' .
  data_general-division   = ztpper_mapping-spart.
  data_generalx-division   = 'X' .

  IF it_ztpperm-eusage EQ '01'.
    data_specific-serialno  = it_ztpperm-eassyid.
    data_specificx-serialno = 'X'.
  ENDIF.
  data_specific-equicatgry = 'E'.
  data_specificx-equicatgry = 'X'.

  CALL FUNCTION 'BAPI_EQUI_CHANGE'
    EXPORTING
      equipment         = external_number
      data_general      = data_general
      data_generalx     = data_generalx
      data_specific     = data_specific
      data_specificx    = data_specificx
    IMPORTING
      data_general_exp  = data_general
      data_specific_exp = data_specific
      return            = return
    EXCEPTIONS
      OTHERS            = 1.

  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
    EXPORTING
      type   = return-type
      cl     = return-id
      number = return-number
      par1   = return-message_v1
      par2   = return-message_v2
      par3   = return-message_v3
      par4   = return-message_v4
    IMPORTING
      return = return.

  IF return-type  = 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    PERFORM update_classification_e07 .
  ENDIF.
ENDFORM.                    " EN_CID_E07

*&---------------------------------------------------------------------*
*&      Form  EN_CID_T04
*&---------------------------------------------------------------------*
FORM en_cid_t04.
*  EXTERNAL_NUMBER          = IT_ZTPPERM-EASSYID.    "Equipment number
*    DATA_GENERAL-SALES_ORG  = ZTPPER_MAPPING-VKORG.
*    DATA_GENERALX-SALES_ORG  = 'X' .
*    DATA_GENERAL-DISTR_CHAN = ZTPPER_MAPPING-VTWEG.
*    DATA_GENERALX-DISTR_CHAN = 'X' .
*    DATA_GENERAL-DIVISION   = ZTPPER_MAPPING-SPART.
*    DATA_GENERALX-DIVISION   = 'X' .

**  IF IT_ZTPPERM-EUSAGE EQ '01'.
**    DATA_SPECIFIC-SERIALNO  = IT_ZTPPERM-EASSYID.
**    DATA_SPECIFICX-SERIALNO = 'X'.
**  ENDIF.
*  DATA_SPECIFIC-EQUICATGRY = 'E'.
*  DATA_SPECIFICX-EQUICATGRY = 'X'.
*
*  CALL FUNCTION 'BAPI_EQUI_CHANGE'
*       EXPORTING
*            EQUIPMENT         = EXTERNAL_NUMBER
*            DATA_GENERAL      = DATA_GENERAL
*            DATA_GENERALX     = DATA_GENERALX
*            DATA_SPECIFIC     = DATA_SPECIFIC
*            DATA_SPECIFICX    = DATA_SPECIFICX
*       IMPORTING
*            DATA_GENERAL_EXP  = DATA_GENERAL
*            DATA_SPECIFIC_EXP = DATA_SPECIFIC
*            RETURN            = RETURN
*       EXCEPTIONS
*            OTHERS            = 1.
*
*  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
*       EXPORTING
*            TYPE   = RETURN-TYPE
*            CL     = RETURN-ID
*            NUMBER = RETURN-NUMBER
*            PAR1   = RETURN-MESSAGE_V1
*            PAR2   = RETURN-MESSAGE_V2
*            PAR3   = RETURN-MESSAGE_V3
*            PAR4   = RETURN-MESSAGE_V4
*       IMPORTING
*            RETURN = RETURN.
  SELECT SINGLE * FROM mara
         WHERE matnr EQ it_ztpperm-eitem.
  IF sy-subrc EQ 0.
    PERFORM update_classification_t04 .
  ELSE.
    wa_flag = 'X'.
    wa_msgid = 'SF'.
    wa_msgty = 'E'.
    wa_msgno = '999'.
    wa_msgv1 = 'Material Code is Invalid!!'.
  ENDIF.
ENDFORM.                    " EN_CID_T04

*&---------------------------------------------------------------------*
*&      Form  EN_CID_E98
*&---------------------------------------------------------------------*
FORM en_cid_e98.
*  SELECT SINGLE *
*    FROM MARA
*   WHERE MATNR EQ IT_ZTPPERM-EITEM.
*
*  IF SY-SUBRC EQ 0.
*    PERFORM UPDATE_CLASSIFICATION_T04 .
*  ELSE.
*    WA_FLAG = 'X'.
*    WA_MSGID = 'SF'.
*    WA_MSGTY = 'E'.
*    WA_MSGNO = '999'.
*    WA_MSGV1 = 'Material Code is Invalid!!'.
*  ENDIF.

  DATA: goodsmvt_header   LIKE  bapi2017_gm_head_01,
         goodsmvt_code     LIKE  bapi2017_gm_code,
         it_goodsmvt_item
         LIKE TABLE OF bapi2017_gm_item_create WITH HEADER LINE,
         it_return  LIKE TABLE OF bapiret2 WITH HEADER LINE.
  DATA: l_mblnr    TYPE  bapi2017_gm_head_ret-mat_doc,
        l_mjahr    TYPE  bapi2017_gm_head_ret-doc_year.
  DATA: l_cnt LIKE it_vmaster-atwrt.
*  IF IT_ZTPPERM-EUSAGE NE SPACE.   "A/S Stock Transfer
*  CLEAR : ZTPP_MIP_STK_TRA.
*  SELECT SINGLE MATNR INTO W_OITEM
*      FROM EQUI
*      WHERE EQUNR = IT_ZTPPERM-EASSYID.

  CLEAR: it_vmaster,  it_vmaster[].

  wa_equnr = it_ztpperm-eassyid.
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = wa_equnr
      mode         = 'R'
      display      = 'X'
    TABLES
      val_table    = it_vmaster
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  READ TABLE it_vmaster WITH KEY atnam = 'EN_ITEM_CODE'.
  IF sy-subrc = 0.
    w_oitem = it_vmaster-atwrt.
    READ TABLE it_vmaster WITH KEY atnam = 'EN_CHG_CNT'.
    IF sy-subrc = 0.
      l_cnt = it_vmaster-atwrt.
    ENDIF.
    READ TABLE it_vmaster WITH KEY atnam = 'EN_CID'.
    IF sy-subrc = 0 AND
         ( it_vmaster-atwrt = 'E05' OR it_vmaster-atwrt = 'E09' ).
      IF it_vmaster-atwrt = 'E05'.
        IF  it_ztpperm-eitem+0(2) = 'AU'.
          it_goodsmvt_item-stge_loc   = 'E110'.      "FROM
          it_goodsmvt_item-move_stloc = 'E110'.      "TO
        ELSE.
          it_goodsmvt_item-stge_loc   = 'E210'.      "FROM
          it_goodsmvt_item-move_stloc = 'E210'.      "TO
        ENDIF.
      ELSE.
        it_goodsmvt_item-stge_loc   = 'E301'.      "FROM
        it_goodsmvt_item-move_stloc = 'E301'.      "TO
      ENDIF.
      goodsmvt_header-pstng_date = it_ztpperm-rdate.
      goodsmvt_header-doc_date   = it_ztpperm-rdate.
      goodsmvt_header-header_txt = it_ztpperm-eassyid.
      goodsmvt_code = '04'.

      it_goodsmvt_item-material = w_oitem.
      it_goodsmvt_item-move_mat = it_ztpperm-eitem.
*      IT_GOODSMVT_ITEM-MATERIAL  = IT_ZTPPERM-EITEM.   "MATERIAL
*      IT_GOODSMVT_ITEM-MOVE_MAT = W_OITEM.

** Furong 01/27/12
*      it_goodsmvt_item-plant     = 'E001'. "Engine PLANT
*      it_goodsmvt_item-move_plant = 'E001'.
      it_goodsmvt_item-plant = it_ztpperm-en_reserve_02.
      it_goodsmvt_item-move_plant = it_ztpperm-en_reserve_02.
** on 01/27/12

*      CLEAR : MARC.
*      SELECT SINGLE * FROM MARC
*             WHERE MATNR EQ IT_ZTPPERM-EITEM          "ENGINE MATERIAL
*               AND WERKS EQ 'E001'.  "ENGINE PLANT
*      IT_GOODSMVT_ITEM-STGE_LOC   = MARC-LGPRO.      "FROM
*      IT_GOODSMVT_ITEM-MOVE_STLOC = MARC-LGPRO.      "TO

      it_goodsmvt_item-move_type  = '309'. "MVT type
      it_goodsmvt_item-entry_qnt  = it_ztpperm-eqty.
      it_goodsmvt_item-entry_uom  = it_ztpperm-meins.

      APPEND it_goodsmvt_item.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header       = goodsmvt_header
          goodsmvt_code         = goodsmvt_code
        IMPORTING
          materialdocument      = l_mblnr
          matdocumentyear       = l_mjahr
        TABLES
          goodsmvt_item         = it_goodsmvt_item
*         GOODSMVT_SERIALNUMBER =
          return                = it_return.


      IF sy-subrc = 0 AND NOT l_mblnr IS INITIAL.

        CLEAR : return-message, return-type .
        MOVE 'S'      TO   return-type .
        CONCATENATE 'TR:' l_mblnr INTO return-message
                                 SEPARATED BY space .

        l_cnt = l_cnt + 1.
        CLEAR : it_vmaster,  it_vmaster[] .
        PERFORM append_vmaster USING 'EN_OITEM_CODE'  w_oitem.
        PERFORM append_vmaster USING 'EN_ITEM_CODE' it_ztpperm-eitem  .
        PERFORM append_vmaster USING 'EN_ODATE'   it_ztpperm-rdate  .
        PERFORM append_vmaster USING 'EN_OTIME'   it_ztpperm-rtime  .
        PERFORM append_vmaster USING 'EN_OSEQ'    it_ztpperm-rseq .
        PERFORM append_vmaster USING 'EN_CHG_CNT' l_cnt .
        PERFORM append_vmaster USING 'EN_MDOC_SPECCHANGE'  l_mblnr.

        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object     = wa_equnr
            mode       = 'W'
            ctype      = '002'
          TABLES
            val_table  = it_vmaster
          EXCEPTIONS
            no_data    = 1
            error_mode = 2
            OTHERS     = 3.
        IF sy-subrc NE 0.
          wa_flag  = 'X' .
          wa_msgid = sy-msgid .
          wa_msgty = sy-msgty .
          wa_msgno = sy-msgno .
          wa_msgv1 = sy-msgv1 .
        ENDIF.
      ENDIF.
    ELSE.
      READ TABLE it_return INDEX 1.
      MOVE-CORRESPONDING it_return TO return.
*  PERFORM UPDATE_CLASSIFICATION_E98A.
    ENDIF.
  ELSE.
    CLEAR : return-message, return-type .
    MOVE 'E'      TO   return-type .
    return-message = 'Invalid current RP for SPEC changes'.
  ENDIF.
ENDFORM.                    " EN_CID_E98

*&---------------------------------------------------------------------*
*&      Form  EN_CID_E99
*&---------------------------------------------------------------------*
FORM en_cid_e99.
  DATA: l_encid  LIKE zspp_vin_value-atwrt,
        l_matnr  LIKE zspp_vin_value-atwrt.

  CLEAR: it_vmaster,  it_vmaster[].

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = wa_equnr
      mode         = 'R'
      display      = 'X'
    TABLES
      val_table    = it_vmaster
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  READ TABLE it_vmaster WITH KEY atnam = 'EN_CID'.
  IF sy-subrc = 0.
    l_encid = it_vmaster-atwrt.
  ELSE.
    " Error...
    wa_flag  = 'X' .
    EXIT.
  ENDIF.

  SELECT SINGLE matnr
    FROM equi
    INTO l_matnr
   WHERE equnr EQ wa_equnr.

  IF sy-subrc NE 0.   wa_flag  = 'X' .   EXIT.   ENDIF.

  CLEAR: it_vals, it_vals[], it_vmaster.
  CASE l_encid .
    WHEN 'E00' .
      READ TABLE it_vmaster WITH KEY atnam =  'EN_ORD_SEQ'.
      IF sy-subrc = 0.
        it_vals-atnam = 'EN_OSEQ'.     it_vals-atwrt = it_vmaster-atwrt.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
        it_vals-atnam = 'EN_ORD_SEQ'.  it_vals-atwrt = it_ztpperm-rseq.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
      ENDIF.
      READ TABLE it_vmaster WITH KEY atnam =  'EN_ORD_DATE'.
      IF sy-subrc = 0.
        it_vals-atnam = 'EN_ODATE'.    it_vals-atwrt = it_vmaster-atwrt.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
        it_vals-atnam = 'EN_ORD_DATE'. it_vals-atwrt = it_ztpperm-rdate.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
      ENDIF.
      READ TABLE it_vmaster WITH KEY atnam =  'EN_ORD_TIME'.
      IF sy-subrc = 0.
        it_vals-atnam = 'EN_OTIME'.    it_vals-atwrt = it_vmaster-atwrt.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
        it_vals-atnam = 'EN_ORD_TIME'. it_vals-atwrt = it_ztpperm-rtime.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
      ENDIF.
    WHEN 'E04' .
      READ TABLE it_vmaster WITH KEY atnam =  'EN_IN_SEQ'.
      IF sy-subrc = 0.
        it_vals-atnam = 'EN_OSEQ'.     it_vals-atwrt = it_vmaster-atwrt.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
        it_vals-atnam = 'EN_IN_SEQ'.   it_vals-atwrt = it_ztpperm-rseq.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
      ENDIF.
      READ TABLE it_vmaster WITH KEY atnam =  'EN_IN_DATE'.
      IF sy-subrc = 0.
        it_vals-atnam = 'EN_ODATE'.    it_vals-atwrt = it_vmaster-atwrt.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
        it_vals-atnam = 'EN_IN_DATE'.  it_vals-atwrt = it_ztpperm-rdate.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
      ENDIF.
      READ TABLE it_vmaster WITH KEY atnam =  'EN_IN_TIME'.
      IF sy-subrc = 0.
        it_vals-atnam = 'EN_OTIME'.    it_vals-atwrt = it_vmaster-atwrt.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
        it_vals-atnam = 'EN_IN_TIME'.  it_vals-atwrt = it_ztpperm-rtime.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
      ENDIF.
    WHEN 'E05' .
      READ TABLE it_vmaster WITH KEY atnam =  'EN_COMPL_SEQ'.
      IF sy-subrc = 0.
        it_vals-atnam = 'EN_OSEQ'.     it_vals-atwrt = it_vmaster-atwrt.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
        it_vals-atnam = 'EN_COMPL_SEQ'.
        it_vals-atwrt = it_ztpperm-rseq.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
      ENDIF.
      READ TABLE it_vmaster WITH KEY atnam =  'EN_COMPL_DATE'.
      IF sy-subrc = 0.
        it_vals-atnam = 'EN_ODATE'.    it_vals-atwrt = it_vmaster-atwrt.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
        it_vals-atnam = 'EN_COMPL_DATE'.
        it_vals-atwrt = it_ztpperm-rdate.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
      ENDIF.
      READ TABLE it_vmaster WITH KEY atnam =  'EN_COMPL_TIME'.
      IF sy-subrc = 0.
        it_vals-atnam = 'EN_OTIME'.    it_vals-atwrt = it_vmaster-atwrt.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
        it_vals-atnam = 'EN_COMPL_TIME'.
        it_vals-atwrt = it_ztpperm-rtime.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
      ENDIF.
    WHEN 'E07' .
      READ TABLE it_vmaster WITH KEY atnam =  'EN_OUT_SEQ'.
      IF sy-subrc = 0.
        it_vals-atnam = 'EN_OSEQ'.     it_vals-atwrt = it_vmaster-atwrt.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
        it_vals-atnam = 'EN_OUT_SEQ'. it_vals-atwrt = it_ztpperm-rseq.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
      ENDIF.
      READ TABLE it_vmaster WITH KEY atnam =  'EN_OUT_DATE'.
      IF sy-subrc = 0.
        it_vals-atnam = 'EN_ODATE'.    it_vals-atwrt = it_vmaster-atwrt.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
        it_vals-atnam = 'EN_OUT_DATE'. it_vals-atwrt = it_ztpperm-rdate.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
      ENDIF.
      READ TABLE it_vmaster WITH KEY atnam =  'EN_OUT_TIME'.
      IF sy-subrc = 0.
        it_vals-atnam = 'EN_OTIME'.    it_vals-atwrt = it_vmaster-atwrt.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
        it_vals-atnam = 'EN_OUT_TIME'. it_vals-atwrt = it_ztpperm-rtime.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
      ENDIF.
    WHEN 'T04' .
      READ TABLE it_vmaster WITH KEY atnam =  'EN_MOUNT_SEQ'.
      IF sy-subrc = 0.
        it_vals-atnam = 'EN_OSEQ'.     it_vals-atwrt = it_vmaster-atwrt.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
        it_vals-atnam = 'EN_MOUNT_SEQ'.
        it_vals-atwrt = it_ztpperm-rseq.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
      ENDIF.
      READ TABLE it_vmaster WITH KEY atnam =  'EN_MOUNT_DATE'.
      IF sy-subrc = 0.
        it_vals-atnam = 'EN_ODATE'.    it_vals-atwrt = it_vmaster-atwrt.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
        it_vals-atnam = 'EN_MOUNT_DATE'.
        it_vals-atwrt = it_ztpperm-rdate.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
      ENDIF.
      READ TABLE it_vmaster WITH KEY atnam =  'EN_MOUNT_TIME'.
      IF sy-subrc = 0.
        it_vals-atnam = 'EN_OTIME'.    it_vals-atwrt = it_vmaster-atwrt.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
        it_vals-atnam = 'EN_MOUNT_TIME'.
        it_vals-atwrt = it_ztpperm-rtime.
        APPEND it_vals.                CLEAR: it_vals, it_vmaster.
      ENDIF.
  ENDCASE.

  READ TABLE it_vmaster WITH KEY atnam =  'EN_ITEM_CODE'.
  IF sy-subrc = 0.
    it_vals-atnam = 'EN_OITEM_CODE'.   it_vals-atwrt = it_vmaster-atwrt.
    APPEND it_vals.                    CLEAR: it_vals, it_vmaster.
    it_vals-atnam = 'EN_ITEM_CODE'.    it_vals-atwrt = it_ztpperm-eitem.
    APPEND it_vals.                    CLEAR: it_vals, it_vmaster.
  ENDIF.

  READ TABLE it_vmaster WITH KEY atnam =  'EN_CHG_CNT'.
  IF sy-subrc = 0.
    it_vmaster-atwrt = it_vmaster-atwrt + 1 .
    it_vals-atnam = 'EN_CHG_CNT'.    it_vals-atwrt = it_vmaster-atwrt.
    APPEND it_vals.                  CLEAR: it_vals, it_vmaster.
  ELSE.
    it_vals-atnam = 'EN_CHG_CNT'.    it_vals-atwrt = 1 .
    APPEND it_vals.                  CLEAR: it_vals, it_vmaster.
  ENDIF.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = wa_equnr
      mode         = 'W'
    TABLES
      val_table    = it_vals
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  IF sy-subrc <> 0.
    wa_flag  = 'X' .
  ENDIF.
ENDFORM.                    " EN_CID_E99

*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E00
*&---------------------------------------------------------------------*
FORM update_classification_e00.
  DATA: l_cnt LIKE zspp_vin_value-atwrt.
  l_cnt = '0' .
  CLEAR : it_vmaster,  it_vmaster[].
** Changed by Furong on 12/01/09
  IF wa_old_cid < it_ztpperm-erpid.
    PERFORM append_vmaster USING 'EN_CID'        it_ztpperm-erpid  .
  ENDIF.
** End of change on 12/01/09
  PERFORM append_vmaster USING 'EN_ORD_DATE'   it_ztpperm-rdate  .
*  PERFORM APPEND_VMASTER USING 'EN_ORD_DATE'   WA_DATUM         .
  PERFORM append_vmaster USING 'EN_ORD_TIME'   it_ztpperm-rtime  .
  PERFORM append_vmaster USING 'EN_ORD_SEQ'    it_ztpperm-rseq   .
  PERFORM append_vmaster USING 'EN_CHG_CNT'    l_cnt            .
  PERFORM append_vmaster USING 'EN_PLD_NO'     it_ztpperm-epono  .
  PERFORM append_vmaster USING 'EN_ITEM_CODE'  it_ztpperm-eitem  .
** furong on 02/06/12
  PERFORM append_vmaster USING 'EN_RESERVE_02'  it_ztpperm-EN_RESERVE_02 .
** end on 02/06/12
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object     = wa_equnr
      mode       = 'W'
      ctype      = '002'
    TABLES
      val_table  = it_vmaster
    EXCEPTIONS
      no_data    = 1
      error_mode = 2
      OTHERS     = 3.
  IF sy-subrc NE 0.
    wa_flag  = 'X' .
    wa_msgid = sy-msgid .
    wa_msgty = sy-msgty .
    wa_msgno = sy-msgno .
    wa_msgv1 = sy-msgv1 .
  ENDIF.
ENDFORM.                    " UPDATE_CLASSIFICATION_E00


*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E04
*&---------------------------------------------------------------------*
FORM update_classification_e04 .

  CLEAR: it_vmaster,
         it_vmaster[].
** Changed by Furong on 12/01/09
  IF wa_old_cid < it_ztpperm-erpid.
    PERFORM append_vmaster USING 'EN_CID'        it_ztpperm-erpid  .
  ENDIF.
** End of change
  PERFORM append_vmaster USING 'EN_IN_DATE'    it_ztpperm-rdate  .
*  PERFORM APPEND_VMASTER USING 'EN_IN_DATE'    WA_DATUM         .
  PERFORM append_vmaster USING 'EN_IN_TIME'    it_ztpperm-rtime  .
  PERFORM append_vmaster USING 'EN_IN_SEQ'     it_ztpperm-rseq   .
  PERFORM append_vmaster USING 'EN_RESERVE_02'
     it_ztpperm-EN_RESERVE_02.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object     = wa_equnr
      mode       = 'W'
      cmode      = '002'
    TABLES
      val_table  = it_vmaster
    EXCEPTIONS
      no_data    = 1
      error_mode = 2
      OTHERS     = 3.
  IF sy-subrc NE 0.
    wa_flag  = 'X' .
    wa_msgid = sy-msgid .
    wa_msgty = sy-msgty .
    wa_msgno = sy-msgno .
    wa_msgv1 = sy-msgv1 .
  ENDIF.
ENDFORM.                    " UPDATE_CLASSIFICATION_E04

*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E06
*&---------------------------------------------------------------------*
FORM update_classification_e06 USING p_cid .
*---> KD Engine Assy : P_CID = 'KD'
*---> not KD Engine Assy : P_CID = SPACE
  CLEAR: it_vmaster,
         it_vmaster[].
** Changed by Furong on 12/01/09
  IF wa_old_cid < it_ztpperm-erpid.
    PERFORM append_vmaster USING 'EN_CID'        it_ztpperm-erpid  .
  ENDIF.
** End of change
  PERFORM append_vmaster USING 'EN_ITEM_CODE'  it_ztpperm-eitem  .
  PERFORM append_vmaster USING 'EN_COMPL_DATE' it_ztpperm-rdate  .
*  PERFORM APPEND_VMASTER USING 'EN_COMPL_DATE' WA_DATUM         .
  PERFORM append_vmaster USING 'EN_COMPL_TIME' it_ztpperm-rtime  .
  PERFORM append_vmaster USING 'EN_COMPL_SEQ'  it_ztpperm-rseq   .
  PERFORM more_new_char.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object     = wa_equnr
      mode       = 'W'
      cmode      = '002'
    TABLES
      val_table  = it_vmaster
    EXCEPTIONS
      no_data    = 1
      error_mode = 2
      OTHERS     = 3.
  IF sy-subrc NE 0.
    wa_flag  = 'X' .
    wa_msgty = 'E'.
    wa_msgid = sy-msgid .
    wa_msgty = sy-msgty .
    wa_msgno = sy-msgno .
    wa_msgv1 = sy-msgv1 .
  ELSE.
    IF p_cid NE 'KD' .
      PERFORM select_mat_doc .
    ENDIF.
  ENDIF.
ENDFORM.                    " UPDATE_CLASSIFICATION_E06

*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E07
*&---------------------------------------------------------------------*
FORM update_classification_e07.

  CLEAR: it_vmaster,
         it_vmaster[].
** Changed by Furong on 12/01/09
  IF wa_old_cid < it_ztpperm-erpid.
    PERFORM append_vmaster USING 'EN_CID'        it_ztpperm-erpid  .
  ENDIF.
** End of change
  PERFORM append_vmaster USING 'EN_OUT_DATE'   it_ztpperm-rdate  .
*  PERFORM APPEND_VMASTER USING 'EN_OUT_DATE'   WA_DATUM         .
  PERFORM append_vmaster USING 'EN_OUT_TIME'   it_ztpperm-rtime  .
  PERFORM append_vmaster USING 'EN_OUT_SEQ'    it_ztpperm-rseq   .
  PERFORM append_vmaster USING 'EN_RESERVE_02'
          it_ztpperm-EN_RESERVE_02.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object     = wa_equnr
      mode       = 'W'
      cmode      = '002'
    TABLES
      val_table  = it_vmaster
    EXCEPTIONS
      no_data    = 1
      error_mode = 2
      OTHERS     = 3.
  IF sy-subrc NE 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    wa_flag  = 'X' .
    wa_msgid = sy-msgid .
    wa_msgty = sy-msgty .
    wa_msgno = sy-msgno .
    wa_msgv1 = sy-msgv1 .
  ENDIF.
ENDFORM.                    " UPDATE_CLASSIFICATION_E07

*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_T04
*&---------------------------------------------------------------------*
FORM update_classification_t04.
  CLEAR: it_vmaster,  it_vmaster[].
** Changed by Furong on 12/01/09
  IF wa_old_cid < it_ztpperm-erpid.
    PERFORM append_vmaster USING 'EN_CID'          it_ztpperm-erpid  .
  ENDIF.
** End of change
  PERFORM append_vmaster USING 'EN_COMPL_DATE'   it_ztpperm-rdate  .
  PERFORM append_vmaster USING 'EN_COMPL_TIME'   it_ztpperm-rtime  .
* PERFORM APPEND_VMASTER USING 'EN_MOUNT_SEQ'    IT_ZTPPERM-RSEQ   .

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = wa_equnr
      mode         = 'W'
      cmode        = '002'
    TABLES
      val_table    = it_vmaster
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  IF sy-subrc NE 0.
    wa_flag  = 'X' .
    wa_msgid = sy-msgid .
    wa_msgty = sy-msgty .
    wa_msgno = sy-msgno .
    wa_msgv1 = sy-msgv1 .
  ENDIF.
ENDFORM.                    " UPDATE_CLASSIFICATION_T04

*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E98
*&---------------------------------------------------------------------*
FORM update_classification_e98.

  CLEAR: it_vmaster,
         it_vmaster[].
** Changed by Furong on 12/01/09
  IF wa_old_cid < it_ztpperm-erpid.
    PERFORM append_vmaster USING 'EN_CID'          it_ztpperm-erpid  .
  ENDIF.
** End of change
  PERFORM append_vmaster USING 'EN_COMPL_DATE'   it_ztpperm-rdate  .
  PERFORM append_vmaster USING 'EN_COMPL_TIME'   it_ztpperm-rtime  .
  PERFORM append_vmaster USING 'EN_KKK_SEQ'      it_ztpperm-rseq   .

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object     = wa_equnr
      mode       = 'W'
      cmode      = '002'
    TABLES
      val_table  = it_vmaster
    EXCEPTIONS
      no_data    = 1
      error_mode = 2
      OTHERS     = 3.
  IF sy-subrc NE 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    wa_flag  = 'X' .
    wa_msgid = sy-msgid .
    wa_msgty = sy-msgty .
    wa_msgno = sy-msgno .
    wa_msgv1 = sy-msgv1 .
  ENDIF.
ENDFORM.                    " UPDATE_CLASSIFICATION_E98

*&---------------------------------------------------------------------*
*&      Form  ASSIGN_CLASS
*&---------------------------------------------------------------------*
FORM assign_class USING p_eassyid.
  DATA: l_equi    LIKE    v_equi       ,
        l_class   TYPE    klah-class   ,
        l_klart   TYPE    kssk-klart   .

  l_equi-equnr = p_eassyid.
  l_class      = ztpper_mapping-class       .
  l_klart      = ztpper_mapping-klart       .

  CALL FUNCTION 'EQUIPMENT_CLASS_ALLOCATE'
    EXPORTING
      eq_class           = l_class   " ENG_ASSY_MASTER
      eq_class_type      = l_klart                          " '002'
*     IS_STANDARD        = ' '
      init_new           = 'X'
      lock_new           = 'X'
      update_new         = 'X'
      commit_new         = 'X'
    CHANGING
      s_equi             = l_equi
    EXCEPTIONS
      err_class_allocate = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    wa_flag  = 'X' .
    wa_msgty = 'E'.
    wa_msgid = sy-msgid .
    wa_msgty = sy-msgty .
    wa_msgno = sy-msgno .
    wa_msgv1 = sy-msgv1 .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .
*----> Update Classification
    CASE it_ztpperm-erpid .
      WHEN 'E00' .
        PERFORM update_classification_e00 .
      WHEN 'E05' .
        PERFORM update_classification_e06 USING 'KD' .
      WHEN 'E06' .
        PERFORM update_classification_e06_ckd USING 'KD' .
    ENDCASE .
  ENDIF.
ENDFORM.                    " ASSIGN_CLASS
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E01
*&---------------------------------------------------------------------*
FORM en_cid_e01.

** Changed by Furong on 06/12/08
  IF NOT it_ztpperm-eassyid IS INITIAL.
    SELECT SINGLE *
      FROM ztpperm
       WHERE erpid = it_ztpperm-erpid
         AND eassyid = it_ztpperm-eassyid
         AND zresult = 'S'.
    IF sy-subrc = 0.
      wa_flag  = 'X'.
      wa_msgid = 'SF'  .
      wa_msgty = 'S'   .
      wa_msgno = '999' .
      wa_msgv1 = text-801.  "Duplicated record
      EXIT.
    ENDIF.
  ENDIF.
** End of change

*-----> CANCEL PROCESS
  IF NOT it_ztpperm-prtnr IS INITIAL.
    CALL FUNCTION 'BAPI_REPMANCONF1_CANCEL'
      EXPORTING
        confirmation     = it_ztpperm-prtnr
        postdate         = wa_datum
      IMPORTING
        cancconfirmation = wa_cancco
        return           = return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    it_ztpperm-canc_prtnr = wa_cancco.
  ENDIF.

*& Ref. BFLUSHFLAG-BACKFLTYPE = '01'
*&                              '02' Z : Reporting Point Backflush
*&                                     : Goods issue for reporting point
*&                              '10' A : Only activity posting
*&                              '11' U : Unplanned consumption message
*&                              '12' V : Scrap Message
*&                              '20'
*------> YIELD QTY

  IF it_ztpperm-erpid = 'E05'.

    CLEAR : bflushdatagen,
            bflushflags.
    bflushflags-bckfltype       = '01'            .   "Backflushing type

    bflushdatagen-materialnr    = it_ztpperm-eitem .   "Material number
** Furong on 01/17/12
*    bflushdatagen-prodplant     = 'E001'          .   "Plant
    bflushdatagen-prodplant     = it_ztpperm-en_reserve_02.
** end on 01/17/12
    bflushdatagen-prodversion   = '01'            .   "Production version
*  BFLUSHDATAGEN-POSTDATE      = IT_ZTPPERM-RDATE .   "Posting date
    bflushdatagen-postdate      = wa_datum .          "Posting date
    bflushdatagen-docdate       = sy-datum        .   "Document date

** Changed by Furong on 06/13/08, requested by MY Hur
*    BFLUSHDATAGEN-DOCHEADERTXT  = IT_ZTPPERM-RTIME .   "Doc header text
    bflushdatagen-docheadertxt  = it_ztpperm-eassyid.  "Engine serial no
** end of change

    bflushdatagen-unitofmeasure = it_ztpperm-meins .   "UoM

    IF it_ztpperm-eqty > 0.
      bflushdatagen-backflquant   = it_ztpperm-eqty.   "Yield Qty
    ENDIF.

    IF it_ztpperm-sqty > 0.
      bflushdatagen-scrapquant    = it_ztpperm-sqty.   "Scrap qty
    ENDIF.

    IF it_ztpperm-rqty > 0.
      bflushdatagen-backflquant   = it_ztpperm-rqty .  "Rework Qty
      bflushdatagen-storageloc    = 'E128'         .  "Rework SLoc
    ENDIF.

*-----> BAPI FOR BACKFLUSH
    CALL FUNCTION 'BAPI_REPMANCONF1_CREATE_MTS'
      EXPORTING
        bflushflags   = bflushflags
        bflushdatagen = bflushdatagen
        bflushdatamts = bflushdatamts
      IMPORTING
        confirmation  = wa_confirmation
        return        = return
      TABLES
        serialnr      = it_serialnr.

    PERFORM call_return_message .   "  USING L_CONFIRMATION .
  ENDIF.
*
ENDFORM.                    " EN_CID_E01

*&---------------------------------------------------------------------*
*&      Form  call_return_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_return_message. "  USING PA_CONFIRM.
  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
    EXPORTING
      type   = return-type
      cl     = return-id
      number = return-number
      par1   = return-message_v1
      par2   = return-message_v2
      par3   = return-message_v3
      par4   = return-message_v4
    IMPORTING
      return = return.

*  IF NOT PA_CONFIRM IS INITIAL.
  IF NOT wa_confirmation IS INITIAL.
    it_ztpperm-prtnr = wa_confirmation . "PA_CONFIRM.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
*-----> Classification Update
    IF it_ztpperm-erpid EQ 'E05' .
      PERFORM en_cid_e06 USING space .
    ELSE.
      PERFORM select_mat_doc .
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
** Furong on 04/02/12 for correct empty message with 'S' status
    CLEAR : return-message, return-type .
    MOVE 'E'      TO   return-type .
    MOVE 'NO MAT DOC CREATED' TO return-message.
** end on 04/02/12
  ENDIF.
ENDFORM.                    " call_return_message

*&---------------------------------------------------------------------*
*&      Form  APPEND_VMASTER
*&---------------------------------------------------------------------*
FORM append_vmaster USING p_atnam  p_atwrt .
  CLEAR it_vmaster .
  it_vmaster-atnam = p_atnam    .
  it_vmaster-atwrt = p_atwrt    .
  APPEND it_vmaster.
ENDFORM.                    " APPEND_VMASTER
*&---------------------------------------------------------------------*
*&      Form  PROCESS_REPORT_POINT
*&---------------------------------------------------------------------*
FORM process_report_point.
  CLEAR : wa_cancco , wa_confirmation .
  wa_equnr = it_ztpperm-eassyid.

** Changed by Furong on 12/01/09
  CLEAR : it_vmaster,  it_vmaster[], wa_old_cid.
  PERFORM append_vmaster USING 'EN_CID'   ' '.
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object     = wa_equnr
      mode       = 'R'
      ctype      = '002'
    TABLES
      val_table  = it_vmaster
    EXCEPTIONS
      no_data    = 1
      error_mode = 2
      OTHERS     = 3.
  READ TABLE it_vmaster INDEX 1.
  wa_old_cid = it_vmaster-atwrt.

  CASE it_ztpperm-erpid.
    WHEN 'E00'.    "Process order
      PERFORM en_cid_e00.
    WHEN 'E01'.    "Cylinder Block of Process Line
      PERFORM en_cid_e01.
    WHEN 'E02'.    "Crank Shaft of Process Line
      PERFORM en_cid_e01.
    WHEN 'E03'.    "Cylinder Head of Process Line
      PERFORM en_cid_e01.
    WHEN 'E04'.    "Engine Assembly Line
      PERFORM en_cid_e04.
    WHEN 'E05'.    "Engine Line Out
*-----> Create Engine Assy Master for KD Engine Assy ID
      PERFORM en_cid_e05.
*    WHEN 'E06'.    "Storage In
*      PERFORM EN_CID_E06.
** Added by Furong Wang on 04/15/09
    WHEN 'E06'.    "Storage In
      PERFORM en_cid_e06_ckd.
** End of addition
    WHEN 'E07'.    "Storage Out
      PERFORM en_cid_e07.

** Changed by Furong Wang on 08/18/09
** Added by Furong Wang on 07/22/09
*    WHEN 'E09'.
*      PERFORM EN_CID_E09.
*    WHEN 'E10'.
*      PERFORM EN_CID_E10.
** end of addtion by Furong Wang
** end of addtion by Furong Wang on 08/18/09
    WHEN 'T04'.    "
*        PERFORM EN_CID_T04.
** Changed by Furong Wang on 08/18/09
*    WHEN 'E98'.    "
*      PERFORM EN_CID_E98.
** end of addtion by Furong Wang on 08/18/09
    WHEN 'E99'.    "
      PERFORM en_cid_e99.
  ENDCASE.
ENDFORM.                    " PROCESS_REPORT_POINT

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
*&      Form  WRITE_OPEN
*&---------------------------------------------------------------------*
FORM write_open.

  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  WRITE AT: /001(030) 'Processing Time...(Start)' ,
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
         l_ztpperm_ix     TYPE    sy-tabix ,
         l_error_ix      TYPE    sy-tabix ,
         l_success_ix    TYPE    sy-tabix .

  DESCRIBE TABLE it_ztpperm  LINES   l_ztpperm_ix .
  DESCRIBE TABLE it_error   LINES   l_error_ix  .
  l_success_ix   =  l_ztpperm_ix  -  l_error_ix  .
  WRITE:/ text-311 , l_ztpperm_ix  .
  WRITE:/ text-312 , l_success_ix .
  WRITE:/ text-313 , l_error_ix   .
  SKIP 2.
*  LOOP AT IT_ERROR.
*    AT FIRST.
*      FORMAT RESET INTENSIFIED ON.
*      WRITE :/ '********** BEGIN OF ERROR Detail List ***********'.
*    ENDAT.
*    L_LINE = SY-TABIX MOD 2.
*    IF L_LINE EQ 1.
*      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
*    ELSE.
*      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*    ENDIF.
*      WRITE :/ IT_ERROR-TSEQ      COLOR COL_KEY ,
*               IT_ERROR-ERPID     COLOR COL_KEY ,
*               IT_ERROR-RSEQ      COLOR COL_KEY ,
*               IT_ERROR-EITEM     COLOR COL_KEY ,
*               IT_ERROR-EASSYID   COLOR COL_KEY ,
*               IT_ERROR-EPONO     COLOR COL_KEY ,
*
*               COLOR COL_NEGATIVE,COLOR COL_NORMAL.
*    AT LAST.
*      FORMAT RESET INTENSIFIED ON.
*      WRITE :/ '********** END OF ERROR Detail List ***********'.
*    ENDAT.
*  ENDLOOP.
ENDFORM.                    " WRITE_RESULT
*&---------------------------------------------------------------------*
*&      Form  WRITE_CLOSE
*&---------------------------------------------------------------------*
FORM write_close.
  SKIP 2.
  WRITE :/ '********** END OF PROCESS ***********'.
  WRITE AT: /001(030) 'Processing Time...(End)' ,
           031(010) sy-datum                    ,
           042(010) sy-uzeit                    .
  WRITE :/ '********** END OF PROCESS ***********'.
ENDFORM.                    " WRITE_CLOSE
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
FORM execute_process.

  CASE c_mark.
    WHEN r1 OR r2.
      PERFORM write_open.      "Write Header for Batch job
      PERFORM read_process.    "Read from ZTPPERM Where condition
      PERFORM data_process.    "Data processing for Report Point
      PERFORM write_result.    "Write Result for Batch job
      PERFORM write_close.     "Write Botton for Batch job
    WHEN r3.
      PERFORM read_process.    "Read from ZTPPERM Where condition
      CALL SCREEN 9000 .
  ENDCASE.
ENDFORM.                    " EXECUTE_PROCESS
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
      EXPORTING
        container_name = wa_container.

    CREATE OBJECT alv_grid
      EXPORTING
        i_parent = gs_custom_container.

    PERFORM  build_variant.
    PERFORM  build_layout.
    PERFORM  build_fieldcat.

*-----> SET OBJECT
    CALL METHOD alv_grid->set_table_for_first_display
      EXPORTING
*       I_BYPASSING_BUFFER            =
*       I_BUFFER_ACTIVE               =
*       I_CONSISTENCY_CHECK           =
        i_structure_name              = 'ZTPPERM'
        is_variant                    = gs_variant
        i_save                        = 'A'
*       I_DEFAULT                     = 'X'
        is_layout                     = gs_layout
*       IS_PRINT                      =
*       IT_SPECIAL_GROUPS             =
*       IT_TOOLBAR_EXCLUDING          =
*       IT_HYPERLINK                  =
*       IT_ALV_GRAPHICS               =
      CHANGING
        it_outtab                     = it_ztpperm[]
        it_fieldcatalog               = gt_fieldcat[]
*       IT_SORT                       =
*       IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

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

  l_struct = 'ZTPPERM'.
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
*&      Form  SELECT_MAT_DOC
*&---------------------------------------------------------------------*
FORM select_mat_doc.
  DATA : l_belnr     TYPE   blpp-belnr  .  "Material Doc#
*---->
  CLEAR : blpk, blpp.
  SELECT SINGLE *
              FROM blpk
              WHERE prtnr EQ wa_confirmation
                AND matnr EQ it_ztpperm-eitem
** Furong 01/27/12
*                AND WERKS EQ 'E001'
                AND werks EQ it_ztpperm-en_reserve_02
** on 01/27/12
                AND verid EQ '01'      .
  IF sy-subrc NE 0.
** Added by Furong on 07/09/08
    CLEAR : return-message, return-type .
    MOVE 'E'      TO   return-type .
    MOVE 'NO MAT DOC CREATED' TO return-message.
** end of addition
  ELSE.
    SELECT SINGLE belnr
                  INTO l_belnr
                  FROM blpp
                  WHERE prtnr  EQ  blpk-prtnr
                    AND prtps  EQ  '0001' .
    IF sy-subrc NE 0.
** Added by Furong on 07/09/08
      CLEAR : return-message, return-type .
      MOVE 'E'      TO   return-type .
      MOVE 'NO MAT DOC CREATED' TO return-message.
** end of addition
    ELSE.
      CLEAR : return-message, return-type .
      MOVE 'S'      TO   return-type .
      CONCATENATE 'GR:' l_belnr INTO return-message
                               SEPARATED BY space .
** Changed by Furong on 07/08/08
      IF it_ztpperm-erpid EQ 'E05'.
        CLEAR: it_vmaster,
               it_vmaster[].
        PERFORM append_vmaster USING 'EN_GR_DOCU_NO'  l_belnr .

        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object     = wa_equnr
            mode       = 'W'
            cmode      = '002'
          TABLES
            val_table  = it_vmaster
          EXCEPTIONS
            no_data    = 1
            error_mode = 2
            OTHERS     = 3.
      ENDIF.
** End of change
    ENDIF.
  ENDIF.

ENDFORM.                    " SELECT_MAT_DOC
*&---------------------------------------------------------------------*
*&      Form  COPY_TO_ZTPPERM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM copy_to_ztpperm.
  DATA: l_rdate LIKE ztpper2-rdate,
        l_tseq LIKE ztpper2-tseq,
        l_index LIKE sy-tabix.
  DATA: lt_ztpper_o LIKE TABLE OF ztpper WITH HEADER LINE,
        lt_ztpper2 LIKE TABLE OF ztpper2 WITH HEADER LINE,
        lt_ztpperm LIKE TABLE OF ztpperm WITH HEADER LINE.

** copy ztpper table to ztpperm

*  SELECT MAX( RDATE ) MAX( TSEQ ) INTO (L_RDATE, L_TSEQ )
*    FROM ZTPPERM
*   WHERE STABLE = 'R'.
*  IF L_RDATE IS INITIAL.
*    SELECT * INTO TABLE LT_ZTPPER_O
*      FROM ZTPPER
*     WHERE RDATE = SY-DATUM.
*  ELSE.
*    SELECT * INTO TABLE LT_ZTPPER_O
*       FROM ZTPPER
*      WHERE ( TSEQ > L_TSEQ AND RDATE = L_RDATE )
*         OR ( RDATE > L_RDATE ).
*  ENDIF.
  SELECT * INTO TABLE lt_ztpper_o
         FROM ztpper
        WHERE zresult = 'I'.
  IF sy-subrc = 0.
    LOOP AT lt_ztpper_o.
      MOVE-CORRESPONDING lt_ztpper_o TO lt_ztpperm.
      lt_ztpperm-stable = 'R'.

** Changed by Furong Wang on 06/19/08

** Changed by Furong on 07/16/08

*      CALL FUNCTION 'Z_FPP_CHANGE_DATE'
*           EXPORTING
*                IWERKS                     = 'E001'
*                IDATE                      = LT_ZTPPERM-RDATE
*                ITIME                      = LT_ZTPPERM-RTIME
*           IMPORTING
*                ODATE                      = LT_ZTPPERM-PROD_DT
*           EXCEPTIONS
*                FACTORY_CALENDAR_NOT_FOUND = 1
*                HOLIDAY_CALENDAR_NOT_FOUND = 2
*                DATE_HAS_INVALID_FORMAT    = 3
*                DATE_INCONSISTENCY         = 4
*                ERROR_TIME                 = 5
*                OTHERS                     = 6.

** End of change

      IF lt_ztpperm-rtime GE '051500' AND lt_ztpperm-rtime LE '240000' .
        lt_ztpperm-prod_dt  =  lt_ztpperm-rdate.
      ELSEIF lt_ztpperm-rtime GE '000000' AND
             lt_ztpperm-rtime LT '051500'.
        lt_ztpperm-prod_dt  =  lt_ztpperm-rdate - 1.
      ENDIF.

** End of change on 07/16/08


      APPEND lt_ztpperm.
      CLEAR: lt_ztpper_o,lt_ztpperm.
    ENDLOOP.
  ENDIF.

  INSERT ztpperm FROM TABLE lt_ztpperm.
  IF sy-subrc EQ 0.
    COMMIT WORK.
    LOOP AT lt_ztpper_o.
      l_index = sy-tabix.
      lt_ztpper_o-zresult = 'P'.
      MODIFY lt_ztpper_o INDEX l_index TRANSPORTING zresult.
    ENDLOOP.

    MODIFY ztpper FROM TABLE lt_ztpper_o.

    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
  REFRESH lt_ztpper_o.
  REFRESH lt_ztpperm.

** copy ztpper2 table to ztpperm
  CLEAR: l_rdate, l_tseq.
*  SELECT MAX( RDATE ) MAX( TSEQ ) INTO (L_RDATE, L_TSEQ )
*   FROM ZTPPERM
*  WHERE STABLE = 'M'.
*  IF L_RDATE IS INITIAL.
*    SELECT * INTO TABLE LT_ZTPPER2
*      FROM ZTPPER2
*     WHERE RDATE = SY-DATUM.
*  ELSE.
*    SELECT * INTO TABLE LT_ZTPPER2
*       FROM ZTPPER2
*      WHERE ( TSEQ > L_TSEQ AND RDATE = L_RDATE )
*         OR ( RDATE > L_RDATE ).
*  ENDIF.
  SELECT * INTO TABLE lt_ztpper2
        FROM ztpper2
         WHERE zresult = 'I'
       %_HINTS ORACLE 'RULE'.    "Addition

** Changed by Furong on 11/30/09
  LOOP AT lt_ztpper2.
    IF lt_ztpper2-erpid = 'E09' OR
       lt_ztpper2-erpid = 'E10' OR
       lt_ztpper2-erpid = 'E98'.
      DELETE lt_ztpper2.
    ENDIF.
  ENDLOOP.
** End of change on 11/30/09

  IF sy-subrc = 0.
    LOOP AT lt_ztpper2.
      MOVE-CORRESPONDING lt_ztpper2 TO lt_ztpperm.
      lt_ztpperm-stable = 'M'.
      APPEND lt_ztpperm.
      CLEAR: lt_ztpper2,lt_ztpperm.
    ENDLOOP.

    INSERT ztpperm FROM TABLE lt_ztpperm.
    IF sy-subrc EQ 0.
      COMMIT WORK.
      LOOP AT lt_ztpper2.
        l_index = sy-tabix.
        lt_ztpper2-zresult = 'P'.
        MODIFY lt_ztpper2 INDEX l_index TRANSPORTING zresult.
      ENDLOOP.
      MODIFY ztpper2 FROM TABLE lt_ztpper2.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
  REFRESH lt_ztpper2.
ENDFORM.                    " COPY_TO_ZTPPERM

*---------------------------------------------------------------------*
*       FORM more_new_char                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM more_new_char.

** Changed by Furong on 12/17/09
* PERFORM APPEND_VMASTER USING 'EN_HEAD_LH_SERIAL' IT_ZTPPERM-HEAD_SRL_N
*.
* PERFORM APPEND_VMASTER USING 'EN_HEAD_RH_SERIAL'
*                                IT_ZTPPERM-HEAD_SRL_N_RH.
*
* PERFORM APPEND_VMASTER USING 'EN_BLOCK_SERIAL' IT_ZTPPERM-BLOCK_SRL_NO
*.
* PERFORM APPEND_VMASTER USING 'EN_CRANK_SERIAL' IT_ZTPPERM-CRANK_SRL_NO
*.
  IF it_ztpperm-head_srl_n IS INITIAL.
  ELSE.
    PERFORM append_vmaster USING 'EN_HEAD_LH_SERIAL'
                                  it_ztpperm-head_srl_n.
  ENDIF.
  IF it_ztpperm-head_srl_n_rh IS INITIAL.
  ELSE.
    PERFORM append_vmaster USING 'EN_HEAD_RH_SERIAL'
                                  it_ztpperm-head_srl_n_rh.
  ENDIF.
  IF it_ztpperm-block_srl_no IS INITIAL.
  ELSE.
    PERFORM append_vmaster USING 'EN_BLOCK_SERIAL'
                                  it_ztpperm-block_srl_no.
  ENDIF.
  IF it_ztpperm-crank_srl_no IS INITIAL.
  ELSE.
    PERFORM append_vmaster USING 'EN_CRANK_SERIAL'
                                  it_ztpperm-crank_srl_no.
  ENDIF.
** End of change on 12/17/09
ENDFORM.                    "MORE_NEW_CHAR
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E06_ckd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM en_cid_e06_ckd.

*  check assy ID.
  SELECT SINGLE * FROM equi
         WHERE equnr EQ wa_equnr .  "IT_ZTPPERM-EASSYID.
  IF sy-subrc EQ 0 .
    PERFORM update_classification_e06_ckd  USING 'KD'.
  ELSE.
    CLEAR ztpper_mapping .
    SELECT SINGLE * FROM ztpper_mapping.

    external_number         = it_ztpperm-eassyid.      "Equipment Master
* General Tab
    data_general-objecttype = ztpper_mapping-eqart.	"Object Type
    data_general-descript   = ztpper_mapping-shtxt.	"Description

* Location Tab
    data_general-maintplant = ztpper_mapping-swerk.   "Maint Plant
    data_general-maintloc   = ztpper_mapping-stort.   "Maint Location
    data_general-maintroom  = ztpper_mapping-msgrp.	"Room
    data_general-plsectn    = ztpper_mapping-beber.	"Plant Section

*    DATA_GENERAL-PP_WKCTR   = ZTPPER_MAPPING-ARBPL.  "Object ID (W/C)
    SELECT SINGLE objid
           INTO data_general-pp_wkctr
           FROM crhd
           WHERE arbpl EQ ztpper_mapping-arbpl
** Furong 01/27/12
*             AND WERKS EQ 'E001' .  "ZTPPER_MAPPING-WERGW .
             AND werks EQ it_ztpperm-en_reserve_02.
** on 01/27/12

* Organization Tab
    data_general-comp_code  = ztpper_mapping-bukrs.	"Company Code
    data_general-costcenter = ztpper_mapping-kostl.	"Cost Center
    data_general-planplant  = ztpper_mapping-iwerk.	"Planning Plant
    data_general-plangroup  = ztpper_mapping-ingrp.   "Planner Group

*    DATA_GENERAL-WORK_CTR   = ZTPPER_MAPPING-ARBPL.  "Work Center
    SELECT SINGLE objid
           INTO data_general-work_ctr                "Work Center
           FROM crhd
           WHERE arbpl EQ ztpper_mapping-gewrk       "Work Center
** Furong 01/27/12
*             AND WERKS EQ 'E001' .  "ZTPPER_MAPPING-WERGW .
             AND werks EQ it_ztpperm-en_reserve_02.
** on 01/27/12

* Sales and Distribution Tab
    data_general-sales_org  = ztpper_mapping-vkorg.   "Sales Org
    data_general-distr_chan = ztpper_mapping-vtweg.   "Distribution
    data_general-division   = ztpper_mapping-spart.   "Division

* Ser Data Tab
    data_specific-material   = it_ztpperm-eitem.       "ITEM
    data_specific-equicatgry = ztpper_mapping-eqtyp.  "Equi Category 'E'

    CALL FUNCTION 'BAPI_EQUI_CREATE'
      EXPORTING
        external_number   = external_number
        data_general      = data_general
        data_specific     = data_specific
        valid_date        = wa_datum
        data_install      = data_install
      IMPORTING
        equipment         = external_number
        data_general_exp  = data_general
        data_specific_exp = data_specific
        return            = return.

*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*    WAIT UP TO 1 SECONDS.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = return-type
        cl     = return-id
        number = return-number
        par1   = return-message_v1
        par2   = return-message_v2
        par3   = return-message_v3
        par4   = return-message_v4
      IMPORTING
        return = return.

    IF return-type  = 'E'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
*----> Assign class to Engine Master
      PERFORM assign_class USING it_ztpperm-eassyid.

    ENDIF.
  ENDIF.

ENDFORM.                    " EN_CID_E06_ckd
*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E06_ckd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_classification_e06_ckd  USING p_cid.
  CLEAR: it_vmaster,
       it_vmaster[].
** Changed by Furong on 12/01/09
  IF wa_old_cid < it_ztpperm-erpid.
    PERFORM append_vmaster USING 'EN_CID'        it_ztpperm-erpid  .
  ENDIF.
** End of change
  PERFORM append_vmaster USING 'EN_ITEM_CODE'  it_ztpperm-eitem  .
  PERFORM append_vmaster USING 'EN_ASRS_IN_DATE' it_ztpperm-rdate  .
*  PERFORM APPEND_VMASTER USING 'EN_COMPL_DATE' WA_DATUM         .
  PERFORM append_vmaster USING 'EN_ASRS_IN_TIME' it_ztpperm-rtime  .
  PERFORM append_vmaster USING 'EN_ASRS_IN_SEQ'  it_ztpperm-rseq   .
  PERFORM append_vmaster USING 'EN_RESERVE_02'
          it_ztpperm-EN_RESERVE_02.

  PERFORM more_new_char.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object     = wa_equnr
      mode       = 'W'
      cmode      = '002'
    TABLES
      val_table  = it_vmaster
    EXCEPTIONS
      no_data    = 1
      error_mode = 2
      OTHERS     = 3.
  IF sy-subrc NE 0.
    wa_flag  = 'X' .
    wa_msgid = sy-msgid .
    wa_msgty = sy-msgty .
    wa_msgno = sy-msgno .
    wa_msgv1 = sy-msgv1 .
  ELSE.
*    IF P_CID NE 'KD' .
*      PERFORM SELECT_MAT_DOC .
*    ENDIF.
  ENDIF.

ENDFORM.                    " UPDATE_CLASSIFICATION_E06_ckd
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E09
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM en_cid_e09.
  PERFORM update_classification_e09 .
  PERFORM post_mat_mb1b.
ENDFORM.                    " EN_CID_E09
*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E09
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_classification_e09.
  DATA: l_cnt LIKE zspp_vin_value-atwrt.

  CLEAR : it_vmaster,  it_vmaster[] .
*  PERFORM APPEND_VMASTER USING 'EN_ITEM_CODE'  IT_ZTPPERM-EITEM  .
** Changed by Furong on 12/01/09
  IF wa_old_cid < it_ztpperm-erpid.
    PERFORM append_vmaster USING 'EN_CID'        it_ztpperm-erpid  .
  ENDIF.
** End of change
  PERFORM append_vmaster USING 'EN_RACK_DATE'   it_ztpperm-rdate  .
  PERFORM append_vmaster USING 'EN_RACK_TIME'   it_ztpperm-rtime  .
  PERFORM append_vmaster USING 'EN_RACK_SEQ'    it_ztpperm-rseq   .
  PERFORM append_vmaster USING 'EN_RACK_NO'     it_ztpperm-en_rack_no.
  PERFORM append_vmaster USING 'EN_RACK_WORKER'   it_ztpperm-en_worker.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object     = wa_equnr
      mode       = 'W'
      ctype      = '002'
    TABLES
      val_table  = it_vmaster
    EXCEPTIONS
      no_data    = 1
      error_mode = 2
      OTHERS     = 3.
  IF sy-subrc NE 0.
    wa_flag  = 'X' .
    wa_msgid = sy-msgid .
    wa_msgty = sy-msgty .
    wa_msgno = sy-msgno .
    wa_msgv1 = sy-msgv1 .
  ENDIF.

ENDFORM.                    " UPDATE_CLASSIFICATION_E09
*&---------------------------------------------------------------------*
*&      Form  EN_CID_E10
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM en_cid_e10.
  PERFORM update_classification_e10.
ENDFORM.                    " EN_CID_E10
*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E10
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_classification_e10.

  CLEAR : it_vmaster,  it_vmaster[] .
*  PERFORM APPEND_VMASTER USING 'EN_ITEM_CODE'  IT_ZTPPERM-EITEM  .
** Changed by Furong on 12/01/09
  IF wa_old_cid < it_ztpperm-erpid.
    PERFORM append_vmaster USING 'EN_CID'        it_ztpperm-erpid  .
  ENDIF.
** End of change
  PERFORM append_vmaster USING 'EN_TRUCK_DATE'   it_ztpperm-rdate  .
  PERFORM append_vmaster USING 'EN_TRUCK_TIME'   it_ztpperm-rtime  .
  PERFORM append_vmaster USING 'EN_TRUCK_SEQ'    it_ztpperm-rseq   .
  PERFORM append_vmaster USING 'EN_RACK_NO'     it_ztpperm-en_rack_no.
  PERFORM append_vmaster USING 'EN_TRUCK_NO'  it_ztpperm-en_truck_no.
  PERFORM append_vmaster USING 'EN_WORK_FLAG' it_ztpperm-en_work_flag.
  PERFORM append_vmaster USING 'EN_TRUCK_WORKER'   it_ztpperm-en_worker.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object     = wa_equnr
      mode       = 'W'
      ctype      = '002'
    TABLES
      val_table  = it_vmaster
    EXCEPTIONS
      no_data    = 1
      error_mode = 2
      OTHERS     = 3.
  IF sy-subrc NE 0.
    wa_flag  = 'X' .
    wa_msgid = sy-msgid .
    wa_msgty = sy-msgty .
    wa_msgno = sy-msgno .
    wa_msgv1 = sy-msgv1 .
  ENDIF.

ENDFORM.                    " UPDATE_CLASSIFICATION_E10
*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_E98A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_classification_e98a.

  CLEAR : it_vmaster,  it_vmaster[] .
  PERFORM append_vmaster USING 'EN_OITEM_CODE'  w_oitem.

*  PERFORM APPEND_VMASTER USING 'EN_GR_DOCU_NO'  L_BELNR .
*  IT_ZTPPERM-EITEM  .
*  PERFORM APPEND_VMASTER USING 'EN_CID'        IT_ZTPPERM-ERPID  .
*  PERFORM APPEND_VMASTER USING 'EN_TRUCK_DATE'   IT_ZTPPERM-RDATE  .
*  PERFORM APPEND_VMASTER USING 'EN_TRUCK_TIME'   IT_ZTPPERM-RTIME  .
**  PERFORM APPEND_VMASTER USING 'EN_TRUCK_SEQ'    IT_ZTPPERM-RSEQ   .
*  PERFORM APPEND_VMASTER USING 'EN_RACK_NO'     IT_ZTPPERM-EN_RACK_NO.
*  PERFORM APPEND_VMASTER USING 'EN_TRUCK_NO'  IT_ZTPPERM-EN_TRUCK_NO.
*  PERFORM APPEND_VMASTER USING 'EN_WORK_FLAG' IT_ZTPPERM-EN_WORK_FLAG.
*  PERFORM APPEND_VMASTER USING 'EN_TRUCK_WORKER'  IT_ZTPPERM-EN_WORKER.


  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object     = wa_equnr
      mode       = 'W'
      ctype      = '002'
    TABLES
      val_table  = it_vmaster
    EXCEPTIONS
      no_data    = 1
      error_mode = 2
      OTHERS     = 3.
  IF sy-subrc NE 0.
    wa_flag  = 'X' .
    wa_msgid = sy-msgid .
    wa_msgty = sy-msgty .
    wa_msgno = sy-msgno .
    wa_msgv1 = sy-msgv1 .
  ENDIF.

ENDFORM.                    " UPDATE_CLASSIFICATION_E98A
*&---------------------------------------------------------------------*
*&      Form  post_mat_mb1b
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_mat_mb1b.
  DATA: goodsmvt_header   LIKE  bapi2017_gm_head_01,
       goodsmvt_code     LIKE  bapi2017_gm_code,
       it_goodsmvt_item
       LIKE TABLE OF bapi2017_gm_item_create WITH HEADER LINE,
       it_return  LIKE TABLE OF bapiret2 WITH HEADER LINE.
  DATA: l_mblnr    TYPE  bapi2017_gm_head_ret-mat_doc,
        l_mjahr    TYPE  bapi2017_gm_head_ret-doc_year.

  SELECT SINGLE *
      FROM ztpperm
       WHERE erpid = it_ztpperm-erpid
         AND eassyid = it_ztpperm-eassyid
         AND zresult = 'S'
         AND zmsg <> ' '.

  IF sy-subrc = 0.
** this is duplicate data
    EXIT.
  ENDIF.

*  IF IT_ZTPPERM-EUSAGE NE SPACE.
*    CLEAR : ZTPP_MIP_STK_TRA.
*    SELECT SINGLE *
*           FROM ZTPP_MIP_STK_TRA
*           WHERE WERKS  EQ 'E001'
*             AND EUSAGE EQ IT_ZTPPERM-EUSAGE.
*
*    IF SY-SUBRC EQ 0.
  goodsmvt_header-pstng_date = wa_datum.
  goodsmvt_header-doc_date   = it_ztpperm-rdate.
  goodsmvt_header-header_txt = it_ztpperm-eassyid.
*  GM_Code 01: Goods receipt for purchase order
*  GM_Code 02: Goods receipt for production order
*  GM_Code 03: Goods issue
*  GM_Code 04: Transfer posting
*  GM_Code 05: Other goods receipts
*  GM_Code 06: Reversal of goods movements
  goodsmvt_code = '04'.
  it_goodsmvt_item-material  = it_ztpperm-eitem.   "MATERIAL

** Furong on 01/17/12
*  it_goodsmvt_item-plant     = 'E001' . "Engine PLANT
  it_goodsmvt_item-plant     =  it_ztpperm-en_reserve_02.
** end on 01/17/12

  CLEAR : marc.
  SELECT SINGLE * FROM marc
         WHERE matnr EQ it_ztpperm-eitem          "ENGINE MATERIAL
           AND werks EQ ztpp_mip_stk_tra-werks.  "ENGINE PLANT
  IF it_ztpperm-eitem+0(2) = 'AU'.
    it_goodsmvt_item-stge_loc   = 'E110'.      "FROM
  ELSE.
    it_goodsmvt_item-stge_loc   = 'E210'.      "FROM
  ENDIF.
  it_goodsmvt_item-move_type  = '311' . "MVT type
  it_goodsmvt_item-entry_qnt  = it_ztpperm-eqty.
  it_goodsmvt_item-entry_uom  = it_ztpperm-meins.

  it_goodsmvt_item-move_stloc = 'E301'.  "TO Stock
  APPEND it_goodsmvt_item.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header       = goodsmvt_header
      goodsmvt_code         = goodsmvt_code
    IMPORTING
      materialdocument      = l_mblnr
      matdocumentyear       = l_mjahr
    TABLES
      goodsmvt_item         = it_goodsmvt_item
*     GOODSMVT_SERIALNUMBER =
      return                = it_return.

  READ TABLE it_return INDEX 1.
  MOVE-CORRESPONDING it_return TO return.

  IF l_mblnr IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
** Update engine master
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

    CLEAR: it_vmaster,
           it_vmaster[].
    PERFORM append_vmaster USING 'EN_MDOC_STORAGE'  l_mblnr.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        object     = it_ztpperm-eassyid
        mode       = 'W'
        cmode      = '002'
      TABLES
        val_table  = it_vmaster
      EXCEPTIONS
        no_data    = 1
        error_mode = 2
        OTHERS     = 3.

    CLEAR : return-message, return-type .
    MOVE 'S'      TO   return-type .
    CONCATENATE 'TR:' l_mblnr INTO return-message
                             SEPARATED BY space .
  ENDIF.
ENDFORM.                    " post_mat_mb1b
*&---------------------------------------------------------------------*
*&      Form  check_batch_job
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_batch_job.
** Added by Furong Wang on 03/12/10
  DATA: lt_status LIKE TABLE OF tbtco WITH HEADER LINE,
        l_cn TYPE i,
*        L_JOBNAME LIKE TBTCO-JOBNAME default ',
        l_flag TYPE char1.

  DATA: lt_check_log LIKE TABLE OF ztpp_pmt07jb_b WITH HEADER LINE.

  SELECT * INTO TABLE lt_status
    FROM tbtco
    WHERE jobname LIKE 'ZPP_ENG_SBF%'
      AND status = 'R'.
*      AND ( STATUS = 'P'
*         OR STATUS = 'S'
*         OR STATUS = 'Y'
*         OR STATUS = 'R' ).
  DESCRIBE TABLE lt_status LINES l_cn.
  IF l_cn > 1.
    w_batch_job = 'X'.
    MESSAGE e001 WITH 'Please check Job log for same job'.
  ELSE.
    CLEAR: w_batch_job.
  ENDIF.
** End of addition

ENDFORM.                    " check_batch_job

*&---------------------------------------------------------------------*
*&      Form  POST_GR (C3)
*&---------------------------------------------------------------------*

FORM post_gr_c3.
  DATA: lt_3c LIKE ztpperm OCCURS 0 WITH HEADER LINE.
  DATA: l_tabix LIKE sy-tabix,
        l_msg LIKE cfgnl-msglin.
*& Ref. BFLUSHFLAG-BACKFLTYPE = '01'
*&                              '02' Z : Reporting Point Backflush
*&                                     : Goods issue for reporting point
*&                              '10' A : Only activity posting
*&                              '11' U : Unplanned consumption message
*&                              '12' V : Scrap Message
*&                              '20'
*------> YIELD QTY

  SORT it_ztpperm BY zresult erpid eitem prod_dt.

  LOOP AT it_ztpperm WHERE zresult = 'S'.
    IF it_ztpperm-erpid = 'E01' OR
       it_ztpperm-erpid = 'E02' OR
       it_ztpperm-erpid = 'E03'.
      lt_3c-erpid = it_ztpperm-erpid.
      lt_3c-eitem = it_ztpperm-eitem.
** Furong on 04/02/12 for seperated plant
*      lt_3c-plant_cd = it_ztpperm-plant_cd.
** End on 04/02/12
      lt_3c-prod_dt = it_ztpperm-prod_dt.
      lt_3c-eqty = it_ztpperm-eqty.
      lt_3c-sqty = it_ztpperm-sqty.
      lt_3c-rqty =  it_ztpperm-rqty.
*     lt_3c = it_ztpperm.
      COLLECT  lt_3c.
      CLEAR: lt_3c.
    ENDIF.
  ENDLOOP.

  SORT lt_3c BY erpid eitem.

  LOOP AT lt_3c.
    READ TABLE it_ztpperm WITH KEY zresult = 'S'
                                   erpid = lt_3c-erpid
                                   eitem = lt_3c-eitem
                                   prod_dt = lt_3c-prod_dt
                                   BINARY SEARCH.
    l_tabix = sy-tabix.

    CLEAR : bflushdatagen,
          bflushflags.
    bflushflags-bckfltype       = '01'            .   "Backflushing type

    bflushdatagen-materialnr    = it_ztpperm-eitem .   "Material number

** Furong on 01/27/12
*    bflushdatagen-prodplant     = 'E001'          .   "Plant
    bflushdatagen-prodplant     = it_ztpperm-en_reserve_02.
** on 01/27/12

    bflushdatagen-prodversion   = '01'            .   "Production version
*  BFLUSHDATAGEN-POSTDATE      = IT_ZTPPERM-RDATE .   "Posting date
    bflushdatagen-postdate      = lt_3c-prod_dt .    "Posting date
    bflushdatagen-docdate       = sy-datum        .   "Document date

** Changed by Furong on 06/13/08, requested by MY Hur
*    BFLUSHDATAGEN-DOCHEADERTXT  = IT_ZTPPERM-RTIME .   "Doc header text
    bflushdatagen-docheadertxt  = it_ztpperm-eassyid.  "Engine serial no
** end of change

    bflushdatagen-unitofmeasure = it_ztpperm-meins .   "UoM

    IF lt_3c-eqty > 0.
      bflushdatagen-backflquant   = lt_3c-eqty.   "Yield Qty
    ENDIF.

    IF lt_3c-sqty > 0.
      bflushdatagen-scrapquant    = lt_3c-sqty.   "Scrap qty
    ENDIF.

    IF lt_3c-rqty > 0.
      bflushdatagen-backflquant   = lt_3c-rqty .  "Rework Qty
      bflushdatagen-storageloc    = 'E128'         .  "Rework SLoc
    ENDIF.

*-----> BAPI FOR BACKFLUSH
    CALL FUNCTION 'BAPI_REPMANCONF1_CREATE_MTS'
      EXPORTING
        bflushflags   = bflushflags
        bflushdatagen = bflushdatagen
        bflushdatamts = bflushdatamts
      IMPORTING
        confirmation  = wa_confirmation
        return        = return
      TABLES
        serialnr      = it_serialnr.

    PERFORM call_return_message .   "  USING L_CONFIRMATION .


    LOOP AT it_ztpperm WHERE zresult = 'S' AND
                       erpid = lt_3c-erpid AND
                       eitem = lt_3c-eitem AND
                       prod_dt = lt_3c-prod_dt.

      PERFORM it_ztpperm_modify USING return-message
                                         return-type
                                         sy-tabix.

      MOVE-CORRESPONDING it_ztpperm TO wa_ztpperm.
      IF it_ztpperm-zresult EQ 'E' .
        MOVE-CORRESPONDING it_ztpperm TO it_error .
        APPEND it_error .
      ENDIF.

      UPDATE ztpperm FROM wa_ztpperm.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      ENDIF.
      CLEAR: wa_equnr   ,
             wa_ztpperm ,
             wa_flag    ,
*           WA_MSGID   ,
*           WA_MSGTY   ,
*           WA_MSGNO   ,
*           WA_MSGV1   ,
*           WA_MSGV2   ,
*           WA_MSGV3   ,
*           WA_MSGV4   ,
             it_error   .
*           L_MSG.

    ENDLOOP.
  ENDLOOP.
ENDFORM.                                                    "POST_GR_C3
