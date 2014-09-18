************************************************************************
*
** Program Name      : ZEMMPM41E_PRC_ANALY
** Author            : Min-su Park
** Creation Date     : 2003.12.01.
** Specifications By : Min-su Park
** Pattern           : Report 1-1
** Development Request No : UD1K901872
** Addl Documentation:
** Description       : Master Inspection Characteristic Uploading
**
** Modification Logs
** Date       Developer    RequestNo    Description
**
**
**
************************************************************************
*
REPORT  zemmpm41e_prc_analy           .
*
*DATA : BEGIN OF wa_matdoc       .
*        INCLUDE STRUCTURE ztmm_analy1.
*DATA : END OF wa_matdoc         .
*DATA : wa_mattmp LIKE wa_matdoc                          ,
*       it_matdoc LIKE STANDARD TABLE OF wa_matdoc        ,
*       it_diff   LIKE wa_matdoc OCCURS 0 WITH HEADER LINE.
*
*DATA : w_datef LIKE sy-datum,
*       w_datet LIKE sy-datum,
*       w_seq   TYPE n       .
*
*
**----- Selection Screen
*
*PARAMETERS: p_profl LIKE mara-profl,         "Local, KD
*            p_month LIKE s001-spmon.         "Month
*
*INITIALIZATION.
*  PERFORM set_initial.
**  PERFORM GET_PERIOD.
*
*START-OF-SELECTION.
**Get basic data
*  SELECT msg~werks   "Plant
*         msg~matnr   "Material No.
*         msg~lifnr   "Vendor Code
*         msg~bwart   "Movement Type
*         mkp~budat   "Posting Date
*         msg~erfmg   "Receipt Qty
*         msg~dmbtr   "Receipt Amount
*         msg~waers   "Currency
*      INTO CORRESPONDING FIELDS OF TABLE it_matdoc
*      FROM mara AS mat INNER JOIN mseg AS msg
*             ON mat~matnr = msg~matnr
*            INNER JOIN mkpf AS mkp
*             ON msg~mblnr = mkp~mblnr
*     WHERE mat~mtart = 'ROH'
*       AND mkp~mjahr = sy-datum+0(4)
*       AND ( mkp~budat >= w_datef AND
*             mkp~budat <= w_datet ).
*
**Get Posting Month, Actual Price, Standard Price, Standard adjust day
*  DATA : w_remain TYPE i.
*  LOOP AT it_matdoc INTO wa_matdoc.
*    w_remain = wa_matdoc-bwart / 2.
*    IF w_remain = 0.
*
*    ELSE.
*      wa_matdoc-erfmg = wa_matdoc-erfmg * -1.
*      wa_matdoc-dmbtr = wa_matdoc-dmbtr * -1.
*    ENDIF.
*    wa_matdoc-spmon = wa_matdoc-budat+0(6). "Posting Month
*    wa_matdoc-netpr = wa_matdoc-dmbtr / wa_matdoc-erfmg.
*    "Actual Price
*
*    MODIFY it_matdoc FROM wa_matdoc.
*    CLEAR wa_matdoc.
*  ENDLOOP.
*
**Sort
*  SORT it_matdoc BY werks  "Plant
*                    matnr  "Material No.
*                    lifnr  "Vendor Code
*                    spmon  "Posting month - *
*                    netpr ."Actual Price    *
*
*  LOOP AT it_matdoc INTO wa_matdoc.
*    MOVE-CORRESPONDING wa_matdoc TO it_diff.
*    AT END OF netpr.
*      SUM.
*      it_diff-erfmg = wa_matdoc-erfmg.
*      it_diff-dmbtr = wa_matdoc-dmbtr.
**3.2.1.3 add info about standard price for material.
*      SELECT SINGLE valid_d kbetr source ekorg kzust MAX( base_d )
*              INTO CORRESPONDING FIELDS OF it_diff
*              FROM ztmm_analy
*             WHERE werks = it_diff-werks
*               AND matnr = it_diff-matnr
*             GROUP by valid_d kbetr source ekorg kzust.
*      PERFORM create_diff USING it_diff.
*    ENDAT.
*  ENDLOOP.
*
*  PERFORM update_ztmm_analy1.
**&---------------------------------------------------------------------
*
**&      Form  GET_PERIOD
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM get_period.
*  w_datef = sy-datum.
** Get first day for period
*  w_datef+6(02) = '01'.
** Get last day for period
*  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
*       EXPORTING
*            day_in            = sy-datum
*       IMPORTING
*            last_day_of_month = w_datet.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*ENDFORM.                    " GET_PERIOD
**&---------------------------------------------------------------------
*
**&      Form  CREATE_DIFF
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**      -->P_IT_DIFF  text
**----------------------------------------------------------------------
*
*FORM create_diff USING p_diff LIKE wa_matdoc.
*  DATA : BEGIN OF it_a017 OCCURS 0.
*          INCLUDE STRUCTURE a017.
*  DATA : END OF it_a017.
*
*  DATA : w_cprice LIKE eine-netpr ,
*         w_oprice LIKE eine-netpr ,
*         w_next   LIKE sy-tabix   ,
*         wa_a017  LIKE a017       .
*
**Get price condition
*  SELECT * FROM a017
*           INTO CORRESPONDING FIELDS OF TABLE it_a017
*          WHERE kappl = 'M'
*            AND kschl = 'PB00'
*            AND lifnr = wa_matdoc-lifnr
*            AND matnr = wa_matdoc-matnr
*            AND ekorg = wa_matdoc-ekorg
*            AND datab <= sy-datum
*            AND datbi >= wa_matdoc-valid_d.
*
**Sort for price condition to process from recently price condition
*  SORT it_a017 BY datab DESCENDING.
*
*  CASE p_diff-source.
*    WHEN 'M'.
*      LOOP AT it_a017.
*        IF wa_matdoc-valid_d >= it_a017-datab
*       AND wa_matdoc-valid_d <= it_a017-datbi.
*          EXIT.
*        ENDIF.
**       [1] Current(first) net price
**       KZUST LIKE KONH-KZUST, "Reason Code       =
*        SELECT SINGLE kzust
*                 INTO p_diff-kzust
*                 FROM konh
*                WHERE knumh = it_a017-knumh.
**       Currency Price
*        SELECT SINGLE kbetr
*                 INTO w_cprice
*                 FROM konp
*                WHERE knumh = it_a017-knumh
*                  AND kappl = 'M'
*                  AND kschl = 'PB00'.
**        [2] Before net price
*        w_next = sy-tabix + 1.
*        READ TABLE it_a017 INTO wa_a017 INDEX w_next.
*        IF wa_a017-datab <= wa_matdoc-valid_d AND
*           wa_a017-datbi >= wa_matdoc-valid_d.
*          w_oprice = wa_matdoc-kbetr.
*        ELSE.
**       Previous Price
*          SELECT SINGLE kbetr
*                   INTO w_oprice
*                   FROM konp
*                  WHERE knumh = wa_a017-knumh
*                    AND kappl = 'M'
*                    AND kschl = 'PB00'.
*        ENDIF.
**        [3] Calculate Difference Price, Difference Amount,
**            Difference Rate.
**       NETPRD LIKE EINE-NETPR, "Difference Price  =
*        p_diff-netprd = w_cprice - w_oprice.
**       DMBTRD LIKE MSEG-DMBTR, "Difference Amount =
*        p_diff-dmbtrd = p_diff-netprd * p_diff-erfmg.
**       DMBTRR LIKE MSEG-DMBTR, "Difference Rate   =
*        p_diff-dmbtrr = ( p_diff-netpr - p_diff-kbetr ) / p_diff-kbetr.
*        p_diff-seq = p_diff-seq + 1.
*
**        [4]Get Record
*        APPEND p_diff TO it_diff.
*      ENDLOOP.
*    WHEN 'I'.
*      LOOP AT it_a017.
*        IF wa_matdoc-valid_d >= it_a017-datab
*       AND wa_matdoc-valid_d <= it_a017-datbi.
*          EXIT.
*        ENDIF.
**       [1] Current(first) net price
**       KZUST LIKE KONH-KZUST, "Reason Code       =
*        SELECT SINGLE kzust
*                 INTO p_diff-kzust
*                 FROM konh
*                WHERE knumh = it_a017-knumh.
**       Currency Price
*        SELECT SINGLE kbetr
*                 INTO w_cprice
*                 FROM konp
*                WHERE knumh = it_a017-knumh
*                  AND kappl = 'M'
*                  AND kschl = 'PB00'.
**       [2] Before net price
*        w_next = sy-tabix + 1.
*        READ TABLE it_a017 INTO wa_a017 INDEX w_next.
**       Previous Price
*        SELECT SINGLE kbetr
*                 INTO w_oprice
*                 FROM konp
*                WHERE knumh = wa_a017-knumh
*                  AND kappl = 'M'
*                  AND kschl = 'PB00'.
**        [3] Calculate Difference Price, Difference Amount,
**            Difference Rate.
**       NETPRD LIKE EINE-NETPR, "Difference Price  =
*        p_diff-netprd = w_cprice - w_oprice.
**       DMBTRD LIKE MSEG-DMBTR, "Difference Amount =
*        p_diff-dmbtrd = p_diff-netprd * p_diff-erfmg.
**       DMBTRR LIKE MSEG-DMBTR, "Difference Rate   =
*        p_diff-dmbtrr = ( p_diff-netpr - p_diff-kbetr ) / p_diff-kbetr.
*        p_diff-seq = p_diff-seq + 1.
**        [4]Get Record
*        APPEND p_diff TO it_diff.
*      ENDLOOP.
*  ENDCASE.
*ENDFORM.                    " CREATE_DIFF
**&---------------------------------------------------------------------
*
**&      Form  UPDATE_ZTMM_ANALY1
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM update_ztmm_analy1.
*  INSERT ztmm_analy1 FROM TABLE it_diff.
*ENDFORM.                    " UPDATE_ZTMM_ANALY1
**&---------------------------------------------------------------------
*
**&      Form  set_month
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM set_initial.
**----- Set Local
*  p_profl = 'K'.
*
**----- Set last month
*  IF sy-datum+4(2) EQ '01'.
*    p_month(4) = sy-datum(4) - 1.
*    CONCATENATE p_month(4) '12' INTO p_month.
*  ELSE.
*    p_month = sy-datum(6).
*  ENDIF.
*ENDFORM.                    " set_initial
