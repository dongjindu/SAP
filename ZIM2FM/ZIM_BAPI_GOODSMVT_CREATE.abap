FUNCTION ZIM_BAPI_GOODSMVT_CREATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_ZFIVNO) LIKE  ZTIV-ZFIVNO
*"     REFERENCE(P_CHG_MODE) TYPE  C DEFAULT 'X'
*"     REFERENCE(P_MVT_TYPE) LIKE  MSEG-BWART DEFAULT '101'
*"     REFERENCE(P_BLDAT) LIKE  MKPF-BLDAT DEFAULT SY-DATUM
*"     REFERENCE(P_BUDAT) LIKE  MKPF-BUDAT DEFAULT SY-DATUM
*"     REFERENCE(P_GRUND) LIKE  MSEG-GRUND
*"  EXPORTING
*"     VALUE(MATERIALDOCUMENT) TYPE  BAPI2017_GM_HEAD_RET-MAT_DOC
*"     VALUE(MATDOCUMENTYEAR) TYPE  BAPI2017_GM_HEAD_RET-DOC_YEAR
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2
*"      IT_ZSIVHSTIT STRUCTURE  ZSIVHSTIT OPTIONAL
*"  EXCEPTIONS
*"      MVT_ERROR
*"----------------------------------------------------------------------
DATA : W_TEXT70(70),
       L_MVT_TYPE    LIKE   P_MVT_TYPE,
       L_ITEM_TEXT   LIKE   GOODSMVT_ITEM-ITEM_TEXT,
       L_CHK         VALUE  'Y',
       W_ZFGRST      LIKE   ZTIV-ZFGRST.

DATA : W_MENGE       LIKE   EKPO-MENGE.
DATA : W_MENGE_GR    LIKE   EKPO-MENGE.
DATA : W_MENGE_IV    LIKE   EKPO-MENGE.
DATA : L_MVT_IND     LIKE   GOODSMVT_ITEM-MVT_IND,
       L_NO_MORE_GR  LIKE   GOODSMVT_ITEM-NO_MORE_GR.

DATA : BEGIN OF IT_GRMENGE OCCURS 0,
       EBELN         LIKE   ZTIVHSTIT-EBELN,
       EBELP         LIKE   ZTIVHSTIT-EBELP,
       GRMENGE       LIKE   ZTIVHSTIT-GRMENGE,
       END   OF IT_GRMENGE.

   REFRESH : GOODSMVT_ITEM, GOODSMVT_SERIALNUMBER, RETURN,
             IT_ZSIVIT.

   CLEAR :   GOODSMVT_ITEM, GOODSMVT_SERIALNUMBER, RETURN,
             MATERIALDOCUMENT, MATDOCUMENTYEAR,
             GOODSMVT_HEADER,  GOODSMVT_CODE,
             IT_ZSIVIT.

   SELECT SINGLE * FROM ZTIMIMG00.
   IF SY-SUBRC NE 0.
      MESSAGE E963   RAISING    MVT_ERROR.
   ENDIF.

*"----------------------------------------------------------------------
*>> KEY VALUE CHECK..
   IF P_ZFIVNO IS INITIAL.
      MESSAGE E412   RAISING    MVT_ERROR.
   ENDIF.

*"----------------------------------------------------------------------
*>> COMMERCIAL INVOICE HEADER SELECT.
   SELECT SINGLE * FROM   ZTIV
                   WHERE  ZFIVNO  EQ   P_ZFIVNO.
   IF SY-SUBRC NE 0.
      MESSAGE  E413 WITH P_ZFIVNO RAISING    MVT_ERROR.
   ENDIF.

*>>> Customs Clearance Status
  CASE ZTIV-ZFCUST.
     WHEN '1' OR '2' OR '3'.
        PERFORM   GET_DD07T_SELECT(SAPMZIM01)
                  USING      'ZDCUST'  ZTIV-ZFCUST
                  CHANGING    W_TEXT70.

        MESSAGE E419 WITH P_ZFIVNO W_TEXT70 'Goods Receipt'
                           RAISING MVT_ERROR.
     WHEN 'Y'.
  ENDCASE.

*>>> Import Expense Distrubution Status.
  CASE ZTIV-ZFCDST.
     WHEN 'X'.
     WHEN 'Y'.
     WHEN 'N'.
        PERFORM   GET_DD07T_SELECT(SAPMZIM01)
                  USING      'ZDCDST'  ZTIV-ZFCDST
                  CHANGING    W_TEXT70.
        MESSAGE E420 WITH P_ZFIVNO W_TEXT70 'G/R'
                           RAISING MVT_ERROR.
     WHEN OTHERS.
        MESSAGE E420 WITH P_ZFIVNO 'uncompleted input' 'G/R'
                           RAISING MVT_ERROR.
   ENDCASE.
*>>> G/R Status Check.
   CASE ZTIV-ZFGRST.
      WHEN 'Y' OR 'X'.
         PERFORM   GET_DD07T_SELECT(SAPMZIM01)
                   USING      'ZDGRST'  ZTIV-ZFGRST
                   CHANGING    W_TEXT70.
         MESSAGE E422 WITH P_ZFIVNO W_TEXT70 'G/R'
                           RAISING MVT_ERROR.
      WHEN 'N' OR 'P'.
   ENDCASE.

*>> B/L DOCUMENT.
   IF NOT ZTIV-ZFBLNO IS INITIAL.
      SELECT SINGLE * FROM ZTBL
                      WHERE ZFBLNO   EQ   ZTIV-ZFBLNO.

      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTBLIT
               FROM  ZTBLIT
               WHERE ZFBLNO   EQ   ZTIV-ZFBLNO.
   ENDIF.

*"----------------------------------------------------------------------
*>> G/R Requested ITEM SELECT.
   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTIVIT
            FROM  ZTIVIT
            WHERE ZFIVNO   EQ   P_ZFIVNO.

   CLEAR : W_LINE.
   DESCRIBE TABLE  IT_ZTIVIT LINES  W_LINE.
   IF W_LINE EQ 0.
      MESSAGE E549 RAISING MVT_ERROR.
   ENDIF.

*-----------------------------------------------------------------------
*  GOODSMVT_TRANSACTION.
*01:     MB01   : G/R Reference P/O
*02:     MB31   : G/R Based Order
*03:     MB1A   : Good Issue
*04:     MB1B   : Stock Transfer
*05:     MB1C   : Others G/R
*06:     MB11   : Goods Movement

   CASE P_MVT_TYPE.
      WHEN '101' OR '103' OR '105' OR '122' OR '124'.
         L_ITEM_TEXT = 'G/R about P/O'.
         GOODSMVT_CODE-GM_CODE = '01'.
         L_MVT_IND = 'B'.
      WHEN '501' OR '503' OR '505' OR '511'.
         L_ITEM_TEXT = 'Other G/R'.
         GOODSMVT_CODE-GM_CODE = '06'.
         CLEAR : L_MVT_IND.
      WHEN OTHERS.
         MESSAGE E641 WITH P_MVT_TYPE RAISING MVT_ERROR.
   ENDCASE.
   MOVE P_MVT_TYPE TO L_MVT_TYPE.

   CLEAR : L_NO_MORE_GR.
   IF ZTIV-ZFLGRST EQ 'X'.
      L_NO_MORE_GR = '1'.            ">Delivery Complete Yes/No
   ENDIF.

*>> ITEM LEVEL CHECK.
   REFRESH : IT_ZTIVHSTIT.
   W_LINE = 0.

   LOOP AT IT_ZSIVHSTIT WHERE UMSON EQ 'X'.
      MOVE-CORRESPONDING IT_ZSIVHSTIT TO IT_GRMENGE.

      IF GOODSMVT_CODE-GM_CODE EQ '01' OR
         GOODSMVT_CODE-GM_CODE EQ '06'.
         MOVE IT_ZSIVHSTIT-GRMENGE TO IT_GRMENGE-GRMENGE.
      ELSE.
         MOVE IT_ZSIVHSTIT-GRMENGE TO IT_GRMENGE-GRMENGE.
         IT_GRMENGE-GRMENGE = IT_GRMENGE-GRMENGE * -1.
      ENDIF.
      COLLECT IT_GRMENGE.
   ENDLOOP.

   LOOP AT IT_ZSIVHSTIT WHERE UMSON EQ 'X'.
      CLEAR : IT_ZTIVHSTIT.

*>> P/O HEADER Check
      IF NOT IT_ZSIVHSTIT-EBELN IS INITIAL.
         SELECT SINGLE * FROM  EKKO              ">P/O HEADER CHECK..
                  WHERE EBELN EQ IT_ZSIVHSTIT-EBELN.
         IF EKKO-LOEKZ NE SPACE.                 ">Deleted Mark
            MESSAGE E005 WITH IT_ZSIVHSTIT-EBELN RAISING    MVT_ERROR.
         ENDIF.

         SELECT SINGLE * FROM ZTREQHD
                WHERE  ZFREQNO EQ
                     ( SELECT ZFREQNO FROM ZTIVIT
                              WHERE   ZFIVNO  EQ IT_ZSIVHSTIT-ZFIVNO
                              AND     ZFIVDNO EQ IT_ZSIVHSTIT-ZFIVDNO ).

*>> < 2002.05.10 NHJ Mark of Fixed exchange rate CHECK! >
         IF ZTIMIMG00-ZFEXFIX EQ 'X' AND ZTIMIMG00-ZFEXMTD NE 'G'.
            IF EKKO-KUFIX IS INITIAL AND
               NOT ( ZTREQHD-ZFREQTY EQ 'PU'
                  OR ZTREQHD-ZFREQTY EQ 'LO' ).
               MESSAGE E101(ZIM1)
                  WITH IT_ZSIVHSTIT-EBELN RAISING    MVT_ERROR.
            ENDIF.
         ENDIF.

*>> P/O ITEM Check.
         SELECT SINGLE * FROM  EKPO              "> P/O ITEM CHECK..
                         WHERE EBELN EQ IT_ZSIVHSTIT-EBELN
                         AND   EBELP EQ IT_ZSIVHSTIT-EBELP.
         IF EKPO-LOEKZ NE SPACE.                "> Deleted Mark Check.
            MESSAGE E069 WITH IT_ZSIVHSTIT-EBELN IT_ZSIVHSTIT-EBELP
                         RAISING    MVT_ERROR.
         ENDIF.

*> Goods Price Quantity.
         SELECT SUM( CMENGE ) INTO W_MENGE_IV
                FROM  ZVCIVHD_IT
                WHERE EBELN  EQ IT_ZSIVHSTIT-EBELN
                AND   EBELP  EQ IT_ZSIVHSTIT-EBELP
                AND   ZFIVST EQ 'Y'.
         W_MENGE = IT_ZSIVHSTIT-GRMENGE * ( EKPO-BPUMZ / EKPO-BPUMN ).
      ELSE.
         IF GOODSMVT_CODE-GM_CODE EQ '01'.
            GOODSMVT_CODE-GM_CODE = '06'.
            L_MVT_TYPE = '501'.
         ENDIF.
         W_MENGE = IT_ZSIVHSTIT-GRMENGE.
      ENDIF.

*>> Valuation Class.
      SELECT * FROM T134G UP TO 1 ROWS
               WHERE WERKS EQ IT_ZSIVHSTIT-WERKS.
      ENDSELECT.

      MOVE-CORRESPONDING  IT_ZSIVHSTIT TO IT_ZTIVHSTIT.

*>> ITEM DATA MOVE.
      CLEAR : GOODSMVT_ITEM.
      ADD    1    TO     W_LINE.
      MOVE:IT_ZSIVHSTIT-MATNR  TO  GOODSMVT_ITEM-MATERIAL,
           IT_ZSIVHSTIT-WERKS  TO  GOODSMVT_ITEM-PLANT,
           IT_ZSIVHSTIT-LGORT  TO  GOODSMVT_ITEM-STGE_LOC,
           IT_ZSIVHSTIT-BATCH  TO  GOODSMVT_ITEM-BATCH,
           P_MVT_TYPE          TO  GOODSMVT_ITEM-MOVE_TYPE,
           SPACE               TO  GOODSMVT_ITEM-STCK_TYPE,
           SPACE               TO  GOODSMVT_ITEM-SPEC_STOCK,
           EKKO-LIFNR          TO  GOODSMVT_ITEM-VENDOR,
           SPACE               TO  GOODSMVT_ITEM-CUSTOMER,
           SPACE               TO  GOODSMVT_ITEM-SALES_ORD,
           SPACE               TO  GOODSMVT_ITEM-S_ORD_ITEM,
           SPACE               TO  GOODSMVT_ITEM-SCHED_LINE,
           SPACE               TO  GOODSMVT_ITEM-VAL_TYPE,
          IT_ZSIVHSTIT-GRMENGE TO  GOODSMVT_ITEM-ENTRY_QNT,
           EKPO-MEINS       TO  GOODSMVT_ITEM-ENTRY_UOM,
           EKPO-BPRME       TO  GOODSMVT_ITEM-ORDERPR_UN,
           EKPO-EBELN       TO  GOODSMVT_ITEM-PO_NUMBER,
           EKPO-EBELP       TO  GOODSMVT_ITEM-PO_ITEM,
           SPACE            TO  GOODSMVT_ITEM-SHIPPING,
           SPACE            TO  GOODSMVT_ITEM-COMP_SHIP,
           L_NO_MORE_GR     TO  GOODSMVT_ITEM-NO_MORE_GR,
           L_ITEM_TEXT      TO  GOODSMVT_ITEM-ITEM_TEXT,
           SY-UNAME         TO  GOODSMVT_ITEM-GR_RCPT,
           SPACE            TO  GOODSMVT_ITEM-UNLOAD_PT,
           SPACE            TO  GOODSMVT_ITEM-COSTCENTER,
           SPACE            TO  GOODSMVT_ITEM-ORDERID,
           SPACE            TO  GOODSMVT_ITEM-ORDER_ITNO,
           SPACE            TO  GOODSMVT_ITEM-CALC_MOTIVE,
           SPACE            TO  GOODSMVT_ITEM-ASSET_NO,
           SPACE            TO  GOODSMVT_ITEM-SUB_NUMBER,
           SPACE            TO  GOODSMVT_ITEM-RESERV_NO,
           SPACE            TO  GOODSMVT_ITEM-RES_ITEM,
           SPACE            TO  GOODSMVT_ITEM-RES_TYPE,
           SPACE            TO  GOODSMVT_ITEM-WITHDRAWN,
           IT_ZSIVHSTIT-MATNR  TO  GOODSMVT_ITEM-MOVE_MAT,
           IT_ZSIVHSTIT-WERKS  TO  GOODSMVT_ITEM-MOVE_PLANT,
           IT_ZSIVHSTIT-LGORT  TO  GOODSMVT_ITEM-MOVE_STLOC,
           SPACE            TO  GOODSMVT_ITEM-MOVE_BATCH,
           SPACE            TO  GOODSMVT_ITEM-MOVE_VAL_TYPE,
           L_MVT_IND        TO  GOODSMVT_ITEM-MVT_IND,
           P_GRUND          TO  GOODSMVT_ITEM-MOVE_REAS,
           SPACE            TO  GOODSMVT_ITEM-RL_EST_KEY,
           SPACE            TO  GOODSMVT_ITEM-REF_DATE,
           SPACE            TO  GOODSMVT_ITEM-COST_OBJ,
           SPACE            TO  GOODSMVT_ITEM-PROFIT_SEGM_NO,
           SPACE            TO  GOODSMVT_ITEM-PROFIT_CTR,
           SPACE            TO  GOODSMVT_ITEM-WBS_ELEM,
           SPACE            TO  GOODSMVT_ITEM-NETWORK,
           SPACE            TO  GOODSMVT_ITEM-ACTIVITY,
           SPACE            TO  GOODSMVT_ITEM-PART_ACCT,
           SPACE            TO  GOODSMVT_ITEM-REF_DOC_YR,
           SPACE            TO  GOODSMVT_ITEM-REF_DOC,
           SPACE            TO  GOODSMVT_ITEM-REF_DOC_IT,
           SY-DATUM         TO  GOODSMVT_ITEM-EXPIRYDATE,
           SPACE            TO  GOODSMVT_ITEM-PROD_DATE,
           SPACE            TO  GOODSMVT_ITEM-FUND,
           SPACE            TO  GOODSMVT_ITEM-FUNDS_CTR,
           SPACE            TO  GOODSMVT_ITEM-CMMT_ITEM,
           SPACE            TO  GOODSMVT_ITEM-VAL_SALES_ORD,
           SPACE            TO  GOODSMVT_ITEM-VAL_S_ORD_ITEM,
           SPACE            TO  GOODSMVT_ITEM-VAL_WBS_ELEM,
           SPACE            TO  GOODSMVT_ITEM-GL_ACCOUNT,
           SPACE            TO  GOODSMVT_ITEM-IND_PROPOSE_QUANX,
           SPACE            TO  GOODSMVT_ITEM-XSTOB,
           SPACE            TO  GOODSMVT_ITEM-EAN_UPC,
           SPACE            TO  GOODSMVT_ITEM-DELIV_NUMB_TO_SEARCH,
           SPACE            TO  GOODSMVT_ITEM-DELIV_ITEM_TO_SEARCH,
           SPACE  TO  GOODSMVT_ITEM-SERIALNO_AUTO_NUMBERASSIGNMENT,
           IT_ZSIVHSTIT-LICHA TO  GOODSMVT_ITEM-VENDRBATCH,
           SPACE            TO  GOODSMVT_ITEM-STGE_TYPE,
           SPACE            TO  GOODSMVT_ITEM-STGE_BIN,
           SPACE            TO  GOODSMVT_ITEM-SU_PL_STCK_1,
           SPACE            TO  GOODSMVT_ITEM-UNITTYPE_1,
           SPACE            TO  GOODSMVT_ITEM-SU_PL_STCK_2,
           SPACE            TO  GOODSMVT_ITEM-ST_UN_QTYY_2,
           SPACE            TO  GOODSMVT_ITEM-ST_UN_QTYY_2_ISO,
           SPACE            TO  GOODSMVT_ITEM-UNITTYPE_2,
           SPACE            TO  GOODSMVT_ITEM-STGE_TYPE_PC,
           SPACE            TO  GOODSMVT_ITEM-STGE_BIN_PC,
           SPACE            TO  GOODSMVT_ITEM-NO_PST_CHGNT,
           SPACE            TO  GOODSMVT_ITEM-GR_NUMBER,
           SPACE            TO  GOODSMVT_ITEM-STGE_TYPE_ST,
           SPACE            TO  GOODSMVT_ITEM-STGE_BIN_ST,
           SPACE            TO  GOODSMVT_ITEM-MATDOC_TR_CANCEL,
           SPACE            TO  GOODSMVT_ITEM-MATITEM_TR_CANCEL,
           SPACE            TO  GOODSMVT_ITEM-MATITEM_TR_CANCEL,
           SPACE            TO  GOODSMVT_ITEM-MATYEAR_TR_CANCEL,
           SPACE            TO  GOODSMVT_ITEM-NO_TRANSFER_REQ,
           SPACE            TO  GOODSMVT_ITEM-CO_BUSPROC,
           SPACE            TO  GOODSMVT_ITEM-ACTTYPE,
           SPACE            TO  GOODSMVT_ITEM-SUPPL_VEND,
           SPACE            TO  GOODSMVT_ITEM-MATERIAL_EXTERNAL,
           SPACE            TO  GOODSMVT_ITEM-MATERIAL_GUID,
           SPACE            TO  GOODSMVT_ITEM-MATERIAL_VERSION,
           SPACE            TO  GOODSMVT_ITEM-MOVE_MAT_EXTERNAL,
           SPACE            TO  GOODSMVT_ITEM-MOVE_MAT_GUID,
           SPACE            TO  GOODSMVT_ITEM-MOVE_MAT_VERSION.
*>> ITEM APPEND
      APPEND GOODSMVT_ITEM.

      GOODSMVT_SERIALNUMBER-MATDOC_ITM = W_LINE.
      GOODSMVT_SERIALNUMBER-SERIALNO   = W_LINE.
      APPEND   GOODSMVT_SERIALNUMBER.

*> Distribution Quantity TABLE.
      APPEND IT_ZTIVHSTIT.

*> Addition G/R Quantity.
      READ TABLE IT_ZTIVIT WITH KEY ZFIVDNO  = IT_ZTIVHSTIT-ZFIVDNO.
      W_TABIX = SY-TABIX.
      IF SY-SUBRC EQ 0.
         IT_ZTIVIT-GRTOTMN = IT_ZTIVIT-GRTOTMN + IT_ZTIVHSTIT-GRMENGE.
         MODIFY IT_ZTIVIT  INDEX W_TABIX.
      ELSE.
         MESSAGE E643 WITH IT_ZTIVHSTIT-ZFIVNO
                           IT_ZTIVHSTIT-ZFIVDNO
                      RAISING MVT_ERROR.
      ENDIF.

   ENDLOOP.

*  GOODSMVT_HEADER.
   MOVE:P_BUDAT       TO   GOODSMVT_HEADER-PSTNG_DATE,
        P_BLDAT       TO   GOODSMVT_HEADER-DOC_DATE,
        ZTIV-ZFIVNO   TO   GOODSMVT_HEADER-REF_DOC_NO,
*        ZTBL-ZFHBLNO  TO   GOODSMVT_HEADER-BILL_OF_LADING, " temp comt.
        SPACE         TO   GOODSMVT_HEADER-GR_GI_SLIP_NO,
        SY-UNAME      TO   GOODSMVT_HEADER-PR_UNAME,
        'Import G/R'  TO   GOODSMVT_HEADER-HEADER_TXT.

*"----------------------------------------------------------------------
   CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
            GOODSMVT_HEADER        =   GOODSMVT_HEADER
            GOODSMVT_CODE          =   GOODSMVT_CODE
            TESTRUN                =   ' '
        IMPORTING
            GOODSMVT_HEADRET       =   GOODSMVT_HEADRET
            MATERIALDOCUMENT       =   MATERIALDOCUMENT
            MATDOCUMENTYEAR        =   MATDOCUMENTYEAR
        TABLES
           GOODSMVT_ITEM           =   GOODSMVT_ITEM
           GOODSMVT_SERIALNUMBER   =   GOODSMVT_SERIALNUMBER
           RETURN                  =   RETURN.

   CLEAR : ZTIVHST.
   IF RETURN[] IS INITIAL.

      SELECT SINGLE * FROM ZTIV
                      WHERE ZFIVNO EQ P_ZFIVNO.
      MOVE-CORRESPONDING   ZTIV   TO  *ZTIV.

   ELSE.
      RAISE    MVT_ERROR.
   ENDIF.

   IF P_CHG_MODE EQ 'X'.
      IF RETURN[] IS INITIAL.

         MOVE : SY-MANDT           TO     ZTIVHST-MANDT,
                P_ZFIVNO           TO     ZTIVHST-ZFIVNO,
                'Y'                TO     ZTIVHST-ZFGRST,
                P_BLDAT            TO     ZTIVHST-BLDAT,
                P_BUDAT            TO     ZTIVHST-BUDAT,
                P_MVT_TYPE         TO     ZTIVHST-BWART,
                SY-UNAME           TO     ZTIVHST-ERNAM,
                SY-DATUM           TO     ZTIVHST-CDAT,
                SY-UZEIT           TO     ZTIVHST-CTME,
                MATERIALDOCUMENT   TO     ZTIVHST-MBLNR,
                MATDOCUMENTYEAR    TO     ZTIVHST-MJAHR,
                'S'                TO     ZTIVHST-SHKZG.

         SELECT MAX( ZFIVHST ) INTO ZTIVHST-ZFIVHST
                FROM   ZTIVHST
                WHERE  ZFIVNO    EQ    P_ZFIVNO.

         ADD    1                 TO    ZTIVHST-ZFIVHST.
         INSERT   ZTIVHST.
         IF SY-SUBRC NE 0.
            MESSAGE E644   RAISING    MVT_ERROR.
         ENDIF.

         INSERT ZTIVHSTIT FROM TABLE IT_ZTIVHSTIT.
         IF SY-SUBRC NE 0.
            MESSAGE E645   RAISING    MVT_ERROR.
         ENDIF.

         W_ZFGRST = 'Y'.
         LOOP AT IT_ZTIVIT  WHERE UMSON EQ 'X'.
            W_TABIX = SY-TABIX.
            IF IT_ZTIVIT-GRMENGE NE IT_ZTIVIT-GRTOTMN.
               W_ZFGRST = 'P'.  EXIT.
            ENDIF.
         ENDLOOP.
         MODIFY ZTIVIT FROM TABLE IT_ZTIVIT.
         IF SY-SUBRC NE 0.
            MESSAGE E646   RAISING    MVT_ERROR.
         ENDIF.

         MOVE : ZTIVHST-ZFIVHST   TO   ZTIV-ZFIVHST,
                W_ZFGRST          TO   ZTIV-ZFGRST,
                SY-UNAME          TO   ZTIV-UNAM,
                SY-DATUM          TO   ZTIV-UDAT.

         UPDATE  ZTIV.

*--------------------------------------------------------------------
*>> Change Document.
*---------------------------------------------------------------------
         CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_CCHD'
              EXPORTING
                      UPD_CHNGIND    =    'U'
                      N_ZTIV         =    ZTIV
                      O_ZTIV         =    *ZTIV.
*--------------------------------------------------------------------

         IF ZTIV-ZFLGRST EQ 'X'.
            MOVE : SPACE         TO      ZTBL-ZFELIKZ,
                   SY-UNAME      TO      ZTBL-UNAM,
                   SY-DATUM      TO      ZTBL-UDAT.
         ELSE.
            L_CHK = 'Y'.
            LOOP AT IT_ZTBLIT.
               SELECT SUM( GRMENGE ) INTO W_MENGE
                      FROM  ZTIVIT
                      WHERE ZFBLNO   EQ    IT_ZTBLIT-ZFBLNO
                      AND   ZFBLIT   EQ    IT_ZTBLIT-ZFBLIT
                      AND   ZFIVNO   IN
                     ( SELECT ZFIVNO FROM ZTIV
                              WHERE  ZFBLNO  EQ   IT_ZTBLIT-ZFBLNO
                              AND    ZFGRST  EQ   'Y' ).

               IF IT_ZTBLIT-BLMENGE NE W_MENGE.
                  L_CHK = 'N'.
                  EXIT.
               ENDIF.
            ENDLOOP.
            IF L_CHK EQ 'Y'.
               MOVE : SPACE         TO      ZTBL-ZFELIKZ,
                      SY-UNAME      TO      ZTBL-UNAM,
                      SY-DATUM      TO      ZTBL-UDAT.
            ENDIF.
         ENDIF.
      ELSE.
         RAISE    MVT_ERROR.
      ENDIF.
   ELSE.
      IF NOT RETURN[] IS INITIAL.
         RAISE    MVT_ERROR.
      ENDIF.
   ENDIF.
*"----------------------------------------------------------------------

ENDFUNCTION.
