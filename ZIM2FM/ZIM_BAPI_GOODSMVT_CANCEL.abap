FUNCTION ZIM_BAPI_GOODSMVT_CANCEL .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_ZFIVNO) LIKE  ZTIV-ZFIVNO
*"     VALUE(P_ZFIVHST) LIKE  ZTIVHST-ZFIVHST OPTIONAL
*"     VALUE(MATERIALDOCUMENT) TYPE  BAPI2017_GM_HEAD_02-MAT_DOC
*"     VALUE(MATDOCUMENTYEAR) TYPE  BAPI2017_GM_HEAD_02-DOC_YEAR
*"     VALUE(GOODSMVT_PSTNG_DATE) TYPE  BAPI2017_GM_HEAD_02-PSTNG_DATE
*"       OPTIONAL
*"     VALUE(GOODSMVT_PR_UNAME) LIKE  BAPI2017_GM_HEAD_01-PR_UNAME
*"       OPTIONAL
*"  EXPORTING
*"     VALUE(GOODSMVT_HEADRET) TYPE  BAPI2017_GM_HEAD_RET
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2
*"      GOODSMVT_MATDOCITEM STRUCTURE  BAPI2017_GM_ITEM_04 OPTIONAL
*"  EXCEPTIONS
*"      MVT_ERROR
*"----------------------------------------------------------------------
DATA : IT_ZTIVHST  LIKE ZTIVHST OCCURS 0 WITH HEADER LINE.
DATA : W_TEXT70(70),
       W_ZFGRST     LIKE ZTIV-ZFGRST.

DATA : L_MVT_IND     LIKE   GOODSMVT_ITEM-MVT_IND,
       L_NO_MORE_GR  LIKE   GOODSMVT_ITEM-NO_MORE_GR.

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

*>>> 입고 상태 체크.
   CASE ZTIV-ZFGRST.
      WHEN 'X' OR 'N'.     ">미대상/입고대상.(오류)
         PERFORM   GET_DD07T_SELECT(SAPMZIM01)
                   USING      'ZDGRST'  ZTIV-ZFGRST
                   CHANGING    W_TEXT70.
         MESSAGE E422 WITH P_ZFIVNO W_TEXT70 'G/R Cancelation'
                           RAISING MVT_ERROR.
      WHEN 'Y'.     ">입고완료.(NONE)
   ENDCASE.

*>> 문서번호.
   IF MATERIALDOCUMENT IS INITIAL.
      MESSAGE E167 WITH 'Material Document No'   RAISING    MVT_ERROR.
   ENDIF.

*>> 전기년도.
   IF MATDOCUMENTYEAR IS INITIAL.
      MESSAGE E167 WITH 'Material Document Year'  RAISING    MVT_ERROR.
   ENDIF.

*>> B/L DOCUMENT.
   SELECT SINGLE * FROM ZTBL
                   WHERE ZFBLNO   EQ   ZTIV-ZFBLNO.
*>
   SELECT * INTO TABLE IT_ZTIVHST
            FROM ZTIVHST
            WHERE ZFIVNO   EQ  P_ZFIVNO.

*> IMG SETTING BASIC TABLE.
   SELECT SINGLE * FROM ZTIMIMG00.

   IF ZTIMIMG00-GRPARTX NE 'X' OR
      ZTIV-ZFREQTY EQ 'LO'     OR
      ZTIV-ZFREQTY EQ 'PU'.

*>
      SELECT MAX( ZFIVHST ) INTO W_ZFIVHST
             FROM  ZTIVHST
             WHERE ZFIVNO   EQ  P_ZFIVNO
             AND ( CMBLNR   IS  NULL
             OR    CMBLNR   EQ  SPACE ).

      IF W_ZFIVHST IS INITIAL.
         MESSAGE E427   RAISING  MVT_ERROR.
      ENDIF.

   ELSE.
      W_ZFIVHST = P_ZFIVHST.
   ENDIF.

   READ TABLE IT_ZTIVHST WITH KEY ZFIVHST = W_ZFIVHST.
   IF NOT ( IT_ZTIVHST-MBLNR EQ MATERIALDOCUMENT AND
            IT_ZTIVHST-MJAHR EQ MATDOCUMENTYEAR ).
      MESSAGE E428    RAISING  MVT_ERROR.
   ENDIF.

*> 자재내역 SELECT..
   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTIVIT
            FROM  ZTIVIT
            WHERE ZFIVNO EQ P_ZFIVNO
            AND   UMSON  EQ 'X'.
   IF SY-SUBRC NE 0.
*      MESSAGE E649 WITH ZTIV-ZFIVNO RAISING  MVT_ERROR.
      MESSAGE E649 RAISING  MVT_ERROR.
   ENDIF.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTIVHSTIT
            FROM  ZTIVHSTIT
            WHERE ZFIVNO  EQ P_ZFIVNO
            AND   ZFIVHST EQ W_ZFIVHST.

*"----------------------------------------------------------------------
   CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
        EXPORTING
            MATERIALDOCUMENT       =   MATERIALDOCUMENT
            MATDOCUMENTYEAR        =   MATDOCUMENTYEAR
            GOODSMVT_PSTNG_DATE    =   GOODSMVT_PSTNG_DATE
            GOODSMVT_PR_UNAME      =   GOODSMVT_PR_UNAME
        IMPORTING
            GOODSMVT_HEADRET       =   GOODSMVT_HEADRET
        TABLES
           GOODSMVT_MATDOCITEM     =   GOODSMVT_MATDOCITEM
           RETURN                  =   RETURN.

*-----------------------------------------------------------------------
   CLEAR : ZTIVHST.
   IF RETURN[] IS INITIAL AND NOT MATERIALDOCUMENT IS INITIAL.
*      COMMIT WORK.
*>> 입고 상태 변경.
      SELECT SINGLE * FROM ZTIV
                      WHERE ZFIVNO EQ P_ZFIVNO.
      MOVE-CORRESPONDING  ZTIV TO     *ZTIV.

      LOOP AT IT_ZTIVHSTIT.
         W_TABIX = SY-TABIX.
         IT_ZTIVHSTIT-ZFGRST = 'N'.
         MODIFY IT_ZTIVHSTIT INDEX W_TABIX.

         READ TABLE IT_ZTIVIT WITH KEY ZFIVNO  = IT_ZTIVHSTIT-ZFIVNO
                                       ZFIVDNO = IT_ZTIVHSTIT-ZFIVDNO.
         W_TABIX = SY-TABIX.
         IF SY-SUBRC EQ 0.
            IF IT_ZTIVIT-GRTOTMN NE 0.
               IT_ZTIVIT-GRTOTMN = IT_ZTIVIT-GRTOTMN
                                 - IT_ZTIVHSTIT-GRMENGE.
               MODIFY IT_ZTIVIT INDEX W_TABIX.
            ENDIF.
         ENDIF.
      ENDLOOP.

      W_ZFGRST = 'N'.
      LOOP AT IT_ZTIVIT.
         IF IT_ZTIVIT-GRMENGE NE IT_ZTIVIT-GRTOTMN AND
            IT_ZTIVIT-GRTOTMN NE 0.
            W_ZFGRST = 'P'.  EXIT.
         ENDIF.
      ENDLOOP.

      MOVE : W_ZFGRST          TO      ZTIV-ZFGRST,
             SY-DATUM          TO      ZTIV-UDAT,
             SY-UNAME          TO      ZTIV-UNAM.
      UPDATE  ZTIV.

      IF SY-SUBRC NE 0.
         RAISE   MVT_ERROR.
      ENDIF.

*>> 변경문서.
      CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_CCHD'
              EXPORTING
                      UPD_CHNGIND    =    'U'
                      N_ZTIV         =    ZTIV
                      O_ZTIV         =    *ZTIV.
*>> 이력관리.
      SELECT  * FROM ZTIVHST UP TO 1 ROWS
                WHERE ZFIVNO   EQ P_ZFIVNO
                AND    MBLNR   EQ MATERIALDOCUMENT
                AND    MJAHR   EQ MATDOCUMENTYEAR.
      ENDSELECT.

      MOVE : GOODSMVT_HEADRET-MAT_DOC      TO  ZTIVHST-CMBLNR,
             GOODSMVT_HEADRET-DOC_YEAR     TO  ZTIVHST-CMJAHR,
             GOODSMVT_PSTNG_DATE           TO  ZTIVHST-CBUDAT,
             'N'                           TO  ZTIVHST-ZFGRST,
             SY-UNAME                      TO  ZTIVHST-UNAM,
             SY-DATUM                      TO  ZTIVHST-UDAT,
             SY-UZEIT                      TO  ZTIVHST-UTME.

      UPDATE ZTIVHST.
      IF SY-SUBRC NE 0.
         MESSAGE E644   RAISING    MVT_ERROR.
      ENDIF.

      MODIFY ZTIVIT FROM TABLE IT_ZTIVIT.
      IF SY-SUBRC NE 0.
         MESSAGE E646   RAISING    MVT_ERROR.
      ENDIF.

      MODIFY ZTIVHSTIT FROM TABLE IT_ZTIVHSTIT.
      IF SY-SUBRC NE 0.
         MESSAGE E645   RAISING    MVT_ERROR.
      ENDIF.
*>> B/L UPDATE.
      IF ZTBL-ZFELIKZ EQ 'X'.
         MOVE : SPACE         TO      ZTBL-ZFELIKZ,
                SY-UNAME      TO      ZTBL-UNAM,
                SY-DATUM      TO      ZTBL-UDAT.
         UPDATE  ZTBL.
      ENDIF.
   ELSE.
*      ROLLBACK WORK.
      RAISE   MVT_ERROR.
   ENDIF.

*"----------------------------------------------------------------------


ENDFUNCTION.
