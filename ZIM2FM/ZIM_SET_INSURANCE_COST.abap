FUNCTION ZIM_SET_INSURANCE_COST.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     VALUE(N_ZTINS) LIKE  ZTINS STRUCTURE  ZTINS
*"     VALUE(O_ZTINS) LIKE  ZTINS STRUCTURE  ZTINS
*"     VALUE(N_ZTINSRSP) LIKE  ZTINSRSP STRUCTURE  ZTINSRSP
*"     VALUE(O_ZTINSRSP) LIKE  ZTINSRSP STRUCTURE  ZTINSRSP
*"----------------------------------------------------------------------
   DATA : W_ZFCSQ     LIKE    ZTRECST-ZFCSQ,
*          W_MWSKZ     LIKE    ZTRECST-MWSKZ    VALUE  'V0',
          W_MWSKZ     LIKE    ZTRECST-MWSKZ    VALUE  SPACE,
          W_ZFCSCD    LIKE    ZTRECST-ZFCSCD   VALUE  '1AB',
          W_OK_CODE   LIKE    SY-UCOMM,
          W_STATUS,
          W_ZTIMIMG08 LIKE    ZTIMIMG08,
          L_BLART     LIKE    ZTBKPF-BLART     VALUE  'RE'.

* Import System Basic configuration
  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.   MESSAGE E963.   ENDIF.
* 비용자동 생성이 설정되지 않은 상태.
  IF ZTIMIMG00-ZFATIC NE 'X'.
     MESSAGE S988.    EXIT.
*     MESSAGE I988.    EXIT.
  ENDIF.

* 비용코드 Read.
  SELECT SINGLE * FROM ZTREQHD
         WHERE ZFREQNO EQ N_ZTINS-ZFREQNO.

  SELECT SINGLE * FROM ZTIMIMG11
         WHERE    BUKRS  EQ   ZTREQHD-BUKRS.
  IF SY-SUBRC NE 0.
     MESSAGE E987 WITH ZTREQHD-BUKRS..
  ENDIF.

*>> 손보사 IMG READ.
  SELECT SINGLE * FROM ZTINSSG5
                  WHERE ZFREQNO EQ N_ZTINS-ZFREQNO
                  AND   ZFLSG5  EQ '00010'.
  IF SY-SUBRC EQ 0.
     SELECT SINGLE * FROM ZTIMIMG08
                     WHERE ZFCDTY EQ '010'
                     AND   ZFCD   EQ ZTINSSG5-ZFINSC.
     IF SY-SUBRC NE 0.   MESSAGE E980.   ENDIF.
  ENDIF.

*> 비용코드.
  SELECT SINGLE * INTO W_ZTIMIMG08 FROM ZTIMIMG08
                  WHERE ZFCDTY EQ '003'
                  AND   ZFCD   EQ W_ZFCSCD.
  IF SY-SUBRC NE 0.
     MESSAGE E430 WITH '003' W_ZFCSCD.
  ENDIF.

* 수입의뢰 Header
   CLEAR : ZTREQHD.
   SELECT SINGLE * FROM ZTREQHD
          WHERE   ZFREQNO   EQ    N_ZTINS-ZFREQNO.

*   IF N_ZTINSRSP-ZFVPRC EQ 'KRW'.
*      N_ZTINS-ZFKRWAMT = N_ZTINS-ZFKRWAMT - N_ZTINSRSP-ZFVPR.
*   ENDIF.

*-----------------------------------------------------------------------
*>>> 2000/05/20 KSB INSERT
* DESC : CHARGE DOCUMENT 사용시.
*-----------------------------------------------------------------------
   IF ZTIMIMG00-ZFPSMS EQ '2'.
      CLEAR : ZTBKPF, *ZTBKPF, IT_ZSBSEG_OLD, IT_ZSBSEG,
              IT_ZSBDIV, IT_ZSBHIS.
      REFRESH : IT_ZSBDIV, IT_ZSBHIS, IT_ZSBSEG_OLD, IT_ZSBSEG.

      IF UPD_CHNGIND EQ 'U' OR UPD_CHNGIND EQ 'I' OR UPD_CHNGIND EQ 'D'.
         SELECT * FROM ZTBSEG UP TO 1 ROWS
                  WHERE ZFIMDNO   EQ ZTREQHD-ZFREQNO
                  AND   ZFINSEQ   EQ N_ZTINS-ZFINSEQ
                  AND   ZFCSTGRP  EQ '003'
                  AND   ZFCD      EQ W_ZFCSCD
                  ORDER BY BUKRS DESCENDING GJAHR DESCENDING
                           BELNR DESCENDING.
         ENDSELECT.
         W_SUBRC = SY-SUBRC.
         IF SY-SUBRC EQ 0.

            CALL FUNCTION 'ZIM_GET_CHARGE_DOCUMENT'
                 EXPORTING
                    BUKRS           =    ZTBSEG-BUKRS
                    BELNR           =    ZTBSEG-BELNR
                    GJAHR           =    ZTBSEG-GJAHR
                 IMPORTING
                    W_ZTBKPF        =    ZTBKPF
                 TABLES
                    IT_ZSBSEG       =    IT_ZSBSEG
                    IT_ZSBSEG_OLD   =    IT_ZSBSEG_OLD
                    IT_ZSBDIV       =    IT_ZSBDIV
                    IT_ZSBHIS       =    IT_ZSBHIS
                EXCEPTIONS
                    NOT_FOUND               =    4
                    COMPANDYCODE_NOT_INPUT  =    6
                    DOCUMENT_NO_NOT_INPUT   =    8
                    FISC_YEAR_NOT_INPUT     =   10.

            MOVE-CORRESPONDING ZTBKPF TO *ZTBKPF.
            REFRESH : IT_ZSBSEG.
            IF ZTBKPF-ZFPOSYN EQ 'Y'.
               MESSAGE I606 WITH ZTBKPF-GJAHR ZTBKPF-BELNR.
               EXIT.
            ENDIF.
         ELSE.
            MOVE : ZTIMIMG08-ZFCD5      TO  ZTBKPF-LIFNR,
                   ZTIMIMG08-ZFCD5      TO  ZTBKPF-ZFVEN,
                   ZTREQHD-BUKRS        TO  ZTBKPF-BUKRS.
         ENDIF.
      ENDIF.

      CALL FUNCTION 'COMPANY_CODE_READ'
           EXPORTING
                I_BUKRS = ZTREQHD-BUKRS
           IMPORTING
                E_T001  = T001
                E_T004  = T004
                E_T005  = T005
                E_T014  = T014
                E_T043  = T043
                E_T043T = T043T.

      MOVE : N_ZTINS-ZFINSDT      TO  ZTBKPF-BUDAT,
             L_BLART              TO  ZTBKPF-BLART.
      IF NOT N_ZTINSRSP-ZFVPR IS INITIAL.
         MOVE:   'SA'             TO  ZTBKPF-BLART.
      ENDIF.

      PERFORM PERIODE_ERMITTELN(SAPMZIM02) USING ZTBKPF-BUDAT
                                                 ZTBKPF-GJAHR
                                                 ZTBKPF-MONAT.
*> 문서 종류 체크.
      PERFORM BELEGART_PRUEFEN(SAPFF001)
                              USING ZTBKPF-BLART ZTBKPF-GJAHR.
*> 전기년도 체크.
      BKPF-BUKRS = ZTBKPF-BUKRS.

*>
      ZTBKPF-LIFNR = N_ZTINS-ZFOPCD.
      CALL FUNCTION 'FI_VENDOR_DATA'
           EXPORTING
              i_bukrs = ZTBKPF-BUKRS
              i_lifnr = ZTBKPF-LIFNR
           IMPORTING
              e_kred  = vf_kred.

      SELECT MIN( BRANCH ) INTO ZTBKPF-BUPLA
                                FROM J_1BBRANCH
                           WHERE BUKRS EQ T001-BUKRS.


      CASE UPD_CHNGIND.
         WHEN 'I' OR 'U'.    ">삽입 / 갱신.
* 보험료가 없는 경우 EXIT.
            IF N_ZTINS-ZFINAMT  IS INITIAL AND
               N_ZTINS-ZFKRWAMT IS INITIAL AND
               N_ZTINSRSP-ZFVPR IS INITIAL.
               EXIT.
            ENDIF.

            MOVE : ZTREQHD-BUKRS        TO  ZTBKPF-BUKRS,
                   '003'                TO  ZTBKPF-ZFCSTGRP,
                   N_ZTINS-ZFINSDT+4(2) TO  ZTBKPF-MONAT,
                   N_ZTINS-ZFINSDT      TO  ZTBKPF-BLDAT,
                   'X'                  TO  ZTBKPF-ZFPCUR,
                   '31'                 TO IT_ZSBSEG-NEWBS,
                   N_ZTINS-ZFOPCD       TO  ZTBKPF-LIFNR,
                   N_ZTINS-ZFOPCD       TO  ZTBKPF-ZFVEN,
                   vf_kred-AKONT        TO  ZTBKPF-AKONT,
                   VF_KRED-ZTERM        TO  ZTBKPF-ZTERM,
                   N_ZTINS-ZFINAMTC     TO  ZTBKPF-WAERS,
                   N_ZTINSRSP-ZFEXRT    TO  ZTBKPF-KURSF,
                   N_ZTINS-ZFINSDT      TO  ZTBKPF-WWERT,
                   N_ZTINS-ZFINAMT      TO  ZTBKPF-WRBTR,
                   T001-WAERS           TO  ZTBKPF-HWAER,
                   N_ZTINS-ZFKRWAMT     TO  ZTBKPF-DMBTR,
                   W_MWSKZ              TO  ZTBKPF-MWSKZ,
                   0                    TO  ZTBKPF-WMWST,
                   SPACE                TO  ZTBKPF-XMWST,
                   ZTBKPF-BLDAT         TO  ZTBKPF-ZFBDT,
                   SPACE                TO  ZTBKPF-ZLSPR,
                   SPACE                TO  ZTBKPF-ZLSCH,
                   ZTREQHD-ZFREQNO      TO  ZTBKPF-ZFIMDNO,
                   '적하부보 보험료'    TO  ZTBKPF-BKTXT,
                   SPACE                TO  ZTBKPF-XBLNR.

            IF W_ZTIMIMG08-COND_TYPE IS INITIAL.
               CLEAR : ZTBKPF-ZFDCSTX.
            ELSE.
               SELECT SINGLE * FROM  EKKO
                               WHERE EBELN  EQ   ZTREQHD-EBELN.
               IF SY-SUBRC EQ 0.
                  SELECT * FROM  ZTREQIT UP TO 1 ROWS
                           WHERE ZFREQNO EQ ZTREQHD-ZFREQNO.

                  ENDSELECT.
                  IF SY-SUBRC EQ 0.
                     CLEAR : KONV.
                     SELECT * FROM KONV UP TO 1 ROWS
                              WHERE KNUMV   EQ   EKKO-KNUMV
                              AND   KPOSN   EQ   ZTREQIT-EBELP
                              AND   KSCHL   EQ   W_ZTIMIMG08-COND_TYPE.
*                              AND   KNTYP   EQ     'B'.
                     ENDSELECT.
                     IF SY-SUBRC EQ 0 AND KONV-KNTYP EQ 'B'.
                        ZTBKPF-ZFDCSTX = 'X'.
                     ELSE.
                        CLEAR : ZTBKPF-ZFDCSTX.
                     ENDIF.
                  ELSE.
                     CLEAR : ZTBKPF-ZFDCSTX.
                  ENDIF.
               ELSE.
                  CLEAR : ZTBKPF-ZFDCSTX.
               ENDIF.
            ENDIF.
            IF ZTBKPF-ZFDCSTX NE 'X'.
               CLEAR : W_ZTIMIMG08-COND_TYPE.
            ENDIF.

            IF NOT N_ZTINSRSP-ZFVPR IS INITIAL.
               MOVE :'적하부보(VP 금액포함)' TO ZTBKPF-BKTXT.
*                     '15100300'                     TO ZTBKPF-AKONT.
            ENDIF.

            MOVE : '003'                  TO IT_ZSBSEG-ZFCSTGRP,
                   W_ZFCSCD               TO IT_ZSBSEG-ZFCD,
                   ZTREQHD-ZFREQNO        TO IT_ZSBSEG-ZFIMDNO,
                   ZTBKPF-ZFDCSTX         TO IT_ZSBSEG-ZFDCSTX,
                   W_ZTIMIMG08-COND_TYPE  TO IT_ZSBSEG-COND_TYPE,
                   W_MWSKZ                TO IT_ZSBSEG-MWSKZ,
                   '40'                   TO IT_ZSBSEG-NEWBS,
                   ZTIMIMG11-ZFIOCAC1     TO IT_ZSBSEG-NEWKO,
                   'S'                    TO IT_ZSBSEG-SHKZG,
                   N_ZTINS-ZFINAMT        TO IT_ZSBSEG-WRBTR,
                   0                      TO IT_ZSBSEG-WMWST,
                   N_ZTINS-ZFKRWAMT       TO IT_ZSBSEG-DMBTR,
                   0                      TO IT_ZSBSEG-FWBAS,
                   N_ZTINSRSP-ZFVPR       TO IT_ZSBSEG-ZFVPR,
                   N_ZTINSRSP-ZFEXRT      TO IT_ZSBSEG-KURSF,
                   N_ZTINS-ZFINSDT        TO IT_ZSBSEG-WWERT,
                   ZTREQHD-ZFOPNNO        TO IT_ZSBSEG-ZUONR,
                   'Insurance'            TO IT_ZSBSEG-SGTXT,
                   N_ZTINS-ZFINSEQ        TO IT_ZSBSEG-ZFINSEQ,
                   N_ZTINS-ZFDOCNO        TO IT_ZSBSEG-ZFDOCNO.

            IF NOT N_ZTINSRSP-ZFVPR IS INITIAL.
*               MOVE: '90204000'         TO IT_ZSBSEG-NEWKO,
               MOVE: 'Insurance(VP)'    TO IT_ZSBSEG-SGTXT.
            ENDIF.
            APPEND IT_ZSBSEG.

            IF W_SUBRC EQ 0.
               W_STATUS  = 'U'.
            ELSE.
               W_STATUS  = 'C'.
            ENDIF.
            W_OK_CODE = 'SAVE'.
         WHEN 'D'.    ">삭제.
            IF W_SUBRC NE 0.
               EXIT.
            ENDIF.

            W_OK_CODE = 'DELE'.
            W_STATUS  = 'D'.
         WHEN OTHERS.
            EXIT.
      ENDCASE.

      CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
                 EXPORTING
                     W_OK_CODE           =   W_OK_CODE
                     BUKRS               =   ZTBKPF-BUKRS
                     GJAHR               =   ZTBKPF-GJAHR
                     ZFSTATUS            =   W_STATUS
                     W_ZTBKPF_OLD        =  *ZTBKPF
                     W_ZTBKPF            =   ZTBKPF
                TABLES
                     IT_ZSBSEG_OLD       =   IT_ZSBSEG_OLD
                     IT_ZSBSEG           =   IT_ZSBSEG
                     IT_ZSBDIV           =   IT_ZSBDIV
                     IT_ZSBHIS           =   IT_ZSBHIS
                CHANGING
                     BELNR               =   ZTBKPF-BELNR
                EXCEPTIONS
                    ERROR_UPDATE.
      IF SY-SUBRC EQ 0.
         COMMIT WORK.
      ELSE.
         ROLLBACK WORK.
      ENDIF.
      EXIT.
   ENDIF.

   CHECK : ZTIMIMG00-ZFPSMS EQ '1'.

*-----------------------------------------------------------------------

* 적하보험에 해당 금?
   REFRESH : IT_ZTRECST.
   SELECT * INTO TABLE IT_ZTRECST FROM ZTRECST
            WHERE   ZFREQNO   EQ    N_ZTINS-ZFREQNO
            AND     ZFCSCD    EQ    W_ZFCSCD " INSURANCE EDI CODE
            AND     ZFINSEQ   EQ    N_ZTINS-ZFINSEQ
            AND     ZFACDO    EQ    SPACE.   " 회계전표번?

   MOVE : SY-SUBRC   TO   W_SY_SUBRC.

   CASE UPD_CHNGIND.
      WHEN  'I'.    " 삽?
* 보험료가 없는 경우 EXIT.
         IF N_ZTINS-ZFINAMT  IS INITIAL AND
            N_ZTINS-ZFKRWAMT IS INITIAL AND
            N_ZTINSRSP-ZFVPR IS INITIAL.
            EXIT.
         ENDIF.

         IF W_SY_SUBRC EQ 0.           " DATA가 있을 경우...
            READ TABLE IT_ZTRECST INDEX 1.
* 이전 ?
            MOVE-CORRESPONDING  IT_ZTRECST  TO   *ZTRECST.
* 이후 ?
            MOVE-CORRESPONDING  IT_ZTRECST  TO   ZTRECST.

            ZTRECST-ZFCAMT  = ZTRECST-ZFCAMT  + N_ZTINS-ZFINAMT.
            ZTRECST-ZFCKAMT = ZTRECST-ZFCKAMT + N_ZTINS-ZFKRWAMT.
            ZTRECST-ZFVPR   = ZTRECST-ZFVPR   + N_ZTINSRSP-ZFVPR.

            IF ZTRECST-ZFWERKS IS INITIAL.   " PLANT
               MOVE : ZTREQHD-ZFWERKS   TO   ZTRECST-ZFWERKS.
            ENDIF.
            IF ZTRECST-ZTERM   IS INITIAL.   " Terms of Payment
               MOVE : ZTREQHD-ZTERM     TO   ZTRECST-ZTERM.
            ENDIF.
            IF ZTRECST-MWSKZ   IS INITIAL.   " Tax CODE
               MOVE : W_MWSKZ           TO   ZTRECST-MWSKZ.
            ENDIF.
            IF ZTRECST-ZFVEN   IS INITIAL.   " Vendor Code
               MOVE : ZTIMIMG08-ZFCD5  TO   ZTRECST-ZFVEN.
            ENDIF.
            IF ZTRECST-ZFPAY   IS INITIAL.   " 지불?
               MOVE : ZTIMIMG08-ZFCD5  TO   ZTRECST-ZFPAY.
            ENDIF.
            MOVE : SY-UNAME          TO     ZTRECST-UNAM,
                   SY-DATUM          TO     ZTRECST-UDAT,
                   ZTREQHD-BUKRS     TO     ZTRECST-BUKRS.

            UPDATE  ZTRECST.
* CNAHNGE DOCUMENT
            PERFORM   P3000_ZTRECST_CHANGE_DOC  USING  'U'
                                                    *ZTRECST
                                                     ZTRECST.

         ELSE.               " DATA가 없을 경?
            SELECT MAX( ZFCSQ ) INTO W_ZFCSQ  FROM ZTRECST
                                WHERE ZFREQNO EQ   N_ZTINS-ZFREQNO.

            IF W_ZFCSQ IS INITIAL.   W_ZFCSQ = '00010'.
            ELSE.                    W_ZFCSQ = W_ZFCSQ + 10.
            ENDIF.

            CLEAR : ZTRECST, *ZTRECST.

            MOVE : SY-MANDT          TO     ZTRECST-MANDT,
                   N_ZTINS-ZFREQNO   TO     ZTRECST-ZFREQNO,
                   ZTREQHD-BUKRS     TO     ZTRECST-BUKRS,
                   W_ZFCSQ           TO     ZTRECST-ZFCSQ,
                   W_ZFCSCD          TO     ZTRECST-ZFCSCD,
                   N_ZTINS-ZFINAMT   TO     ZTRECST-ZFCAMT,
                   N_ZTINS-ZFINAMTC  TO     ZTRECST-WAERS,
                   N_ZTINS-ZFKRWAMT  TO     ZTRECST-ZFCKAMT,
                   N_ZTINSRSP-ZFVPR  TO     ZTRECST-ZFVPR,
                   'KRW'             TO     ZTRECST-ZFKRW,
                   N_ZTINSRSP-ZFEXRT TO     ZTRECST-ZFEXRT,
                   N_ZTINS-ZFINSEQ   TO     ZTRECST-ZFINSEQ,
*                  0                 TO     ZTRECST-ZFVAT,
*                                    TO     ZTRECST-ZFOCDT,
*                                    TO     ZTRECST-ZFPSDT,
*                                    TO     ZTRECST-ZFFIYR,
*                                    TO     ZTRECST-ZFACDO,
                   ZTIMIMG08-ZFCD5   TO     ZTRECST-ZFVEN,
                   ZTIMIMG08-ZFCD5   TO     ZTRECST-ZFPAY,
                   SY-UNAME          TO     ZTRECST-ERNAM,
                   SY-DATUM          TO     ZTRECST-CDAT,
                   SY-UNAME          TO     ZTRECST-UNAM,
                   SY-DATUM          TO     ZTRECST-UDAT,
                   ZTREQHD-ZTERM     TO     ZTRECST-ZTERM,
                   ZTREQHD-ZFWERKS   TO     ZTRECST-ZFWERKS,
                   W_MWSKZ           TO     ZTRECST-MWSKZ,
                   N_ZTINS-ZFDOCNO   TO     ZTRECST-ZFDOCNO.

            INSERT ZTRECST.
* CNAHNGE DOCUMENT
            PERFORM   P3000_ZTRECST_CHANGE_DOC  USING  'I'
                                                    *ZTRECST
                                                     ZTRECST.
         ENDIF.

      WHEN  'U'.    " 갱?
* 보험료가 이전/이후 변경이 없는 경우 EXIT.
         IF O_ZTINS-ZFINAMT  EQ N_ZTINS-ZFINAMT  AND
            O_ZTINS-ZFKRWAMT EQ N_ZTINS-ZFKRWAMT AND
            O_ZTINSRSP-ZFVPR EQ N_ZTINSRSP-ZFVPR.
            EXIT.
         ENDIF.

         IF W_SY_SUBRC EQ 0.           " DATA가 있을 경우...
            READ TABLE IT_ZTRECST INDEX 1.
* 이전 ?
            MOVE-CORRESPONDING  IT_ZTRECST  TO   *ZTRECST.
* 이후 ?
            MOVE-CORRESPONDING  IT_ZTRECST  TO   ZTRECST.

            ZTRECST-ZFCAMT  = ZTRECST-ZFCAMT
                            - O_ZTINS-ZFINAMT  + N_ZTINS-ZFINAMT.
            ZTRECST-ZFCKAMT = ZTRECST-ZFCKAMT
                            - O_ZTINS-ZFKRWAMT + N_ZTINS-ZFKRWAMT.
            ZTRECST-ZFVPR   = ZTRECST-ZFVPR
                            - O_ZTINSRSP-ZFVPR + N_ZTINSRSP-ZFVPR.

            IF ZTRECST-ZFWERKS IS INITIAL.   " PLANT
               MOVE : ZTREQHD-ZFWERKS   TO   ZTRECST-ZFWERKS.
            ENDIF.
            IF ZTRECST-ZTERM   IS INITIAL.   " Terms of Payment
               MOVE : ZTREQHD-ZTERM     TO   ZTRECST-ZTERM.
            ENDIF.
            IF ZTRECST-MWSKZ   IS INITIAL.   " Tax CODE
               MOVE : W_MWSKZ           TO   ZTRECST-MWSKZ.
            ENDIF.
            IF ZTRECST-ZFVEN   IS INITIAL.   " Vendor Code
               MOVE : ZTIMIMG08-ZFCD5  TO   ZTRECST-ZFVEN.
            ENDIF.
            IF ZTRECST-ZFPAY   IS INITIAL.   " 지불?
               MOVE : ZTIMIMG08-ZFCD5  TO   ZTRECST-ZFPAY.
            ENDIF.
            MOVE  ZTREQHD-BUKRS     TO     ZTRECST-BUKRS.
            UPDATE  ZTRECST.
* CNAHNGE DOCUMENT
        PERFORM   P3000_ZTRECST_CHANGE_DOC  USING  'U'
                                                    *ZTRECST
                                                     ZTRECST.

         ELSE.                         " DATA가 없을 경?
            SELECT MAX( ZFCSQ ) INTO W_ZFCSQ  FROM ZTRECST
                                WHERE ZFREQNO EQ   N_ZTINS-ZFREQNO.

            IF W_ZFCSQ IS INITIAL.   W_ZFCSQ = '00010'.
            ELSE.                    W_ZFCSQ = W_ZFCSQ + 10.
            ENDIF.

            CLEAR : ZTRECST, *ZTRECST.
            ZTRECST-ZFCAMT       =     N_ZTINS-ZFINAMT
                                 -     O_ZTINS-ZFINAMT.
            ZTRECST-ZFCKAMT      =     N_ZTINS-ZFKRWAMT
                                 -     O_ZTINS-ZFKRWAMT.
            ZTRECST-ZFVPR        =     N_ZTINSRSP-ZFVPR
                                 -     O_ZTINSRSP-ZFVPR.

            MOVE : SY-MANDT          TO     ZTRECST-MANDT,
                   N_ZTINS-ZFREQNO   TO     ZTRECST-ZFREQNO,
                   W_ZFCSQ           TO     ZTRECST-ZFCSQ,
                   W_ZFCSCD          TO     ZTRECST-ZFCSCD,
                   N_ZTINS-ZFINAMTC  TO     ZTRECST-WAERS,
                   'KRW'             TO     ZTRECST-ZFKRW,
                   N_ZTINSRSP-ZFEXRT TO     ZTRECST-ZFEXRT,
                   N_ZTINS-ZFINSEQ   TO     ZTRECST-ZFINSEQ,
                   ZTREQHD-BUKRS     TO     ZTRECST-BUKRS,
*                  0                 TO     ZTRECST-ZFVAT,
*                                    TO     ZTRECST-ZFOCDT,
*                                    TO     ZTRECST-ZFPSDT,
*                                    TO     ZTRECST-ZFFIYR,
*                                    TO     ZTRECST-ZFACDO,
                   ZTIMIMG08-ZFCD5   TO     ZTRECST-ZFVEN,
                   ZTIMIMG08-ZFCD5   TO     ZTRECST-ZFPAY,
                   SY-UNAME          TO     ZTRECST-ERNAM,
                   SY-DATUM          TO     ZTRECST-CDAT,
                   SY-UNAME          TO     ZTRECST-UNAM,
                   SY-DATUM          TO     ZTRECST-UDAT,
                   ZTREQHD-ZTERM     TO     ZTRECST-ZTERM,
                   ZTREQHD-ZFWERKS   TO     ZTRECST-ZFWERKS,
                   W_MWSKZ           TO     ZTRECST-MWSKZ,
                   N_ZTINS-ZFDOCNO   TO     ZTRECST-ZFDOCNO.

            INSERT ZTRECST.
* CNAHNGE DOCUMENT
            PERFORM   P3000_ZTRECST_CHANGE_DOC  USING  'I'
                                                    *ZTRECST
                                                     ZTRECST.

         ENDIF.

      WHEN  'D'.    " 삭?
* 보험료가 이전/이후 변경이 없는 경우 EXIT.
         IF O_ZTINS-ZFINAMT  EQ N_ZTINS-ZFINAMT   AND
            O_ZTINS-ZFKRWAMT EQ N_ZTINS-ZFKRWAMT  AND
            O_ZTINSRSP-ZFVPR EQ N_ZTINSRSP-ZFVPR.
            EXIT.
         ENDIF.

         IF W_SY_SUBRC EQ 0.           " DATA가 있을 경우...
            READ TABLE IT_ZTRECST INDEX 1.
* 이전 ?
            MOVE-CORRESPONDING  IT_ZTRECST  TO   *ZTRECST.
* 이후 ?
            MOVE-CORRESPONDING  IT_ZTRECST  TO   ZTRECST.

            ZTRECST-ZFCAMT  = ZTRECST-ZFCAMT  - N_ZTINS-ZFINAMT
                                              + O_ZTINS-ZFINAMT.
            ZTRECST-ZFCKAMT = ZTRECST-ZFCKAMT - N_ZTINS-ZFKRWAMT
                                              + O_ZTINS-ZFKRWAMT.
            ZTRECST-ZFVPR   = ZTRECST-ZFVPR   - N_ZTINSRSP-ZFVPR
                                              + O_ZTINSRSP-ZFVPR.

            IF ZTRECST-ZFCAMT  IS INITIAL AND
               ZTRECST-ZFCKAMT IS INITIAL AND
               ZTRECST-ZFVPR   IS INITIAL.
               DELETE  ZTRECST.
* CNAHNGE DOCUMENT
               PERFORM   P3000_ZTRECST_CHANGE_DOC  USING  'D'
                                                    *ZTRECST
                                                     ZTRECST.
            ELSE.
               IF ZTRECST-ZFWERKS IS INITIAL.   " PLANT
                  MOVE : ZTREQHD-ZFWERKS   TO   ZTRECST-ZFWERKS.
               ENDIF.
               IF ZTRECST-ZTERM   IS INITIAL.   " Terms of Payment
                  MOVE : ZTREQHD-ZTERM     TO   ZTRECST-ZTERM.
               ENDIF.
               IF ZTRECST-MWSKZ   IS INITIAL.   " Tax CODE
                  MOVE : W_MWSKZ           TO   ZTRECST-MWSKZ.
               ENDIF.
               IF ZTRECST-ZFVEN   IS INITIAL.   " Vendor Code
                  MOVE : ZTIMIMG08-ZFCD5  TO   ZTRECST-ZFVEN.
               ENDIF.
               IF ZTRECST-ZFPAY   IS INITIAL.   " 지불?
                  MOVE : ZTIMIMG08-ZFCD5  TO   ZTRECST-ZFPAY.
               ENDIF.

               UPDATE  ZTRECST.
* CNAHNGE DOCUMENT
               PERFORM   P3000_ZTRECST_CHANGE_DOC  USING  'U'
                                                    *ZTRECST
                                                     ZTRECST.

            ENDIF.
         ELSE.                         " DATA가 없을 경?
            SELECT MAX( ZFCSQ ) INTO W_ZFCSQ  FROM ZTRECST
                                WHERE ZFREQNO EQ   N_ZTINS-ZFREQNO.

            IF W_ZFCSQ IS INITIAL.   W_ZFCSQ = '00010'.
            ELSE.                    W_ZFCSQ = W_ZFCSQ + 10.
            ENDIF.

            CLEAR : ZTRECST, *ZTRECST.
            ZTRECST-ZFCAMT       =      O_ZTINS-ZFINAMT  * -1.
            ZTRECST-ZFCKAMT      =      O_ZTINS-ZFKRWAMT * -1.
            ZTRECST-ZFVPR        =      O_ZTINSRSP-ZFVPR * -1.

            MOVE : SY-MANDT          TO     ZTRECST-MANDT,
                   ZTINS-ZFREQNO     TO     ZTRECST-ZFREQNO,
                   W_ZFCSQ           TO     ZTRECST-ZFCSQ,
                   W_ZFCSCD          TO     ZTRECST-ZFCSCD,
*                   ZTINS-ZFINAMT    TO     ZTRECST-ZFCAMT,
                   O_ZTINS-ZFINAMTC  TO     ZTRECST-WAERS,
*                   ZTINS-ZFKRWAMT   TO     ZTRECST-ZFCKAMT,
                   'KRW'             TO     ZTRECST-ZFKRW,
                   O_ZTINSRSP-ZFEXRT TO     ZTRECST-ZFEXRT,
*                  0                 TO     ZTRECST-ZFVAT,
*                                    TO     ZTRECST-ZFOCDT,
*                                    TO     ZTRECST-ZFPSDT,
*                                    TO     ZTRECST-ZFFIYR,
*                                    TO     ZTRECST-ZFACDO,
                   ZTIMIMG08-ZFCD5   TO     ZTRECST-ZFVEN,
                   ZTIMIMG08-ZFCD5   TO     ZTRECST-ZFPAY,
                   SY-UNAME          TO     ZTRECST-ERNAM,
                   SY-DATUM          TO     ZTRECST-CDAT,
                   SY-UNAME          TO     ZTRECST-UNAM,
                   SY-DATUM          TO     ZTRECST-UDAT,
                   ZTREQHD-ZTERM     TO     ZTRECST-ZTERM,
                   ZTREQHD-ZFWERKS   TO     ZTRECST-ZFWERKS,
                   W_MWSKZ           TO     ZTRECST-MWSKZ.
*                   ZTINS-ZFDOCNO     TO     ZTRECST-ZFDOCNO.

            INSERT ZTRECST.
* CNAHNGE DOCUMENT
            PERFORM   P3000_ZTRECST_CHANGE_DOC  USING  'I'
                                                    *ZTRECST
                                                     ZTRECST.
         ENDIF.
      WHEN OTHERS.
         EXIT.
   ENDCASE.

ENDFUNCTION.
