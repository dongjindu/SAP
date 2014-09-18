*&---------------------------------------------------------------------
*& INCLUDE ZRIM00F02 .
*&---------------------------------------------------------------------
*&  프로그램명 : 수입의뢰 Main Amend SUB MODULE Include
*&      작성자 : 강석봉 INFOLINK Ltd.
*&      작성일 : 2000.05.26
*&  적용회사PJT:
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------


*&---------------------------------------------------------------------*
*&      Form  P2000_INS_AMEND_CHECK
*&---------------------------------------------------------------------*
FORM P2000_INS_AMEND_CHECK.
* 문서 상?
  CASE ZTINS-ZFDOCST.
     WHEN 'N'.          " NONE 상?
        MESSAGE E999 WITH ZTINS-ZFREQNO 'Not opened' 'Amend'.
     WHEN 'R'.          " 의?
        IF ZTINS-ZFEDIST = 'S'.
           MESSAGE E999 WITH ZTINS-ZFREQNO 'EDI opened' 'Amend'.
        ELSEIF ZTINS-ZFEDIST = 'N'.
           MESSAGE E999 WITH ZTINS-ZFREQNO 'Flat Data created' 'Amend'.
        ENDIF.
     WHEN 'C'.          " CANCEL
        MESSAGE E999 WITH ZTINS-ZFREQNO 'CALCELED' 'Amend'.
     WHEN 'A'.          " AMEND
        MESSAGE E999 WITH ZTINS-ZFREQNO 'Amended' 'Amend'.
  ENDCASE.

ENDFORM.                    " P2000_INS_AMEND_CHECK

*&---------------------------------------------------------------------*
*&      Form  GET_ORIJIN_NAME
*&---------------------------------------------------------------------*
FORM GET_ORIJIN_NAME USING    P_CODE
                     CHANGING P_NAME.
DATA: L_TEXT(15).

  CLEAR : T005T.
  SELECT SINGLE * FROM T005T WHERE   SPRAS EQ 'E'
                             AND     LAND1 EQ P_CODE.

*  TRANSLATE T005T-LANDX TO UPPER CASE.
  MOVE : T005T-LANDX     TO   L_TEXT.   " 국가?
  TRANSLATE L_TEXT  TO UPPER CASE.
  L_TEXT = P_NAME.
ENDFORM.                    " GET_ORIJIN_NAME

*&---------------------------------------------------------------------*
*&      Form  P2000_TRANS_METHOD_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_TRANS_METHOD_SET.
DATA : L_ZFLAGR   LIKE   ZTINSAGR-ZFLAGR.

* 보험구분.

  CASE ZTINS-ZFTRANS.
     WHEN 'A' OR 'B'.
        MOVE: 'A'                   TO ZTINSSG3-ZFCARNU, " 항?
             'PER REGULAR AIRLINER' TO ZTINSSG3-ZFCARNM. " 선기?
     WHEN 'O'.
        MOVE:'T.B.D'                TO ZTINSSG3-ZFCARNU, " 항?
             'TO BE DECLARED'       TO ZTINSSG3-ZFCARNM. " 선기?
  ENDCASE.


*------------------------------------------------------------------
* LG화재 자동 INTERFACE로 변경.
*------------------------------------------------------------------
  SELECT SINGLE * FROM EKKO
         WHERE    EBELN   EQ   ZTREQHD-EBELN.

*  IF SY-SUBRC EQ 0.
*-------------------------------------------------------------------
*> dreamland remark.
*-------------------------------------------------------------------
*> 보험 조건.
*     MOVE : EKKO-ZFBSINS    TO ZTINS-ZFBSINS.
*> 부가존건.
*     CLEAR : L_ZFLAGR.
*     REFRESH : IT_ZSINSAGR.
*
*     IF NOT EKKO-ZFINSCD1 IS INITIAL.
*        L_ZFLAGR = L_ZFLAGR + 10.
*        IT_ZSINSAGR-ZFLAGR   = L_ZFLAGR.
*        IT_ZSINSAGR-ZFINSCD  = EKKO-ZFINSCD1.
*        PERFORM  GET_DD07T_SELECT USING 'ZDINSCD' IT_ZSINSAGR-ZFINSCD
*                                  CHANGING  IT_ZSINSAGR-ZFCNCDNM.
*        APPEND IT_ZSINSAGR.
*     ENDIF.
*
*     IF NOT EKKO-ZFINSCD2 IS INITIAL.
*        L_ZFLAGR = L_ZFLAGR + 10.
*        IT_ZSINSAGR-ZFLAGR   = L_ZFLAGR.
*        IT_ZSINSAGR-ZFINSCD  = EKKO-ZFINSCD2.
*        PERFORM  GET_DD07T_SELECT USING 'ZDINSCD' IT_ZSINSAGR-ZFINSCD
*                                  CHANGING  IT_ZSINSAGR-ZFCNCDNM.
*        APPEND IT_ZSINSAGR.
*     ENDIF.
*
*     IF NOT EKKO-ZFINSCD3 IS INITIAL.
*        L_ZFLAGR = L_ZFLAGR + 10.
*        IT_ZSINSAGR-ZFLAGR   = L_ZFLAGR.
*        IT_ZSINSAGR-ZFINSCD  = EKKO-ZFINSCD3.
*        PERFORM  GET_DD07T_SELECT USING 'ZDINSCD' IT_ZSINSAGR-ZFINSCD
*                                  CHANGING  IT_ZSINSAGR-ZFCNCDNM.
*        APPEND IT_ZSINSAGR.
*     ENDIF.
*
*     IF NOT EKKO-ZFINSCD4 IS INITIAL.
*        L_ZFLAGR = L_ZFLAGR + 10.
*        IT_ZSINSAGR-ZFLAGR   = L_ZFLAGR.
*        IT_ZSINSAGR-ZFINSCD  = EKKO-ZFINSCD4.
*        PERFORM  GET_DD07T_SELECT USING 'ZDINSCD' IT_ZSINSAGR-ZFINSCD
*                                  CHANGING  IT_ZSINSAGR-ZFCNCDNM.
*        APPEND IT_ZSINSAGR.
*     ENDIF.
*
*     IF NOT EKKO-ZFINSCD5 IS INITIAL.
*        L_ZFLAGR = L_ZFLAGR + 10.
*        IT_ZSINSAGR-ZFLAGR   = L_ZFLAGR.
*        IT_ZSINSAGR-ZFINSCD  = EKKO-ZFINSCD5.
*        PERFORM  GET_DD07T_SELECT USING 'ZDINSCD' IT_ZSINSAGR-ZFINSCD
*                                  CHANGING  IT_ZSINSAGR-ZFCNCDNM.
*        APPEND IT_ZSINSAGR.
*     ENDIF.
*  ENDIF.

*------------------------------------------------------------------
* LG화재 자동 INTERFACE로 변경으로 막음.
*------------------------------------------------------------------
* 기본조건 명?
*  REFRESH : IT_ZSINSAGR.
*  CASE ZTINS-ZFTRANS.
*     WHEN 'A'.
*        IT_ZSINSAGR-ZFBCNYN  = 'X'.
*        IT_ZSINSAGR-ZFLAGR   = '00010'.
*        IT_ZSINSAGR-ZFCNCD   = '025'.
*        PERFORM  GET_DD07T_SELECT USING 'ZDCNCD' IT_ZSINSAGR-ZFCNCD
*                                  CHANGING  IT_ZSINSAGR-ZFCNCDNM.
*        APPEND IT_ZSINSAGR.
*     WHEN 'O'.
*        IT_ZSINSAGR-ZFBCNYN  = 'X'.
*        IT_ZSINSAGR-ZFLAGR   = '00010'.
*        IT_ZSINSAGR-ZFCNCD   = '012'.
*        PERFORM  GET_DD07T_SELECT USING 'ZDCNCD' IT_ZSINSAGR-ZFCNCD
*                                  CHANGING  IT_ZSINSAGR-ZFCNCDNM.
*        APPEND IT_ZSINSAGR.
*     WHEN 'B'.
*        IT_ZSINSAGR-ZFBCNYN  = 'X'.
*        IT_ZSINSAGR-ZFLAGR   = '00010'.
*        IT_ZSINSAGR-ZFCNCD   = '012'.
*        PERFORM  GET_DD07T_SELECT USING 'ZDCNCD' IT_ZSINSAGR-ZFCNCD
*                                  CHANGING  IT_ZSINSAGR-ZFCNCDNM.
*        APPEND IT_ZSINSAGR.
*        IT_ZSINSAGR-ZFBCNYN  = 'X'.
*        IT_ZSINSAGR-ZFLAGR   = '00020'.
*        IT_ZSINSAGR-ZFCNCD   = '025'.
*        PERFORM  GET_DD07T_SELECT USING 'ZDCNCD' IT_ZSINSAGR-ZFCNCD
*                                  CHANGING  IT_ZSINSAGR-ZFCNCDNM.
*        APPEND IT_ZSINSAGR.
*  ENDCASE.

** HEI 기존 시스템 로직 반?
*  IF  ZTINS-ZFRSTAW(5) EQ '21701'.
*      IT_ZSINSAGR-ZFBCNYN  = 'X'.
*      IT_ZSINSAGR-ZFCNCD   = 'B09'.
*
*      IF ZTINS-ZFTRANS EQ 'B'.
*         IT_ZSINSAGR-ZFLAGR   = '00030'.
*      ELSE.
*         IT_ZSINSAGR-ZFLAGR   = '00020'.
*      ENDIF.
*      PERFORM  GET_DD07T_SELECT USING 'ZDCNCD' IT_ZSINSAGR-ZFCNCD
*                                CHANGING  IT_ZSINSAGR-ZFCNCDNM.
*      APPEND IT_ZSINSAGR.
*  ENDIF.

ENDFORM.                    " P2000_TRANS_METHOD_SET

*&---------------------------------------------------------------------*
*&      Form  P1000_AMEND_LIST_MAKE
*&---------------------------------------------------------------------*
FORM P1000_AMEND_LIST_MAKE.

DATA : L_FIELD_NAME       LIKE   DD03L-FIELDNAME,
       L_BEFORE_NAME(255) TYPE C,
       L_AFTER_NAME(255)  TYPE C,
       L_REF_NAME(255)    TYPE C,
       L_LEN              TYPE I,
       L_LEN1             TYPE I,
       L_STRING(65535),
       L_WAERS            LIKE ZTREQHD-WAERS.

   REFRESH : IT_ZSAMDLIST.

   SELECT * INTO TABLE IT_DD03L FROM  DD03L
                                WHERE TABNAME EQ 'ZTREQHD'.

   LOOP AT IT_DD03L.
      CONCATENATE 'ZTREQHD_TMP-' IT_DD03L-FIELDNAME
                                 INTO L_BEFORE_NAME.
      CONCATENATE 'ZTREQHD-' IT_DD03L-FIELDNAME
                                 INTO L_AFTER_NAME.
      ASSIGN (L_BEFORE_NAME)   TO    <FS_F>.
      ASSIGN (L_AFTER_NAME)    TO    <FS_F2>.
      IF <FS_F> NE <FS_F2>.
         CLEAR : IT_ZSAMDLIST.
* DATA ELEMENT NAME GET
         PERFORM P1000_GET_DATA_ELEMENT_NAME
                       USING    IT_DD03L-ROLLNAME
                                IT_ZSAMDLIST-SCRTEXT_M.

         CASE IT_DD03L-INTTYPE.
            WHEN 'D'.
               WRITE : <FS_F>        TO     IT_ZSAMDLIST-ZFBEFORE,
                       <FS_F2>       TO     IT_ZSAMDLIST-ZFAFTER.

            WHEN 'P'.
               IF IT_DD03L-REFTABLE IS INITIAL.
                  WRITE : <FS_F>     TO     IT_ZSAMDLIST-ZFBEFORE,
                          <FS_F2>    TO     IT_ZSAMDLIST-ZFAFTER.
               ELSE.
                  CONCATENATE IT_DD03L-REFTABLE '_TMP-'
                              IT_DD03L-REFFIELD INTO L_REF_NAME.
                  ASSIGN (L_REF_NAME)   TO    <FS_REF>.
                  WRITE : <FS_F>   TO  IT_ZSAMDLIST-ZFBEFORE
                                       CURRENCY <FS_REF>.
                  CONCATENATE IT_DD03L-REFTABLE '-' IT_DD03L-REFFIELD
                                         INTO L_REF_NAME.
                  ASSIGN (L_REF_NAME)   TO    <FS_REF>.
                  WRITE : <FS_F2>   TO  IT_ZSAMDLIST-ZFAFTER
                                        CURRENCY <FS_REF>.
               ENDIF.
               CONDENSE         IT_ZSAMDLIST-ZFBEFORE NO-GAPS.
               CONDENSE         IT_ZSAMDLIST-ZFAFTER  NO-GAPS.
            WHEN OTHERS.
               MOVE : <FS_F>         TO     IT_ZSAMDLIST-ZFBEFORE,
                      <FS_F2>        TO     IT_ZSAMDLIST-ZFAFTER.
         ENDCASE.
         MOVE L_AFTER_NAME TO IT_ZSAMDLIST-ZFFIELD.
         APPEND  IT_ZSAMDLIST.
      ENDIF.
   ENDLOOP.

* ORGIN 변경 검?
   CLEAR : IT_ZSAMDLIST.
   DESCRIBE TABLE IT_ZTREQORJ     LINES  W_COUNTER.
   DESCRIBE TABLE IT_ZTREQORJ_TMP LINES  W_COUNTER1.

   IF W_COUNTER NE W_COUNTER1.
      PERFORM  P3000_AMDLIST_ORIGIN_APPEND.
   ELSE.
      LOOP AT IT_ZTREQORJ_TMP.
         READ TABLE IT_ZTREQORJ WITH KEY
                                ZFORIG = IT_ZTREQORJ_TMP-ZFORIG.
         IF SY-SUBRC NE 0.
            PERFORM  P3000_AMDLIST_ORIGIN_APPEND.  EXIT.
         ELSE.
            IF IT_ZTREQORJ_TMP-ZFORIG NE IT_ZTREQORJ-ZFORIG.
               PERFORM  P3000_AMDLIST_ORIGIN_APPEND.  EXIT.
            ENDIF.
         ENDIF.
      ENDLOOP.
   ENDIF.
* 수입의뢰 품?
   L_STRING = ZTREQIT.
   L_LEN  = STRLEN( L_STRING ).
   L_LEN  = L_LEN  - 13.
   L_STRING = ZTREQIT_TMP.
   L_LEN1 = STRLEN( L_STRING ).
   L_LEN1 = L_LEN1  - 18.

   LOOP AT IT_ZSREQIT_TMP.
      READ TABLE IT_ZSREQIT WITH KEY ZFITMNO = IT_ZSREQIT_TMP-ZFITMNO
                                                BINARY SEARCH.
      IF SY-SUBRC EQ 0.
         IF IT_ZSREQIT-MENGE GT 0.
            IF IT_ZSREQIT_TMP+18(L_LEN1) NE IT_ZSREQIT+14(L_LEN).
               IF IT_ZSREQIT_TMP-STAWN NE IT_ZSREQIT-STAWN OR
                  IT_ZSREQIT_TMP-MENGE NE IT_ZSREQIT-MENGE OR
                  IT_ZSREQIT_TMP-TXZ01 NE IT_ZSREQIT-TXZ01 OR
                  IT_ZSREQIT_TMP-NETPR NE IT_ZSREQIT-NETPR OR
                  IT_ZSREQIT_TMP-PEINH NE IT_ZSREQIT-PEINH OR
                  IT_ZSREQIT_TMP-BPRME NE IT_ZSREQIT-BPRME OR
                  IT_ZSREQIT_TMP-MEINS NE IT_ZSREQIT-MEINS.

                  CONCATENATE '품목 Item 변경:'
                              IT_ZSREQIT_TMP-ZFITMNO
                              INTO IT_ZSAMDLIST-SCRTEXT_M.
                  WRITE : IT_ZSREQIT_TMP-MENGE TO W_TEXT13
                          UNIT IT_ZSREQIT_TMP-MEINS.
                  CONCATENATE IT_ZSREQIT_TMP-MATNR
                              W_TEXT13
                              INTO IT_ZSAMDLIST-ZFBEFORE
                              SEPARATED BY SPACE.
                  WRITE : IT_ZSREQIT-MENGE TO W_TEXT13
                          UNIT IT_ZSREQIT-MEINS.
                  CONCATENATE IT_ZSREQIT-MATNR
                              W_TEXT13
                              INTO IT_ZSAMDLIST-ZFAFTER
                              SEPARATED BY SPACE.
                  MOVE 'ZTREQIT'   TO    IT_ZSAMDLIST-ZFFIELD.
                  APPEND  IT_ZSAMDLIST.
               ENDIF.
            ENDIF.
         ELSE.

         ENDIF.
      ELSE.
         CONCATENATE '품목 Item 삭제:'
                     IT_ZSREQIT_TMP-ZFITMNO
                     INTO IT_ZSAMDLIST-SCRTEXT_M.

         WRITE : IT_ZSREQIT_TMP-MENGE TO W_TEXT13
                 UNIT IT_ZSREQIT_TMP-MEINS.
         CONCATENATE IT_ZSREQIT_TMP-MATNR IT_ZSREQIT_TMP-STAWN
                     W_TEXT13
                     INTO IT_ZSAMDLIST-ZFBEFORE
                     SEPARATED BY SPACE.

         CLEAR : IT_ZSAMDLIST-ZFAFTER.
         MOVE 'ZTREQIT'   TO    IT_ZSAMDLIST-ZFFIELD.
         APPEND  IT_ZSAMDLIST.
      ENDIF.
   ENDLOOP.

   LOOP AT IT_ZSREQIT WHERE MENGE > 0.
      READ TABLE IT_ZSREQIT_TMP WITH KEY
                            ZFITMNO = IT_ZSREQIT-ZFITMNO
                                      BINARY SEARCH.

      IF SY-SUBRC NE 0.
         CONCATENATE '품목 Item 삽입:'
                     IT_ZSREQIT-ZFITMNO
                     INTO IT_ZSAMDLIST-SCRTEXT_M.

         WRITE : IT_ZSREQIT-MENGE TO W_TEXT13 UNIT IT_ZSREQIT-MEINS.
         CONCATENATE IT_ZSREQIT-MATNR IT_ZSREQIT-STAWN
                     W_TEXT13
                     INTO IT_ZSAMDLIST-ZFAFTER
                     SEPARATED BY SPACE.

         CLEAR : IT_ZSAMDLIST-ZFBEFORE.
         MOVE 'ZTREQIT'   TO    IT_ZSAMDLIST-ZFFIELD.
         APPEND  IT_ZSAMDLIST.
      ENDIF.
   ENDLOOP.

ENDFORM.                    " P1000_AMEND_LIST_MAKE

*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR1199  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR1199 INPUT.
* W_OK_CODE = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'TXTG'.
       IF ZTREQHD-ZFREQTY NE 'LC'.
          MESSAGE E253 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
       ENDIF.
* 기존에 생성된 text 삭?
       DELETE IT_ZSMLCAMNARR WHERE ZFFIELD NE SPACE.
* 새로운 Amend List 생?
       LOOP AT IT_ZSAMDLIST
               WHERE ZFFIELD = 'ZTREQHD-INCO1'
               OR    ZFFIELD = 'ZTREQHD-ZZPSHIP'
               OR    ZFFIELD = 'ZTREQHD-ZZTSHIP'
               OR    ZFFIELD = 'ZTREQHD-ZFTRANS'
               OR    ZFFIELD = 'ZTREQHD-ZFCHG'
               OR    ZFFIELD = 'ZTREQORJ-ZFORIG'.

          CASE IT_ZSAMDLIST-ZFFIELD.
             WHEN 'ZTREQHD-INCO1'.
                IT_ZSMLCAMNARR-ZFNARR = 'Incoterms : Before '.
                PERFORM  P2000_TEXT_AMEND_MOVE.
             WHEN 'ZTREQHD-ZZPSHIP'.
                IT_ZSMLCAMNARR-ZFNARR = 'Partial Shipment : Before '.
                PERFORM  P2000_ALLOWED_AMEND_MOVE.
                PERFORM  P2000_TEXT_AMEND_MOVE.
             WHEN 'ZTREQHD-ZZTSHIP'.
                IT_ZSMLCAMNARR-ZFNARR = 'Transhipment : Before '.
                PERFORM  P2000_ALLOWED_AMEND_MOVE.
                PERFORM  P2000_TEXT_AMEND_MOVE.
             WHEN 'ZTREQHD-ZFTRANS'.   " Shipping Mode
                IT_ZSMLCAMNARR-ZFNARR = 'Shipping Mode : Before '.
                CASE IT_ZSAMDLIST-ZFBEFORE.
                   WHEN 'A'.     IT_ZSAMDLIST-ZFBEFORE = 'Air'.
                   WHEN 'O'.     IT_ZSAMDLIST-ZFBEFORE = 'Ocean'.
                   WHEN 'B'.     IT_ZSAMDLIST-ZFBEFORE = 'Air or Ocean'.
                   WHEN OTHERS.  IT_ZSAMDLIST-ZFBEFORE = 'None'.
                ENDCASE.
                CASE IT_ZSAMDLIST-ZFAFTER.
                   WHEN 'A'.     IT_ZSAMDLIST-ZFAFTER = 'Air'.
                   WHEN 'O'.     IT_ZSAMDLIST-ZFAFTER = 'Ocean'.
                   WHEN 'B'.     IT_ZSAMDLIST-ZFAFTER = 'Air or Ocean'.
                   WHEN OTHERS.  IT_ZSAMDLIST-ZFAFTER = 'None'.
                ENDCASE.
                PERFORM  P2000_TEXT_AMEND_MOVE.
             WHEN 'ZTREQHD-ZFCHG'.     " Banking charge
                IT_ZSMLCAMNARR-ZFNARR = 'Banking charge : Before '.
                CASE IT_ZSAMDLIST-ZFBEFORE.
                   WHEN 'S'.     IT_ZSAMDLIST-ZFBEFORE = 'Seller'.
                   WHEN 'B'.     IT_ZSAMDLIST-ZFBEFORE = 'Buyer'.
                   WHEN OTHERS.  IT_ZSAMDLIST-ZFBEFORE = 'None'.
                ENDCASE.
                CASE IT_ZSAMDLIST-ZFAFTER.
                   WHEN 'S'.     IT_ZSAMDLIST-ZFAFTER = 'Seller'.
                   WHEN 'B'.     IT_ZSAMDLIST-ZFAFTER = 'Buyer'.
                   WHEN OTHERS.  IT_ZSAMDLIST-ZFAFTER = 'None'.
                ENDCASE.
                PERFORM  P2000_TEXT_AMEND_MOVE.
             WHEN 'ZTREQORJ-ZFORIG'.   " Origin Code
                IT_ZSMLCAMNARR-ZFNARR = 'Origin : Before '.
                PERFORM  P2000_TEXT_AMEND_MOVE.
             WHEN OTHERS.
                CONTINUE.
          ENDCASE.
*         SORT IT_ZSMLCAMNARR  BY  ZFLNARR.
          PERFORM   P2000_IT_ZSMLCAMNARR_UPDATE.
       ENDLOOP.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.   EXIT.
  ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR1199  INPUT

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_DATA_ELEMENT_NAME
*&---------------------------------------------------------------------*
FORM P1000_GET_DATA_ELEMENT_NAME USING    P_ROLLNAME
                                          P_SCRTEXT_M.

         SELECT SINGLE  SCRTEXT_M INTO P_SCRTEXT_M
                                FROM  DD04T
                                WHERE ROLLNAME   EQ P_ROLLNAME
                                AND   DDLANGUAGE EQ SY-LANGU
                                AND   AS4LOCAL   EQ 'A'
                                AND   AS4VERS    EQ '0000'.

ENDFORM.                    " P1000_GET_DATA_ELEMENT_NAME

*&---------------------------------------------------------------------*
*&      Form  P3000_AMDLIST_ORIGIN_APPEND
*&---------------------------------------------------------------------*
FORM P3000_AMDLIST_ORIGIN_APPEND.
  DATA : L_LEN     TYPE   I.

* DATA ELEMENT NAME GET( 원산지 국가코드 )
      PERFORM P1000_GET_DATA_ELEMENT_NAME
                       USING    'HERKL'
                                IT_ZSAMDLIST-SCRTEXT_M.
* DATA MOVE
      L_LEN = 0.
      LOOP AT IT_ZTREQORJ_TMP.
         IT_ZSAMDLIST-ZFBEFORE+L_LEN(3) = IT_ZTREQORJ_TMP-ZFORIG.
         L_LEN = L_LEN + 3.
      ENDLOOP.
* DATA MOVE
      L_LEN = 0.
      LOOP AT IT_ZTREQORJ.
         IT_ZSAMDLIST-ZFAFTER+L_LEN(3) = IT_ZTREQORJ-ZFORIG.
         L_LEN = L_LEN + 3.
      ENDLOOP.
      MOVE 'ZTREQORJ-ZFORIG' TO IT_ZSAMDLIST-ZFFIELD.
      APPEND IT_ZSAMDLIST.

ENDFORM.                    " P3000_AMDLIST_ORIGIN_APPEND

*&---------------------------------------------------------------------*
*&      Form  P2000_TEXT_AMEND_MOVE
*&---------------------------------------------------------------------*
FORM P2000_TEXT_AMEND_MOVE.

   CONCATENATE IT_ZSMLCAMNARR-ZFNARR
               IT_ZSAMDLIST-ZFBEFORE
               ' After '
               IT_ZSAMDLIST-ZFAFTER
               INTO IT_ZSMLCAMNARR-ZFNARR
   SEPARATED BY SPACE.

   IT_ZSMLCAMNARR-ZFLNARR = SY-TABIX.
   IT_ZSMLCAMNARR-ZFFIELD = IT_ZSAMDLIST-ZFFIELD.
   APPEND IT_ZSMLCAMNARR.

ENDFORM.                    " P2000_TEXT_AMEND_MOVE

*&---------------------------------------------------------------------*
*&      Form  P2000_ALLOWED_AMEND_MOVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_ALLOWED_AMEND_MOVE.

   IF IT_ZSAMDLIST-ZFBEFORE EQ 'X'.
      IT_ZSAMDLIST-ZFBEFORE = 'ALLOWED'.
   ELSE.
      IT_ZSAMDLIST-ZFBEFORE = 'PROHIBITED'.
   ENDIF.
   IF IT_ZSAMDLIST-ZFAFTER EQ 'X'.
      IT_ZSAMDLIST-ZFAFTER = 'ALLOWED'.
   ELSE.
      IT_ZSAMDLIST-ZFAFTER = 'PROHIBITED'.
   ENDIF.

ENDFORM.                    " P2000_ALLOWED_AMEND_MOVE
*&---------------------------------------------------------------------*
*&      Module  GET_INSU_NAME_4103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_INSU_NAME_4103 INPUT.

   CLEAR  W_ZFOPCD1.
   MOVE : ZTINS-ZFOPCD TO W_TEXT12.

   CALL FUNCTION 'AIA6_ALPHA_OUTPUT_C12_TO_C24'
        EXPORTING
             I_INTERN = W_TEXT12
        IMPORTING
             E_EXTERN = W_TEXT24.

   W_ZFOPCD1 = W_TEXT24.
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*       EXPORTING
*           INPUT      = ZTINS-ZFOPCD
*       IMPORTING
*           OUPUT      = W_ZFOPCD1.

   SELECT SINGLE  ZFCD4   INTO  ZTINS-ZFEDI
   FROM   ZTIMIMG08
   WHERE  ZFCDTY  EQ  '010'
   AND    ZFCD5   EQ  W_ZFOPCD1.

   SELECT  SINGLE NAME1  NAME2
   INTO    (ZTINS-ZFINSU1, ZTINS-ZFINSU2)
   FROM    LFA1
   WHERE   LIFNR     EQ  ZTINS-ZFOPCD.

ENDMODULE.                 " GET_INSU_NAME_4103  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_DOC_CHANGE_OPEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_DOC_CHANGE_OPEN.

  SPOP-TITEL = 'L/C COST 내역 상태 변경 및 조회'.
  OPTION = 1.

  CALL SCREEN 0192 STARTING AT 5  6
                   ENDING   AT 85 18.

  IF ANTWORT EQ 'C' OR ANTWORT EQ 'N'.       " Cancel Or No
     SET SCREEN SY-DYNNR.
  ENDIF.

ENDFORM.                    " P2000_DOC_CHANGE_OPEN
*&---------------------------------------------------------------------*
*&      Form  P2000_FI_DOC_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSRECST_ZFACDO  text
*      -->P_IT_ZSRECST_BUKRS  text
*      -->P_IT_ZSRECST_ZFFIYR  text
*----------------------------------------------------------------------*
FORM P2000_FI_DOC_DISPLAY USING    P_BELNR
                                   P_BUKRS
                                   P_GJAHR.
  IF P_BELNR IS INITIAL.
     MESSAGE S814.
     EXIT.
  ENDIF.
  IF P_BUKRS IS INITIAL.
     MESSAGE S167 WITH '회사코드'.
     EXIT.
  ENDIF.
  IF P_GJAHR IS INITIAL.
     MESSAGE S167 WITH '회계년도'.
     EXIT.
  ENDIF.

  SET PARAMETER ID 'BLN' FIELD P_BELNR.
  SET PARAMETER ID 'BUK' FIELD P_BUKRS.
  SET PARAMETER ID 'GJR' FIELD P_GJAHR.

  CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.


ENDFORM.                    " P2000_FI_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_INS_AMT_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_INS_AMT_CHECK.
  CHECK  W_STATUS EQ C_OPEN_C.

  W_AMOUNT = ZTINSRSP-ZFCAMI + ZTINSRSP-ZFDAMI.

  IF ZTINSRSP-ZFTAMI NE W_AMOUNT.
     PERFORM  P2000_NO_INPUT(SAPMZIM01) USING 'ZTINSRSP' 'ZFTAMI'
                                              dfies-scrtext_m W_SUBRC.
     MESSAGE  E521.
  ENDIF.

  CLEAR  W_AMOUNT.
  W_AMOUNT = ZTINSRSP-ZFCPR + ZTINSRSP-ZFDPR + ZTINSRSP-ZFVPR +
             ZTINSRSP-ZFIPR.

  IF ZTINSRSP-ZFTPR NE W_AMOUNT.
     PERFORM  P2000_NO_INPUT(SAPMZIM01) USING 'ZTINSRSP' 'ZFTPR'
                                               dfies-scrtext_m W_SUBRC.
     MESSAGE  E521.
  ENDIF.

ENDFORM.                    " P2000_INS_AMT_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_INS_AMD_DISP_SELECT3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_INS_AMD_DISP_SELECT3.
  W_ZFOPNNO = ZSREQHD-ZFOPNNO.
  W_ZFREQNO = ZSREQHD-ZFREQNO.
  W_EBELN   = ZSREQHD-EBELN.
  W_ZFAMDNO = ZSREQHD-ZFAMDNO.
  W_ZFINSEQ = ZSREQHD-ZFINSEQ.

  REFRESH IT_ZSREQHD.
* Table Multi-Select
*   IF ZSREQHD-ZFINSEQ IS INITIAL.    " 일련번?
  IF ZSREQHD-ZFAMDNO IS INITIAL. " Amend Seq.
    SELECT  I~ZFREQNO I~ZFAMDNO R~ZFOPNNO I~ZFOPNO
            I~ZFINAMT I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST
            I~ZFINSEQ
      INTO (IT_ZSREQHD-ZFREQNO,
            IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
            IT_ZSREQHD-ZFOPNO,  IT_ZSREQHD-ZFINAMT,
            IT_ZSREQHD-WAERS,   IT_ZSREQHD-EBELN,
            IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST,
            IT_ZSREQHD-ZFINSEQ)
       FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
             ON R~ZFREQNO  EQ I~ZFREQNO
       WHERE R~ZFREQNO   EQ ZSREQHD-ZFREQNO
       AND   R~ZFAMDNO   EQ '00000'
       AND   I~ZFAMDNO   NE '00000'.
      APPEND IT_ZSREQHD.
    ENDSELECT.
  ELSE.
    SELECT  I~ZFREQNO I~ZFAMDNO R~ZFOPNNO I~ZFOPNO
            I~ZFINAMT I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST
            I~ZFINSEQ
      INTO (IT_ZSREQHD-ZFREQNO,
            IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
            IT_ZSREQHD-ZFOPNO,  IT_ZSREQHD-ZFINAMT,
            IT_ZSREQHD-WAERS,   IT_ZSREQHD-EBELN,
            IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST,
            IT_ZSREQHD-ZFINSEQ)
       FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
             ON R~ZFREQNO  EQ I~ZFREQNO
       WHERE R~ZFREQNO EQ ZSREQHD-ZFREQNO
       AND   R~ZFAMDNO EQ '00000'
       AND   I~ZFAMDNO EQ ZSREQHD-ZFAMDNO.
      APPEND IT_ZSREQHD.
    ENDSELECT.
  ENDIF.
*   ELSE.
*      IF ZSREQHD-ZFAMDNO IS INITIAL. " Amend Seq.
*         SELECT  I~ZFREQNO I~ZFINSEQ I~ZFAMDNO R~ZFOPNNO I~ZFOPNO
*                 I~ZFINAMT I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST
*              INTO (IT_ZSREQHD-ZFREQNO, IT_ZSREQHD-ZFINSEQ,
*                    IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
*                    IT_ZSREQHD-ZFOPNO, IT_ZSREQHD-ZFINAMT,
*                    IT_ZSREQHD-WAERS, IT_ZSREQHD-EBELN,
*                    IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST)
*               FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
*                     ON R~ZFREQNO  EQ I~ZFREQNO
*               WHERE R~EBELN   EQ ZSREQHD-EBELN
*               AND   I~ZFINSEQ EQ ZSREQHD-ZFINSEQ
*               AND   R~ZFAMDNO EQ '00000'
*               AND   I~ZFAMDNO NE '00000'.
*            APPEND IT_ZSREQHD.
*         ENDSELECT.
*      ELSE.
*         SELECT  I~ZFREQNO I~ZFINSEQ I~ZFAMDNO R~ZFOPNNO I~ZFOPNO
*                 I~ZFINAMT I~ZFINAMTC R~EBELN R~ZFREQTY I~ZFDOCST
*              INTO (IT_ZSREQHD-ZFREQNO, IT_ZSREQHD-ZFINSEQ,
*                    IT_ZSREQHD-ZFAMDNO, IT_ZSREQHD-ZFOPNNO,
*                    IT_ZSREQHD-ZFOPNO, IT_ZSREQHD-ZFINAMT,
*                    IT_ZSREQHD-WAERS, IT_ZSREQHD-EBELN,
*                    IT_ZSREQHD-ZFREQTY, IT_ZSREQHD-ZFDOCST)
*               FROM  ZVREQHD_ST AS R INNER JOIN ZTINS AS I
*                     ON R~ZFREQNO  EQ I~ZFREQNO
*               WHERE R~EBELN   EQ ZSREQHD-EBELN
*               AND   I~ZFINSEQ EQ ZSREQHD-ZFINSEQ
*               AND   R~ZFAMDNO EQ '00000'
*               AND   I~ZFAMDNO EQ ZSREQHD-ZFAMDNO.
*            APPEND IT_ZSREQHD.
*         ENDSELECT.
*      ENDIF.
*   ENDIF.
*-----------------------------------------------------------------------
* position
*-----------------------------------------------------------------------
  DESCRIBE TABLE IT_ZSREQHD LINES TFILL.
  IF TFILL = 0.
    MESSAGE E406.
  ENDIF.
* PERFORM   P2000_GET_POSITION.         " GET POSITION
  W_STATUS_CHK = 'C'.
  INCLUDE = 'INDISPL3'.                 " 보험 변?

  CALL SCREEN 0014 STARTING AT  01 03
                   ENDING   AT  90 15.
* CALL SCREEN 0014 STARTING AT  12 3
*                  ENDING   AT  77 15.

ENDFORM.                    " P2000_INS_AMD_DISP_SELECT3
*&---------------------------------------------------------------------*
*&      Form  LC_KIND_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LC_KIND_CHECK.
   CASE  ZTREQHD-ZFREQTY.
      WHEN 'LC'.
         IF NOT ( ZTREQHD-ZFLCKN EQ '1' OR ZTREQHD-ZFLCKN EQ '2'
                                        OR ZTREQHD-ZFLCKN EQ '3' ).
            MESSAGE  E567  WITH ZTREQHD-ZTERM ZTREQHD-ZFLCKN.
         ENDIF.
         IF ZTREQHD-ZFLCKN EQ '1'.
            CLEAR  ZTMLCHD-ZFPAGR.
         ENDIF.
         IF ZTREQHD-ZFLCKN EQ '2'.
            MOVE  '2AA'  TO  ZTMLCHD-ZFPAGR.
         ENDIF.
         IF ZTREQHD-ZFLCKN EQ '3'.
            MOVE  '2AB'  TO  ZTMLCHD-ZFPAGR.
         ENDIF.
      WHEN 'LO'.
         IF NOT ( ZTREQHD-ZFLCKN EQ '8' ).
            MESSAGE  E567  WITH ZTREQHD-ZTERM ZTREQHD-ZFLCKN.
         ENDIF.
      WHEN 'DA'.
         IF NOT ( ZTREQHD-ZFLCKN EQ '4' ).
            MESSAGE  E567  WITH ZTREQHD-ZTERM ZTREQHD-ZFLCKN.
         ENDIF.
      WHEN 'DP'.
         IF NOT ( ZTREQHD-ZFLCKN EQ '5' ).
            MESSAGE  E567  WITH ZTREQHD-ZTERM ZTREQHD-ZFLCKN.
         ENDIF.
      WHEN 'TT'.
         IF ZTREQHD-ZFBACD EQ 'A'.
            IF ZTREQHD-ZFLCKN NE '7'.
               MESSAGE E567 WITH ZTREQHD-ZTERM ZTREQHD-ZFLCKN.
            ENDIF.
         ENDIF.
         IF ZTREQHD-ZFBACD EQ 'B'.
            IF ZTREQHD-ZFLCKN NE '6'.
               MESSAGE E567 WITH ZTREQHD-ZTERM ZTREQHD-ZFLCKN.
            ENDIF.
         ENDIF.
      WHEN 'PU'.
         IF NOT ( ZTREQHD-ZFLCKN EQ '8' ).
            MESSAGE  E567  WITH ZTREQHD-ZTERM ZTREQHD-ZFLCKN.
         ENDIF.
      WHEN 'GS'.
         IF NOT ( ZTREQHD-ZFLCKN EQ 'G' ).
            MESSAGE  E567  WITH ZTREQHD-ZTERM ZTREQHD-ZFLCKN.
         ENDIF.
   ENDCASE.

ENDFORM.                    " LC_KIND_CHECK
*&---------------------------------------------------------------------*
*&      Form  P4000_DISPLAY_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_DISPLAY_DOCUMENT.

   SET  PARAMETER ID  'BUK'       FIELD   ZTINS-BUKRS.
   SET  PARAMETER ID  'GJR'       FIELD   ZTINS-GJAHR.
   SET  PARAMETER ID  'ZPBENR'    FIELD   ZTINS-BELNR.
   CALL TRANSACTION 'ZIMY3'.

ENDFORM.                    " P4000_DISPLAY_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  P2000_REFRESH_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_REFRESH_PRICE.

   W_TABIX  =  SY-TABIX.

   SELECT  SINGLE *  FROM  EKPO
   WHERE   EBELN     EQ    IT_ZSREQIT-EBELN
   AND     EBELP     EQ    IT_ZSREQIT-EBELP.

   SELECT SINGLE * FROM EKKO
    WHERE EBELN EQ IT_ZSREQIT-EBELN.

   IF EKKO-BSTYP EQ 'F'.       ">P/O
       MOVE EKPO-MENGE TO IT_ZSREQIT-ZFPOMENGE.
   ELSEIF EKKO-BSTYP EQ 'L'.   ">Contract
       MOVE EKPO-KTMNG TO IT_ZSREQIT-ZFPOMENGE.
   ELSEIF EKKO-BSTYP EQ 'K'.   ">Contract
       MOVE EKPO-KTMNG TO IT_ZSREQIT-ZFPOMENGE.
   ENDIF.

   MOVE :  EKPO-NETPR   TO   IT_ZSREQIT-NETPR,
           EKPO-MEINS   TO   IT_ZSREQIT-MEINS,
           EKPO-PEINH   TO   IT_ZSREQIT-PEINH,
           EKPO-BPRME   TO   IT_ZSREQIT-BPRME.

   MODIFY  IT_ZSREQIT      INDEX  W_TABIX.

ENDFORM.                    " P2000_REFRESH_PRICE
*&---------------------------------------------------------------------*
*&      Form  P2000_REFRESH_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_REFRESH_ITEM.

   LOOP AT IT_ZSREQIT.
      W_TABIX  =  SY-TABIX.

      SELECT SINGLE * FROM EKPO
      WHERE  EBELN EQ  IT_ZSREQIT-EBELN
      AND    EBELP EQ  IT_ZSREQIT-EBELP.

      IF EKPO-LOEKZ NE SPACE.
         CONTINUE.
      ENDIF.

      MOVE : EKPO-TXZ01     TO  IT_ZSREQIT-TXZ01,
             EKPO-MATNR     TO  IT_ZSREQIT-MATNR,
             EKPO-MEINS     TO  IT_ZSREQIT-MEINS,
             EKPO-BEDNR     TO  IT_ZSREQIT-BEDNR,
             EKPO-NETPR     TO  IT_ZSREQIT-NETPR,
             EKPO-PEINH     TO  IT_ZSREQIT-PEINH,
             EKPO-BPRME     TO  IT_ZSREQIT-BPRME,
             EKPO-MENGE     TO  IT_ZSREQIT-MENGE,
             EKPO-MENGE     TO  IT_ZSREQIT-ZFPOMENGE,
             0              TO  IT_ZSREQIT-ZFLCMENGE.

      MODIFY  IT_ZSREQIT  INDEX  W_TABIX.
   ENDLOOP.

   SELECT  *   FROM  EKPO  WHERE  EBELN  EQ  ZTREQHD-EBELN
                           AND    LOEKZ  NE  'X'
                           AND    ELIKZ  NE  'X'.

      READ TABLE IT_ZSREQIT WITH KEY EBELN  =  EKPO-EBELN
                                     EBELP  =  EKPO-EBELP.

      IF SY-SUBRC NE 0.
         MOVE-CORRESPONDING  EKPO  TO  IT_ZSREQIT.
         MOVE :  EKPO-EBELP        TO  IT_ZSREQIT-ZFITMNO,
                 EKPO-MENGE        TO  IT_ZSREQIT-ZFPOMENGE,
                 0                 TO  IT_ZSREQIT-ZFLCMENGE.

         INSERT  TABLE  IT_ZSREQIT.
      ENDIF.
   ENDSELECT.

   PERFORM P2000_IL_DATA_MOVE.

ENDFORM.                    " P2000_REFRESH_ITEM
