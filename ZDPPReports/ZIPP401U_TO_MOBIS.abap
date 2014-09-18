************************************************************************
* Author                 : Chris Li
* Creation Date          : 02/01/2004
* Specifications By      :
* Development Request No : UD1K914110
* Addl documentation     :
* Description            : VIN INFORMATION DOWNLOAD FOR MOBIS CO
* Modification Log
* Date       Developer    Request ID Description
* 11/19/07   Furong       Create other file for MI > 7
* 11/21/07   Furong       Version data need to be sent
************************************************************************

REPORT  ZIPP401U_TO_MOBIS  MESSAGE-ID ZMPP.
*
TABLES: AUSP, CABN.

*
DATA : BEGIN OF IT_AUSP OCCURS 0,
        OBJEK LIKE AUSP-OBJEK,
        ATINN LIKE AUSP-ATINN,
        ATWRT LIKE AUSP-ATWRT,
        ATFLV LIKE AUSP-ATFLV,
      END OF IT_AUSP.

RANGES R_ATINN FOR AUSP-ATINN OCCURS 0.
RANGES R_ATFLV FOR AUSP-ATFLV.
RANGES: R_ATNAM FOR CABN-ATNAM.
DATA : DSN(90),
       DSNEBOM(90).
DATA : BEGIN OF IT_CABN OCCURS 0,
       ATINN LIKE CABN-ATINN,
       ATNAM LIKE CABN-ATNAM,
       END OF IT_CABN.

DATA : BEGIN OF IT_LIST OCCURS 0,
         VIN(17),
         WORDR(15),
         YEAR(1),
         NATION(5),
         MODEL(10),
         OCN(4),
         VERSN(3),
         EXTCL(3),
         INTCL(3),
         PDATE(8),
         AREA(3),
       END OF IT_LIST.

** for EBOM
DATA : BEGIN OF IT_LIST_EBOM OCCURS 0,
         VIN(17),
         WORDR(15),
         YEAR(2),
         NATION(4),
         MODEL(12),
         OCN(4),
         VERSN(2),
         EXTCL(3),
         INTCL(3),
         PDATE(8),
         AREA(3),
         ENGNO(12),
         TMNO(12),
       END OF IT_LIST_EBOM.
** end of change

DATA : W_CNT TYPE I,
       W_CNT_EBOM TYPE I,
       W_ATINN LIKE CABN-ATINN,
       W_ATNAM LIKE CABN-ATNAM,
       W_ATFLV(10).


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_DATE  FOR SY-DATUM OBLIGATORY DEFAULT SY-DATUM.
SELECTION-SCREEN END OF BLOCK B1.


*
START-OF-SELECTION.
  MESSAGE i000 WITH 'DO NOT USE THIS PROGRAM'.
  LEAVE PROGRAM.

  PERFORM INIT_CABN.
  PERFORM READ_DATA.
  PERFORM MODIFY_DATA.


*
END-OF-SELECTION.
  PERFORM DOWNLOAD_DATA.





************************************************************************
*                            FORMS                                     *
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM READ_DATA.

  PERFORM GET_ATINN USING 'P_RP18_SHOP_DATE'.

  SELECT OBJEK ATINN ATWRT ATFLV
      INTO CORRESPONDING FIELDS OF TABLE IT_AUSP
      FROM AUSP
      WHERE OBJEK IN ( SELECT OBJEK
                                FROM AUSP
                               WHERE ATINN = W_ATINN
                                 AND KLART = '002'
                                 AND ATFLV IN R_ATFLV )
          AND ATINN IN R_ATINN
          AND KLART = '002'.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  INIT_CABN
*&---------------------------------------------------------------------*
FORM INIT_CABN.

* MAKE THE CHARACTERISTIC LIST
  CLEAR: R_ATNAM, R_ATNAM[].
  PERFORM BUILD_ATNAM USING 'P_VIN'.
  PERFORM BUILD_ATNAM USING 'P_WORK_ORDER'.
  PERFORM BUILD_ATNAM USING 'P_MODEL_YEAR'.
  PERFORM BUILD_ATNAM USING 'P_DESTINATION_CODE'.
  PERFORM BUILD_ATNAM USING 'P_MI'.
  PERFORM BUILD_ATNAM USING 'P_OCN'.
  PERFORM BUILD_ATNAM USING 'P_VERSION'.
  PERFORM BUILD_ATNAM USING 'P_EXT_COLOR'.
  PERFORM BUILD_ATNAM USING 'P_INT_COLOR'.
  PERFORM BUILD_ATNAM USING 'P_RP18_SHOP_DATE'.
  PERFORM BUILD_ATNAM USING 'P_NATN_CODE'.
  PERFORM BUILD_ATNAM USING 'P_ENGINE_NO'.
  PERFORM BUILD_ATNAM USING 'P_TM_NO'.
  REFRESH : IT_CABN, R_ATINN.
  CLEAR   : IT_CABN, R_ATINN.
* FIND THE CHARACTERISTIC INTERNAL NUMBER LIST
  SELECT ATINN ATNAM INTO TABLE IT_CABN
     FROM CABN
     WHERE ATNAM IN R_ATNAM.


  R_ATINN-SIGN = 'I'.
  R_ATINN-OPTION = 'EQ'.

  LOOP AT IT_CABN.
    R_ATINN-LOW = IT_CABN-ATINN.
    APPEND R_ATINN.
  ENDLOOP.

* CONVERT THE DATE RANGE FORMAT
  REFRESH R_ATFLV. CLEAR R_ATFLV.
  LOOP AT S_DATE.
    R_ATFLV-SIGN   = S_DATE-SIGN.
    R_ATFLV-OPTION = S_DATE-OPTION.
    W_ATFLV = S_DATE-LOW.
    R_ATFLV-LOW = W_ATFLV.

    W_ATFLV = S_DATE-HIGH.
    R_ATFLV-HIGH = W_ATFLV.

    APPEND R_ATFLV. CLEAR R_ATFLV.
  ENDLOOP.
ENDFORM.                    " INIT_CABN
*&---------------------------------------------------------------------*
*&      Form  GET_ATINN
*&---------------------------------------------------------------------*
FORM GET_ATINN USING P_ATNAM.
  READ TABLE IT_CABN WITH KEY ATNAM = P_ATNAM.
  IF SY-SUBRC = 0.
    W_ATINN = IT_CABN-ATINN.
  ENDIF.
ENDFORM.                    " GET_ATINN
*&---------------------------------------------------------------------*
*&      Form  GET_ATNAM
*&---------------------------------------------------------------------*
FORM GET_ATNAM USING P_ATINN.
  READ TABLE IT_CABN WITH KEY ATINN = P_ATINN.
  IF SY-SUBRC = 0.
    W_ATNAM = IT_CABN-ATNAM.
  ENDIF.
ENDFORM.                    " GET_ATINN
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM MODIFY_DATA.
  DATA : W_OBJEK LIKE AUSP-OBJEK.
  DATA : WA_AUSP LIKE IT_AUSP.
  DATA : L_TABIX LIKE SY-TABIX.
  DATA : L_DATE  TYPE I.
  DATA:  L_LEN TYPE I,
         L_NATN_OLD(2),
         L_NATN_NEW(1),
         L_ENGNO(12),
         L_TMNO(12).
  DATA: L_MODEL_2(2),
        L_COLOR_3(3).


  REFRESH IT_LIST. CLEAR IT_LIST.
  SORT IT_AUSP BY OBJEK ATINN.

* TRANSFER THE VALUE TO OUTPUT TABLE
  LOOP AT IT_AUSP.
    L_TABIX = SY-TABIX + 1.


    PERFORM GET_ATNAM USING IT_AUSP-ATINN.

    CASE W_ATNAM.
      WHEN 'P_VIN'.
        IT_LIST-VIN     = IT_AUSP-ATWRT.

      WHEN 'P_WORK_ORDER'.
        IT_LIST-WORDR   = IT_AUSP-ATWRT.

      WHEN 'P_MODEL_YEAR'.
        IT_LIST-YEAR    = IT_AUSP-ATWRT.

      WHEN 'P_DESTINATION_CODE'.
        IT_LIST-NATION  = IT_AUSP-ATWRT.

      WHEN 'P_MI'.
        IT_LIST-MODEL   = IT_AUSP-ATWRT.

      WHEN 'P_OCN'.
        IT_LIST-OCN     = IT_AUSP-ATWRT.

      WHEN 'P_VERSION'.
*-->requested by Catherine, changed by CHRIS
*        IT_LIST-VERSN   = IT_AUSP-ATWRT.
** Added by Furong on 11/21/07
*        IT_LIST-VERSN   = SPACE.
        IT_LIST-VERSN   = IT_AUSP-ATWRT.
        IF IT_LIST-VERSN = '000'.
          CLEAR: IT_LIST-VERSN.
        ENDIF.
** End of change on 11/21/07
*-->end of change on 02/21/2005
      WHEN 'P_EXT_COLOR'.
        IT_LIST-EXTCL   = IT_AUSP-ATWRT.
** Added by Furong on 02/17/10
        L_MODEL_2 = IT_LIST-MODEL+0(2).
        SELECT SINGLE CTRN_KEY_COLR INTO L_COLOR_3
         FROM ZTBM_ABYCOLDT
         WHERE CTRN_CARS_C = L_MODEL_2
           AND CTRN_GUBN_C = 'EXT'
           AND CTRN_CONF_COLR = IT_LIST-EXTCL.
        L_LEN = STRLEN( L_COLOR_3 ).
        IF L_LEN = 3.
          IT_LIST-EXTCL = L_COLOR_3.
        ENDIF.
** End of change
      WHEN 'P_INT_COLOR'.
        IT_LIST-INTCL   = IT_AUSP-ATWRT.
** Added by Furong on 02/17/10
        L_MODEL_2 = IT_LIST-MODEL+0(2).
        SELECT SINGLE CTRN_KEY_COLR INTO L_COLOR_3
         FROM ZTBM_ABYCOLDT
         WHERE CTRN_CARS_C = L_MODEL_2
           AND CTRN_GUBN_C = 'INT'
           AND CTRN_CONF_COLR = IT_LIST-INTCL.
        L_LEN = STRLEN( L_COLOR_3 ).
        IF L_LEN = 3.
          IT_LIST-INTCL = L_COLOR_3.
        ENDIF.
** End of change

      WHEN 'P_RP18_SHOP_DATE'.
        L_DATE          = IT_AUSP-ATFLV.
        IT_LIST-PDATE   = L_DATE .
      WHEN 'P_NATN_CODE'.
        IT_LIST-AREA    = IT_AUSP-ATWRT.
** Added by Furong on 11/19/07 for EBOM
      WHEN 'P_ENGINE_NO'.
        L_ENGNO    = IT_AUSP-ATWRT.
      WHEN 'P_TM_NO'.
        L_TMNO    = IT_AUSP-ATWRT.
** end of change
    ENDCASE.

    CLEAR: WA_AUSP, L_LEN.
    READ TABLE IT_AUSP INTO WA_AUSP INDEX L_TABIX.
    IF IT_AUSP-OBJEK NE WA_AUSP-OBJEK.
      W_OBJEK = IT_AUSP-OBJEK.
** Changed by Furong on 11/19/07 for EBOM
*      APPEND IT_LIST.
      L_LEN = STRLEN( IT_LIST-MODEL ).
      IF L_LEN > 7.
** Changed by Furong on 11/27/07
        CASE IT_LIST-AREA.
          WHEN 'B28'.
            IT_LIST-AREA = 'A7'.
          WHEN 'B35'.
            IT_LIST-AREA = 'A7'.
          WHEN 'B06'.
            IT_LIST-AREA = 'A6'.
        ENDCASE.
** End of change
        L_NATN_OLD = IT_LIST-NATION+3(2).
        CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
             EXPORTING
                  OLD_DEALER = L_NATN_OLD
             IMPORTING
                  NEW_DEALER = L_NATN_NEW.
        CONCATENATE IT_LIST-NATION+0(3) L_NATN_NEW INTO IT_LIST-NATION.
        IF IT_LIST-VERSN = '000'.
          CLEAR: IT_LIST-VERSN.
        ELSE.
          IT_LIST-VERSN = IT_LIST-VERSN+1(2).
        ENDIF.
        MOVE-CORRESPONDING IT_LIST TO IT_LIST_EBOM.
        IT_LIST_EBOM-ENGNO = L_ENGNO.
        IT_LIST_EBOM-TMNO = L_TMNO.
        APPEND IT_LIST_EBOM.
        CLEAR: IT_LIST_EBOM, L_ENGNO, L_TMNO.
      ELSE.
        APPEND IT_LIST.
        CLEAR IT_LIST.
      ENDIF.
** End of change
    ENDIF.

  ENDLOOP.
* DELETE THE 'XX','XY' cars
  PERFORM FILTER_RESULT.
*  DELETE it_list WHERE NOT rp18_shop_date IN s_date.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
FORM DOWNLOAD_DATA.
  DATA: L_DATE(06).

  DESCRIBE TABLE IT_LIST LINES W_CNT.
  DESCRIBE TABLE IT_LIST_EBOM LINES W_CNT_EBOM.

  IF W_CNT = 0 AND W_CNT_EBOM = 0.
    MESSAGE I000 WITH TEXT-M02.
    STOP.
  ENDIF.

  L_DATE = S_DATE-LOW+2(6).

*  IF S_DATE-HIGH IS INITIAL.
*    L_DATE = S_DATE-LOW.
*  ELSE.
*   CONCATENATE S_DATE-LOW '_' S_DATE-HIGH+4(4) INTO L_DATE.
*  ENDIF.

  IF W_CNT > 0.
    CONCATENATE  '/usr/sap/EDI_SAP/'
                 'V' L_DATE
                 '.txt'
                 INTO DSN.

    OPEN DATASET DSN IN TEXT MODE FOR OUTPUT.
    LOOP AT IT_LIST.
      OPEN DATASET DSN IN TEXT MODE FOR APPENDING.
      TRANSFER IT_LIST TO DSN.
    ENDLOOP.
    CLOSE DATASET DSN.

    IF SY-SUBRC = 0.
      WRITE: /10 'FILE IS DOWNLOADED SUCCESSFULLY.'.
*     SKIP.
      WRITE: /10 'FILE NAME:', DSN.
*     SKIP.
      WRITE: /10 'TOTAL RECORDS:', W_CNT.
    ELSE.
      FORMAT COLOR 6.
      WRITE: /10 'FILE NAME:', DSN.
*     SKIP.
      WRITE: /10 'TOTAL RECORDS: ', W_CNT.
*     SKIP.
      WRITE: /10 'FILE DOWNLOAD FAILED!'.
      FORMAT COLOR OFF.
      MESSAGE E000 WITH 'FILE DOWLOAD FAILED.'.
    ENDIF.
  ENDIF.

  IF W_CNT_EBOM > 0.
    CONCATENATE  '/usr/sap/EDI_SAP/'
                   'VE' L_DATE
                   '.txt'
                   INTO DSNEBOM.

    OPEN DATASET DSNEBOM IN TEXT MODE FOR OUTPUT.
    LOOP AT IT_LIST_EBOM.
*      OPEN DATASET DSNEBOM IN TEXT MODE FOR APPENDING.
      TRANSFER IT_LIST_EBOM TO DSNEBOM.
    ENDLOOP.
    CLOSE DATASET DSNEBOM.

    IF SY-SUBRC = 0.
      SKIP.
      WRITE: /10 'FILE IS DOWNLOADED SUCCESSFULLY.'.
*      SKIP.
      WRITE: /10 'FILE NAME:', DSNEBOM.
*      SKIP.
      WRITE: /10 'TOTAL RECORDS:', W_CNT_EBOM.
    ELSE.
      SKIP.
      FORMAT COLOR 6.
      WRITE: /10 'FILE NAME:', DSNEBOM.
*      SKIP.
      WRITE: /10 'TOTAL RECORDS: ', W_CNT_EBOM.
*      SKIP.
      WRITE: /10 'FILE DOWNLOAD FAILED!'.
      FORMAT COLOR OFF.
      MESSAGE E000 WITH 'FILE DOWLOAD FAILED.'.
    ENDIF.
  ENDIF.

ENDFORM.                    " DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  BUILD_ATNAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0197   text
*----------------------------------------------------------------------*
FORM BUILD_ATNAM USING  P_ATNAM.
  R_ATNAM-SIGN = 'I'.
  R_ATNAM-OPTION = 'EQ'.
  R_ATNAM-LOW = P_ATNAM.
  APPEND R_ATNAM.
ENDFORM.                    " BUILD_ATNAM
*&---------------------------------------------------------------------*
*&      Form  FILTER_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILTER_RESULT.
  LOOP AT IT_LIST.
    IF IT_LIST-NATION CS 'XX' OR
       IT_LIST-NATION CS 'XY' OR
       IT_LIST-NATION CS 'XA'.
      DELETE IT_LIST.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " FILTER_RESULT
