************************************************************************
* Program Name      : ZIMMPM21I_KDPO
* Author            : Sung-Tae, Lim
* Creation Date     : 2003.11.13.
* Specifications By : Sung-Tae, Lim
* Pattern           : Interface 5.2.2 - Outbound(Real Time)
* Development Request No : UD1K902659
* Addl Documentation:
* Description       : Inteface with KDWeb PO(Outbound : SAP MM -> KDWeb)
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.13.     Sung-Tae Lim     UD1K902659     Initial Coding
* 2005.03.23      Furong Wang                     Trafic light
*                                                 check duplicate part
*                                                 in a PO
* 2005.06.02      Furong Wang                     Add ZP16-18
* 2007.12.12      Rakesh Gandhi    UD1K942394     Add Vendor code
************************************************************************

REPORT ZIMMPM21I_KDPO NO STANDARD PAGE HEADING
                      LINE-SIZE 180
                      LINE-COUNT 58
                      MESSAGE-ID ZMMM.

**---
INCLUDE : ZRMMPMXXR_INCL,
          <ICON>,
          <SYMBOL>.


**---
DATA : IT_SEND LIKE ZSMM_KD_PO OCCURS 0 WITH HEADER LINE.

DATA : IT_SUCCESS LIKE IT_SEND OCCURS 0 WITH HEADER LINE,
       IT_R_ERROR LIKE IT_SEND OCCURS 0 WITH HEADER LINE,
       IT_ERROR   LIKE IT_SEND OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF IT_ITAB OCCURS 0,
         CHECK(1),
         EBELN LIKE EKKO-EBELN,
         LIFNR LIKE EKKO-LIFNR,
*         NAME1 LIKE LFA1-NAME1,
         NAME1(25),

         EKGRP LIKE EKKO-EKGRP,
         EKNAM LIKE T024-EKNAM,
         WERKS LIKE EKPO-WERKS,
         AEDAT LIKE EKKO-AEDAT,
*         bedat LIKE ekko-bedat,
         KNUMV LIKE EKKO-KNUMV,
         FLAG  LIKE ZTMM_KD_PO-FLAG,
         ZMSG(15),
*A__by Paul
         ZRESULTC LIKE ZTMM_KD_PO-ZRESULTC,
         ZMSGC(65),
*
       END OF IT_ITAB.

DATA : BEGIN OF IT_EKPO OCCURS 0.
        INCLUDE STRUCTURE EKPO.
DATA:  END OF IT_EKPO.

*DATA: IT_MLGN LIKE MLGN OCCURS 0 WITH HEADER LINE.

DATA : IT_TEMP LIKE IT_ITAB OCCURS 0 WITH HEADER LINE.

DATA : IT_RESULT LIKE ZTMM_KD_PO OCCURS 0 WITH HEADER LINE.

**---
DATA : W_TABIX TYPE SY-TABIX,
       W_MSGTXT(100).

DATA : W_SUCCESS_COUNT TYPE I,
       W_ERROR_COUNT TYPE I.

DATA : WA_EKPO LIKE IT_EKPO,
       W_MULTIPO(1),
       W_TAXCODE(1),
       W_GRINV(1),
       W_MATNRERR LIKE EKPO-MATNR,
       W_ITEMNO LIKE EKPO-EBELP.

**---
CONSTANTS : C_DEST(10) VALUE 'WMPM01',
*            c_dest(10) VALUE 'WMMM01',
            C_BSART LIKE EKKO-BSART VALUE 'KD'.

**--- insert by stlim (2004/05/11)
CONSTANTS : C_LIFNR_G LIKE LFA1-LIFNR VALUE 'SEF9',
            C_LIFNR_H LIKE LFA1-LIFNR VALUE 'SBC3',
* by ig.moon {
            C_LIFNR_X LIKE LFA1-LIFNR VALUE 'SSTX'.
* }
**--- end of insert


TABLES : ZTREQHD.
**A__by Paul
*---------------------------------------------------*
* BDC
*---------------------------------------------------*
DATA: BEGIN OF BDCDATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.
DATA : IT_MESSAGE   TYPE TABLE OF BDCMSGCOLL  WITH HEADER LINE.
DATA : BDC_MODE(1)  VALUE 'N'.
DATA : CTU_PARAMS TYPE CTU_PARAMS.
**E__<

**---
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_WERKS FOR T001W-WERKS OBLIGATORY NO-EXTENSION
                                                    NO INTERVALS
                                                    DEFAULT 'P001',
                 S_LIFNR FOR LFA1-LIFNR NO-EXTENSION
                                        NO INTERVALS DEFAULT 'SEF9' ,
                 S_EKGRP FOR EKKO-EKGRP,
                 S_MATKL FOR MARA-MATKL,
                 S_BEDAT FOR EKKO-BEDAT,
                 S_EBELN FOR EKKO-EBELN.
SELECTION-SCREEN ULINE.
*PARAMETERS : p_check AS CHECKBOX DEFAULT 'X'.
PARAMETERS : P_NOSEND RADIOBUTTON GROUP GRP1 DEFAULT 'X',
             P_ERROR RADIOBUTTON GROUP GRP1,
             P_SENT RADIOBUTTON GROUP GRP1,
             P_BYPASS RADIOBUTTON GROUP GRP1,
             P_ALL RADIOBUTTON GROUP GRP1.

SELECTION-SCREEN END OF BLOCK BLOCK1.


**---
TOP-OF-PAGE.
  PERFORM WRITE_TOP_OF_PAGE.

**---
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM WRITE_TOP_OF_PAGE.

**---
START-OF-SELECTION.
  PERFORM GET_DATA.
**---
END-OF-SELECTION.
  IF IT_ITAB[] IS INITIAL.
    MESSAGE S999 WITH TEXT-M01.
  ELSE.
    IF P_SENT = 'X' OR P_BYPASS = 'X'.
      SET PF-STATUS 'ST1'.
    ELSE.
      SET PF-STATUS 'BASE'.
    ENDIF.
    PERFORM WRITE_DATA.
  ENDIF.

**---
AT USER-COMMAND.
  PERFORM USER_COMMAND.


**---
AT LINE-SELECTION.
  PERFORM DISPLAY_PO.





*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM USER_COMMAND.
*---
  CLEAR : IT_SEND, IT_SEND[], IT_RESULT, IT_RESULT[],
          IT_SUCCESS, IT_SUCCESS[], IT_ERROR, IT_ERROR[],
          IT_R_ERROR, IT_R_ERROR[].

  SY-LSIND = SY-LSIND - 1.

  CASE SY-UCOMM.
*    WHEN 'PICK'.
*      PERFORM display_po.
    WHEN 'SALL'.
      PERFORM SELECT_DESELECT_ALL USING 'X'.
    WHEN 'DALL'.
      PERFORM SELECT_DESELECT_ALL USING ' '.
    WHEN 'SEND'.
      PERFORM SELECT_CHECKED_PO.
**Auto IR Create 05112011 by Paul
      PERFORM CREATE_IR.
**
      PERFORM POITEM_CHECK.
      IF NOT IT_TEMP[] IS INITIAL.
        READ TABLE IT_TEMP WITH KEY FLAG = 'S'.
        IF SY-SUBRC EQ 0.
          MESSAGE E999 WITH TEXT-M02.
          EXIT.
        ELSE.
          READ TABLE IT_TEMP WITH KEY FLAG = 'R'.
          IF SY-SUBRC EQ 0.
            MESSAGE E999 WITH TEXT-M02.
            EXIT.
          ELSE.
            READ TABLE IT_TEMP WITH KEY FLAG = 'P'.
            IF SY-SUBRC EQ 0.
              MESSAGE E999 WITH TEXT-M02.
              EXIT.
            ELSE.
              PERFORM SEND_PO.
              PERFORM WRITE_DATA.
              PERFORM CALL_MESSAGE_SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE W999 WITH TEXT-M04.
      ENDIF.
    WHEN 'RESEND'.
      PERFORM SELECT_CHECKED_PO.
      PERFORM POITEM_CHECK.
      IF NOT IT_TEMP[] IS INITIAL.
        READ TABLE IT_TEMP WITH KEY FLAG = ' '.
        IF SY-SUBRC EQ 0.
          MESSAGE E999 WITH TEXT-M03.
          EXIT.
        ELSE.
          READ TABLE IT_TEMP WITH KEY FLAG = 'S'.
          IF SY-SUBRC EQ 0.
            MESSAGE E999 WITH TEXT-M03.
          ELSE.
            READ TABLE IT_TEMP WITH KEY FLAG = 'P'.
            IF SY-SUBRC EQ 0.
              MESSAGE E999 WITH TEXT-M03.
            ELSE.
              PERFORM SEND_PO.
              PERFORM WRITE_DATA.
              PERFORM CALL_MESSAGE_SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE W999 WITH TEXT-M04.
      ENDIF.

    WHEN 'BYPASS'.
      PERFORM SELECT_CHECKED_PO.
*      perform poitem_check.
      IF NOT IT_TEMP[] IS INITIAL.
        READ TABLE IT_TEMP WITH KEY FLAG = 'S'.
        IF SY-SUBRC EQ 0.
          MESSAGE E999 WITH TEXT-M02.
          EXIT.
        ELSE.
          READ TABLE IT_TEMP WITH KEY FLAG = 'R'.
          IF SY-SUBRC EQ 0.
            MESSAGE E999 WITH TEXT-M02.
          ELSE.
            READ TABLE IT_TEMP WITH KEY FLAG = 'P'.
            IF SY-SUBRC EQ 0.
              MESSAGE E999 WITH TEXT-M02.
            ELSE.
              PERFORM UPDATE_ZPO_BYPASS.
              PERFORM WRITE_DATA.
              PERFORM CALL_MESSAGE_SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE W999 WITH TEXT-M04.
      ENDIF.

  ENDCASE.
ENDFORM.                    " user_command

*&---------------------------------------------------------------------*
*&      Form  send_po
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_PO.
*---
  DATA: IT_KONV TYPE KONV OCCURS 0 WITH HEADER LINE.
  SELECT * INTO TABLE IT_KONV FROM KONV FOR ALL ENTRIES IN IT_TEMP
           WHERE KNUMV = IT_TEMP-KNUMV.
*A__By Paul
  LOOP AT IT_TEMP.
*  LOOP AT IT_TEMP WHERE ZRESULTC = 'S'.

*    SELECT a~ebeln
*           zzkdwebpo
*           a~aedat
*           ebelp
*           matnr
*           menge
*           meins
*           netpr
*           waers
*           expvz
*           bednr
*           zzeta
*           lifnr     " 2004/02/17 add by stlim
*           peinh     " 2004/06/23 add by stlim
*                 INTO (ekko-ebeln, ekko-zzkdwebpo, ekko-aedat,
*                       ekpo-ebelp, ekpo-matnr,     ekpo-menge,
*                       ekpo-meins, ekpo-netpr,     ekko-waers,
*                       eikp-expvz, ekpo-bednr,     ekko-zzeta,
*                       ekko-lifnr, ekpo-peinh)
*                 FROM ekko AS a INNER JOIN ekpo AS b
*                   ON a~mandt EQ b~mandt
*                  AND a~ebeln EQ b~ebeln
*                      LEFT OUTER JOIN eikp AS c
*                        ON a~mandt EQ c~mandt
*                       AND a~exnum EQ c~exnum
*                WHERE a~ebeln EQ it_temp-ebeln
*                  AND a~loekz EQ space
*                  AND b~loekz EQ space.

    SELECT A~EBELN
            ZZKDWEBPO
            A~AEDAT
            B~EBELP
            MATNR
            B~MENGE
            MEINS
            NETPR
            WAERS
            EXPVZ
            BEDNR
            EINDT
            LIFNR     " 2004/02/17 add by stlim
            PEINH     " 2004/06/23 add by stlim
*           a~bedat   " added by FR ON 06/02/2005
            A~KNUMV
                  INTO (EKKO-EBELN, EKKO-ZZKDWEBPO, EKKO-AEDAT,
                        EKPO-EBELP, EKPO-MATNR,     EKPO-MENGE,
                        EKPO-MEINS, EKPO-NETPR,     EKKO-WAERS,
                        EIKP-EXPVZ, EKPO-BEDNR,     EKET-EINDT,
                        EKKO-LIFNR, EKPO-PEINH,
*                       ekko-bedat,
                        EKKO-KNUMV)
                  FROM EKKO AS A INNER JOIN EKPO AS B
                    ON A~MANDT EQ B~MANDT
                   AND A~EBELN EQ B~EBELN
                       INNER JOIN EKET AS D
                         ON A~MANDT EQ D~MANDT
                       AND A~EBELN EQ D~EBELN
                       AND B~EBELP EQ D~EBELP
                       LEFT OUTER JOIN EIKP AS C
                         ON A~MANDT EQ C~MANDT
                        AND A~EXNUM EQ C~EXNUM
                 WHERE A~EBELN EQ IT_TEMP-EBELN
                   AND A~LOEKZ EQ SPACE
                   AND B~LOEKZ EQ SPACE.

      MOVE : EKKO-EBELN     TO IT_SEND-EBELN,
             EKKO-ZZKDWEBPO TO IT_SEND-ZZKDWEBPO,
*** requested by BK Lee
**             ekko-aedat     TO it_send-aedat,
             SY-DATUM       TO IT_SEND-AEDAT ,
**  end of insertion

*             ekko-bedat     TO it_send-bedat,
             EKPO-EBELP     TO IT_SEND-EBELP,
             EKPO-MATNR     TO IT_SEND-MATNR,
             EKKO-LIFNR     TO IT_SEND-LIFNR,      " +UD1K942394
             EKPO-MENGE     TO IT_SEND-MENGE.
*      WRITE : ekpo-menge UNIT ekpo-meins TO it_send-menge.
*      it_send-menge = it_send-menge / 1000.
      MOVE : EKPO-MEINS     TO IT_SEND-MEINS,
             EKPO-NETPR     TO IT_SEND-NETPR,
             EKKO-WAERS     TO IT_SEND-WAERS,
             EIKP-EXPVZ     TO IT_SEND-EXPVZ,
             EKPO-BEDNR     TO IT_SEND-BEDNR,
             EKET-EINDT     TO IT_SEND-EINDT.
      IT_SEND-ZUSER = SY-UNAME.
      IT_SEND-ZSDAT = IT_SEND-ZEDAT = SY-DATUM.
      IT_SEND-ZSTIM = IT_SEND-ZETIM = SY-UZEIT.
      IT_SEND-ZMODE = 'C'.

** Furon on 12/16/11
     CLEAR: IT_SEND-STAWN.
     select single STAWN into IT_SEND-STAWN
       from marc
       where matnr = IT_SEND-MATNR.
** end on 12/16/11
*--- 2004/02/17 add by stlim
*--- block & insert by stlim (2004/05/11)

* by ig.moon 11/08/2007 {
*      IF ekko-lifnr EQ c_lifnr_g.
      IF EKKO-LIFNR EQ C_LIFNR_G OR EKKO-LIFNR EQ C_LIFNR_X.
* }
*      IF ekko-lifnr EQ 'N006'.     " Glovis
        MOVE : 'G' TO IT_SEND-COMPANY.
      ELSEIF EKKO-LIFNR EQ C_LIFNR_H.
*      ELSEIF ekko-lifnr EQ 'N099'.     " HMC
        MOVE : 'H' TO IT_SEND-COMPANY.
      ENDIF.
*--- 2004/02/17
      MOVE : EKPO-PEINH    TO IT_SEND-PEINH.

*** added by FR ON 06/02/2005
      LOOP AT IT_KONV WHERE KNUMV = EKKO-KNUMV
                      AND KPOSN = EKPO-EBELP.
        CASE IT_KONV-KSCHL.
          WHEN 'ZP16'.
            IT_SEND-ZP16 = IT_KONV-KBETR.
          WHEN 'ZP17'.
            IT_SEND-ZP17 = IT_KONV-KBETR.
          WHEN 'ZP18'.
            IT_SEND-ZP18 = IT_KONV-KBETR.
        ENDCASE.
      ENDLOOP.
*** end of addition
      APPEND IT_SEND.
      CLEAR : IT_SEND.
    ENDSELECT.
  ENDLOOP.
*A__ BY PAUL
  IF NOT IT_SEND[] IS INITIAL.
*---
    SORT IT_SEND BY EBELN.

    CALL FUNCTION 'Z_FMM_SAP_TO_KD'
      DESTINATION  C_DEST
      TABLES
        T_CKD_PO       = IT_SEND
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1  MESSAGE W_MSGTXT
      SYSTEM_FAILURE        = 2  MESSAGE W_MSGTXT.

*---
    IF SY-SUBRC EQ 0.
      SORT IT_SEND BY EBELN.
      LOOP AT IT_SEND.
        IF IT_SEND-ZZRET EQ 'S'.     " Success
          MOVE-CORRESPONDING IT_SEND TO IT_SUCCESS.
          APPEND IT_SUCCESS.
        ELSEIF IT_SEND-ZZRET EQ 'E'.     " Error
          MOVE-CORRESPONDING IT_SEND TO IT_ERROR.
          APPEND IT_ERROR.
        ENDIF.
      ENDLOOP.

      SORT : IT_SUCCESS BY EBELN,
             IT_ERROR   BY EBELN.

*---
      DATA : L_FLAG(1).

*--- Success Processing
      LOOP AT IT_SUCCESS.
        CLEAR : L_FLAG.
        MOVE-CORRESPONDING IT_SUCCESS TO IT_RESULT.
        MOVE : IT_SUCCESS-ZZRET       TO L_FLAG.
        AT END OF EBELN.
          IF L_FLAG EQ 'S'.
            MOVE : 'S' TO IT_RESULT-FLAG.
            MOVE : 'Sent' TO IT_RESULT-ZMSG.
          ENDIF.
**A__ BY PAUL ABOUT IR CREATE
          READ TABLE IT_TEMP WITH KEY EBELN = IT_SUCCESS-EBELN.
          IT_RESULT-ZRESULTC = IT_TEMP-ZRESULTC.
          IT_RESULT-ZMSGC    = IT_TEMP-ZMSGC.
**E__<
          APPEND IT_RESULT.
          CLEAR : IT_RESULT.
        ENDAT.
      ENDLOOP.

      DESCRIBE TABLE IT_RESULT LINES W_SUCCESS_COUNT.

      MODIFY ZTMM_KD_PO FROM TABLE IT_RESULT.

*--- internal table field modify
      LOOP AT IT_ITAB.
        READ TABLE IT_RESULT WITH KEY EBELN = IT_ITAB-EBELN.
        IF SY-SUBRC EQ 0.
          MOVE : 'S' TO IT_ITAB-FLAG.
          MOVE : 'Sent' TO IT_ITAB-ZMSG.
          MODIFY IT_ITAB TRANSPORTING FLAG ZMSG
                                WHERE EBELN EQ IT_ITAB-EBELN.
        ENDIF.
      ENDLOOP.
    ENDIF.

*--- Error Processing (include Resend Error)
    CLEAR : IT_RESULT, IT_RESULT[].

    LOOP AT IT_ERROR.
      CLEAR : L_FLAG.
      MOVE-CORRESPONDING IT_ERROR TO IT_RESULT.
      MOVE : IT_ERROR-ZZRET       TO L_FLAG.
      AT END OF EBELN.
**A__ BY PAUL ABOUT IR CREATE
        READ TABLE IT_TEMP WITH KEY EBELN = IT_ERROR-EBELN.
        IT_RESULT-ZRESULTC = IT_TEMP-ZRESULTC.
        IT_RESULT-ZMSGC    = IT_TEMP-ZMSGC.
**E__<
        IF L_FLAG EQ 'E'.
          MOVE : 'E' TO IT_RESULT-FLAG.
          MOVE : 'Sent Fail' TO IT_RESULT-ZMSG.
        ENDIF.
        APPEND IT_RESULT.
        CLEAR : IT_RESULT.
      ENDAT.
    ENDLOOP.

    DESCRIBE TABLE IT_RESULT LINES W_ERROR_COUNT.

    LOOP AT IT_RESULT.
      CLEAR : ZTMM_KD_PO.
      SELECT SINGLE * FROM ZTMM_KD_PO
                     WHERE EBELN EQ IT_RESULT-EBELN.
      IF SY-SUBRC EQ 0.     " if resend error
        MOVE-CORRESPONDING IT_RESULT TO ZTMM_KD_PO.
        MOVE : 'R'                   TO ZTMM_KD_PO-FLAG.
        MOVE : 'Resent Fail' TO ZTMM_KD_PO-ZMSG.
        READ TABLE IT_ITAB WITH KEY EBELN = IT_RESULT-EBELN.
        IF SY-SUBRC EQ 0.
          MOVE : 'R' TO IT_ITAB-FLAG.
          MOVE : 'Resent Fail' TO IT_ITAB-ZMSG.
          MODIFY IT_ITAB TRANSPORTING FLAG ZMSG
                                     WHERE EBELN EQ IT_RESULT-EBELN.
        ENDIF.
      ELSE.
** INSERTED BY FURONG
        MOVE : 'E' TO IT_ITAB-FLAG.
        MOVE : 'Sent Fail' TO IT_ITAB-ZMSG.
        MODIFY IT_ITAB TRANSPORTING FLAG ZMSG
                                  WHERE EBELN EQ IT_RESULT-EBELN.
** END OF CHANGE
        MOVE-CORRESPONDING IT_RESULT TO ZTMM_KD_PO.
        MOVE : 'Sent Fail' TO ZTMM_KD_PO-ZMSG.
      ENDIF.
      MODIFY ZTMM_KD_PO.
    ENDLOOP.

*--- logging interface table
    DATA : ST_ZTCA_IF_LOG LIKE ZTCA_IF_LOG.

    CLEAR : ST_ZTCA_IF_LOG.

    MOVE : SY-TCODE        TO ST_ZTCA_IF_LOG-TCODE.
    ST_ZTCA_IF_LOG-TOTAL = W_SUCCESS_COUNT + W_ERROR_COUNT.
    MOVE : W_SUCCESS_COUNT TO ST_ZTCA_IF_LOG-ZSUCC,
           W_ERROR_COUNT   TO ST_ZTCA_IF_LOG-ERROR,
           SY-DATUM        TO ST_ZTCA_IF_LOG-ERDAT,
           SY-UZEIT        TO ST_ZTCA_IF_LOG-ERZET,
           SY-UNAME        TO ST_ZTCA_IF_LOG-ERNAM,
           SY-DATUM        TO ST_ZTCA_IF_LOG-AEDAT,
           SY-UZEIT        TO ST_ZTCA_IF_LOG-AEZET,
           SY-UNAME        TO ST_ZTCA_IF_LOG-AENAM.

    CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
      EXPORTING
        I_ZTCA_IF_LOG              = ST_ZTCA_IF_LOG
*   IMPORTING
*     E_ZTCA_IF_LOG              =
     EXCEPTIONS
       UPDATE_FAILED              = 1
       NUMBER_RANGE_ERROR         = 2
       TCODE_DOES_NOT_EXIST       = 3
       OTHERS                     = 4.

  ELSE.

  CLEAR : W_ERROR_COUNT, W_SUCCESS_COUNT.

    LOOP AT IT_TEMP.
      IF IT_TEMP-ZRESULTC = 'E'.
       W_ERROR_COUNT = W_ERROR_COUNT + 1.
      ELSE.
       W_SUCCESS_COUNT = W_SUCCESS_COUNT + 1.
      ENDIF.
*      CLEAR : ZTMM_KD_PO.
*      MOVE-CORRESPONDING IT_TEMP TO ZTMM_KD_PO.
*      ZTMM_KD_PO-FLAG = 'E'.
*      MODIFY ZTMM_KD_PO.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " send_po


**************

FORM UPDATE_ZPO_BYPASS.
  LOOP AT IT_TEMP.
    MOVE-CORRESPONDING IT_TEMP TO ZTMM_KD_PO.
    ZTMM_KD_PO-FLAG = 'P'.
    ZTMM_KD_PO-ZMSG = 'Bypass'.
    ZTMM_KD_PO-ZUSER = SY-UNAME.
    ZTMM_KD_PO-ZSDAT = ZTMM_KD_PO-ZEDAT = SY-DATUM.
    ZTMM_KD_PO-ZSTIM = ZTMM_KD_PO-ZETIM = SY-UZEIT.
    MODIFY ZTMM_KD_PO.
    CLEAR ZTMM_KD_PO.
    CLEAR IT_TEMP.
  ENDLOOP.
  DESCRIBE TABLE IT_TEMP LINES W_SUCCESS_COUNT.
  CLEAR W_ERROR_COUNT.

** Furong on 05/18/12
  SORT IT_TEMP BY EBELN.
** End on 05/18/12
  LOOP AT IT_ITAB.
    READ TABLE IT_TEMP WITH KEY EBELN = IT_ITAB-EBELN
** Furong on 05/18/12
                                BINARY SEARCH.
** End on 05/18/12
    IF SY-SUBRC EQ 0.
      MOVE : 'P' TO IT_ITAB-FLAG.
      MOVE : 'Bypass' TO IT_ITAB-ZMSG.
      MODIFY IT_ITAB TRANSPORTING FLAG ZMSG
                            WHERE EBELN EQ IT_ITAB-EBELN.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " update_zpo_bypass



*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
*---
  CLEAR : IT_SEND, IT_SEND[], IT_ITAB, IT_ITAB[], IT_TEMP, IT_TEMP[].

*  IF p_check NE space.
*    PERFORM get_data_to_be_sent.
*  ELSEIF p_check EQ space.
*    PERFORM get_data_all.
*  ENDIF.

  IF P_NOSEND  = 'X'.
    PERFORM GET_DATA_TO_BE_SENT.
  ELSE.
    IF P_SENT = 'X'.
      PERFORM GET_DATA_SENT.
    ELSE.
      IF P_ERROR = 'X'.
        PERFORM GET_DATA_ERROR.
      ELSE.
        IF P_ALL = 'X'.
          PERFORM GET_DATA_ALL.
        ELSE.
          PERFORM GET_DATA_BYPASS.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  SORT IT_ITAB BY EBELN.

ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_DATA.
*---
  DATA : L_INT.

  ULINE.

  LOOP AT IT_ITAB.
    NEW-LINE.
    PERFORM CHANGE_COLOR USING L_INT.
    CLEAR IT_ITAB-CHECK.
*    IF it_itab-lifnr EQ c_lifnr_g OR it_itab-lifnr EQ c_lifnr_h.
* by ig.moon 11/08/2007 {
    IF IT_ITAB-LIFNR EQ C_LIFNR_G OR IT_ITAB-LIFNR EQ C_LIFNR_H OR
       IT_ITAB-LIFNR EQ C_LIFNR_X.
* }
      WRITE :     IT_ITAB-CHECK AS CHECKBOX INPUT ON.
    ELSE.
      WRITE :     IT_ITAB-CHECK AS CHECKBOX INPUT OFF.
    ENDIF.
    WRITE :     IT_ITAB-WERKS,
                IT_ITAB-EBELN HOTSPOT,
                IT_ITAB-LIFNR,
                IT_ITAB-NAME1,
                IT_ITAB-EKGRP,
                IT_ITAB-EKNAM,
                IT_ITAB-AEDAT.
**    IF it_itab-flag EQ 'S'.         " Success
**      WRITE : (4) icon_green_light AS ICON.
**    ELSEIF it_itab-flag EQ 'R'.     " Send Success but Resend Error
**      WRITE : (4) icon_yellow_light AS ICON.
**    ELSE.     " IF it_itab-flag EQ 'E'.     " Error
**      WRITE : (4) icon_red_light AS ICON.
**    ENDIF.
**A__ IR CREATE BY PAUL

    SELECT SINGLE *
      FROM ZTREQHD
     WHERE EBELN EQ IT_ITAB-EBELN.

     IF SY-SUBRC = 0.
       IT_ITAB-ZRESULTC = 'S'.
       IT_ITAB-ZMSGC = 'I/R Created or already Exist.'.
     ENDIF.

    IF IT_ITAB-ZRESULTC EQ 'S'.         " Success
      WRITE : (4) ICON_GREEN_LIGHT AS ICON.
*      IT_ITAB-ZMSGC = 'Created'.
    ELSE.
      IF IT_ITAB-ZRESULTC EQ 'R' OR IT_ITAB-ZRESULTC EQ 'E'.
        " send error
        WRITE : (4) ICON_RED_LIGHT AS ICON.
*        IT_ITAB-ZMSGC = 'Error'.
      ELSE.
        IF IT_ITAB-ZRESULTC EQ 'P'.
          WRITE: (4) ICON_BUSINAV_VALUE_CHAIN AS ICON.
*          IT_ITAB-ZMSGC = 'Bypass'.

        ELSE.       " IF it_itab-ZRESULTC EQ ' '.     " Error
          WRITE : (4) ICON_YELLOW_LIGHT AS ICON.
          IT_ITAB-ZMSGC = 'Ready to Create'.
        ENDIF.
      ENDIF.
    ENDIF.
    WRITE : IT_ITAB-ZMSGC.

** CHANGED BY FURONG
    IF IT_ITAB-FLAG EQ 'S'.         " Success
      WRITE : (4) ICON_GREEN_LIGHT AS ICON.
      IT_ITAB-ZMSG = 'Sent'.
    ELSE.
      IF IT_ITAB-FLAG EQ 'R' OR IT_ITAB-FLAG EQ 'E'.
        " send error
        WRITE : (4) ICON_RED_LIGHT AS ICON.
        IT_ITAB-ZMSG = 'Error'.
      ELSE.
        IF IT_ITAB-FLAG EQ 'P'.
          WRITE: (4) ICON_BUSINAV_VALUE_CHAIN AS ICON.
          IT_ITAB-ZMSG = 'Bypass'.

        ELSE.       " IF it_itab-flag EQ ' '.     " Error
          WRITE : (4) ICON_YELLOW_LIGHT AS ICON.
          IT_ITAB-ZMSG = 'Ready to Send'.
        ENDIF.
      ENDIF.
    ENDIF.
    WRITE : IT_ITAB-ZMSG.

** END OF CHANGE
    HIDE : SY-TABIX, IT_ITAB.
    MODIFY IT_ITAB.
  ENDLOOP.

  ULINE.
ENDFORM.                    " write_data

*&---------------------------------------------------------------------*
*&      Form  write_top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_TOP_OF_PAGE.
*--- write information
**  WRITE : / icon_green_light  AS ICON, text-002,
**          / icon_yellow_light AS ICON, text-003,
**          / icon_red_light    AS ICON, text-004.

* changed by Furong

  IF P_ALL = 'X'.
    WRITE : / ICON_GREEN_LIGHT  AS ICON, TEXT-S02,
          / ICON_YELLOW_LIGHT AS ICON, TEXT-S03,
          / ICON_RED_LIGHT    AS ICON, TEXT-S04,
          / ICON_BUSINAV_VALUE_CHAIN AS ICON, 'Bypass'.
  ELSE.
    IF P_NOSEND = 'X'.
      WRITE : / ICON_YELLOW_LIGHT AS ICON, TEXT-S03.
    ELSE.
      IF P_SENT = 'X'.
        WRITE : / ICON_GREEN_LIGHT AS ICON, TEXT-S02.
      ELSE.
        IF P_ERROR = 'X'.
          WRITE : / ICON_RED_LIGHT AS ICON, TEXT-S04.
        ELSE.
          WRITE : / ICON_BUSINAV_VALUE_CHAIN AS ICON, 'Bypass'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

* end of change

*---
  ULINE.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  WRITE :   /3       TEXT-101,
                     TEXT-102,
                     TEXT-104,
                     TEXT-105,
                     TEXT-106,
                     TEXT-107,
                     TEXT-108,
                     TEXT-111,
                     TEXT-110.
ENDFORM.                    " write_top_of_page

*&---------------------------------------------------------------------*
*&      Form  select_deselect_all
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0170   text
*----------------------------------------------------------------------*
FORM SELECT_DESELECT_ALL USING    P_VALUE.
*---
  DO.
    READ LINE SY-INDEX FIELD VALUE : SY-TABIX IT_ITAB-CHECK.
    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.
    IF SY-TABIX GT 0.
      MODIFY CURRENT LINE FIELD VALUE IT_ITAB-CHECK FROM P_VALUE.
    ENDIF.
    CLEAR : SY-TABIX.
  ENDDO.
ENDFORM.                    " select_deselect_all

*&---------------------------------------------------------------------*
*&      Form  select_checked_po
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_CHECKED_PO.
*---
  DATA: W_TABIX LIKE SY-TABIX.

  DO.
    READ LINE SY-INDEX FIELD VALUE : SY-TABIX IT_ITAB-CHECK.
    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.
    W_TABIX = SY-TABIX + 1.
    IF IT_ITAB-CHECK NE SPACE AND SY-TABIX GT 0.
*     IF it_itab-check NE space AND w_tabix GT 0.
      READ TABLE IT_ITAB INDEX SY-TABIX.
*      READ TABLE it_itab INDEX w_tabix.

      MOVE : 'X' TO IT_ITAB-CHECK.
      MODIFY IT_ITAB INDEX SY-TABIX.

*      MODIFY it_itab INDEX w_tabix.

    ELSEIF IT_ITAB-CHECK EQ SPACE AND SY-TABIX GT 0.
*    ELSEIF it_itab-check EQ space AND w_tabix GT 0.

      READ TABLE IT_ITAB INDEX SY-TABIX.
*      READ TABLE it_itab INDEX w_tabix.

      MOVE : SPACE TO IT_ITAB-CHECK.
      MODIFY IT_ITAB INDEX SY-TABIX.
*      MODIFY it_itab INDEX w_tabix.

    ENDIF.
    CLEAR : IT_ITAB-CHECK, SY-TABIX, W_TABIX.
  ENDDO.

*---
  CLEAR : IT_TEMP, IT_TEMP[].

  LOOP AT IT_ITAB WHERE CHECK NE SPACE.
    MOVE-CORRESPONDING IT_ITAB TO IT_TEMP.
    APPEND IT_TEMP.
    CLEAR : IT_TEMP, IT_ITAB.
  ENDLOOP.
ENDFORM.                    " select_checked_po

*&---------------------------------------------------------------------*
*&      Form  display_po
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_PO.
*---
  CLEAR : IT_ITAB.

  READ TABLE IT_ITAB INDEX SY-TABIX.

  SET PARAMETER ID 'BES' FIELD IT_ITAB-EBELN.

  CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
ENDFORM.                    " display_po

*&---------------------------------------------------------------------*
*&      Form  get_data_to_be_sent
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_TO_BE_SENT.
*---
  SELECT DISTINCT
         B~WERKS
         A~EBELN
         A~LIFNR
         NAME1
         A~EKGRP
         EKNAM
         A~AEDAT
*         a~bedat
         A~KNUMV
*         flag
               INTO CORRESPONDING FIELDS OF TABLE IT_ITAB
               FROM EKKO AS A INNER JOIN EKPO AS B
                 ON A~MANDT EQ B~MANDT
                AND A~EBELN EQ B~EBELN
                    INNER JOIN LFA1 AS C
                       ON A~MANDT EQ C~MANDT
                      AND A~LIFNR EQ C~LIFNR
                          INNER JOIN T024 AS D
                             ON A~MANDT EQ D~MANDT
                            AND A~EKGRP EQ D~EKGRP
          WHERE A~BSART EQ C_BSART
            AND A~LIFNR IN S_LIFNR
            AND A~EKGRP IN S_EKGRP
            AND A~EBELN IN S_EBELN
            AND A~BEDAT IN S_BEDAT
            AND B~WERKS IN S_WERKS
*            AND b~matkl IN s_matkl
            AND A~LOEKZ EQ SPACE
            AND B~LOEKZ EQ SPACE
            AND NOT EXISTS
                ( SELECT EBELN FROM ZTMM_KD_PO
                              WHERE EBELN EQ A~EBELN ).
ENDFORM.                    " get_data_to_be_sent

*&---------------------------------------------------------------------*
*&      Form  get_data_all
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_ALL.
*---
  SELECT DISTINCT
         B~WERKS
         A~EBELN
         A~LIFNR
         NAME1
         A~EKGRP
         EKNAM
         A~AEDAT
*         a~bedat
         A~KNUMV
         FLAG
               INTO CORRESPONDING FIELDS OF TABLE IT_ITAB
               FROM EKKO AS A INNER JOIN EKPO AS B
                 ON A~MANDT EQ B~MANDT
                AND A~EBELN EQ B~EBELN
                    INNER JOIN LFA1 AS C
                       ON A~MANDT EQ C~MANDT
                      AND A~LIFNR EQ C~LIFNR
                          INNER JOIN T024 AS D
                             ON A~MANDT EQ D~MANDT
                            AND A~EKGRP EQ D~EKGRP
                                LEFT OUTER JOIN ZTMM_KD_PO AS E
                                  ON A~MANDT EQ E~MANDT
                                 AND A~EBELN EQ E~EBELN
          WHERE A~BSART EQ C_BSART
            AND A~LIFNR IN S_LIFNR
            AND A~EKGRP IN S_EKGRP
            AND A~EBELN IN S_EBELN
            AND A~BEDAT IN S_BEDAT
            AND B~WERKS IN S_WERKS
*            AND b~matkl IN s_matkl
            AND A~LOEKZ EQ SPACE
            AND B~LOEKZ EQ SPACE.
ENDFORM.                    " get_data_all


*---------------------------------------------------------------------*
*       FORM get_data_sent                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_DATA_SENT
.
*---
  SELECT DISTINCT
         B~WERKS
         A~EBELN
         A~LIFNR
         NAME1
         A~EKGRP
         EKNAM
         A~AEDAT
*         a~bedat
         A~KNUMV
         A~KNUMV
         FLAG
               INTO CORRESPONDING FIELDS OF TABLE IT_ITAB
               FROM EKKO AS A INNER JOIN EKPO AS B
                 ON A~MANDT EQ B~MANDT
                AND A~EBELN EQ B~EBELN
                    INNER JOIN LFA1 AS C
                       ON A~MANDT EQ C~MANDT
                      AND A~LIFNR EQ C~LIFNR
                          INNER JOIN T024 AS D
                             ON A~MANDT EQ D~MANDT
                            AND A~EKGRP EQ D~EKGRP
                                  JOIN ZTMM_KD_PO AS E
                                  ON A~MANDT EQ E~MANDT
                                 AND A~EBELN EQ E~EBELN
          WHERE A~BSART EQ C_BSART
            AND A~LIFNR IN S_LIFNR
            AND A~EKGRP IN S_EKGRP
            AND A~EBELN IN S_EBELN
            AND A~BEDAT IN S_BEDAT
            AND B~WERKS IN S_WERKS
*            AND b~matkl IN s_matkl
            AND E~FLAG = 'S'
            AND A~LOEKZ EQ SPACE
            AND B~LOEKZ EQ SPACE.
ENDFORM.                    " get_data_sent


*---------------------------------------------------------------------*
*       FORM get_data_error                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_DATA_ERROR.
*---
  SELECT DISTINCT
         B~WERKS
         A~EBELN
         A~LIFNR
         NAME1
         A~EKGRP
         EKNAM
         A~AEDAT
*         a~bedat
         A~KNUMV
         FLAG
               INTO CORRESPONDING FIELDS OF TABLE IT_ITAB
               FROM EKKO AS A INNER JOIN EKPO AS B
                 ON A~MANDT EQ B~MANDT
                AND A~EBELN EQ B~EBELN
                    INNER JOIN LFA1 AS C
                       ON A~MANDT EQ C~MANDT
                      AND A~LIFNR EQ C~LIFNR
                          INNER JOIN T024 AS D
                             ON A~MANDT EQ D~MANDT
                            AND A~EKGRP EQ D~EKGRP
                                  JOIN ZTMM_KD_PO AS E
                                  ON A~MANDT EQ E~MANDT
                                 AND A~EBELN EQ E~EBELN
          WHERE A~BSART EQ C_BSART
            AND A~LIFNR IN S_LIFNR
            AND A~EKGRP IN S_EKGRP
            AND A~EBELN IN S_EBELN
            AND A~BEDAT IN S_BEDAT
            AND B~WERKS IN S_WERKS
*            AND b~matkl IN s_matkl
            AND ( E~FLAG = 'E' OR E~FLAG = 'R' )
            AND A~LOEKZ EQ SPACE
            AND B~LOEKZ EQ SPACE.
ENDFORM.                    " get_data_error

*---------------------------------------------------------------------*
*       FORM get_data_bypass                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_DATA_BYPASS.
*---
  SELECT DISTINCT
         B~WERKS
         A~EBELN
         A~LIFNR
         NAME1
         A~EKGRP
         EKNAM
         A~AEDAT
*         a~bedat
         A~KNUMV
         FLAG
               INTO CORRESPONDING FIELDS OF TABLE IT_ITAB
               FROM EKKO AS A INNER JOIN EKPO AS B
                 ON A~MANDT EQ B~MANDT
                AND A~EBELN EQ B~EBELN
                    INNER JOIN LFA1 AS C
                       ON A~MANDT EQ C~MANDT
                      AND A~LIFNR EQ C~LIFNR
                          INNER JOIN T024 AS D
                             ON A~MANDT EQ D~MANDT
                            AND A~EKGRP EQ D~EKGRP
                                  JOIN ZTMM_KD_PO AS E
                                  ON A~MANDT EQ E~MANDT
                                 AND A~EBELN EQ E~EBELN
          WHERE A~BSART EQ C_BSART
            AND A~LIFNR IN S_LIFNR
            AND A~EKGRP IN S_EKGRP
            AND A~EBELN IN S_EBELN
            AND A~BEDAT IN S_BEDAT
            AND B~WERKS IN S_WERKS
*            AND b~matkl IN s_matkl
            AND  E~FLAG = 'P'
            AND A~LOEKZ EQ SPACE
            AND B~LOEKZ EQ SPACE.
ENDFORM.                    " get_data_bypass


*&---------------------------------------------------------------------*
*&      Form  change_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_INT  text
*----------------------------------------------------------------------*
FORM CHANGE_COLOR USING    P_L_INT.
*---
  IF P_L_INT EQ SPACE.
    FORMAT COLOR 2 INTENSIFIED OFF.
    MOVE : 'X' TO P_L_INT.
  ELSE.
    FORMAT COLOR 2 INTENSIFIED ON.
    CLEAR : P_L_INT.
  ENDIF.
ENDFORM.                    " change_color

*&---------------------------------------------------------------------*
*&      Form  call_message_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_MESSAGE_SCREEN.
*---
  CALL SCREEN 9000 STARTING AT 30 10
                   ENDING   AT 60 14.
ENDFORM.                    " call_message_screen

*&---------------------------------------------------------------------*
*&      Module  status_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
*---
  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000'.
ENDMODULE.                 " status_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
*---
  CASE SY-UCOMM.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " exit  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
*---
  CASE SY-UCOMM.
    WHEN 'ENTER'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT

*&---------------------------------------------------------------------*
*&      Form  poitem_check
*&---------------------------------------------------------------------*
FORM POITEM_CHECK.
  CLEAR: IT_EKPO, IT_EKPO[].
*  CLEAR: IT_MLGN, IT_MLGN[].
  DATA W_WAREHOUSE_FLAG(1).

  SELECT * INTO TABLE IT_EKPO FROM EKPO FOR ALL ENTRIES IN IT_ITAB
                                     WHERE EBELN = IT_ITAB-EBELN
                                       AND LOEKZ EQ SPACE
                                       AND ELIKZ EQ SPACE.
** 10/28/13  not use WM module
*  SELECT * INTO TABLE IT_MLGN FROM MLGN FOR ALL ENTRIES IN IT_EKPO
*                                      WHERE MATNR = IT_EKPO-MATNR
*                                        AND LVORM EQ SPACE.
*  DELETE ADJACENT DUPLICATES FROM IT_MLGN.
*
  SORT IT_EKPO BY EBELN MATNR.
  LOOP AT IT_TEMP.
    LOOP AT IT_EKPO WHERE EBELN = IT_TEMP-EBELN.
      W_TABIX = SY-TABIX + 1.
*C__ PAUL : P400 -> G100
*      IF IT_EKPO-LGORT = 'P400'.   " check warehouse
** 10/28/13  not use WM module
*      IF IT_EKPO-LGORT = 'G100'.   " check warehouse
**E<06/16/11
*        READ TABLE IT_MLGN WITH KEY MATNR = IT_EKPO-MATNR.
**                                    LGNUM = 'P01'.
*        IF SY-SUBRC NE 0.
*          W_WAREHOUSE_FLAG = '1'.
*          W_MATNRERR = IT_EKPO-MATNR.
*          W_ITEMNO = IT_EKPO-EBELP.
*        ENDIF.
*      ENDIF.
** End on 10/28/13
      IF IT_EKPO-MWSKZ <> 'U0'.
        W_TAXCODE = '1'.
        W_MATNRERR = IT_EKPO-MATNR.
        W_ITEMNO = IT_EKPO-EBELP.
      ENDIF.
      IF IT_EKPO-WEBRE <> ' '.
        W_GRINV = '1'.
        W_MATNRERR = IT_EKPO-MATNR.
        W_ITEMNO = IT_EKPO-EBELP.
      ENDIF.
      READ TABLE IT_EKPO INTO WA_EKPO INDEX W_TABIX.
      IF SY-SUBRC = 0 AND ( WA_EKPO-MATNR EQ IT_EKPO-MATNR )
                      AND WA_EKPO-EBELN = IT_EKPO-EBELN.
        W_MULTIPO = '1'.
        W_MATNRERR = IT_EKPO-MATNR.
        W_ITEMNO = IT_EKPO-EBELP.
      ENDIF.
    ENDLOOP.
    IF W_MULTIPO = '1'.
      CLEAR W_MULTIPO.
      MESSAGE I999 WITH TEXT-M10 W_MATNRERR IT_TEMP-EBELN.
      DELETE IT_TEMP.
      CONTINUE.
    ENDIF.
    IF W_TAXCODE = '1'.
      CLEAR W_TAXCODE.
      MESSAGE I999 WITH TEXT-M11 W_MATNRERR IT_TEMP-EBELN W_ITEMNO.
      DELETE IT_TEMP.
      CONTINUE.
    ENDIF.
    IF W_GRINV = '1'.
      CLEAR W_GRINV.
      MESSAGE I999 WITH TEXT-M12 W_MATNRERR IT_TEMP-EBELN.
      DELETE IT_TEMP.
      CONTINUE.
    ENDIF.
    IF W_WAREHOUSE_FLAG = '1'.
      CLEAR W_WAREHOUSE_FLAG.
      MESSAGE I999 WITH TEXT-M13 W_MATNRERR IT_TEMP-EBELN.
      DELETE IT_TEMP.
      CONTINUE.
    ENDIF.
  ENDLOOP.
  CLEAR W_TABIX.
ENDFORM.                    " poitem_check
*&---------------------------------------------------------------------*
*&      Form  warehouse_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  CREATE_IR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_IR.

  LOOP AT IT_TEMP.

    REFRESH : BDCDATA, IT_MESSAGE.
    CLEAR : BDCDATA, IT_MESSAGE.

    PERFORM BDC_DYNPRO_PROCESSING USING:
                        'X' 'SAPMZIM00'               '0100',
                        ' ' 'BDC_OKCODE'              '=ENTR' ,
                        ' ' 'ZSREQHD-EBELN'           IT_TEMP-EBELN.

    PERFORM BDC_DYNPRO_PROCESSING USING:
                        'X' 'SAPMZIM00'               '0131',
                        ' ' 'BDC_OKCODE'              '=SAVE' .

    PERFORM BDC_DYNPRO_PROCESSING USING:
                        'X' 'SAPMZIM00'               '0001',
                        ' ' 'BDC_OKCODE'              '=YES' .

    CALL TRANSACTION 'ZIM01' USING BDCDATA MODE 'N'
                     MESSAGES INTO IT_MESSAGE.

    READ TABLE IT_MESSAGE WITH KEY MSGTYP = 'E'.

    IF SY-SUBRC = 0.
      IT_TEMP-ZRESULTC = 'E'.
    ELSE.
      IT_TEMP-ZRESULTC = 'S'.
    ENDIF.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              MSGID               = IT_MESSAGE-MSGID
              MSGNR               = IT_MESSAGE-MSGNR
              MSGV1               = IT_MESSAGE-MSGV1
              MSGV2               = IT_MESSAGE-MSGV2
              MSGV3               = IT_MESSAGE-MSGV3
              MSGV4               = IT_MESSAGE-MSGV4
         IMPORTING
              MESSAGE_TEXT_OUTPUT = IT_TEMP-ZMSGC.

    MODIFY IT_TEMP.

    READ TABLE IT_ITAB WITH KEY EBELN = IT_TEMP-EBELN.

    IT_ITAB-ZRESULTC = IT_TEMP-ZRESULTC.
*    IF IT_ITAB-ZRESULTC = 'E'.
*      IT_ITAB-ZMSGC = 'Error'.
*    ELSE.
*      IT_ITAB-ZMSGC = 'Success'.
*    ENDIF.

    IT_ITAB-ZMSGC    = IT_TEMP-ZMSGC.
    MODIFY IT_ITAB TRANSPORTING ZRESULTC ZMSGC
                               WHERE EBELN EQ IT_TEMP-EBELN.

  ENDLOOP.

ENDFORM.                    " CREATE_IR
*
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2635   text
*      -->P_2636   text
*      -->P_2637   text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO_PROCESSING  USING   DY_BEGIN  PG_NAME   SC_NO.
  IF DY_BEGIN = 'X'.
    CLEAR BDCDATA.
    MOVE  PG_NAME  TO BDCDATA-PROGRAM.
    MOVE  SC_NO    TO BDCDATA-DYNPRO.
    MOVE  'X'      TO BDCDATA-DYNBEGIN.
    APPEND BDCDATA.
  ELSE.
    CLEAR BDCDATA.
    MOVE  PG_NAME  TO BDCDATA-FNAM.
    MOVE  SC_NO    TO BDCDATA-FVAL.
    APPEND BDCDATA.
  ENDIF.
ENDFORM.                    " bdc_dynpro_processing
