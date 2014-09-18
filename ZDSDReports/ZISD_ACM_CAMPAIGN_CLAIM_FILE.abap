************************************************************************
* Program Name      : ZISD_ACM_CAMPAIGN_CLAIM_FILE
* Author            : Haseeb Mohammad
* Creation Date     : 2007-01-23
* Specifications By : Lance Younce
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :
*
*
* Modification Logs
* Date      Developer        Ticket/Transport      Description
* 03/08/2007 Haseeb Mohammad  738B475174/UD1K931011 Add Vin, Serial in
**                                                 selection criteria
*
************************************************************************

REPORT ZISD_ACM_CAMPAIGN_CLAIM_FILE MESSAGE-ID ZMSD.

TABLES : ZTSD_ACM_H,
         TCURR.

DATA : BEGIN OF IT_FILE OCCURS 0,
       RECORD(140),
       END OF IT_FILE.

DATA : BEGIN OF IT_ACM_H OCCURS 0.
        INCLUDE STRUCTURE ZTSD_ACM_H.
DATA : END OF IT_ACM_H.
DATA : REC_CNT TYPE I.

DATA : BEGIN OF IT_ACM OCCURS 0,
*        KDPLN(5) TYPE C VALUE 'B28MM',
*        ZACLN(7) TYPE C, " LIKE ZTSD_ACM_H-ZACLN,
*        ZCDST(5) TYPE C, "LIKE ZTSD_ACM_H-ZCDST,
*        ZCDLR(5) TYPE C, "LIKE ZTSD_ACM_H-ZCDLR,
*        ZCSER(6) TYPE C, "LIKE ZTSD_ACM_H-ZCSER,
*        C1FLAG
*        ZRCST TYPE C, "LIKE ZTSD_ACM_H-ZRCST,
*        ZCTYP TYPE C,"LIKE ZTSD_ACM_H-ZCTYP,
*        ZCSEQ(2) TYPE C, "LIKE ZTSD_ACM_H-ZCSEQ,
*        ZVIN(17) TYPE C,  "LIKE ZTSD_ACM_H-ZVIN,
*        ZVSFG TYPE C, "LIKE ZTSD_ACM_H-ZVSFG,
*        ZDLVY(8) TYPE C,"LIKE ZTSD_ACM_H-ZDLVY,
*        ZRPDT(8) TYPE C, "LIKE ZTSD_ACM_H-ZRPDT,
*        ZODRD(6) TYPE C, "LIKE ZTSD_ACM_H-ZODRD,
*        ZCPIS(8) TYPE C, "LIKE ZTSD_ACM_H-ZCPIS,
*        ZSBPP(10) TYPE C, "LIKE ZTSD_ACM_H-ZSBPP,
*        ZSBLL(10) TYPE C, "LIKE ZTSD_ACM_H-ZSBLL,
*        ZSBSS(10) TYPE C, "LIKE ZTSD_ACM_H-ZSBSS,
*        ZPYCR(3) TYPE C, "LIKE ZTSD_ACM_H-ZPYCR,
*        CONVE(11) TYPE C, "LIKE TCURR-UKURS,
*        CODE(7) TYPE C,
         C1HMCB(5)   TYPE C VALUE 'B28MM',
         C1HACL(7)   TYPE C,
         C1DIST(5)   TYPE C,
         C1DLRC(5)   TYPE C,
         C1CSER(6)   TYPE C,
         C1FLAG(1)   TYPE C VALUE ' ',
         C1TYPE(1)   TYPE C,
         C1LINE(2)   TYPE C,
         C1VIN(17)   TYPE C,
         C1VSFG(1)   TYPE C,
         C1DLDT(8)   TYPE C,
         C1WODT(8)   TYPE C,
         C1WOML(6)   TYPE C,
         C1ISSN(8)   TYPE C,
         C1PAMS(7)   TYPE C,
         C1LAMS(7)   TYPE C,
         C1SAMS(7)   TYPE C,
         C1CRCY(3)   TYPE C VALUE '   ',
         C1EXCH(6)   TYPE C VALUE '000000',
         C1ACL(7)    TYPE C,
         C1FILL(33)  TYPE C,
        END OF IT_ACM.

SELECTION-SCREEN BEGIN OF BLOCK B1.

*PARAMETERS : P_FILE LIKE RLGRAP-FILENAME OBLIGATORY.
* PARAMETERS : IT_ZACLN LIKE ZTSD_ACM_H-ZACLN,
*              IT_ZCDST LIKE ZTSD_ACM_H-ZCDST,
*              IT_ZCDLR LIKE ZTSD_ACM_H-ZCDLR,
*              IT_ZRCST LIKE ZTSD_ACM_H-ZRCST,
*              IT_ZCPIS LIKE ZTSD_ACM_H-ZCPIS,
*              IT_ZCTYP LIKE ZTSD_ACM_H-ZCTYP,
*              IT_ZDLVY LIKE ZTSD_ACM_H-ZDLVY,
*              IT_ZRPDT LIKE ZTSD_ACM_H-ZRPDT.

SELECTION-SCREEN END OF BLOCK B1.
SELECT-OPTIONS IT_ZACLN FOR ZTSD_ACM_H-ZACLN NO INTERVALS.
SELECT-OPTIONS IT_ZCDST FOR ZTSD_ACM_H-ZCDST NO INTERVALS.
SELECT-OPTIONS IT_ZCDLR FOR ZTSD_ACM_H-ZCDLR NO INTERVALS.
*SELECT-OPTIONS IT_ZRCST FOR ZTSD_ACM_H-ZRCST NO INTERVALS.
SELECT-OPTIONS IT_ZCPIS FOR ZTSD_ACM_H-ZCPIS NO INTERVALS.
SELECT-OPTIONS IT_ZCTYP FOR ZTSD_ACM_H-ZCTYP NO INTERVALS.
SELECT-OPTIONS IT_ZDLVY FOR ZTSD_ACM_H-ZDLVY NO INTERVALS.
SELECT-OPTIONS IT_ZRPDT FOR ZTSD_ACM_H-ZRPDT NO INTERVALS.
*738B475174/UD1K931011 BEGIN
SELECT-OPTIONS IT_ZVIN FOR ZTSD_ACM_H-ZVIN   NO INTERVALS.
SELECT-OPTIONS IT_ZCSER FOR ZTSD_ACM_H-ZCSER NO INTERVALS.
*738B475174/UD1K931011  END
SELECT-OPTIONS IT_ZERDA FOR ZTSD_ACM_H-ZERDA .
*PARAMETERS : P_CHK AS CHECKBOX.

INITIALIZATION.


START-OF-SELECTION.

  PERFORM GET_ACM_DATA.
  PERFORM DISPLAY_DATA.
  IF SY-BATCH = 'X'.
    PERFORM WRITE_FILE.
  ENDIF.
  SET PF-STATUS 'SCREEN1000'.
  SET TITLEBAR 'TITLE'.

END-OF-SELECTION.

AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'SAVE'.
      PERFORM WRITE_FILE.
  ENDCASE.


*&---------------------------------------------------------------------*
*&      Form  GET_ACM_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_ACM_DATA.

  DATA : ZUKURS LIKE TCURR-UKURS,
         F(7) TYPE C VALUE '0000000',
         TEMPDATE(7) TYPE C.


*   SELECT SINGLE UKURS INTO ZUKURS FROM TCURR
*          WHERE FCURR = 'CAD'
*          AND TCURR ='USD'
*          AND GDATU = ( SELECT MIN( GDATU ) FROM TCURR ).



  SELECT ZACLN ZCDST ZCDLR ZCSER ZRCST ZCTYP ZCSEQ
         ZVIN ZVSFG ZDLVY ZRPDT ZODRD ZCPIS ZSBPP
         ZSBLL ZSBSS ZPYCR ZRMPP ZRMLL ZRMSS
         FROM ZTSD_ACM_H
         INTO CORRESPONDING FIELDS OF TABLE IT_ACM_H
         WHERE ZACLN IN IT_ZACLN AND
               ZCDST IN IT_ZCDST AND
               ZCDLR IN IT_ZCDLR AND
               ZCSER IN IT_ZCSER AND  "738B475174/UD1K931011
               ZCPIS IN IT_ZCPIS AND
               ZCTYP IN IT_ZCTYP AND
               ZVIN  IN IT_ZVIN  AND  "738B475174/UD1K931011
               ZDLVY IN IT_ZDLVY AND
               ZRPDT IN IT_ZRPDT AND
               ZERDA IN IT_ZERDA.
  CONCATENATE SY-DATUM+2(4) 'MC1' INTO TEMPDATE.
*  TEMPDATE = SY-DATUM+2(4).
  IF SY-SUBRC = 0.
    LOOP AT IT_ACM_H.

*      IT_ACM-C1HACL =  IT_ACM_H-ZACLN+0(7).
*      CONCATENATE TEMPDATE 'MC1' INTO IT_ACM-C1HACL.
       IT_ACM-C1HACL = TEMPDATE.
      IT_ACM-C1DIST =  IT_ACM_H-ZCDST+0(5).
      IT_ACM-C1DLRC =  IT_ACM_H-ZCDLR+0(5).
      IT_ACM-C1CSER =  IT_ACM_H-ZCSER.
      IT_ACM-C1TYPE =  IT_ACM_H-ZCTYP.
      IT_ACM-C1LINE =  IT_ACM_H-ZCSEQ.
      IT_ACM-C1VIN  =  IT_ACM_H-ZVIN+0(17).
      IT_ACM-C1VSFG =  IT_ACM_H-ZVSFG.
      IT_ACM-C1DLDT =  IT_ACM_H-ZDLVY.
      IT_ACM-C1WODT =  IT_ACM_H-ZRPDT.
      IT_ACM-C1WOML =  IT_ACM_H-ZODRD.
      IT_ACM-C1ISSN =  IT_ACM_H-ZCPIS.

      IT_ACM-C1PAMS =  IT_ACM_H-ZRMPP * 100.
      PERFORM ADD_LEADZERO USING IT_ACM-C1PAMS.
      IT_ACM-C1LAMS =  IT_ACM_H-ZRMLL * 100.
      PERFORM ADD_LEADZERO USING IT_ACM-C1LAMS.
      IT_ACM-C1SAMS =  IT_ACM_H-ZRMSS * 100.
      PERFORM ADD_LEADZERO USING IT_ACM-C1SAMS.
      IT_ACM-C1CRCY =  '   '.
      IT_ACM-C1EXCH =  '000000'.

      IT_ACM-C1ACL =  IT_ACM_H-ZACLN+0(7).
      APPEND IT_ACM.
    ENDLOOP.
*    PERFORM WRITE_FILE.
  ELSE.
    MESSAGE I000 WITH 'NO DATA FOUND FOR THE SELECTION'.
     WRITE: / 'NO DATA FOUND FOR THE SELECTION,'.
     WRITE: / 'PLEASE CHANGE SELECTION AND TRY'.
  ENDIF.



ENDFORM.                    " GET_ACM_DATA
*&---------------------------------------------------------------------*
*&      Form  WRITE_FILE
*&---------------------------------------------------------------------*
FORM WRITE_FILE.

*ZISD05U_ACM_UPLOAD_WS.

  DATA : P_FILE LIKE RLGRAP-FILENAME,
         P_PATH  like filenameci-pathintern.


  P_PATH = '/usr/sap/EDI_SAP/'.
  CONCATENATE P_PATH 'HM_CAMPCLM_' SY-DATUM '.TXT' INTO P_FILE.
  OPEN DATASET P_file FOR OUTPUT IN TEXT MODE.
  IF SY-SUBRC <> 0.
    WRITE: / 'ERROR IN CREATING A FILE', SY-SUBRC.
    STOP.
  ENDIF.
  LOOP AT IT_ACM.
    TRANSFER IT_ACM TO P_FILE.

    if sy-subrc <> 0.
      write: /'***ERROR writing to file', p_filE, 'rc=', sy-subrc.
      write: /'Record:'.
      write: / p_file.
      stop.
    endif.
  ENDLOOP.
  CLOSE DATASET p_file.
  if sy-subrc <> 0.
    write: /'***ERROR closing file', p_file, 'rc=', sy-subrc.
    stop.
  else.
    DESCRIBE TABLE IT_ACM LINES REC_CNT.
    WRITE : / 'FILE', P_FILE, 'CREATED SUCESSFULLY'.
    WRITE : / 'TOTAL NUMBER OF RECORDS :' ,REC_CNT.
  endif.


ENDFORM.                    " WRITE_FILE
*&---------------------------------------------------------------------*
*&      Form  WRITE_FILE
*&---------------------------------------------------------------------*
FORM ADD_LEADZERO USING    P_FIELD.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = P_FIELD
       IMPORTING
            OUTPUT = P_FIELD.

ENDFORM.                   " ADD_LEADZERO
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM DISPLAY_DATA.
  LOOP AT IT_ACM.
    WRITE: / IT_ACM.
  ENDLOOP.
  DESCRIBE TABLE IT_ACM LINES REC_CNT.
  MESSAGE S000 WITH 'TOTAL RECORDS :' REC_CNT.
ENDFORM.                    " DISPLAY_DATA
