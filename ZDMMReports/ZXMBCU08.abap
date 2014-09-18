*&---------------------------------------------------------------------*
* - update reservation
*&---------------------------------------------------------------------*
*&  Include           ZXMBCU08
*&---------------------------------------------------------------------*
*"      TI_RKPF STRUCTURE  RKPF
*"      TI_RESB_NEW STRUCTURE  RESB
*"      TI_RESB_OLD STRUCTURE  RESB
*"      TI_DM07R STRUCTURE  DM07R
*"      TI_RESB_ADDED STRUCTURE  RESBN OPTIONAL
 DATA: LS_RESB LIKE RESB.
 DATA: I_BODY  LIKE ZMMT0038.

*-- Send Feeding Order to GCS interface
 LOOP AT TI_RESB_NEW  INTO LS_RESB.

   IF NOT LS_RESB-XLOEK IS INITIAL.
     I_BODY-PKKEY     = '999'.
     I_BODY-REVERSED  = 'X'.
** Added by furong on 10/06/11
     MOVE 'Reservation cancelled'
             TO I_BODY-MESSAGE.
** end on 10/06/11
   ELSE.
     I_BODY-PKKEY     = '998'.
     I_BODY-REVERSED  = ' '.
   ENDIF.

   I_BODY-RSNUM  = LS_RESB-RSNUM.
   I_BODY-RSPOS  = LS_RESB-RSPOS.
   I_BODY-MATNR  = LS_RESB-MATNR.
   I_BODY-WERKS  = LS_RESB-WERKS.
*   i_body-prvbe  = ls_resb-prvbe.
   I_BODY-LGORT  = LS_RESB-LGORT.
   I_BODY-LGPRO  = LS_RESB-UMLGO.
   I_BODY-PRVBE  = LS_RESB-PRVBE.
   I_BODY-PKBMG  = LS_RESB-BDMNG.
   I_BODY-MEINS  = LS_RESB-MEINS.

   I_BODY-SAEDT  = LS_RESB-BDTER.
   I_BODY-SAEUZ  = LS_RESB-BDZTP.
   I_BODY-ETNAM  = SY-UNAME.
   I_BODY-ETDAT  = SY-DATUM.
   I_BODY-ETTIM  = SY-UZEIT.
**PAUL#1
   SELECT SINGLE PRVBE ZFEEDER ABLAD
     INTO (I_BODY-PRVBE, I_BODY-ZFEEDER, I_BODY-ABLAD)
     FROM PKHD
    WHERE WERKS = I_BODY-WERKS
      AND MATNR = I_BODY-MATNR.

*   SELECT SINGLE ferth formt
*          INTO (i_body-ferth, i_body-formt)
*          FROM mara
*          WHERE matnr = ls_resb-matnr.

   CALL FUNCTION 'Z_MM_IF_OB_02_003_DB'
        EXPORTING
             I_BODY = I_BODY.

   COMMIT WORK.
 ENDLOOP.
