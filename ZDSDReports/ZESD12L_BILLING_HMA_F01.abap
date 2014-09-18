*----------------------------------------------------------------------*
*   INCLUDE ZESD12L_BILLING_HMA_F01                                    *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  REFRESH : IT_VKDFS, R_KUNAG, W_KUNNR.
  CLEAR   : IT_VKDFS, R_KUNAG, W_KUNNR.

  R_KUNAG-SIGN = 'I'.
  R_KUNAG-OPTION = 'BT'.

  R_KUNAG-LOW  = 'B28AA'.
  R_KUNAG-HIGH = 'B28ZZ'.
  APPEND R_KUNAG.

  SELECT *
         INTO TABLE IT_VKDFS
         FROM VKDFS
        WHERE FKTYP EQ 'L'
        AND   FKDAT IN S_FKDAT
        AND   KUNNR IN R_KUNAG.

  LOOP AT IT_VKDFS.
    W_KUNNR-KUNNR = IT_VKDFS-KUNNR.
    COLLECT W_KUNNR. CLEAR W_KUNNR.
  ENDLOOP.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
FORM PROCESS_DATA.
  DATA : W_FKDAT(8),
         W_FKDAB(8).

  REFRESH : BDC_TAB, MESS_TAB.
  CLEAR   : BDC_TAB, MESS_TAB.

  DESCRIBE TABLE IT_VKDFS LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M01.
    EXIT.
  ELSE.
    MESSAGE I000 WITH W_CNT TEXT-M02.
  ENDIF.

  DESCRIBE TABLE W_KUNNR LINES W_CNT. "SPRIT OR NOT

  IF S_FKDAT-HIGH IS INITIAL.
    S_FKDAT-HIGH = S_FKDAT-LOW.
  ENDIF.

  SELECT SINGLE *
         FROM USR01
        WHERE BNAME = SY-UNAME.
  CASE USR01-DATFM.
    WHEN '1'. "DD.MM.YYYY
      W_FKDAT+4(4) = S_FKDAT-LOW+0(4).
      W_FKDAT+2(2) = S_FKDAT-LOW+4(2).
      W_FKDAT+0(2) = S_FKDAT-LOW+6(2).
      W_FKDAB+4(4) = S_FKDAT-HIGH+0(4).
      W_FKDAB+2(2) = S_FKDAT-HIGH+4(2).
      W_FKDAB+0(2) = S_FKDAT-HIGH+6(2).
    WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
      W_FKDAT+4(4) = S_FKDAT-LOW+0(4).
      W_FKDAT+0(2) = S_FKDAT-LOW+4(2).
      W_FKDAT+2(2) = S_FKDAT-LOW+6(2).
      W_FKDAB+4(4) = S_FKDAT-HIGH+0(4).
      W_FKDAB+0(2) = S_FKDAT-HIGH+4(2).
      W_FKDAB+2(2) = S_FKDAT-HIGH+6(2).
  ENDCASE.

** : BDC Logic modify(12/07/2011 BY KDM) - Start UD1K953451
*  PERFORM BDC_FILL USING :
*          'X' 'SDBILLDL'             '1000',
*          ' ' 'P_FKDAT'              W_FKDAT,
*          ' ' 'P_FKDAB'              W_FKDAB,
*          ' ' 'P_KUNNR-LOW'          'B28*',
*          ' ' 'BDC_OKCODE'           '=ONLI',
*          'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*          ' ' 'BDC_OKCODE'           '=&ALL',
*          'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*          ' ' 'BDC_OKCODE'           '=SAMH'.
*  IF W_CNT = 1. "SPRIT OR NOT
*    PERFORM BDC_FILL USING :
*            'X' 'SAPMV60A'             '0104',
*            ' ' 'BDC_OKCODE'           '=SICH'.
*  ELSE.
*    PERFORM BDC_FILL USING :
*            'X' 'SAPMV60A'             '0103',
*            ' ' 'BDC_OKCODE'           '=SICH'. "SPRIT
*  ENDIF.
*  PERFORM BDC_FILL USING :
*          'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*          ' ' 'BDC_OKCODE'           '=&F03',
*          'X' 'SDBILLDL'             '1000',
*          ' ' 'BDC_OKCODE'           '/EERW'.

  PERFORM BDC_FILL USING :
          'X' 'SDBILLDL'             '1000',
          ' ' 'P_FKDAT'              W_FKDAT,
          ' ' 'P_FKDAB'              W_FKDAB,
          ' ' 'P_KUNNR-LOW'          'B28*',
          ' ' 'P_ALLEL'              'X',
          ' ' 'BDC_OKCODE'           '=ONLI',
          'X' 'SAPMSSY0'             '0120',
          ' ' 'BDC_OKCODE'           '=SAMH'.
  IF W_CNT = 1. "SPRIT OR NOT
    PERFORM BDC_FILL USING :
            'X' 'SAPMV60A'             '0104',
            ' ' 'BDC_OKCODE'           '=SICH'.
  ELSE.
    PERFORM BDC_FILL USING :
            'X' 'SAPMV60A'             '0103',
            ' ' 'BDC_OKCODE'           '=SICH'. "SPRIT
  ENDIF.
  PERFORM BDC_FILL USING :
          'X' 'SAPMSSY0'             '0120',
          ' ' 'BDC_OKCODE'           '=&F03',
          'X' 'SDBILLDL'             '1000',
          ' ' 'BDC_OKCODE'           '/EERW'.
** : BDC Logic modify(12/07/2011 BY KDM) - End  UD1K953451

  CALL TRANSACTION 'VF04' USING BDC_TAB MODE WWW "'N'
                                UPDATE 'S'
                                MESSAGES INTO MESS_TAB.

  SKIP 1.
  LOOP AT MESS_TAB.
    WRITE:/ MESS_TAB-MSGTYP, MESS_TAB-MSGID.
    CLEAR W_RESULT_MSG.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
           MSGID               = MESS_TAB-MSGID
           MSGNR               = MESS_TAB-MSGNR
           MSGV1               = MESS_TAB-MSGV1
           MSGV2               = MESS_TAB-MSGV2
           MSGV3               = MESS_TAB-MSGV3
           MSGV4               = MESS_TAB-MSGV4
         IMPORTING
           MESSAGE_TEXT_OUTPUT = W_RESULT_MSG.
    WRITE: W_RESULT_MSG.
  ENDLOOP.

  SKIP.
  LOOP AT IT_VKDFS.
    WRITE:/ IT_VKDFS-FKTYP,
            IT_VKDFS-VKORG,
            IT_VKDFS-FKDAT,
            IT_VKDFS-KUNNR,
            IT_VKDFS-FKART,
*           IT_VKDFS-LLAND,
            IT_VKDFS-VBELN,
*           IT_VKDFS-VBTYP,
*           IT_VKDFS-ADRNR,
            IT_VKDFS-SORTKRI,
*           IT_VKDFS-FAKSK,
            IT_VKDFS-VTWEG,
            IT_VKDFS-SPART,
            IT_VKDFS-VSTEL,
*           IT_VKDFS-PDSTK,
            IT_VKDFS-NETWR,
            IT_VKDFS-WAERK.
  ENDLOOP.
ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  BDC_FILL
*&---------------------------------------------------------------------*
FORM BDC_FILL USING    P1 P2 P3.
  CLEAR BDC_TAB.
  IF P1 = 'X'.
     BDC_TAB-DYNBEGIN = P1.
     BDC_TAB-PROGRAM  = P2.
     BDC_TAB-DYNPRO   = P3.
  ELSE.
     BDC_TAB-DYNBEGIN = P1.
     BDC_TAB-FNAM     = P2.
     BDC_TAB-FVAL     = P3.
  ENDIF.
  APPEND BDC_TAB.
ENDFORM.                    " BDC_FILL
