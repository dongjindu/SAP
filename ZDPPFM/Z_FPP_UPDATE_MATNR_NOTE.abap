FUNCTION Z_FPP_UPDATE_MATNR_NOTE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(MATNR) LIKE  MARA-MATNR
*"     REFERENCE(TXLINE) LIKE  RSTXT-TXLINE
*"     REFERENCE(MODE) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     REFERENCE(STATUS) LIKE  BAPIRETURN-TYPE
*"     REFERENCE(MESSAG) LIKE  BAPIRETURN-MESSAGE
*"----------------------------------------------------------------------


  DATA : LV_MATNR LIKE MARA-MATNR,
         LV_TXLIN LIKE RSTXT-TXLINE,
         LV_MODE LIKE MODE.
  DATA : MESSAGE LIKE CFGNL-MSGLIN .

  REFRESH  BDC_TAB.

  MOVE : MATNR  TO LV_MATNR,
         TXLINE TO LV_TXLIN.

  PERFORM DYNPRO USING :
      'X' 'SAPLMGMM'	             '60'	   ,
      ' ' 'BDC_OKCODE'	      '=ENTR'    ,
      ' ' 'RMMG1-MATNR'	       LV_MATNR  ,

      'X' 'SAPLMGMM'	              '70'	    ,
      ' ' 'BDC_OKCODE'	       '=ENTR'    ,
      ' ' 'MSICHTAUSW-KZSEL(01)'	'X'        ,

      'X' 'SAPLMGMM'	            '5004'     ,
      ' ' 'BDC_OKCODE'           '=PB26'    ,

      'X' 'SAPLMGMM'	            '5300'  ,
      ' ' 'BDC_OKCODE'	     '=LTEX' ,

      'X' 'SAPLSTXX'	            '1100'	   ,
      ' ' 'BDC_OKCODE'	     '=TXDE'     ,

      'X' 'SAPLSPO1'	            '100'       ,
      ' ' 'BDC_OKCODE'	     '=YES'      ,

      'X' 'SAPLSTXX'	            '1100'      ,
      ' ' 'BDC_OKCODE'	     'TXVB'     ,

      'X' 'SAPLSTXX'	            '1100'      ,
      ' ' 'BDC_OKCODE'	     '=TXVB'     ,
      ' ' 'RSTXT-TXLINE(02)'     LV_TXLIN    ,

*      'X' 'SAPLMGMM'	            '5300',
*      ' ' 'BDC_OKCODE'	     '=TELO'     ,
*      'X' 'SAPLSPO1'	            '100'    ,
*      ' ' 'BDC_OKCODE'	     '=YES'      ,
*      'X' 'SAPLMGMM'	            '5300'		  ,
*      ' ' 'BDC_OKCODE'	     '=TEAN'     ,
*      'X' 'SAPLMG19'	            '1000'		  ,
*      ' ' 'BDC_OKCODE'	     '=ENTR'     ,
*      ' ' 'DESC_LANGU_NEW'	     'E'         ,
*      'X' 'SAPLMGMM'	            '5300'		  ,
*      ' ' 'BDC_OKCODE'	     '=LTEX'     ,
*
*

      'X' 'SAPLSTXX'	            '1100'		  ,
      ' ' 'BDC_CURSOR'	     'RSTXT-TXLINE(02)',
      ' ' 'BDC_OKCODE'	     '=TXBA'  ,

      'X' 'SAPLMGMM'	            '5300'	 ,
      ' ' 'BDC_OKCODE'	      '=BU'.


  IF MODE IS INITIAL.
    LV_MODE = 'A'.
  ELSE .
    MOVE MODE TO LV_MODE.
  ENDIF.
  CALL TRANSACTION 'MM02'  USING BDC_TAB MODE MODE
                                 UPDATE 'S'
                                 MESSAGES INTO ITAB.

  IF  SY-SUBRC <> 0.
    CALL FUNCTION 'RKC_MSG_STRING'
         EXPORTING
              ID      = SY-MSGID
              MTYPE   = 'E'
              NUMBER  = SY-MSGNO
              PAR1    = SY-MSGV1
              PAR2    = SY-MSGV2
              PAR3    = SY-MSGV3
              PAR4    = SY-MSGV4
         IMPORTING
              MSG_LIN = MESSAGE.

    MOVE : 'E' TO STATUS ,
            MESSAGE TO MESSAG.

*    MOVE:  'E'              TO  OUTBUF-ERRCODE,
*           REC-MATNR        TO  OUTBUF-MATNR,

  ELSE.
    MOVE : 'S' TO STATUS .

  ENDIF.


ENDFUNCTION.
