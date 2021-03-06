FUNCTION ZIM_GET_USER_BUSINESS_AREA.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(UNAME) LIKE  USR02-BNAME DEFAULT SY-UNAME
*"     VALUE(WERKS) LIKE  T001W-WERKS OPTIONAL
*"  EXPORTING
*"     VALUE(BUPLA) LIKE  ZTBKPF-BUPLA
*"     VALUE(GSBER) LIKE  ZTBKPF-GSBER
*"  EXCEPTIONS
*"      AREA_ERROR
*"----------------------------------------------------------------------
   CLEAR : GSBER, bupla.

   IF UNAME IS INITIAL.
      MESSAGE E104(ZIM1).
   ENDIF.

   SELECT * INTO TABLE IT_ZTIMIMG19
            FROM ZTIMIMG19
            WHERE USNAM EQ UNAME.

   IF SY-SUBRC NE 0.
      MESSAGE E105(ZIM1) WITH UNAME RAISING AREA_ERROR.
   ENDIF.

   SORT IT_ZTIMIMG19 BY ZFFIX DESCENDING.

   READ TABLE IT_ZTIMIMG19 WITH KEY ZFFIX = 'X'.
   IF SY-SUBRC EQ 0.
      GSBER = IT_ZTIMIMG19-GSBER.
   ELSE.
      IF WERKS IS INITIAL.
         EXIT.
      ENDIF.
      READ TABLE IT_ZTIMIMG19 WITH KEY ZFFIX = SPACE
                                       WERKS = WERKS.
      IF SY-SUBRC EQ 0.
         GSBER = IT_ZTIMIMG19-GSBER.
      ELSE.
         MESSAGE E106(ZIM1) WITH UNAME WERKS RAISING AREA_ERROR.
      ENDIF.
   ENDIF.

*> �������.
   SELECT SINGLE  * FROM T001W
          WHERE WERKS EQ WERKS.
   IF SY-SUBRC EQ 0.
      BUPLA = T001W-J_1BBRANCH.
   ENDIF.

ENDFUNCTION.
