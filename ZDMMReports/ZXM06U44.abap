*----------------------------------------------------------------------*
*   INCLUDE ZXM06U44                                                   *
*----------------------------------------------------------------------*

 LOOP AT YEKPO.  "old value

   IF YEKPO-ELIKZ = 'X' AND YEKPO-ZZTYPE = 'F'.
     READ TABLE XEKPO WITH KEY EBELN = YEKPO-EBELN
                               EBELP = YEKPO-EBELP.

     IF SY-SUBRC = 0 AND XEKPO-ELIKZ = ''.
       CALL FUNCTION 'ZMMF_IF_PO_FLAG_UPDATE' IN UPDATE TASK
            EXPORTING
                 I_EBELN = XEKPO-EBELN
                 I_EBELP = XEKPO-EBELP.
     ENDIF.

   ENDIF.

 ENDLOOP.
