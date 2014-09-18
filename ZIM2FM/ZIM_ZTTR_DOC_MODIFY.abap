FUNCTION ZIM_ZTTR_DOC_MODIFY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(ZFTRNO) LIKE  ZTTRHD-ZFTRNO OPTIONAL
*"     VALUE(ZFSTATUS)
*"     VALUE(W_ZTTRHD_OLD) LIKE  ZTTRHD STRUCTURE  ZTTRHD OPTIONAL
*"     VALUE(W_ZTTRHD) LIKE  ZTTRHD STRUCTURE  ZTTRHD
*"     VALUE(W_OK_CODE)
*"  TABLES
*"      IT_DOHD STRUCTURE  ZSDOHD
*"      IT_DOIT STRUCTURE  ZSDOIT
*"  EXCEPTIONS
*"      ERROR_UPDATE
*"----------------------------------------------------------------------
  DATA : W_ZFTRIT        LIKE   ZTTRIT-ZFTRIT,
         W_TABIX_DETAIL  LIKE   SY-TABIX,
         WL_ZFBLNO       LIKE   ZTTRIT-ZFBLNO,
         WL_VGBEL        LIKE   LIPS-VGBEL,
         WL_VGPOS        LIKE   LIPS-VGPOS,
         WL_MENGE        LIKE   ZTTRIT-GIMENGE.

*> Delivery Order Header
  MOVE-CORRESPONDING : W_ZTTRHD      TO   ZTTRHD.

  MOVE : ZFTRNO       TO     ZTTRHD-ZFTRNO,
         SY-MANDT     TO     ZTTRHD-MANDT,
         SY-UNAME     TO     ZTTRHD-UNAM,
         SY-DATUM     TO     ZTTRHD-UDAT.

  DELETE  FROM  ZTTRIT  WHERE ZFTRNO EQ ZFTRNO.
  DELETE  FROM  ZTTRITD WHERE ZFTRNO EQ ZFTRNO.

  IF W_OK_CODE EQ 'DELE'.
    ZFSTATUS = 'X'.
  ENDIF.

  SORT  IT_DOIT  BY  ZFBLNO  VGBEL  VGPOS.

  LOOP AT IT_DOIT.
     IF SY-TABIX EQ 1.
        MOVE : IT_DOIT-ZFBLNO   TO   WL_ZFBLNO,
               IT_DOIT-VGBEL    TO   WL_VGBEL,
               IT_DOIT-VGPOS    TO   WL_VGPOS.
     ENDIF.
     IF WL_ZFBLNO  NE  IT_DOIT-ZFBLNO   OR
        WL_VGBEL   NE  IT_DOIT-VGBEL    OR
        WL_VGPOS   NE  IT_DOIT-VGPOS    .

        MOVE  :  WL_ZFBLNO   TO  IT_ZSTRIT-ZFBLNO,
                 WL_VGBEL    TO  IT_ZSTRIT-EBELN,
                 WL_VGPOS    TO  IT_ZSTRIT-EBELP.

        SELECT SINGLE * FROM ZTBLIT
        WHERE  ZFBLNO   EQ   IT_ZSTRIT-ZFBLNO
        AND    EBELN    EQ   IT_ZSTRIT-EBELN
        AND    EBELP    EQ   IT_ZSTRIT-EBELP.

        IF SY-SUBRC EQ 0.
           MOVE-CORRESPONDING  ZTBLIT  TO  IT_ZSTRIT.
        ENDIF.

        " Customs Clearance Quantity Get.
        SELECT SINGLE * FROM ZTIVIT
        WHERE  ZFBLNO   EQ   IT_ZSTRIT-ZFBLNO
        AND    ZFBLIT   EQ   IT_ZSTRIT-ZFBLIT
        AND    EBELN    EQ   IT_ZSTRIT-EBELN
        AND    EBELP    EQ   IT_ZSTRIT-EBELP.

        IF SY-SUBRC EQ 0.
           MOVE  ZTIVIT-CCMENGE  TO  IT_ZSTRIT-CCMENGE.
        ENDIF.

        " Good Issue Quantity Set.
        MOVE  WL_MENGE   TO  IT_ZSTRIT-GIMENGE.
        W_TABIX          =   W_TABIX  +  1.
        IT_ZSTRIT-ZFTRIT =   W_TABIX  *  10.

        APPEND  IT_ZSTRIT.

        MOVE  :  IT_DOIT-ZFBLNO   TO  WL_ZFBLNO,
                 IT_DOIT-VGBEL    TO  WL_VGBEL,
                 IT_DOIT-VGPOS    TO  WL_VGPOS.
        CLEAR : WL_MENGE.
     ENDIF.
     WL_MENGE  =  WL_MENGE  +  IT_DOIT-LFIMG.

     AT LAST.
        MOVE  :  WL_ZFBLNO   TO  IT_ZSTRIT-ZFBLNO,
                 WL_VGBEL    TO  IT_ZSTRIT-EBELN,
                 WL_VGPOS    TO  IT_ZSTRIT-EBELP.

        SELECT SINGLE * FROM ZTBLIT
        WHERE  ZFBLNO   EQ   IT_ZSTRIT-ZFBLNO
        AND    EBELN    EQ   IT_ZSTRIT-EBELN
        AND    EBELP    EQ   IT_ZSTRIT-EBELP.

        IF SY-SUBRC EQ 0.
           MOVE-CORRESPONDING  ZTBLIT  TO  IT_ZSTRIT.
        ENDIF.

        " Customs Clearance Quantity Get.
        SELECT SINGLE * FROM ZTIVIT
        WHERE  ZFBLNO   EQ   IT_ZSTRIT-ZFBLNO
        AND    ZFBLIT   EQ   IT_ZSTRIT-ZFBLIT
        AND    EBELN    EQ   IT_ZSTRIT-EBELN
        AND    EBELP    EQ   IT_ZSTRIT-EBELP.

        IF SY-SUBRC EQ 0.
           MOVE  ZTIVIT-CCMENGE  TO  IT_ZSTRIT-CCMENGE.
        ENDIF.

        " Good Issue Quantity Set.
        MOVE  WL_MENGE   TO  IT_ZSTRIT-GIMENGE.
        W_TABIX          =   W_TABIX  +  1.
        IT_ZSTRIT-ZFTRIT =   W_TABIX  *  10.
        APPEND  IT_ZSTRIT.

     ENDAT.
  ENDLOOP.

  CASE ZFSTATUS.
    "------------------------------------------------
    " Create
    "------------------------------------------------
    WHEN 'C'.
      MOVE : SY-UNAME      TO    ZTTRHD-ERNAM,
             SY-DATUM      TO    ZTTRHD-CDAT.

      INSERT   ZTTRHD.
      IF SY-SUBRC NE 0.   RAISE ERROR_UPDATE.    ENDIF.
      LOOP AT IT_ZSTRIT.
        CLEAR : ZTTRIT.
        MOVE-CORRESPONDING IT_ZSTRIT  TO ZTTRIT.
        MOVE : ZFTRNO                 TO ZTTRIT-ZFTRNO,
               SY-MANDT               TO ZTTRIT-MANDT,
               SY-UNAME               TO ZTTRIT-ERNAM,
               SY-DATUM               TO ZTTRIT-CDAT,
               SY-UNAME               TO ZTTRIT-UNAM,
               SY-DATUM               TO ZTTRIT-UDAT.

        INSERT   ZTTRIT.
        IF SY-SUBRC NE 0.
          DELETE  FROM ZTTRHD   WHERE ZFTRNO  EQ ZFTRNO.
          DELETE  FROM ZTTRIT   WHERE ZFTRNO  EQ ZFTRNO.
          RAISE ERROR_UPDATE.
        ENDIF.
        CLEAR : W_TABIX_DETAIL.
        LOOP  AT  IT_DOIT WHERE  ZFBLNO  EQ  ZTTRIT-ZFBLNO
                          AND    VGBEL   EQ  ZTTRIT-EBELN
                          AND    VGPOS   EQ  ZTTRIT-EBELP.
           MOVE  :  SY-MANDT       TO   ZTTRITD-MANDT,
                    ZFTRNO         TO   ZTTRITD-ZFTRNO,
                    ZTTRIT-ZFTRIT  TO   ZTTRITD-ZFTRIT,
                    IT_DOIT-VBELN  TO   ZTTRITD-VBELN.
           W_TABIX_DETAIL  =  W_TABIX_DETAIL +  1.
           ZTTRITD-ZFTRSEQ =  W_TABIX_DETAIL * 10.
           INSERT  ZTTRITD.
        ENDLOOP.
      ENDLOOP.
    "-------------------------------------------------
    " DELETE
    "-------------------------------------------------
    WHEN 'X'.

      DELETE  FROM ZTTRHD     WHERE ZFTRNO  EQ ZFTRNO.
      IF SY-SUBRC NE 0.     RAISE ERROR_UPDATE.   ENDIF.
      DELETE  FROM ZTTRIT  WHERE ZFTRNO EQ ZFTRNO.
      DELETE  FROM ZTTRITD WHERE ZFTRNO EQ ZFTRNO.

    "-------------------------------------------------
    " UPDATE
    "-------------------------------------------------
    WHEN OTHERS.

      UPDATE   ZTTRHD.
      IF SY-SUBRC NE 0.    RAISE ERROR_UPDATE.   ENDIF.

      LOOP AT IT_ZSTRIT.
        CLEAR : ZTTRIT.
        MOVE-CORRESPONDING IT_ZSTRIT  TO ZTTRIT.
        MOVE : ZFTRNO                 TO ZTTRIT-ZFTRNO,
               SY-MANDT               TO ZTTRIT-MANDT,
               SY-UNAME               TO ZTTRIT-ERNAM,
               SY-DATUM               TO ZTTRIT-CDAT,
               SY-UNAME               TO ZTTRIT-UNAM,
               SY-DATUM               TO ZTTRIT-UDAT.

        INSERT   ZTTRIT.
        IF SY-SUBRC NE 0.
          DELETE  FROM ZTTRHD   WHERE ZFTRNO  EQ ZFTRNO.
          DELETE  FROM ZTTRIT   WHERE ZFTRNO  EQ ZFTRNO.
          RAISE ERROR_UPDATE.
        ENDIF.
        CLEAR : W_TABIX_DETAIL.
        LOOP  AT  IT_DOIT WHERE  ZFBLNO  EQ  ZTTRIT-ZFBLNO
                          AND    VGBEL   EQ  ZTTRIT-EBELN
                          AND    VGPOS   EQ  ZTTRIT-EBELP.
           MOVE  :  SY-MANDT       TO   ZTTRITD-MANDT,
                    ZFTRNO         TO   ZTTRITD-ZFTRNO,
                    ZTTRIT-ZFTRIT  TO   ZTTRITD-ZFTRIT,
                    IT_DOIT-VBELN  TO   ZTTRITD-VBELN.
           W_TABIX_DETAIL  =  W_TABIX_DETAIL +  1.
           ZTTRITD-ZFTRSEQ =  W_TABIX_DETAIL * 10.
           INSERT  ZTTRITD.
        ENDLOOP.
      ENDLOOP.

  ENDCASE.

ENDFUNCTION.
