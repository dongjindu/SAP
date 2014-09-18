FORM CD_CALL_ZIPMTIV                       .
   IF   ( UPD_ZTPMTIV                        NE SPACE )
     OR ( UPD_ICDTXT_ZIPMTIV         NE SPACE )
   .
     CALL FUNCTION 'ZIPMTIV_WRITE_DOCUMENT        ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTPMTIV
                      = *ZTPMTIV
                  N_ZTPMTIV
                      = ZTPMTIV
                  UPD_ZTPMTIV
                      = UPD_ZTPMTIV
                  UPD_ICDTXT_ZIPMTIV
                      = UPD_ICDTXT_ZIPMTIV
          TABLES  ICDTXT_ZIPMTIV
                      = ICDTXT_ZIPMTIV
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
