FORM CD_CALL_ZICGIT                        .
   IF   ( UPD_ZTCGIT                         NE SPACE )
     OR ( UPD_ICDTXT_ZICGIT          NE SPACE )
   .
     CALL FUNCTION 'ZICGIT_WRITE_DOCUMENT         ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTCGIT
                      = *ZTCGIT
                  N_ZTCGIT
                      = ZTCGIT
                  UPD_ZTCGIT
                      = UPD_ZTCGIT
                  UPD_ICDTXT_ZICGIT
                      = UPD_ICDTXT_ZICGIT
          TABLES  ICDTXT_ZICGIT
                      = ICDTXT_ZICGIT
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
