FORM CD_CALL_ZICCHD                        .
   IF   ( UPD_ZTIV                           NE SPACE )
     OR ( UPD_ICDTXT_ZICCHD          NE SPACE )
   .
     CALL FUNCTION 'ZICCHD_WRITE_DOCUMENT         ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTIV
                      = *ZTIV
                  N_ZTIV
                      = ZTIV
                  UPD_ZTIV
                      = UPD_ZTIV
                  UPD_ICDTXT_ZICCHD
                      = UPD_ICDTXT_ZICCHD
          TABLES  ICDTXT_ZICCHD
                      = ICDTXT_ZICCHD
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
