FORM CD_CALL_ZICGHD                        .
   IF   ( UPD_ZTCGHD                         NE SPACE )
     OR ( UPD_ICDTXT_ZICGHD          NE SPACE )
   .
     CALL FUNCTION 'ZICGHD_WRITE_DOCUMENT         ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTCGHD
                      = *ZTCGHD
                  N_ZTCGHD
                      = ZTCGHD
                  UPD_ZTCGHD
                      = UPD_ZTCGHD
                  UPD_ICDTXT_ZICGHD
                      = UPD_ICDTXT_ZICGHD
          TABLES  ICDTXT_ZICGHD
                      = ICDTXT_ZICGHD
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
