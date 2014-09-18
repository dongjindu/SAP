FORM CD_CALL_ZTBKPF                        .
   IF   ( UPD_ZTBKPF                         NE SPACE )
     OR ( UPD_ICDTXT_ZTBKPF          NE SPACE )
   .
     CALL FUNCTION 'ZTBKPF_WRITE_DOCUMENT         ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTBKPF
                      = *ZTBKPF
                  N_ZTBKPF
                      = ZTBKPF
                  UPD_ZTBKPF
                      = UPD_ZTBKPF
                  UPD_ICDTXT_ZTBKPF
                      = UPD_ICDTXT_ZTBKPF
          TABLES  ICDTXT_ZTBKPF
                      = ICDTXT_ZTBKPF
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
