FORM CD_CALL_ZIBLIT                        .
   IF   ( UPD_ZTBLIT                         NE SPACE )
     OR ( UPD_ICDTXT_ZIBLIT          NE SPACE )
   .
     CALL FUNCTION 'ZIBLIT_WRITE_DOCUMENT         ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTBLIT
                      = *ZTBLIT
                  N_ZTBLIT
                      = ZTBLIT
                  UPD_ZTBLIT
                      = UPD_ZTBLIT
                  UPD_ICDTXT_ZIBLIT
                      = UPD_ICDTXT_ZIBLIT
          TABLES  ICDTXT_ZIBLIT
                      = ICDTXT_ZIBLIT
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
