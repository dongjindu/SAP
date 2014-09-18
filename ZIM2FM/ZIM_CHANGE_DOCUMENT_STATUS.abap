FUNCTION ZIM_CHANGE_DOCUMENT_STATUS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(W_ZFREQNO) LIKE  ZTREQST-ZFREQNO
*"     REFERENCE(W_ZFAMDNO) LIKE  ZTREQST-ZFAMDNO
*"     REFERENCE(N_ZTREQST) LIKE  ZTREQST STRUCTURE  ZTREQST
*"     REFERENCE(O_ZTREQST) LIKE  ZTREQST STRUCTURE  ZTREQST
*"----------------------------------------------------------------------
DATA : W_AMEND     LIKE ZTREQST-ZFAMDNO.

  CLEAR : *ZTMLCHD, *ZTMLCSG2, *ZTMLCSG910, *ZTREQHD,
          *ZTLLCHD, *ZTLLCSG23, *ZTMLCAMHD, *ZTLLCAMHD,
          *ZTPUR,
          *ZTTTHD.

  W_AMEND = N_ZTREQST-ZFAMDNO.

 SELECT SINGLE * FROM ZTREQHD
        WHERE    ZFREQNO EQ  W_ZFREQNO.

 CALL FUNCTION 'ZIM_REQUEST_DOC_MODIFY'
       EXPORTING
             W_OK_CODE           =   'SAVE'
             ZFREQNO             =   W_ZFREQNO
             ZFAMDNO             =   W_ZFAMDNO
             ZFSTATUS            =   'U'
             W_ZTREQHD           =   ZTREQHD
*             W_ZTREQHD_OLD       =   W_ZTREQHD_OLD
             W_ZTREQST           =   N_ZTREQST
             W_MODIFY            =   SPACE
*       TABLES
*             IT_ZSREQIT          =   IT_ZSREQIT
*             IT_ZSREQIT_OLD      =   IT_ZSREQIT_OLD
*             IT_ZSREQIL          =   IT_ZSREQIL
*             IT_ZSREQIL_OLD      =   IT_ZSREQIL_OLD
*             IT_ZTREQORJ         =   IT_ZTREQORJ
*             IT_ZTREQORJ_OLD     =   IT_ZTREQORJ_OLD
       EXCEPTIONS
              ERROR_UPDATE.


  CASE O_ZTREQST-ZFREQTY.
     WHEN 'LC'.
        IF W_AMEND IS INITIAL.
           CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_LC'
              EXPORTING
                  UPD_CHNGIND    =    'U'
                  N_ZTMLCHD      =    *ZTMLCHD
                  O_ZTMLCHD      =    *ZTMLCHD
                  N_ZTMLCSG2     =    *ZTMLCSG2
                  O_ZTMLCSG2     =    *ZTMLCSG2
                  N_ZTMLCSG910   =    *ZTMLCSG910
                  O_ZTMLCSG910   =    *ZTMLCSG910
                  N_ZTREQHD      =    *ZTREQHD
                  O_ZTREQHD      =    *ZTREQHD
                  N_ZTREQST      =    N_ZTREQST
                  O_ZTREQST      =    O_ZTREQST.
        ELSE.   " AMEND ½Ã
           CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_LC_AMEND'
              EXPORTING
                  UPD_CHNGIND    =    'U'
                  N_ZTMLCAMHD    =    *ZTMLCAMHD
                  O_ZTMLCAMHD    =    *ZTMLCAMHD
                  N_ZTREQHD      =    *ZTREQHD
                  O_ZTREQHD      =    *ZTREQHD
                  N_ZTREQST      =    N_ZTREQST
                  O_ZTREQST      =    O_ZTREQST.

        ENDIF.
     WHEN 'LO'.
        IF W_AMEND IS INITIAL.
           CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_LO'
              EXPORTING
                  UPD_CHNGIND    =    'U'
                  N_ZTLLCHD      =    *ZTLLCHD
                  O_ZTLLCHD      =    *ZTLLCHD
                  N_ZTLLCSG23    =    *ZTLLCSG23
                  O_ZTLLCSG23    =    *ZTLLCSG23
                  N_ZTREQHD      =    *ZTREQHD
                  O_ZTREQHD      =    *ZTREQHD
                  N_ZTREQST      =    N_ZTREQST
                  O_ZTREQST      =    O_ZTREQST.
        ELSE.    " AMEND
           CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_LO_AMEND'
              EXPORTING
                  UPD_CHNGIND    =    'U'
                  N_ZTLLCAMHD    =    *ZTLLCAMHD
                  O_ZTLLCAMHD    =    *ZTLLCAMHD
                  N_ZTREQHD      =    *ZTREQHD
                  O_ZTREQHD      =    *ZTREQHD
                  N_ZTREQST      =    N_ZTREQST
                  O_ZTREQST      =    O_ZTREQST.
        ENDIF.
     WHEN 'PU'.
        CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_PU'
           EXPORTING
               UPD_CHNGIND    =    'U'
               N_ZTPUR        =    *ZTPUR
               O_ZTPUR        =    *ZTPUR
               N_ZTREQHD      =    *ZTREQHD
               O_ZTREQHD      =    *ZTREQHD
               N_ZTREQST      =    N_ZTREQST
               O_ZTREQST      =    O_ZTREQST.
     WHEN 'TT'.
        CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_TT'
           EXPORTING
               UPD_CHNGIND    =    'U'
               N_ZTTTHD       =    *ZTTTHD
               O_ZTTTHD       =    *ZTTTHD
               N_ZTREQHD      =    *ZTREQHD
               O_ZTREQHD      =    *ZTREQHD
               N_ZTREQST      =    N_ZTREQST
               O_ZTREQST      =    O_ZTREQST.

     WHEN 'DA' OR 'DP'.
        CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_OT'
           EXPORTING
               UPD_CHNGIND    =    'U'
               N_ZTREQHD      =    *ZTREQHD
               O_ZTREQHD      =    *ZTREQHD
               N_ZTREQST      =    N_ZTREQST
               O_ZTREQST      =    O_ZTREQST.

  ENDCASE.

ENDFUNCTION.
