***INCLUDE RVADOPFO .

DATA: LVS_ITCPO         TYPE   ITCPO,
      LVF_DEVICE(30)    TYPE   C,
      LVF_DIALOG(1)     TYPE   C   VALUE ' ',
      LVS_RECIPIENT     LIKE   SWOTOBJID,
      LVS_SENDER        LIKE   SWOTOBJID,
      LVS_SNAST         TYPE   SNAST,
      LVF_PROGRAM       LIKE   SY-REPID,
      LVS_COMM_TYPE     TYPE   AD_COMM,
      LVS_COMM_VALUES   TYPE   SZADR_COMM_VALUES.


* reset return code
  RETCODE = 0.

* if there is a communication strategy used ...
  IF NOT NAST-TCODE IS INITIAL AND NAST-NACHA EQ '5'.

*   ... use stratagy to get communication type
    CALL FUNCTION 'ADDR_GET_NEXT_COMM_TYPE'
         EXPORTING
              STRATEGY           = NAST-TCODE
*             ADDRESS_TYPE       =
*             ADDRESS_NUMBER     = VBDKA-ADRNR
*             PERSON_NUMBER      = VBDKA-ADRNP
              ADDRESS_NUMBER     = ADDR_KEY-ADDRNUMBER
              PERSON_NUMBER      = ADDR_KEY-PERSNUMBER
         IMPORTING
              COMM_TYPE          = LVS_COMM_TYPE
              COMM_VALUES        = LVS_COMM_VALUES
*        TABLES
*             STRATEGY_TABLE     =
         EXCEPTIONS
              ADDRESS_NOT_EXIST  = 1
              PERSON_NOT_EXIST   = 2
              NO_COMM_TYPE_FOUND = 3
              INTERNAL_ERROR     = 4
              PARAMETER_ERROR    = 5
              OTHERS             = 6.
    IF SY-SUBRC <> 0.
      retcode = sy-subrc.
      SYST-MSGTY = 'E'.
      perform protocol_update.
    ENDIF.

  ENDIF.


* convert communication data
  MOVE-CORRESPONDING NAST TO LVS_SNAST.
  MOVE SY-REPID           TO LVF_PROGRAM.
  CALL FUNCTION 'CONVERT_COMM_TYPE_DATA'
       EXPORTING
            PI_COMM_TYPE              = LVS_COMM_TYPE
            PI_COMM_VALUES            = LVS_COMM_VALUES
            PI_SCREEN                 = US_SCREEN
*           PI_NEWID                  =
            PI_COUNTRY                = US_COUNTRY
            PI_REPID                  = LVF_PROGRAM
            PI_SNAST                  = LVS_SNAST
       IMPORTING
            PE_ITCPO                  = LVS_ITCPO
            PE_DEVICE                 = LVF_DEVICE
            PE_MAIL_RECIPIENT         = LVS_RECIPIENT
            PE_MAIL_SENDER            = LVS_SENDER
       EXCEPTIONS
            COMM_TYPE_NOT_SUPPORTED   = 1
            RECIPIENT_CREATION_FAILED = 2
            SENDER_CREATION_FAILED    = 3
            OTHERS                    = 4.
  IF SY-SUBRC <> 0.
    RETCODE = SY-SUBRC.
    SYST-MSGTY = 'E'.
    PERFORM PROTOCOL_UPDATE.
  ENDIF.

  check retcode eq 0.

* if there is no communication type
  IF  LVS_COMM_TYPE IS INITIAL.
*   set device
    CASE NAST-NACHA.
      WHEN '1'.
        LVF_DEVICE = 'PRINTER'.
      WHEN '2'.
        LVF_DEVICE = 'TELEFAX'.
        LVS_ITCPO-TDTELENUM = NAST-TELFX.
        IF NAST-TLAND IS INITIAL.
          LVS_ITCPO-TDTELELAND = US_COUNTRY.
        ELSE.
          LVS_ITCPO-TDTELELAND = NAST-TLAND.
        ENDIF.
        LVS_ITCPO-TDSENDDATE = NAST-VSDAT.
        LVS_ITCPO-TDSENDTIME = NAST-VSURA.
        LVS_ITCPO-TDFAXUSER  = NAST-USNAM.
      WHEN '3'.
        LVF_DEVICE = 'TELETEX'.
        LVS_ITCPO-TDTELENUM = NAST-TELTX.
        IF NAST-TLAND IS INITIAL.
          LVS_ITCPO-TDTELELAND = US_COUNTRY.
        ELSE.
          LVS_ITCPO-TDTELELAND = NAST-TLAND.
        ENDIF.
        LVS_ITCPO-TDSENDDATE = NAST-VSDAT.
        LVS_ITCPO-TDSENDTIME = NAST-VSURA.
     WHEN '4'.
        LVF_DEVICE = 'TELEX'.
        LVS_ITCPO-TDTELENUM = NAST-TELX1.
        IF NAST-TLAND IS INITIAL.
          LVS_ITCPO-TDTELELAND = US_COUNTRY.
        ELSE.
          LVS_ITCPO-TDTELELAND = NAST-TLAND.
        ENDIF.
        LVS_ITCPO-TDSENDDATE = NAST-VSDAT.
        LVS_ITCPO-TDSENDTIME = NAST-VSURA.
      WHEN OTHERS.
        LVF_DEVICE = 'PRINTER'.
    ENDCASE.
  ENDIF.

* fill structure itcpo
  ITCPO = LVS_ITCPO.

* insert note 508569 {
* OTF-Output, wenn Browser-Druck
  if nast-sort1 = 'EBPP'.
    lvs_itcpo-tdgetotf = 'X'.
  endif.
* } end note 508569

* open form
  CALL FUNCTION 'OPEN_FORM'
       EXPORTING
*           APPLICATION        = 'TX'
            ARCHIVE_INDEX      = TOA_DARA
            ARCHIVE_PARAMS     = ARC_PARAMS
            DEVICE             = LVF_DEVICE
            DIALOG             = ' '
            FORM               = TNAPR-FONAM
            LANGUAGE           = NAST-SPRAS
            OPTIONS            = LVS_ITCPO
            MAIL_SENDER        = LVS_SENDER
            MAIL_RECIPIENT     = LVS_RECIPIENT
*           MAIL_APPL_OBJECT   = ' '
*           RAW_DATA_INTERFACE = '*'
*      IMPORTING
*           LANGUAGE           =
*           NEW_ARCHIVE_PARAMS =
*           RESULT             =
       EXCEPTIONS
            CANCELED           = 1
            DEVICE             = 2
            FORM               = 3
            OPTIONS            = 4
            UNCLOSED           = 5
            MAIL_OPTIONS       = 6
            ARCHIVE_ERROR      = 7
            OTHERS             = 8.
  IF SY-SUBRC NE 0.
    CASE SY-SUBRC.
      WHEN 7.
        RETCODE = SY-SUBRC.
        SYST-MSGID = 'VN'.
        SYST-MSGNO = '096'.
        SYST-MSGTY = 'E'.
        SYST-MSGV1 = NAST-KSCHL.
        SYST-MSGV2 = NAST-KAPPL.
        PERFORM PROTOCOL_UPDATE.
      WHEN OTHERS.
        RETCODE = SY-SUBRC.
        PERFORM PROTOCOL_UPDATE.
    ENDCASE.
  ENDIF.
  SET COUNTRY US_COUNTRY.

* reset data for CTCV
  CALL FUNCTION 'CTCV_INIT_USER_DATA'.
