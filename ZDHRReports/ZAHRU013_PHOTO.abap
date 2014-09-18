REPORT ZAHRU013_PHOTO.

PARAMETERS:
  p_pernr like pa0001-pernr,
  URL LIKE SAPB-URI no-display,
  CREP_ID LIKE CREP-CREP_ID no-display,
  DOC_ID(32) no-display,
  COMP_ID LIKE SCMS_ACINF-COMP_ID no-display,
  DP_ONLY(1) no-display,
  HT_ONLY(1) no-display,
  IMFLG(1) no-display.

DATA:
  IMAGE_CONTROL TYPE REF TO CL_GUI_PICTURE,
  CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  HTML_VIEWER TYPE REF TO CL_GUI_HTML_VIEWER,
  OK_CODE LIKE SY-UCOMM.

    IF sy-sysid NE 'UP2'.
      CALL FUNCTION 'HRWPC_RFC_EP_READ_PHOTO_URI'
        DESTINATION 'UP2'
        EXPORTING
          pernr                  = p_pernr
          datum                  = sy-datum
          tclas                  = 'A'
        IMPORTING
          uri                    = url
       EXCEPTIONS
         not_supported          = 1
         nothing_found          = 2
         no_authorization       = 3
         internal_error         = 4
         OTHERS                 = 5        .
    ELSE.
      CALL FUNCTION 'HRWPC_RFC_EP_READ_PHOTO_URI'
           EXPORTING
                pernr            = p_pernr
                datum            = sy-datum
                tclas            = 'A'
           IMPORTING
                uri              = URL
           EXCEPTIONS
                not_supported    = 1
                nothing_found    = 2
                no_authorization = 3
                internal_error   = 4
                OTHERS           = 5.
    ENDIF.

IF URL = SPACE.
  CALL FUNCTION 'SCMS_DOC_URL_READ'
       EXPORTING
*           MANDT            = SY-MANDT
            STOR_CAT         = SPACE
            CREP_ID          = CREP_ID
            DOC_ID           = DOC_ID
            COMP_ID          = COMP_ID
            SIGNATURE        = 'X'
            DP_URL_ONLY    = DP_ONLY
            HTTP_URL_ONLY  = HT_ONLY
            SECURITY         = 'F'
            LIFETIME         = CNDP_LIFETIME_ALL
       IMPORTING
            URL              = URL
       EXCEPTIONS
            OTHERS           = 1
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'DP_SYNC_URLS'
    EXCEPTIONS
      CNTL_ERROR               = 1
      CNTL_SYSTEM_ERROR        = 2
      DP_CREATE_ERROR          = 3
      DATA_SOURCE_ERROR        = 4
      DP_SEND_DATA_ERROR       = 5
      GENERAL_ERROR            = 6
      OTHERS                   = 7
            .
ENDIF.

CALL SCREEN 100  STARTING AT 10 1
                      ENDING AT 45  14.
IF NOT IMAGE_CONTROL IS INITIAL.
  CLEAR IMAGE_CONTROL.
ENDIF.
IF NOT HTML_VIEWER IS INITIAL.
  CLEAR HTML_VIEWER.
ENDIF.
IF NOT CONTAINER IS INITIAL.
  CLEAR CONTAINER.
ENDIF.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

  IF CONTAINER IS INITIAL.

    CREATE OBJECT CONTAINER
      EXPORTING
        CONTAINER_NAME                = 'CUSTOM'
      EXCEPTIONS
        CNTL_ERROR                    = 1
        CNTL_SYSTEM_ERROR             = 2
        CREATE_ERROR                  = 3
        LIFETIME_ERROR                = 4
        LIFETIME_DYNPRO_DYNPRO_LINK   = 5.
  ENDIF.

  IF IMFLG = 'X'.
    IF IMAGE_CONTROL IS INITIAL.
      CREATE OBJECT IMAGE_CONTROL
         EXPORTING PARENT = CONTAINER.
    ENDIF.

    CALL METHOD IMAGE_CONTROL->LOAD_PICTURE_FROM_URL
         EXPORTING URL = URL.
  ELSE.
    IF HTML_VIEWER IS INITIAL.
      CREATE OBJECT HTML_VIEWER
         EXPORTING PARENT = CONTAINER
                   SAPHTMLP = 'X'.
    ENDIF.

    CALL METHOD HTML_VIEWER->SHOW_URL
       EXPORTING URL = URL.
  ENDIF.

ENDMODULE.                             " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE OK_CODE.
    WHEN 'BACK'
      OR 'EXIT'
      OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                             " USER_COMMAND_0100  INPUT
