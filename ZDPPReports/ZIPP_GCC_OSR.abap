*&---------------------------------------------------------------------*
*& Report  ZIPP_GCC_OSR                                                *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZIPP_GCC_OSR     NO STANDARD PAGE HEADING
                      LINE-SIZE 132
                      LINE-COUNT 64(1)
                      MESSAGE-ID ZMMM                  .

*---------------------------------------------------------------------*
*  INCLUDE
*---------------------------------------------------------------------*
INCLUDE ZIPP_GCC_OSR_TOP.
INCLUDE ZIPP_GCC_OSR_SEL.
INCLUDE ZIPP_GCC_OSR_F01.
INCLUDE ZIPP_GCC_OSR_C01.


*---------------------------------------------------------------------*
*  START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM P2000_GET_DATA.
  IF GT_ITEM[] IS INITIAL.
    MESSAGE S009 WITH 'No data'.
  ELSE.
    IF P_CHECK EQ 'X'.
      PERFORM P3000_SEND_EAI.
    ENDIF.
    PERFORM 1000_DISPLAY_DATA.
  ENDIF.

END-OF-SELECTION.
