*&---------------------------------------------------------------------*
*& Report  ZMMR90500T_STOCK_TO_GLOVIS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zsdc04300t NO STANDARD PAGE HEADING
                          MESSAGE-ID zmmM.

data: p_date like sy-datum,
      E_RETURN like table of ZMMS0053 with header line.

p_date = sy-datum.

CALL FUNCTION 'Z_MM_IF_OB_02_004_DB'
       EXPORTING
            I_DATE   = P_DATE
       IMPORTING
            E_RETURN = E_RETURN.

read table E_RETURN with key = 'E'.
if sy-subrc = 0.
   message s999 with E_RETURN-message.
else.
read table E_RETURN with key = 'S'.
   message s999 with E_RETURN-message.
endif.
*SUBMIT  ZMMR90500T  " VIA SELECTION-SCREEN
*            USING SELECTION-SET 'STOCK-GLOVIS'.
