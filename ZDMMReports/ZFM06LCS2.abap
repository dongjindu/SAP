*eject
*--------------------------------------------------------------------*
*        COMMON DATA                                                 *
*--------------------------------------------------------------------*
*        Selektionsbedingungen 1                                     *
*--------------------------------------------------------------------*

DATA:    BEGIN OF COMMON PART FM06LCS2.

SELECT-OPTIONS: S_EBELN FOR EKKO-EBELN MATCHCODE OBJECT MEKK,
                S_LIFNR FOR EKKO-LIFNR MATCHCODE OBJECT KRED,
                S_RESWK FOR EKKO-RESWK,
                S_MATNR FOR EKPO-MATNR MATCHCODE OBJECT MAT1,
                S_MATKL FOR EKPO-MATKL,
                S_BEDAT FOR EKKO-BEDAT,
                S_EAN11 FOR EKPO-EAN11,
                S_IDNLF FOR EKPO-IDNLF,
                S_LTSNR FOR EKPO-LTSNR,
                S_AKTNR FOR EKPO-AKTNR,
                S_SAISO FOR EKPO-SAISO,
                S_SAISJ FOR EKPO-SAISJ.
PARAMETERS:     P_TXZ01 LIKE EKPO-TXZ01,
                P_NAME1 LIKE ADDR1_DATA-NAME1.

DATA:    END OF COMMON PART.
