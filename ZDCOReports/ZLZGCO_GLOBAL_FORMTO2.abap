*----------------------------------------------------------------------*
*   INCLUDE ZLZGCO_GLOBAL_FORMTO2                                      *
*----------------------------------------------------------------------*

** Costants
CONSTANTS : C_BPL(4) VALUE 'ZPCP',
            C_STD(4) VALUE 'PPC1',
            C_ACT(4) VALUE 'PPC1'.
CONSTANTS : C_FSC(4)       VALUE 'FERT',
            C_HALB(4)      VALUE 'HALB',
            C_FSC_PLANT(4) VALUE 'P001',
            C_RP_PLNNR(8)  VALUE 'RP'.
CONSTANTS : C_KSTAR_M(10)  VALUE '0000540300'.
CONSTANTS : C_CAT_M(1)     VALUE 'M',
            C_CAT_E(1)     VALUE 'E',
            C_CAT_V(1)     VALUE 'V'.
*CONSTANTS : C_SHP_M_E001(10)     VALUE 'MXEX',
*            C_SHP_M_P001(10)     VALUE 'MXSX'.
CONSTANTS : C_GP_KOSTL_E(15)    VALUE 'HMMA-SHOP',
            C_GP_DIRECT(15)     VALUE 'DIRECT'.


* For Cost Components
DATA : BEGIN OF IT_CSKB  OCCURS 0.
DATA :  ELEMT LIKE TCKH2-ELEMT,
        KSTAR LIKE CSKB-KSTAR.
DATA : END OF IT_CSKB .


*----------------------------------------------------------------------*
*   Macro                                                              *
*----------------------------------------------------------------------*
DEFINE DEF_CO_SUM_TAB.
  DATA : BEGIN OF IT_COS&1A OCCURS 0.
  DATA :  BEWEG LIKE KKBCS_OUT-BEWEG.
          INCLUDE STRUCTURE COS&1A.
  DATA : END OF IT_COS&1A.
END-OF-DEFINITION.
