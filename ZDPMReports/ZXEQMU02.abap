*----------------------------------------------------------------------*
*   INCLUDE ZXEQMU02                                                   *
*----------------------------------------------------------------------*
* CHECK DATA_EQUI-EQTYP EQ 'M' OR
*"//Equip type: Production Equipment
*       DATA_EQUI-EQTYP EQ 'Q' OR     "//Quality Equipment
*       DATA_EQUI-EQTYP EQ 'U'.       "//Utility Equipment
*
* CHECK DATA_EQUI-EQART EQ '10' OR   "//PM Line(Direct)
*       DATA_EQUI-EQART EQ '11' OR   "//PM Line(Indirect)
*       DATA_EQUI-EQART EQ '20' OR   "//PM Off-Line
*       DATA_EQUI-EQART EQ '30'.     "//PM No Supervision
**       DATA_EQUI-EQART EQ '50' OR   "//Equipment + Asset
**       DATA_EQUI-EQART EQ '51'.     "//Equipment Only
*
* TABLES: CRHD.
*
* DATA: WA_ARBPL LIKE CRHD-ARBPL.
*
* SELECT SINGLE ARBPL INTO WA_ARBPL
*               FROM  CRHD
*               WHERE OBJTY = DATA_ILOA-CR_OBJTY
*               AND   OBJID = DATA_ILOA-PPSID.
* IF SY-SUBRC EQ 0.
*   MOVE-CORRESPONDING DATA_EQUI TO UPDATE_DATA_EQ.
*
*   MOVE: WA_ARBPL+2(5) TO UPDATE_DATA_EQ-LINE.
*   UPDATE_FLAGS_EQ-LINE = 'X'.
*
*   MOVE: WA_ARBPL+2(2) TO UPDATE_DATA_EQ-SHOP.
*   UPDATE_FLAGS_EQ-SHOP = 'X'.
* ENDIF.

data: zequi like equi occurs 0 with header line.

if  DATA_EQUI-EQTYP EQ 'Z'.
select single * from Equi into zequi where serge = DATA_EQUI-serge
                                      and equnr <> data_equi-equnr
                                      and serge <> ' '.
*                                      and groes <> data_equi-groes
*                                      and herst <> data_equi-herst.
if  sy-subrc = 0.
if zequi-groes = data_equi-groes and zequi-herst = data_equi-herst.

MESSAGE ID 'ZE' TYPE 'W' NUMBER '000'.
endif.
*Message E100.
endif. endif.
