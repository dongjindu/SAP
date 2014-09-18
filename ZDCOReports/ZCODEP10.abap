*
* by Andy Choi
*
* V_T093_00  : dep.area     (update)
* V_T093D_00 : posting rule (add/deletion)
* IF T093D-AFBRHY IS INITIAL. -> error (RABUCHF1 104 line)
REPORT zcodep10 .

TABLES: t093, t093c, t093a, t093d.

PARAMETERS: p_bukrs LIKE t093c-bukrs,
            p_set AS CHECKBOX DEFAULT 'X'.

SELECT SINGLE * FROM t093c
     WHERE bukrs = p_bukrs.

CHECK sy-subrc = 0.

SELECT SINGLE * FROM t093a
     WHERE afapl = t093c-afapl
       AND bertyp = '07'.    " Costing
CHECK sy-subrc = 0.


SELECT SINGLE * FROM t093
WHERE afapl = t093c-afapl
  AND afaber = t093a-afabe.


IF p_set = 'X'.
  t093-buhbkt = '3'. MODIFY t093.

*XRESTV	Smoothing (deprec. for past) when posting depreciation
*XKTDAU	Below zero account after end of planned life
*AFBRHY	Number of periods between two depreciation runs
*SOPNET	Indicator: Special reserves setting (net)
*ZINBUC	Indicator: Post imputed interest
*AFBKST	Indicator: Depreciation posting with cost center
*AFBAUF	Indicator: Depreciation posting on internal order
  t093d-bukrs  = p_bukrs.
  t093d-afaber = t093a-afabe.
  t093d-xrestv = 'X'.
  t093d-afbrhy = '1'.
  t093d-afbkst = 'X'.
  t093d-afbkst = 'X'.
  t093d-xrestv = 'X'.
  MODIFY t093d.

  WRITE:/ 'Depreciation Simulation for Costing Plan is Ready'.
  WRITE:/ t093-afaber, t093-buhbkt.
  WRITE:/ t093d-xrestv,
          t093d-afbrhy,
          t093d-afbkst,
          t093d-afbkst,
          t093d-xrestv.
ELSE.
  SELECT SINGLE * FROM t093d
  WHERE bukrs  = p_bukrs
    AND afaber = t093a-afabe.

  IF sy-subrc = 0.
    DELETE FROM t093d
      WHERE bukrs  = p_bukrs
        AND afaber = t093a-afabe.
  ENDIF.


  WRITE:/ 'Depreciation Simulation for Costing Plan is Disabled'.
  WRITE:/ t093-afaber, t093-buhbkt.
ENDIF.
