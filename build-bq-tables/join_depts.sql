SELECT
  *
FROM (
  SELECT
    SUBJECT_ID,
    HADM_ID,
    ICUSTAY_ID,
    ITEMID,
    CHARTTIME,
    CGID
  FROM
    MIMIC3_V1_4.CHARTEVENTS) AS charts
LEFT JOIN (
  SELECT
    ICUSTAY_ID,
    INTIME as INTIME_TRANS,
    OUTTIME as OUTTIME_TRANS,
    PREV_CAREUNIT,
    CURR_CAREUNIT,
    LOS as LOS_TRANS
  FROM
    MIMIC3_V1_4.TRANSFERS ) AS trans
USING
  (ICUSTAY_ID)
WHERE
  trans.INTIME_TRANS < charts.CHARTTIME
  AND charts.CHARTTIME < trans.OUTTIME_TRANS 