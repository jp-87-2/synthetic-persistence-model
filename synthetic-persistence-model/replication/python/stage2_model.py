"""
Stage 2 cohort persistence & NTR model (Python / numpy / pandas).

Called from R through reticulate (see replication/R/pipeline.R). Two layers:

  A. Exact ports of src/utils/calculator.ts (term coding, aid tiers,
     calendar-aware survival curve, 4-year entrant NTR projection).
  B. Recomputation from the cohort tables embedded in the site source
     (src/components/CohortMatrixInspector.tsx, CohortsSection.tsx), parsed
     directly from the .tsx files: census diagonals, offset=2 vs offset=1,
     vintage actuals, combined discount tiers.
"""

import json
import re
from pathlib import Path

import numpy as np
import pandas as pd

# ---------------------------------------------------------------------------
# A. Exact ports of src/utils/calculator.ts
# ---------------------------------------------------------------------------

MULTIPLIER_MIN = 0.5
MULTIPLIER_MAX = 1.15


def term_to_int(term):
    clean = term.strip().upper()
    year, season = int(clean[:4]), clean[4:]
    if season == "SU":
        return year * 10 + 1
    if season == "FA":
        return year * 10 + 2
    if season == "SP":
        return (year - 1) * 10 + 3
    return year * 10


def int_to_term(term_int):
    year, suffix = divmod(int(term_int), 10)
    if suffix == 1:
        return f"{year}SU"
    if suffix == 3:
        return f"{year + 1}SP"
    return f"{year}FA"


def classify_aid_tier(aid_pct):
    r = aid_pct / 100
    if r < 0.15:
        return "No Award (<15%)"
    if r < 0.35:
        return "Minimal Aid (15–35%)"
    if r < 0.6:
        return "Partial Aid (35–60%)"
    if r < 0.8:
        return "Substantial Aid (60–80%)"
    return "Near-Full Aid (≥80%)"


def validate_calculator_inputs(entering, charge, aid_pct, multiplier):
    if entering < 0 or int(entering) != entering:
        return "Entering class must be a whole number, zero or more."
    if charge < 0:
        return "Gross tuition charge must be zero or more."
    if aid_pct < 0 or aid_pct > 100:
        return "Tuition discount rate must be between 0 and 100 percent."
    if multiplier < MULTIPLIER_MIN or multiplier > MULTIPLIER_MAX:
        return f"Retention multiplier must be between {MULTIPLIER_MIN} and {MULTIPLIER_MAX}."
    return ""


def build_cohort_survival_curve(rates, terms_per_year, spring_to_fall_offset,
                                multiplier=1.0, additive_delta_frac=0.0):
    curve = [1.0]
    clipped = False
    for i, raw in enumerate(rates):
        t = i + 1
        is_summer = terms_per_year == 3 and t % 3 == 2
        adj = raw * multiplier + additive_delta_frac * (0.25 if is_summer else 1.0)
        if adj > 1.0:
            adj, clipped = 1.0, True
        elif adj < 0.0:
            adj = 0.0
        is_fall_return = terms_per_year == 3 and t % 3 == 0
        ref = max(0, t - 2) if (is_fall_return and spring_to_fall_offset == 2) else t - 1
        curve.append(curve[ref] * adj)
    return curve, clipped


def compute_entrant_projection(cfg, years, entering, charge, aid_pct,
                               multiplier=1.0, retention_delta_pp=0.0):
    """Port of computeEntrantProjection(); returns (summary dict, rows DataFrame)."""
    error = validate_calculator_inputs(entering, charge, aid_pct, multiplier)
    tpy = int(cfg["terms_per_year"])
    offset = int(cfg.get("spring_to_fall_offset") or (2 if tpy == 3 else 1))
    fee = float(cfg.get("comp_fee_per_term") or 0.0)
    if error:
        return {"valid": False, "error": error}, pd.DataFrame()

    delta = retention_delta_pp / 100.0
    rates = list(cfg["rates"])
    curve, clipped = build_cohort_survival_curve(rates, tpy, offset, multiplier, delta)
    low, _ = build_cohort_survival_curve(rates, tpy, offset, multiplier, delta - 0.025)
    high, _ = build_cohort_survival_curve(rates, tpy, offset, multiplier, delta + 0.025)
    at = lambda c, i: c[i] if i < len(c) else 0.0

    aid_frac = aid_pct / 100.0
    aid_per_term = charge * aid_frac
    net_per_term = charge - aid_per_term + fee  # Gross - Grant + Comprehensive Fee

    rows = []
    for y in range(years):
        idx = range(y * tpy, y * tpy + tpy)
        terms = sum(entering * at(curve, i) for i in idx)
        lo = sum(entering * at(low, i) for i in idx)
        hi = sum(entering * at(high, i) for i in idx)
        entry_heads = round(entering * at(curve, y * tpy))
        rows.append({
            "year": y + 1,
            "entry_term_heads": entry_heads,
            "spring_term_heads": round(entering * at(curve, y * tpy + 1)),
            "summer_term_heads": round(entering * at(curve, y * tpy + 2)) if tpy == 3 else 0,
            "student_terms": terms,
            "end_of_year_survival": at(curve, (y + 1) * tpy),
            "fall_term_ntr": entry_heads * net_per_term,
            "gross_tuition": terms * (charge + fee),
            "institutional_aid": terms * aid_per_term,
            "ntr": terms * net_per_term,
            "low_ntr": lo * net_per_term,
            "high_ntr": hi * net_per_term,
        })
    df = pd.DataFrame(rows)
    total = df["ntr"].sum()
    summary = {
        "valid": True, "error": "", "clipped": clipped,
        "entering": entering, "charge_per_term": charge,
        "aid_per_term": aid_per_term, "comp_fee_per_term": fee,
        "net_per_term": net_per_term, "aid_pct": aid_pct,
        "total_ntr": total, "total_low_ntr": df["low_ntr"].sum(),
        "total_high_ntr": df["high_ntr"].sum(),
        "total_gross_tuition": df["gross_tuition"].sum(),
        "total_institutional_aid": df["institutional_aid"].sum(),
        "total_student_terms": df["student_terms"].sum(),
        "implied_fall_to_fall_retention": at(curve, tpy),
        "ntr_per_entrant": total / entering if entering > 0 else 0.0,
        "year1_share": df["ntr"].iloc[0] / total if total > 0 else 0.0,
        "aid_tier_label": classify_aid_tier(aid_pct),
        "aid_efficiency": net_per_term / (charge + fee) if charge + fee > 0 else 1 - aid_frac,
        "survival_curve": curve,
    }
    return summary, df


def layer_a(site):
    """Run the calculator on siteData.entrant and rebuild the stored curves."""
    ent = site["entrant"]
    out = {}
    for school, cfg in ent["schools"].items():
        summ, rows = compute_entrant_projection(
            cfg, int(ent["years"]), int(cfg["entering"]),
            float(cfg["charge_per_term"]), float(cfg["aid_rate"]) * 100, 1.0)
        naive, _ = build_cohort_survival_curve(cfg["rates"], cfg["terms_per_year"], 1)
        curve = summ["survival_curve"]
        out[school] = {
            "summary": {k: v for k, v in summ.items() if k != "survival_curve"},
            "rows": rows,
            "curve": pd.DataFrame({
                "school": school, "t": range(len(curve)),
                "rebuilt": curve, "site": list(cfg["curve"])[:len(curve)],
                "naive_offset1": naive,
            }),
            "ntr_per_student": round(cfg["charge_per_term"] * (1 - cfg["aid_rate"])
                                     + (cfg.get("comp_fee_per_term") or 0)),
        }
    return out


# ---------------------------------------------------------------------------
# B. Embedded site cohort tables (parsed from the .tsx source)
# ---------------------------------------------------------------------------

_OBS = re.compile(r"(\d+):\s*\{\s*calendarTerm:\s*'(\w+)',\s*heads:\s*(\d+)\s*\}")


def _parse_cohort_block(block, school):
    rows = []
    for chunk in block.split("cohortTerm:")[1:]:
        term = re.search(r"'(\w+)'", chunk).group(1)
        entrants = int(re.search(r"entrants:\s*(\d+)", chunk).group(1))
        intake = re.search(r"intakeType:\s*'(\w+)'", chunk).group(1)
        obs = [(int(d), cal, int(h)) for d, cal, h in _OBS.findall(chunk)]
        if not obs:
            rows.append(dict(school=school, cohort_term=term, entrants=entrants,
                             intake=intake, d=np.nan, calendar_term=None, heads=np.nan))
        for d, cal, h in obs:
            rows.append(dict(school=school, cohort_term=term, entrants=entrants,
                             intake=intake, d=d, calendar_term=cal, heads=h))
    return rows


def parse_site_tables(root):
    root = Path(root)
    insp = (root / "src/components/CohortMatrixInspector.tsx").read_text()
    mcm = insp.split("const MCM_COHORTS")[1].split("const MCC_COHORTS")[0]
    mcc = insp.split("const MCC_COHORTS")[1].split("const TRANSITION_RATES")[0]
    cohorts = pd.DataFrame(_parse_cohort_block(mcm, "MCM") + _parse_cohort_block(mcc, "MCC"))

    rates = {}
    for school, nc, wc in re.findall(
            r"(MCM|MCC):\s*\{\s*noCovid:\s*\[([^\]]*)\],\s*withCovid:\s*\[([^\]]*)\]", insp):
        rates[school] = {
            "noCovid": [float(x) for x in nc.replace("\n", " ").split(",") if x.strip()],
            "withCovid": [float(x) for x in wc.replace("\n", " ").split(",") if x.strip()],
        }

    cs = (root / "src/components/CohortsSection.tsx").read_text()
    block = cs.split("const MCC_DISCOUNT_TIERS")[1].split("];")[0]
    tiers = []
    for chunk in block.split("tier_id:")[1:]:
        g = lambda k: float(re.search(rf"{k}:\s*([\d.]+)", chunk).group(1))
        h, m, ret = g("headcount"), g("mean_ntr"), g("first_year_retention")
        retained = round(h * ret * m)
        tiers.append(dict(
            tier_id=int(re.match(r"\s*(\d+)", chunk).group(1)),
            discount_description=re.search(r"discount_description:\s*'([^']+)'", chunk).group(1),
            school="MCC", headcount=h, mean_ntr=m, first_year_retention=ret,
            exit_hazard=g("exit_hazard"), discount_rate=g("discount_rate"),
            retained_ntr=retained, ntr_at_risk=h * m - retained))
    return cohorts, rates, pd.DataFrame(tiers)


def step_term_by_int(term, steps, tpy):
    start = term_to_int(term)
    year, suffix = divmod(start, 10)
    for _ in range(steps):
        if tpy == 3:
            if suffix == 1:
                suffix = 2
            elif suffix == 2:
                suffix = 3
            else:
                year, suffix = year + 1, 1
        else:
            if suffix == 2:
                suffix = 3
            else:
                year, suffix = year + 1, 2
    return int_to_term(year * 10 + suffix)


def get_step_model_rate(intake, cal_term, d, tpy, rates, exclude_covid):
    if d <= 0:
        return 1.0
    if tpy == 2:
        return rates[min(len(rates) - 1, d - 1)]
    if d >= 12:
        return rates[11]
    if cal_term.endswith("SU"):
        return rates[min(3, (d - 1) // 3) * 3 + 1]
    if cal_term.endswith("FA"):
        if intake == "summer" and d == 1:
            return 112 / 120 if exclude_covid else 0.91
        return rates[min(2, (d - 2) // 3) * 3 + 2]
    return rates[min(3, (d - 1) // 3) * 3]


def project_cohort_with_rule(cohort_rows, school, tpy, offset_rule, covid_masked, rate_tbl):
    """Port of projectCohortWithRule() for one cohort; returns {d: cell}."""
    first = cohort_rows.iloc[0]
    term, entrants, intake = first["cohort_term"], int(first["entrants"]), first["intake"]
    observed = {int(r.d): (r.calendar_term, int(r.heads))
                for r in cohort_rows.itertuples() if not pd.isna(r.d)}
    rates_run = rate_tbl[school]["noCovid" if covid_masked else "withCovid"]
    baseline = rate_tbl[school]["noCovid"]
    is_baseline = covid_masked and offset_rule == (2 if school == "MCM" else 1)
    cells = {}
    for d in range(tpy * 4 + 1):
        cal = step_term_by_int(term, d, tpy)
        if d == 0:
            cells[0] = dict(calendar_term=cal, heads=entrants, projected=not observed)
            continue
        fall_ret = tpy == 3 and cal.endswith("FA") and d >= 2
        base_ref = d - 2 if fall_ret else d - 1
        ref = d - 2 if (fall_ret and offset_rule == 2) else d - 1
        ref_heads = cells.get(ref, cells[0])["heads"]
        base_rate = get_step_model_rate(intake, cal, d, tpy, baseline, True)
        act_rate = get_step_model_rate(intake, cal, d, tpy, rates_run, covid_masked)
        if d in observed:
            obs_ref = entrants if base_ref == 0 else observed.get(base_ref, (None, entrants))[1]
            obs_rate = observed[d][1] / max(1, obs_ref)
            scale = act_rate / base_rate if base_rate > 0 else 1.0
            target = min(1.0, max(0.0, obs_rate * scale))
            heads = observed[d][1] if is_baseline else round(ref_heads * target)
            cells[d] = dict(calendar_term=observed[d][0], heads=heads, projected=False)
        else:
            cells[d] = dict(calendar_term=cal, heads=round(ref_heads * act_rate), projected=True)
    return cells


def layer_b(site, root):
    cohorts, rate_tbl, mcc_tiers = parse_site_tables(root)
    ntr_ps = {s: round(c["charge_per_term"] * (1 - c["aid_rate"]) + c["comp_fee_per_term"])
              for s, c in site["entrant"]["schools"].items()}

    # Census diagonals under offset=2 (MCM baseline) vs naive offset=1
    census = []
    terms = {"MCM": ["2025SU", "2025FA", "2026SP", "2026SU", "2026FA"],
             "MCC": ["2025FA", "2026SP", "2026FA"]}
    for school in ("MCM", "MCC"):
        tpy = site["entrant"]["schools"][school]["terms_per_year"]
        sub = cohorts[cohorts.school == school]
        for rule in ((2, 1) if school == "MCM" else (1,)):
            for covid in (True, False):
                proj = {c: project_cohort_with_rule(g, school, tpy, rule, covid, rate_tbl)
                        for c, g in sub.groupby("cohort_term", sort=False)}
                for t in terms[school]:
                    heads = sum(cell["heads"] for cells in proj.values()
                                for cell in cells.values()
                                if cell["calendar_term"] == t and cell["heads"] > 0)
                    census.append(dict(school=school, offset=rule, exclude_covid=covid,
                                       census_term=t, heads=heads,
                                       ntr_m=heads * ntr_ps[school] / 1e6))
    census = pd.DataFrame(census)

    # Vintage actuals vs siteData.vintage (MCM cohort tables)
    vint = []
    for v in site["vintage"]:
        g = cohorts[(cohorts.school == "MCM") & (cohorts.cohort_term == v["cohort"])]
        g = g[g.d < v["terms_scored"]]
        vint.append(dict(
            cohort=v["cohort"], entrants=v["entrants"], terms_scored=v["terms_scored"],
            site_actual=v["actual_heads"], table_actual=g.heads.sum(),
            expected_heads=v["expected_heads"],
            rate_alpha=v["rate_alpha_heads"], mix_alpha=v["mix_alpha_heads"],
            total_alpha_ntr_m=v["total_alpha_ntr_m"]))
    vint = pd.DataFrame(vint)

    # Combined MCM + MCC discount tiers (CohortsSection.tsx combinedTiers)
    mcm = pd.DataFrame(site["discount_tiers"])
    comb = []
    for (_, m), (_, c) in zip(mcm.iterrows(), mcc_tiers.iterrows()):
        h = m.headcount + c.headcount
        haz = (m.headcount * m.exit_hazard + c.headcount * c.exit_hazard) / h
        comb.append(dict(discount_description=m.discount_description, headcount=h,
                         mean_ntr=round((m.retained_ntr + c.retained_ntr
                                         + m.ntr_at_risk + c.ntr_at_risk) / h),
                         exit_hazard=haz, first_year_retention=1 - haz,
                         retained_ntr=m.retained_ntr + c.retained_ntr,
                         ntr_at_risk=m.ntr_at_risk + c.ntr_at_risk))
    return {"cohorts": cohorts, "census": census, "vintage": vint,
            "mcc_tiers": mcc_tiers, "combined_tiers": pd.DataFrame(comb),
            "rates_noCovid_MCM": rate_tbl["MCM"]["noCovid"],
            "rates_noCovid_MCC": rate_tbl["MCC"]["noCovid"]}


def run_stage2(root):
    site = json.loads((Path(root) / "src/data/siteData.json").read_text())
    return {"A": layer_a(site), "B": layer_b(site, root)}
