import React, { useState, useEffect, useRef } from 'react';
import siteDataRaw from './data/siteData.json';
import computed from './data/computed.json';
import { SiteData } from './data/types';
import { Navbar } from './components/Navbar';
import { Hero } from './components/Hero';
import { SidebarNav, navItemsList } from './components/SidebarNav';
import { MethodSection } from './components/MethodSection';
import { CohortMatrixInspector } from './components/CohortMatrixInspector';
import { CalculatorSection } from './components/CalculatorSection';
import { Footer } from './components/Footer';

// Calculator inputs come from computed.json (replication/R/export_site_data.R, run on the CSV).
// siteData.json supplies only display labels (school names, calendar labels) and links.
const base = siteDataRaw as unknown as SiteData;
const D: SiteData = {
  ...base,
  entrant: {
    as_of: computed.entrant.as_of,
    years: computed.entrant.years,
    schools: Object.fromEntries(
      Object.entries(computed.entrant.schools).map(([k, v]) => [
        k,
        { ...v, name: base.entrant.schools[k].name, calendar_label: base.entrant.schools[k].calendar_label },
      ]),
    ) as SiteData['entrant']['schools'],
  },
};

export const App: React.FC = () => {
  const accent = '#0d4f30';
  const paper = '#f7f4ec';

  const [activeSection, setActiveSection] = useState<string>('overview');
  const rootRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    const handleScroll = () => {
      if (!rootRef.current) return;
      const ids = navItemsList.map((n) => n.id);
      let cur = ids[0];
      for (const id of ids) {
        const el = rootRef.current.querySelector(`[data-section="${id}"]`);
        if (el && el.getBoundingClientRect().top <= 140) {
          cur = id;
        }
      }
      setActiveSection(cur);
    };

    window.addEventListener('scroll', handleScroll, { passive: true });
    handleScroll();
    return () => window.removeEventListener('scroll', handleScroll);
  }, []);

  const scrollToSection = (id: string) => {
    if (!rootRef.current) return;
    const el = rootRef.current.querySelector(`[data-section="${id}"]`);
    if (el) {
      const top = el.getBoundingClientRect().top + window.scrollY - 64;
      window.scrollTo({ top, behavior: 'smooth' });
    }
  };

  const T = {
    hero: `Two-stage Higher Education enrollment and Net Tuition Revenue (NTR) cohort survival model. Stage 1 links longitudinal student enrollment records across terms and classifies tuition discount rates into five NACUBO-style financial aid tiers. Stage 2 projects four-year cohort headcount and Net Tuition Revenue across Millbury College of Music (MCM, 3 terms/year) and Millbury College Conservatory (MCC, 2 terms/year).`,
    calcTag: `02 · 4-Year Net Tuition Revenue & Tuition Discount Simulator`,
    calcIntro: `Adjust school calendar (MCM 3-Term vs. MCC 2-Term), entering class size, gross tuition per term, institutional grant discount rate (%), and term-to-term return rate shift to project four-year cohort headcount and Net Tuition Revenue.`,
    calcAssume: `Baseline projections use term-to-term return rates computed from the synthetic CSV, COVID terms excluded (as of ${D.entrant.as_of}) with Spring-to-Fall offset = 2 on MCM and offset = 1 on MCC.`,
  };

  return (
    <div
      ref={rootRef}
      className="min-h-screen text-[#23241f] selection:bg-[#0d4f30]/20 font-sans"
      style={{
        backgroundColor: paper,
      }}
    >
      <Navbar
        accent={accent}
        paper={paper}
        repoUrl={D.repo_url}
        linkedinUrl={D.linkedin_url}
        published={D.published}
        onNavigate={scrollToSection}
      />

      <Hero heroText={T.hero} />

      {/* Main Container with Sidebar + 4-Year Priority Stream */}
      <div className="max-w-[1200px] mx-auto px-6 md:px-10 pt-5 pb-14 grid grid-cols-1 lg:grid-cols-[200px_1fr] gap-8 lg:gap-12 items-start">
        <SidebarNav
          activeSection={activeSection}
          accent={accent}
          onNavigate={scrollToSection}
          published={D.published}
          repoUrl={D.repo_url}
          linkedinUrl={D.linkedin_url}
        />

        <main className="min-w-0 flex flex-col gap-8 md:gap-10">
          {/* 2. Longitudinal Cohort Matrix & Diagonal Census Highlight */}
          <CohortMatrixInspector accent={accent} />

          {/* 3. Entering class to four-year Net Tuition Revenue (NTR) */}
          <CalculatorSection
            accent={accent}
            siteData={D}
            calcTag={T.calcTag}
            calcIntro={T.calcIntro}
            calcAssume={T.calcAssume}
          />

          {/* Reference: Two-Stage Methodology & Higher Education Terminology Guide */}
          <MethodSection accent={accent} />

          <Footer
            repoUrl={D.repo_url}
            linkedinUrl={D.linkedin_url}
            published={D.published}
          />
        </main>
      </div>
    </div>
  );
};

export default App;



