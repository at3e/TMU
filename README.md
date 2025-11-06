# TMU

# 🌿 Air Pollution & Asthma Risk Modeling
This study investigates the temporal relationship between air pollution and asthma hospitalization using advanced time-series modeling. By applying Distributed Lag Non-Linear Models (DLNM), the project quantifies the delayed and nonlinear effects of six key pollutants over a 15-day lag period.
🔍 Objectives
- Analyze three years of outpatient asthma data and pollutant levels (CO, NO₂, O₃, SO₂, PM₂.₅, PM₁₀).
- Visualize seasonal trends and correlations using Pearson coefficients.
- Model exposure-lag-response associations with DLNM and estimate cumulative relative risks (RR).
- Identify pollutant-specific lag periods with peak hospitalization risk.
📊 Methodology
- Data Summary: Statistical profiling of pollutant concentrations and asthma cases (2011–2013).
- Correlation Analysis: Monthly averages correlated with asthma incidence.
- DLNM Modeling: Poisson-based spline models with cross-basis functions for lagged effects.
📈 Key Findings
- CO and NO₂ showed peak RR at lag 5 days (RR ≈ 1.15 and 1.05).
- SO₂ peaked at lag 7 (RR ≈ 1.5), O₃ at lag 10 (RR ≈ 1.01).
- PM₂.₅ and PM₁₀ had similar lag profiles with peak RR ≈ 1.01 at lag 10.
👩‍🔬 Author & Supervision
- Author: Atreyee Saha, IIT Madras
- Supervisor: Prof. Ben-Chang Shia, Taipei Medical University
🧰 Tech Stack
- Language: R / Python (for statistical modeling and visualization)
- Libraries: DLNM, ggplot2, spline functions
📁 Repository Structure
├── data/           # Raw and processed datasets
├── scripts/        # Analysis and modeling scripts
├── figures/        # Visualizations and plots
├── report/         # Final report and documentation
└── README.md       # Project overview





