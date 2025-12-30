# =============================================
# figure_generator.py
# Generates Figure 1 for Medical Hypotheses paper
# Illustrative data based on NHANES-like parameters
# =============================================

import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

# Set style for scientific publication
plt.style.use('seaborn-v0_8-whitegrid')
plt.rcParams.update({
    'font.size': 11,
    'axes.titlesize': 13,
    'axes.labelsize': 12,
    'xtick.labelsize': 10,
    'ytick.labelsize': 10,
    'legend.fontsize': 10,
    'figure.dpi': 300,
    'savefig.dpi': 300,
    'savefig.bbox': 'tight',
    'savefig.pad_inches': 0.1
})

# =============================================
# 1. Generate realistic age-dependent parameters
# Based on NHANES patterns for non-diabetic adults
# =============================================

# Age range from 40 to 80 years
ages = np.linspace(40, 80, 41)

# Mean HbA1c: increases nonlinearly with age
# Based on NHANES patterns: ~5.4% at 40, ~5.8% at 80
mean_hba1c = 5.4 + 0.003 * (ages - 40) + 0.00005 * (ages - 40)**2

# Standard deviation: also increases with age
# Represents increasing heterogeneity in older populations
sd_hba1c = 0.3 + 0.001 * (ages - 40)

# =============================================
# 2. Create the figure
# =============================================

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

# =============================================
# Panel A: Age-specific distributions with examples
# =============================================

# Plot mean and ±1σ band
ax1.fill_between(ages, mean_hba1c - sd_hba1c, mean_hba1c + sd_hba1c,
                 alpha=0.2, color='steelblue', label='±1σ band (68% of population)')
ax1.plot(ages, mean_hba1c, 'b-', linewidth=2.5, label='Population mean μ(a)')

# Add ±2σ band for context
ax1.fill_between(ages, mean_hba1c - 2*sd_hba1c, mean_hba1c + 2*sd_hba1c,
                 alpha=0.1, color='steelblue', label='±2σ band (95% of population)')

# Example 1: Individual maintaining absolute HbA1c = 5.8%
example_age_points = [45, 55, 65, 75]
example_hba1c = 5.8

for age in example_age_points:
    idx = np.argmin(np.abs(ages - age))
    z_score = (example_hba1c - mean_hba1c[idx]) / sd_hba1c[idx]
    percentile = norm.cdf(z_score) * 100
    
    ax1.plot(age, example_hba1c, 'ro', markersize=9, markeredgecolor='black', markeredgewidth=0.5)
    ax1.text(age+0.8, example_hba1c+0.03, 
             f'z={z_score:.2f}\n({percentile:.0f}%)', 
             fontsize=9, verticalalignment='bottom')

# Example 2: Individual following 60th percentile trajectory
percentile_target = 60
z_target = norm.ppf(percentile_target/100)
example2_hba1c = mean_hba1c + z_target * sd_hba1c
ax1.plot(ages, example2_hba1c, 'g--', linewidth=2, alpha=0.7, 
         label=f'{percentile_target}th percentile trajectory')

# Formatting Panel A
ax1.set_xlabel('Age (years)', fontweight='bold')
ax1.set_ylabel('HbA1c (%)', fontweight='bold')
ax1.set_title('(A) Age-Conditioned Reference Distribution\nwith Example Measurements', 
              fontweight='bold', pad=15)
ax1.grid(True, alpha=0.3, linestyle='--')
ax1.legend(loc='upper left', framealpha=0.9)
ax1.set_xlim(40, 80)
ax1.set_ylim(5.0, 6.2)

# =============================================
# Panel B: Sigma positions over time
# =============================================

# Calculate z-scores for the two examples
z_example1 = (example_hba1c - mean_hba1c) / sd_hba1c  # Constant absolute value
z_example2 = np.full_like(ages, z_target)  # Constant percentile

# Plot sigma trajectories
ax2.plot(ages, z_example1, 'r-', linewidth=2.5, 
         label=f'HbA1c = {example_hba1c}% (constant absolute)')
ax2.plot(ages, z_example2, 'g-', linewidth=2.5, 
         label=f'{percentile_target}th percentile (constant relative)')

# Highlight specific ages
highlight_ages = [45, 55, 65, 75]
for age in highlight_ages:
    idx = np.argmin(np.abs(ages - age))
    ax2.plot(age, z_example1[idx], 'ro', markersize=8, markeredgecolor='black')
    ax2.plot(age, z_example2[idx], 'go', markersize=8, markeredgecolor='black')
    
    # Add annotations
    if age == 55:
        ax2.annotate(f'z={z_example1[idx]:.2f}', 
                    xy=(age, z_example1[idx]), 
                    xytext=(age+2, z_example1[idx]+0.3),
                    arrowprops=dict(arrowstyle='->', color='red', alpha=0.7),
                    fontsize=9, color='red')
        ax2.annotate(f'z={z_example2[idx]:.2f}', 
                    xy=(age, z_example2[idx]), 
                    xytext=(age+2, z_example2[idx]-0.3),
                    arrowprops=dict(arrowstyle='->', color='green', alpha=0.7),
                    fontsize=9, color='green')

# Add reference lines
ax2.axhline(y=0, color='b', linestyle=':', alpha=0.5, linewidth=1, label='Mean (z=0)')
ax2.axhline(y=1, color='gray', linestyle=':', alpha=0.3, linewidth=0.8)
ax2.axhline(y=-1, color='gray', linestyle=':', alpha=0.3, linewidth=0.8)
ax2.axhline(y=2, color='gray', linestyle=':', alpha=0.3, linewidth=0.8)
ax2.axhline(y=-2, color='gray', linestyle=':', alpha=0.3, linewidth=0.8)

# Add percentile labels on right y-axis
ax2_right = ax2.twinx()
percentile_ticks = [-2, -1, 0, 1, 2]
percentile_labels = [f'{norm.cdf(z)*100:.1f}%' for z in percentile_ticks]
ax2_right.set_ylim(ax2.get_ylim())
ax2_right.set_yticks(percentile_ticks)
ax2_right.set_yticklabels(percentile_labels)
ax2_right.set_ylabel('Percentile Position', fontweight='bold')

# Formatting Panel B
ax2.set_xlabel('Age (years)', fontweight='bold')
ax2.set_ylabel('Sigma Position (z-score)', fontweight='bold')
ax2.set_title('(B) Sigma Trajectories: Two Preventive Scenarios', 
              fontweight='bold', pad=15)
ax2.grid(True, alpha=0.3, linestyle='--')
ax2.legend(loc='upper left', framealpha=0.9)
ax2.set_xlim(40, 80)
ax2.set_ylim(-2.5, 3.5)

# =============================================
# 3. Add overall figure title and save
# =============================================

plt.suptitle('Illustrative Sigma Positioning for HbA1c\n(Using NHANES-like Population Parameters)', 
             fontsize=14, fontweight='bold', y=1.02)
plt.tight_layout()

# Save in multiple formats
plt.savefig('figure1_sigma_hba1c.png', dpi=300, bbox_inches='tight')
plt.savefig('figure1_sigma_hba1c.pdf', bbox_inches='tight')  # For publication
plt.savefig('figure1_sigma_hba1c.svg', bbox_inches='tight')  # For editing

print("Figure saved as:")
print("  - figure1_sigma_hba1c.png (for manuscript)")
print("  - figure1_sigma_hba1c.pdf (publication quality)")
print("  - figure1_sigma_hba1c.svg (editable vector)")

# Display the figure
plt.show()

# =============================================
# 4. Generate LaTeX code for including the figure
# =============================================

latex_code = '''
\\begin{{figure}}[htbp]
\\centering
\\includegraphics[width=\\textwidth]{{figure1_sigma_hba1c.png}}
\\caption{{\\textbf{{Illustrative sigma positioning for HbA1c using NHANES-like population parameters.}} 
(A) Age-specific reference distribution showing population mean HbA1c (blue line) with $\\pm1\\sigma$ (dark shading) and $\\pm2\\sigma$ bands (light shading). 
Red points show an individual maintaining constant absolute HbA1c (5.8\\%) at different ages, with corresponding z-scores and percentiles. 
The green dashed line shows a trajectory maintaining constant relative position (60th percentile). 
(B) Sigma trajectories over age: maintaining constant absolute value (red) leads to declining z-scores as population mean increases with age, 
while maintaining constant percentile (green) represents successful preventive positioning with rightward sigma drift. 
All values are illustrative for conceptual demonstration, using parameters consistent with NHANES population characteristics.}}
\\label{{fig:sigma_illustration}}
\\end{{figure}}
'''

print("\n" + "="*70)
print("LaTeX code for your paper:")
print("="*70)
print(latex_code)