# Plant-First Break-Even Radius Model for Decentralized Biochar Systems

## Overview

This repository documents a plant-first techno-economic modeling framework for evaluating decentralized biochar production systems.

The model links pyrolysis plant performance, product revenues, upstream biomass logistics, and transport distance to determine:
- The maximum payable feedstock price from the plant perspective
- The economically feasible biomass procurement radius
- The interaction between plant profitability and logistics cost

The approach follows a plant-centered logic: if the pyrolysis unit cannot operate profitably, the upstream biomass supply chain becomes economically irrelevant.

## System Basis

The modeled system is based on the Pyro-ClinX 150 unit (PyroPower GmbH, 2025).

Technical documentation: [Full technical documentation is available here](https://pyro-power.com/wp-content/uploads/2025/02/Pyro-ClinX-150-engl.-final-l-pyropower-GmbH.pdf)

Nominal technical characteristics include:
- Wet biomass throughput ≈ 315 kg/h
- Biochar yield ≈ 25% of dry mass
- Net electrical output ≈ 130 kW
- Nominal heat output ≈ 200 kW
- Annual biochar production ≈ 600 t/year (at 8,000 h operation)
- CO₂ equivalent storage ≈ 1,720 t/year (manufacturer specification)
- The plant represents a small-scale, decentralized pyrolysis system designed for regional biomass utilization.

## Interactive Web Application
![](https://github.com/manzim/break_even_distance/blob/master/break_even_distance_webApp_demo.gif)

An interactive implementation of the model is publicly available:

🔗 Live Application: **https://manzim.shinyapps.io/BE_radius/**

The web interface allows users to:
- Modify plant technical parameters
- Adjust market prices (biochar, electricity, heat)
- Change upstream logistics assumptions
- Simulate transport distance effects
- Observe real-time changes in payable price and break-even radius

### Output from the Interactive Web App
- Break Even distance with payable prices at the gate
- Relation between payable chip price vs biochar price

This interactive tool demonstrates the practical application of the conceptual framework described in this repository.

## Application Interface Demo



## Model Structure (Conceptual)

The model consists of three core components:

### 1. Plant Economics

- Calculation of total hourly revenue from the likes of Biochar sales, Electricity generation, Heat utilization etc. 
- Operating costs includes labor cost, O&M etc

From these values, the maximum payable chip price is derived.

### 2. Upstream Biomass Costs

Upstream cost estimation includes:Chipping costs, handling cost, transport related cost based on payload & speed

### 3. Break-Even Radius Concept

The break-even radius represents the maximum economically affordable transport distance at which total upstream logistics cost equals the plant’s maximum payable feedstock price.

The radius therefore defines the spatial boundary of feasible biomass procurement under given market and technical conditions.

## Key Insight

The maximum payable chip price is determined solely by plant performance and product markets.

Transport distance does not change the plant’s total payment capacity; it reduces the share available to suppliers after logistics costs are deducted.

This framework highlights the spatial limitations of decentralized biochar systems and the importance of feedstock aggregation within economically viable procurement zones.

## Repository Contents

### Conceptual system flowchart

### Model documentation summary
Model Documentation Summary

This repository documents a plant-centered techno-economic framework for assessing decentralized biochar production systems. The model integrates plant revenue generation, operating costs, upstream biomass processing, and transport logistics into a unified spatial-economic assessment.
The analytical structure follows a deterministic accounting logic:
- Plant revenues are calculated from biochar output, electricity generation, and heat utilization.
- Operating costs are deducted to determine the maximum payable feedstock price.
- Upstream costs include chipping, handling, fixed logistics overhead, and distance-proportional transport costs.
- The break-even radius defines the maximum procurement distance at which delivered biomass cost equals the plant’s payment capacity.

The model assumes steady-state operation at nominal technical capacity and continuous biomass supply. Transport is represented using productivity-based cost derivation (€/t·km) under full-load conditions.

The current documentation presents the conceptual structure and system logic only. Mathematical formulation, parameter calibration methodology, and computational implementation are not publicly distributed due to ongoing academic development.

**Future extensions may include:** 
- Multi-modal transport optimization
- Seasonal variability in logistics
- Spatial biomass availability constraints
- Sensitivity and uncertainty analysis

**Source code and mathematical implementation are not publicly distributed.**

## Code Availability
The computational model and implementation are not openly released at this stage due to ongoing academic work and potential future development.

Academic access may be requested via email.

## Snippets from the Live 

## About the Author
AHMED MANZIM RIDWAN
M.Sc. Student, Forest Information & Technology
Eberswalde University for Sustainable Development (HNEE), Germany

This work was developed as part of a research project course under the supervision of [**Dr. Peter Zander**](https://www.scopus.com/authid/detail.uri?authorId=6603131907).

## Contact for academic inquiries: 
Feel free to reach for any collaboration at **manzim.ridwan@gmail.com** 

## Rights and Usage
© 2026 [AHMED MANZIM RIDWAN]. All rights reserved.

This repository contains conceptual documentation only.
Reproduction, redistribution, or commercial use of the underlying model without permission is not permitted.
