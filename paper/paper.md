---
title: 'sticRs: the R package for STICS users and developers'
authors:
- affiliation: '1,2'
  name: R. Vezy
  orcid: 0000-0002-0808-1461
- affiliation: '1,2'
  name: E. Justes
  orcid: 0000-0001-7390-7058
date: "30 August 2018"
output: word_document
bibliography: paper.bib
tags:
- R
- STICS
- agronomy
- sole crop
- intercrop
affiliations:
- index: 1
  name: INRA, UMR SYSTEM, Batiment 27, 2 Place Viala, 34060 Montpellier Cedex 1, France
- index: 2
  name: SYSTEM, Univ Montpellier, CIRAD, INRA, IRD, Montpellier SupAgro, Montpellier, France
---

# Summary

Crop models are often written in programming languages with relatively low-level of abstraction such as ``FORTRAN`` or ``C`` because they are well-suited for scientific calculation. However, higher-level tasks such as programmatic model parameterization, data manipulation (*e.g.* import/export of measurements and model outputs, statistics, plots), distributed computation, model evaluation or sensitivity analyses are often not integrated into the model, but rather made as an independent, external process. The ease of use and reproducibility of these steps depends strongly on the ability to program them [@Piccolo2016]. Hence, developers tend to design modelling environments for that purpose using different tools such as java applications, Python modules or R packages.

The STICS model [@Brisson:2008] is a dynamic, generic and robust model to simulate the soil-crop-atmosphere system. It has been developed for more than twenty years [@brisson1998stics] by more than 50 authors, and is now used by hundreds of users (1100 downloads as of 2012) across academic field (*e.g.* INRA, CIRAD, IRSTEA, more than 230 international papers), government agencies and technical institutions (Arvalis, French chamber of agriculture), international projects (H2020 ReMIX, agMIP), and schools (SupAgro). Hence, developing a complex model with as many users implies a thorough and careful development, while remaining easy to parameterize and use.

Excluding meteorological data, the STICS model needs more than 300 parameters for sole crops. These parameters are not always easily measurable on the field, measured with high variability, or simply not readily available to the user [@tremblay2004comparison; @launay2005assimilating]. Hence, a user should always evaluate the model sensitivity to the parameters before setting an approximated value [@VARELLA2010310].
The model has also different options for some formalisms (*e.g.* light interception), and the user should evaluate which is better suited for his particular use case by comparing the model outputs.

A developer that modifies a new formalism or designing a new one should also evaluate its impacts on the model outputs, and make a systematic comparison between different model versions or parameterization along field observations.

Therefore, STICS users and developers often experience the same different steps in their journey:

* Read, write or add parameters and their values
* Test the model programmatically for different parameter values, formalisms or model versions
* Evaluate the model against field observations
* Make plots
* Make a sensitivity analysis
* Distribute the runs across multiple cores / machines

``sticRs`` is an R package that was designed to make these steps as easy and fast as possible, and more importantly, reproducible. It has been adapted for the regular sole crop version of STICS, but also to its intercrop version [@CORREHELLOU200972].
The following diagram presents a typical workflow using the main functions from ``sticRs``: ![sticRs workflow](sticRs_workflow.png)

Evaluating the model is so fast and straightforward using ``sticRs`` that this step is often bypassed by the user to directly make a model evaluation using the ``stics_eval`` function, which is a wrapper of the previous functions. The package also automatically distributes the simulations across computer cores when the user call ``stics_eval`` and  ``sensitive_stics``.

``sticRs`` documentation is accessible on its dedicated website (https://vezy.github.io/sticRs), and its source code has been archived to Zenodo with a concept (*i.e.* permanent) DOI [@zenodo].

# Acknowledgements

The authors acknowledge the financial support from the European H2020 funded ReMIX project (https://www.remix-intercrops.eu).

# References
