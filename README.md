# Feasibility of controlling 2019-nCoV outbreaks by isolation of cases and contacts

## Abstract


**Background:** To assess the viability of isolation and contact tracing to control onwards transmission from imported cases of 2019-nCoV.

**Methods:** We developed a stochastic transmission model, parameterised to the 2019-nCoV outbreak. We used the model to quantify the potential effectiveness of contact tracing and isolation of cases at controlling a 2019 nCoV-like pathogen. We considered scenarios that varied in: the number of initial cases; the basic reproduction number R0; the delay from symptom onset to isolation; the probability contacts were traced; the proportion of transmission that occurred before symptom onset, and the proportion of subclinical infections. We assumed isolation prevented all further transmission in the model. Outbreaks were deemed controlled if transmission ended within 12 weeks or before 5000 cases in total. We measured the success of controlling outbreaks using isolation and contact tracing, and quantified the weekly maximum number of cases traced to measure feasibility of public health effort. 

**Findings:** While simulated outbreaks starting with only 5 initial cases, R0 of 1.5 and little transmission before symptom onset could be controlled even with low contact tracing probability, the prospects of controlling an outbreak dramatically dropped with the number of initial cases, with higher R0, and with more transmission before symptom onset. Across different initial numbers of cases, the majority of scenarios with an R0 of 1.5 were controllable with under 50% of contacts successfully traced. For R0 of 2.5 and 3.5, more than 70% and 90% of contacts respectively had to be traced to control the majority of outbreaks. The delay between symptom onset and isolation played the largest role in determining whether an outbreak was controllable for lower values of R0. For higher values of R0 and a large initial number of cases, contact tracing and isolation was only potentially feasible when less than 1% of transmission occurred before symptom onset.

**Interpretation:** We found that in most scenarios contact tracing and case isolation alone is unlikely to control a new outbreak of 2019-nCov within three months. The probability of control decreases with longer delays from symptom onset to isolation, fewer cases ascertained by contact tracing, and increasing transmission before symptoms. This model can be modified to reflect updated transmission characteristics and more specific definitions of outbreak control to assess the potential success of local response efforts.

## Usage

### Set up

Set your working directory to the home directory of this project (or use the provided Rstudio project). Install the analysis and all dependencies with: 

```r
remotes::install_github("epiforecasts/ringbp", dependencies = TRUE)
```

### Run a single scenario

Run a single scenario for a 100 simulations.

```r
library(ringbp)
library(ggplot2)

res <- ringbp::scenario_sim(n.sim = 10, num.initial.cases = 1,prop.asym=0,
                     prop.ascertain = 0.2, cap_cases = 4500, cap_max_days = 350,
                     r0isolated = 0, r0community = 2.5, disp.com = 0.16, disp.iso = 1, delay_shape = 1.651524,
                     delay_scale = 4.287786,k = 0, quarantine = FALSE)

# Plot of weekly cases
ggplot2::ggplot(data=res, ggplot2::aes(x=week, y=cumulative, col = as.factor(sim))) +
  ggplot2::geom_line(show.legend = FALSE, alpha=0.3) +
  ggplot2::scale_y_continuous(name="Number of cases") + 
  ggplot2::theme_bw()

ringbp::extinct_prob(res,cap_cases = 4500)
```

### Run the full analysis

Run the analysis with the following:

```bash
Rscript inst/scripts/generate_results.R
```

### Generate figures

Render figures with the following:

```bash
Rscript inst/scripts/generate_figures.R
```

## Docker 

This analysis was developed in a docker container based on the tidyverse docker image. 

To build the docker image run (from the `ringbp` directory):

```bash
docker build . -t ringbp
```

To run the docker image run:

```bash
docker run -d -p 8787:8787 --name ringbp -e USER=ringbp -e PASSWORD=ringbp ringbp
```

The rstudio client can be found on port :8787 at your local machines ip. The default username:password is ringbp:ringbp, set the user with -e USER=username, and the password with - e PASSWORD=newpasswordhere. The default is to save the analysis files into the user directory.

To mount a folder (from your current working directory - here assumed to be `tmp`) in the docker container to your local system use the following in the above docker run command (as given mounts the whole `ringbp` directory to `tmp`).

```{bash, eval = FALSE}
--mount type=bind,source=$(pwd)/tmp,target=/home/ringbp
```

To access the command line run the following:

```{bash, eval = FALSE}
docker exec -ti ringbp bash
```

