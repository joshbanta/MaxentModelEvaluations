# MaxentModelEvaluations
Script for evaluating many combinations of maxent parameters for a set of layers and occurrence points.

How to use:
1. Add species occurrence points to the ‘occurrences’ folder (.csv files).
2. Add predictive layers to the 'layers' folder (.asc).

#### Inside the "Script" section of MaxentModelEvaluations

3. Edit the 'categoricals' variable to be a list of the layer indices that are categorical rather than numeric
4. Set 'create.bias' to TRUE to use a kernel density function to generate bg points, FALSE to generate uniformly random bg points.
5. Run the full MaxentModelEvaluations script. 
   - The "Install Packages and Files" section of the script should take care of installations of packages and the maxent java file.
6. Results will be placed into a 'models' folder with sub-folders for each species occurrence point file.


Maxent model evaluations makes use of a forked version of the ENMeval package, found on my GitHub, which adds MaxKappa as an evaluation metric.

This project is licensed under the terms of the GNU General Public License v3.0

Cite as: 
>Joseph Scavetta. (2019, July 15). jscavetta95/MaxentModelEvaluations: MaxentModelEvaluations (Version v1.0.0). Zenodo. http://doi.org/10.5281/zenodo.3337225

BibTeX:
```
@misc{joseph_scavetta_2019_3337225,
  author       = {Joseph Scavetta},
  title        = {{jscavetta95/MaxentModelEvaluations: 
                   MaxentModelEvaluations}},
  month        = jul,
  year         = 2019,
  doi          = {10.5281/zenodo.3337225},
  url          = {https://doi.org/10.5281/zenodo.3337225}
}
```
