% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_cost_effectiveness.R
\name{project_cost_effectiveness}
\alias{project_cost_effectiveness}
\title{Project cost effectiveness}
\usage{
project_cost_effectiveness(x)
}
\arguments{
\item{x}{project prioritization \code{\link[=problem]{problem()}}.}
}
\value{
A \code{\link[tibble:tibble]{tibble::tibble()}} table containing the following
columns:

\describe{

\item{\code{"project"}}{\code{character} name of each project}

\item{\code{"cost"}}{\code{numeric} cost of each project.}

\item{\code{"benefit"}}{\code{numeric} benefit for each project. For a
given project, this is calculated as the difference between (i) the
objective value for a solution containing all of the management actions
associated with the project and all zero cost actions, and (ii) the
objective value for a solution containing the baseline project.}

\item{\code{"ce"}}{\code{numeric} cost-effectiveness of each project.
For a given project, this is calculated as the difference between the
the benefit for the project and the benefit for the baseline project,
divided by the cost of the project. Note that the baseline
project will have a \code{NaN} value because it has a zero cost.}

\item{\code{"rank"}}{\code{numeric} rank for each project according to
is cost-effectiveness value. The project with a rank of one is the
most cost-effective project. Ties are accommodated using averages.}

}
}
\description{
Calculate the individual cost-effectiveness of each conservation project
in a project prioritization \code{\link[=problem]{problem()}}
(Joseph, Maloney & Possingham 2009).
}
\details{
Note that project cost-effectiveness cannot be calculated for
problems with minimum set objectives because the objective function
for these problems is to minimize cost and not maximize some measure
of biodiversity persistence.
}
\examples{
# load data
data(sim_projects, sim_features, sim_actions)

# print project data
print(sim_projects)

# print action data
print(sim_features)

# print feature data
print(sim_actions)

# build problem
p <- problem(sim_projects, sim_actions, sim_features,
             "name", "success", "name", "cost", "name") \%>\%
     add_max_richness_objective(budget = 400) \%>\%
     add_feature_weights("weight") \%>\%
     add_binary_decisions()

# print problem
print(p)

# calculate cost-effectiveness of each project
pce <- project_cost_effectiveness(p)

# print project costs, benefits, and cost-effectiveness values
print(pce)

# plot histogram of cost-effectiveness values
hist(pce$ce, xlab = "Cost effectiveness", main = "")
}
\references{
Joseph LN, Maloney RF & Possingham HP (2009) Optimal allocation of
resources among threatened species: A project prioritization protocol.
\emph{Conservation Biology}, \strong{23}, 328--338.
}
\seealso{
\code{\link[=solution_statistics]{solution_statistics()}}, \code{\link[=replacement_costs]{replacement_costs()}}.
}
