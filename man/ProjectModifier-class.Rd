% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ProjectModifier-proto.R
\name{ProjectModifier-class}
\alias{ProjectModifier-class}
\alias{ProjectModifier}
\title{Conservation problem modifier prototype}
\description{
This super-prototype is used to represent prototypes that in turn are used to
modify a \linkS4class{ProjectProblem} object. Specifically, the
\linkS4class{Constraint}, \linkS4class{Decision},
\linkS4class{Objective}, and \linkS4class{Target} prototypes
inherit from this class. \strong{Only experts should interact with
this class directly because changes to these class will have profound and
far reaching effects.}
}
\section{Fields}{


\describe{

\item{$name}{\code{character} name of object.}

\item{$parameters}{\code{list} object used to customize the modifier.}

\item{$data}{\code{list} object with data.}

\item{$compressed_formulation}{\code{logical} can this constraint be applied
to the compressed version of the conservation planning problem?. Defaults
to \code{TRUE}.}

}
}

\section{Usage}{


\code{x$print()}

\code{x$show()}

\code{x$repr()}

\code{x$get_data(name)}

\code{x$set_data(name, value)}

\code{x$calculate(cp)}

\code{x$output()}

\code{x$apply(op,cp)}

\code{x$get_parameter(id)}

\code{x$get_all_parameters()}

\code{x$set_parameter(id, value)}

\code{x$render_parameter(id)}

\code{x$render_all_parameter()}
}

\section{Arguments}{

\describe{

\item{name}{\code{character} name for object}

\item{value}{any object}

\item{id}{\code{id} or \code{name} of parameter}

\item{cp}{\linkS4class{ProjectProblem} object}

\item{op}{\linkS4class{OptimizationProblem} object}

}
}

\section{Details}{

\describe{

\item{print}{print the object.}

\item{show}{show the object.}

\item{repr}{return \code{character} representation of the object.}

\item{get_data}{return an object stored in the \code{data} field with
the corresponding \code{name}. If the object is not present in the
\code{data} field, a \code{waiver} object is returned.}

\item{set_data}{store an object stored in the \code{data} field with
the corresponding name. If an object with that name already
exists then the object is overwritten.}

\item{calculate}{\code{function} used to perform preliminary calculations
and store the data so that they can be reused later without
performing the same calculations multiple times. Data can be stored
in the \code{data} slot of the input \code{ProjectModifier} or
\code{ProjectProblem} objects.}

\item{output}{\code{function} used to generate an output from the object.
This method is only used for \linkS4class{Target} objects.}

\item{apply}{\code{function} used to apply the modifier to an
\linkS4class{OptimizationProblem} object.
This is used by \linkS4class{Constraint},
\linkS4class{Decision}, and \linkS4class{Objective} objects.}

\item{get_parameter}{retrieve the value of a parameter.}

\item{get_all_parameters}{generate \code{list} containing all the
parameters.}

\item{set_parameter}{change the value of a parameter to new value.}

\item{render_parameter}{generate a \emph{shiny} widget to modify the
the value of a parameter (specified by argument \code{id}).}

\item{render_all_parameters}{generate a \code{\link[shiny:reexports]{shiny::div()}}
containing all the parameters" widgets.}

}
}

