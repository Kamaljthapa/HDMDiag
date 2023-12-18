#' Create a Mermaid diagram for Hierarchical Decision Modeling (HDM)
#'
#' This function generates a Mermaid diagram representing the HDM structure
#' with the given objective, criteria, and alternatives.
#'
#' @name createHDMDiagram
#' @param objective The main objective of the decision problem.
#' @param criteria A character vector specifying the criteria for evaluation.
#' @param alternatives A character vector specifying the alternative options.
#' @return A character string containing the Mermaid code for the diagram.
#' @examples
#' objective <- "Selecting the best site for a new sales office and showroom"
#' criteria <- c("Cost", "Distance from Downtown", "Floor Area", "Image of Location")
#' alternatives <- c("Galleria Building", "Pearl District", "Hillsboro", "Lakes Oswego")
#' mermaidCode <- createHDMDiagram(objective, criteria, alternatives)
#' DiagrammeR::mermaid(mermaidCode)
#'
#' @import DiagrammeR
#' @export
library(DiagrammeR)


createHDMDiagram <- function(objective, criteria, alternatives) {
  mermaidCode <- paste("
    graph TB
        ", paste0(sapply(seq_along(criteria), function(i) paste0("O[", objective, "] --> C", i, "[", criteria[i], "]")), collapse = "\n        "), "

        subgraph Criteria
            ", paste0(sapply(seq_along(criteria), function(i) paste0("C", i, "[", criteria[i], "]")), collapse = "\n            "), "
        end
        subgraph Alternatives
            ", paste0(sapply(seq_along(criteria), function(i) {
              paste0(sapply(seq_along(alternatives), function(j) {
                paste0("C", i, "[", criteria[i], "] --> A", j, "[", alternatives[j], "]")
              }), collapse = "\n            ")
            }), collapse = "\n            "), "
        end
        subgraph Objective
            O[", objective, "]
        end
    style O fill:#f9f,stroke:#333,stroke-width:2px
    ", paste0(sapply(seq_along(criteria), function(i) paste0("style C", i, " fill:#ccf,stroke:#333,stroke-width:1px")), collapse = "\n    "), "
    ", paste0(sapply(seq_along(alternatives), function(i) paste0("style A", i, " fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5")), collapse = "\n    "), "

    ", sep = "")

  return(mermaidCode)
}

objective <- "Selecting the best site for a new sales office and showroom"
criteria <- c("Cost", "Distance from Downtown", "Floor Area", "Image of Location")
alternatives <- c("Galleria Building", "Pearl District", "Hillsboro", "Lakes Oswego")

mermaidCode <- createHDMDiagram(objective, criteria, alternatives)

# Correct usage of DiagrammeR::mermaid
DiagrammeR::mermaid(mermaidCode)


