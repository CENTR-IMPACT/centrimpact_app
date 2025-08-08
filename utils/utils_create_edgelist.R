#' Create a multi-layer network edgelist
#'
#' @description Creates a multi-layer network with three layers where:
#' - Layer 1: Initial agents (researchers and community partners)
#' - Layer 2: Agents connected to Layer 1 based on influence parameters
#' - Layer 3: Agents connected to Layer 2 agents
#' 
#' @param degree_1_researchers_count Number of researcher nodes in Layer 1
#' @param degree_1_community_partners_count Number of community partner nodes in Layer 1
#' @param degree_1_researchers_influence Influence score for researchers (determines Layer 2 connections)
#' @param degree_2_community_partners_influence Influence score for community partners (determines Layer 2 connections)
#' @param degree_2_agents_influence Number of Layer 3 agents per Layer 2 agent
#' @param degree_1_cross_connection_pct Percentage of Layer 2 nodes that connect to an additional Layer 1 node (0-100)
#' @param degree_2_connection_pct Percentage of Layer 2 nodes that connect to another Layer 2 node (0-100)
#' @param degree_3_connection_pct Percentage of Layer 3 nodes that connect to another Layer 3 node (0-100)
#' @param degree_3_cross_connection_pct Percentage of Layer 3 nodes that connect to an additional Layer 2 node (0-100)
#' 
#' @return A list with two data frames:
#' - edgelist: Contains 'from', 'to', and 'layer' columns for all connections
#' - nodes: Contains 'id' and 'role' columns for all nodes
#' 
#' @noRd
create_edgelist <- function(degree_1_researchers_count,
                            degree_1_community_partners_count,
                            degree_1_researchers_influence,
                            degree_2_community_partners_influence,
                            degree_2_agents_influence,
                            degree_1_cross_connection_pct,
                            degree_2_connection_pct,
                            degree_3_connection_pct,
                            degree_3_cross_connection_pct) {
  
  logger::log_info("Starting create_edgelist function")
  # Create Layer 1 agents (starting from 1)
  layer1_total <- sum(degree_1_researchers_count, degree_1_community_partners_count)
  layer1_agents <- c(1:layer1_total)
  logger::log_info("Created {length(layer1_agents)} Layer 1 agents")
  
  # Initialize next available ID (starts after the last Layer 1 agent)
  next_id <- max(layer1_agents) + 1
  logger::log_info("Next available ID: {next_id}")

  # Create agent mapping (optional for undirected edge list, but kept for completeness)
  #agent_layers <- data.frame(agent_id = layer1_agents, layer = rep(1, length(layer1_agents)))

  # Connections will be stored as a list of data frames initially
  all_connections_list <- list()

  # Connect all Layer 1 agents with each other (undirected)
  # Generate all unique pairs to avoid duplicates in undirected network
  logger::log_info("Creating Layer 1 connections...")
  if (length(layer1_agents) > 1) {
    logger::log_info("Generating combinations for {length(layer1_agents)} agents")
    layer1_pairs <- combn(layer1_agents, 2)
    logger::log_info("Generated {ncol(layer1_pairs)} unique pairs")
    layer1_connections <- data.frame(
      from = layer1_pairs[1, ],
      to = layer1_pairs[2, ],
      layer = 1 # Associate layer with the connection within Layer 1
    )
    all_connections_list[[length(all_connections_list) + 1]] <- layer1_connections
  }

  # Parameters for Layer 2
  logger::log_info("Starting Layer 2 connections...")
  layer2_connections <- data.frame(from = integer(), to = integer(), layer = integer())
  layer2_agents <- c()
  
  # Initialize agent_layers if not already done
  if (!exists("agent_layers")) {
    logger::log_info("Initializing agent_layers data frame")
    agent_layers <- data.frame(agent_id = integer(), layer = integer())
  }
  
  # Determine number of Layer 2 agents based on influence parameters
  # Scale the number of connections by the influence parameters
  logger::log_info("Creating Layer 2 agents for {length(layer1_agents)} Layer 1 agents")
  for (agent in layer1_agents) {
    logger::log_info("Processing Layer 1 agent {agent}")
    # Determine if agent is a researcher or community partner
    is_researcher <- agent <= degree_1_researchers_count
    
    # Calculate number of connections based on influence
    if (is_researcher) {
      # For researchers, use degree_1_researchers_influence
      influence <- degree_1_researchers_influence
    } else {
      # For community partners, use degree_2_community_partners_influence
      influence <- degree_2_community_partners_influence
    }
    
    # Scale the number of connections based on influence (1-5 scale)
    # Convert influence (1-5) to a reasonable number of connections (e.g., 1-5)
    n_connections <- max(1, round(influence))
    
    # Create new agents
    new_agents <- next_id:(next_id + n_connections - 1)
    next_id <- next_id + n_connections
    layer2_agents <- c(layer2_agents, new_agents)
    
    # Add to agent mapping
    agent_layers <- rbind(
      agent_layers,
      data.frame(agent_id = new_agents, layer = rep(2, length(new_agents)))
    )
    
    # Layer 1 to Layer 2 connections (undirected)
    new_connections <- data.frame(
      from = rep(agent, n_connections),
      to = new_agents,
      layer = rep(2, n_connections)
    )
    
    layer2_connections <- rbind(layer2_connections, new_connections)
  }

  # Connect some Layer 2 agents to an additional Layer 1 agent based on percentage
  unique_layer2_agents <- unique(layer2_agents)
  n_layer2 <- length(unique_layer2_agents)
  n_layer1 <- length(layer1_agents)
  
  if (n_layer2 > 0 && n_layer1 > 1 && degree_1_cross_connection_pct > 0) {
    # Calculate how many Layer 2 agents should get an additional connection
    n_additional <- max(1, round(n_layer2 * (degree_1_cross_connection_pct / 100)))
    
    if (n_additional > 0) {
      # Select random Layer 2 agents to get an additional connection
      agents_to_connect <- sample(unique_layer2_agents, min(n_additional, n_layer2))
      
      additional_connections <- lapply(agents_to_connect, function(agent) {
        # Get the current Layer 1 connection for this agent
        current_connections <- layer2_connections$from[layer2_connections$to == agent]
        
        # Find all possible Layer 1 agents it's not already connected to
        possible_connections <- setdiff(layer1_agents, current_connections)
        
        if (length(possible_connections) > 0) {
          # Connect to one random additional Layer 1 agent
          new_connection <- sample(possible_connections, 1)
          return(data.frame(
            from = new_connection,
            to = agent,
            layer = 2
          ))
        }
        return(NULL)
      })
      
      # Combine all additional connections and add to the main list
      additional_connections <- do.call(rbind, Filter(Negate(is.null), additional_connections))
      if (nrow(additional_connections) > 0) {
        all_connections_list[[length(all_connections_list) + 1]] <- additional_connections
        logger::log_info("Added {nrow(additional_connections)} additional Layer 2 to Layer 1 connections")
      }
    }
  }
  all_connections_list[[length(all_connections_list) + 1]] <- layer2_connections


  # Connect Layer 2 agents with each other based on percentage
  layer2_internal_connections <- data.frame(from = integer(), to = integer(), layer = integer())

  if (n_layer2 > 1 && degree_2_connection_pct > 0) {
    # Calculate how many Layer 2 agents should be connected to at least one other
    n_to_connect <- max(1, round(n_layer2 * (degree_2_connection_pct / 100)))
    
    if (n_to_connect >= 2) {  # Need at least 2 agents to form a connection
      # Select agents to be part of the connected set
      connected_agents <- sample(unique_layer2_agents, min(n_to_connect, n_layer2))
      
      # Create a minimum spanning tree to ensure connectivity
      if (length(connected_agents) > 1) {
        # Create a simple path connecting all agents
        layer2_internal_connections <- data.frame(
          from = connected_agents[-length(connected_agents)],
          to = connected_agents[-1],
          layer = 2
        )
        
        # Add some random connections to create a more complex network
        n_possible_connections <- (length(connected_agents) * (length(connected_agents) - 1)) / 2
        n_additional <- min(round(n_possible_connections * 0.3), 10)  # Add up to 30% more connections
        
        if (n_additional > 0) {
          # Generate all possible pairs
          all_pairs <- t(combn(connected_agents, 2))
          # Remove pairs that are already connected
          existing_pairs <- paste(layer2_internal_connections$from, layer2_internal_connections$to)
          all_pairs <- all_pairs[!paste(all_pairs[,1], all_pairs[,2]) %in% existing_pairs, ]
          
          if (length(all_pairs) > 0 && nrow(all_pairs) > 0) {
            # Select random pairs to add
            idx <- sample(1:nrow(all_pairs), min(n_additional, nrow(all_pairs)))
            additional_connections <- data.frame(
              from = all_pairs[idx, 1],
              to = all_pairs[idx, 2],
              layer = 2
            )
            layer2_internal_connections <- rbind(layer2_internal_connections, additional_connections)
          }
        }
        
        # Add the connections to the main list
        if (nrow(layer2_internal_connections) > 0) {
          all_connections_list[[length(all_connections_list) + 1]] <- layer2_internal_connections
          logger::log_info("Added {nrow(layer2_internal_connections)} Layer 2 internal connections")
        }
      }
    }
  }

  # Layer 3: Connect Layer 2 agents to Layer 3 agents
  logger::log_info("Starting Layer 3 connections...")
  logger::log_debug("Starting Layer 3 connections...")
  layer3_connections <- data.frame(from = integer(), to = integer(), layer = integer())
  layer3_agents <- c()
  logger::log_info("Initialized empty data structures for Layer 3")
  logger::log_debug("Initialized empty data structures for Layer 3")

  # Each Layer 2 agent connects to degree_2_agents_influence number of Layer 3 agents
  unique_layer2_agents <- unique(layer2_agents)
  
  if (length(unique_layer2_agents) > 0 && degree_2_agents_influence > 0) {
    logger::log_info("Creating Layer 3 agents for {length(unique_layer2_agents)} Layer 2 agents")
    logger::log_debug("Creating Layer 3 agents for {length(unique_layer2_agents)} Layer 2 agents")
    for (agent in unique_layer2_agents) {
      logger::log_info("Processing Layer 2 agent {agent} for Layer 3 connections")
      # Determine number of Layer 3 agents to connect (at least 1, at most degree_2_agents_influence)
      n_connections <- min(degree_2_agents_influence, 3)  # Cap at 3 connections per agent
      
      # Create new Layer 3 agents
      new_agents <- next_id:(next_id + n_connections - 1)
      next_id <- next_id + n_connections
      layer3_agents <- c(layer3_agents, new_agents)
      
      # Add to agent mapping
      agent_layers <- rbind(
        agent_layers,
        data.frame(agent_id = new_agents, layer = rep(3, length(new_agents)))
      )
      
      # Create connections from Layer 2 to Layer 3
      new_connections <- data.frame(
        from = rep(agent, n_connections),
        to = new_agents,
        layer = rep(3, n_connections)
      )
      
      layer3_connections <- rbind(layer3_connections, new_connections)
    }
  }
  all_connections_list[[length(all_connections_list) + 1]] <- layer3_connections


  # Initialize data frames for new connections
  layer3_internal_connections <- data.frame(from = integer(), to = integer(), layer = integer())
  layer3_cross_connections <- data.frame(from = integer(), to = integer(), layer = integer())
  
  unique_layer3_agents <- unique(layer3_agents)
  n_layer3 <- length(unique_layer3_agents)
  unique_layer2_agents <- unique(layer2_agents)
  n_layer2 <- length(unique_layer2_agents)
  
  # 1. Internal Layer 3 connections
  # Connect Layer 3 agents with each other based on percentage
  if (n_layer3 > 1 && degree_3_connection_pct > 0) {
    # Calculate how many Layer 3 agents should be connected to at least one other
    n_to_connect <- max(1, round(n_layer3 * (degree_3_connection_pct / 100)))
    
    if (n_to_connect >= 2) {  # Need at least 2 agents to form a connection
      # Select agents to be part of the connected set
      connected_agents <- sample(unique(layer3_agents), min(n_to_connect, n_layer3))
      
      # Create a minimum spanning tree to ensure connectivity
      if (length(connected_agents) > 1) {
        # Create a simple path connecting all agents
        connections <- data.frame(
          from = connected_agents[-length(connected_agents)],
          to = connected_agents[-1],
          layer = 3
        )
        
        # Add some random connections to create a more complex network
        n_possible_connections <- (length(connected_agents) * (length(connected_agents) - 1)) / 2
        n_additional <- min(round(n_possible_connections * 0.2), 5)  # Add up to 20% more connections, max 5
        
        if (n_additional > 0) {
          # Generate all possible pairs
          all_pairs <- t(combn(connected_agents, 2))
          # Remove pairs that are already connected
          existing_pairs <- paste(connections$from, connections$to)
          all_pairs <- all_pairs[!paste(all_pairs[,1], all_pairs[,2]) %in% existing_pairs, ]
          
          if (length(all_pairs) > 0 && nrow(all_pairs) > 0) {
            # Select random pairs to add
            idx <- sample(1:nrow(all_pairs), min(n_additional, nrow(all_pairs)))
            additional_connections <- data.frame(
              from = all_pairs[idx, 1],
              to = all_pairs[idx, 2],
              layer = 3
            )
            connections <- rbind(connections, additional_connections)
          }
        }
        
        # Add the connections to the internal connections
        if (nrow(connections) > 0) {
          layer3_internal_connections <- rbind(layer3_internal_connections, connections)
          logger::log_info("Added {nrow(connections)} Layer 3 internal connections")
        }
      }
    }
  }
  
  # 2. Cross-layer connections from Layer 3 to Layer 2
  if (n_layer3 > 0 && n_layer2 > 1 && degree_3_cross_connection_pct > 0) {
    # Calculate how many Layer 3 agents should get an additional Layer 2 connection
    n_additional <- max(1, round(n_layer3 * (degree_3_cross_connection_pct / 100)))
    
    if (n_additional > 0) {
      # Select random Layer 3 agents to get an additional Layer 2 connection
      agents_to_connect <- sample(unique_layer3_agents, min(n_additional, n_layer3))
      
      additional_connections <- lapply(agents_to_connect, function(agent3) {
        # Get the primary Layer 2 connection for this agent
        current_connections <- layer3_connections$from[layer3_connections$to == agent3]
        
        # Find all possible Layer 2 agents it's not already connected to
        possible_connections <- setdiff(unique_layer2_agents, current_connections)
        
        if (length(possible_connections) > 0) {
          # Connect to one random additional Layer 2 agent
          new_connection <- sample(possible_connections, 1)
          return(data.frame(
            from = new_connection,
            to = agent3,
            layer = 3
          ))
        }
        return(NULL)
      })
      
      # Combine all additional connections
      additional_connections <- do.call(rbind, Filter(Negate(is.null), additional_connections))
      if (nrow(additional_connections) > 0) {
        layer3_cross_connections <- rbind(layer3_cross_connections, additional_connections)
        logger::log_info("Added {nrow(additional_connections)} additional Layer 3 to Layer 2 connections")
      }
    }
  }
  
  # Add the new connections to the connections list
  if (nrow(layer3_internal_connections) > 0) {
    all_connections_list[[length(all_connections_list) + 1]] <- layer3_internal_connections
  }
  if (nrow(layer3_cross_connections) > 0) {
    all_connections_list[[length(all_connections_list) + 1]] <- layer3_cross_connections
  }


  # Combine all connections
  logger::log_info("Combining all connections")
  
  # Remove any NULL or empty data frames from the list
  all_connections_list <- Filter(function(x) !is.null(x) && nrow(x) > 0, all_connections_list)
  logger::log_info("Found {length(all_connections_list)} non-empty connection data frames")
  
  # Handle case where no connections were created
  if (length(all_connections_list) == 0) {
    logger::log_warn("No valid connections were created in the network")
    return(list(
      edgelist = data.frame(from = integer(), to = integer(), layer = integer()),
      nodes = data.frame(id = integer(), role = character())
    ))
  }
  
  # Combine all connections
  all_connections <- do.call(rbind, all_connections_list)
  logger::log_info("Successfully combined {nrow(all_connections)} total connections")
  
  # Standardize undirected edges and remove duplicates
  # Ensure 'from' is always the smaller ID and 'to' is the larger ID
  standardized_connections <- as.data.frame(t(apply(all_connections[, c("from", "to")], 1, sort)))
  colnames(standardized_connections) <- c("from", "to")
  standardized_connections$layer <- all_connections$layer
  
  # Remove duplicate connections
  standardized_connections <- unique(standardized_connections)
  
  # Create a data frame of all nodes with their roles
  all_nodes <- unique(c(standardized_connections$from, standardized_connections$to))
  
  if (length(all_nodes) == 0) {
    return(list(
      edgelist = data.frame(from = integer(), to = integer(), layer = integer()),
      nodes = data.frame(id = integer(), role = character())
    ))
  }
  
  # Directly define Layer 1 nodes based on configuration
  total_layer1_nodes <- degree_1_researchers_count + degree_1_community_partners_count
  layer1_nodes <- 1:total_layer1_nodes
  
  logger::log_info("Defined {length(layer1_nodes)} Layer 1 nodes: {paste(layer1_nodes, collapse=', ')}")
  logger::log_info("Expected: {degree_1_researchers_count} researchers and {degree_1_community_partners_count} community members")
  
  # For Layer 1, assign roles based on the input counts
  if (length(layer1_nodes) > 0) {
    # The first 'degree_1_researchers_count' nodes are researchers
    # The next 'degree_1_community_partners_count' nodes are community members
    researchers <- 1:degree_1_researchers_count
    community_members <- (degree_1_researchers_count + 1):(degree_1_researchers_count + degree_1_community_partners_count)
    
    # Verify we have enough nodes
    if (max(community_members, researchers) > length(layer1_nodes)) {
      stop("Not enough Layer 1 nodes for the requested number of researchers and community members")
    }
    
    logger::log_info("Assigned {length(researchers)} researchers (IDs: {paste(researchers, collapse=', ')}) and {length(community_members)} community members (IDs: {paste(community_members, collapse=', ')})")
  } else {
    researchers <- integer(0)
    community_members <- integer(0)
  }
  
  # Debug: Print node counts before role assignment
  logger::log_info("Total nodes: {length(all_nodes)}")
  logger::log_info("Researcher IDs: {paste(researchers, collapse=', ')}")
  logger::log_info("Community Member IDs: {paste(community_members, collapse=', ')}")
  
  # Get layer 2 nodes (connected to layer 1)
  layer2_nodes <- unique(standardized_connections$to[standardized_connections$layer == 2])
  logger::log_info("Layer 2 nodes: {length(layer2_nodes)}")
  
  # Create node data frame with roles
  nodes <- data.frame(
    id = all_nodes,
    role = ifelse(all_nodes %in% researchers, "Researcher",
                 ifelse(all_nodes %in% community_members, "Community Member",
                        ifelse(all_nodes %in% layer2_nodes, 
                               "2nd Degree", "3rd Degree"))),
    stringsAsFactors = FALSE
  )
  
  # Debug: Print role counts
  role_counts <- table(nodes$role)
  logger::log_info("Final role assignments:")
  for (role in names(role_counts)) {
    logger::log_info("  {role}: {role_counts[role]}")
  }
  
  # Make sure we're using the final standardized connections
  final_edgelist <- standardized_connections[, c("from", "to", "layer"), drop = FALSE]
  
  # Remove any duplicate edges
  final_edgelist <- unique(final_edgelist)
  
  logger::log_info("Created edgelist with {nrow(final_edgelist)} edges and {nrow(nodes)} nodes")
  
  # Return both the edge list and node information
  list(
    edgelist = final_edgelist,
    nodes = nodes
  )
}
