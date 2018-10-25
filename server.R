library(shiny);#require(scales)

### Parte donde se procesa la data
options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output,session) {
  

  ### Carga de la data
  
  Data1 <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df.raw <- read.table(inFile$datapath, header=T, sep="\t", stringsAsFactors = F)
    #df.raw <- tratbase(df.raw)
    
    return(df.raw)
  })
  
  Data2 <- reactive({
    inFile <- input$file2
    if (is.null(inFile))
      return(NULL)
    df.raw <- read.table(inFile$datapath, header=T, sep="\t", stringsAsFactors = F)
    #df.raw <- tratbase(df.raw)
    
    return(df.raw)
  })
  
  
  
  
  output$matriz1 <- renderDataTable({
    
    matriz<-Data1()
    
    return(matriz)
    
    
  },
  options=list(
    paging = FALSE,
    searching=F
  ))
  
  output$matriz2 <- renderDataTable({
    
    matriz<-Data2()
    
    return(matriz)
    
    
  },
  options=list(
    paging = FALSE,
    searching=F
  ))  
    
  
  output$network_hello <- renderVisNetwork({
    
    prueba1 <- Data1()
    
    nodes <- tibble( id = 1:length(prueba1[,1]) , label = prueba1[,1]) 
    
    edges <- melt(prueba1,id="id")
    
    nodes$font.size <- input$tamano
    
    edges2 <- merge(edges,nodes,by.x="id",by.y = "label")
    
    edges2$id<-NULL
    
    edges2 <- merge(edges2,nodes,by.x="variable",by.y = "label")
    
    edges<- as_tibble(edges2[edges2$value>0,c("id.y","id","value")])
    
    names(edges)<-c("from","to","weight")
    
    edges <- mutate(edges, width = weight)
    
    
    visNetwork(nodes, edges) %>% 
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visEdges(arrows = "to")
    
  })
  
  
  ##################################################
  
  
    
    dataind <- reactive({  
    
    prueba1<-Data1()
    
    sp <- colSums(prueba1[,-1] >0 , na.rm=TRUE)
    
    x <- prueba1[,-1]
    
    x[x<0] = 0
    
    spval <- colSums(x , na.rm=TRUE)
    
    sn <- colSums(prueba1[,-1] < 0, na.rm=TRUE)
    
    y <- prueba1[,-1]
    
    y[y>0] = 0
    
    snval <- colSums( y , na.rm=TRUE)
    
    ep <- rowSums(prueba1[,-1] > 0, na.rm=TRUE)
    
    en <- rowSums(prueba1[,-1] < 0, na.rm=TRUE)
    
    prueba12 <- apply(prueba1[,-1], 2, function(x) ifelse(x <= 0, 0, x))
    
    prueba12 <- apply(prueba12, 2, function(x) ifelse(x > 0, 1, x))
    
    prueba2 <- as.data.frame(t(prueba12))
    
    names(prueba2) <- names(data.frame(prueba12))
    
    rp <- colSums(data.frame(prueba12)==prueba2 & data.frame(prueba12) > 0 & prueba2 > 0)
    
    prueba13 <- apply(prueba1[,-1], 2, function(x) ifelse(x < 0, 1, x))
    
    prueba13 <- apply(prueba13, 2, function(x) ifelse(x >= 0, 0, x))
    
    prueba3 <- as.data.frame(t(prueba13))
    
    names(prueba3) <- names(data.frame(prueba13))
    
    rn <- colSums(data.frame(prueba13)==prueba3 & data.frame(prueba13) > 0 & prueba3 > 0)
    
    prueba14 <- apply(prueba1[,-1], 2, function(x) ifelse(x > 0 , 1 , x))
    
    prueba14 <- apply(prueba14, 2, function(x) ifelse(x < 0, -1 , x))
    
    prueba4 <- as.data.frame(t(prueba14))
    
    names(prueba4) <- names(data.frame(prueba14))
    
    os <- colSums( (data.frame(prueba14) * prueba4) < 0)
    
    Popularidad <- round(sp/(length(prueba1[,1])-1),2)
    
    Antipatia <- round(sn/(length(prueba1[,1])-1),2)
    
    ExpansionPositiva <- round(ep/(length(prueba1[,1])-1),2)
    
    ExpansionNegativa <- round(en/(length(prueba1[,1])-1),2)
    
    ConeccionAfectiva <- ifelse(sp>0,round(rp/sp,2),0)
    
    Betweenness <- round(betweenness(g),2)
    
    NBetweenness <- round(betweenness(g, normalized=TRUE),2)
    
    Closeness.out <- round(closeness(g,normalized = TRUE, mode = c("out")),2)

    Closeness.in <- round(closeness(g,normalized = TRUE, mode = c("in")),2)
        
    indicadores <- data.frame(rbind(sp,spval,sn,snval,rp,rn,ep,en,os,
                                    Popularidad,Antipatia,ExpansionPositiva,
                                    ExpansionNegativa,ConeccionAfectiva,
                                    Betweenness,
                                    NBetweenness,
                                    Closeness.out,
                                    Closeness.in))
    
    
    return(indicadores)
    
    })
    
    output$indicadores <- renderDataTable({  
    
    datos <-  dataind()
    
    Total <- rowSums(datos)
    
    return(data.frame(cbind(datos,Total)))
    
  },
  options=list(
    paging = FALSE,
    searching=F
  ))

    output$ind_tot <- renderDataTable({
      
      indicadores <-  dataind()
      
      nodos <- Data1()
      
      data <-data.frame( rowSums(indicadores))
      data$ind <- row.names(data)
      
      g<-graph.adjacency(as.matrix(nodos[,-1]))
      
      Orden <- gorder(g)
      
      n <- length(nodos[,1])
      
      Densidad <- sp/ (n*(n-1))
      
      Asociacion <- round(data[data$ind=="rp",1]/((input$elecpos)*length(nodos[,1])),2)

      Disociacion <- round(data[data$ind=="rn",1]/((input$elecpos)*length(nodos[,1])),2)
      
      Coherencia <- round(data[data$ind=="rp",1]/data[data$ind=="sp",1],2)
      
      Intensidad.Social <- round(data[data$ind=="sp",1]+(data[data$ind=="sn",1]/(length(nodos[,1])-1)),2)
      
      final <- data.frame(rbind(Orden, Asociacion,
                              Disociacion,
                              Coherencia,
                              Intensidad.Social))
      names(final) <- "Valores"
      
      return(final)
      
    },
    options=list(
      paging = FALSE,
      searching=F
    ))
      
  
  ##### filtros #####################################
  
  output$filt <- renderUI({
    d <- Data2()
    x<- names(d)
    selectInput('carac', 'Escoja una caracteristica',
                choices = x ) #,selected = "Ninguno")
  })
  
  output$filt2 <- renderUI({
    d <- Data2()
    x<- d[,1]
    selectInput('per', 'Escoja una persona',
                choices = x ) #,selected = "Ninguno")
  })
  
  datass <- reactive({
    
    prueba1 <- Data1()
    data2 <- Data2()
    
    data2$label <- data2$id
    data2$id <- 1:length(data2$id)
    
    filtro <- input$carac
    
    nodes <- as_tibble(data2[,c("id","label",filtro)])
    
    names(nodes) <- c("id","label","group")

    nodes$font.size <- input$tamano
        
    edges <- melt(prueba1,id="id")
    
    
    edges2 <- merge(edges,nodes,by.x="id",by.y = "label")
    
    edges2$id<-NULL
    
    edges2 <- merge(edges2,nodes,by.x="variable",by.y = "label")
    
    edges<- as_tibble(edges2[edges2$value > 0,c("id.y","id","value")])
    
    names(edges)<-c("from","to","weight")
    
    edges <- mutate(edges, width = weight)
    
    list(nodes = nodes, edges = edges)
  })
  
  
  datassn <- reactive({
    
    prueba1 <- Data1()
    data2 <- Data2()
    
    data2$label <- data2$id
    data2$id <- 1:length(data2$id)
    
    filtro <- input$carac
    
    nodes <- as_tibble(data2[,c("id","label",filtro)])
    
    names(nodes) <- c("id","label","group")
    
    nodes$font.size <- input$tamano
    
    edges <- melt(prueba1,id="id")
    
    
    edges2 <- merge(edges,nodes,by.x="id",by.y = "label")
    
    edges2$id<-NULL
    
    edges2 <- merge(edges2,nodes,by.x="variable",by.y = "label")
    
    edges<- as_tibble(edges2[edges2$value < 0,c("id.y","id","value")])
    edges$value <- -1*edges$value
    names(edges)<-c("from","to","weight")
    
    edges <- mutate(edges, width = weight)
    
    list(nodes = nodes, edges = edges)
  })
  
  
  output$network_hello2 <- renderVisNetwork({
    

    visNetwork(datass()$nodes, datass()$edges) %>% 
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visEdges(arrows = "to") %>%
      visOptions(selectedBy = list(variable = "group"))
    
  })
  
  
  output$network_hello3 <- renderVisNetwork({
    
    
    visNetwork(datassn()$nodes, datassn()$edges) %>% 
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visEdges(arrows = "to", color = "red") %>%
      visOptions(selectedBy = list(variable = "group"))
    
  })
  
  
  output$network_hello4 <- renderVisNetwork({
    
    prueba1 <- Data1()
    
    nodes <- tibble( id = 1:length(prueba1[,1]) , label = prueba1[,1]) 
    
    nodes$font.size <- input$tamano
    
    edges <- melt(prueba1,id="id")
    
    
    edges2 <- merge(edges,nodes,by.x="id",by.y = "label")
    
    edges2$id<-NULL
    
    edges2 <- merge(edges2,nodes,by.x="variable",by.y = "label")
    
    edges<- as_tibble(edges2[edges2$value<0,c("id.y","id","value")])
    
    edges$value <- -1*edges$value
    
    names(edges)<-c("from","to","weight")
    
    edges <- mutate(edges, width = weight)
    
    
    visNetwork(nodes, edges) %>% 
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visEdges(arrows = "to",color="red")
    
  })
  
  
  output$iden2 <- renderUI({
    d <- Data2()
    x<- c(sort(unique(d[,"id"])))
    selectInput('ident2', 'Escoja un sujeto',
                choices = x)
  })
  
  output$segm3 <- renderUI({
    d <- Data1()
    x<- c("Todos",sort(unique(d[,"SEGMENTO"])))
    selectInput('segm3', 'Escoja un segmento para PD',
                choices = x,selected = "Todos")
  })
  
  
  output$values <- renderPrint({
    
    prueba1 <- Data1()
    
    var <- input$ident2
    
    sp <- sum(prueba1[,var] > 0, na.rm=TRUE)
    
    spval <- sum(prueba1[ prueba1[,var]>0 ,var] , na.rm=TRUE)
    
    sn <- sum(prueba1[,var] < 0, na.rm=TRUE)
    
    snval <- sum(prueba1[ prueba1[,var]<0 ,var] , na.rm=TRUE)
    
    ep <- sum(prueba1[prueba1$id == var,-1] > 0, na.rm=TRUE)
    
    en <- sum(prueba1[prueba1$id == var,-1] < 0, na.rm=TRUE)
    
    prueba12 <- apply(prueba1[,-1], 2, function(x) ifelse(x <= 0, 0, x))
    
    prueba12 <- apply(prueba12, 2, function(x) ifelse(x > 0, 1, x))
    
    prueba2 <- as.data.frame(t(prueba12))
    
    names(prueba2) <- names(prueba12)
    
    rp <- sum(data.frame(prueba12)[,var]==prueba2[,var])
    
    prueba13 <- apply(prueba1[,-1], 2, function(x) ifelse(x < 0, 1, x))
    
    prueba13 <- apply(prueba13, 2, function(x) ifelse(x >= 0, 0, x))
    
    prueba3 <- as.data.frame(t(prueba13))
    
    names(prueba3) <- names(prueba13)
    
    rn <- sum(data.frame(prueba13)[,var]==prueba3[,var])
    
    list(Popularidad=round(sp/(length(prueba1[,1])-1),2),
         Antipatia=round(sn/(length(prueba1[,1])-1),2),
         ExpansionPositiva = round(ep/(length(prueba1[,1])-1),2),
         ExpansionNegativa = round(en/(length(prueba1[,1])-1),2),
         ConeccionAfectiva=ifelse(sp>0,round(rp/sp,2),0))
    
        
  })


  
  
  })

