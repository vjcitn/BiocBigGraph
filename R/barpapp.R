#' app for exploring the barplots
#' @import shiny
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @examples
#' example(simba_barplot) # defines sce, adds 'celltype'
#' data(p3k.fine)
#' sce$celltype = p3k.fine
#' if (interactive()) {
#' barpapp(sce, "celltype")
#' }
#' @export
barpapp = function(sce, colvar) {
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    helpText("barplot explorer"),
    selectInput("gene", "gene", selected="CST3", 
        choices=sort(rownames(altExp(sce, "sprobs")))), 
    numericInput("ncells", "nbars", min=10, max=ncol(sce), value=ncol(sce)/2),
    numericInput("totag", "nlabGini", min=15, max=50, value=20),
    width=2
   ),
   mainPanel(
    tabsetPanel(
     tabPanel("Barplot", plotlyOutput("barp")),
     tabPanel("Gini map", plotOutput("gini")),
     tabPanel("Cells", verbatimTextOutput("tab"))
    )
   )
  )
 )
 server = function(input, output) {
  output$gini = renderPlot({
   gi = gini(t(assay(altExps(sce)$sprobs)))
   ma = as.numeric(max_scores(sce))
   mydf = data.frame(max=as.numeric(ma), gini=gi, gene=rownames(altExps(sce)$sprobs))
   litd = mydf[order(mydf$gini,decreasing=TRUE),][seq_len(input$totag),]
   hh = mydf[mydf$gene=="GAPDH",] 
   litd = rbind(litd, hh)
   mm = ggplot(mydf, aes(x=max, y=gini, text=gene)) + geom_point() 
   mm + ggrepel::geom_text_repel(data=litd, 
       aes(x=max, y=gini, label=gene), colour="purple")
   })
  output$barp = renderPlotly({
   pp = simba_barplot(sce, sym=input$gene, colvar=colvar, maxrank=input$ncells)
   ggplotly(pp)
  })
  output$tab = renderPrint({
   table(sce[[colvar]])
  })
 }
 runApp(list(ui=ui, server=server))
}
 
