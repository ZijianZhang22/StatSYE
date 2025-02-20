library(shiny)
library(tm)
library(stringr)

catHat_book <- readLines("~/stat 343 zzj/new/catinthehat.txt")
catHat_text <- Corpus(VectorSource(catHat_book))

catHat_clean <- tm_map(catHat_text, removePunctuation)
catHat_clean <- tm_map(catHat_clean, content_transformer(tolower))
catHat_clean <- tm_map(catHat_clean, removeNumbers)
catHat_clean <- tm_map(catHat_clean, stripWhitespace)

td_mat <- TermDocumentMatrix(catHat_clean, control = list(stopwords = NULL))
matrix <- as.matrix(td_mat)
sorted <- sort(rowSums(matrix), decreasing = TRUE)
data_text <- data.frame(word = names(sorted), freq = sorted)

k = unlist(str_split(catHat_clean$content, "\ "))
k <- Filter(nchar, k)

string_list = data_text$word

for(i in seq_along(k)){
  if (!(k[i] %in% string_list)) {
    string_list <- c(string_list, k[i])
  }
}

n <- length(string_list)
mat <- matrix(0, nrow = n, ncol = n)
rownames(mat) <- string_list
colnames(mat) <- string_list

for (i in seq_along(k) - 1){
  mat[k[i], k[i+1]] = mat[k[i], k[i+1]] + 1
}

row_dict <- setNames(vector("list", length(rownames(mat))), rownames(mat))
row_dict <- lapply(row_dict, function(x) 0)

len = length(rownames(mat))
len2 = length(catHat_clean)
for (i in 1:len2){
  text = catHat_clean[[i]]$content
  first_word <- word(text, 1)
  row_dict[[first_word]] = row_dict[[first_word]] + 1
}
count = unlist(row_dict)

ui <- fluidPage(
  titlePanel("Text Generation App"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("numWords", "Number of Words to Generate:", 
                   value = 10, min = 1, step = 1),
      actionButton("generateBtn", "Generate Sentence")
    ),
    
    mainPanel(
      h4("Generated Sentence:"),
      textOutput("generatedText")
    )
  )
)

server <- function(input, output, session) {
  
  generated_sentence <- eventReactive(input$generateBtn, {
    number = input$numWords
    topn_indices <- names(sort(count, decreasing = TRUE))[1:20]
    selected_col <- sample(topn_indices, 1)
    
    liststring <- list()
    liststring[[1]] = selected_col
    
    for(i in 2:number){
      pre = liststring[[i-1]]
      topm_indices <- names(sort(mat[pre, ], decreasing = TRUE))[1:2]
      selected_col2 <- sample(topm_indices, 1)
      liststring[i] = selected_col2
    }
    
    paste(unlist(liststring), collapse = " ")
  })
  
  output$generatedText <- renderText({
    generated_sentence()
  })
}

shinyApp(ui = ui, server = server)
