# script para analisar dados da tabela fipe de veiculos obtidos via 
# projeto http://www.github.com/avitaliano/scrapR


library(dplyr)
library(stringr)

download_dir <- "/Users/arnaldo/Projects/scrapR/fipe_data"

load_fipe_DF <- function(dir){
  fipe_files <- list.files(path = dir, pattern = "fipe*")
  fipe_files <- paste0(dir, "/", fipe_files)
  fipe.df <- do.call(rbind, lapply(fipe_files,
                                   function(x) read.csv(x, stringsAsFactors = FALSE)))
  return(fipe.df)
}

fipe.df <- load_fipe_DF(download_dir)

# adicionando colunas
fipe.df$moeda <- str_sub(fipe.df$Valor, 1, 2)
fipe.df$Valor <- as.numeric(str_replace(str_replace(str_sub(fipe.df$Valor, 4), "[.]", ""), ",", "."))
fipe.df$MesDatabase <- as.Date(paste0("01 ", fipe.df$MesReferencia), "%d %B de %Y")
fipe.df$ZeroKM <- fipe.df$AnoModelo == 32000

split_modelo <- function(modelo){
        tokens <- unlist(str_split(modelo, pattern = " "))
        if(tokens[1] == "New"){
                return(paste(tokens[1], tokens[2]))
        } else{
                return(tokens[1])
        }
}
fipe.df$Prefixo.modelo <- sapply(fipe.df$Modelo, split_modelo)
fipe.df %>% select(Modelo, Prefixo.modelo)

faixa_valor <- function(valor){

        faixas <- seq(from = 10, to = 100, by = 10)
        valor <- 50
        
        m <- cbind(faixas - 10, 
                   faixas, 
                   valor > faixas - 10 & valor <= faixas,
                   paste("de", faixas - 10, "a", faixas,"mil"))
        
        m[ m[,3] == 1]
}

fipe.df[ fipe.df$Valor == max(fipe.df$Valor), ]
unique(fipe.df$AnoModelo)

