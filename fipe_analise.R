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
fipe.df$Valor.formatado <- fipe.df$Valor
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

faixa_valor <- function(valor, indice = 1){

        faixa_superior <- c(seq(from = 10, to = 100, by = 10),
                    seq(from = 200, to = 500, by = 100))
        
        faixa_inferior <- c(0, faixa_superior)
        faixa_inferior <- faixa_inferior[-length(faixa_inferior)]
        
        faixa_id <- 1:length(faixa_superior)
        m <- cbind(faixa_id,
                   faixa_inferior,
                   faixa_superior,
                   valor > faixa_inferior & valor <= faixa_superior,
                   paste("de", faixa_inferior, "a", faixa_superior,"mil"))
        
        return(m[ m[,4] == "TRUE", indice])
}

fipe.df$faixa.id <- sapply(as.integer(fipe.df$Valor / 1000), faixa_valor, 1)
fipe.df$faixa.valor <- sapply(as.integer(fipe.df$Valor / 1000), faixa_valor, 5)
fipe.df %>% select(Marca, Prefixo.modelo, Valor.formatado, faixa.valor) 
