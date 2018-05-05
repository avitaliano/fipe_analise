# script para analisar dados da tabela fipe de veiculos obtidos via 
# projeto http://www.github.com/avitaliano/scrapR


library(dplyr)
library(data.table)
library(stringr)
library(lubridate)
library(ggplot2)

download_dir <- "/Users/arnaldo/Projects/scrapR/fipe_data"

load_fipe_DF <- function(dir){
  fipe_files <- list.files(path = dir, pattern = "fipe*")
  fipe_files <- paste0(dir, "/", fipe_files)
  fipe.df <- do.call(rbind, lapply(fipe_files,
                                   function(x) fread(x, stringsAsFactors = FALSE)))
  return(fipe.df)
}

adiciona_campos_fipe <- function(fipe.df){
        # alterando tipo de dados de colunar
        fipe.df[, AnoModelo := as.numeric(AnoModelo)]
        # adicionando colunas
        fipe.df[, moeda := str_sub(Valor, 1, 2)]
        fipe.df[, Valor.formatado := Valor]
        fipe.df[, Valor := as.numeric(str_replace(str_replace(str_sub(Valor, 4), "[.]", ""), ",", "."))]
        fipe.df[, MesDatabase := as.Date(paste0("01 ", fipe.df$MesReferencia), "%d %B de %Y")]
        fipe.df[, ZeroKM := AnoModelo ==32000 ]
        fipe.df[ZeroKM == TRUE, AnoModelo := year(MesDatabase)]
        fipe.df[, Cambio := 'Manual']
        fipe.df[str_detect(Modelo, "Aut."), Cambio := 'Automático']
        
        split_modelo <- function(modelo){
                tokens <- unlist(str_split(modelo, pattern = " "))
                if(tokens[1] == "New"){
                        return(paste(tokens[1], tokens[2]))
                } else{
                        return(tokens[1])
                }
        }
        fipe.df$Prefixo.modelo <- sapply(fipe.df$Modelo, split_modelo)
        
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
        
        return(fipe.df)
}

fipe.df <- adiciona_campos_fipe(load_fipe_DF(download_dir))

valor_meu_carro <- as.numeric(fipe.df %>% filter(MesDatabase == "2016-11-01") %>%
        filter(CodigoFipe == '014045-7' & AnoModelo == 2013) %>% select(Valor))

tracker <- fipe.df %>% 
        filter(MesDatabase == "2016-11-01") %>%
        filter(str_detect(Modelo, "TRACKER") | str_detect(Modelo, "Tracker")) %>%
        filter(str_detect(Modelo, "4x4")) %>%
        arrange(desc(AnoModelo), desc(Valor)) %>%
        select(Marca, Modelo, AnoModelo, Combustivel, Valor.formatado, faixa.valor)

jipes <- fipe.df %>% 
        filter(MesDatabase == "2016-11-01") %>%
        # filter(str_detect(Modelo, "Pajero TR4") 
        #        | str_detect(Modelo, "Grand Vitara")
        #        | str_detect(Modelo, "Vitara")) %>%
        filter(! Prefixo.modelo %in% c("Ranger", "L200", "S10")) %>%
        filter(str_detect(Modelo, "4x4")) %>%
        #filter(Valor <= 70000) %>%
        #filter(Valor <= (valor_meu_carro * 1.2)) %>%
        filter(AnoModelo >= 2007) %>% 
        #arrange(desc(Valor), desc(AnoModelo)) %>%
        arrange(desc(AnoModelo), desc(Valor)) %>%
        select(Marca, Modelo, Cambio, AnoModelo, Combustivel, Valor.formatado, faixa.valor, Valor)

jipes
jipes %>% 
        filter(str_detect(Modelo, "Grand Vitara") & Cambio == "Automático" ) %>% 
        arrange(desc(AnoModelo)) %>% View

ggplot(data = jipes, aes(y = Valor, x = AnoModelo)) +
        geom_point(aes(colour = Modelo, size = 3))

