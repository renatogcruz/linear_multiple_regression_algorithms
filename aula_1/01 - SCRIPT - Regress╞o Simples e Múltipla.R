##################################################################################
#                 INSTALA«√O E CARREGAMENTO DE PACOTES NECESS¡RIOS               #
##################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


##################################################################################
#                               REGRESS√O LINEAR SIMPLES                         #
#                       EXEMPLO 01 - CARREGAMENTO DA BASE DE DADOS               #
##################################################################################

#Listar os arquivos do nosso project
list.files()

#Carregando a base de dados
load(file = "tempodist.RData")

#################################################################################
#                 OBSERVANDO OS DADOS CARREGADOS DO DATASET tempodist           #
#################################################################################
tempodist %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)

#Visualizando as observa√ß√µes e as especifica√ß√µes referentes √†s vari√°veis do dataset
glimpse(tempodist) 

#EstatÌsticas univariadas
summary(tempodist)

#################################################################################
#                             GR¡FICO DE DISPERS√O                              #
#################################################################################
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", se = F, size = 2) +
    xlab("Dist‚ncia") +
    ylab("Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)

#################################################################################
#            MODELAGEM DE UMA REGRESS√O LINEAR SIMPLES PARA O EXEMPLO 01        #
#################################################################################
#Estimando o modelo (ols)
modelo_tempodist <- lm(formula = tempo ~ distancia,
                       data = tempodist)

#Observando os par‚metros do modelo_tempodist
summary(modelo_tempodist) # p-value < 0.05 H¡ MODELO

#Outras maneiras de apresentar os outputs do modelo
#funÁ„o summ do pacote jtools
summ(modelo_tempodist, confint = T, digits = 4, ci.width = .95)
export_summs(modelo_tempodist, scale = F, digits = 4)

#Salvando fitted values (vari√°vel yhat) e residuals (vari√°vel erro) no dataset
tempodist$yhat <- modelo_tempodist$fitted.values
tempodist$erro <- modelo_tempodist$residuals

#Visualizando a base de dados com as vari√°veis yhat e erro
tempodist %>%
  select(tempo, distancia, yhat, erro) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Gr·fico did√°tico para visualizar o conceito de R¬≤
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", se = F, size = 2) +
    geom_hline(yintercept = 30, color = "grey50", size = .5) +
    geom_segment(aes(color = "Ychap√©u - Ym√©dio", x = distancia, xend = distancia,
                     y = yhat, yend = mean(tempo)), size = 0.7, linetype = 2) +
    geom_segment(aes(color = "Erro = Y - Ychap√©u", x = distancia, xend = distancia,
                     y = tempo, yend = yhat), size = 0.7, linetype = 3) +
    labs(x = "Dist√¢ncia",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = c("#55C667FF", "grey50", "#440154FF")) +
    theme_classic()
)

#C√°lculo manual do R¬≤
R2 <- (sum((tempodist$yhat - mean(tempodist$tempo))^2))/
      ((sum((tempodist$yhat - mean(tempodist$tempo))^2)) + (sum((tempodist$erro)^2)))

round(R2, digits = 4)

#coeficiente de ajuste (R¬≤) √© a correla√ß√£o ao quadrado
cor(tempodist[1:2])

#Modelo auxiliar para mostrar R¬≤ igual a 100% (para fins did√°ticos)
modelo_auxiliar <- lm(formula = yhat ~ distancia, #note que aqui o yhat √© a dependente
                   data = tempodist)
summary(modelo_auxiliar)

#Gr√°fico mostrando o perfect fit
my_plot <- 
  ggplot(tempodist, aes(x = distancia, y = yhat)) +
  geom_point(color = "#39568CFF", size = 5) +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm", se = F, size = 2) +
  labs(x = "Dist√¢ncia",
       y = "Tempo") +
  scale_color_manual("Legenda:",
                     values = "grey50") +
  theme_cowplot()
my_plot

#Com JPEG
ggdraw() + #fun√ß√µes ggdraw, draw_image e draw_plot do pacote cowplot
  draw_image("https://cdn.pixabay.com/photo/2017/02/16/10/20/target-2070972_960_720.png",
             x = 0.075, y = -0.15, scale = .34) +
  draw_image("https://imagensemoldes.com.br/wp-content/uploads/2019/10/O-Show-da-Luna-Luna-PNG-08.png",
             x = -0.235, y = 0.25, scale = .37) +
  draw_plot(my_plot)


##Voltando ao nosso modelo original:
#Plotando o Intervalo de Confian√ßa de 90%
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.90,) +
    labs(x = "Dist√¢ncia",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)

#Plotando o Intervalo de Confian√ßa de 95%
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.95) +
    labs(x = "Dist√¢ncia",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)

#Plotando o Intervalo de Confian√ßa de 99%
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.99) +
    labs(x = "Dist√¢ncia",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)

#Plotando o Intervalo de Confian√ßa de 99,999%
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.99999) +
    labs(x = "Dist√¢ncia",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)

#Calculando os intervalos de confian√ßa

confint(modelo_tempodist, level = 0.90) # siginific√¢ncia 10%
confint(modelo_tempodist, level = 0.95) # siginific√¢ncia 5%
confint(modelo_tempodist, level = 0.99) # siginific√¢ncia 1%
confint(modelo_tempodist, level = 0.99999) # siginific√¢ncia 0,001%

#Fazendo predi√ß√µes em modelos OLS - e.g.: qual seria o tempo gasto, em m√©dia, para
#percorrer a dist√¢ncia de 25km?
predict(object = modelo_tempodist,
        data.frame(distancia = 25)) # output = 41.35 (minutos)

#Caso se queira obter as predi√ß√µes com os IC
predict(object = modelo_tempodist,
        data.frame(distancia = 25),
        interval = "confidence", level = 0.95) # output = 41.35 (varia entre 34.8 a 47.8)


#####################################################################################
#     NOVA MODELAGEM PARA O EXEMPLO 01, COM NOVO DATASET QUE CONT…M REPLICA«’ES     #
#####################################################################################

# Quantas replica√ß√µes de cada linha voc√™ quer? -> fun√ß√£o slice
tempodistnovo <- tempodist %>%
  slice(rep(1:n(), each=3))

# Reestimando o modelo
modelo_tempodistnovo <- lm(formula = tempo ~ distancia,
                        data = tempodistnovo)

#Observando os par√¢metros do modelo_tempodistnovo
summary(modelo_tempodistnovo)

#Calculando os novos intervalos de confian√ßa
confint(modelo_tempodistnovo, level = 0.95) # siginific√¢ncia 5%

#Plotando o Novo Gr√°fico com Intervalo de Confian√ßa de 95%
#Note o estreitamento da amplitude dos intervalos de confian√ßa!
ggplotly(
  ggplot(tempodistnovo, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.95) +
    labs(x = "Dist√¢ncia",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)

#PROCEDIMENTO ERRADO: ELIMINAR O INTERCEPTO QUANDO ESTE N√O SE MOSTRAR
#ESTATISTICAMENTE SIGNIFICANTE
modelo_errado <- lm(formula = tempo ~ 0 + distancia,
                              data = tempodist)

#Observando os par‚metros do modelo_errado
summary(modelo_errado)

#Comparando os par‚metros do modelo_tempodist X modelo_errado
export_summs(modelo_tempodist, modelo_errado, scale = F, digits = 4)

#Gr·fico did·tico para visualizar o viÈs decorrente de se eliminar erroneamente
#o intercepto em modelos regressivos
my_plot2 <-
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
  geom_point(color = "#39568CFF", size = 2.5) +
  geom_smooth(aes(color = "Fitted Values OLS"),
              method = "lm", se = F, size = 1.5) +
  geom_segment(aes(color = "Sem Intercepto",
                   x = min(distancia),
                   xend = max(distancia),
                   y = modelo_errado$coefficients[1]*min(distancia),
                   yend = modelo_errado$coefficients[1]*max(distancia)),
               size = 1.5) +
  labs(x = "Dist√¢ncia",
       y = "Tempo") +
  scale_color_manual("Legenda:",
                     values = c("grey50", "#1F968BFF")) +
  theme_cowplot()
my_plot2

#Com JPEG
ggdraw() +
  draw_image("https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQKNf7Jk3b2LG23egCN7w7TW0275Vd2_lhYWHLlGGizplLYc74wLukF-EbOIB8YY8YB9L0&usqp=CAU",
             x = 0.065, y = -0.151, scale = .49) +
  draw_plot(my_plot2)

my_plot2
ggdraw() +
  draw_image("https://cdn.pixabay.com/photo/2014/04/03/00/36/mark-308835_960_720.png",
             x = -0.23, y = 0.24, scale = .25) +
  draw_image("https://i.pinimg.com/originals/38/13/f4/3813f4996821abb81b888c9a3f6d7c07.png",
             x = 0.07, y = -0.151, scale = .23) +
  draw_plot(my_plot2)
beep() #funÁ„o do pacote beepr


##################################################################################
#                              REGRESS√O LINEAR M⁄LTIPLA                         #
#                 EXEMPLO 02 - CARREGAMENTO DA BASE DE DADOS paises              #
##################################################################################

load(file = "paises.RData")

#EstatÌsticas univariadas
summary(paises)

#Gr·fico 3D com scatter
scatter3d(cpi ~ idade + horas,
          data = paises,
          surface = F,
          point.col = "#440154FF",
          axis.col = rep(x = "black",
                         times = 3))

##################################################################################
#                               ESTUDO DAS CORRELA«’ES                           #
##################################################################################
#Visualizando a base de dados
paises %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 20)

#A funÁ„o correlation do pacote correlation faz com que seja estruturado um
#diagrama interessante que mostra a inter-relaÁ„o entre as varii·veis e a
#magnitude das correlaÁıµes entre elas
#Requer instala√ß√£o e carregamento dos pacotes see e ggraph para a plotagem
paises %>%
  correlation(method = "pearson") %>%
  plot()

#A fun√ß√£o chart.Correlation() do pacote PerformanceAnalytics apresenta as
#distribui√ß√µes das vari√°veis, scatters, valores das correla√ß√µes e suas
#respectivas signific√¢ncias
chart.Correlation((paises[2:4]), histogram = TRUE)

##################################################################################
#     ESTIMANDO UM MODELO M√öLTIPLO COM AS VARI√ÅVEIS DA BASE DE DADOS paises      #
##################################################################################
#Estimando a Regress√£o M√∫ltipla
modelo_paises <- lm(formula = cpi ~ . - pais,
                    data = paises)            # o . significa todo mundo (menos pais, neste caso)

#Par√¢metros do modelo
summary(modelo_paises)
confint(modelo_paises, level = 0.95) # siginific√¢ncia de 5%

#Outro modo de apresentar os outputs do modelo - fun√ß√£o summ do pacote jtools
summ(modelo_paises, confint = T, digits = 3, ci.width = .95)
export_summs(modelo_paises, scale = F, digits = 5)

#Salvando os fitted values na base de dados
paises$cpifit <- modelo_paises$fitted.values

#Gr·fico 3D com scatter e fitted values
scatter3d(cpi ~ idade + horas,
          data = paises,
          surface = T, fit = "linear",
          point.col = "#440154FF",
          axis.col = rep(x = "black",
                         times = 3))


##################################################################################
#              REGRESS√O COM UMA VARI¡VEL EXPLICATIVA (X) QUALITATIVA            #
#                  EXEMPLO 03 - CARREGAMENTO DA BASE DE DADOS                    #
##################################################################################

load(file = "corrupcao.RData")

##################################################################################
#                    OBSERVANDO OS DADOS CARREGADOS DA BASE corrupcao            #
##################################################################################
glimpse(corrupcao) #Visualiza√ß√£o das observaÁıes e das especificaÁıes referentes
                   #·s vari·veis da base de dados

levels(glimpse(corrupcao$regiao)) #Observando os r√≥tulos da vari√°vel regiao
table(corrupcao$regiao) #Tabela de frequ√™ncias da vari√°vel regiao

#EstatÌsticas univariadas
summary(corrupcao)

#Estimando um modelo, erroneamente, com o problema da ponderaÁ„o arbitr·ria
modelo_corrupcao <- lm(formula = cpi ~ as.numeric(regiao), 
                       data = corrupcao)

#Observando os par√¢metros do modelo_corrupcao
summary(modelo_corrupcao)

#Calculando os intervalos de confian√ßa

confint(modelo_corrupcao, level = 0.95) # siginific√¢ncia 5%

#Plotando os fitted values do modelo_corrupcao considerando, PROPOSITALMENTE, a
#pondera√ß√£o arbitr√°ria, isto √©, assumindo que a Am√©rica do Sul vale 1; que a 
#Oceania vale 2; a Europa, 3; EUA e Canad√°, 4; e √Åsia, 5.
corrupcao %>%
  mutate(rotulo = paste(pais, cpi)) %>%
  ggplot(aes(x = as.numeric(regiao), y = cpi, label = rotulo)) +
  geom_point(color = "#FDE725FF") +
  stat_smooth(aes(color = "Fitted Values"),
              method = "lm", 
              formula = y ~ x) +
  labs(x = "Regi√£o",
       y = "Corruption Perception Index") +
  scale_x_discrete(labels = c("1" = "Am√©rica do Sul", 
                                "2" = "Oceania", 
                                "3" = "Europa", 
                                "4" = "EUA e Canad√°", 
                                "5" = "√Åsia")) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  geom_text_repel() +
  theme_bw()

#################################################################################
#                            PROCEDIMENTO N-1 DUMMIES                           #
#################################################################################
#Dummizando a vari·vel regiao. O cÛdigo abaixo, automaticamente, far·: a) o
#estabelecimento de dummies que representaÁ„o cada uma das regiıes da base de 
#dados; b)remover· a vari·vel dummizada original; c) estabelecer· como categoria 
#de referÍncia a dummy mais frequente.
corrupcao_dummies <- dummy_columns(.data = corrupcao,
                                   select_columns = "regiao",
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = T)

#Visualizando a base de dados dummizada
corrupcao_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

##################################################################################
#                        ESTIMA«√O DO MODELO DE REGRESS√O                        #
##################################################################################
#Modelagem com todas as vari·veis
modelo_corrupcao_dummies <- lm(cpi ~ . - pais, corrupcao_dummies)

#Par√¢metros do modelo_corrupcao_dummies
summary(modelo_corrupcao_dummies)

#Plotando o modelo_corrupcao_dummies de forma interpolada
my_plot3 <- 
corrupcao %>%
  mutate(rotulo = paste(pais, cpi)) %>%
  ggplot(aes(x = as.numeric(regiao), y = cpi, label = rotulo)) +
  geom_point(color = "#FDE725FF") +
  stat_smooth(aes(color = "Fitted Values"),
              method = "lm", 
              formula = y ~ bs(x, df = 4)) +
  labs(x = "Regi√£o",
       y = "Corruption Perception Index") +
  scale_x_discrete(labels = c("1" = "Am√©rica do Sul", 
                                "2" = "Oceania", 
                                "3" = "Europa", 
                                "4" = "EUA e Canad√°", 
                                "5" = "√Åsia")) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  geom_text_repel() +
  theme_bw()
my_plot3

#Com GIF
ggsave("my_plot3.png")
my_plot3 <- image_read("my_plot3.png") #fun√ß√£o do pacote magick

gif <- image_read("https://i.pinimg.com/originals/89/2e/09/892e09e6609a951fa45d6799cc3fa3f3.gif")

frames <- image_composite(my_plot3, gif, offset = "+750+30")

animation <- image_animate(frames, fps = 10) #fun√ß√£o do pacote magick
image_scale(animation, "x550")
beep("treasure")


##################################################################################
#             REGRESS√O N√O LINEAR SIMPLES E TRANSFORMA«√O DE BOX-COX            #
#                   EXEMPLO 04 - CARREGAMENTO DA BASE DE DADOS                   #
##################################################################################

load(file = "bebes.RData")

#Estat·sticas univariadas
summary(bebes)

#Gr·fico de dispers„o
ggplotly(
  bebes %>% 
    ggplot() +
    geom_point(aes(x = idade, y = comprimento),
               color = "grey20", alpha = 0.6, size = 2) +
    labs(x = "Idade em semanas",
         y = "Comprimento em cm") +
    theme_bw()
)

#Gr·fico de dispers„o com ajustes (fits) linear e n„o linear
ggplotly(
  bebes %>% 
    ggplot() +
    geom_point(aes(x = idade, y = comprimento),
               color = "grey20", alpha = 0.6, size = 2) +
    geom_smooth(aes(x = idade, y = comprimento),
                method = "lm", color = "#FDE725FF", se = F) +
    geom_smooth(aes(x = idade, y = comprimento),
                color = "#440154FF", se = F) +
    labs(x = "Idade em semanas",
         y = "Comprimento em cm") +
    theme_bw()
)

#EstimaÁ„o do modelo OLS linear
modelo_linear <- lm(formula = comprimento ~ idade,
                    data = bebes)

summary(modelo_linear)

##################################################################################
#          TESTE DE VERIFICA«√O DA ADER NCIA DOS RESÕDUOS ¿ NORMALIDADE          #
#                               SHAPIRO-FRANCIA                                  #
##################################################################################
#  Shapiro-Wilk: n <= 30 (amostras menores, atÈ 30)
## shapiro.test(modelo_linear$residuals) 

#Shapiro-Francia: n > 30 (amostras maiores que 30)
sf.test(modelo_linear$residuals) #funÁ„o sf.test do pacote nortest

#Histograma dos resÌduos do modelo OLS linear
bebes %>%
  mutate(residuos = modelo_linear$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_linear$residuals),
                            sd = sd(modelo_linear$residuals)),
                aes(color = "Curva Normal TeÛrica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "ResÌduos",
       y = "FrequÍncia") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

##################################################################################
#                             TRANSFORMAÁ„o DE BOX-COX                           #
##################################################################################
#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(bebes$comprimento) #fun√ß√£o powerTransform do pacote car#
lambda_BC

#Inserindo o lambda de Box-Cox na base de dados para a estimaÁ„o de um novo modelo
bebes$bc_comprimento <- (((bebes$comprimento ^ lambda_BC$lambda) - 1) / 
                            lambda_BC$lambda)

#Estimando um novo modelo OLS com vari·vel dependente transformada por Box-Cox
modelo_bc <- lm(formula = bc_comprimento ~ idade,
                data = bebes)

#Par‚metros do modelo
summary(modelo_bc)

#Comparando os par‚metros do modelo_linear com os do modelo_bc
#CUIDADO!!! OS PAR‚METROS N√O S√O DIRETAMENTE COMPAR¡VEIS!
export_summs(modelo_linear, modelo_bc, scale = F, digits = 4)

#Repare que h· um salto na qualidade do ajuste para o modelo n„o linear (R≤)
data.frame("R≤OLS" = round(summary(modelo_linear)$r.squared, 4),
           "R≤BoxCox" = round(summary(modelo_bc)$r.squared, 4)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)

#Teste de Shapiro-Francia para os resÌduos do modelo_bc
sf.test(modelo_bc$residuals) #funÁ„o sf.test do pacote nortest

#Histograma dos resÌduos do modelo_bc
bebes %>%
  mutate(residuos = modelo_bc$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "gray90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_bc$residuals),
                            sd = sd(modelo_bc$residuals)),
                aes(color = "Curva Normal Te√≥rica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  labs(x = "ResÌduos",
       y = "FrequÍncia") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

#Fazendo prediÁıes com os modelos OLS linear e Box-Cox
#qual È o comprimento esperado de um bebÍ com 52 semanas de vida?
#Modelo OLS Linear:
predict(object = modelo_linear,
        data.frame(idade = 52),
        interval = "confidence", level = 0.95)

#Modelo N√£o Linear (Box-Cox):
predict(object = modelo_bc,
        data.frame(idade = 52),
        interval = "confidence", level = 0.95)
#N√o podemos nos esquecer de fazer o c·lculo para a obtenÁ„o do fitted
#value de Y (vari·vel 'comprimento')
(((54251.12 * 2.659051) + 1)) ^ (1 / 2.659051)

#Salvando os fitted values dos dois modelos (modelo_linear e modelo_bc) no
#dataset 'bebes'
bebes$yhat_linear <- modelo_linear$fitted.values
bebes$yhat_modelo_bc <- (((modelo_bc$fitted.values*(lambda_BC$lambda))+
                                    1))^(1/(lambda_BC$lambda))

#Visualizando os fitted values dos dois modelos no dataset
bebes %>%
  select(idade, comprimento, yhat_linear, yhat_modelo_bc) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 20)

#Ajustes dos modelos: valores previstos (fitted values) X valores reais
bebes %>%
  ggplot() +
  geom_smooth(aes(x = comprimento, y = yhat_linear, color = "OLS Linear"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = comprimento, y = yhat_linear),
             color = "#FDE725FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = comprimento, y = yhat_modelo_bc, color = "Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = comprimento, y = yhat_modelo_bc),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = comprimento, y = comprimento), method = "lm", 
              color = "gray30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("#440154FF", "#FDE725FF")) +
  labs(x = "Comprimento", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
      panel.grid = element_line("grey95"),
      panel.border = element_rect(NA),
      legend.position = "bottom")


##################################################################################
#                             REGRESS√O N√O LINEAR M⁄LTIPLA                      #
#                       EXEMPLO 05 - CARREGAMENTO DA BASE DE DADOS               #
##################################################################################

load(file = "empresas.RData")

#EstatÌsticas univariadas
summary(empresas)

##################################################################################
#                               ESTUDO DAS CORRELA«’ES                           #
##################################################################################
#A funÁ„o correlation do pacote correlation faz com que seja estruturado um
#diagrama interessante que mostra a inter-relaÁ„o entre as vari·veis e a
#magnitude das correlaÁıes entre elas
#Requer instalaÁ„o e carregamento dos pacotes see e ggraph para a plotagem
empresas %>%
  correlation(method = "pearson") %>%
  plot()

#A funÁ„o chart.Correlation do pacote PerformanceAnalytics apresenta as
#distribuiÁıes das vari·veis, scatters, valores das correlaÁıes e suas
#respectivas signific‚ncias
chart.Correlation((empresas[2:6]), histogram = TRUE)

#A funÁ„o corr_plot do pacote metan tambÈm apresenta as distribuiÁıes
#das vari·veis, scatters, valores das correlaÁıes e suas respectivas
#signific‚ncias
empresas %>%
  corr_plot(retorno, disclosure, endividamento, ativos, liquidez,
            shape.point = 21,
            col.point = "black",
            fill.point = "#FDE725FF",
            size.point = 2,
            alpha.point = 0.6,
            maxsize = 4,
            minsize = 2,
            smooth = TRUE,
            col.smooth = "black",
            col.sign = "#440154FF",
            upper = "corr",
            lower = "scatter",
            diag.type = "density",
            col.diag = "#440154FF",
            pan.spacing = 0,
            lab.position = "bl")

##################################################################################
#     ESTIMANDO UM MODELO M⁄LTIPLO COM AS VARI·VEIS DA BASE DE DADOS empresas    #
##################################################################################
# Visualizando a base de dados
empresas %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

#Estimando a Regress√£o M√∫ltipla
modelo_empresas <- lm(formula = retorno ~ . - empresa,
                      data = empresas)

#Par√¢metros do modelo
summary(modelo_empresas)

#Note que o par‚metro da vari·vel 'endividamento' n„o È estatisticamente
#significante ao nÌvel de signific‚ncia de 5% (nÌvel de confianÁa de 95%)

##################################################################################
#                                 PROCEDIMENTO STEPWISE                          #
##################################################################################
#Aplicando o procedimento Stepwise, temos o seguinte cÛdigo:
step_empresas <- step(modelo_empresas, k = 3.841459)

#De onde vem o argumento k = 3.841459?
qchisq(p = 0.05, df = 1, lower.tail = F)
round(pchisq(3.841459, df = 1, lower.tail = F),7)

summary(step_empresas)
#Este procedimento no R removeu a vari·vel 'endividamento'. Note que a vari·vel
#'disclosure' tambÈm acabou sendo excluÌda apÛs o procedimento Stepwise, nesta
#forma funcional linear!

export_summs(step_empresas, scale = F, digits = 5)

#Par‚metros reais do modelo com procedimento Stepwise
confint(step_empresas, level = 0.95) # siginific√¢ncia 5%
plot_summs(step_empresas, colors = "#440154FF") #fun√ß√£o plot_summs do pacote ggstance

#Par‚metros padronizados
plot_summs(step_empresas, scale = TRUE, colors = "#440154FF")

#Adicionando a caracteriza√ß√£o da distribi√ß√£o normal no IC de cada par√¢metro beta
plot_summs(step_empresas, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "#440154FF")

#Comparando os ICs dos betas dos modelos sem e com procedimento Stepwise
plot_summs(modelo_empresas, step_empresas, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("#FDE725FF", "#440154FF"))

##################################################################################
#          TESTE DE VERIFICA«√O DA ADER NCIA DOS RESÕDUOS ¿ NORMALIDADE          #
#                               SHAPIRO-FRANCIA                                  #
##################################################################################
#Shapiro-Francia: n > 30
sf.test(step_empresas$residuals) #funÁ„o sf.test do pacote nortest

#Plotando os resÌduos do modelo step_empresas
empresas %>%
  mutate(residuos = step_empresas$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  labs(x = "ResÌduos",
       y = "FrequÍncia") + 
  theme_bw()

#Acrescentando uma curva normal teÛrica para comparaÁ„o entre as distribuiÁıes
empresas %>%
  mutate(residuos = step_empresas$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_empresas$residuals),
                            sd = sd(step_empresas$residuals)),
                size = 2, color = "grey30") +
    scale_color_manual(values = "grey50") +
    labs(x = "ResÌduos",
         y = "FrequÍncia") +
  theme_bw()

##################################################################################
#                            TRANSFORMA«√O DE BOX-COX                            #
##################################################################################
#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(empresas$retorno) #funÁ„o powerTransform do pacote car#
lambda_BC

#Inserindo o lambda de Box-Cox na base de dados para a estimaÁ„o de um novo modelo
empresas$bcretorno <- (((empresas$retorno ^ lambda_BC$lambda) - 1) / 
                            lambda_BC$lambda)

#Visualizando a nova vari·vel na base de dados
empresas %>%
  select(empresa, retorno, bcretorno, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

#Estimando um novo modelo m˙ltiplo com vari·vel dependente transformada por Box-Cox
modelo_bc <- lm(formula = bcretorno ~ . -empresa -retorno, 
                data = empresas)

#Par‚metros do modelo
summary(modelo_bc)

#Aplicando o procedimento Stepwise
step_modelo_bc <- step(modelo_bc, k = 3.841459)

summary(step_modelo_bc)
#Note que a vari·vel 'disclosure' acaba voltando ao modelo na forma funcional n√o linear!

#Verificando a normalidade dos resÌduos do modelo step_modelo_bc
sf.test(step_modelo_bc$residuals) #funÁ„o sf.test do pacote nortest

#Plotando os novos resÌduos do step_modelo_bc
empresas %>%
    mutate(residuos = step_modelo_bc$residuals) %>%
    ggplot(aes(x = residuos)) +
    geom_histogram(aes(y = ..density..),
                   color = "white",
                   fill = "#287D8EFF",
                   bins = 30,
                   alpha = 0.6) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(step_modelo_bc$residuals),
                              sd = sd(step_modelo_bc$residuals)),
                  size = 2, color = "grey30") +
    scale_color_manual(values = "grey50") +
    labs(x = "ResÌduos",
         y = "FrequÍncia") +
    theme_bw()

#Resumo dos dois modelos obtidos pelo procedimento Stepwise (linear e com Box-Cox)
#FunÁ„o export_summs do pacote jtools
export_summs(step_empresas, step_modelo_bc, scale = F, digits = 6)

#Par‚metros reais do modelo com procedimento Stepwise e Box-Cox
confint(step_modelo_bc, level = 0.95) # siginific‚ncia 5%
plot_summs(step_modelo_bc, colors = "#287D8EFF") #funÁ„o plot_summs do pacote ggstance

#Par‚metros padronizados
plot_summs(step_modelo_bc, scale = TRUE, colors = "#287D8EFF")

#Adicionando caracterizaÁ„o da distribuÌÁ„o normal no IC de cada par‚metro beta
plot_summs(step_modelo_bc, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "#287D8EFF")

#Comparando os ICs do betas dos modelos sem e com TransformaÁ„oo de Box-Cox
plot_summs(step_empresas, step_modelo_bc, scale = T, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("#440154FF", "#287D8EFF"))

#Fazendo prediÁıes com o step_modelo_bc, e.g.: qual È o valor do retorno, em
#mÈdia, para disclosure igual a 50, liquidez igual a 14 e ativo igual a 4000,
#ceteris paribus?
predict(object = step_modelo_bc, 
        data.frame(disclosure = 50, 
                   liquidez = 14, 
                   ativos = 4000),
        interval = "confidence", level = 0.95)

#N√o podemos nos esquecer de fazer o c·lculo para a obtenÁ„o do fitted
#value de Y (retorno)
(((3.702015 * -0.02256414) + 1)) ^ (1 / -0.02256414)

#Salvando os fitted values dos modelos step_empresas e step_modelo_bc no
#dataset empresas
empresas$yhat_step_empresas <- step_empresas$fitted.values
empresas$yhat_step_modelo_bc <- (((step_modelo_bc$fitted.values*(lambda_BC$lambda))+
                                    1))^(1/(lambda_BC$lambda))

#Visualizando os dois fitted values no dataset
#modelos step_empresas e step_modelo_bc
empresas %>%
  select(empresa, retorno, yhat_step_empresas, yhat_step_modelo_bc) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

#Ajustes dos modelos: valores previstos (fitted values) X valores reais
empresas %>%
  ggplot() +
  geom_smooth(aes(x = retorno, y = yhat_step_empresas, color = "Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = retorno, y = yhat_step_empresas),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = retorno, y = yhat_step_modelo_bc, color = "Stepwise Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = retorno, y = yhat_step_modelo_bc),
             color = "#287D8EFF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = retorno, y = retorno), method = "lm", 
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("#287D8EFF", "#440154FF")) +
  labs(x = "Retorno", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")


##################################################################################
#            DIAGN”STICO DE MULTICOLINEARIDADE EM MODELOS DE REGRESS√O           #
#                   EXEMPLO 06 - CARREGAMENTO DA BASE DE DADOS                   #
##################################################################################

load("salarios.RData")

##################################################################################
#                OBSERVANDO OS DADOS CARREGADOS DA BASE salarios                 #
##################################################################################

#Estat·sticas univariadas
summary(salarios)

##CORRELA«√O PERFEITA:
cor(salarios$rh1, salarios$econometria1)

salarios %>% select(2:4) %>% 
  correlation(method = "pearson") %>%
  plot()

modelo1 <- lm(formula = salario ~ rh1 + econometria1,
              data = salarios)

summary(modelo1)

##CORRELA«√O BAIXA:
cor(salarios$rh3, salarios$econometria3)

salarios %>% select(2,7,8) %>% 
  correlation(method = "pearson") %>%
  plot()

modelo3 <- lm(formula = salario ~ rh3 + econometria3,
              data = salarios)

summary(modelo3)

#DiagnÛstico de multicolinearidade (Variance Inflation Factor e Tolerance)
ols_vif_tol(modelo3)
#funÁ„o ols_vif_tol do pacote olsrr

##CORRELA«√O MUITO ALTA, POR…M N√O PERFEITA:
cor(salarios$rh2, salarios$econometria2)

salarios %>% select(2,5,6) %>% 
  correlation(method = "pearson") %>%
  plot()

modelo2 <- lm(formula = salario ~ rh2 + econometria2,
              data = salarios)

summary(modelo2)
ols_vif_tol(modelo2)


##################################################################################
#           DIAGN”STICO DE HETEROCEDASTICIDADE EM MODELOS DE REGRESS√O           #
#                   EXEMPLO 07 - CARREGAMENTO DA BASE DE DADOS                   #
##################################################################################

load(file = "saeb_rend.RData")

##################################################################################
#               OBSERVANDO OS DADOS CARREGADOS DA BASE saeb_rend                 #
##################################################################################

#EstatÌsticas univariadas
summary(saeb_rend)

#Tabela de frequÍncias absolutas das vari·veis 'uf' e rede'
table(saeb_rend$uf)
table(saeb_rend$rede)

#Plotando saeb em fun√ß√£o de rendimento, com linear fit
ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb)) +
    geom_point(size = 1, color = "#FDE725FF") +
    geom_smooth(method = "lm", 
                color = "grey40", se = F) +
    xlab("rendimento") +
    ylab("saeb") +
    theme_classic()
)

#Plotando saeb em fun√Á„oo de rendimento, com destaque para rede escolar 
ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb, color = rede, shape = rede)) +
    geom_point(size = 1) +
    xlab("rendimento") +
    ylab("saeb") +
    scale_colour_viridis_d() +
    theme_classic()
)

#Plotando saeb em fun√Á„o de rendimento, com destaque para rede escolar e linear fits
ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb, color = rede, shape = rede)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = F) +
    xlab("rendimento") +
    ylab("saeb") +
    scale_colour_viridis_d() +
    theme_classic()
)

##################################################################################
#                       ESTIMA«√O DO MODELO DE REGRESS√O E                       #
#                       DIAGN√STICO DE HETEROCEDASTICIDADE                       #                                                            
##################################################################################
#EstimaÁ„o do modelo
modelosaeb <- lm(formula = saeb ~ rendimento,
                 data = saeb_rend)

summary(modelosaeb)

#Teste de Breusch-Pagan para diagnÛstico de heterocedasticidade
ols_test_breusch_pagan(modelosaeb)
#funÁ„o ols_test_breusch_pagan do pacote olsrr
#PresenÁa de heterocedasticidade -> omiss„o de vari·vel(is) explicativa(s) relevante(s)

#H0 do teste: ausÍncia de heterocedasticidade.
#H1 do teste: heterocedasticidade, ou seja, correla√Á„o entre resÌduos e uma ou mais
#vari·veis explicativas, o que indica omiss„o de vari·vel relevante!

#################################################################################
#              PROCEDIMENTO N-1 DUMMIES PARA UNIDADES FEDERATIVAS               #
#################################################################################

saeb_rend_dummies_uf <- dummy_columns(.data = saeb_rend,
                                      select_columns = "uf",
                                      remove_selected_columns = T,
                                      remove_most_frequent_dummy = T)

##################################################################################
#             ESTIMA«√O DO MODELO DE REGRESS√O M⁄LTIPLA COM DUMMIES E            #
#                       DIAGN”STICO DE HETEROCEDASTICIDADE                       #
##################################################################################
#Modelo considerando as UF's 
modelosaeb_dummies_uf <- lm(formula = saeb ~ . -municipio -codigo -escola -rede,
                            data = saeb_rend_dummies_uf)

summary(modelosaeb_dummies_uf)

#Teste de Breusch-Pagan para diagn√≥stico de heterocedasticidade
ols_test_breusch_pagan(modelosaeb_dummies_uf)

#Plotando saeb em fun√ß√£o de rendimento, com destaque para UFs e linear fits
ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb, color = uf, shape = uf)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = F) +
    xlab("rendimento") +
    ylab("saeb") +
    scale_colour_viridis_d() +
    theme_classic()
)


##################################################################################
#                    REGRESS√O N√O LINEAR M⁄LTIPLA COM DUMMIES                   #
#                    EXEMPLO 08 - CARREGAMENTO DA BASE DE DADOS                  #
##################################################################################

load(file = "planosaude.RData")

##################################################################################
#              OBSERVANDO OS DADOS CARREGADOS DA BASE planosaude                 #
##################################################################################
glimpse(planosaude)

#EstatÌsticas univariadas
summary(planosaude)

#Categorias da vari·vel 'plano'
levels(factor(planosaude$plano))

#Tabela de frequÍncias absolutas da vari·vel 'plano'
table(planosaude$plano)

##################################################################################
#                               ESTUDO DAS CORRELA«’ES                           #
##################################################################################

chart.Correlation((planosaude[2:5]), histogram = TRUE)

#################################################################################
#                            PROCEDIMENTO N-1 DUMMIES                           #
#################################################################################

planosaude_dummies <- dummy_columns(.data = planosaude,
                                    select_columns = "plano",
                                    remove_selected_columns = T,
                                    remove_most_frequent_dummy = T)

#Visualizando a base de dados dummizada
planosaude_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

##################################################################################
#                       ESTIMA«√O DA REGRESS√O LINEAR M⁄LTIPLA                   #
##################################################################################
#Modelagem com todas as vari√°veis
modelo_planosaude <- lm(despmed ~ . - id, planosaude_dummies)

#Par√¢metros do modelo_planosaude
summary(modelo_planosaude)

##################################################################################
#                               PROCEDIMENTO STEPWISE                            #
##################################################################################

step_planosaude <- step(modelo_planosaude, k = 3.841459)

summary(step_planosaude)

##################################################################################
#            TESTE DE VERIFICA«√O DA ADER NCIA DOS RESÕDUOS ¿ NORMALIDADE        #
##################################################################################

#Teste de Shapiro-Francia
sf.test(step_planosaude$residuals) #funÁ„o sf.test do pacote nortest

#Plotando os resÌduos do modelo step_planosaude 
planosaude %>%
    mutate(residuos = step_planosaude$residuals) %>%
    ggplot(aes(x = residuos)) +
    geom_histogram(color = "white", 
                   fill = "#55C667FF", 
                   bins = 15,
                   alpha = 0.6) +
    labs(x = "ResÌduos",
         y = "FrequÍncias") + 
    theme_bw()

#Acrescentando uma curva normal teÛrica para comparaÁ„o entre as distribuiÁıes
planosaude %>%
  mutate(residuos = step_planosaude$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_planosaude$residuals),
                            sd = sd(step_planosaude$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "ResÌduos",
       y = "FrequÍncia") +
  theme_bw()

#Kernel density estimation (KDE) - forma n„o-paramÍtrica para estimar a
#funÁ„o densidade de probabilidade de uma vari·vel aleatÛria
planosaude_dummies %>%
  ggplot() +
  geom_density(aes(x = step_planosaude$residuals), fill = "#55C667FF") +
  labs(x = "ResÌduos do Modelo Stepwise",
       y = "Densidade") +
  theme_bw()

##################################################################################
#                        DIAGN”STICO DE HETEROCEDASTICIDADE                      #
##################################################################################

#Teste de Breusch-Pagan para diagn√≥stico de heterocedasticidade
ols_test_breusch_pagan(step_planosaude)
#fun√ß√£o ols_test_breusch_pagan do pacote olsrr
#Presen√ßa de heterocedasticidade -> omiss√£o de vari√°vel(is) explicativa(s) relevante(s)

#H0 do teste: aus√™ncia de heterocedasticidade.
#H1 do teste: heterocedasticidade, ou seja, correla√ß√£o entre resÌduos e uma ou mais
#vari√°veis explicativas, o que indica omiss√£o de vari√°vel relevante!

#Adicionando fitted values e resÌduos do modelo 'step_planosaude'
#no dataset 'planosaude_dummies'
planosaude_dummies$fitted_step <- step_planosaude$fitted.values
planosaude_dummies$residuos_step <- step_planosaude$residuals

#Gr·fico que relaciona resÌduos e fitted values do modelo 'step_planosaude'
planosaude_dummies %>%
  ggplot() +
  geom_point(aes(x = fitted_step, y = residuos_step),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise",
       y = "ResÌduos do Modelo Stepwise") +
  theme_bw()

##################################################################################
#                              TRANSFORMAÁ√O DE BOX-COX                          #
##################################################################################
#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(planosaude$despmed)
lambda_BC

#Inserindo o lambda de Box-Cox na nova base de dados para a estima√ß√£o de um
#novo modelo
planosaude_dummies$bcdespmed <- (((planosaude$despmed ^ lambda_BC$lambda) - 1) / 
                                      lambda_BC$lambda)

#Visualizando a nova vari√°vel na base de dados
planosaude_dummies %>%
  select(id, despmed, bcdespmed, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#Estimando um novo modelo m˙ltiplo com dummies
modelo_bc_planosaude <- lm(formula = bcdespmed ~ . -id -despmed -fitted_step
                           -residuos_step, 
                           data = planosaude_dummies)

#Par‚metros do modelo
summary(modelo_bc_planosaude)

#Aplicando o procedimento Stepwise
step_bc_planosaude <- step(modelo_bc_planosaude, k = 3.841459)

summary(step_bc_planosaude)

#Verificando a normalidade dos resÌduos do modelo step_bc_planosaude
#Teste de Shapiro-Francia
sf.test(step_bc_planosaude$residuals) #fun√ß√£o sf.test do pacote nortest

#Plotando os novos resÌduos do modelo step_bc_planosaude com curva normal te√≥rica
planosaude_dummies %>%
  mutate(residuos = step_bc_planosaude$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_bc_planosaude$residuals),
                            sd = sd(step_bc_planosaude$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "ResÌduos",
       y = "FrequÍncia") +
  theme_bw()

#Kernel density estimation (KDE)
planosaude_dummies %>%
  ggplot() +
  geom_density(aes(x = step_bc_planosaude$residuals), fill = "#440154FF") +
  labs(x = "ResÌduos do Modelo Stepwise com TransformaÁ„o de Box-Cox",
       y = "Densidade") +
  theme_bw()

#DiagnÛstico de Heterocedasticidade para o Modelo Stepwise com Box-Cox
ols_test_breusch_pagan(step_bc_planosaude)

#Adicionando fitted values e resÌduos do modelo 'step_bc_planosaude'
#no dataset 'planosaude_dummies'
planosaude_dummies$fitted_step_novo <- step_bc_planosaude$fitted.values
planosaude_dummies$residuos_step_novo <- step_bc_planosaude$residuals

#Gr√°fico que relaciona resÌduos e fitted values do modelo 'step_bc_planosaude'
planosaude_dummies %>%
  ggplot() +
  geom_point(aes(x = fitted_step_novo, y = residuos_step_novo),
             color = "#440154FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise com Transforma√ß√£o de Box-Cox",
       y = "ResÌduos do Modelo Stepwise com Transforma√ß√£o de Box-Cox") +
  theme_bw()

#################################### FIM ########################################
