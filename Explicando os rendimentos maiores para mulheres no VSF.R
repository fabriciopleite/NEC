install.packages("PNADcIBGE")
install.packages("Hmisc")
library("PNADcIBGE")
library("Hmisc")
PNADC_012025<-get_pnadc(year=2025, quarter=1, labels=FALSE, deflator=FALSE, design=FALSE)
PNADC_012025sel<-PNADC_012025[c("Ano", "Trimestre", "UF", "Capital", "RM_RIDE", "UPA", "Estrato", "V1008", "V1014", "V1022", "V1023", "V1028", "V2001", "V2003", "V2005", "V2007", "V2009", "V2010", "V4010", "V4013", "V4019", "V4040", "V4073", "V4074A", "V4077", "VD2002", "VD2003", "VD2004", "VD3004", "VD3005", "VD3006", "VD4001", "VD4002", "VD4003", "VD4004", "VD4004A", "VD4005", "VD4007", "VD4008", "VD4009", "VD4010", "VD4011", "VD4012", "VD4013", "VD4014", "VD4015", "VD4016", "VD4017", "VD4018", "VD4019", "VD4020", "VD4023", "VD4030", "VD4031", "VD4032", "VD4033", "VD4034", "VD4035", "VD4036", "VD4037")]
X<-as.data.frame(PNADC_012025sel)
###########################################
#Vale do São Francisco
X_VSF<-subset(X,subset=(X$Estrato>=2955000 & X$Estrato<2956000))
nrow(X_VSF)

#Definindo dummies para Sexo (V2007)
X_VSF$D_Homens<-as.numeric(X_VSF$V2007==1)
X_VSF$D_Mulheres<-as.numeric(X_VSF$V2007==2)

#Taxa de participação
#Filtrando para População em Idade de Trabalhar
PIT_VSF<-subset(X_VSF, X_VSF$V2009>=14)
PFT_VSF<-subset(X_VSF, X_VSF$VD4001==1)
#Taxa de desocupação
DESOCUP_VSF<-subset(PFT_VSF, PFT_VSF$VD4002==2)
td_VSF<-sum(DESOCUP_VSF$V1028)/sum(PFT_VSF$V1028)
print(td_VSF)
#Taxa de informalidade
OCUP_VSF<-subset(PFT_VSF, PFT_VSF$VD4002==1)
#V4019 somente 2015T4 em diante
INFORM_VSF<-subset(OCUP_VSF, subset=(OCUP_VSF$VD4009=="02" | OCUP_VSF$VD4009=="04" | OCUP_VSF$VD4009=="10" | OCUP_VSF$VD4009=="08" & OCUP_VSF$V4019==2 | OCUP_VSF$VD4009=="09" & OCUP_VSF$V4019==2))
ti_VSF<-sum(INFORM_VSF$V1028)/sum(OCUP_VSF$V1028)
print(ti_VSF)
#Taxa de empregados no setor público com relação à mão de obra formalmente ocupada
FORM_VSF<-subset(OCUP_VSF, subset=(OCUP_VSF$VD4009=="01" | OCUP_VSF$VD4009=="03" | OCUP_VSF$VD4009=="05" | OCUP_VSF$VD4009=="06"| OCUP_VSF$VD4009=="07"| OCUP_VSF$VD4009=="08" & OCUP_VSF$V4019==1 | OCUP_VSF$VD4009=="09" & OCUP_VSF$V4019==1))
ESP_VSF<-subset(OCUP_VSF, subset=(OCUP_VSF$VD4009=="05" | OCUP_VSF$VD4009=="06"| OCUP_VSF$VD4009=="07"))
tsp_VSF<-sum(ESP_VSF$V1028)/sum(FORM_VSF$V1028)
print(tsp_VSF)
#VD4010
#01	Agricultura, pecuária, produção florestal, pesca e aquicultura 
OCUP_VSF$D_Agropec<-as.numeric(OCUP_VSF$VD4010=="01")
#02	Indústria geral
OCUP_VSF$D_Ind<-as.numeric(OCUP_VSF$VD4010=="02")
#03	Construção
OCUP_VSF$D_Const<-as.numeric(OCUP_VSF$VD4010=="03")
#04	Comércio, reparação de veículos automotores e motocicletas
OCUP_VSF$D_Com<-as.numeric(OCUP_VSF$VD4010=="04")
#05	Transporte, armazenagem e correio
OCUP_VSF$D_Transp<-as.numeric(OCUP_VSF$VD4010=="05")
#06	Alojamento e alimentação
OCUP_VSF$D_Aloj_ali<-as.numeric(OCUP_VSF$VD4010=="06")
#07	Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas
OCUP_VSF$D_ICFIPA<-as.numeric(OCUP_VSF$VD4010=="07")
#08	Administração pública, defesa e seguridade social
OCUP_VSF$D_Adm_pub<-as.numeric(OCUP_VSF$VD4010=="08")
#09	Educação, saúde humana e serviços sociais
OCUP_VSF$D_Ed_sa<-as.numeric(OCUP_VSF$VD4010=="09")
#10	Outros Serviços
OCUP_VSF$D_Outros_serv<-as.numeric(OCUP_VSF$VD4010=="10")
#11	Serviços domésticos
OCUP_VSF$D_Serv_dom<-as.numeric(OCUP_VSF$VD4010=="11")

#Tomando somente com rendimentos declarados
OCUP_VSFRD<-subset(OCUP_VSF, !is.na(OCUP_VSF$VD4020))
sum(OCUP_VSFRD$VD4020*OCUP_VSFRD$V1028)/sum(OCUP_VSFRD$V1028)
INFORM_VSFRD<-subset(INFORM_VSF, !is.na(INFORM_VSF$VD4020))
sum(INFORM_VSFRD$VD4020*INFORM_VSFRD$V1028)/sum(INFORM_VSFRD$V1028)

#Tabela Grupamentos de Atividade
Pessoas<-c(sum(OCUP_VSFRD$D_Agropec*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Ind*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Const*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Com*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Transp*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Aloj_ali*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_ICFIPA*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Adm_pub*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Ed_sa*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Outros_serv*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Serv_dom*OCUP_VSFRD$V1028))

Percentual<-c(sum(OCUP_VSFRD$D_Agropec*OCUP_VSFRD$V1028)*100/sum(OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Ind*OCUP_VSFRD$V1028)*100/sum(OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Const*OCUP_VSFRD$V1028)*100/sum(OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Com*OCUP_VSFRD$V1028)*100/sum(OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Transp*OCUP_VSFRD$V1028)*100/sum(OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Aloj_ali*OCUP_VSFRD$V1028)*100/sum(OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_ICFIPA*OCUP_VSFRD$V1028)*100/sum(OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Adm_pub*OCUP_VSFRD$V1028)*100/sum(OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Ed_sa*OCUP_VSFRD$V1028)*100/sum(OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Outros_serv*OCUP_VSFRD$V1028)*100/sum(OCUP_VSFRD$V1028),sum(OCUP_VSFRD$D_Serv_dom*OCUP_VSFRD$V1028)*100/sum(OCUP_VSFRD$V1028))

REM<-c(sum(OCUP_VSFRD$VD4020*OCUP_VSFRD$D_Agropec*OCUP_VSFRD$V1028)/sum(OCUP_VSFRD$D_Agropec*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$VD4020*OCUP_VSFRD$D_Ind*OCUP_VSFRD$V1028)/sum(OCUP_VSFRD$D_Ind*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$VD4020*OCUP_VSFRD$D_Const*OCUP_VSFRD$V1028)/sum(OCUP_VSFRD$D_Const*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$VD4020*OCUP_VSFRD$D_Com*OCUP_VSFRD$V1028)/sum(OCUP_VSFRD$D_Com*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$VD4020*OCUP_VSFRD$D_Transp*OCUP_VSFRD$V1028)/sum(OCUP_VSFRD$D_Transp*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$VD4020*OCUP_VSFRD$D_Aloj_ali*OCUP_VSFRD$V1028)/sum(OCUP_VSFRD$D_Aloj_ali*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$VD4020*OCUP_VSFRD$D_ICFIPA*OCUP_VSFRD$V1028)/sum(OCUP_VSFRD$D_ICFIPA*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$VD4020*OCUP_VSFRD$D_Adm_pub*OCUP_VSFRD$V1028)/sum(OCUP_VSFRD$D_Adm_pub*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$VD4020*OCUP_VSFRD$D_Ed_sa*OCUP_VSFRD$V1028)/sum(OCUP_VSFRD$D_Ed_sa*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$VD4020*OCUP_VSFRD$D_Outros_serv*OCUP_VSFRD$V1028)/sum(OCUP_VSFRD$D_Outros_serv*OCUP_VSFRD$V1028),sum(OCUP_VSFRD$VD4020*OCUP_VSFRD$D_Serv_dom*OCUP_VSFRD$V1028)/sum(OCUP_VSFRD$D_Serv_dom*OCUP_VSFRD$V1028))

tabela<-data.frame(Pessoas,Percentual,REM)
colnames(tabela)<-c("Pessoas","Percentual","Renda Efetiva Média")
rownames(tabela)<-c("01 Agricultura, pecuária, produção florestal, pesca e aquicultura ", "02 Indústria geral", "03 Construção", "04 Comércio, reparação de veículos automotores e motocicletas", "05 Transporte, armazenagem e correio", "06 Alojamento e alimentação", "07 Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas", "08 Administração pública, defesa e seguridade social", "09 Educação, saúde humana e serviços sociais", "10 Outros Serviços", "11 Serviços domésticos")
print(round(tabela, digits=1))

# Uma primeira explicação poderia tomar somente os diferenciais de rendimento entre ocupados em geral e informais, dados os dois terços de informalidade no VSF e a predominância de homens entre os informais (também dois terços)
sum(OCUP_VSFRD$VD4020*OCUP_VSFRD$D_Homens*OCUP_VSFRD$V1028)/sum(OCUP_VSFRD$V1028*OCUP_VSFRD$D_Homens)
sum(OCUP_VSFRD$VD4020*OCUP_VSFRD$D_Mulheres*OCUP_VSFRD$V1028)/sum(OCUP_VSFRD$V1028*OCUP_VSFRD$D_Mulheres)
sum(INFORM_VSFRD$D_Homens*INFORM_VSFRD$V1028)/sum(INFORM_VSFRD$V1028)
# Rendimentos entre os informais continua a ser superior para homens, mas são as proporções que fazem a diferença
sum(INFORM_VSFRD$VD4020*INFORM_VSFRD$D_Homens*INFORM_VSFRD$V1028)/sum(INFORM_VSFRD$V1028*INFORM_VSFRD$D_Homens)
sum(INFORM_VSFRD$VD4020*INFORM_VSFRD$D_Mulheres*INFORM_VSFRD$V1028)/sum(INFORM_VSFRD$V1028*INFORM_VSFRD$D_Mulheres)
sum(OCUP_VSFRD$D_Homens*OCUP_VSFRD$V1028)/sum(OCUP_VSFRD$V1028)

# Indo mais fundo e olhando para as proporções na Agropecuária (um quarto de todas as ocupações)
sum(OCUP_VSF$D_Agropec*OCUP_VSF$D_Mulheres*OCUP_VSF$V1028)/sum(OCUP_VSF$V1028*OCUP_VSF$D_Agropec)
# E para Educação, saúde humana e serviços sociais (um oitavo das ocupações)
sum(OCUP_VSF$D_Ed_sa*OCUP_VSF$D_Mulheres*OCUP_VSF$V1028)/sum(OCUP_VSF$V1028*OCUP_VSF$D_Ed_sa)

#Sim, é uma questão de proporções! Na Agropecuária, com rendimentos médios baixos (R$1121), temos mais de 85% de homens, enquanto na Educação, saúde e serviços sociais, com rendimentos quatro vezes maiores (R$4335), temos 83% de mulheres.


