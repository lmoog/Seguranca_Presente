# Avaliação de Impacto – Segurança Presente

## Objetivo
Avaliar causalmente o efeito do programa **Segurança Presente** sobre a dinâmica da criminalidade no Rio de Janeiro.

O projeto estima se a implementação das bases do programa gerou mudanças estatística e economicamente relevantes nos indicadores de roubos e furtos.

---

## Pergunta de Negócio / Política Pública
A presença do programa reduziu o crime nas áreas tratadas em comparação com localidades semelhantes que ainda não haviam recebido a intervenção?

---

## Dados
- Painel mensal por delegacia (CISP × mês)  
- Indicadores: roubo de rua, roubo de veículos, total de roubos e total de furtos  
- Controles demográficos e territoriais (população e área)  
- Data de tratamento definida pela primeira instalação do programa em cada CISP  

---

## Metodologia
- Inferência causal em dados em painel  
- Difference-in-Differences com adoção escalonada (Callaway e Santanna)
- Estudo de evento para dinâmica temporal dos efeitos  
- Estimação com controles e grupo de comparação "never treated"

---

## Ferramentas Utilizadas
- R  

---

## Principais Resultados
Os resultados indicam **reduções de curto prazo em crimes de roubo** nas áreas tratadas, com efeitos menos claros para furtos.
Contudo, os efeitos não são duradouros. Também parece haver heterogeneidade de efeitos a depender do coorte analisado.

---

## Outputs
O repositório inclui:
- Código em R
- Working paper
---
**Leonardo Moog**  
Economista | Inferência Causal | Avaliação de Políticas Públicas
