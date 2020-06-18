library(plotly)

p <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = c("A1", "A2", "B1", "B2", "C1", "C2"),
    color = c("blue", "blue", "blue", "blue", "blue", "blue"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = c(0,1,0,2,3,3),
    target = c(2,3,3,4,4,5),
    value =  c(8,4,2,8,4,2)
  )
) %>% 
  layout(
    title = "Basic Sankey Diagram",
    font = list(
      size = 10
    )
  )

p

### Plotly 적용
### 자료: 통계청,「인구총조사」
### 2018년 성별 및 연령별 1인가구 현황

library(plotly)
library(dplyr)

count <- plot_ly(type = "sankey",
                 orientation = "h",
                 node = list(
                   label = c("남자", "여자", "미성년자", "20대",
                             "30대", "40대", "50대", "60대", "70대", "80세이상"), 
                   color = c("blue", "red", "green", "green",
                             "green", "green", "green", "green", "green", "green"),
                   pad = 5,
                   thickness = 30,
                   line = list(color = "black", width = 0.5)
                 ),
                 link = list(
                   line = list(color="black", width = 0.2),
                   source = c(0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1),
                   target = c(2,3,4,5,6,7,8,9,2,3,4,5,6,7,8,9),
                   value =  c(27623, 542781, 637042, 545819, 543074, 366564, 172025, 71392,
                              30531, 477163, 355695, 318402, 431370, 503340, 498872, 326901)
                 )
) %>% 
  layout(
    title = "성별 및 연령별 1인가구 현황",
    font = list(size = 10, color = "Black")
  )

count



## networkD3 적용
## 통계청,「인구총조사」
## 2018년 성별 및 연령별 1인가구 현황

library(networkD3)
library(dplyr)

links <- data.frame(
  source=c("남자", "남자", "남자", "남자", "남자", "남자", "남자", "남자", 
           "여자", "여자", "여자", "여자", "여자", "여자", "여자", "여자"), 
  target=c("미성년자", x"20대", "30대", "40대", "50대", "60대", "70대", "80세이상", 
           "미성년자", "20대", "30대", "40대", "50대", "60대", "70대", "80세이상"), 
  value=c(27623, 542781, 637042, 545819, 543074, 366564, 172025, 71392,
          30531, 477163, 355695, 318402, 431370, 503340, 498872, 326901)
)

nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

links$sex <- match(links$source, nodes$name)-1 
links$age <- match(links$target, nodes$name)-1

count <- sankeyNetwork(Links = links, Nodes = nodes,
                       Source = "sex", Target = "age",
                       Value = "value", NodeID = "name", 
                       sinksRight=FALSE)
count

x``
