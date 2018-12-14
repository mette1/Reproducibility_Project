# install.packages("readxl")
library("readxl")
library(tidyverse)

data = read_excel("fig2-data1.xlsx")

# meta = filter(data, Taxon == 'Equus hemionus' | Taxon == 'francisci type' |
#                 Taxon == 'Equus caballus' | Taxon == 'Equus lambei' | Taxon == 'Equus cf. scotti')


meta = data[,c(1,8:18)]
meta = drop_na(meta)


meta$`GL: Greatest Length` = as.double(meta$`GL: Greatest Length`)
meta$`Pb: Proximal Breadth` = as.double(meta$`Pb: Proximal Breadth`)
meta$`Dp: Proximal depth` = as.double(meta$`Dp: Proximal depth`)
meta$`SD: Midshaft breadth` = as.double(meta$`SD: Midshaft breadth`)
meta$`Midshaft depth (#4)` = as.double(meta$`Midshaft depth (#4)`)
meta$`Distal articular breadth at midline (#11)` = as.double(meta$`Distal articular breadth at midline (#11)`)
meta$`Distal metaphyseal breadth (#10)` = as.double(meta$`Distal metaphyseal breadth (#10)`)
meta$`Maximum depth of distal "keel" (#12)` = as.double(meta$`Maximum depth of distal "keel" (#12)`)
meta$`Minimum depth of medial condyle` = as.double(meta$`Minimum depth of medial condyle`)
meta$`Maximum depth of medial condyle (#14)` = as.double(meta$`Maximum depth of medial condyle (#14)`)

Taxon = meta$Taxon
Iden =meta$`Identification based on`
meta = meta[2:11]

meta4 = meta[,1:4]
# metaR = meta[,5:10]

# meta = na.omit(meta)
meta4 = as.matrix(meta4)

 f1 <- function(vec) {
   m <- mean(vec, na.rm = TRUE)
   vec[is.na(vec)] <- m
   return(vec)
 }

meta4 = apply(meta4,2,f1)


# metaPC = prcomp(~'GL: Greatest Length'+'Pb: Proximal Breadt'+'Dp: Proximal depth'+'SD: Midshaft breadth',meta4, na.action = na.omit)
# metaPC = prcomp(na.omit(meta4))
metaPC = prcomp(meta4)
meta3 =  as.tibble(metaPC$x)
meta3 = mutate(meta3, Taxon=Taxon, `Identification Method`=Iden)
# Create a data frame
# scores <- as.tibble(chrXPC$x)
# scores = scores %>% mutate(Gender = sampleData$Gender)

g = ggplot(meta3, aes(x=PC1, y=PC2, color=Taxon, shape=`Identification Method`)) +
  geom_point() + theme_bw() + xlab("Principle Component 1") + ylab("Principle Component 2") 
# +
#   theme(legend.position="top")

# ggsave("graph.pdf")

# symbol shapes = Identification based on
# Color = Taxon
# PC1