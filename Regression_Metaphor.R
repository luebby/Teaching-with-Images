# Article about metaphor for linear regression:
# Dennis Tay (2022) Metaphor Types as Strategies for Teaching Regression to Novice Learners, 
# Journal of Statistics and Data Science Education, 30:1, 3-14, DOI: 10.1080/26939169.2021.2024777

# Needed packages
library(jpeg)
library(dplyr)



# Read jpeg picture
# Source: https://github.com/allisonhorst/stats-illustrations/raw/master/other-stats-artwork/dragons.png 
# (saved as jpg)
# Artwork by @allison_horst
# https://github.com/allisonhorst/stats-illustrations
picture <- readJPEG("dragons.jpg")

# Dimensions of the picture
dim_picture <- dim(picture)
dim_picture

# Convert into data frame
rgb_picture <- data.frame(
  x = rep(1:dim_picture[2], each = dim_picture[1]), # x-Pixel
  y = rep(dim_picture[1]:1, dim_picture[2]), # y-Pixel
  R = as.vector(picture[,,1]), # Red
  G = as.vector(picture[,,2]), # Yellow
  B = as.vector(picture[,,3]) # Blue
)

# n, i.e. number of pixels
n <- dim_picture[1] * dim_picture[2]

# Add some noise but trunctate to [0,1]
set.seed(1896)
# Standard Deviation of Gaussian noise
sdn <- 0.4

rgb_pictureNoise <- rgb_picture %>%
  mutate(R = R + rnorm(n = n, mean = 0, sd = sdn),
         G = G + rnorm(n = n, mean = 0, sd = sdn),
         B = B + rnorm(n = n, mean = 0, sd = sdn)) %>%
  mutate(R = case_when(R < 0 ~ 0,
                       R > 1 ~ 1,
                       TRUE ~ R),
         G = case_when(G < 0 ~ 0,
                       G > 1 ~ 1,
                       TRUE ~ G),
         B = case_when(B < 0 ~ 0,
                       B > 1 ~ 1,
                       TRUE ~ B)
         )

# Convert back to picture
picture_Noise <- array(NA, dim_picture)
for(i in 1:3) picture_Noise[,,i] <- matrix(rgb_pictureNoise[,(i+2)], 
                                           nrow = dim_picture[1]) 
# Save picture as jpg
writeJPEG(picture_Noise, "dragons-noise.jpg")


# Add fragments, i.e. only part of observations are seen, others are set to white
# Fraction to be eliminated, randomly!
frag <- 0.7
rgb_pictureFrag <- rgb_pictureNoise %>%
  mutate(white = rbinom(n, 1, frag)) %>%
  mutate(R = ifelse(white == 1, 1, R),
         G = ifelse(white == 1, 1, G),
         B = ifelse(white == 1, 1, B))
  
# Convert to picture
picture_Frag <- array(NA, dim_picture)
for(i in 1:3) picture_Frag[,,i] <- matrix(rgb_pictureFrag[,(i+2)], 
                                           nrow = dim_picture[1]) 
# Save picture
writeJPEG(picture_Frag, "dragons-Frag.jpg")

# To estimate model, use only pixels which were not eliminated
dataFrag <- rgb_pictureFrag %>%
  filter(white == 0)

# Regression of R,G,B on randomly fragmented, noisy data
lm_R <- lm(R ~ G + B, data = dataFrag)
lm_G <- lm(G ~ R + B, data = dataFrag)
lm_B <- lm(B ~ R + G, data = dataFrag)

# Prediction by estimated model on noisy data, trunctated to [0,1]
rgb_pictureReg <- rgb_picture %>%
  mutate(R = predict(lm_R, newdata = rgb_pictureNoise),
         G = predict(lm_G, newdata = rgb_pictureNoise),
         B = predict(lm_B, newdata = rgb_pictureNoise))  %>%
  mutate(R = case_when(R < 0 ~ 0,
                       R > 1 ~ 1,
                       TRUE ~ R),
         G = case_when(G < 0 ~ 0,
                       G > 1 ~ 1,
                       TRUE ~ G),
         B = case_when(B < 0 ~ 0,
                       B > 1 ~ 1,
                       TRUE ~ B)
  )

# Convert to picture
picture_Reg <- array(NA, dim_picture)
for(i in 1:3) picture_Reg[,,i] <- matrix(rgb_pictureReg[,(i+2)], 
                                           nrow = dim_picture[1]) 
# Save picture
writeJPEG(picture_Reg, "dragons-Reg.jpg")


# Add fragments, i.e. select pixels based on RGB values
rgb_pictureFrag2 <- rgb_pictureNoise %>%
  mutate(white = (R + B > 0.2)) %>%
  mutate(R = ifelse(white == 1, 1, R),
         G = ifelse(white == 1, 1, G),
         B = ifelse(white == 1, 1, B))
# Convert to picture
picture_Frag2 <- array(NA, dim_picture)
for(i in 1:3) picture_Frag2[,,i] <- matrix(rgb_pictureFrag2[,(i+2)], 
                                          nrow = dim_picture[1]) 
# Save picture
writeJPEG(picture_Frag2, "dragons-Frag2.jpg")

# Filter on availabe data
dataFrag2 <- rgb_pictureFrag2 %>%
  filter(white == 0)

# Regression of R,G,B on systematically fragmented, noisy data
lm_R2 <- lm(R ~ G + B, data = dataFrag2)
lm_G2 <- lm(G ~ R + B, data = dataFrag2)
lm_B2 <- lm(B ~ R + G, data = dataFrag2)

# Prediction by estimated model on noisy data, trunctated to [0,1]
rgb_pictureReg2 <- rgb_picture %>%
  mutate(R = predict(lm_R2, newdata = rgb_pictureNoise),
         G = predict(lm_G2, newdata = rgb_pictureNoise),
         B = predict(lm_B2, newdata = rgb_pictureNoise)) %>%
  mutate(R = case_when(R < 0 ~ 0,
                       R > 1 ~ 1,
                       TRUE ~ R),
         G = case_when(G < 0 ~ 0,
                       G > 1 ~ 1,
                       TRUE ~ G),
         B = case_when(B < 0 ~ 0,
                       B > 1 ~ 1,
                       TRUE ~ B)
         )

# Convert to picture
picture_Reg2 <- array(NA, dim_picture)
for(i in 1:3) picture_Reg2[,,i] <- matrix(rgb_pictureReg2[,(i+2)], 
                                         nrow = dim_picture[1]) 

# Save picture
writeJPEG(picture_Reg2, "dragons-Reg2.jpg")
