# 1 ####
library("tidyverse")

# Baseball Referenceのデータの読み込み
bat_files <- dir("input/", full.names = T) %>% str_subset("Batting201[5-8].csv")
bat_tmp <- list()
for(i in 1:length(bat_files)){
  bat_tmp <- c(bat_tmp, list(
    read_csv(bat_files[i], locale = locale(encoding = "cp932"))
  ))
}
batting <- bind_rows(bat_tmp)
rm(i, bat_tmp, bat_files)
glimpse(batting)

# ベストナインのデータの読み込み（英語名は頑張って手動でくっつけた）
# 投手は対象外
bestnine <- read_csv("input/bestnine.csv", locale = locale(encoding = "cp932")) %>% 
  filter(position != "投手")
glimpse(bestnine)


# 2 ####
# 15〜17年の分布を見てみる
ggplot(bestnine %>% filter(year <= 2017),
       aes(vote, group = flag, fill = factor(flag))) +
  geom_density(alpha = .3) +
  facet_wrap(~league)


# 3 ####
# 分析用データのベースを作成
bestnine_uni <- bestnine %>% 
  mutate(flag = (vote>=100) * 1) %>%  # 100票以上獲得者にフラグ
  select(name_en, league, year, flag) %>% 
  group_by(name_en, league, year) %>% 
  summarise(flag = max(flag)) %>% # 選手ごとにユニークなデータにする
  ungroup

DV <- function(x) scale(x) * 10 + 50 # 偏差値変換
dat <- batting %>% 
  left_join(bestnine_uni, by = c("Name" = "name_en", "Year" = "year")) %>%
  arrange(Team, league) %>% 
  fill(league) %>% 
  mutate(flag = if_else(is.na(flag), 0, flag)) %>% 
  filter(PA >= 443 * 1/2) %>% 
  group_by(Year, league) %>% 
  mutate_at(vars(G:TB), DV) %>% 
  as.data.frame
# 出場機会が多かった選手の中での偏差値を出したいので
# 規定打席1/2以上の選手にしぼった

# 分析用データを作成
train <- dat %>% filter(Year != 2018)
test <- dat %>% filter(Year == 2018)

# 何人いるか確認
table(train$flag)
table(test$flag)

# 4 ####
vari <- c("BA", "HR", "RBI", "SB")
glm_res <- glm(data = train %>% select_(.dots = c("flag", vari)),
               formula = flag~., family = "binomial")
summary(glm_res)
# 相関係数を見てみる
train %>% select_(.dots = vari) %>% cor %>% round(2)

# ステップワイズ
summary(step(glm_res))

# 5 ####
rownames(train) <- with(train, paste(Year, Name, sep="_"))
pca <- prcomp(train %>% select_(.dots = vari), center = T, scale. = T)
summary(pca)
round(pca$rotation, 2)
# 一部を可視化してみる
tmp_pca <- pca
tmp_pca$x <- pca$x[sample(1:nrow(pca$x), 80),]
(p <- factoextra::fviz_pca_biplot(X = tmp_pca, repel = T, 
                                  col.var = "red", col.ind = "#696969"))
ggsave("fviz_pca_biplot.png", p, width = 12, height = 10)

# 6 ####
train_pca <- train %>% 
  select(Year, league, Team, Name, flag, PA) %>% 
  cbind(pca$x) %>% 
  mutate(PC2 = -PC2) # わかりやすくするため走力は大きいほど良い値に変換
glm_res <- glm(data = train_pca, formula = flag~PC1+PC2, family = "binomial")
summary(glm_res)
# 結果の可視化
baseline <- sum(glm_res$y==1)/length(glm_res$y) # ベースライン
tmp <- data.frame(x = glm_res$linear.predictors, 
                  y = glm_res$y,
                  pred = predict(glm_res, type = "response")) %>%  
  mutate(pred_y = (pred >= baseline) * 1)
ggplot(tmp, aes(x, y)) +
  geom_point(aes(col = factor(pred_y))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_hline(yintercept = baseline) +
  geom_vline(xintercept = log(baseline/(1-baseline)))

# 精度確認
(accuracy <- round(prop.table(table(tmp$y, tmp$pred_y), margin = 1), 2)) # 横比
sum(diag(accuracy))/sum(accuracy)

# 7 ####
test_pca <- test %>% 
  select(Year, league, Team, Name) %>% 
  cbind(predict(pca, test %>% select_(.dots = vari))) %>% 
  mutate(PC2 = -PC2) %>% 
  left_join(bestnine_uni, by = c("Name" = "name_en", "Year" = "year", "league")) %>% 
  mutate(flag = if_else(is.na(flag), 0, flag))

test_pred <- predict(glm_res, test_pca, type = "response") >= baseline
(test_accuracy <- round(prop.table(table(test_pca$flag, test_pred), margin = 1), 2))
sum(diag(test_accuracy))/sum(test_accuracy)

# 8 ####
res <- test_pca %>% 
  select(-PC3, -PC4) %>% 
  mutate(pred = predict(glm_res, test_pca, type = "response"), 
         pred_flag = (pred >= baseline) * 1)
