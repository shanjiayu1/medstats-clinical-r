


remotes::install_github("shanjiayu1/medstats")

library(medstats)
#ceshi

ft <- format_flextable(
  head(mtcars[, 1:5])
)
ft
# Format a gtsummary object

tbl <- tbl_summary(
  trial,
  include = c(age, grade, trt),
  by = trt
)

ft <- format_flextable(tbl)
ft

export_word(
  data_list = list(
    head(mtcars),
    head(iris)
  ),
  table_titles = c(
    "Table 1. mtcars Dataset",
    "Table 2. iris Dataset"
  ),
  output_file = "my_tables.docx"
)

my_clinical_data <- ChickWeight |>
  mutate(
    is_reach_150g = ifelse(weight >= 150, 1, 0)
  )
surv_data <- long_to_surv_data(
  data = my_clinical_data,
  id_var = "Chick",
  event_flag_var = "is_reach_150g",
  time_var = "Time",
  baseline_vars = c("Diet")
)

options(timeout =1200)
install.packages("TH.data")

make_table1(
  data = trial,
  vars = c("age", "marker", "stage", "grade"),
  specific_vars = "marker",  # Variable with a non-normal distribution
  group_var = "trt"
)

run_glm_auto(
  data = mtcars,
  vars = c("hp", "wt"),
  outcome_var = "mpg",
  family = "gaussian"
)

run_glm_auto(
  data = trial,
  vars = c("age", "stage"),
  outcome_var = "response",
  family = "binomial"
)

run_cox_auto(
  data = survival::lung,
  vars = c("age", "sex"),
  time_var = "time",
  event_var = "status"
)


plot_forest(
  data = import("森林图260424.xlsx", sheet = 1, trim_ws = FALSE),
  ci_column = 2, 
  x_ticks = c(0, 0.5,1,1.5,2),
  width = 6.5, 
  output_name = "TCM_Forestplot1.png"
)
f_data <- run_glm_auto(
  data = trial,
  vars = c("age", "stage"),
  outcome_var = "response",
  family = "binomial"
)

plot_forest(
  data = f_data[1:3],
  ci_column = 2, 
  x_ticks = c(0, 0.5,1,1.5,2),
  width = 6.5, 
  output_name = "TCM_Forestplot2.png"
)

my_data <- ChickWeight |>
  filter(Time %in% c(0, 4, 10, 14, 21)) |>   # 挑选第 0,4,10,14,21 天
  mutate(
    time_str = paste0("第", Time, "天"),      # 制造 "第10天" 这种字符串
    Diet_Name = paste0("饮食配方", Diet)      # 把组别从 1,2,3,4 改为有意义的名称
  )

plot_meanse(
  data         = my_data,
  target_var   = "weight",         # 结局指标：体重
  time_var     = "time_str",       # 时间列名："第x天"
  group_var    = "Diet_Name",      # 分组变量："饮食配方x"
  xlab         = "生长天数 (Days)",# 自定义 X 轴标签
  ylab         = "平均体重 (g)",   # 自定义 Y 轴标签
  legend_title = "不同饮食分组",
)


chick_weight <- ChickWeight
chick_weight$Time <- factor(
  chick_weight$Time,
  levels = sort(unique(chick_weight$Time))
)

plot_stacked(
  data = chick_weight,
  target_var = "weight",
  time_var = "Time",
  breaks = c(-Inf, 100, 200, 300, Inf),
  labels = c("≤100 g", "101–200 g", "201–300 g", ">300 g"),
  colors = c("#B5D1E8", "#A3D9A5", "#F2C68F", "#EB938F"),
  legend_title = "Weight range",
  xlab="fol"
)


stacked_data <- datasets::ChickWeight
stacked_data$Time <- factor(
  stacked_data$Time,
  levels = sort(unique(stacked_data$Time))
)

stacked_result <- plot_stacked(
  data = stacked_data,
  target_var = "weight",
  time_var = "Time",
  group_var = "Diet",
  breaks = c(-Inf, 100, 200, 300, Inf),
  labels = c("≤100 g", "101–200 g", "201–300 g", ">300 g"),
  colors = c("#B5D1E8", "#A3D9A5", "#F2C68F", "#EB938F"),
  legend_title = "Weight range",
  label_size = 4.5
)


remotes::install_github("nx10/httpgd")
library(httpgd)

install.packages("rstatix")
lung <- survival::lung

lung_fixed <- survival::lung
lung_fixed$status <- ifelse(lung_fixed$status == 2, 1, 0) 

# ==========================================
# 4. 调用函数进行测试
# ==========================================
# 注意：肺癌数据集的 time 是“天”，所以我们改一下坐标范围和标签


test_results <- plot_km(
  data = lung_fixed,
  time_var = "time",             # 时间变量名称
  status_var = "status",         # 状态变量名称
  xlab = "随访时间 (天)",
  ylab = "累积死亡率 (%)",
  
  # 因为单位是“天”，所以需要调整以下三个参数，避免图缩作一团
  xlim = c(0, 1000),             # X轴范围设为 0 到 1000天
  break_time = 200,              # 每 200 天一个刻度
  pval_coord = c(200, 0.8),      # 把 P值 放在图的左上方 (X=200天处, Y=80%高度)
  
  show_risk_table = TRUE,        # 顺便开启底部风险表格测试
  save_filename = "Lung_Test_Cumulative_KM.png"
)


library(nlme)  
my_data_2groups <- Orthodont %>%
  as.data.frame() %>% 
  mutate(
    time_str = paste0(age, "岁") # 故意构造文本格式时间："8岁", "10岁"...
  )
results_2groups <- longdata_analysis(
  data          = my_data_2groups,
  id_col        = "Subject",      # 个体ID
  treatment_col = "Sex",          # 分组：男/女（仅2组）
  time_col      = "time_str",     # 时间：8岁/10岁...
  score_col     = "distance"      # 结局指标：距离
)

# 4. 打印结果
print(results_2groups)



#logsitic模型：马力(hp)对自动变速箱(am)的影响，调整体重(wt)，4个节点，OR图
res1 <- plot_rcs(
  data = mtcars,
  exposure = "hp",
  outcome = "am",
  covars = c("wt"),
  nk = 4,
  # ylim=c(0,5),
  model_type = "logistic",
  xlab = "马力(hp)",
  ylab = "自动变速箱概率"
)

res1$plot


#线性模型：体重(wt)对每加仑英里数(mpg)的影响，调整马力(hp)和排量(disp)，4个节点，预测值图
res2 <- plot_rcs(
  data = mtcars,
  exposure = "wt",
  outcome = "mpg",
  covars = c("hp", "disp"),
  model_type = "linear",
  # ylim=c(0,20),
  ylab = "Predicted MPG"
)

res2$plot

#cox模型：年龄(age)对生存时间(time)和状态(status)的影响
res3 <- plot_rcs(
  data = lung,
  exposure = "age",
  outcome = "Surv(time, status)",
  covars = c("sex", "ph.ecog"),
  model_type = "cox",
  ylab = "Hazard Ratio"
)

res3$plot
install.packages("Hmisc")

plot_rcs(
  data = mtcars,
  exposure = "wt",
  outcome = "mpg",
  model_type = "linear"
)




model <- glm(am ~ mpg + hp + wt, data = mtcars, family = binomial)
train_data <- mtcars
train_data$pred_prob <- predict(model, newdata = train_data, type = "response")

res_train <- plot_roc(
  data = train_data, 
  true_var = "am", 
  pred_var = "pred_prob", 
  title = "训练集 ROC 曲线 (mtcars)",
  line_color = "#2E86AB"   # 蓝色
)
res_train$plot

library(medstats)


my_test_data <- ChickWeight %>%
  filter(Time %in% c(0, 10, 20)) %>%
  mutate(
    Visit_Time = factor(paste0("第 ", Time, " 天"), levels = c("第 0 天", "第 10 天", "第 20 天")),
    Weight_Status = case_when(
      weight < 50  ~ "偏瘦 (Light)",
      weight >= 50 & weight < 150 ~ "正常 (Normal)",
      weight >= 150 ~ "超重 (Overweight)"
    ),
    Weight_Status = factor(Weight_Status, levels = c("偏瘦 (Light)", "正常 (Normal)", "超重 (Overweight)"))
  )

# ==========================================
# 4. 调用新版函数！
# ==========================================
sankey_plot <- plot_sankey(
  data           = my_test_data,
  id_var         = "Chick",           
  time_var       = "Visit_Time",      
  state_var      = "Weight_Status",   
  na_strategy    = "drop",             # <--- 改为 "drop" 即可无缝切换为完整版
  missing_label  = "Drop-out (失访)"  # 可以自定义叫什么名字          
)



mtcars




original_data <- datasets::airquality[1:5, ]

# 添加记录编号，作为判断重复的变量
original_data$record_id <- seq_len(nrow(original_data))

# 构造两份互补数据：
# 第一份 Ozone 缺失，第二份 Solar.R 缺失
data_with_duplicates <- rbind(
  transform(original_data, Ozone = NA),
  transform(original_data, Solar.R = NA)
)

# 查看重复数据
data_with_duplicates

# 检查并合并重复记录
data_clean <- merge_duplicate_records(
  data = data_with_duplicates,
  group_vars = "record_id"
)

clinical_data <- data.frame(
  patient_id = c("P001", "P001", "P002", "P002"),
  age = c(NA, 65, 52, 53),
  diagnosis = c("Hypertension", NA, NA, "Diabetes"),
  admission_date = as.Date(c(NA, NA, "2026-01-02", NA))
)

data_clean <- merge_duplicate_records(
  data = clinical_data,
  group_vars = "patient_id"
)

data_clean

data_clean <- merge_duplicate_records(
  data = original_data,
  group_vars = "record_id"
)



# 查看结果
data_clean

# 检查每个 record_id 是否只保留一条记录
anyDuplicated(data_clean$record_id)


sankey_data <- datasets::ChickWeight |>
  dplyr::filter(Time %in% c(0, 10, 20)) |>
  dplyr::mutate(
    visit = factor(
      paste0("Day ", Time),
      levels = c("Day 0", "Day 10", "Day 20")
    ),
    weight_status = dplyr::case_when(
      weight < 50 ~ "Light",
      weight < 150 ~ "Normal",
      TRUE ~ "Heavy"
    ),
    weight_status = factor(
      weight_status,
      levels = c("Light", "Normal", "Heavy")
    )
  )

sankey_plot <- plot_sankey(
  data = sankey_data,
  id_var = "Chick",
  time_var = "visit",
  state_var = "weight_status",
  na_strategy = "show",
  missing_label = "Drop-out",
  legend_label = "Status",
  xlab="Follow time",
  ylab="Count"
)

sankey_plot

ggsave(sankey_plot,
file="sankey_plot.png")



stacked_data <- datasets::ChickWeight
stacked_data$Time <- factor(
  stacked_data$Time,
  levels = sort(unique(stacked_data$Time))
)

stacked_result <- plot_stacked(
  data = stacked_data,
  target_var = "weight",
  time_var = "Time",
  # group_var = "Diet",
  breaks = c(-Inf, 100, 200, 300, Inf),
  labels = c("≤100 g", "101–200 g", "201–300 g", ">300 g"),
  colors = c("#B5D1E8", "#A3D9A5", "#F2C68F", "#EB938F"),
  legend_title = "Weight range",
  label_size = 4.5,
  xlab="Follow time",
  ylab="Percent(%)"
)

stacked_result

ggsave(stacked_result$plot,
file="stacked_result.png")




lung_data <- survival::lung
lung_data$status_event <- as.integer(lung_data$status == 2)
lung_data$sex <- factor(
  lung_data$sex,
  levels = c(1, 2),
  labels = c("Male", "Female")
)

plot_km(
  data = lung_data,
  group_var = "sex",
  time_var = "time",
  status_var = "status_event",
  legend_labs = c("Male", "Female"),
  legend_title = "Sex",
  xlab = "Follow-up time (days)",
  ylab = "Cumulative mortality (%)",
  xlim = c(0, 1000),
  break_time = 200,
  show_risk_table = TRUE,
  save_filename = "Lung_KM.png"
)


forest_data <- data.frame(
  Variable = c("Age", "Stage II", "Stage III"),
  `OR (95% CI)` = c(
    "1.02 (0.99, 1.05)",
    "1.45 (0.82, 2.56)",
    "2.10 (1.12, 3.94)"
  ),
  `P value` = c("0.180", "0.200", "0.021"),
  check.names = FALSE
)

plot_forest(
  data = forest_data,
  ci_column = "OR (95% CI)",
  x_ticks = c(0, 0.5, 1, 2, 4),
  output_name = "Forest_plot.png"
)



roc_model <- glm(
  am ~ mpg + hp + wt,
  data = mtcars,
  family = binomial
)

roc_data <- mtcars
roc_data$predicted_probability <- predict(
  roc_model,
  newdata = roc_data,
  type = "response"
)

roc_result <- plot_roc(
  data = roc_data,
  true_var = "am",
  pred_var = "predicted_probability",
  title = "Training ROC curve",
  line_color = "#2E86AB",
    xlab = "Follow-up time (days)",
  ylab = "Cumulative mortality (%)",
)

roc_result$plot

library(survival)
lung_rcs <- survival::lung
lung_rcs$status_event <- as.integer(lung_rcs$status == 2)

rcs_cox <- plot_rcs(
  data = lung_rcs,
  exposure = "age",
  outcome = "Surv(time, status_event)",
  covars = c("sex", "ph.ecog"),
  model_type = "cox",
  ylab = "Hazard ratio"
)

rcs_cox$plot

ggsave(rcs_cox$plot,
file="rcs_cox.png")


rcs_linear <- plot_rcs(
  data = mtcars,
  exposure = "wt",
  outcome = "mpg",
  covars = c("hp", "disp"),
  nk = 4,
  model_type = "linear",
  xlab = "Weight",
  ylab = "Predicted MPG"
)

rcs_linear$plot

ggsave(rcs_cox$plot,
file="rcs_cox.png")


roc_model <- glm(
  am ~ mpg + hp + wt,
  data = mtcars,
  family = binomial
)

roc_data <- mtcars
roc_data$predicted_probability <- predict(
  roc_model,
  newdata = roc_data,
  type = "response"
)

roc_result <- plot_roc(
  data = roc_data,
  true_var = "am",
  pred_var = "predicted_probability",
  title = "Training ROC curve",
  line_color = "#2E86AB"
)
ggsave(roc_result$plot,
file="roc_result.png")

roc_result$plot

library(ResourceSelection)
install.packages("ResourceSelection")

library(medstats)
roc_result <- plot_roc(
  data = roc_data,
  true_var = "am",
  pred_var = "predicted_probability",
  title = "Training ROC curve",
  line_color = "#2E86AB",
  xlab = "1 - Specificity",
  ylab = "Sensitivity"
)
roc_result


ft_data <- format_flextable(
  head(mtcars[, 1:5])
)

# Format a gtsummary object
tbl <- gtsummary::tbl_summary(
  data = gtsummary::trial,
  include = c(age, marker, grade),
  by = trt
)

format_flextable(tbl)


lung_data <- survival::lung
lung_data$status_event <- as.integer(lung_data$status == 2)
lung_data$sex <- factor(
  lung_data$sex,
  levels = c(1, 2),
  labels = c("Male", "Female")
)

cox_results <- run_cox_auto(
  data = lung_data,
  vars = c("age", "sex"),
  time_var = "time",
  event_var = "status_event"
)
format_flextable(cox_results)



linear_results <- run_glm_auto(
  data = mtcars,
  vars = c("hp", "wt"),
  outcome_var = "mpg",
  family = "gaussian"
)

linear_results


data("Orthodont", package = "nlme")

orthodont_data <- Orthodont |>
  as.data.frame() |>
  dplyr::mutate(
    time_str = paste0(age, " years")
  )

gee_results <- longdata_analysis(
  data = orthodont_data,
  id_col = "Subject",
  treatment_col = "Sex",
  time_col = "time_str",
  score_col = "distance"
)

print(gee_results)
format_flextable(gee_results)


growth_data <- datasets::ChickWeight |>
  dplyr::filter(Time %in% c(0, 4, 10, 14, 21)) |>
  dplyr::mutate(
    time_label = paste0("Day ", Time),
    diet_label = paste0("Diet ", Diet)
  )

meanse_result <- plot_meanse(
  data = growth_data,
  target_var = "weight",
  time_var = "time_label",
  group_var = "diet_label",
  xlab = "Growth time (days)",
  ylab = "Mean weight (g)",
  legend_title = "Diet"
)



library(medstats)
# 准备表格和图片
table1 <- head(mtcars)
p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
  ggplot2::geom_point()
 
# 详细方式：自定义标题
export_word(
  data_list = list(table1, p1),
  table_titles = c(
    "Table 1. mtcars dataset",
    "Figure 1. MPG and weight"
  ),
  output_file = "tables_and_plots.docx",
  figure_width = 6,
  figure_height = 5
)
 
# 简化方式：直接传入对象，标题自动生成
export_word(table1, p1, "tables_and_plots.docx")



growth_data <- datasets::ChickWeight |>
  dplyr::filter(Time %in% c(0, 4, 10, 14, 21)) |>
  dplyr::mutate(
    time_label = paste0("Day ", Time),
    diet_label = paste0("Diet ", Diet)
  )

meanse_result <- plot_meanse(
  data = growth_data,
  target_var = "weight",
  time_var = "time_label",
  group_var = "diet_label",
  xlab = "Growth time (days)",
  ylab = "Mean weight (g)",
  legend_title = "Diet"
)

meanse_result

ggsave(meanse_result$plot, file = "meanse_plot.png", width = 6, height = 5)





library(survival)

lung_rcs <- survival::lung
lung_rcs$status_event <- as.integer(lung_rcs$status == 2)

rcs_cox <- plot_rcs(
  data = lung_rcs,
  exposure = "age",
  outcome = "Surv(time, status_event)",
  covars = c("sex", "ph.ecog"),
  model_type = "cox",
  ylab = "Hazard ratio"
)



two_diet_result <- plot_meanse(
  data = dplyr::filter(growth_data, diet_label %in% c("Diet 1", "Diet 2")),
  target_var = "weight",
  time_var = "time_label",
  group_var = "diet_label",
  test_method = "wilcox",
  legend_title = "Diet",
  xlab="Growth time (days)",
  ylab="Mean weight (g)"
)
ggsave(two_diet_result$plot, file = "two_diet_plot.png", width = 6, height = 5)
two_diet_result$test_data