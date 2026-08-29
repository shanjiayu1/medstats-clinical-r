# medstats

[English](README.md) | 简体中文

**medstats** 是一个用于简化医学与流行病学研究常见工作流程的 R 包，
提供临床数据处理、统计建模、适合发表的表格以及高质量数据可视化工具。

## 主要功能

- 创建基线特征表和适合论文发表的三线表。
- 将纵向临床记录转换为生存分析数据集。
- 拟合广义线性模型、Cox 模型和 GEE 重复测量模型。
- 绘制 Kaplan–Meier 曲线、森林图、限制性立方样条图、ROC 曲线、
  桑基图和纵向汇总图。
- 将格式化表格与 R 图形导出到同一个 Word 文档。

## 安装

从 GitHub 安装开发版本：

```r
install.packages("remotes")

remotes::install_github(
  "shanjiayu1/medstats",
  dependencies = TRUE
)

library(medstats)
```

`dependencies = TRUE` 还会安装可选示例、测试和 vignette 所需的依赖包。

## 函数速查

| 类别       | 函数                          | 说明                                                                            |
| ---------- | ----------------------------- | ------------------------------------------------------------------------------- |
| 表格       | `format_flextable()`        | 为`data.frame`、`gtsummary` 或 `flextable` 对象应用适合发表的三线表格式。 |
| 表格       | `export_word()`             | 将格式化表格和图形导出到一个 Word 文档。                                        |
| 数据处理   | `long_to_surv_data()`       | 将纵向记录转换为每位受试者一行的生存分析数据。                                  |
| 数据处理   | `merge_duplicate_records()` | 使用每个字段的第一个非缺失值合并重复记录。                                      |
| 数据处理   | `make_table1()`             | 创建基线特征表，并可进行组间比较。                                              |
| 统计建模   | `longdata_analysis()`       | 使用描述性统计、组间比较和 GEE 模型分析重复测量数据。                           |
| 统计建模   | `run_glm_auto()`            | 自动完成单因素和多因素广义线性模型分析。                                        |
| 统计建模   | `run_cox_auto()`            | 自动完成单因素和多因素 Cox 回归分析。                                           |
| 数据可视化 | `plot_meanse()`             | 绘制各组均值及标准误随时间变化的折线图。                                        |
| 数据可视化 | `plot_stacked()`            | 绘制不同时间点类别百分比堆积柱状图。                                            |
| 数据可视化 | `plot_km()`                 | 绘制 Kaplan–Meier 累积事件曲线，并可显示风险表。                               |
| 数据可视化 | `plot_sankey()`             | 展示不同时间点之间的状态转移。                                                  |
| 数据可视化 | `plot_forest()`             | 根据格式化的效应估计值和置信区间文本绘制森林图。                                |
| 数据可视化 | `plot_rcs()`                | 使用限制性立方样条评估并展示非线性关联。                                        |
| 数据可视化 | `plot_roc()`                | 使用 ROC 曲线、AUC 和最佳截断值评估区分度。                                     |

## 表格格式化

### 格式化表格

```r
# 格式化 data.frame
ft_data <- format_flextable(
  head(mtcars[, 1:5])
)

# 格式化 gtsummary 对象
tbl <- gtsummary::tbl_summary(
  data = gtsummary::trial,
  include = c(age, marker, grade),
  by = trt
)

ft_summary <- format_flextable(tbl)
```

### 将表格和图形导出到 Word

```r
# 详细用法：分别指定数据、标题和其他参数
table1 <- head(mtcars)

p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
  ggplot2::geom_point()

export_word(
  data_list = list(
    table1,
    p1
  ),
  table_titles = c(
    "Table 1. mtcars dataset",
    "Figure 1. MPG and weight"
  ),
  output_file = "tables_and_plots.docx",
  figure_width = 6,
  figure_height = 5
)

# 简化用法：直接传入对象，标题根据对象类型自动生成
export_word(table1, p1, "tables_and_plots.docx")
```

直接传入对象或省略 `table_titles` 时，函数会自动生成标题
（`Table 1`、`Table 2`、`Figure 1` 等）。表题位于表格上方，
图题位于图形下方。

表格可以与绘图对象或已有的 PNG 图片混合导出。默认情况下，
图形在 9 × 7 英寸的画布上渲染，然后按 Word 页面的可用宽度缩放。
对于 PNG 文件，省略 `word_height` 可以保留图片原始宽高比：

```r
p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
  ggplot2::geom_point()

ggplot2::ggsave(
  filename = "p1.png",
  plot = p1,
  width = 7,
  height = 7,
  dpi = 300
)

export_word(
  data_list = list(table1, "p1.png"),
  table_titles = c("Table 1", "Figure 1"),
  output_file = "results.docx",
  word_width = 6.2
)
```

该函数还可以直接接收 `plot_meanse()`、`plot_roc()` 等包函数返回的
结果对象，并自动使用其中的 `$plot`；也支持图片路径以及由
`officer::plot_instr()` 包装的 base R 图形。

## 数据处理

### 将纵向数据转换为生存分析格式

```r
clinical_data <- datasets::ChickWeight |>
  dplyr::mutate(
    reached_150g = as.integer(weight >= 150)
  )

surv_data <- long_to_surv_data(
  data = clinical_data,
  id_var = "Chick",
  event_flag_var = "reached_150g",
  time_var = "Time",
  baseline_vars = "Diet"
)
```

### 合并重复记录

```r
clinical_records <- data.frame(
  patient_id = c("P001", "P001", "P002", "P002"),
  age = c(NA, 65, 52, 53),
  diagnosis = c("Hypertension", NA, NA, "Diabetes")
)

clean_records <- merge_duplicate_records(
  data = clinical_records,
  group_vars = "patient_id"
)

clean_records
```

当同一重复组中出现相互冲突的非缺失值时，函数会保留当前行顺序中的
第一个值。如果需要采用其他优先级，应先对数据排序。

### 创建基线特征表

```r
table1 <- make_table1(
  data = gtsummary::trial,
  vars = c("age", "marker", "stage", "grade"),
  specific_vars = "marker",  # 使用中位数（P25，P75）描述
  group_var = "trt"
)

table1
```

![基线特征表示例](image/README/1785225766077.png)

## 统计建模

### 使用 GEE 进行重复测量分析

`longdata_analysis()` 汇总各时间点的结局变量，进行横断面组间比较，
评估组内时间趋势，并使用 GEE 检验时间与组别的交互作用。

```r
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
```

![GEE 重复测量分析结果](image/README/1787995086307.png)

输入数据必须为长格式，每一行代表一位受试者在一个测量时间点的记录。
时间变量必须包含可提取的数值部分。

### 自动广义线性模型

线性回归：

```r
linear_results <- run_glm_auto(
  data = mtcars,
  vars = c("hp", "wt"),
  outcome_var = "mpg",
  family = "gaussian"
)
```

Logistic 回归：

```r
logistic_results <- run_glm_auto(
  data = gtsummary::trial,
  vars = c("age", "stage"),
  outcome_var = "response",
  family = "binomial"
)

format_flextable(logistic_results)
```

格式化后的回归表在表头使用模型上标，并在表格左下角添加英文脚注：
`1 Univariable analysis.` 和 `2 Multivariable analysis.`。

### 自动 Cox 回归

```r
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
```

![Cox 回归结果](image/README/1787995016520.png)

## 数据可视化

### 均值 ± 标准误折线图

```r
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
```

对于两组数据，可以设置 `test_method = "t"` 或
`test_method = "wilcox"`，在每个时间点比较两组。具有统计学意义的结果
会标注在对应时间点上方，具体 p 值则返回在 `test_data` 中。

```r
two_diet_result <- plot_meanse(
  data = dplyr::filter(growth_data, diet_label %in% c("Diet 1", "Diet 2")),
  target_var = "weight",
  time_var = "time_label",
  group_var = "diet_label",
  test_method = "wilcox",
  legend_title = "Diet"
)

two_diet_result$test_data
```

![均值与标准误折线图](image/README/1785225730190.png)

### 百分比堆积柱状图

```r
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
```

设置 `group_var = NULL`（默认值）时，每个时间点只绘制一个堆积柱。
提供分组变量后，百分比会在每个“时间 × 组别”组合内分别计算，
各组堆积柱会并排显示。使用 `label_size` 调整柱内百分比标签的字号。

![百分比堆积柱状图](image/README/1787983129815.png)

### Kaplan–Meier 累积事件曲线

```r
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
```

![Kaplan–Meier 累积事件曲线](image/README/1787983164950.png)

### 桑基图

```r
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
  missing_label = "Drop-out"
)

sankey_plot
```

![桑基图](image/README/1787982962290.png)

### 森林图

```r
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
```

![森林图](image/README/1787983212108.png)

### 限制性立方样条图

线性模型：

```r
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
```

Cox 模型：

```r
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
```

![限制性立方样条图](image/README/1787983418170.png)

### ROC 曲线

```r
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
  xlab = "1 - Specificity",
  ylab = "Sensitivity"
)

roc_result$plot
```

![ROC 曲线](image/README/1787993564863.png)
