#سوال یک
# نصب و بارگذاری بسته car
if (!require(car)) install.packages("car")
library(car)

# مقداردهی my.id
my.id <- (20 + 67)
set.seed(my.id)

# مشاهده داده های Salaries
data("Salaries")

# گرفتن نمونه به اندازه my.id
sample_data <- Salaries[sample(1:nrow(Salaries), my.id, replace = FALSE), ]

# متغیر yrs.service
yrs_service <- sample_data$yrs.service

# 1. شاخص های تمرکز (میانگین و میانه)
mean_yrs <- mean(yrs_service)         # میانگین
median_yrs <- median(yrs_service)     # میانه

# 2. شاخص های پراکندگی
sd_yrs <- sd(yrs_service)             # انحراف معیار
var_yrs <- var(yrs_service)           # واریانس
range_yrs <- range(yrs_service)       # دامنه تغییرات
iqr_yrs <- IQR(yrs_service)  #دامنه بین چارکی
# 3. دامنه تغییرات
range_diff <- diff(range_yrs)

##تفسیر چارک ها
#چارک اول(Q1): 25درصد از داده های yrs_service کمتر از این مقدار است.
#چارک دوم(Q2):همان میانه است و نصف داده ها بالای این مقدار و نصف داده ها پایین این مقدار است.
#چارک سوم(Q3):75 درصد از داده ها کمتر از این مقدار است..


# محاسبه چولگی و کشیدگی با پکیج e1071
if (!require(e1071)) install.packages("e1071")
library(e1071)

skewness_yrs <- skewness(yrs_service)
kurtosis_yrs <- kurtosis(yrs_service)

#نتایج
list(
  "میانگین" = mean_yrs,
  "میانه" = median_yrs,
  "انحراف معیار" = sd_yrs,
  "واریانس" = var_yrs,
  "دامنه تغییرات" = range_diff,
  "کمترین و بیشترین مقدار" = range_yrs,
  "دامنه بین چارکی"=iqr_yrs
)



#سوال دو
# مقداردهی my.id
my.id <- (20 + 67)
set.seed(my.id)

# مشاهده داده های Salaries
data("Salaries")

# گرفتن نمونه به اندازه my.id
sample_data <- Salaries[sample(1:nrow(Salaries), my.id, replace = FALSE), ]

# بررسی متغیرهای مورد نظر
head(sample_data)

# نمودارهای مناسب

# 1. نمودار میله‌ای برای rank
barplot(table(sample_data$rank), 
        main = "نمودار میله‌ای برای Rank",
        xlab = "رتبه (Rank)",
        ylab = "تعداد افراد",
        col = c("yellow", "orange", "red"))

# 2. هیستوگرام برای salary
hist(sample_data$salary, 
     main = "هیستوگرام برای Salary",
     xlab = "حقوق (Salary)",
     ylab = "تعداد افراد",
     col = "skyblue",
     breaks = 10)

# 3. نمودار جعبه‌ای (Boxplot) برای salary
boxplot(sample_data$salary, 
        main = "نمودار جعبه‌ای برای Salary",
        ylab = "حقوق (Salary)",
        col = "lightgreen")



#سوال سه
# مقداردهی my.id
my.id <- (20 + 67)
set.seed(my.id)

# تولید 100 عدد از توزیع نرمال با میانگین 50 و انحراف معیار 10
numbers <- rnorm(100, mean = 50, sd = 10)

# گرد کردن اعداد تا 10 رقم اعشار
rounded_numbers <- round(numbers, 10)

# پیدا کردن اعدادی که مضربی از 6 هستند
multiples_of_6 <- rounded_numbers[rounded_numbers %% 6 == 0]

# چاپ اعداد مضربی از 6
multiples_of_6



#سوال چهار
# مقداردهی my.id
my.id <- (20 + 67)
set.seed(my.id)

# نصب و بارگذاری بسته car
if (!require(car)) install.packages("car")
library(car)

# بارگذاری داده Salaries
data("Salaries")

# تعریف تابع f(x) = Σχ
sum_function <- function(x) {
  sum(x)
}

# اعمال تابع به ستون salary
salary_sum <- sum_function(Salaries$salary)

# تعداد مشاهدات
n <- length(Salaries$salary)

# نمایش خروجی
list("مجموع حقوق" = salary_sum, "تعداد مشاهدات" = n)



#سوال پنج
# مقداردهی my.id
my.id <- (20 + 67)
set.seed(my.id)

# تعریف تابع (1 + x^2)
integrand <- function(x) {
  1 + x^2
}

# محاسبه انتگرال در دامنه 1/2 تا 3
result <- integrate(integrand, lower = 1/2, upper = 3)

# نمایش نتیجه
result$value

# مقداردهی my.id
my.id <- (20 + 67)
set.seed(my.id)

# تعریف تابع (x + y^2 - 1)
integrand <- function(x, y) {
  x + y^2 - 1
}

# محاسبه انتگرال دوگانه با استفاده از تابع integrate
result <- integrate(function(y) {
  # محاسبه انتگرال داخلی نسبت به x
  integrate(function(x) integrand(x, y), lower = 3, upper = 5)$value
}, lower = 0, upper = 1)

# نمایش نتیجه
result$value



#سوال شش
# مقداردهی my.id
my.id <- (20 + 67)
set.seed(my.id)

# تعریف ماتریس A
A <- matrix(c(1, 2, 4, 6, 1, 3, 9, 5, 3), nrow = 3, byrow = TRUE)

# 1. محاسبه دترمینان
det_A <- det(A)

# 2. محاسبه وارون ماتریس (اگر موجود باشد)
inv_A <- tryCatch({
  solve(A)
}, error = function(e) {
  "وارون وجود ندارد"
})

# 3. تعویض مقادیر روی قطر اصلی با بردار (0, 0, 0)
A[1, 1] <- 0
A[2, 2] <- 0
A[3, 3] <- 0

# نمایش نتایج
list(
  "دترمینان ماتریس A" = det_A,
  "وارون ماتریس A" = inv_A,
  "ماتریس بعد از تغییرات" = A
)
