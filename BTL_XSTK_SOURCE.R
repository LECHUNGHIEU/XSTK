#đọc dữ liệu
dirty_data <- read.csv("C:/R/dirty_data.csv")
head (dirty_data,10)
missing_data <- read.csv("C:/R/missing_data.csv")
head (missing_data,10)
warehouses <- read.csv("C:/R/warehouses.csv")
head (warehouses,10)

file.exists("C:/R/dirty_data.csv")#làm sạch dữ liệu
new_data <-dirty_data[,c(
  "order_total",
  "season",
  "delivery_charges",
  "customer_lat",
  "customer_long",
  "is_expedited_delivery",
  "distance_to_nearest_warehouse")]

head (new_data,10)

#kiểm tra dữ liệu khuyết
apply (is.na(new_data),2,which)

#kiểm tra biến season
unique (new_data$season)
new_data$season[new_data$season == 'spring'] <- 'Spring'
new_data$season[new_data$season == 'summer'] <- 'Summer'
new_data$season[new_data$season == 'autumn'] <- 'Autumn'
new_data$season[new_data$season == 'winter'] <- 'Winter'

#kiểm tra về các biến season
unique (new_data$season)

#thống kê mô tả 
des_function <- function(x) {c(mean(x),median(x),sd(x),min(x),max(x))}

#tạo một ma trận chứ chứa các biến: delivery_charges, customer_lat, customer_long,order_total, distance_to_nearest_warehouse.
des_table <-apply (new_data [,c("delivery_charges","customer_lat","customer_long","order_total","distance_to_nearest_warehouse")],2,des_function)
rownames (des_table) =c("mean","median","sd","min","max")
des_table

#thống kê số lượng
table(new_data$season)

#tạo bảng tần số cho biến is_expedited_delivery
table(new_data$is_expedited_delivery)

#biểu đồ histogram
hist (new_data$order_total,xlab="order_total",main ="Đồ thị tần số của tổng giá trị đơn hàng",col ="lightgreen")
hist (new_data$delivery_charges,xlab="delivery_charges",main ="Đồ thị tần số của phí vận chuyển",col ="lightblue")
boxplot (order_total~season,new_data,main="Biểu đồ tổng giá trị đơn hàng theo mùa")

#tạo hàm và chuyển ngoại lai thành NA
rm.out <- function (x,na.rm = TRUE,...){
  qnt <- quantile(x,probs = c(.25,.75),na.rm = na.rm,...)
  H <- 1.5*IQR(x,na.rm = na.rm)
  y <- x
  y [x<(qnt[1] - H )] <- NA
  y [x>(qnt[2] + H )] <- NA
  y}

#Áp dụng hàm rm.out vừa tạo để loại bỏ các giá trị ngoại lai từ cột order_total để làm sạch dữ liệu
Spring_data = subset (new_data ,new_data $ season == "Spring")
Spring_data $ order_total = rm.out (Spring_data $ order_total)

Summer_data = subset (new_data,new_data $ season == "Summer")
Summer_data $ order_total = rm.out (Summer_data $ order_total)

Autumn_data = subset (new_data,new_data $ season == "Autumn")
Autumn_data $ order_total = rm.out (Autumn_data $ order_total)

Winter_data = subset (new_data,new_data $ season == "Winter")
Winter_data $ order_total = rm.out (Winter_data $ order_total)

#gộp dũ liệu 4 mùa 
new_data_1 <- rbind (Spring_data, Summer_data, Autumn_data, Winter_data)
apply (is.na(new_data_1),2,sum)
apply (is.na(new_data_1),2,mean)
new_data_1 <- na.omit (new_data_1)

#Tạo biểu đồ boxplot hiển thị phân phối của order_total theo từng mùa
boxplot(order_total ~ season,new_data_1,col =c(2,3,4,5),main="Biểu đồ tổng giá trị đơn hàng theo mùa")

#Tạo biểu đồ boxplot hiển thị phân phối của delivery_charges theo từng mùa.
boxplot(delivery_charges ~ season,new_data_1,col =c(2,3,4,5),main="Biểu đồ phí vận chuyển theo mùa")

#Tạo biểu đồ Boxplot thể hiện phân phối của delivery_charges theo is_expedited_delivery
boxplot(delivery_charges ~ is_expedited_delivery,new_data_1,col =c(2,3),main="Biểu đồ phí vận chuyển theo yêu cầu chuyển phát nhanh")

#Biểu đồ Scatter Plot
par (mfrow =c(1,1))
plot ( delivery_charges ~ customer_lat, main ="Biểu đồ tương quan giữa phí vận chuyển và vĩ độ khách hàng", col =c(1),data =new_data)

#Tạo biểu đồ phân tán thể hiện mối quan hệ  giữa delivery_charges với customer_lat
plot (delivery_charges ~ customer_long, main ="Biểu đồ tương quan giữa phí vận chuyển và kinh độ khách hàng", col =c(1),data =new_data)

#Tạo biểu đồ phân tán thể hiện mối quan hệ  giữa delivery_charges với distance_to_nearest_warehouse.
plot (delivery_charges ~ distance_to_nearest_warehouse, main ="Biểu đồ tương quan giữa phí vận chuyển và khoảng cách từ địa chỉ khách hàng đến nhà kho gần nhất", col=c(1),data =new_data)

set.seed(123)
#Kiểm định 1 mẫu
# khoang tin cay cho chi phi tổng
mean_order <- mean(new_data$order_total, na.rm = TRUE)  # Trung bình
sd_order <- sd(new_data$order_total, na.rm = TRUE)      # Độ lệch chuẩn
n_order <- sum(!is.na(new_data$order_total))            # Số lượng mẫu

error_order <- qt(0.975, df = n_order - 1) * sd_order / sqrt(n_order)  # Biên sai số
CI_order <- c(mean_order - error_order, mean_order + error_order)      # Khoảng tin cậy
CI_order


# khoang tin cay cho chi phí giao hàng
p_delivery <- mean(new_data$delivery_charges, na.rm = TRUE)  # Tỷ lệ
sd_delivery <- sd(new_data$delivery_charges, na.rm = TRUE)      # Độ lệch chuẩn
n_delivery <- sum(!is.na(new_data$delivery_charges))         # Số lượng mẫu

error_delivery <- qt(0.975, df = n_delivery - 1) * sd_delivery / sqrt(n_delivery)  # Biên sai số
CI_delivery <- c(p_delivery - error_delivery, p_delivery + error_delivery)             # Khoảng tin cậy
CI_delivery

#KIỂM ĐỊNH CHO GIÁ TRỊ TRUNG BÌNH
# Kiểm tra dữ liệu sơ bộ
summary(new_data$order_total)

# Tính trung bình mẫu
mean_order <- mean(new_data$order_total, na.rm = TRUE)
print(mean_order)

# Kiểm định t một mẫu
test_result <- t.test(
  x = new_data$order_total,
  mu = 10000,
  alternative = "less",
  conf.level = 0.95
)

# Xem kết quả
print(test_result)

#KIỂM ĐỊNH 1 MẪU CHI PHÍ GIAO HÀNG
# Kiểm tra dữ liệu
summary(new_data$delivery_charges)

# Tính trung bình mẫu
mean_delivery <- mean(new_data$delivery_charges, na.rm = TRUE)
print(mean_delivery)

# Kiểm định t một mẫu
test_result <- t.test(
  x = new_data$delivery_charges,
  mu = 75,
  alternative = "greater",
  conf.level = 0.95
)

# Hiển thị kết quả
print(test_result)

#KIỂM ĐỊNH 2 MẪU
# Cố định tính ngẫu nhiên
set.seed(123)

# Kiểm tra tính đồng nhất của phương sai
var.test(delivery_charges ~ is_expedited_delivery, data = new_data_1)

# 4.3.1 Kiểm định t 2 mẫu (So sánh trung bình chi phí giao hàng giữa 2 loại dịch vụ)
# H0: Chi phí giao hàng trung bình của Hỏa tốc và Thường là như nhau
# H1: Chi phí giao hàng trung bình của Hỏa tốc cao hơn Giao thường
t_test_2_mean <- t.test(delivery_charges ~ is_expedited_delivery, 
                        data = new_data_1, 
                        alternative = "less")
print("--- Kết quả kiểm định 2 trung bình ---")
print(t_test_2_mean)

# 4.3.2 Kiểm định 2 tỷ lệ (So sánh tỷ lệ chọn hỏa tốc giữa mùa Spring và Summer)
# Bước 1: Lập bảng tần số
table_2_prop <- table(new_data_1$season, new_data_1$is_expedited_delivery)

# Bước 2: Trích xuất số ca 'TRUE' (hỏa tốc) và tổng số mẫu của Spring và Summer
success_counts <- c(table_2_prop["Spring", 2], table_2_prop["Summer", 2])
total_counts <- c(sum(table_2_prop["Spring", ]), sum(table_2_prop["Summer", ]))

# Bước 3: Thực hiện kiểm định tỷ lệ
test_2_prop <- prop.test(success_counts, total_counts)
print("--- Kết quả kiểm định 2 tỷ lệ ---")
print(test_2_prop)

#Anova
#Kiểm định Shapiro-Wilk (Kiểm định phân phối chuẩn)
library(nortest)

shapiro.test(Spring_data $order_total)

shapiro.test(Summer_data $order_total)

shapiro.test(Autumn_data $order_total)

shapiro.test(Winter_data $order_total)

av_residual <- rstandard(aov(delivery_charges ~ season*is_expedited_delivery, data=new_data_1))
shapiro.test(av_residual)

#Kiểm tra bằng qqplot

qqnorm(Spring_data$order_total)
qqline(Spring_data$order_total, col = "red")

qqnorm(Summer_data$order_total)
qqline(Summer_data$order_total, col = "red")

qqnorm(Autumn_data$order_total)
qqline(Autumn_data$order_total, col = "red")

qqnorm(Winter_data$order_total)
qqline(Winter_data$order_total, col = "red")

qqnorm(av_residual)
qqline(av_residual, col = "red")

# ==================================================================
# [MỞ RỘNG 1 - NÂNG CẤP] KIỂM ĐỊNH PHI THAM SỐ CHUYÊN SÂU
# ==================================================================
cat("\n--- THỰC HIỆN KIỂM ĐỊNH PHI THAM SỐ NÂNG CẤP ---\n")

# Cài đặt thư viện rstatix để tính Effect Size (Bỏ dấu # để chạy 1 lần nếu máy chưa có)
# install.packages("rstatix")
library(rstatix)

# 1. THỐNG KÊ MÔ TẢ ĐẶC THÙ (DÀNH CHO PHI THAM SỐ)
cat("\n[1.1] Thống kê Trung vị (Median) và IQR chi phí giao hàng:\n")
# Tính Median và IQR để so sánh độ hội tụ của dữ liệu thay vì Mean
summary_delivery <- aggregate(delivery_charges ~ is_expedited_delivery, data = new_data_1, 
                              FUN = function(x) c(Median = median(x), IQR = IQR(x)))
print(summary_delivery)

cat("\n[1.2] Thống kê Trung vị (Median) và IQR tổng đơn hàng theo mùa:\n")
summary_season <- aggregate(order_total ~ season, data = new_data_1, 
                            FUN = function(x) c(Median = median(x), IQR = IQR(x)))
print(summary_season)

# 2. KIỂM ĐỊNH WILCOXON & ĐO LƯỜNG EFFECT SIZE
cat("\n[2] Kiểm định Wilcoxon (So sánh 2 nhóm) & Kích thước hiệu ứng:\n")
wilcox_test_result <- wilcox.test(delivery_charges ~ is_expedited_delivery, 
                                  data = new_data_1, alternative = "two.sided")
print(wilcox_test_result)

# Tính Effect Size (r) cho Wilcoxon
# Quy ước: r < 0.3 (Nhỏ) | 0.3 <= r < 0.5 (Trung bình) | r >= 0.5 (Lớn)
wilcox_eff <- wilcox_effsize(new_data_1, delivery_charges ~ is_expedited_delivery)
print("--- Độ lớn của sự khác biệt (Effect Size 'r') ---")
print(wilcox_eff)

# 3. KIỂM ĐỊNH KRUSKAL-WALLIS & ĐO LƯỜNG EFFECT SIZE
cat("\n[3] Kiểm định Kruskal-Wallis (So sánh nhiều nhóm) & Kích thước hiệu ứng:\n")
kruskal_test_result <- kruskal.test(order_total ~ season, data = new_data_1)
print(kruskal_test_result)

# Tính Effect Size (eta squared) cho Kruskal-Wallis
kruskal_eff <- kruskal_effsize(new_data_1, order_total ~ season)
print("--- Độ lớn của sự tác động (Effect Size 'eta2') ---")
print(kruskal_eff)
# ==================================================================

new_data_1$season <- as.factor(new_data_1$season)
new_data_1$is_expedited_delivery <- as.factor(new_data_1$is_expedited_delivery)

#Kiểm định tính đồng nhất của phương sai
library(car)
leveneTest(order_total ~ season, data=new_data_1)

leveneTest(delivery_charges ~ season*is_expedited_delivery, data=new_data_1)

#Phân tích Anova 1 yếu tố
anova_model <- aov(order_total ~ season, data=new_data_1)
summary(anova_model)

#Phân tích Anova 2 yếu tố
anova_model_2 <- aov(delivery_charges ~ season*is_expedited_delivery, data=new_data_1)
summary(anova_model_2)

#Hồi quy tuyến tính đơn
# Cố định tính ngẫu nhiên
set.seed(123)
# Chia bộ dữ liệu thành 2 phần: train_data (80%) dùng để xây dựng mô hình hồi quy và test_data (20%) dùng để thực hiện dự báo. 
train.rows <- sample ( rownames ( new_data ) , dim (new_data ) [1]* 0.8)
train_data <- new_data_1 [ train.rows , ]
test.rows <- setdiff ( rownames (new_data_1 ) , train.rows )
test_data <- new_data_1 [ test.rows , ]

model_simple <- lm(delivery_charges ~ is_expedited_delivery, data = train_data)
summary(model_simple)

#Sử dụng hàm plot() để kiểm tra các giả định của hồi quy tuyến tính (tính chuẩn của sai số, phương sai không đổi).
par(mfrow = c(2, 2))
plot(model_simple)

#Sử dụng hàm predict() để tính toán giá trị dự báo cho 20% dữ liệu còn lại.
test_data$predicted_value <- predict(model_simple, test_data)
head(test_data[, c("delivery_charges", "is_expedited_delivery", "predicted_value")], 5)

#hồi quy tuyến tính bội
#Sử dụng Hàm lm() để xây dựng một mô hình hồi quy tuyến tính.
model_1 <-lm( delivery_charges ~ customer_lat + customer_long + distance_to_nearest_warehouse + season +is_expedited_delivery , data = train_data )
summary ( model_1)

#Ta xây dựng mô hình model_2 bỏ đi biến customer_lat,customer_long ,distance_to_nearest_warehouse từ model_1.
model_2 <-lm( delivery_charges ~ season +is_expedited_delivery , data = train_data )
summary ( model_2)

#Ta xây dựng mô hình model_3 bỏ đi biến season từ mô hình model_2 .
model_3 <-lm( delivery_charges ~is_expedited_delivery , data = train_data )
summary ( model_3)

#So sánh hai model_1 và model_2: 
anova ( model_1 , model_2)

#So sánh hai model_2 và model_3: 
anova ( model_2 , model_3)

#Biểu đồ residuals vs. leverage: Để phát hiện các điểm ngoại lai hoặc ảnh hưởng mạnh.
par ( mfrow =c(2,2))
plot ( model_2)

#Sử dụng hàm predict()  để tính toán giá trị dự đoán chi phí vận chuyển dựa trên mô hình hồi quy tốt nhất (ở đây là model_2) cho dữ liệu mới (ở đây là test_data).
test_data $ predicted_value <- predict ( model_2, test_data )
head ( test_data ,10)
