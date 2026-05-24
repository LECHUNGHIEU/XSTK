# BÀI TẬP LỚN XÁC SUẤT THỐNG KÊ  
## Phân tích dữ liệu giao dịch cửa hàng điện tử bằng ngôn ngữ R

## 1. Giới thiệu đề tài

Đề tài được thực hiện trong học phần **Xác suất và Thống kê**, với mục tiêu ứng dụng các phương pháp thống kê và phân tích dữ liệu bằng ngôn ngữ **R** để khai thác bộ dữ liệu giao dịch của một cửa hàng điện tử (*Transactional Retail Dataset of Electronics Store*).

Thông qua quá trình xử lý dữ liệu, trực quan hóa và xây dựng mô hình thống kê, đề tài hướng đến việc phân tích hành vi khách hàng, đánh giá các yếu tố ảnh hưởng đến chi phí vận chuyển và xây dựng mô hình dự báo phục vụ cho bài toán thương mại điện tử.

---

## 2. Mục tiêu nghiên cứu

Đề tài tập trung thực hiện các nội dung sau:

- Tiền xử lý và làm sạch dữ liệu
- Phân tích thống kê mô tả
- Trực quan hóa dữ liệu
- Thực hiện thống kê suy diễn
- Xây dựng khoảng tin cậy (Confidence Interval)
- Kiểm định giả thuyết thống kê
- Phân tích phương sai ANOVA
- Xây dựng mô hình hồi quy tuyến tính đơn và hồi quy tuyến tính bội
- Đánh giá tác động của mùa vụ và loại hình vận chuyển đến chi phí giao hàng

---

## 3. Cấu trúc thư mục

```text
XSTK/
│── BTL_XSTK_SOURCE.R          # File mã nguồn R thực hiện toàn bộ quá trình phân tích
│── dirty_data.csv        # Bộ dữ liệu giao dịch ban đầu
│── missing_data.csv      # Dữ liệu chứa giá trị thiếu dùng cho xử lý dữ liệu
│── warehouses.csv        # Dữ liệu thông tin kho hàng
