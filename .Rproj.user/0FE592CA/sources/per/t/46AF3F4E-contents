---
title: "BÁO CÁO ĐỀ XUẤT CHƯƠNG TRÌNH KHUYẾN KHÍCH SỨC KHỎE"
author: "Anh Thư"
date: "2025-07-20"
output:
  html_document:
    toc: true
    toc_depth: 3
    number_sections: true
    df_print: paged
    code_folding: hide
---

```{r setup, include=FALSE}
# Load ALL required packages
library(knitr)
library(dplyr)
library(ggplot2)
library(tibble)  # This is the missing package
library(janitor)
library(scales)
library(purrr)
library(lubridate)

knitr::opts_chunk$set(
  echo = TRUE, 
  message = FALSE, 
  warning = FALSE,
  fig.path = "figures/"  # Organize figures in subfolder
)
```
# Giới thiệu

SuperLife, một trong những công ty bảo hiểm nhân thọ hàng đầu tại Lumaria, đang tìm kiếm giải pháp nhằm cải thiện tỷ lệ tử vong kỳ vọng của khách hàng sau khi mua hợp đồng bảo hiểm nhân thọ. Trong bối cảnh thị trường ngày càng cạnh tranh và người tiêu dùng quan tâm nhiều hơn đến các yếu tố sức khỏe, SuperLife nhận thấy rằng việc kết hợp các chương trình khuyến khích sức khỏe vào các sản phẩm bảo hiểm dài hạn có thể mang lại lợi ích kép – vừa nâng cao sức khỏe cộng đồng, vừa tạo ra giá trị kinh tế lâu dài cho doanh nghiệp.

Chương trình này được thiết kế dựa trên dữ liệu thực tế và các mô hình mô phỏng tử vong – chi phí – chi trả bảo hiểm.

## Mục tiêu chính:

- Giảm xác suất tử vong kỳ vọng thông qua can thiệp sức khỏe

- Tối ưu hóa chi phí can thiệp và lợi ích tài chính

- Gia tăng giá trị kỳ vọng trên mỗi hợp đồng bảo hiểm

- Cá nhân hóa chương trình theo nhóm khách hàng

- Gắn kết khách hàng và tạo động lực duy trì hợp đồng

## Phạm vi:

Áp dụng cho hai sản phẩm:

- Bảo hiểm trọn đời

- Bảo hiểm kỳ hạn 20 năm

# Thiết kế chương trình can thiệp

## Mục tiêu thiết kế
Thiết lập một chương trình can thiệp sức khỏe toàn diện, hướng tới:
- Giảm tỷ lệ tử vong trung bình dài hạn
- Tăng hiệu quả chi phí đầu người
- Kết hợp đa dạng loại hình can thiệp (ngắn/dài hạn, cá nhân/cộng đồng)

## Nguyên lý thiết kế
- Dựa trên dữ liệu và bằng chứng khoa học
- Khả thi và linh hoạt
- Tối ưu hoá chi phí – hiệu quả

## Khung phân nhóm can thiệp
10 can thiệp có tác động tối ưu (về chi phí, hiệu quả) được chia thành 5 nhóm, mỗi nhóm phản ánh cơ chế tác động đặc thù:

```{r}
group_data <- tibble(
  `Nhóm Can thiệp` = c("Financial", "Behavioral", "Update", "Health Plan", "Education"),
  `Mục tiêu chính` = c("Ổn định tài chính", "Thay đổi hành vi", "Thông tin cập nhật", "Kế hoạch cá nhân", "Giáo dục cộng đồng"),
  `Tính chất` = c("Dài hạn", "Tức thời", "Linh hoạt", "Từng bước", "Theo mùa")
)
kable(group_data, caption = "Khung phân nhóm can thiệp")
```

## Mô tả can thiệp (tóm tắt)

### Financial Group

**Can thiệp**: *Financial Literacy Workshops*  
&nbsp;&nbsp;&nbsp;&nbsp;- **Thời gian**: Tháng 3 hàng năm (bắt đầu từ 2024, mỗi 2 năm tổ chức 1 lần)  
&nbsp;&nbsp;&nbsp;&nbsp;- **Công cụ**: Webinar qua Zoom (2 buổi), tài liệu minh họa gửi qua email, khảo sát tự đánh giá sau buổi học  
&nbsp;&nbsp;&nbsp;&nbsp;- **Nội dung**: Chi tiêu sức khỏe hợp lý, dự phòng rủi ro tài chính y tế  
&nbsp;&nbsp;&nbsp;&nbsp;- **Đo lường**:  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Tỷ lệ hoàn thành khóa học  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Tỷ lệ trả lời đúng tăng qua pre-test vs post-test  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Mức độ hài lòng trung bình >80%

---

### Behavioral Group

**Can thiệp 1**: *Sun Safety Awareness*  
&nbsp;&nbsp;&nbsp;&nbsp;- **Thời gian**: Tháng 4–10 (khi chỉ số UV tăng cao)  
&nbsp;&nbsp;&nbsp;&nbsp;- **Công cụ**: SMS cảnh báo UV, email hướng dẫn chống nắng, người tư vấn tặng sản phẩm  
&nbsp;&nbsp;&nbsp;&nbsp;- **Đo lường**:  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Tỷ lệ mở email  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Số lượt nhận quà

**Can thiệp 2**: *Hydration Campaign*  
&nbsp;&nbsp;&nbsp;&nbsp;- **Thời gian**: Tháng 5–8  
&nbsp;&nbsp;&nbsp;&nbsp;- **Công cụ**: SMS nhắc uống nước, gửi PDF theo dõi uống nước  
&nbsp;&nbsp;&nbsp;&nbsp;- **Đo lường**: Khảo sát giữa chiến dịch về tiêu thụ nước

---

### Update Group

**Can thiệp**: *Online Health Resource Hub*  
&nbsp;&nbsp;&nbsp;&nbsp;- **Thời gian**: Cập nhật định kỳ hằng tháng  
&nbsp;&nbsp;&nbsp;&nbsp;- **Công cụ**: Website thông tin sức khỏe, tin tức, SMS nhắc nhở  
&nbsp;&nbsp;&nbsp;&nbsp;- **Đo lường**:  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Số lượt truy cập  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Tỷ lệ quay lại

---

### Health Plan Group

**Can thiệp**: *Cá nhân hóa kế hoạch sức khỏe*  
&nbsp;&nbsp;&nbsp;&nbsp;- **Thời gian**: 6 bước triển khai từ tháng 1 đến tháng 12  
&nbsp;&nbsp;&nbsp;&nbsp;- **Công cụ**:  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Email kế hoạch mẫu (Tháng 1)  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Tư vấn 1-1 qua Teams (Tháng 2)  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Đánh giá giữa kỳ (Tháng 6)  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Điều chỉnh kế hoạch (Tháng 7)  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Tổng kết và định hướng (Tháng 12)  
&nbsp;&nbsp;&nbsp;&nbsp;- **Đo lường**:  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Tỷ lệ hoàn thành từng giai đoạn  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Mức độ tuân thủ kế hoạch

---

### Education Group

**Can thiệp 1**: *Educational Workshops*  
&nbsp;&nbsp;&nbsp;&nbsp;- **Thời gian**: Mỗi 6 tháng, từ năm 1 đến năm 4 (Tháng 5 & 12)  
&nbsp;&nbsp;&nbsp;&nbsp;- **Công cụ**: Hội thảo trực tuyến, chia sẻ qua website/email  
&nbsp;&nbsp;&nbsp;&nbsp;- **Nội dung**: Chủ đề sức khỏe cộng đồng  
&nbsp;&nbsp;&nbsp;&nbsp;- **Đo lường**:  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Số người đăng ký  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Tỷ lệ hoàn thành khảo sát  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Chỉ số thay đổi kiến thức

**Can thiệp 2**: *Environmental Wellness*  
&nbsp;&nbsp;&nbsp;&nbsp;- **Thời gian**: Mỗi 6 tháng, từ năm 1 đến năm 4  
&nbsp;&nbsp;&nbsp;&nbsp;- **Công cụ**: Hội thảo về sức khỏe môi trường  
&nbsp;&nbsp;&nbsp;&nbsp;- **Nội dung**: Hướng dẫn cải thiện môi trường sống  
&nbsp;&nbsp;&nbsp;&nbsp;- **Đo lường**:  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Khảo sát hành vi  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Truy cập video  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Lượt tải tài liệu

**Can thiệp 3**: *Community Gardens*  
&nbsp;&nbsp;&nbsp;&nbsp;- **Thời gian**: Tháng 6 mỗi năm, từ năm 1 đến năm 4  
&nbsp;&nbsp;&nbsp;&nbsp;- **Công cụ**: Hội thảo trồng rau sạch  
&nbsp;&nbsp;&nbsp;&nbsp;- **Nội dung**: Hướng dẫn kỹ thuật, thi đua chia sẻ  
&nbsp;&nbsp;&nbsp;&nbsp;- **Đo lường**:  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Số người gửi ảnh  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Truy cập nội dung  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Phản hồi hài lòng

**Can thiệp 4**: *Travel Safety Tips*  
&nbsp;&nbsp;&nbsp;&nbsp;- **Thời gian**: Tháng 6 mỗi năm, từ năm 1 đến năm 4  
&nbsp;&nbsp;&nbsp;&nbsp;- **Công cụ**: Hội thảo du lịch an toàn, checklist cá nhân  
&nbsp;&nbsp;&nbsp;&nbsp;- **Nội dung**: Hướng dẫn vệ sinh, thuốc, checklist du lịch  
&nbsp;&nbsp;&nbsp;&nbsp;- **Đo lường**:  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Lượt tải checklist  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Số người tham gia  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ Đánh giá hữu ích


##  Kênh triển khai

- **Online**: Email, Website, Zoom, Teams, YouTube
- **Offline**: Tài liệu in, tư vấn tại điểm dịch vụ
- **Tự động hóa**: SMS, email nhắc lịch, form tự đánh giá

## Lộ trình triển khai

```{r}
timeline <- tibble(
  'Giai đoạn' = c("2024–2026", "2026–2028", "2028–"),
  'Mục tiêu chính' = c("Tăng nhận thức và hành vi cơ bảng", "Duy trì can thiệp dài hạn hiệu quả", "Duy trì hiệu quả"),
  'Ưu tiên nhóm' = c("Behavioral + Education + Health Plan", "Education + Health Plan", "Health Plan")
)
kable(timeline, caption = "Lộ trình triển khai chương trình")
```
![Time line](plots/intervention_timeline.png)


# Phân tích tài chính

## Chi phí dự kiến

Chi phí hàng năm là biến số phụ thuộc vào số lượng khách hàng cần dịch vụ từ chương trình sức khỏe, cụ thể có kỳ vọng và độ lệch chuẩn hàng năm như sau

```{r table-chi-phi, echo=FALSE, message=FALSE, warning=FALSE}
library(dplyr)
library(knitr)
library(kableExtra)

# Tạo bảng dữ liệu
chi_phi_data <- data.frame(
  STT = 1:31,
  Nam = 2024:2054,
  Tong_chi_phi = c(254, 444, 256, 294, 262, 251, 274, 319, 334, 422,
                   525, 490, 692, 627, 704, 866, 835, 684, 871, 695,
                   823, 718, 537, 570, 549, 479, 465, 388, 274, 365, 465),
  Do_lech_chuan = c(813, 1096, 853, 646, 551, 527, 558, 608, 636, 620,
                        819, 729, 946, 848, 882, 844, 1010, 887, 957, 840,
                        954, 912, 765, 769, 747, 691, 737, 729, 532, 625, 676)
)

# Xuất bảng kable đẹp
chi_phi_data %>%
  kable(
    format = "html",  # nếu knit HTML
    col.names = c("STT", "Năm", "Tổng chi phí (triệu Luma)", "Chi phí bình quân đầu người (nghìn Luma)"),
    caption = "Chi phí chương trình theo từng năm (2024–2054), làm tròn đến hàng trăm"
  ) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed"))
```

Tổng chi phí can thiệp theo năm cho thấy xu hướng tăng nhanh trong giai đoạn 2024–2040, đạt đỉnh quanh năm 2040, sau đó lại giảm. Vùng sai số (±1 độ lệch chuẩn) mở rộng theo thời gian, đặc biệt sau năm 2035, cho thấy sự gia tăng không chắc chắn trong các ước lượng chi phí dài hạn. Điều này nhấn mạnh nhu cầu giám sát sát sao các yếu tố đầu vào và cập nhật mô hình định kỳ để quản lý rủi ro tài chính, đồng thời thiết kế các chiến lược kiểm soát chi phí hiệu quả cho trung và dài hạn.

![Phân phối chi phí từng năm](plots/cost_by_year.png)

Cơ cấu chi phí như sau:


```{r display_cost_ratios, echo=FALSE, message=FALSE}
library(knitr)
library(kableExtra)
library(dplyr)

# Tạo bảng dữ liệu
cost_ratio_table <- tribble(
  ~Intervention, ~`2024`, ~`2025`, ~`2026`, ~`2027`, ~`2028`, ~`2029`,
  "Personalized Health Plans", 15.4, 15.7, 17.0, 32.6, 36.5, 38.2,
  "Educational Workshops", 3.7, 3.8, 4.1, 7.9, 0.0, 0.0,
  "Online Health Resources", 9.5, 9.7, 10.6, 20.2, 22.6, 23.7,
  "Hydration Campaigns", 41.4, 42.2, 45.7, 0.0, 0.0, 0.0,
  "Sun Safety Awareness", 9.5, 9.7, 0.0, 0.0, 0.0, 0.0,
  "Environmental Wellness", 1.6, 1.6, 1.8, 3.4, 0.0, 0.0,
  "Holistic Health Assessments", 15.4, 15.7, 17.0, 32.6, 36.5, 38.2,
  "Community Gardens", 0.8, 0.8, 0.9, 1.7, 0.0, 0.0,
  "Travel Safety Tips", 0.8, 0.8, 0.9, 1.7, 0.0, 0.0,
  "Financial Literacy Workshops", 1.9, 0.0, 2.1, 0.0, 4.4, 0.0
)

# Hiển thị bảng
cost_ratio_table %>%
  kable(format = "html", caption = "Tỷ lệ Chi phí (%) theo Intervention và Năm") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  column_spec(1, bold = TRUE, width = "6cm") %>%
  row_spec(0, bold = TRUE, background = "#DDEEFF")
```
![Tỷ lệ chi phí cho những intervention](plots/cost_ratio.png)
- **Xu hướng nổi bật: **

Personalized Health Plans và Holistic Health Assessments chiếm tỷ lệ cao và ngày càng tăng từ 15.4% (2024) lên đến 38.2% (2029). Điều này phản ánh sự ưu tiên cho các giải pháp chăm sóc cá nhân và toàn diện trong trung và dài hạn.

Hydration Campaigns đóng vai trò chiếm tỷ trọng lớn trong giai đoạn đầu (41.4%–45.7% trong 2024–2026), sau đó kết thúc hẳn từ 2027, cho thấy tính chất ngắn hạn và tập trung của chương trình này.

- **Suy giảm hoặc chấm dứt sau giai đoạn đầu:**

Các chương trình như Sun Safety Awareness, Educational Workshops, và Environmental Wellness có xu hướng giảm dần hoặc kết thúc trước năm 2029. Điều này có thể là do mục tiêu can thiệp đã đạt được hoặc do giới hạn ngân sách và hiệu quả tương đối thấp.

Community Gardens và Travel Safety Tips duy trì tỷ lệ rất thấp và cũng kết thúc trước 2029, cho thấy chúng có vai trò bổ trợ chứ không phải trọng tâm.

- **Sự xen kẽ định kỳ:**

Financial Literacy Workshops xuất hiện gián đoạn theo mô hình 2 năm một lần (có mặt năm 2024, 2026, 2028), phản ánh tính chu kỳ cố định trong timeline can thiệp. Điều này cũng đồng nghĩa rằng sau 2029, tỷ lệ chi phí cho nhóm này sẽ chỉ tăng vào các năm chẵn tương ứng như 2030, 2032,…

- **Tính ổn định hậu 2029:**

Vì timeline giữ nguyên, nên mô hình phân bổ chi phí theo tỷ lệ sẽ lặp lại theo chu kỳ sau 2029, với sự thay đổi chủ yếu đến từ:

Những intervention định kỳ (như Financial Literacy Workshops).

Các yếu tố ngẫu nhiên như hiệu quả thực tế và chi phí thực hiện (nếu mô phỏng lại).

Personalized Health Plans, Holistic Health Assessments và Online Health Resources có khả năng duy trì tỷ lệ chi phí cao ổn định sau 2029 do được lặp lại đều đặn hằng năm trong timeline dài hạn.

## Hiệu quả tài chính 
Khi áp dụng các interventions, do khả năng bồi thường cho khách hàng giảm đi, do đó, lợi ích khi thực hiện chương trình được đo bằng số tiền phải chi trả cho khách hàng nếu không có interventions trừ đi số tiền phải chi trả cho khách hàng nếu có interventions (tính cả chi phí chương trình).
Khi đó, dòng tiền ước tính hàng năm như sau:

```{r savings_table, echo=FALSE, message=FALSE, warning=FALSE}
library(knitr)
library(dplyr)

# Tạo bảng dữ liệu
savings_df <- data.frame(
  year = 2024:2054,
  avg_savings = c(84246,111056,48744,156706,170738,164749,133726,223181,226666,320078,
                  382975,357010,438808,434373,554796,499134,471165,452316,509629,353805,
                  195677,246782,150463,206430,225451,117021,139035,151112,144726,182135,188535),
  sd_saving = c(338223,334097,210723,458704,462968,431619,361540,500497,484510,606885,
                684923,642386,715298,704622,817537,654973,727031,760325,824132,584924,
                364801,532788,377619,418531,533728,337040,417443,458786,439464,489744,502755)
)

# Hiển thị bảng với kable
kable(savings_df, col.names = c("Năm", "Trung bình (LUM)", "Độ lệch chuẩn (LUM)"),
      caption = "Dòng tiền tiết kiệm trung bình hàng năm từ chương trình can thiệp sức khỏe")

```
Dòng tiền tiết kiệm từ chương trình can thiệp sức khỏe có những đặc điểm đáng chú ý sau:

- **Tăng trưởng ban đầu và dao động trung hạn:** Trong giai đoạn đầu (2024–2026), tiết kiệm trung bình có xu hướng dao động mạnh với giá trị thấp (dao động quanh 48,000–111,000) và độ lệch chuẩn tương đối cao, phản ánh sự không ổn định trong giai đoạn mới triển khai.

- **Bùng nổ giai đoạn 2027–2041:** Từ năm 2027 đến khoảng 2041, giá trị tiết kiệm trung bình tăng mạnh và đạt đỉnh, vượt ngưỡng 550,000 LUM vào năm 2038. Đây là giai đoạn các can thiệp phát huy hiệu quả tối đa, đi kèm với sự gia tăng đáng kể về độ lệch chuẩn, thể hiện sự không đồng đều giữa các nhóm khách hàng hoặc điều kiện triển khai.

- **Giảm dần và ổn định về sau:** Từ sau năm 2042, dòng tiền tiết kiệm giảm dần và trở nên ổn định hơn với độ lệch chuẩn cũng giảm nhẹ. Điều này phản ánh hiệu ứng của các can thiệp đã bão hòa, đồng thời lực tác động giảm do thời gian kéo dài và thay đổi về dân số mục tiêu.

- **Tính bền vững:** Mặc dù không duy trì được mức tiết kiệm cực đại sau năm 2041, chương trình vẫn đem lại khoản tiết kiệm ổn định và có giá trị trong suốt giai đoạn 30 năm, với mức trung bình từ 150,000–250,000/năm vào cuối chu kỳ.

- **Biến động lớn về độ lệch chuẩn:**  Độ lệch chuẩn cao trong nhiều năm phản ánh tính không chắc chắn và sự khác biệt trong mức độ hiệu quả giữa các nhóm dân cư hoặc các can thiệp cụ thể. Điều này cho thấy cần xem xét chiến lược điều chỉnh hoặc tái thiết kế để tối ưu hóa hiệu quả đồng đều hơn.

![Saving by year](plots/saving_plot.png)
Do có sự biến động khá lớn trong dòng tiền, để đánh giá tổng thể hiệu quả tài chính của chương trình, chúng tôi tiến hành phân tích phân phối Giá trị hiện tại ròng (NPV) của dòng tiền tiết kiệm với lãi suất chiết khấu 5%. Phân tích này cho phép phản ánh mức độ biến động và rủi ro tài chính của chương trình trong dài hạn, từ đó hỗ trợ ra quyết định về tính bền vững và khả thi của chiến lược can thiệp.
![Saving distribution](plots/FULL_npv_difference_dist.png)
Phân phối NPV của dự án cho thấy giá trị kỳ vọng khoảng 4,050,907. Đáng chú ý, xác suất NPV nhỏ hơn hoặc bằng 0 gần như bằng 0, cho thấy khả năng lỗ là cực kỳ thấp. Điều này chứng tỏ rằng khoản tiết kiệm từ chương trình gần như chắc chắn sẽ vượt qua chi phí đầu tư ban đầu, khẳng định hiệu quả tài chính tích cực của dự án.
Để đánh giá độ nhạy của hiệu quả tài chính đối với sự thay đổi trong giả định chiết khấu, chúng tôi tiến hành phân tích NPV của chương trình dưới nhiều mức lãi suất khác nhau. Việc này giúp làm rõ mức độ ảnh hưởng của lãi suất chiết khấu đến giá trị hiện tại ròng của dòng tiền, từ đó cung cấp góc nhìn toàn diện hơn về tính bền vững tài chính của dự án trong các kịch bản kinh tế khác nhau.
![Sensitive of NPV depend on interest rate](plots/discount_rate_sensitivity.png)
Dựa trên bảng kết quả phân tích độ nhạy theo lãi suất chiết khấu, có thể đưa ra các nhận xét sau:

- **NPV kỳ vọng giảm dần khi lãi suất chiết khấu tăng** thể hiện mối quan hệ nghịch chiều giữa chiết khấu và giá trị hiện tại ròng.

- **Khi lãi suất tăng từ 1% lên 10%, NPV trung bình giảm mạnh** từ khoảng 6.9 triệu xuống còn 2.3 triệu, tức giảm khoảng 66%.

- **Dù giảm theo lãi suất, NPV vẫn dương trong toàn bộ khoảng 1%–10%** cho thấy dự án giữ được hiệu quả tài chính trong nhiều điều kiện lãi suất khác nhau.

- **Sự thay đổi lớn về NPV cũng nhấn mạnh rằng hiệu quả tài chính của chương trình khá nhạy với giả định chiết khấu** đặc biệt trong môi trường lạm phát cao hoặc chi phí vốn tăng.

Tóm lại, chương trình vẫn khả thi về mặt tài chính trong nhiều kịch bản, nhưng cần cân nhắc kỹ khi giả định mức lãi suất chiết khấu để phản ánh đúng bối cảnh kinh tế thực tế.

# Các giả định

Phân tích tài chính của chương trình được xây dựng dựa trên các giả định chính sau:

- **Hiệu quả can thiệp:**  Mức giảm tỷ lệ tử vong và chi phí y tế do các can thiệp được sử dụng từ bảng mortality_impact, giả định là ổn định và phản ánh đúng hiệu quả thực tế trong từng năm triển khai.

- **Thời gian triển khai:** Chương trình bắt đầu từ năm 2024 và kéo dài đến năm 2054 (31 năm), với lịch trình triển khai từng nhóm can thiệp đã được xác định từ trước.

- **Dữ liệu khách hàng:**  Cấu trúc dân số, đặc điểm nhân khẩu học và hồ sơ bảo hiểm được giữ nguyên như trong bộ dữ liệu gốc của năm 2024. Không xem xét các yếu tố như tăng trưởng dân số, biến động hành vi hoặc thay đổi cấu trúc hợp đồng bảo hiểm trong tương lai.

- **Chi phí của từng can thiệp:** Được giả định là chi phí phát sinh đầu năm (nghĩa là nếu khách hàng có yêu cầu bổi thương trong năm thì chi phí bỏ ra không bị ảnh hưởng) nhưng lại được tính toán vào cuối năm.

- **Khoản tiết kiệm:** được tính dựa trên số lượng khách hàng tránh được tử vong và khoản chi trả bảo hiểm tương ứng (face amount) không phát sinh.

- **Giá trị hiện tại ròng (NPV):** Dòng tiền tiết kiệm và chi phí được quy đổi về hiện tại theo các mức tỷ lệ chiết khấu (discount rate) khác nhau, với giá trị mặc định là 5%.

- **Không tính yếu tố ngoại sinh:** Các yếu tố kinh tế vĩ mô như lạm phát y tế, thay đổi luật pháp, đại dịch bất ngờ, hoặc biến động chi phí y tế không được đưa vào mô hình.

- **Sự độc lập của can thiệp:** Mỗi can thiệp được giả định là hoạt động độc lập, không có sự chồng lấn hiệu quả hoặc ảnh hưởng tương hỗ giữa các chương trình.
