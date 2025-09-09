import cv2
import matplotlib.pyplot as plt
import numpy as np
import cv2
import numpy as np


class ZebrafishAnalyzer:
    def __init__(self, min_area=6000, max_area=10000, window_width=50):
        """
        初始化斑马鱼分析器
        参数:
            min_area: 最小轮廓面积阈值
            max_area: 最大轮廓面积阈值
            window_width: 两侧检测窗口宽度
        """
        self.min_area = min_area
        self.max_area = max_area
        self.window_width = window_width
        self.direction = None  # 1: 左, 2: 右

    def analyze_contours(self, gray_img, contours):
        """
        分析轮廓并判断方向
        参数:
            gray_img: 灰度图像
            contours: 检测到的轮廓列表
        返回:
            绘制了检测框的图像
        """
        output_img = cv2.cvtColor(gray_img, cv2.COLOR_GRAY2BGR)

        for contour in contours:
            area = cv2.contourArea(contour)

            if self.min_area < area < self.max_area:
                # 获取边界框
                x, y, w, h = cv2.boundingRect(contour)
                cv2.rectangle(output_img, (x, y), (x + w, y + h), (0, 255, 0), 2)

                # 计算两侧检测区域
                left_box_x = max(0, x - self.window_width)
                right_box_x = min(gray_img.shape[1], x + w + 1)

                # 提取左右检测区域
                left_box = gray_img[y:y + h, left_box_x:left_box_x + self.window_width]
                right_box = gray_img[y:y + h, right_box_x:right_box_x + self.window_width]

                # 绘制检测窗口
                cv2.rectangle(output_img,
                              (left_box_x, y),
                              (left_box_x + self.window_width, y + h),
                              (255, 0, 0), 1)
                cv2.rectangle(output_img,
                              (right_box_x, y),
                              (right_box_x + self.window_width, y + h),
                              (0, 0, 255), 1)

                # 计算平均灰度值
                left_avg = np.mean(left_box) if left_box.size > 0 else 0
                right_avg = np.mean(right_box) if right_box.size > 0 else 0

                # 判断方向
                self._determine_direction(left_avg, right_avg)

                # 在图像上标注信息
                self._annotate_image(output_img, x, y, area, left_avg, right_avg)

        return output_img

    def _determine_direction(self, left_avg, right_avg):
        """根据灰度值判断方向"""
        if left_avg > right_avg + 10:  # 添加10的容差阈值
            self.direction = 1
            print("方向: 向左 ←")
        elif right_avg > left_avg + 10:
            self.direction = 2
            print("方向: 向右 →")
        else:
            self.direction = 0
            print("方向: 不确定")

    def _annotate_image(self, img, x, y, area, left_avg, right_avg):
        """在图像上标注分析结果"""
        cv2.putText(img, f"Area: {area:.0f}", (x, y - 10),
                    cv2.FONT_HERSHEY_SIMPLEX, 0.5, (0, 255, 0), 1)
        cv2.putText(img, f"L: {left_avg:.1f}", (x, y + h + 20),
                    cv2.FONT_HERSHEY_SIMPLEX, 0.5, (255, 0, 0), 1)
        cv2.putText(img, f"R: {right_avg:.1f}", (x, y + h + 40),
                    cv2.FONT_HERSHEY_SIMPLEX, 0.5, (0, 0, 255), 1)

        if self.direction == 1:
            cv2.putText(img, "LEFT", (x, y - 30),
                        cv2.FONT_HERSHEY_SIMPLEX, 0.7, (255, 255, 0), 2)
        elif self.direction == 2:
            cv2.putText(img, "RIGHT", (x, y - 30),
                        cv2.FONT_HERSHEY_SIMPLEX, 0.7, (255, 255, 0), 2)


def main():
    # 初始化摄像头
    cap = cv2.VideoCapture(0)
    analyzer = ZebrafishAnalyzer(min_area=6000, max_area=10000, window_width=50)

    while True:
        ret, frame = cap.read()
        if not ret:
            break

        # 转换为灰度图
        gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)

        # 二值化处理 (可替换为自适应阈值)
        _, binary = cv2.threshold(gray, 127, 255, cv2.THRESH_BINARY_INV)

        # 查找轮廓
        contours, _ = cv2.findContours(binary, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)

        # 分析轮廓
        result_img = analyzer.analyze_contours(gray, contours)

        # 显示结果
        cv2.imshow('Original', frame)
        cv2.imshow('Binary', binary)
        cv2.imshow('Analysis', result_img)

        if cv2.waitKey(30) & 0xFF == ord('q'):
            break

    cap.release()
    cv2.destroyAllWindows()


if __name__ == "__main__":
    main()