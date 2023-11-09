# probReview.r	
#
# R examples for probability concepts chapter
#
# Core R functions used:
#

# R packages/functions used
#
# mvtnorm
#   dmvnorm             density of multivariate normal
#   pmvnorm             CDF of multivariate normal
#   qmvnorm             quantiles of multivariate normal
#   rmvnorm             simulate random numbers from multivariate normal

# scatterplot3D
options(digits = 4)


#
# General normal distribution
#

# Example: R ~ N(0.01, 0.10)
mu.r = 0.01
sd.r = 0.1
x.vals = seq(-4, 4, length=150)*sd.r + mu.r
plot(x.vals, dnorm(x.vals, mean=mu.r, sd=sd.r), type="l", lwd=2, 
     col="blue", xlab="x", ylab="pdf")
pnorm(-0.5, mean=0.01, sd=0.1)
pnorm(0, mean=0.01, sd=0.1)
1 - pnorm(0.5, mean=0.01, sd=0.1)
1 - pnorm(1, mean=0.01, sd=0.1)

a.vals = c(0.01, 0.05, 0.95, 0.99)
qnorm(a.vals, mean=0.01, sd=0.10)


# Example: risk-return tradeoff
mu.r = 0.02
sd.r = 0.10
x.vals = seq(-3, 3, length=150)*sd.r + mu.r
plot(x.vals, dnorm(x.vals, mean=mu.r, sd=sd.r), type="l", lwd=2, 
     ylim=c(0, max(dnorm(x.vals, mean=0.01, sd=0.05))),
     col="black", xlab="x", ylab="pdf")
points(x.vals, dnorm(x.vals, mean=0.01, sd=0.05), type="l", lwd=2,
       col="blue", lty="dotted")
segments(0.02, 0, 0.02, dnorm(0.02, mean=0.02, sd=0.1), lwd=2)
segments(0.01, 0, 0.01, dnorm(0.01, mean=0.01, sd=0.05), lwd=2, 
         col="blue", lty="dotted")
legend(x="topleft", legend=c("Amazon", "Boeing"), lwd=2,
       col=c("black", "blue"), lty=c("solid","dotted"))
                   

mu_x <- 0.05  # 마이크로소프트
sigma_x <- 0.10
mu_y <- 0.025  # 스타벅스
sigma_y <- 0.05

# x 값 범위 설정
x_vals <- seq(from = -0.25, to = 0.35, length.out = 150)

# 마이크로소프트 주식의 정규 분포 곡선 그리기
plot(x_vals, dnorm(x_vals, mean = mu_x, sd = sigma_x), type = "l", lwd = 2,
     ylim = c(0, max(dnorm(x_vals, mean = mu_x, sd = sigma_x))),
     col = "black", xlab = "월간 수익률", ylab = "확률 밀도 함수(pdf)")

# 스타벅스 주식의 정규 분포 곡선 추가
points(x_vals, dnorm(x_vals, mean = mu_y, sd = sigma_y), type = "l", lwd = 2,
       col = "blue", lty = "dotted")

# 평균 수익률에 대한 수직선 추가
segments(mu_x, 0, mu_x, dnorm(mu_x, mean = mu_x, sd = sigma_x), lwd = 2)
segments(mu_y, 0, mu_y, dnorm(mu_y, mean = mu_y, sd = sigma_y), lwd = 2, col = "blue", lty = "dotted")

# 범례 추가
legend("topright", legend = c("Microsoft(X)", "Starbucks(Y)"), lwd = 2, col = c("black", "blue"), lty = c(1, 3))

# 그리드 추가
grid()

#2번
# R ~ N(0.04, (0.09)^2), W0 = $100,000
mu_R <- 0.04  # 예상 수익률
sigma_R <- 0.09  # 수익률의 표준편차
W0 <- 100000  # 초기 투자 금액

# 1% VaR 계산
VaR_01 <- W0 * (mu_R + sigma_R * qnorm(0.01))

# 5% VaR 계산
VaR_05 <- W0 * (mu_R + sigma_R * qnorm(0.05))

# 결과 출력
print(VaR_01)
print(VaR_05)


##3번-1
# r ~ iidN(0.04, (0.09)^2), W0 = $100,000
mu_r <- 0.04  # 예상 연속 복리 수익률
sigma_r <- 0.09  # 수익률의 표준편차
W0 <- 100000  # 초기 투자 금액

# 1% 및 5% 분위수 계산
q_01_r <- qnorm(0.01, mean = mu_r, sd = sigma_r)
q_05_r <- qnorm(0.05, mean = mu_r, sd = sigma_r)

# 연속 복리 수익률 분위수를 단순 수익률 분위수로 변환
R_01 <- exp(q_01_r) - 1
R_05 <- exp(q_05_r) - 1

# VaR 계산
VaR_01 <- W0 * R_01
VaR_05 <- W0 * R_05

# 결과 출력
print(VaR_01)
print(VaR_05)


##3번-2


# 연간 수익률의 정규 분포 파라미터 계산
mu_annual_r <- 12 * mu_r  # 월간 평균 수익률을 12배로 증가
sigma_annual_r <- sigma_r * sqrt(12)  # 월간 표준편차를 sqrt(12)배로 증가

# 연간 1% 및 5% 분위수 계산
q_01_annual_r <- qnorm(0.01, mean = mu_annual_r, sd = sigma_annual_r)
q_05_annual_r <- qnorm(0.05, mean = mu_annual_r, sd = sigma_annual_r)

# 연속 복리 수익률 분위수를 단순 수익률 분위수로 변환
R_01_annual <- exp(q_01_annual_r) - 1
R_05_annual <- exp(q_05_annual_r) - 1

# 연간 VaR 계산
VaR_01_annual <- W0 * R_01_annual
VaR_05_annual <- W0 * R_05_annual

# 결과 출력
print(VaR_01_annual)
print(VaR_05_annual)


#
# Value-at-Risk calculations
#

# R(t) ~ N(0.05, (0.10)^2)
# W = 10000
w0 = 10000
# plot return and wealth distributions
mu.R = 0.05
sd.R = 0.1
R.vals = seq(from=(mu.R - 3*sd.R), to=(mu.R + 3*sd.R), length = 100)
mu.w1 = 10500
sd.w1 = 1000
w1.vals = seq(from=(mu.w1 - 3*sd.w1), to=(mu.w1 + 3*sd.w1), length = 100)

par(mfrow=c(2,1))
# plot return density
plot(R.vals, dnorm(R.vals, mean=mu.R, sd=sd.R), type="l", 
     main="R(t) ~ N(0.05,(.10)^2)", xlab="R", ylab="pdf",
     lwd=2, col="blue")
# plot wealth density
plot(w1.vals, dnorm(w1.vals, mean=mu.w1, sd=sd.w1), type="l", 
     main="W1 ~ N(10,500,(1,000)^2)", xlab="W1", ylab="pdf",
     lwd=2, col="blue")
par(mfrow=c(1,1))



# Pr(W1 < 9000)
pnorm(9000, mu.w1, sd.w1)
qnorm(pnorm(9000, mu.w1, sd.w1), mu.w1, sd.w1)

# compute 5% quantile of return and wealth distributions
q.R.05 = qnorm(0.05, mu.R, sd.R)
q.R.05
q.w1.05 = qnorm(0.05, mu.w1, sd.w1)
q.w1.05

# compute 5% VaR using return quantile
w0*q.R.05

# compute 5% VaR using wealth quantile
q.w1.05 - w0

# plot return and loss distributions with VaR
loss.vals = w0*R.vals
mu.loss = w0*mu.R
sd.loss = w0*sd.R
par(mfrow=c(2,1))
# plot return density
plot(R.vals, dnorm(R.vals, mean=mu.R, sd=sd.R), type="l", 
     main="R(t) ~ N(0.05,(.10)^2)", xlab="R", ylab="pdf",
     lwd=2, col="blue")
abline(v=q.R.05, lwd=2, col="red")     
# plot wealth density
plot(loss.vals, dnorm(loss.vals, mean=mu.loss, sd=sd.loss), type="l", 
     main="R*W0 ~ N(500,(1,000)^2)", xlab="W0*R", ylab="pdf",
     lwd=2, col="blue")
abline(v=q.R.05*w0, lwd=2, col="red")       
par(mfrow=c(1,1))


# VaR example
mu.R = 0.05
sd.R = 0.10
w0 = 10000
q.01.R = mu.R + sd.R*qnorm(0.01)
q.05.R = mu.R + sd.R*qnorm(0.05)
VaR.01 = abs(q.01.R*w0)
VaR.05 = abs(q.05.R*w0)
VaR.01
VaR.05

mu.r = 0.05
sd.r = 0.10
q.01.R = exp(mu.r + sd.r*qnorm(0.01)) - 1
q.05.R = exp(mu.r + sd.r*qnorm(0.05)) - 1
VaR.01 = abs(q.01.R*w0)
VaR.05 = abs(q.05.R*w0)
VaR.01
VaR.05

