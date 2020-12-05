# stress test
library(tidyverse)
library(mgcv)
last_clip = function() .Last.value %>% clipr::write_clip()
# 신용 stress test ####
# stmon: 대상월, m1: 대상월의 전년동월 
# 기초데이터  
stmon = 202009
m1 = stmon - 100
# pseudo code
sql1 = "
select x.*, y.target
from 
  (select 기준월,상품코드,대출번호,대출일자
  ,대출잔액,연체회차,부실상태,bss
  from tbl1
  where 기준월in ({m1},{stmon})) x
left join
  (select 기준월,대출번호
  ,case when max(연체회차)>3 then 1 else 0 end) target
  from tbl2
  where months_between(기준월,기준월-12) between 1 and 12
    and 기준월 in ({m1},{stmon}) y
  on x.기준월=y.기준월
  and x.대출번호=y.대출번호
"
# 기부도: 4p이상 or 부실채권(실효포함)
# 기부도건 bss등급은 1년전 bss 적용 
crpd_now = odbc::dbGetQuery(con,sql1) %>% distinct()
crdf = crpd_now %>% 
  mutate(대상 = ifelse(ppd>=4|부실상태!='00','기부도','정상'))

# 시나리오1~3 bss별 부도율 산출
# 기부도건을 1년전 bss기준으로 실현된 부도율로 간주하고
# 시나리오1~3의 bss별 부도율에서 차감한다 
# 즉, 기부도건은 부도율에서 차감시킨 후 부도율 100%를 적용한다 

# 신용 시나리오1~3별 pd모형 
# 원본: 과거 시나리오 시점의 데이터를 이용하여 bss로 부도여부를 적합시킨 모델 
# bss버전 변경후에는 시나리오 시점의 bss데이터가 존재하지 않으므로
# 원본 모델로 산출한 부도확률을 target으로 하고 버전변경된 bss를 설명변수로 하여 
# gam으로 적합시킨 모델을 쓴다 
gam_cfit1 = read_rds('model/gam_cfit1.rds')
gam_cfit2 = read_rds('model/gam_cfit2.rds')
gam_cfit3 = read_rds('model/gam_cfit3.rds')
crdf$pd1 = predict(gam_cfit1,crdf,type='response')
crdf$pd2 = predict(gam_cfit2,crdf,type='response')
crdf$pd3 = predict(gam_cfit3,crdf,type='response')

# 기준월 부도율 추정 
fitd = crdf %>% 
  filter(기준월 == m1) %>% 
  filter(대상 == '정상')
fit0 = glm(target~bss,data=fitd,family='binomial')
crdf$pd0 = predict(fit0,crdf,type='response')

# 신용 LGD 산출 
# pseudo code
sql = "
select 기준월
,sum(충당금)/100000000 충당금
,sum(예상부도액)/100000000 예상부도액
from tbl3
group by 기준월 
"
lgd = odbc::dbGetQuery(con,sql)
lgd = lgd %>% mutate(lgd = 충당금/예상부도액)

# 담보 stress test ####
# 기초데이터 (생략)
# 지역별 낙찰가율: 인포케어에서 수집
# ltv data : 별도 산출 
# 과거 5년 가격변동율 추이
# kb아파트통계자료를 열고 아래와 같이 표부분 전체 복사
# '매매APT'시트: '구분'셀~기타지방 마지막 값있는 셀까지 선택후 복사 
pick = '전국|수도권|5개광역시|기타지방|서울|경기|인천|대구|부산|광주|
울산|대전|세종|전남|전북|경남|경북|충남|충북|강원|제주'
apt = clipr::read_clip_tbl() %>% 
  slice(-1,-2) %>% 
  select_at(vars(matches(pick))) %>% 
  select(-광주.1)
newname = map_chr(names(apt),~str_extract(.x,'(?![X5개])\\w+(?<=..)'))
# 아파트매매가격지수 
aptidx0 = pt %>% 
  select(전국,수도권,광역시,기타지방,서울,경기,인천,대구,부산,광주,울산,
         대전,세종,everything()) %>% 
  mutate_all(as.numeric) %>% 
  add_column(date = seq(ymd(19860101),length.out=nrow(.),by='month'),
             .before=1)
aptidx = aptidx0 %>% 
  filter(date <=lubridate::ymd(str_c(stmon,'01')))
# stresstest 대상월 이전 데이터만 남김 
# 월별 5년내 최고가격대비 하락률
aptret = aptidx %>% 
  mutate_at(vars(-date),~.RcppRoll:rol_meanr(.)-1) %>% 
  drop_na()
# stresstest 대상월기준 최근 5년내 최고가 대비 하락률
max_drawdown = aptret %>% 
  filter(date==max(date)) %>% 
  pivot_longer(cols=-date,names_to='시도',values_to='최고하락률') %>% 
  select(-date)
# 담보id별 집계(생략)
# 손실액 = max(0,담보가*낙찰가율*(1-하락률) - 선순위 )
# simulation
st_pattern_m = function(pds){
  require(furrr)
  # m: 대상년월
  # pds: 시나리오별 부도율 벡터 
  # data: 담보id기준 데이터 
  if(length(pds)<5){
    print('pds에 시나리오4 pd를 입력하시오')
  }
  mort_st = stltv1
  bal = sum(mort_st$원래잔액)/100000000
  기부도건수=sum(mort_st,대상=='기부도')
  기부도손실=filter(mort_st,대상=='기부도') %>% 
    summarise_at(vars(손실액0:손실액4),~sum(.)/100000000)
  mort_st = mort_st %>% 
    filter(대상=='정상') %>% 
    transmute_at(vars(손실액0:손실액4),~./100000000)
  # 부도율 입력 -> 부도건수 산출 -> 시나리오별 정상손실액+기부도손실액 
  # 부도율(pds): 맨마지막꺼 한번 더 써줘야 함 
  pdn = ceiling(pds*nrow(mort_st))-기부도건수 
  pdn = ifelse(pdn<0,1,pdn)
  # 부도건수별로 5000번 돌려 손실분포 만든 후 99퍼센타일 선택 
  lossdist = function(n,pdr){
    future_map_dfr(1:5000,~sample_n(mort_st,n) %>% 
                     summarise_all(sum)) %>% 
      summarise_all(~quantile(.,probs=.99)) %>% 
      bind_rows(기부도손실) %>% 
      summarise_all(sum) %>% 
      add_column(pd = pdr, .before=1)
  }
  # 부도건별로 실행
  els = future_map2_dfr(pdn, pds, ~lossdist(n=.x,pdr=.y))
  els = els %>% 
    mutate_at(vars(-pd),~./1.5) %>% # 연율화 
    mutate_at(vars(-pd),list(r = ~./bal)) %>% 
    add_column(mm = str_c(stmon),.before=1) %>% 
    mutate(seq = row_number()) %>% 
    mutate(손실액 = case_when(seq==1~손실액0,
                           seq==2~손실액1,
                           seq==3~손실액2,
                           seq==4~손실액3,
                           seq==5~손실액4),
           손실률 = case_when(seq==1~손실액0_r,
                           seq==2~손실액1_r,
                           seq==3~손실액2_r,
                           seq==4~손실액3_r,
                           seq==5~손실액4_r)) %>% 
    select(mm,pd,손실액,손실률) %>% 
    add_column(시나리오 = c('현수준','금융위기','중간수준','카드사태','극도악화'),
               .before=3)
  return(els)
}
pds = c(0.48,1.02,1.58,2.09,2.09)/100
sts_m = st_pattern_m(pds)
select(sts_m,시나리오,everything())
last_clip()