项目概况：
提供用户在2016年1月1日至2016年6月30日之间真实线上线下消费行为，预测用户在2016年7月领取优惠券后15天以内是否核销。评测指标采用AUC，先对每个优惠券单独计算核销预测的AUC值，再对所有优惠券的AUC值求平均作为最终的评价标准。
解决方案概述：首先对数据集进行划分，然后提取了用户相关的特征、商家相关的特征，优惠劵相关的特征，对比了各个特征对优惠券使用率的影响。最后训练了逻辑回归模型进行预测。
