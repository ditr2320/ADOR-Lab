# Simple univariate model UNI 
!qiime songbird multinomial \
--i-table mm_seqs_noblank_mom_6m.qza \
--m-metadata-file songbirdData_Av.txt \
--p-formula "MDS_Mom" \
--p-epochs 10000 \
--p-differential-prior 0.5 \
--p-summary-interval 1 \
--o-differentials mm_differentials-aMDS_6m.qza \
--o-regression-stats mm_regression-aMDS_6m.qza \
--o-regression-biplot mm_biplot-aMDS_6m.qza

# Simple univariate model MULTI
!qiime songbird multinomial \
--i-table mm_seqs_noblank_mom_6m.qza \
--m-metadata-file songbirdData_Av.txt \
--p-formula "MDS_Mom+mom_BMI+mother_age+m_ener_Mom+exercise_r+SES_r" \
--p-epochs 10000 \
--p-differential-prior 0.5 \
--p-summary-interval 1 \
--o-differentials mm_differentials-aMDS_6m_full.qza \
--o-regression-stats mm_regression-aMDS_6m_full.qza \
--o-regression-biplot mm_biplot-aMDS_6m_full.qza

# NULL model UNI
!qiime songbird multinomial \
--i-table mm_seqs_noblank_mom_6m.qza \
--m-metadata-file songbirdData_Av.txt \
--p-formula "1" \
--p-epochs 10000 \
--p-differential-prior 0.5 \
--p-summary-interval 1 \
--o-differentials null-diff-aMDS_6m.qza \
--o-regression-stats null-stats-aMDS_6m.qza \
--o-regression-biplot null-biplot-aMDS_6m.qza

#UNI 
!qiime songbird summarize-paired \
--i-regression-stats mm_regression-aMDS_6m.qza \
--i-baseline-stats null-stats-aMDS_6m.qza \
--o-visualization paired-summary-uni-vs-null-aMDS_6m.qzv

#MULTI
!qiime songbird summarize-paired \
--i-regression-stats mm_regression-aMDS_6m_full.qza \
--i-baseline-stats null-stats-aMDS_6m.qza \
--o-visualization paired-summary-multi-vs-null-aMDS_6m_full.qzv

# Visualize using qurro 
#UNI
!qiime qurro differential-plot \
--i-table mm_seqs_noblank_mom_6m.qza \
--i-ranks mm_differentials-aMDS_6m.qza \
--m-sample-metadata-file songbirdData_Av.txt \
--m-feature-metadata-file deblur-sequences-taxonomy-MM.qza \
--o-visualization qurro-plot-aMDS_6m.qzv
#MULTI
!qiime qurro differential-plot \
--i-table mm_seqs_noblank_mom_6m.qza \
--i-ranks mm_differentials-aMDS_6m_full.qza \
--m-sample-metadata-file songbirdData_Av.txt \
--m-feature-metadata-file deblur-sequences-taxonomy-MM.qza \
--o-visualization qurro-plot-aMDS_6m_full.qzv

# Visulaizing biplot in emperor
#UNI
!qiime emperor biplot \
--i-biplot mm_biplot-aMDS_6m.qza \
--m-sample-metadata-file deblur-sequences-taxonomy-MM.qza \
--p-ignore-missing-samples \
--p-number-of-features 2 \
--o-visualization emperor-biplot-aMDS_6m
#MULTI
!qiime emperor biplot \
--i-biplot mm_biplot-aMDS_6m_full.qza \
--m-sample-metadata-file deblur-sequences-taxonomy-MM.qza \
--p-ignore-missing-samples \
--p-number-of-features 2 \
--o-visualization emperor-biplot-aMDS_6m_full

# Output differentials from Songbird
!qiime tools export \
--input-path mm_differentials-aMDS_6m.qza \
--output-path mm_differentials-aMDS_6m
!qiime tools export \
--input-path mm_differentials-aMDS_6m_full.qza \
--output-path mm_differentials-aMDS_6m_full