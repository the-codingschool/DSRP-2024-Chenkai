# ANOVA for DwellType
anova_dwelltype <- aov(OverallQual ~ DwellType, data = train_cleaned)
summary(anova_dwelltype)
tukey_dwelltype <- TukeyHSD(anova_dwelltype)
print(tukey_dwelltype)

# ANOVA for ZoneClass
anova_zoneclass <- aov(OverallQual ~ ZoneClass, data = train_cleaned)
summary(anova_zoneclass)
tukey_zoneclass <- TukeyHSD(anova_zoneclass)
print(tukey_zoneclass)

# ANOVA for LotArea
anova_lotarea <- aov(OverallQual ~ LotArea, data = train_cleaned)
summary(anova_lotarea)