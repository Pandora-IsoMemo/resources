pasteLines <- function(...) {
  paste(..., sep = "\n")
}

# test_that("Parse Curve File", {
#     rawContent <- readLines("oxcal_aquatic_curve.txt")
#
#     curveDefinitions <- parseCurveFile(rawContent)
#
#     expectedDefinitions <- list(
#         list(
#             title = "Marine20",
#             formula = c(
#               'Curve("Marine20","Marine20.14c");',
#               'Delta_R("Aquatic2",%%Delta_R_2%%,%%Delta_R_SD_2%%);'
#             ),
#             mixture = list(
#               'Mix_Curve(%%TARGET_ID%%,"Aquatic1","Aquatic2", %%MEAN_B%%,%%SD_B%%);',
#               'Mix_Curve(%%TARGET_ID%%,"Aquatic1","Aquatic2", P(0,100,[0,%%BINS_B%%,0]));'
#             )
#         ),
#         list(
#             title = "IntCal20",
#             formula = c(
#               'Curve("IntCal20","IntCal20.14c");',
#               'Delta_R("Aquatic2",%%Delta_R_2%%,%%Delta_R_SD_2%%);'
#             ),
#             mixture = list(
#               'Mix_Curve(%%TARGET_ID%%,"Aquatic1","Aquatic2", %%MEAN_B%%,%%SD_B%%);',
#               'Mix_Curve(%%TARGET_ID%%,"Aquatic1","Aquatic2", P(0,100,[0,%%BINS_B%%,0]));'
#             )
#         )
#     )
#
#     expect_equal(curveDefinitions, expectedDefinitions)
# })

test_that("getCurveTitlesXlsx", {
  file1 <- read.xlsx("https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/b7732618-7764-460a-b1fa-c614f4cdbe95/download/terrestrial.xlsx")
  file2 <- read.xlsx("https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/2037632f-f984-4834-8e25-4af5498df163/download/aquatic1.xlsx")
  file3 <- read.xlsx("https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/120d810e-ff7d-49b7-80b8-e9791e2980b3/download/aquatic2.xlsx")

  expect_equal(getCurveTitlesXlsx(file1), structure(1:3, .Names = c("IntCal20", "SHCal20", "Mix IntCal20 andSHCal20")))
  expect_equal(getCurveTitlesXlsx(file2), structure(1:2, .Names = c("Marine20", "Same as terrestrial")))
  expect_equal(getCurveTitlesXlsx(file3), structure(1:2, .Names = c("Marine20", "Same as terrestrial")))
})

test_that("getCodeTerrestrial", {
  file <- read.xlsx(
    "https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/b7732618-7764-460a-b1fa-c614f4cdbe95/download/terrestrial.xlsx"
  )

  expect_equal(
    getCodeTerrestrial(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[1]), ],
      mixOption = NULL,
      mixParams = NULL
    ),
    "Curve(\"terrestrial\",\"IntCal20.14c\");"
  )

  expect_equal(
    getCodeTerrestrial(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[2]), ],
      mixOption = NULL,
      mixParams = NULL
    ),
    "Curve(\"terrestrial\",\"SHCal20.14c\");"
  )

  expect_equal(
    getCodeTerrestrial(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[3]), ],
      mixOption = "Option point",
      mixParams = c(3, 0)
    ),
    "Curve(\"IntCal20\",\"IntCal20.14c\");\r\nCurve(\"SHCal20\",\"SHCal20.14c\");\r\nMix_Curves(\"terrestrial\",\"IntCal20\",\"SHCal20\", 3);"
  )

  expect_equal(
    getCodeTerrestrial(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[3]), ],
      mixOption = "Option Mean SD",
      mixParams = c(2, 1)
    ),
    "Curve(\"IntCal20\",\"IntCal20.14c\");\r\nCurve(\"SHCal20\",\"SHCal20.14c\");\r\nMix_Curves(\"terrestrial\",\"IntCal20\",\"SHCal20\", 2,1);"
  )

  expect_equal(
    getCodeTerrestrial(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[3]), ],
      mixOption = "Option uniform",
      mixParams = c(0, 2)
    ),
    "Curve(\"IntCal20\",\"IntCal20.14c\");\r\nCurve(\"SHCal20\",\"SHCal20.14c\");\r\nMix_Curves(\"terrestrial\",\"IntCal20\",\"SHCal20\", U(0,2));"
  )
})

test_that("getCodeAquatic", {
  file <- read.xlsx(
    "https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/2037632f-f984-4834-8e25-4af5498df163/download/aquatic1.xlsx"
  )

  expect_equal(
    getCodeAquatic(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[1]), ],
      binOption = "Option Mean SD",
      deltaRParams = c(2, 1)
    ),
    "Curve(\"Marine20\",\"Marine20.14c\");\r\nDelta_R(\"Aquatic1\",2,1);\nMix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", %%MEAN%%,%%SD%%);\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);"
  )

  expect_equal(
    getCodeAquatic(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[1]), ],
      binOption = "Option PDF",
      deltaRParams = c(2, 1)
    ),
    "Curve(\"Marine20\",\"Marine20.14c\");\r\nDelta_R(\"Aquatic1\",2,1);\nMix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", P(0,100,[0,%%BINS%%,0]));\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);"
  )

  expect_equal(
    getCodeAquatic(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[2]), ],
      binOption = "Option Mean SD",
      deltaRParams = c(2, 1)
    ),
    "Curve(\"terrestrial\");\r\nDelta_R(\"Aquatic1\",2,1);\nMix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", %%MEAN%%,%%SD%%);\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);"
  )

  expect_equal(
    getCodeAquatic(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[2]), ],
      binOption = "Option PDF",
      deltaRParams = c(2, 1)
    ),
    "Curve(\"terrestrial\");\r\nDelta_R(\"Aquatic1\",2,1);\nMix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", P(0,100,[0,%%BINS%%,0]));\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);"
  )
})

test_that("Create OxCal Output", {
  model <- readRDS("test-oxcalData.rds")
  basicCode <-
    c(
      "Plot()",
      "{",
      "%%Terrestrial_curve_VAR1%%",
      "%%Aquatic_curve_1_VAR1%%",
      "%%Aquatic_curve_2_VAR1%%",
      "",
      "%%String_from_loop%%",
      "",
      "",
      "};"
    )
  terrestrialCurves <- list(
    list(
      title = "IntCal20",
      formula = "Curve(\"terrestrial\",\"IntCal20.14c\");",
      mixture = list()
    ),
    list(
      title = "SHCal20",
      formula = "Curve(\"terrestrial\",\"SHCal20.14c\");",
      mixture = list()
    ),
    list(
      title = "Known mix IntCal20 andSHCal20 - must give mix values in text",
      formula = c(
        "Curve(\"IntCal20\",\"IntCal20.14c\");",
        "Curve(\"SHCal20\",\"SHCal20.14c\");",
        "Mix_Curves(\"terrestrial\",\"IntCal20\",\"SHCal20\", Mean,SD);"
      ),
      mixture = list()
    ),
    list(
      title = "Unknown mix IntCal20 andSHCal20",
      formula = c(
        "Curve(\"IntCal20\",\"IntCal20.14c\");",
        "Curve(\"SHCal20\",\"SHCal20.14c\");",
        "Mix_Curves(\"terrestrial\",\"IntCal20\",\"SHCal20\", U(0,100));"
      ),
      mixture = list()
    )
  )

  aquaticCurves1 <- list(
    list(
      title = "Marine20",
      formula = c(
        "Curve(\"Marine20\",\"Marine20.14c\");",
        "Delta_R(\"Aquatic1\",%%Delta_R_1%%,%%Delta_R_SD_1%%);"
      ),
      mixture = list(
        c(
          "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", %%MEAN%%,%%SD%%);",
          "R_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);"
        ),
        c(
          "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", P(0,100,[0,%%BINS%%,0]));",
          "R_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);"
        )
      )
    ),
    list(
      title = "IntCal20",
      formula = c(
        "Curve(\"IntCal20\",\"IntCal20.14c\");",
        "Delta_R(\"Aquatic1\",%%Delta_R_1%%,%%Delta_R_SD_1%%);"
      ),
      mixture = list(
        "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", %%MEAN%%,%%SD%%);",
        "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", P(0,100,[0,%%BINS%%,0]));"
      )
    ),
    list(
      title = "SHCal20",
      formula = c(
        "Curve(\"SHCal20\",\"SHCal20.14c\");",
        "Delta_R(\"Aquatic1\",%%Delta_R_1%%,%%Delta_R_SD_1%%);"
      ),
      mixture = list(
        "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", %%MEAN%%,%%SD%%);",
        "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", P(0,100,[0,%%BINS%%,0]));"
      )
    ),
    list(
      title = "Known mix IntCal20 andSHCal20 - must give mix values in text",
      formula = c(
        "Curve(\"IntCal20\",\"IntCal20.14c\");",
        "Curve(\"SHCal20\",\"SHCal20.14c\");",
        "Mix_Curves(\"mixed1\",\"IntCal20\",\"SHCal20\", Mean,SD);",
        "Delta_R(\"Aquatic1\",%%Delta_R_1%%,%%Delta_R_SD_1%%);"
      ),
      mixture = list(
        "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", %%MEAN%%,%%SD%%);",
        "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", P(0,100,[0,%%BINS%%,0]));"
      )
    ),
    list(
      title = "Unknown mix IntCal20 andSHCal20",
      formula = c(
        "Curve(\"IntCal20\",\"IntCal20.14c\");",
        "Curve(\"SHCal20\",\"SHCal20.14c\");",
        "Mix_Curves(\"mixed1\",\"IntCal20\",\"SHCal20\", U(0,100));",
        "Delta_R(\"Aquatic1\",%%Delta_R_1%%,%%Delta_R_SD_1%%);"
      ),
      mixture = list(
        "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", %%MEAN%%,%%SD%%);",
        "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", P(0,100,[0,%%BINS%%,0]));"
      )
    )
  )

  aquaticCurves2 <- list(
    list(
      title = "Marine20",
      formula = c(
        "Curve(\"Marine20\",\"Marine20.14c\");",
        "Delta_R(\"Aquatic2\",%%Delta_R_2%%,%%Delta_R_SD_2%%);"
      ),
      mixture = list(
        "Mix_Curve(%%TARGET_ID%%,\"Aquatic1\",\"Aquatic2\", %%MEAN_B%%,%%SD_B%%);",
        "Mix_Curve(%%TARGET_ID%%,\"Aquatic1\",\"Aquatic2\", P(0,100,[0,%%BINS_B%%,0]));"
      )
    ),
    list(
      title = "IntCal20",
      formula = c(
        "Curve(\"IntCal20\",\"IntCal20.14c\");",
        "Delta_R(\"Aquatic2\",%%Delta_R_2%%,%%Delta_R_SD_2%%);"
      ),
      mixture = list(
        "Mix_Curve(%%TARGET_ID%%,\"Aquatic1\",\"Aquatic2\", %%MEAN_B%%,%%SD_B%%);",
        "Mix_Curve(%%TARGET_ID%%,\"Aquatic1\",\"Aquatic2\", P(0,100,[0,%%BINS_B%%,0]));"
      )
    ),
    list(
      title = "SHCal20",
      formula = c(
        "Curve(\"SHCal20\",\"SHCal20.14c\");",
        "Delta_R(\"Aquatic2\",%%Delta_R_2%%,%%Delta_R_SD_2%%);"
      ),
      mixture = list(
        "Mix_Curve(%%TARGET_ID%%,\"Aquatic1\",\"Aquatic2\", %%MEAN_B%%,%%SD_B%%);",
        "Mix_Curve(%%TARGET_ID%%,\"Aquatic1\",\"Aquatic2\", P(0,100,[0,%%BINS_B%%,0]));"
      )
    ),
    list(
      title = "Known mix IntCal20 andSHCal20 - must give mix values in text",
      formula = c(
        "Curve(\"IntCal20\",\"IntCal20.14c\");",
        "Curve(\"SHCal20\",\"SHCal20.14c\");",
        "Mix_Curves(\"mixed2\",\"IntCal20\",\"SHCal20\", Mean,SD);",
        "Delta_R(\"Aquatic2\",%%Delta_R_2%%,%%Delta_R_SD_2%%);"
      ),
      mixture = list(
        "Mix_Curve(%%TARGET_ID%%,\"Aquatic1\",\"Aquatic2\", %%MEAN_B%%,%%SD_B%%);",
        "Mix_Curve(%%TARGET_ID%%,\"Aquatic1\",\"Aquatic2\", P(0,100,[0,%%BINS_B%%,0]));"
      )
    ),
    list(
      title = "Unknown mix IntCal20 andSHCal20",
      formula = c(
        "Curve(\"IntCal20\",\"IntCal20.14c\");",
        "Curve(\"SHCal20\",\"SHCal20.14c\");",
        "Mix_Curves(\"mixed2\",\"IntCal20\",\"SHCal20\", U(0,100));",
        "Delta_R(\"Aquatic2\",%%Delta_R_2%%,%%Delta_R_SD_2%%);"
      ),
      mixture = list(
        "Mix_Curve(%%TARGET_ID%%,\"Aquatic1\",\"Aquatic2\", %%MEAN_B%%,%%SD_B%%);",
        "Mix_Curve(%%TARGET_ID%%,\"Aquatic1\",\"Aquatic2\", P(0,100,[0,%%BINS_B%%,0]));"
      )
    )
  )

  oxCalA <- "Source contributions_Carnivores"
  oxCalB <- "Source contributions_Fish2"
  exportCoordinates <- structure(
    c(45, 50, 55, 56, 43, 0, 5, 10, 6, 16, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    .Dim = 5:4,
    .Dimnames = list(
      c(
        "Individual_1",
        "Individual_2",
        "Individual_3",
        "Individual_4",
        "Individual_5"
      ),
      c(
        "longitude",
        "latitude",
        "LowerLimit/Mean/Point",
        "UpperLimit/SD"
      )
    )
  )

  oxcalLines1 <- createOxCalText(
    model = model,
    basicCode = basicCode,
    terrestrialCurve = terrestrialCurves[1],
    aquaticCurve1 = aquaticCurves1[1],
    aquaticCurve2 = aquaticCurves2[1],
    OxCalA = oxCalA,
    meanDeltaR1 = 0,
    sdDeltaR1 = 1,
    OxCalB = oxCalB,
    meanDeltaR2 = 2,
    sdDeltaR2 = 0.5,
    bins = "meansd",
    coordinates = exportCoordinates
  ) %>%
    strsplit(split = "\n") %>%
    unlist()

  oxcalLines2 <- createOxCalText(
    model = model,
    basicCode = basicCode,
    terrestrialCurve = terrestrialCurves[1],
    aquaticCurve1 = aquaticCurves1[1],
    aquaticCurve2 = aquaticCurves2[1],
    OxCalA = oxCalA,
    meanDeltaR1 = 0,
    sdDeltaR1 = 1,
    OxCalB = oxCalB,
    meanDeltaR2 = 2,
    sdDeltaR2 = 0.5,
    bins = "bins",
    coordinates = exportCoordinates
  ) %>%
    strsplit(split = "\n") %>%
    unlist()

  expect_equal(oxcalLines1[1], "Plot()")
  expect_equal(oxcalLines1[3], "Curve(\"terrestrial\",\"IntCal20.14c\");")
  expect_equal(oxcalLines1[4], "Curve(\"Marine20\",\"Marine20.14c\");")
  expect_equal(oxcalLines1[5], "Delta_R(\"Aquatic1\",0,1);")
  expect_equal(oxcalLines1[6], "Curve(\"Marine20\",\"Marine20.14c\");")
  expect_equal(oxcalLines1[7], "Delta_R(\"Aquatic2\",2,0.5);")
  expect_equal(
    oxcalLines1[9] %>% substr(start = 1, stop = 100),
    "Mix_Curve(Individual_1,\"terrestrial\",\"Aquatic1\", 0.511,0.29); R_Date(“Individual_1“, NA,NA);"
  )
  expect_equal(
    oxcalLines1[23] %>% substr(start = 1, stop = 100),
    "Mix_Curve(Individual_2,\"Aquatic1\",\"Aquatic2\", 0.087,0.102);"
  )

  expect_equal(oxcalLines2[1], "Plot()")
  expect_equal(oxcalLines2[3], "Curve(\"terrestrial\",\"IntCal20.14c\");")
  expect_equal(oxcalLines2[4], "Curve(\"Marine20\",\"Marine20.14c\");")
  expect_equal(oxcalLines2[5], "Delta_R(\"Aquatic1\",0,1);")
  expect_equal(oxcalLines2[6], "Curve(\"Marine20\",\"Marine20.14c\");")
  expect_equal(oxcalLines2[7], "Delta_R(\"Aquatic2\",2,0.5);")
  expect_equal(
    oxcalLines2[9] %>% substr(start = 1, stop = 100),
    "Mix_Curve(Individual_1,\"terrestrial\",\"Aquatic1\", P(0,100,[0,0, 0, 0.001, 0.002, 0.007, 0.013, 0.018,"
  )
  expect_equal(
    oxcalLines2[23] %>% substr(start = 1, stop = 100),
    "Mix_Curve(Individual_2,\"Aquatic1\",\"Aquatic2\", P(0,100,[0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.001, "
  )
})
