pasteLines <- function(...) {
  paste(..., sep = "\n")
}

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
    list(header = "Curve(\"Marine20\",\"Marine20.14c\");\r\nDelta_R(\"Aquatic1\",2,1);", 
         option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", %%MEAN%%,%%SD%%);\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")
  )

  expect_equal(
    getCodeAquatic(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[1]), ],
      binOption = "Option PDF",
      deltaRParams = c(2, 1)
    ),
    list(header = "Curve(\"Marine20\",\"Marine20.14c\");\r\nDelta_R(\"Aquatic1\",2,1);", 
         option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", P(0,100,[0,%%BINS%%,0]));\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")
  )

  expect_equal(
    getCodeAquatic(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[2]), ],
      binOption = "Option Mean SD",
      deltaRParams = c(2, 1)
    ),
    list(header = "Curve(\"terrestrial\");\r\nDelta_R(\"Aquatic1\",2,1);", 
         option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", %%MEAN%%,%%SD%%);\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")
  )

  expect_equal(
    getCodeAquatic(
      curve = file[as.numeric(getCurveTitlesXlsx(file)[2]), ],
      binOption = "Option PDF",
      deltaRParams = c(2, 1)
    ),
    list(header = "Curve(\"terrestrial\");\r\nDelta_R(\"Aquatic1\",2,1);", 
         option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", P(0,100,[0,%%BINS%%,0]));\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")
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
  
  terrestrialCurve <- 
    "Curve(\"IntCal20\",\"IntCal20.14c\");\r\nCurve(\"SHCal20\",\"SHCal20.14c\");\r\nMix_Curves(\"terrestrial\",\"IntCal20\",\"SHCal20\", 2,1);"
  
  aquaticCurve1 <- 
    list(header = "Curve(\"Marine20\",\"Marine20.14c\");\r\nDelta_R(\"Aquatic1\",2,1);", 
         option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", %%MEAN%%,%%SD%%);\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")

  aquaticCurve2 <- list(header = "Curve(\"terrestrial\");\r\nDelta_R(\"Aquatic1\",2,1);", 
                        option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", %%MEAN%%,%%SD%%);\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")

  oxCalA <- "Source contributions_Carnivores"
  oxCalB <- "Source contributions_Fish2"
  exportCoordinates <- structure(
    c(45, 50, 55, 56, 43, 0, 5, 10, 6, 16, 1001:1005, 1:5),
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
    terrestrialCurve = terrestrialCurve,
    aquaticCurve1 = aquaticCurve1,
    aquaticCurve2 = aquaticCurve2,
    OxCalA = oxCalA,
    OxCalB = oxCalB,
    bins = FALSE,
    coordinates = exportCoordinates
  ) %>%
    strsplit(split = "\n") %>%
    unlist()

  oxcalLines2 <- createOxCalText(
    model = model,
    basicCode = basicCode,
    terrestrialCurve = terrestrialCurve,
    aquaticCurve1 = aquaticCurve1,
    aquaticCurve2 = aquaticCurve2,
    OxCalA = oxCalA,
    OxCalB = oxCalB,
    bins = TRUE,
    coordinates = exportCoordinates
  ) %>%
    strsplit(split = "\n") %>%
    unlist()

  expect_equal(
    oxcalLines1,
    c(
      "Plot()",
      "{",
      "Curve(\"IntCal20\",\"IntCal20.14c\");\r",
      "Curve(\"SHCal20\",\"SHCal20.14c\");\r",
      "Mix_Curves(\"terrestrial\",\"IntCal20\",\"SHCal20\", 2,1);",
      "Curve(\"Marine20\",\"Marine20.14c\");\r",
      "Delta_R(\"Aquatic1\",2,1);",
      "Curve(\"terrestrial\");\r",
      "Delta_R(\"Aquatic1\",2,1);",
      "",
      "Mix_Curve(\"Individual_1\",\"terrestrial\",\"Aquatic1\", 0.511,0.29);\r",
      "R_Date(“\"Individual_1\"“, 1001,1);",
      "Mix_Curve(\"Individual_2\",\"terrestrial\",\"Aquatic1\", 0.146,0.159);\r",
      "R_Date(“\"Individual_2\"“, 1002,2);",
      "Mix_Curve(\"Individual_3\",\"terrestrial\",\"Aquatic1\", 0.096,0.119);\r",
      "R_Date(“\"Individual_3\"“, 1003,3);",
      "Mix_Curve(\"Individual_4\",\"terrestrial\",\"Aquatic1\", 0.092,0.119);\r",
      "R_Date(“\"Individual_4\"“, 1004,4);",
      "Mix_Curve(\"Individual_5\",\"terrestrial\",\"Aquatic1\", 0.234,0.205);\r",
      "R_Date(“\"Individual_5\"“, 1005,5);",
      "Mix_Curve(\"male\",\"terrestrial\",\"Aquatic1\", 0.328,0.297);\r",
      "R_Date(“\"male\"“, NA,NA);",
      "Mix_Curve(\"female\",\"terrestrial\",\"Aquatic1\", 0.141,0.167);\r",
      "R_Date(“\"female\"“, NA,NA);",
      "Mix_Curve(\"germany\",\"terrestrial\",\"Aquatic1\", 0.28,0.277);\r",
      "R_Date(“\"germany\"“, NA,NA);",
      "Mix_Curve(\"england\",\"terrestrial\",\"Aquatic1\", 0.119,0.143);\r",
      "R_Date(“\"england\"“, NA,NA);",
      "Mix_Curve(\"germany.male\",\"terrestrial\",\"Aquatic1\", 0.511,0.29);\r",
      "R_Date(“\"germany.male\"“, NA,NA);",
      "Mix_Curve(\"england.male\",\"terrestrial\",\"Aquatic1\", 0.146,0.159);\r",
      "R_Date(“\"england.male\"“, NA,NA);",
      "Mix_Curve(\"germany.female\",\"terrestrial\",\"Aquatic1\", 0.165,0.181);\r",
      "R_Date(“\"germany.female\"“, NA,NA);",
      "Mix_Curve(\"england.female\",\"terrestrial\",\"Aquatic1\", 0.092,0.119);\r",
      "R_Date(“\"england.female\"“, NA,NA);",
      "Mix_Curve(\"Individual_1\",\"terrestrial\",\"Aquatic1\", 0.077,0.08);\r",
      "R_Date(“\"Individual_1\"“, 1001,1);",
      "Mix_Curve(\"Individual_2\",\"terrestrial\",\"Aquatic1\", 0.087,0.102);\r",
      "R_Date(“\"Individual_2\"“, 1002,2);",
      "Mix_Curve(\"Individual_3\",\"terrestrial\",\"Aquatic1\", 0.027,0.041);\r",
      "R_Date(“\"Individual_3\"“, 1003,3);",
      "Mix_Curve(\"Individual_4\",\"terrestrial\",\"Aquatic1\", 0.523,0.233);\r",
      "R_Date(“\"Individual_4\"“, 1004,4);",
      "Mix_Curve(\"Individual_5\",\"terrestrial\",\"Aquatic1\", 0.045,0.041);\r",
      "R_Date(“\"Individual_5\"“, 1005,5);",
      "Mix_Curve(\"male\",\"terrestrial\",\"Aquatic1\", 0.082,0.092);\r",
      "R_Date(“\"male\"“, NA,NA);",
      "Mix_Curve(\"female\",\"terrestrial\",\"Aquatic1\", 0.198,0.269);\r",
      "R_Date(“\"female\"“, NA,NA);",
      "Mix_Curve(\"germany\",\"terrestrial\",\"Aquatic1\", 0.05,0.061);\r",
      "R_Date(“\"germany\"“, NA,NA);",
      "Mix_Curve(\"england\",\"terrestrial\",\"Aquatic1\", 0.305,0.283);\r",
      "R_Date(“\"england\"“, NA,NA);",
      "Mix_Curve(\"germany.male\",\"terrestrial\",\"Aquatic1\", 0.077,0.08);\r",
      "R_Date(“\"germany.male\"“, NA,NA);",
      "Mix_Curve(\"england.male\",\"terrestrial\",\"Aquatic1\", 0.087,0.102);\r",
      "R_Date(“\"england.male\"“, NA,NA);",
      "Mix_Curve(\"germany.female\",\"terrestrial\",\"Aquatic1\", 0.036,0.042);\r",
      "R_Date(“\"germany.female\"“, NA,NA);",
      "Mix_Curve(\"england.female\",\"terrestrial\",\"Aquatic1\", 0.523,0.233);\r",
      "R_Date(“\"england.female\"“, NA,NA);",
      "",
      "",
      "};"
    )
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
