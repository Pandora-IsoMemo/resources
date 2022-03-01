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
      curve = file[as.numeric(getCurveTitlesXlsx(file)[2]), ],
      binOption = "Option PDF",
      deltaRParams = c(2, 1)
    ),
    list(header = "Curve(\"terrestrial\");\r\nDelta_R(\"Aquatic1\",2,1);", 
         option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", P(0,100,[0,%%BINS%%,0]));\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")
  )
  
  file <- read.xlsx(
    "https://pandoradata.earth/dataset/46fe7fc7-55a4-493d-91e8-c9abffbabcca/resource/120d810e-ff7d-49b7-80b8-e9791e2980b3/download/aquatic2.xlsx"
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
  
  aquaticCurve1Mean <-
    list(header = "Curve(\"Marine20\",\"Marine20.14c\");\r\nDelta_R(\"Aquatic1\",2,1);", 
         option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", %%MEAN%%,%%SD%%);\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")
  
  aquaticCurve1PDF <-
    list(header = "Curve(\"terrestrial\");\r\nDelta_R(\"Aquatic1\",2,1);", 
         option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", P(0,100,[0,%%BINS%%,0]));\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")
  
  aquaticCurve2PDF <-
    list(header = "Curve(\"Marine20\",\"Marine20.14c\");\r\nDelta_R(\"Aquatic1\",2,1);", 
         option = "Mix_Curve(%%TARGET_ID%%,\"terrestrial\",\"Aquatic1\", P(0,100,[0,%%BINS%%,0]));\r\nR_Date(“%%TARGET_ID%%“, %%RADIOCARBON_MEAN%%,%%RADIOCARBON_SD%%);")
  
  aquaticCurve2Mean <-
    list(header = "Curve(\"terrestrial\");\r\nDelta_R(\"Aquatic1\",2,1);", 
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
    aquaticCurve1 = aquaticCurve1Mean,
    aquaticCurve2 = aquaticCurve2Mean,
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
    aquaticCurve1 = aquaticCurve1PDF,
    aquaticCurve2 = aquaticCurve2PDF,
    OxCalA = oxCalA,
    OxCalB = oxCalB,
    bins = TRUE,
    coordinates = exportCoordinates
  ) %>%
    strsplit(split = "\n") %>%
    unlist()

  expect_equal(oxcalLines1[1], "Plot()")
  expect_equal(oxcalLines1[3], "Curve(\"IntCal20\",\"IntCal20.14c\");\r")
  expect_equal(
    oxcalLines1[11],
    "Mix_Curve(\"Individual_1\",\"terrestrial\",\"Aquatic1\", 0.511,0.29);"
    )
  
  expect_equal(oxcalLines2[1], "Plot()")
  expect_equal(oxcalLines2[3], "Curve(\"IntCal20\",\"IntCal20.14c\");\r")
  expect_equal(
    oxcalLines2[11],
    "Mix_Curve(\"Individual_1\",\"terrestrial\",\"Aquatic1\", P(0,100,[0,0, 0, 0.001, 0.002, 0.007, 0.013, 0.018, 0.023, 0.034, 0.044, 0.06, 0.073, 0.089, 0.108, 0.124, 0.138, 0.153, 0.167, 0.175, 0.188, 0.208, 0.227, 0.238, 0.248, 0.26, 0.267, 0.28, 0.295, 0.305, 0.317, 0.328, 0.332, 0.345, 0.359, 0.383, 0.394, 0.405, 0.423, 0.434, 0.448, 0.455, 0.467, 0.479, 0.491, 0.497, 0.51, 0.519, 0.523, 0.536, 0.543, 0.55, 0.561, 0.57, 0.58, 0.593, 0.598, 0.608, 0.612, 0.62, 0.628, 0.639, 0.644, 0.652, 0.663, 0.669, 0.677, 0.687, 0.693, 0.7, 0.705, 0.711, 0.718, 0.732, 0.736, 0.74, 0.746, 0.759, 0.766, 0.772, 0.776, 0.786, 0.794, 0.804, 0.813, 0.821, 0.829, 0.839, 0.848, 0.86, 0.869, 0.883, 0.889, 0.9, 0.912, 0.921, 0.931, 0.942, 0.954, 0.964, 0.98, 0.999,0]));"
    )
}
