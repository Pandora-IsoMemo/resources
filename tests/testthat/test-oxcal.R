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

  expect_equal(
    oxcalLines2[1:20],
    c(
      "Plot()",
      "{",
      "Curve(\"IntCal20\",\"IntCal20.14c\");\r",
      "Curve(\"SHCal20\",\"SHCal20.14c\");\r",
      "Mix_Curves(\"terrestrial\",\"IntCal20\",\"SHCal20\", 2,1);",
      "Curve(\"terrestrial\");\r",
      "Delta_R(\"Aquatic1\",2,1);",
      "Curve(\"Marine20\",\"Marine20.14c\");\r",
      "Delta_R(\"Aquatic1\",2,1);",
      "",
      "Mix_Curve(\"Individual_1\",\"terrestrial\",\"Aquatic1\", P(0,100,[0,0, 0, 0.001, 0.002, 0.007, 0.013, 0.018, 0.023, 0.034, 0.044, 0.06, 0.073, 0.089, 0.108, 0.124, 0.138, 0.153, 0.167, 0.175, 0.188, 0.208, 0.227, 0.238, 0.248, 0.26, 0.267, 0.28, 0.295, 0.305, 0.317, 0.328, 0.332, 0.345, 0.359, 0.383, 0.394, 0.405, 0.423, 0.434, 0.448, 0.455, 0.467, 0.479, 0.491, 0.497, 0.51, 0.519, 0.523, 0.536, 0.543, 0.55, 0.561, 0.57, 0.58, 0.593, 0.598, 0.608, 0.612, 0.62, 0.628, 0.639, 0.644, 0.652, 0.663, 0.669, 0.677, 0.687, 0.693, 0.7, 0.705, 0.711, 0.718, 0.732, 0.736, 0.74, 0.746, 0.759, 0.766, 0.772, 0.776, 0.786, 0.794, 0.804, 0.813, 0.821, 0.829, 0.839, 0.848, 0.86, 0.869, 0.883, 0.889, 0.9, 0.912, 0.921, 0.931, 0.942, 0.954, 0.964, 0.98, 0.999,0]));\r",
      "R_Date(“\"Individual_1\"“, 1001,1);",
      "Mix_Curve(\"Individual_2\",\"terrestrial\",\"Aquatic1\", P(0,100,[0,0, 0, 0, 0, 0, 0, 0, 0, 0.001, 0.001, 0.001, 0.001, 0.002, 0.002, 0.002, 0.003, 0.004, 0.005, 0.005, 0.006, 0.007, 0.008, 0.009, 0.011, 0.014, 0.016, 0.018, 0.019, 0.021, 0.023, 0.024, 0.026, 0.028, 0.031, 0.033, 0.036, 0.039, 0.043, 0.045, 0.047, 0.051, 0.054, 0.058, 0.061, 0.063, 0.065, 0.068, 0.071, 0.074, 0.079, 0.08, 0.085, 0.09, 0.095, 0.096, 0.099, 0.107, 0.114, 0.121, 0.125, 0.132, 0.139, 0.144, 0.147, 0.153, 0.159, 0.165, 0.173, 0.179, 0.183, 0.192, 0.205, 0.216, 0.223, 0.232, 0.24, 0.249, 0.256, 0.264, 0.278, 0.288, 0.294, 0.302, 0.314, 0.33, 0.34, 0.354, 0.36, 0.372, 0.38, 0.389, 0.401, 0.43, 0.447, 0.46, 0.471, 0.492, 0.527, 0.555, 0.601, 0.741,0]));\r",
      "R_Date(“\"Individual_2\"“, 1002,2);",
      "Mix_Curve(\"Individual_3\",\"terrestrial\",\"Aquatic1\", P(0,100,[0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.001, 0.001, 0.001, 0.002, 0.002, 0.002, 0.003, 0.003, 0.004, 0.004, 0.005, 0.006, 0.006, 0.007, 0.007, 0.008, 0.009, 0.01, 0.011, 0.012, 0.012, 0.013, 0.015, 0.016, 0.017, 0.018, 0.019, 0.02, 0.021, 0.023, 0.025, 0.026, 0.029, 0.03, 0.032, 0.035, 0.038, 0.042, 0.044, 0.047, 0.048, 0.051, 0.055, 0.059, 0.061, 0.066, 0.067, 0.071, 0.074, 0.077, 0.08, 0.082, 0.085, 0.089, 0.093, 0.099, 0.102, 0.107, 0.112, 0.117, 0.119, 0.124, 0.128, 0.133, 0.139, 0.147, 0.151, 0.159, 0.165, 0.172, 0.181, 0.186, 0.191, 0.197, 0.207, 0.215, 0.226, 0.232, 0.238, 0.246, 0.255, 0.263, 0.282, 0.297, 0.315, 0.341, 0.359, 0.386, 0.432, 0.542, 0.723,0]));\r",
      "R_Date(“\"Individual_3\"“, 1003,3);",
      "Mix_Curve(\"Individual_4\",\"terrestrial\",\"Aquatic1\", P(0,100,[0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.001, 0.001, 0.001, 0.001, 0.002, 0.002, 0.003, 0.003, 0.004, 0.004, 0.005, 0.006, 0.006, 0.007, 0.008, 0.009, 0.009, 0.01, 0.011, 0.013, 0.014, 0.016, 0.017, 0.018, 0.019, 0.021, 0.023, 0.024, 0.025, 0.027, 0.029, 0.031, 0.032, 0.034, 0.035, 0.039, 0.041, 0.043, 0.046, 0.048, 0.051, 0.054, 0.057, 0.06, 0.063, 0.065, 0.068, 0.071, 0.075, 0.079, 0.084, 0.089, 0.093, 0.096, 0.102, 0.105, 0.11, 0.113, 0.118, 0.125, 0.13, 0.134, 0.14, 0.152, 0.158, 0.164, 0.173, 0.18, 0.185, 0.199, 0.206, 0.213, 0.225, 0.235, 0.241, 0.251, 0.263, 0.275, 0.287, 0.302, 0.322, 0.346, 0.377, 0.41, 0.469, 0.513, 0.642,0]));\r",
      "R_Date(“\"Individual_4\"“, 1004,4);",
      "Mix_Curve(\"Individual_5\",\"terrestrial\",\"Aquatic1\", P(0,100,[0,0, 0, 0, 0, 0, 0, 0.001, 0.001, 0.002, 0.002, 0.003, 0.005, 0.007, 0.008, 0.011, 0.013, 0.015, 0.017, 0.02, 0.022, 0.026, 0.03, 0.033, 0.037, 0.041, 0.047, 0.051, 0.056, 0.062, 0.066, 0.074, 0.079, 0.085, 0.091, 0.099, 0.106, 0.112, 0.121, 0.128, 0.133, 0.142, 0.15, 0.152, 0.157, 0.161, 0.166, 0.172, 0.183, 0.19, 0.193, 0.199, 0.207, 0.212, 0.218, 0.224, 0.228, 0.236, 0.241, 0.249, 0.255, 0.263, 0.272, 0.279, 0.286, 0.29, 0.297, 0.303, 0.306, 0.315, 0.322, 0.325, 0.334, 0.344, 0.349, 0.355, 0.361, 0.368, 0.376, 0.382, 0.388, 0.4, 0.409, 0.421, 0.428, 0.442, 0.454, 0.463, 0.481, 0.495, 0.51, 0.52, 0.538, 0.558, 0.569, 0.597, 0.635, 0.651, 0.696, 0.749, 0.828, 0.944,0]));\r",
      "R_Date(“\"Individual_5\"“, 1005,5);"
    )
  )
})
