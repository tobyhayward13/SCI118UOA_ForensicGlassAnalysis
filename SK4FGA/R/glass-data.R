#' glass
#'
#' Glass composition data for seven elements from 200 glass items.
#'
#' These data are from Grzegorz (Greg) Zadora at the [Institute of Forensic Research](http://ies.krakow.pl/) in
#' Krakow, Poland. They are the log of the ratios of each
#' element to oxygen, so logNaO is the log(10) of the Sodium to Oxygen ratio,
#' and logAlO is the log of the Aluminium to Oxygen ratio. The instrumental
#' method was SEM-EDX.
#'
#' The `item` indicates the object the glass came from. The levels for each item
#' are unique to that item. The `fragment` can be considered a sub-item. When
#' collecting these observations Greg took a glass object, say a jam jar, he
#' would then break it, and extract four fragments. Each fragment would be
#' measured three times upon different parts of that fragment. The fragment
#' labels are repeated, so, for example, fragment "f1" from item "s2" has
#' nothing whatsoever to do with fragment "f1" from item "s101".
#'
#' For two level models use `item` as the lower level - three level models can
#' use the additional information from the individual fragments.
#'
#' @name glass
#' @docType data
#'
#' @format a `data.frame` with 2400 rows and 9 columns.
#' \describe{
#'   \item{item}{factor}{200 levels - which item the measurements came from}
#'   \item{fragment}{factor}{4 levels - which of the four fragments from each item the observations were made
#'   upon}
#'   \item{logNaO}{numeric}{log of sodium concentration to oxygen concentration}
#'   \item{logMgO}{numeric}{log of magnesium concentration to oxygen concentration}
#'   \item{logAlO}{numeric}{log of aluminium concentration to oxygen concentration}
#'   \item{logSiO}{numeric}{log of silicon concentration to oxygen concentration}
#'   \item{logKO}{numeric}{log of potassium concentration to oxygen concentration}
#'   \item{logCaO}{numeric}{log of calcium concentration to oxygen concentration}
#'   \item{logFeO}{numeric}{log of iron concentration to oxygen concentration}
#' }
#' @usage
#' data(glass)
#' @references Aitken, C.G.G. Zadora, G. & Lucy, D. (2007) A Two-Level Model for
#'   Evidence Evaluation. \emph{Journal of Forensic Sciences}: \bold{52}(2);
#'   412-419.
#' @source Grzegorz Zadora [Institute of Forensic Research](http://ies.krakow.pl/), Krakow, Poland.
#' @keywords datasets
"glass"



#' glass2
#'
#' Glass Fragment Elemental Composition Data on 15 variables.
#'
#' Log transformed example casework data
#'
#' @name glass2
#' @docType data
#'
#' @format a `data.frame` with 6858 rows and 16 columns.
#' \describe{
#'   \item{item}{factor}{761 levels - which item the measurements came from}
#'   \item{Li7}{numeric}{log of lithium concentration}
#'   \item{Mg25}{numeric}{log of magnesium concentration}
#'   \item{Al27}{numeric}{log of aluminium concentration}
#'   \item{K39}{numeric}{log of potassium concentration}
#'   \item{Ti49}{numeric}{log of titanium concentration}
#'   \item{Mn55}{numeric}{log of manganese concentration}
#'   \item{Fe57}{numeric}{log of iron concentration}
#'   \item{Rb85}{numeric}{log of rubidium concentration}
#'   \item{Sr88}{numeric}{log of strontium concentration}
#'   \item{Zr90}{numeric}{log of zirconium concentration}
#'   \item{Ba137}{numeric}{log of barium concentration}
#'   \item{La139}{numeric}{log of lanthanum concentration}
#'   \item{Ce140}{numeric}{log of cerium concentration}
#'   \item{Nd146}{numeric}{log of neodymium concentration}
#'   \item{Pb208}{numeric}{log of lead concentration}
#' }
#' @usage
#' data(glass2)
#' @references Anuradha Akmeemana, R. C., Jose Almirall, The Calculation of Calibrated Likelihood Ratios (LRs) for
#' Glass Using a Multivariate Kernel Density Model: Introducing a User-Friendly Graphical User Interface (GUI).
#' In American Academy of Forensic Science, Anaheim, CA, 2020.
#' @source Almirall, Jose; Akmeemana, Anuradha, 2022,
#' "casework.tab", Shiny Glass Application, https://doi.org/10.34703/gzx1-9v95/OB8BS9/CP6WXP,
#' FIU Research Data Portal, V2, UNF:6:jQxEQCGZVvlWtc6owbtp+A== [fileUNF]
#' @keywords datasets
"glass2"



#' vehicle.glass
#'
#' FIU Vehicle Glass Database V2.0
#'
#' This freely available research-based database consists of 762 samples of various vehicle glass (windshield, passenger side, driver side, etc.).
#' The samples span various makes and models, and range in year from 2004-2019. All samples were collected from the M&M salvage yard in Ruckersville, VA.
#'
#' @name vehicle.glass
#' @docType data
#'
#' @format a `data.frame` with 6858 rows and 16 columns.
#' \describe{
#'   \item{item}{factor}{761 levels - which item the measurements came from}
#'   \item{Li7}{numeric}{log of lithium concentration}
#'   \item{Mg25}{numeric}{log of magnesium concentration}
#'   \item{Al27}{numeric}{log of aluminium concentration}
#'   \item{K39}{numeric}{log of potassium concentration}
#'   \item{Ti49}{numeric}{log of titanium concentration}
#'   \item{Mn55}{numeric}{log of manganese concentration}
#'   \item{Fe57}{numeric}{log of iron concentration}
#'   \item{Rb85}{numeric}{log of rubidium concentration}
#'   \item{Sr88}{numeric}{log of strontium concentration}
#'   \item{Zr90}{numeric}{log of zirconium concentration}
#'   \item{Ba137}{numeric}{log of barium concentration}
#'   \item{La139}{numeric}{log of lanthanum concentration}
#'   \item{Ce140}{numeric}{log of cerium concentration}
#'   \item{Nd146}{numeric}{log of neodymium concentration}
#'   \item{Pb208}{numeric}{log of lead concentration}
#' }
#' @usage
#' data(vehicle.glass)
#' @references Anuradha Akmeemana, R. C., Jose Almirall, The Calculation of Calibrated Likelihood Ratios (LRs) for
#' Glass Using a Multivariate Kernel Density Model: Introducing a User-Friendly Graphical User Interface (GUI).
#' In American Academy of Forensic Science, Anaheim, CA, 2020.
#' @source Almirall, Jose; Akmeemana, Anuradha, 2022,
#' "FIU Vehicle Glass Database V2.0.tab", Shiny Glass Application, https://doi.org/10.34703/gzx1-9v95/OB8BS9/XGH0IO,
#' FIU Research Data Portal, V2, UNF:6:YDbwWISU04S+UCtb7aRoBQ== [fileUNF]
#' @keywords datasets
"vehicle.glass"

