% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{qa_tbl}
\alias{qa_tbl}
\title{A dataset for testing stpR setup utilities}
\format{
A data frame with 300 rows--one row per col_id
\describe{
\item{col_id}{col_id}
\item{event_date}{event_date}
\item{age}{age}
\item{stage}{stage}
\item{los}{los}
\item{ebl}{ebl}
\item{asa}{asa}
\item{trt}{trt}
\item{approach}{approach}
\item{visit_codes}{visit_codes}
\item{EVENT_DATE_RAW}{EVENT_DATE_RAW}
\item{AGE_RAW}{AGE_RAW}
\item{STAGE_RAW}{STAGE_RAW}
\item{LOS_RAW}{LOS_RAW}
\item{EBL_RAW}{EBL_RAW}
\item{ASA_RAW}{ASA_RAW}
\item{TRT_RAW}{TRT_RAW}
\item{APPROACH_RAW}{APPROACH_RAW}
\item{VISIT_CODES_RAW}{VISIT_CODES_RAW}
\item{visit_type}{visit_type}
}
}
\usage{
qa_tbl
}
\description{
This dataset is used to illustrate and test the setup utilities in stpR. By convention, the columns in all caps are used to denote the raw data, and the columns in lower case are used to denote the standardized data.
}
\keyword{datasets}
