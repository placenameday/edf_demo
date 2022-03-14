
stamp_asc_fu <- function(ascid, ascdic=ac, outputdic=bc){
  ascfile <- paste(ascdic, ascid, ".asc", sep = "")
  asc <- read(ascfile)
  asc_tidy <- apply_tidy_msg(asc)
  asc_tidy$fix <- stamp2_fu(asc_tidy$msg, asc_tidy$fix)
  asc_tidy$sacc <- stamp2_fu(asc_tidy$msg, asc_tidy$sacc)
  asc_tidy$blinks <- stamp2_fu(asc_tidy$msg, asc_tidy$blinks)
  asc_tidy$raw <- stamp_raw_fu(asc_tidy$msg, asc_tidy$raw)
  fix_csvfile <- paste(outputdic, ascid, "_fix.csv", sep = "")
  sacc_csvfile <- paste(outputdic, ascid, "_sacc.csv", sep = "")
  blinks_csvfile <- paste(outputdic, ascid, "_blinks.csv", sep = "")
  raw_csvfile <- paste(outputdic, ascid, "_raw.csv", sep = "")
  msg_csvfile <- paste(outputdic, ascid, "_msg.csv", sep = "")
  write_csv(asc_tidy$fix, fix_csvfile)
  write_csv(asc_tidy$sacc, sacc_csvfile)
  write_csv(asc_tidy$blinks, blinks_csvfile)
  write_csv(asc_tidy$raw, raw_csvfile)
  write_csv(asc_tidy$msg, msg_csvfile)
  asc_tidy
}


stamp_asc <- function(ascid, ascdic=ac, outputdic=bc){
  ascfile <- paste(ascdic, ascid, ".asc", sep = "")
  asc <- read(ascfile)
  asc_tidy <- apply_tidy_msg(asc)
  asc_tidy$fix <- stamp2(asc_tidy$msg, asc_tidy$fix)
  asc_tidy$sacc <- stamp2(asc_tidy$msg, asc_tidy$sacc)
  asc_tidy$blinks <- stamp2(asc_tidy$msg, asc_tidy$blinks)
  asc_tidy$raw <- stamp_raw(asc_tidy$msg, asc_tidy$raw)
  fix_csvfile <- paste(outputdic, ascid, "_fix.csv", sep = "")
  sacc_csvfile <- paste(outputdic, ascid, "_sacc.csv", sep = "")
  blinks_csvfile <- paste(outputdic, ascid, "_blinks.csv", sep = "")
  raw_csvfile <- paste(outputdic, ascid, "_raw.csv", sep = "")
  msg_csvfile <- paste(outputdic, ascid, "_msg.csv", sep = "")
  write_csv(asc_tidy$fix, fix_csvfile)
  write_csv(asc_tidy$sacc, sacc_csvfile)
  write_csv(asc_tidy$blinks, blinks_csvfile)
  write_csv(asc_tidy$raw, raw_csvfile)
  write_csv(asc_tidy$msg, msg_csvfile)
  asc_tidy
}
