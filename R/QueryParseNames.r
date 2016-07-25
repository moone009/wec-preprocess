

css_query_parse <- function(query){
  query <- trim(gsub(",'  ',' ')","" ,query))
  query <- trim(gsub("str_replace[(]","" ,query))
  stop_ = regexpr('from', query)
  unlist(strsplit(substr(query, 7, stop_[1]), ","))
  cols <- trim(gsub("\n ","",unlist(strsplit(substr(query, 7, stop_[1]-1), ","))))
  cols <- trim(gsub("a[.]","" ,cols))
  cols <- trim(gsub("b[.]","" ,cols))
  cols <- trim(gsub("c[.]","" ,cols))
  return(cols)
}


css_query_parse("select 
id_prem, 
id_mtr_rte, 
 ad_hse_no , 
 ad_crdnl_dir ,
 ad_str_nm , 
 ad_str_sx , 
 ad_dir_sx ,
 ad_svc_city , 
 ad_svc_zip , 
 ad_svc_st , 
 ad_multpl_dwl_typ ,
cd_dwelling 
from cu04tb01 
")