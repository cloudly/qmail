#test lines
# HTML_body <-  readChar(file("html_img_eg.html","rb"),file.info("html_img_eg.html")$size)

parsehtmlimg <- function(HTML_body){
  require(stringr)
  detect_img <- grepl("\\[img:.*?\\]",HTML_body)
  detect_table <- grepl("\\[table:.*?\\]",HTML_body)
  if (!detect_table & !detect_img) {
    warning("cannot find any images nor tables. Will send the HTML message directly.")
    return(HTML_body)}
  #replace image tag
  if (!detect_img) {warning("cannot find any images.")} else {
    image_url = unlist(str_extract_all(HTML_body, "\\[img:.*?\\]") )
    image_location = str_locate_all(HTML_body, "\\[img:.*?\\]")[[1]]
    image_url = gsub("\\[img:","",image_url)
    image_url = gsub("\\]","",image_url)
    
    img_string = paste0('<img src="cid:image',1:length(image_url),'.png@01CF6310.49B10C00" alt="">')
    
  }
  if (!detect_table) {warning("cannot find any tables.")}
  
  
}