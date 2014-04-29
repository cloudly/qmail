source("htmltotext.r")
#test variables
subject = "test"
to = "xx@xx.com"
from = "me@me.com"
attachment = c("a.file","b.file")
target_file = "a.txt"

writemail <- function (to, from=NULL, subject, 
                       HTML_body,
                       attachment = list(),
                       email_mode = "pic",
                       css = NULL, target_file =NULL,overwrite=TRUE,  email_engine=NULL,
                       reader = NULL) {
####email mode####
#   if email mode is "plain", then all pics will be attached directly, and tables will be printed with monospaced font, and HTML body will ignore html tags;
#   if email mode is "HTML", then all pics will be attached, and tables will be printed in HTML table format;
#   if email mode is "pic", then all pics will be embeded in email body, and tables will be printed in HTML format.
  require(base64)
  require(XML)

  if (email_mode == "plain"){
    if (is.na(email_engine)) email_engine = "mail"
    HTML_detag = htmlToText(HTML_body)
    if (is.null(target_file)) cat(HTML_detag,  file =target_file,append=!overwrite)
    #switch among email engines
    if (email_engine = "mail"){
      system(
        paste0('echo "',HTML_detag,'" | mail -s "',subject,'" ',to)
        )
    }
    if (email_engine = "mutt"){
      attachment2= paste("-a",attachment,collapse = " ")
      system(
        paste0('echo "',HTML_detag,'" | mutt -s "',subject,'" ',attachment2," ",to)
      )
    }
    
    if (email_engine = "sendmail"){
    }
   
  } else if (email_mode == "HTML") {
    #send HTML emails with mutt
    if (is.null(email_engine)) email_engine="mutt"
    if (email_engine != "mutt") {stop("For the HTML mode email we need mutt.")}
    if (is.null(target_file)) target_file = "new_temp.txt"
    if (email_engine == "mutt"){
      cat(HTML_body,  file =target_file,append=!overwrite)
      attachment2= paste("-a",attachment,collapse = " ")
      system(
        paste0('mutt -e "my_hdr From: ',from,'"  -e "my_hdr  Content-Type: text/html" -s "',subject,'" ',attachment2," ",to,' < ',target_file)
        )
      if (target_file == "new_temp.txt") file.remove(target_file)
    }
  } else if (email_mode == "pic") {
    
    if (reader != "outlook") {warning("We will try to embed the images directly. It may not work for outlook.")}
    
    if (is.null(target_file)) {
      target_file ="newfile.txt"
      } else {
      warning("It will overwrite your old file.")
    }
    cat("To: ",  to,  "\n",  file =target_file,append=F)
    cat("From: ",from ,"\n",  file =target_file,append=T)
    cat('Subject:',subject,"\n",  file =target_file,append=T)
    cat(
      'Content-Type: multipart/related;
    boundary="------------090303020209010600070908"
    type="multipart/alternative"
    MIME-Version: 1.0
    This is a multi-part message in MIME format.
    --------------090303020209010600070908
    Content-Type: text/html; charset=ISO-8859-15
    Content-Transfer-Encoding: 7bit
    ',  file =target_file,append=T)
    #### start HTML part ####
    cat('
      <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
      <html><head><style>',css,'</style></head><body>',
        file =target_file,append=T)
    #need to work out image tag
    #####blank space#####
    #   cat('<img src="cid:image001.png@01CF6310.49B10C00" alt="">', file =target_file,append=T)
    #####blank space#####
    #cat body
    cat(HTML_body,    file =target_file,append=T)
    #need to warp a table print function
    #   print(weekly_report_adhoc, type = "html", file =target_file,
    #         sanitize.text.function = force,append=T)
    #   cat("<p>Trigger campaigns:</p>",    file =target_file,append=T)
    #   print(weekly_report_trigger, type = "html", file =target_file,
    #         sanitize.text.function = force,append=T)
    cat('
--------------090303020209010600070908
Content-Type: image/png; name="image001.png"
Content-Description: image001.png
Content-Disposition: inline; filename="image001.png"; 
Content-ID: <image001.png>
Content-Transfer-Encoding: base64\n
      ',  file =target_file,append=T)
    cat(base64encode(graph_url2),  file =target_file,append=T)
    cat('\n--------------090303020209010600070908--\n',  file =target_file,append=T)
    system(
      paste0("/usr/sbin/sendmail <  ",target_file)
      )   
  } else {
    stop("not supported email mode, please choose \"HTML\", \"plain\" or \"pic\".")
  }

    #test line, works without image
  # system(paste0('mutt -e "my_hdr From: liychen@ebay.com"  -e "my_hdr  Content-Type: text/html" -s "Seller Email Campaign Weekly Summary ',Sys.Date(),'" liychen@ebay.com <',target_file))
}
