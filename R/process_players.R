#' Process Players
#'
#' Processes players data
#' @param none none
#' @keywords Fide players list processing
#' @export
#' @examples
#' process_players()

process_players <- function() {
	require(xtable)
	require(players)
	process_key<-function(ckey) {
		path=paste("current/stats",ckey,sep="/")
		dir.create(path)
		for(key in levels(as.factor(pr[,eval(quote(ckey))]))) {
			if(ckey=="birthday") {
				cc<-pr[complete.cases(pr$birthday),]
				c<-cc[cc$birthday==strtoi(key),]
			} else {
				c<-pr[pr[,eval(quote(ckey))]==key,]
			}
			cs<-c[order(-c$rating),]
			pathtxt<-paste("current/stats/",ckey,"/",key,".txt",sep="")
			pathhtml<-paste("current/stats/",ckey,"/",key,".html",sep="")
			rownames(cs)<-NULL
			write.table(cs,pathtxt,row.names=FALSE)
			print(xtable(cs,row.names=FALSE), type="html", file=pathhtml)
		}
	}
	
	print("removing stats directory")
	#unlink("current/stats",recursive=TRUE)
	print("reading data")
	p<<-read.table("current/players.txt")
	print("filtering unrated")
	pr<<-p[complete.cases(p$rating),]
	print("creating rating cluster")
	pr[["rcluster"]]=floor(pr$rating/100)*100
	print("creating stats directory")
	dir.create("current/stats")
	print("collect country")
	#process_key("country")
	print("collect birthday")
	process_key("birthday")
	print("collect rating cluster")
	#process_key("rcluster")
	print("collect title")
	#process_key("title")
}

