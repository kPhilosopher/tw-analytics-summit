#discover quasars
library(ggplot2)
library(data.table)

data_dir="/Users/davej/TW/tw-analytics-summit/data/astro"

read.astro<-function(){
   #a simple reader for the astro data-set (SDSS Survey)
   file=paste(data_dir,'/','sdss.csv',sep='')
   #read the csv file into a data.frame
   data=read.csv(file)
   #convert to a data.table, a more powerful data structure
   data=data.table(data)
   #keep only rows with specClass = 1,2,3 which is star,galaxy,quasar
   data=data[specClass %in% c(1,2,3),]
   #replace specClass number with a 'factor' 
   spec.classes=factor(c('star','galaxy','quasar'))
   data[,objtype:=spec.classes[specClass]]
   #drop these columns
   data[,specClass:=NULL]
   data[,objid:=NULL]
   #function defined below
   add.colors(data)
   return(data)
}

add.colors<-function(data){
   #add the colors, note this is side effecting because data.table
   #is passed by reference unlike data.frame
   data[,ug:=u-g]
   data[,gr:=g-r]
   data[,ri:=r-i]
   data[,iz:=i-z]
}

plot.radec<-function(
   data=read.astro(),rs.min=0.02,rs.max=0.06,type='galaxy',contours=F
   ){
   #make a plot of ra,dec (like longitude, lattitude for the sky)
   #default data is automatically read from file
   #cont - if set to TRUE, will add contours 
   #rs.min, rs.max the min and max redshifts
   print(paste('object type:',type))
   print(paste('redshift range:',rs.min,'to',rs.max))
   #make the cut on redshifts and obj.type, keep only ra,dec columns
   data=data[objtype==type & redshift> rs.min & redshift <= rs.max,list(ra,dec)]
   #make a ggplot object, p
   p<-ggplot(data,aes(ra,dec))+geom_point(size=0.5)
   #set x,y limits on plot
   p=p+xlim(130,240)+ylim(0,60)
   #add a title
   title=paste("Distribution of objects of type=",type)
   title=paste(title,", Redshift range = [",rs.min,",",rs.max,"]")
   p=p+ggtitle(title)
   if (contours==T) {
      #add contours to plot
      p=p+geom_density2d(size=.5,linetype='solid',colour='red')
   }
   #'printng' a ggplot object means, make the plot
   print(p)
}

get.color.data<-function(data=read.astro()){
   #reshape the data to make it ggplot2 friendly
   #do a rowbind, there are other ways too
   
   #data.table objects are just references, no copy here
   d=data
   #separate by panel
   d.ugr=data.table(x=d$ug,y=d$gr,ctype='1_ugr',objtype=d$objtype,redshift=d$redshift)
   d.gri=data.table(x=d$gr,y=d$ri,ctype='2_gri',objtype=d$objtype,redshift=d$redshift)
   d.riz=data.table(x=d$ri,y=d$iz,ctype='3_riz',objtype=d$objtype,redshift=d$redshift)
   
   #trim out extreme values
   d.ugr=d.ugr[x>-1 & x<3 & y> -1 & y<1.6,]
   d.gri=d.gri[x>-1 & x<1.6 & y> -0.75 & y<0.8,]
   d.riz=d.riz[x>-1 & x<1 & y> -1 & y<1.2,]
   
   #stick them back together
   cdata=rbind(d.ugr,d.gri,d.riz)
   return(cdata)
}

plot.colors<-function(cdata=get.color.data(),contours=F,quasars=F){
   #make a basic ggpplot scatter plot of x versus y
   p<-ggplot(cdata,aes(x,y,colour=objtype,group=objtype,linetype=objtype))
   p=p+geom_point(size=0.9)
   #break it up into fascets so that x,y mean the same thing witin a subplot
   p=p+facet_wrap(~ctype,ncol=2,scales="free")
   if (contours==T) {
      #add contours to plot
      p=p+geom_density2d(size=.5,color='black')
   }
   if (quasars==T) {
      #TODO, unfinished
      #overplot the quasar track
      q=quasar.track()
      qc=get.color.data(q)     
      qc$rs.bin=1:nrow(qc)
   }
   print(p)
}

quasar.track<-function(data=read.astro(),binsize=0.15){
   #calculate the mean colors for quasars as a function of redshift
   
   #define some bins with equal counts per bin
   dat=data[objtype=='quasar' & redshift > 0.15 & redshift < 2.5,
            list(redshift,ug,gr,ri,iz)]
   #create the rs.bin variable, with approx equal number per bin
   rs.min=min(dat$redshift)
   dat[,rs.scaled:=log(.1+redshift-rs.min)]
   dat$rs.scaled=dat$rs.scaled-min(dat$rs.scaled)
   dat[,rs.bin:=floor(rs.scaled/binsize)] 
   #now apply the median function to all cols by bin 
   qdat=dat[,c(lapply(.SD,mean),N=.N),by=rs.bin]
   qdat[,objtype:='quasar']
   qdat=qdat[order(rs.bin),]
   return(qdat)
}

test.all<-function(){
   #a basic test that runs all the programs 
   data=read.astro()
   ok=readline("ok?")
   plot.radec(data)
   ok=readline("ok?")
   plot.radec(data,cont=T)
   ok=readline("ok?")
   plot.colors()
   ok=readline("ok?")
   plot.colors(cont=T)
   print('Congrats! It ran.')
}




