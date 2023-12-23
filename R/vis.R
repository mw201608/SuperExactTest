#Compute overlap test and visualize intersections between multiple sets
#Author: Minghui Wang
#minghui.wang@mssm.edu
#
plot.msets=function(x,Layout=c('circular','landscape'),degree=NULL,keep.empty.intersections=TRUE,
	sort.by=c('set','size','degree','p-value'),min.intersection.size=0,max.intersection.size=Inf,ylim=NULL,
	log.scale=FALSE,yfrac=0.8,margin=NULL,color.scale.pos=c(0.85, 0.9),legend.pos=c(0.85,0.25),legend.col=2,legend.text.cex=1,
	color.scale.cex=1,color.scale.title=expression(paste(-Log[10],'(',italic(P),')')),color.on='#2EFE64',color.off='#EEEEEE',
	show.overlap.size=TRUE, show.fold.enrichment=FALSE, show.set.size=TRUE,	overlap.size.cex=0.9, track.area.range=0.3,bar.area.range=0.2,
	new.gridPage=TRUE,minMinusLog10PValue=0,maxMinusLog10PValue=NULL,show.elements=FALSE,...){
#keep.empty.intersections, whether to retain empty intersections in the plot
	Layout <- match.arg(Layout)
	if(is.character(color.scale.pos)){
		color.scale.pos=color.scale.pos[1]
	}else if(is.numeric(color.scale.pos)){
		if(length(color.scale.pos)!=2) stop('Invalid color.scale.pos\n')
	}else{
		stop('Invalid color.scale.pos\n')
	}
	if(is.null(x$overlap.expected)) show.fold.enrichment=FALSE
	if(show.fold.enrichment==TRUE) show.overlap.size=FALSE
	if(Layout=='circular'){
		return(plot.msets.circular(x=x,degree=degree,keep.empty.intersections=keep.empty.intersections,sort.by=sort.by,
		min.intersection.size=min.intersection.size,max.intersection.size=max.intersection.size,ylim=ylim,log.scale=log.scale,color.scale.pos=color.scale.pos,legend.pos=legend.pos,
		legend.col=legend.col,legend.text.cex=legend.text.cex,color.scale.cex=color.scale.cex,
		color.scale.title=color.scale.title,color.on=color.on,color.off=color.off,show.overlap.size=show.overlap.size,show.fold.enrichment=show.fold.enrichment,overlap.size.cex=overlap.size.cex,margin=margin,
		track.area.range=track.area.range,bar.area.range=bar.area.range,new.gridPage=new.gridPage,minMinusLog10PValue=minMinusLog10PValue,maxMinusLog10PValue=maxMinusLog10PValue,...))
	}else if(Layout=='landscape'){
		return(plot.msets.landscape(x=x,degree=degree,keep.empty.intersections=keep.empty.intersections,sort.by=sort.by,
		min.intersection.size=min.intersection.size,max.intersection.size=max.intersection.size,ylim=ylim,log.scale=log.scale,yfrac=yfrac,margin=margin,color.scale.pos=color.scale.pos,color.scale.cex=color.scale.cex,
		color.scale.title=color.scale.title,color.on=color.on,color.off=color.off,show.overlap.size=show.overlap.size,show.fold.enrichment=show.fold.enrichment,show.set.size=show.set.size,overlap.size.cex=overlap.size.cex,
		new.gridPage=new.gridPage,minMinusLog10PValue=minMinusLog10PValue,maxMinusLog10PValue=maxMinusLog10PValue,show.elements=show.elements,...))
	}else{
		stop('Invalid Layout\n')
	}
}
plot.msets.landscape=function(x,degree=NULL,keep.empty.intersections=TRUE,sort.by=c('set','size','degree','p-value'),min.intersection.size=0,max.intersection.size=Inf,ylim=NULL,
	log.scale=FALSE,yfrac=0.8,margin=NULL,color.scale.pos=c(0.85, 0.9),color.scale.cex=1,color.scale.title=expression(paste(-Log[10],'(',italic(P),')')),
	color.on='#2EFE64',color.off='#EEEEEE',show.overlap.size=FALSE,show.fold.enrichment=FALSE,show.set.size=TRUE,overlap.size.cex=0.9,new.gridPage=TRUE,minMinusLog10PValue=0,maxMinusLog10PValue=NULL,show.elements=FALSE,...){
	#
	Args=list(...)
	enable.debug=ifelse(is.null(Args$enable.debug),FALSE,Args$enable.debug)
	cex=ifelse(is.null(Args$cex),par('cex'),Args$cex)
	cex.lab=ifelse(is.null(Args$cex.lab),1.2,Args$cex.lab)
	ylab=ifelse(is.null(Args$ylab),'Number of elements',Args$ylab)
	#set color scheme
	if(is.null(Args$heatmapColor)){
		heatmapColor = rev(heat.colors(50)[1:50])
	}else{
		heatmapColor = Args$heatmapColor
	}
	#
	flip.vertical=Args$flip.vertical
	if(is.null(flip.vertical)) flip.vertical=FALSE
	circle.radii=Args$circle.radii
	if(is.null(circle.radii)) circle.radii=0.5
	#
	color.expected.overlap=Args$color.expected.overlap
	alpha.expected.overlap=Args$alpha.expected.overlap
	expected.overlap.style=Args$expected.overlap.style
	if(is.null(color.expected.overlap)) color.expected.overlap='grey'
	if(is.null(alpha.expected.overlap)) alpha.expected.overlap=1
	show.expected.overlap=Args$show.expected.overlap
	expected.overlap.lwd=Args$expected.overlap.lwd
	if(is.null(show.expected.overlap)) show.expected.overlap=FALSE
	if(is.null(expected.overlap.style)) expected.overlap.style="hatchedBox"
	if(is.null(expected.overlap.lwd)) expected.overlap.lwd=2
	expected.overlap.style=match.arg(expected.overlap.style,choices=c("hatchedBox","horizBar","box"))
	title = Args$title
	cex.title = Args$cex.title
	if(is.null(cex.title)) cex.title = 1
	#
	etab=x$overlap.expected
	if(is.null(etab)) show.expected.overlap=FALSE
	if(show.fold.enrichment){
		fetab=sprintf("%.1f",x$overlap.sizes/etab)
		fetab[fetab=='NA']=NA
		names(fetab)=names(x$overlap.sizes)
	}
	bar.split=Args$bar.split
	if(!is.null(bar.split)){
		bar.split=sort(bar.split)
		if(any(bar.split <= ylim[1])) stop('bar.split values are invalid\n')
		if(any(bar.split >= ylim[2])) stop('bar.split values are invalid\n')
	}
	nColors=length(heatmapColor)-1
	params=getPlotParams(x,nColors,degree=degree,keep.empty.intersections=keep.empty.intersections,sort.by=sort.by,min.intersection.size=min.intersection.size,max.intersection.size=max.intersection.size,ylim=ylim,log.scale=log.scale,Layout='landscape',minMinusLog10PValue=minMinusLog10PValue,maxMinusLog10PValue=maxMinusLog10PValue,show.expected.overlap=show.expected.overlap)
	if((!is.null(bar.split)) && log.scale==TRUE) bar.split=log(bar.split)
	ylabel=params$ylabel
	ylabel0=params$ylabel0
	ylim=params$ylim
	otab=params$otab
	otab[otab>ylim[2]]=ylim[2]
	otab[otab<ylim[1]]=ylim[1]
	otab=otab-ylim[1]
	otab0=params$otab0
	nO=length(otab)
	nSet=length(x$set.sizes) #number of sets
	cid=params$cid
	mlogp=params$mlogp
	if(is.null(color.on)) color.on = c("#386cb0", "#fdb462", "#7fc97f", "#ef3b2c", "#662506", "#a6cee3", "#fb9a99", "#984ea3", "#ffff33", "#00798c", "#d1495b", "#66a182", "#2e4057")
	if(length(color.on) < nSet) color.on = rep(color.on, length.out = nSet)
	#
	if(show.elements){
		sumtable=Args$elements.list
		if(is.null(sumtable)){
			sumtable=summary(x,degree=degree)$Table
		}else{
			if(!any(colnames(sumtable)=='Elements')) stop('elements.list must be a data.frame or matrix with one column named "Elements"\n')
		}
	}
	#start plotting
	if(new.gridPage){
		grid.newpage()
	}else{
		vp0 = as.character(current.vpPath())
		on.exit(seekViewport(vp0))
	}
	#arrange layout
	if(is.null(margin)) margin=c(0.5,5,1.5,2)+0.1
	if(flip.vertical){
		top.vp <- viewport(layout=grid.layout(4, 3, widths=unit(c(margin[2], 1, margin[4]), c("lines", "null", "lines")), heights=unit(c(margin[1], 100-ceiling(yfrac*100),ceiling(yfrac*100), margin[3]), c("lines", "null", "null", "lines"))))
	}else{
		top.vp <- viewport(layout=grid.layout(4, 3, widths=unit(c(margin[2], 1, margin[4]), c("lines", "null", "lines")), heights=unit(c(margin[3], ceiling(yfrac*100),100-ceiling(yfrac*100), margin[1]), c("lines", "null", "null", "lines"))))
	}
	set1_marginb <- viewport(layout.pos.col = 2, layout.pos.row = 4, name = ".set_internal_marginb")
	set1_margint <- viewport(layout.pos.col = 2, layout.pos.row = 1, name = ".set_internal_margint")
	set1_marginr <- viewport(layout.pos.col = 3, layout.pos.row = 2, name = ".set_internal_marginr")
	if(flip.vertical){
		set1_marginl <- viewport(layout.pos.col = 1, layout.pos.row = 3, name = ".set_internal_marginl")
		set1_plot1 <- viewport(layout.pos.col = 2, layout.pos.row = 3, name = ".set_internal_plot1")
		set1_plot2 <- viewport(layout.pos.col = 2, layout.pos.row = 2, name = ".set_internal_plot2")
	}else{
		set1_marginl <- viewport(layout.pos.col = 1, layout.pos.row = 2, name = ".set_internal_marginl")
		set1_plot1 <- viewport(layout.pos.col = 2, layout.pos.row = 2, name = ".set_internal_plot1")
		set1_plot2 <- viewport(layout.pos.col = 2, layout.pos.row = 3, name = ".set_internal_plot2")
	}
	splot <- vpTree(top.vp, vpList(set1_marginb, set1_marginl, set1_margint, set1_marginr, set1_plot1, set1_plot2))
	pushViewport(splot)
	upViewport()
	vp1 <- as.character(current.vpPath())
	if(!is.null(title)){
		seekViewport(paste0(vp1, '::.set_internal_margint'))
		grid.text(title, 0.5, 0.5, gp = gpar(cex = cex.title))
		upViewport()
	}
	seekViewport(paste0(vp1, '::.set_internal_plot1'))
	if(enable.debug) grid.rect()
	char.size.h=convertUnit(stringHeight('o'), "npc", "y",valueOnly=TRUE)
	if(nSet==1){
		grid.circle(x=0.5, y=0.5, r=0.3)
		grid.text(otab0[1],0.5,0.5,gp=gpar(cex=cex*2.5))
		upViewport()
		return(invisible())
	}
	# if(nSet==2){
	# #to be modified
	# 	grid.circle(x=0.4, y=0.5, r=0.2)
	# 	grid.circle(x=0.6, y=0.5, r=0.2)
	# 	grid.text(otab0['10'],0.3,0.5,gp=gpar(cex=cex*2.5))
	# 	grid.text(otab0['11'],0.5,0.5,gp=gpar(cex=cex*2.5))
	# 	grid.text(otab0['01'],0.7,0.5,gp=gpar(cex=cex*2.5))
	# 	upViewport(0)
	# 	return(invisible())
	# }	
	yLen=1 #height of y axis
	w=1/nO
	h=yLen/(ylim[2]-ylim[1])
	if((!is.null(bar.split)) && ylim[2] > bar.split[1]){
		bar.split[2]=min(bar.split[2],ylim[2])
		h=yLen/(ylim[2]-ylim[1]-bar.split[2]+bar.split[1])
	}
	#
	if(show.elements){
		elements.rot=45
		elements.cex=0.9
		elements.col=1
		elements.maximum=Inf
		if(!is.null(Args$elements.rot)) elements.rot=Args$elements.rot
		if(!is.null(Args$elements.cex)) elements.cex=Args$elements.cex
		if(!is.null(Args$elements.col)) elements.col=Args$elements.col
		if(!is.null(Args$elements.maximum)) elements.maximum=Args$elements.maximum
		ele.text.posy=ifelse(show.overlap.size || show.fold.enrichment,char.size.h/3,0)+ifelse(elements.rot!=0,char.size.h,0)
	}
	#Plot bars
	for(i in 1:nO){
		posx=w*(i-1)+w/2
		if((!is.null(bar.split)) && otab[i] > bar.split[1]){
			grid.rect(x=posx,y=ifelse(flip.vertical,1.0,0.0),width=w*0.8,height=h*bar.split[1],just=c('center',ifelse(flip.vertical,'top','bottom')),gp=gpar(fill=heatmapColor[cid[i]])) #plot bar
			ytop=h*bar.split[1]
			if(otab[i] > bar.split[2]){
				y1=h*bar.split[1]+char.size.h/3
				if(flip.vertical) y1 <- 1-y1
				grid.rect(x=posx,y=y1,width=w*0.8,height=h*(otab[i]-bar.split[2]),just=c('center',ifelse(flip.vertical,'top','bottom')),gp=gpar(fill=heatmapColor[cid[i]])) #plot bar
				ytop=ytop+char.size.h/3+h*(otab[i]-bar.split[2])
			}
		}else{
			grid.rect(x=posx,y=ifelse(flip.vertical,1.0,0.0),width=w*0.8,height=h*otab[i],just=c('center',ifelse(flip.vertical,'top','bottom')),gp=gpar(fill=heatmapColor[cid[i]])) #plot bar
			ytop=h*otab[i]
		}
		hexp1=0
		if(show.expected.overlap && !is.na(etab[names(otab[i])])){
			hexp1=etab[names(otab[i])]
			if(log.scale) hexp1 = log(hexp1 + 1)
			if((!is.null(bar.split)) && hexp1 > bar.split[1]){
				if(hexp1 > bar.split[2]){
					hexp1=hexp1-bar.split[2]+bar.split[1]
				}else{
					hexp1=bar.split[1]
				}
			}
			pushViewport(viewport(x=posx,y=ifelse(flip.vertical,1-h*hexp1/2,h*hexp1/2),width=w*0.8,height = h*hexp1,just = "centre"))
			if(expected.overlap.style=="horizBar"){
				grid.lines(x=c(0,1),y=c(1,1)-ifelse(flip.vertical,1,0),gp=gpar(col=color.expected.overlap,alpha=alpha.expected.overlap,lwd=expected.overlap.lwd))
			}else{
				#plot expected overlap in hatched lines
				if(expected.overlap.style=="hatchedBox"){
					for(ihatch in seq(1,ceiling(hexp1),2)) grid.abline(intercept=(ihatch-1)/ceiling(hexp1), slope=1/ceiling(hexp1),gp=gpar(col=color.expected.overlap,alpha=alpha.expected.overlap))
				}
				grid.rect(x=0.5,y=0.5,width=1,height=1,just=c('center'),gp=gpar(col=color.expected.overlap,alpha=alpha.expected.overlap,lwd=expected.overlap.lwd,fill=NA))
			}
			popViewport(1)
		}
		ytop=max(ytop,hexp1*h)
		if(flip.vertical) ytop <- 1-ytop
		if(show.overlap.size) grid.text(otab0[i],posx,ytop+ifelse(flip.vertical,-3,1) * char.size.h/3,gp=gpar(cex=overlap.size.cex),vjust=0)
		if(show.fold.enrichment && !is.na(fetab[names(otab0)[i]])) grid.text(fetab[names(otab0)[i]],posx,ytop+ifelse(flip.vertical,-3,1) * char.size.h/3,gp=gpar(cex=overlap.size.cex),vjust=0)
		if(show.elements){
			s00=sumtable[names(otab0)[i],'Elements']
			if(is.null(s00) || is.na(s00)) next
			elements=strsplit(s00,', ')[[1]]
			if(length(elements) > elements.maximum) elements=c(elements[1:elements.maximum],'...')
			for(j1 in 1:length(elements)) grid.text(elements[j1],posx,ytop+ifelse(flip.vertical,-1,1)*((char.size.h+ifelse(elements.rot!=0,char.size.h/3,0))*j1+ele.text.posy),gp=gpar(cex=elements.cex,col=elements.col),rot=elements.rot,vjust=0.5)
		}
	}
	#plot y axis
	atVal=(ylabel-ylim[1])*h
	if(!is.null(bar.split)){
		ylabel0=ylabel0[ylabel <= bar.split[1] | ylabel >= bar.split[2]]
		ylabel1=ylabel[ylabel <= bar.split[1] | ylabel >= bar.split[2]]
		atVal=(ylabel1-ylim[1])*h
		for(i in 1:length(ylabel1)){
			ytop=h*(min(ylabel1[i],bar.split[1])-ylim[1])
			if(ylabel1[i] > bar.split[2]){
				ytop=ytop+char.size.h/3+h*(ylabel1[i]-bar.split[2])
			}
			atVal[i]=ytop
		}
		char.size.w=convertUnit(stringWidth('0'), "npc", "x",valueOnly=TRUE)
		atVal=atVal[! ylabel0 %in% bar.split]
		ylabel0=ylabel0[! ylabel0 %in% bar.split]
		if(flip.vertical){
			yaxis1=grid.yaxis(at=1-atVal,label=ylabel0,gp=gpar(cex=cex))
		}else{
			yaxis1=grid.yaxis(at=atVal,label=ylabel0,gp=gpar(cex=cex))
		}
		if(ylim[2]>bar.split[2]){
			if(flip.vertical){
				yaxis1=grid.yaxis(at=1-((bar.split[1]-ylim[1])*h-char.size.h/2),label=bar.split[1],gp=gpar(cex=cex),name="ya")
				yaxis1=grid.yaxis(at=1-((bar.split[1]-ylim[1])*h+char.size.h),label=bar.split[2],gp=gpar(cex=cex),name="yb")
			}else{
				yaxis1=grid.yaxis(at=(bar.split[1]-ylim[1])*h-char.size.h/2,label=bar.split[1],gp=gpar(cex=cex),name="ya")
				yaxis1=grid.yaxis(at=(bar.split[1]-ylim[1])*h+char.size.h,label=bar.split[2],gp=gpar(cex=cex),name="yb")				
			}
			# grid.remove(gPath("ya", "ticks"))
			# grid.remove(gPath("ya", "major"))
			# grid.remove(gPath("yb", "ticks"))
			# grid.remove(gPath("yb", "major"))
			if(flip.vertical){
				grid.lines(x = c(0),y = 1-c((bar.split[1]-ylim[1])*h,(bar.split[1]-ylim[1])*h+char.size.h/3),gp=gpar(col='white')) #make a gap
				grid.lines(x = c(-char.size.w,0),y = 1-(bar.split[1]-ylim[1])*h,gp=gpar(col='grey'))
				grid.lines(x = c(-char.size.w,0),y = 1-((bar.split[1]-ylim[1])*h+char.size.h/3),gp=gpar(col='grey'))
			}else{
				grid.lines(x = c(0),y = c((bar.split[1]-ylim[1])*h,(bar.split[1]-ylim[1])*h+char.size.h/3),gp=gpar(col='white')) #make a gap
				grid.lines(x = c(-char.size.w,0),y = (bar.split[1]-ylim[1])*h,gp=gpar(col='grey'))
				grid.lines(x = c(-char.size.w,0),y = (bar.split[1]-ylim[1])*h+char.size.h/3,gp=gpar(col='grey'))
			}
		}
	}else{
		if(flip.vertical){
			yaxis1=grid.yaxis(at=1-atVal,label=ylabel0,gp=gpar(cex=cex))
		}else{
			yaxis1=grid.yaxis(at=atVal,label=ylabel0,gp=gpar(cex=cex))
		}
	}
	#plot y axis label
	seekViewport(paste0(vp1, '::.set_internal_marginl'))
	if(enable.debug) grid.rect()
	char.size.w=convertUnit(stringWidth('0'), "npc", "x",valueOnly=TRUE)
	grid.text(ylab, x = 1-convertX(unit(1,'lines'), 'npc', valueOnly = TRUE)-2*char.size.w * max(nchar(ylabel0)), rot=90, gp=gpar(cex=cex.lab))

	#plot color scale
	seekViewport(paste0(vp1, '::.set_internal_plot1'))
	if((!is.null(x$n)) & (! is.null(mlogp))){
		if(is.character(color.scale.pos)){
			if(color.scale.pos == 'topright'){
				y.vp=0.95
				x.vp=0.9
			}else if (color.scale.pos == 'topleft'){
				y.vp=0.95
				x.vp=0.1
			}else{
				stop('Invalid color.scale.pos\n')
			}
		}else if(is.numeric(color.scale.pos)){
			x.vp=color.scale.pos[1]
			y.vp=color.scale.pos[2]
		}else{
			stop('Invalid color.scale.pos\n')
		}
		vp11 <- viewport(x=x.vp, y=ifelse(flip.vertical,1-y.vp,y.vp), width=0.2, height=0.1)
		pushViewport(vp11)
		grid.text(color.scale.title,0.5,0.75,just=c('center','bottom'),gp=gpar(cex=color.scale.cex))
		wc=1/length(heatmapColor)
		for(i in 2:length(heatmapColor)){
			grid.polygon(c((i-1)*wc,i*wc,i*wc,(i-1)*wc),c(0.45,0.45,0.7,0.7),gp=gpar(fill=heatmapColor[i],col=NA))
		}
		t1=params$minMinusLog10PValue
		t2=params$maxMinusLog10PValue
		grid.text(t1,0,0.3,just=c('center','top'),gp=gpar(cex=color.scale.cex))
		grid.lines(x = c(wc, wc),y = c(0.45, 0.35))
		grid.text(t2,1,0.3,just=c('center','top'),gp=gpar(cex=color.scale.cex))
		grid.lines(x = c(1-wc, 1-wc),y = c(0.45, 0.35))
		t3=(t1+t2)/2
		if(t3-t1>2){
			grid.text(as.integer(t3),0.5,0.3,just=c('center','top'),gp=gpar(cex=color.scale.cex))
			grid.lines(x = c(0.5, 0.5),y = c(0.45, 0.35))
		}
		upViewport()
	}

	#sub canvas 2, plot intersection matrix
	seekViewport(paste0(vp1, '::.set_internal_plot2'))
	if(enable.debug) grid.rect()
	h=0.9/nSet
	for(i in 1:nO){
		posx=w*(i-1)+w/2
		for(j in 1:nSet){
			vpJ <- viewport(x=posx, y=0.01+(j-0.5)*h, width=w*0.8, height=h*0.75)
			pushViewport(vpJ)
			if(substr(names(otab[i]),j,j)=='1'){
				grid.circle(x=0.5,y=0.5,r=circle.radii,gp=gpar(fill=color.on[j]))
			}else{
				grid.circle(x=0.5,y=0.5,r=circle.radii,gp=gpar(fill=color.off))
			}
			upViewport()
		}
	}
	for(j in 1:nSet){
		grid.text(x$set.names[j], 0.1*w, 0.015+(j-0.5)*h,just=c('right','center'),gp=gpar(cex=cex))
		if(show.set.size==TRUE) grid.text(x$set.sizes[j], 1, 0.015+(j-0.5)*h,just=c('left','center'),gp=gpar(cex=cex))
	}
	upViewport()
	return(invisible())
}
plot.msets.circular=function(x,degree=NULL,keep.empty.intersections=TRUE,sort.by=c('set','size','degree','p-value'),min.intersection.size=0,max.intersection.size=Inf,
	ylim=NULL,log.scale=FALSE,color.scale.pos=c(0.85, 0.9), legend.pos=c(0.85,0.25),legend.col=2,legend.text.cex=1,
	color.scale.cex=1,color.scale.title=expression(paste(-Log[10],'(',italic(P),')')),color.on='#2EFE64',color.off='#EEEEEE',
	show.overlap.size=TRUE,show.fold.enrichment=FALSE,overlap.size.cex=0.9,margin=NULL,track.area.range=0.3,bar.area.range=0.2,
	new.gridPage=TRUE,minMinusLog10PValue=0,maxMinusLog10PValue=NULL,...){
	#
	if(is.character(legend.pos)){
		legend.pos <- legend.pos[1]
	}else if(is.numeric(legend.pos)){
		if(length(legend.pos)!=2) stop('Invalid legend.pos\n')
	}else{
		stop('Invalid legend.pos\n')
	}
	if(is.null(margin)) margin=c(1,1,1,2)+0.1
	Args=list(...)
	title = Args$title
	cex.title = Args$cex.title
	if(is.null(cex.title)) cex.title = 1
	cex=ifelse(is.null(Args$cex),0.8,Args$cex)
	show.track.id=ifelse(is.null(Args$show.track.id),TRUE,Args$show.track.id)
	intersection.size.rotate=ifelse(is.null(Args$intersection.size.rotate),TRUE,Args$intersection.size.rotate)
	if(show.fold.enrichment){
		fetab=sprintf("%.1f",x$overlap.sizes/x$overlap.expected)
		fetab[fetab=='NA']=NA
		names(fetab)=names(x$overlap.sizes)
	}
	#set color scheme
	if(is.null(Args$heatmapColor)){
		heatmapColor = rev(heat.colors(50)[1:50])
	}else{
		heatmapColor = Args$heatmapColor
	}
	nColors=length(heatmapColor)-1
	params=getPlotParams(x,nColors,degree=degree,keep.empty.intersections=keep.empty.intersections,sort.by=sort.by,min.intersection.size=min.intersection.size,max.intersection.size=max.intersection.size,ylim=ylim,log.scale=log.scale,minMinusLog10PValue=minMinusLog10PValue,maxMinusLog10PValue=maxMinusLog10PValue)
	ylim=params$ylim
	otab=params$otab
	otab[otab>ylim[2]]=ylim[2]
	otab[otab<ylim[1]]=ylim[1]
	otab=otab-ylim[1]
	otab0=params$otab0
	nO=length(otab)
	nSet=length(x$set.sizes) #number of sets
	cid=params$cid
	mlogp=params$mlogp
	radial2deg=180/pi
	if(is.null(color.on)) color.on = c("#386cb0", "#fdb462", "#7fc97f", "#ef3b2c", "#662506", "#a6cee3", "#fb9a99", "#984ea3", "#ffff33", "#00798c", "#d1495b", "#66a182", "#2e4057")
	if(length(color.on) < nSet) color.on = rep(color.on, length.out = nSet)
	# set graph layout parameters
	track.area.range=as.numeric(track.area.range)
	bar.area.range=as.numeric(bar.area.range)
	if(track.area.range  < 0) stop('Invalid argument "trak.area.range"\n')
	if(bar.area.range  < 0) stop('Invalid argument "bar.area.range"\n')
	if(track.area.range+bar.area.range>0.5) stop('Sum of track.area.range and bar.area.range should not be larger than 0.5\n')
	width.sets=track.area.range
	width.intersections=bar.area.range
	track.offset=ifelse(is.null(Args$phantom.tracks),2,as.integer(Args$phantom.tracks)) #number of phantom tracks in the middle
	track.width=width.sets/(nSet+track.offset)
	bar.width.unit=width.intersections/(ylim[2]-ylim[1])
	gap.within.track=ifelse(is.null(Args$gap.within.track),0.1,Args$gap.within.track) #ratio of gap width over block width on the same track
	gap.between.track=ifelse(is.null(Args$gap.between.track),0.1,Args$gap.between.track) #ratio of gap width over track width

	#start a canvas
	if(new.gridPage){
		grid.newpage()
	}else{
		vp0 = as.character(current.vpPath())
		on.exit(seekViewport(vp0))
	}
	top.vp <- viewport(layout=grid.layout(3, 3, widths=unit(c(margin[2], 1, margin[4]), c("lines", "null", "lines")), heights=unit(c(margin[3], 1, margin[1]), c("lines", "null", "lines"))))
	set2_margin1 <- viewport(layout.pos.col = 2, layout.pos.row = 3, name = ".set_internal2_marginb")
	set2_margin2 <- viewport(layout.pos.col = 1, layout.pos.row = 2, name = ".set_internal2_marginl")
	set2_margin3 <- viewport(layout.pos.col = 2, layout.pos.row = 1, name = ".set_internal2_margint")
	set2_margin4 <- viewport(layout.pos.col = 3, layout.pos.row = 2, name = ".set_internal2_marginr")
	set2_plot1 <- viewport(layout.pos.col = 2, layout.pos.row = 2, name = ".set_internal2_plot1")
	splot <- vpTree(top.vp, vpList(set2_margin1, set2_margin2, set2_margin3, set2_margin4, set2_plot1))
	pushViewport(splot)
	upViewport()
	vp1 <- as.character(current.vpPath())
	#
	if(!is.null(title)){
		seekViewport(paste0(vp1, '::.set_internal2_margint'))
		grid.text(title, 0.5, 0.5, gp = gpar(cex = cex.title))
		upViewport()
	}
	#Plot tracks
	seekViewport(paste0(vp1, '::.set_internal2_plot1'))
	origin=c(0.5,0.5)
	degreeUnit=2*pi/nO
	degreeStart=(c(1:nO)-1)*degreeUnit
	degreeEnd=(c(1:nO))*degreeUnit
	degree.gap=max(degreeUnit*gap.within.track,2*pi/360)
	char.size.h=as.numeric(convertUnit(stringHeight('o'), "npc", "y"))
	fill.col=rep(c('#dddddd','#999999'),length.out=nSet)
	#plot overlap (intersection)
	for(i in 1:nO){
		which.set=strsplit(names(otab)[i],'')[[1]]=='1'
		for(j in 1:nSet){
			XY1=sapply(seq(degreeStart[i],degreeEnd[i]-degree.gap,length.out=40), function(deg) getXY(origin,(j+track.offset-1)*track.width,deg))
			XY2=sapply(seq(degreeStart[i],degreeEnd[i]-degree.gap,length.out=40), function(deg) getXY(origin,(j+track.offset)*track.width-track.width*gap.between.track,deg))
			pos.x <- c(XY1[1,],rev(XY2[1,]))
			pos.y <- c(XY1[2,],rev(XY2[2,]))
			grid.polygon(pos.x, pos.y,gp=gpar(col = 'black',fill=ifelse(which.set[j],color.on[j],color.off))) #
		}
		#bar plot intersection size
		XY1=sapply(seq(degreeStart[i],degreeEnd[i]-degree.gap,length.out=40), function(deg) getXY(origin,width.sets,deg))
		XY2=sapply(seq(degreeStart[i],degreeEnd[i]-degree.gap,length.out=40), function(deg) getXY(origin,width.sets+bar.width.unit*otab[i],deg))
		pos.x=c(XY1[1,],rev(XY2[1,]))
		pos.y=c(XY1[2,],rev(XY2[2,]))
		grid.polygon(pos.x, pos.y,gp=gpar(fill = heatmapColor[cid[i]],col=1)) #bar height is proportional to the intersection size
		#text intersection size
		if(show.overlap.size==TRUE || show.fold.enrichment==TRUE){
			XY3=sapply(seq(degreeStart[i],degreeEnd[i]-degree.gap,length.out=4), function(deg) getXY(origin,width.sets+bar.width.unit*otab[i]+char.size.h,deg))
			if(show.overlap.size==TRUE) grid.text(otab0[i],mean(XY3[1,]),mean(XY3[2,]),rot=ifelse(intersection.size.rotate,(degreeStart[i]-pi/2)*radial2deg,0),just='center',gp=gpar(cex=overlap.size.cex))
			if(show.fold.enrichment==TRUE && !is.na(fetab[names(otab0)[i]])) grid.text(fetab[names(otab0)[i]],mean(XY3[1,]),mean(XY3[2,]),rot=ifelse(intersection.size.rotate,(degreeStart[i]-pi/2)*radial2deg,0),just='center',gp=gpar(cex=overlap.size.cex))
		}
	}
	#track number (numbering the legends)
	if(show.track.id) for(j in 1:nSet){
		XY1=sapply(seq(degreeStart[1],degreeEnd[1]-degree.gap,length.out=40), function(deg) getXY(origin,(j+track.offset-1)*track.width,deg))
		XY2=sapply(seq(degreeStart[1],degreeEnd[1]-degree.gap,length.out=40), function(deg) getXY(origin,(j+track.offset)*track.width,deg))
		grid.text(j,(XY1[1,1]+XY2[1,1])/2,mean(XY1[2,]),just=c('center'),gp=gpar(cex=cex))
	}

	#plot color scale
	if((!is.null(x$n)) & (! is.null(mlogp))){
		if(is.character(color.scale.pos)){
			if(color.scale.pos == 'topright'){
				y.vp=0.90
				x.vp=0.85
			}else if(color.scale.pos == 'bottomright'){
				y.vp=0.15
				x.vp=0.85
			}else if (color.scale.pos == 'topleft'){
				y.vp=0.90
				x.vp=0.15
			}else if(color.scale.pos == 'bottomleft'){
				y.vp=0.15
				x.vp=0.15
			}else{
				stop('Invalid color.scale.pos\n')
			}
		}else if(is.numeric(color.scale.pos)){
			x.vp=color.scale.pos[1]
			y.vp=color.scale.pos[2]
		}else{
			stop('Invalid color.scale.pos\n')
		}
		vp11 <- viewport(x=x.vp, y=y.vp, width=0.2, height=0.1)
		pushViewport(vp11)
		grid.text(color.scale.title,0.5,0.75,just=c('center','bottom'),gp=gpar(cex=color.scale.cex))
		wc=1/length(heatmapColor)
		for(i in 2:length(heatmapColor)){
			grid.polygon(c((i-1)*wc,i*wc,i*wc,(i-1)*wc),c(0.45,0.45,0.7,0.7),gp=gpar(fill=heatmapColor[i],col=NA))
		}
		t1=params$minMinusLog10PValue #0 #floor(min(mlogp,na.rm=T));
		t2=params$maxMinusLog10PValue #ceiling(max(mlogp,na.rm=T))
		grid.text(t1,0,0.3,just=c('center','top'),gp=gpar(cex=color.scale.cex))
		grid.lines(x = c(wc, wc),y = c(0.45, 0.35))
		grid.text(t2,1,0.3,just=c('center','top'),gp=gpar(cex=color.scale.cex))
		grid.lines(x = c(1-wc/2, 1-wc/2),y = c(0.45, 0.35))
		t3=(t1+t2)/2
		if(t3-t1>2){
			grid.text(as.integer(t3),0.5,0.3,just=c('center','top'),gp=gpar(cex=color.scale.cex))
			grid.lines(x = c(0.5, 0.5),y = c(0.45, 0.35))
		}
		upViewport()
	}

	#plot legend for set names
	if(is.character(legend.pos)){
		if(legend.pos == 'topright'){
			y.vp=0.80
			x.vp=0.85
		}else if(legend.pos == 'bottomright'){
			y.vp=0.25
			x.vp=0.85
		}else if (legend.pos == 'topleft'){
			y.vp=0.80
			x.vp=0.15
		}else if(legend.pos == 'bottomleft'){
			y.vp=0.25
			x.vp=0.15
		}else{
			stop('Invalid legend.pos\n')
		}
	}else if(is.numeric(legend.pos)){
		x.vp=legend.pos[1]
		y.vp=legend.pos[2]
	}else{
		stop('Invalid legend.pos\n')
	}
	legend.box.width=0.2
	legend.box.height=0.2
	vp2 <- viewport(x=x.vp, y=y.vp, width=legend.box.width, height=legend.box.height)
	pushViewport(vp2)
	Legend <- frameGrob()
	Legend <- packGrob(Legend, grid.legend(paste(1:nSet,x$set.names,sep=': '),ncol=legend.col,
		vgap=ifelse(is.null(Args$legend.vgap),0.1*legend.text.cex,Args$legend.vgap),hgap=ifelse(is.null(Args$legend.hgap),0.2*legend.text.cex,Args$legend.hgap),
		gp=gpar(cex=legend.text.cex), draw = FALSE), height = unit(1, "null"),side = ifelse(x.vp>0.5,"right",'left'))
	grid.draw(Legend)
	upViewport()
	upViewport()
	return(invisible())
}
getXY=function(origin,radius,degree){
	X=radius*cos(degree)
	Y=radius*sin(degree)
	origin+c(X,Y)
}
getPlotParams=function(x,nColors=50,degree=NULL,keep.empty.intersections=TRUE,sort.by=c('set','size','degree','p-value'),min.intersection.size=0,max.intersection.size=Inf,ylim=NULL,log.scale=FALSE,Layout=c('circular','landscape'),minMinusLog10PValue=0,maxMinusLog10PValue=NULL,show.expected.overlap=FALSE){
	Layout=match.arg(Layout)
	if(is.null(x$overlap.expected)) show.expected.overlap=FALSE
	etab=c()
	otab=x$overlap.sizes
	if(sort.by[1] %in% c('set','size','degree','p-value')){
		sort.by = sort.by[1]
		if(sort.by=='set'){
			otab.order=order(names(otab))
		}else if(sort.by=='size'){
			otab.order=order(otab,decreasing=TRUE)
		}else if(sort.by=='degree'){
			otab.order=order(sapply(names(otab),function(d) countCharOccurrences('1',d))) #order(sapply(strsplit(names(otab),''),function(d) sum(d=='1')))
		}else if(sort.by=='p-value'){
			otab.order=order(x$P.value)
		}else{
			stop('Invalid sort.by argument\n')
		}
	}else{
		otab.order=match(sort.by,names(otab))
		if(any(is.na(otab.order))) stop(paste0('Unrecognized values found in sort.by: ',paste(sort.by[is.na(otab.order)],collapse=', '),'\n'))
	}
	otab=otab[otab.order]
	if(keep.empty.intersections==FALSE) otab=otab[otab>0]
	otab=otab[otab >= min.intersection.size & otab <= max.intersection.size]
	if(!is.null(degree)){
		kpt=sapply(names(otab),function(d) countCharOccurrences('1',d)) %in% degree #sapply(strsplit(names(otab),''),function(d) sum(d == '1') %in% degree)
		if(sum(kpt)<2) stop('Too few items left for plotting after applying degree filter\n')
		otab=otab[kpt]
	}
	otab0=otab
	if(show.expected.overlap) etab=x$overlap.expected[names(otab)]
	if(is.null(ylim)){
		ylabel=axisTicks(c(0,max(c(etab,otab0),na.rm=TRUE)),log=FALSE) #for landscape layout
		ylim=c(0,max(otab,na.rm=TRUE))
		if(Layout=='landscape' && (ylim[2]-max(ylabel))/(ylabel[2]-ylabel[1])>0.5) {ylabel=c(ylabel,ylabel[length(ylabel)]+ylabel[2]-ylabel[1]);ylim[2]=ylabel[length(ylabel)]}
	}else{
		ylabel=axisTicks(ylim[1:2],log=FALSE)
	}
	if(ylim[2]<ylim[1]) stop("Invalid ylim\n")
	ylabel0=ylabel
	if(log.scale==TRUE){
		if(any(ylim < 0)) stop("ylim can't be negative when log.scale is TRUE\n")
		otab=log(otab+1);
		ylim=log(ylim+1)
		ylabel=log(ylabel+1)
	}
	nO=length(otab)
	cid=rep(1,nO) #color gradient id
	mlogp=NULL
	if((!is.null(x$n)) && nO>0){
		mlogp=-log(x$P.value[names(otab)], base=10)
		mlogp[is.na(mlogp)]=1e-10
		mlogp[mlogp == Inf] = max(320,mlogp[mlogp < Inf],na.rm=TRUE)
		if(is.null(maxMinusLog10PValue)) maxMinusLog10PValue=ceiling(max(c(mlogp,1e-10),na.rm=TRUE))
		maxMinusLog10PValue=ceiling(maxMinusLog10PValue)
		minMinusLog10PValue=floor(minMinusLog10PValue)
		mlogp[mlogp>maxMinusLog10PValue]=maxMinusLog10PValue
		cid=ceiling(nColors*(mlogp-minMinusLog10PValue)/(maxMinusLog10PValue-minMinusLog10PValue))
		cid[cid<1]=1
		if(all(is.na(mlogp))) mlogp=NULL
	}
	return(list(otab=otab,otab0=otab0,cid=cid,mlogp=mlogp,ylim=ylim,ylabel=ylabel,ylabel0=ylabel0,minMinusLog10PValue=minMinusLog10PValue,maxMinusLog10PValue=maxMinusLog10PValue))
}
