#!/usr/bin/env python

import math
from gimpfu import *

def path_to_HQ(self, timg, minFlow, maxFlow, minHead, maxHead, step):
		# Identify image and get path
		img = gimp.image_list()[0]
		name = pdb.gimp_path_get_current(img)
		v = pdb.gimp_path_get_points(img,name)
		p=v[3]

		# get path anchors
		xx = range(0,len(p),9)
		yy = range(1,len(p),9)

		# rotate
		n = len(xx)

		xQ = p[xx[n-3]]
		x0 = p[xx[n-2]]
		xH = p[xx[n-1]]

		yQ = p[yy[n-3]]
		y0 = p[yy[n-2]]
		yH = p[yy[n-1]]

		dy = (yQ-y0)
		dx = (xQ-x0)
		angQ = -math.atan(dy/dx)

		dy = (yH-y0)
		dx = (xH-x0)
		angH = math.atan(dx/dy)

		ang = (angQ+angH)/2

		xr = range(0,n-3,1)
		yr = range(0,n-3,1)

		for i in range(0,n-3,1):
			xr[i] = (p[xx[i]] - x0) * math.cos(ang) - (p[yy[i]] - y0) * math.sin(ang)   + x0
			yr[i] = (p[xx[i]] - x0) * math.sin(ang) + (p[yy[i]] - y0) * math.cos(ang)   + y0

			
		Qr = (xQ - x0) * math.cos(ang) - (yQ - y0) * math.sin(ang) + x0
		Hr = (xH - x0) * math.sin(ang) + (yH - y0) * math.cos(ang) + y0

		# Unit Flow/Head
		pxQ = maxFlow/(Qr-x0)
		pxH = (maxHead-minHead)/(Hr-y0)

		
		text = range(0,n-3,1)
		
		# Pump Curve
		for i in range(0,n-3,1):
			text[i] = round((xr[i]-x0)*pxQ/step,0)*step, round((yr[i]-y0)*pxH,0)+minHead, "     "
			
				
		pdb.gimp_message(text)
		

register(
        "Path_to_HQ",
        "Transform a specified path along a pump curve to discrete H-Q values",
        "Transform a specified path along a pump curve to discrete H-Q values",
        "D. Craig Jones",
        "D. Craig Jones",
        "2018",
        "<Image>/Filters/Image to H-Q Table",
        "RGB*, GRAY*",
        [
                (PF_INT, "minFlow", "Minimum Flow (GPM)", 0),
                (PF_FLOAT, "maxFlow", "Maximum Flow (GPM)", 1000),
				(PF_INT, "minHead", "Minimum Head (FT)", 0),
                (PF_FLOAT, "maxHead", "Maximum Head (FT)", 100),
                (PF_INT, "step", "Rounding", 10)
        ],
        [],
        path_to_HQ)

main()
