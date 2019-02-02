import java.applet.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.lang.*;

public class Compact extends Applet
{
    Image offImage;
    Graphics offGraphics;
    int xnew, ynew;
    int last_x, last_y;
    int last_xnew, last_ynew;
    int xscale, yscale;
    int xscale_tri, yscale_tri;
    int leftx, lefty;
    int rightx, righty;
    boolean continuity;
    boolean inverse_continuity;
    final int n = 30;
    public void init()
    {
        offImage = null;
        continuity = inverse_continuity = true;
    }

    public void compute(int x, int y)
    {
        double X = ((double)x-leftx)/xscale * 3;
        double Y = (lefty-(double)y)/yscale * 3;
        double U = Math.atan(X+Y);
        double V = Math.atan(X-Y);
        double Xnew=(U+V)/Math.PI, Ynew=(U-V)/Math.PI;
        xnew = rightx + (int)(Xnew * xscale_tri);
        ynew = righty - (int) (Ynew * yscale_tri);
    }
  
    public void compute_inverse(int x, int y)
    {
        double X = ((double)x-rightx)/xscale_tri * Math.PI;
        double Y = (righty-(double)y)/yscale_tri * Math.PI;
        double U = Math.tan((X+Y)/2);
        double V = Math.tan((X-Y)/2);
        double Xnew=(U+V)/2, Ynew=(U-V)/2;
        xnew = leftx + (int)(Xnew * xscale) / 3;
        ynew = lefty - (int)(Ynew * yscale) / 3;
    }

    public void doGrid(Graphics g, int x, int y, int xscale, int yscale)
    {
        g.setColor(Color.black);
        g.drawLine(x, y, x+xscale,y);
        g.drawLine(x,y-yscale,x,y);
        int i,j;
        for (i = 0; i <= n; i++)
        {
            g.drawLine(x-3, y-yscale*i/n, x+3, y-yscale*i/n);
            g.drawLine(x+xscale*i/n, y-3, x+xscale*i/n, y+3);
        }

        for (i = 1; i <= n; i++)
        for (j = 1; j <= n; j++)
        {
            g.drawRect(x+xscale*i/n,y-yscale*j/n,1,1);
        }
    }

    public void doTriangle(Graphics g, int x, int y, int xscale, int yscale)
    {
        g.setColor(Color.black);
        g.drawLine(x, y, x+xscale,y);
        g.drawLine(x,y-yscale,x,y);
        g.drawLine(x,y-yscale,x+xscale,y); 
        int i,j;
        for (i = 1; i <= n; i++)
        for (j = 1; j <= n; j++)
            if (i+j < n)
            {
                g.drawRect(x+xscale*i/n,y-yscale*j/n,1,1);
            }
    }

    public void drawArrow(int x, int y, int xn, int yn, Color color)
    {
        Graphics g = this.getGraphics();
        g.setColor(color);
        g.clipRect(x,y-yscale,xscale,yscale);
        g.drawLine(x,y,xn,yn);
    }

    public boolean inside(int x, int y, int xc, int yc)
    {
        return !(x < xc || x >= xc + xscale ||
            y <= yc - yscale || y > yc);
    }
  
    public boolean inside_tri(int x, int y, int xc, int yc)
    {
        return !(x < xc || (x-xc) + (yc-y) >= xscale_tri || y > yc);
    }

    public boolean keyDown(Event e, int key)
    {
        offImage.flush();
        offGraphics.dispose();
        offImage = null;
        paint(this.getGraphics());

        return true;
    }
  
    public boolean mouseDown(Event e, int x, int y)
    {
        if (inside(x,y,leftx,lefty))
        {
            continuity = true;
            compute(x,y);
            last_x = x; last_y = y; 
            last_xnew = xnew; last_ynew = ynew;
            return true;
        }
        else continuity = false;
        if (inside_tri(x,y,rightx,righty))
        {
            inverse_continuity = true;
            compute_inverse(x,y);
            last_x = x; last_y = y; 
            last_xnew = xnew; last_ynew = ynew;
            return true;
        }
        else inverse_continuity = false;
        return false;
    }
 
    public boolean mouseDrag(Event e, int x, int y)
    {
        if (inside(x,y,leftx,lefty))
        {
            compute(x,y);
            if (continuity)
            {
                Graphics g = offImage.getGraphics();
                g.setColor(Color.red);
                g.drawLine(last_x,last_y,x,y);
                g.drawLine(last_xnew,last_ynew,xnew,ynew);
            }
            mouseMove(e,x,y);
            last_x = x; last_y = y;
            last_xnew = xnew; last_ynew = ynew;
            continuity = true;
            return true;
        }
        else continuity = false;
    
        if (inside_tri(x,y,rightx,righty))
        {
            compute_inverse(x,y);
                if (inverse_continuity)
                {
                    Graphics g = offImage.getGraphics();
                    g.setColor(Color.red);
                    g.drawLine(last_x,last_y,x,y);
                    g.clipRect(leftx,lefty-yscale,xscale,yscale);
                    g.drawLine(last_xnew,last_ynew,xnew,ynew);
                }
            mouseMove(e,x,y);
            last_x = x; last_y = y;
            last_xnew = xnew; last_ynew = ynew;
            inverse_continuity = true;
            return true;
        }
        else inverse_continuity = false;
        return false;
    }

    public boolean mouseMove(Event e, int x, int y)
    {
    if (inside(x,y,leftx,lefty))
    {
        Graphics g = this.getGraphics();
        g.drawImage(offImage,0,0,this);
        compute(x,y);
        drawArrow(leftx, lefty, x, y, Color.blue);
        drawArrow(rightx, righty, xnew, ynew, Color.blue); 
        return true;
    }

    if (inside_tri(x,y,rightx,righty))
    {
        Graphics g = this.getGraphics();
        g.drawImage(offImage,0,0,this);
        compute_inverse(x,y);
        drawArrow(rightx, righty, x, y, Color.blue);
        drawArrow(leftx, lefty, xnew, ynew, Color.blue); 
        return true;
    }
        return false;
    }

    public void paint(Graphics g)
    {
        if (offImage == null) 
        {
            offImage = createImage(size().width, size().height);
            offGraphics = offImage.getGraphics();
            xscale = size().width - 280;
            yscale = size().height - 5;
            lefty = 1 + yscale;
            leftx = 5;
            rightx = size().width - 280;
            righty = size().height/2 + 160/2;
            if (yscale > xscale) yscale = xscale;
            if (xscale > yscale) xscale = yscale;
            xscale_tri = yscale_tri = 280;
            doGrid(offGraphics, leftx, lefty, xscale, yscale);
            doTriangle(offGraphics, rightx, righty, xscale_tri, yscale_tri);
        }
        g.drawImage(offImage,0,0,this);
    }

}
