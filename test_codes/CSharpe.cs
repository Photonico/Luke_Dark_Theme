using System;
using System.Timers;

namespace moose
{
    class Program
    {
        public static Timer aTimer = new Timer();
        public static int num = 1;
        public static int num_x = 1;

        public static int num_max = 20;
        static void Main(string[] args)
        {
            Console.WriteLine("请输入要拖动的窗口类名！--不填写则默认当前窗口");

            String boxName = Console.ReadLine();
            if(boxName==""){
                boxName ="ConsoleWindowClass";
            }
            Console.WriteLine("成功-当前窗口类名"+boxName);
            Console.WriteLine("请输入要循环拖动的次数，大于0的数字！");
            num_max = int.Parse(Console.ReadLine());
            if(num_max<=0) num_max =20;
            Console.WriteLine("次数"+num_max+"开始执行");

            IntPtr maindHwnd = MouseHookHelper.FindWindow(boxName, null);
            MouseHookHelper.RECT rect;
            MouseHookHelper.GetWindowRect(maindHwnd,out rect);

            MouseHookHelper.SetCursorPos(rect.Left + 60, rect.Top + 10);
            MouseHookHelper.mouse_event(MouseHookHelper.MOUSEEVENTF_LEFTDOWN,100,1,0,0);
            SetTimerParam();
            Console.ReadLine();
        }
        private void sdf(){
            
        }

        private static void test(object source, System.Timers.ElapsedEventArgs e)  
        {       
            MouseHookHelper.mouse_event(MouseHookHelper.MOUSEEVENTF_MOVE,0,num,0,0);   

            if(num==40)
            {
                num = -1;
                num_x++;
            }

            if(num == -40)
            {
                num =1;
                num_x++;
            }

            if(num_x%2 == 0)
            {
                num --;
            }
            else
            {
                num ++;
            }
            
            Console.WriteLine(num+"____"+num_x);
            if(num_x >=num_max){
                aTimer.Stop();
                MouseHookHelper.mouse_event(MouseHookHelper.MOUSEEVENTF_LEFTUP,0,0,0,0);
            }
        }  

        public static void SetTimerParam()  
        {  
            aTimer.Elapsed += new ElapsedEventHandler(test);  
            aTimer.Interval = 1;  
            aTimer.AutoReset = true;
            aTimer.Enabled = true;  
        }  
    }
}
