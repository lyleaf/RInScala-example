/*
This is a test to see if R works with on your computer. 
It can also be used as an example as to see how we can use R in scala.
*/
import org.ddahl.jvmr.RInScala

object RInScalaTest {
    def main(args: Array[String]) {
       val R = RInScala()
      

      R>"""
          library("forecast")
          forecast_ets <- function(timeList,revList) {
            df <- data.frame(time=timeList,rev=revList)
            ts.month <- ts(data=df[["rev"]],end=c(2014,7),f=12)
            ets.month <- ets(ts.month, model='AAA')
            forecast = unclass(forecast(ets.month)$mean)
            #png(filename=paste("~/time_series/ts_python_v3.2/images/","rev","_","origin","_","destination","_ets",".png"))
            plot(forecast(ets.month),main=paste("rev"," forecast for ","origin" ,"-","destination"," (Exponential Smoothing)"),
                ylab="rev", xlab="Year")
            grid()
            dev.off()
            AIC <- ets.month$aic
            MSE <- ets.month$mse
            RMSE <- sqrt(MSE)
            fitted <- unclass(ets.month$fitted)
            result <- list(AIC=AIC,RMSE=RMSE,fitted=fitted,forecast=forecast)
            return (result)
        }
       
        """
      //println(R.toPrimitive[Double]("forecast_ets(c(\"1993-02\",\"1993-03\",\"1993-04\"),c(250,260,270))"))

    
       val a = List("1993-02","1993-03","1993-04","1993-05","1993-06",
          "1993-07","1993-08","1993-09","1993-10","1993-11",
          "1993-12","1993-01","1993-02","1993-03","1993-02","1993-03","1993-04","1993-05","1993-06",
          "1993-07","1993-08","1993-09","1993-10","1993-11","1993-10","1993-11",
          "1993-12")
       val b = List(250,260,270,280,290,300,310,320,330,340,350,360,370,380,390,400,410,420,430,440,450,460,470,480,490,500,510)
       
       


        R.eval("result <- forecast_ets(c"+a.toString.substring(4)
        +",c"+ b.toString.substring(4)+")")
        println(R.capture("result"))
        R.quit

     } 
    
}
