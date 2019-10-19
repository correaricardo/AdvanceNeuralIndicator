//+------------------------------------------------------------------+
//|                                                      GetData.mq5 |
//|                        Copyright 2019, MetaQuotes Software Corp. |
//|                                             https://www.mql5.com |
//+------------------------------------------------------------------+
#property copyright "Copyright 2019, MetaQuotes Software Corp."
#property link      "https://www.mql5.com"
#property version   "1.00"

// CHANGE THE TIME BETWEEN UPDATES
int minutes = 1;
int hours = 1;
//+------------------------------------------------------------------+
//| Get data function                                                |
//+------------------------------------------------------------------+
void getData()
{  
   MqlRates rates[];
   int copied = CopyRates(NULL, PERIOD_M1, 0, 1100, rates);
   if(copied > 0)
   {  
      int handleFile = FileOpen("Dataset.csv", FILE_WRITE|FILE_CSV);
      // Labels
      string buffer = "Close" + ";";
      buffer += "Open" + ";";
      buffer += "\n";
      FileWriteString(handleFile, buffer);
      // Data
      for(int i = 0; i < ArraySize(rates); i++)
      {
         string buffer;
         buffer += rates[i].close + ";";
         buffer += rates[i].open;
         buffer += "\n";
         FileWriteString(handleFile, buffer);
      }
      FileClose(handleFile);
   }
   else
   {
      Print("Error: ", GetLastError());
   }
}
//+------------------------------------------------------------------+
//| Expert initialization function                                   |
//+------------------------------------------------------------------+
int OnInit()
{  
   getData();
   EventSetTimer(60*minutes*hours) ;
   return(INIT_SUCCEEDED);
}
//+------------------------------------------------------------------+
//| Timer function                                                   |
//+------------------------------------------------------------------+
void OnTimer()
{
   getData();
}
//+------------------------------------------------------------------+
