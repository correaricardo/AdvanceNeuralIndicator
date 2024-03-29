//+------------------------------------------------------------------+
//|                                              EarlyStochastic.mq5 |
//|                        Copyright 2019, MetaQuotes Software Corp. |
//|                                             https://www.mql5.com |
//+------------------------------------------------------------------+
#property copyright "Copyright 2019, MetaQuotes Software Corp."
#property link      "https://www.mql5.com"
#property version   "1.00"
#property indicator_chart_window

// Define the indicators
#property indicator_buffers 6
#property indicator_plots 6

#property indicator_type1 DRAW_LINE
#property indicator_color1 White

#property indicator_type2 DRAW_LINE
#property indicator_color2 Gold

#property indicator_type3 DRAW_LINE
#property indicator_color3 Magenta

#property indicator_type4 DRAW_ARROW
#property indicator_color4 Red

#property indicator_type5 DRAW_ARROW
#property indicator_color5 Green

// Variables for the negotiation
static double rend, Eqc, Eqv;
string Operacion1, Operacion2;
static int sw1, sw2, sw3, sw4, sw5;
static double vs=100000; // Standard volumen
static double tv=0.01;  // MicroVolumen
static double vc,vv,vvc,vvv;

// Variables for buffers
double Buf_0[],Buf_1[], Buf_2[], Crossup[], Crossdown[],Ind[];
double Sum_L;
int i,j,n,k;

// Variables for the models
#define delays 4
#define hiddenNeurons 10
static double c[hiddenNeurons],w[hiddenNeurons][delays],h[hiddenNeurons],g[delays],alpha;
double er, ers;
double M_0,M_1,S_0,S_1;

// Variables for read the data
static int csv,csv2;
string headers;
int csv_io_hnd;
MqlTick tick_struct;

//+------------------------------------------------------------------+
//| Custom indicator initialization function                         |
//+------------------------------------------------------------------+
int OnInit()
  {
//--- indicator buffers mapping

   // Adding buffers to the arrays
   SetIndexBuffer(0,Buf_0,INDICATOR_DATA);
   SetIndexBuffer(1,Buf_1,INDICATOR_DATA);
   SetIndexBuffer(2,Buf_2,INDICATOR_DATA);
   SetIndexBuffer(3,Crossup,INDICATOR_DATA);
   SetIndexBuffer(4,Crossdown,INDICATOR_DATA);
   SetIndexBuffer(5,Ind,INDICATOR_DATA);  
   
   // Read the data
   csv = FileOpen( "GeneticValues.csv", FILE_CSV|FILE_READ|FILE_WRITE|FILE_REWRITE|FILE_ANSI, ",",CP_ACP);
   csv2 = FileOpen( "ModelValues.csv", FILE_CSV|FILE_READ|FILE_WRITE|FILE_REWRITE|FILE_ANSI, ",",CP_ACP);
   headers=FileReadString(csv);
   // Adding the values for the AR Model (With genetic algorithms)
   for(i=0;i<ArraySize(g);i++){
      g[i]=FileReadString(csv);
   }
   // Ignore the headers
   int x=delays;
   for(i=0;i<x+2;i++){
      headers=FileReadString(csv2);
   }
   // Adding the values for the Madaline Model
   for(i=0;i<hiddenNeurons;i++){
      for(j=0;j<x;j++){
         w[i][j]=FileReadString(csv2);
      }
      c[i]=FileReadString(csv2);
      alpha=FileReadString(csv2);
   }
   
//---
   return(INIT_SUCCEEDED);
  }
//+------------------------------------------------------------------+
//| Custom indicator iteration function                              |
//+------------------------------------------------------------------+
int OnCalculate(const int rates_total,
                const int prev_calculated,
                const datetime &time[],
                const double &open[],
                const double &high[],
                const double &low[],
                const double &close[],
                const long &tick_volume[],
                const long &volume[],
                const int &spread[])
  {
//---
   
   for(i=delays;i<rates_total;i++){ // Calculate the values for the buffers
      Buf_0[i] = 0;
      Sum_L = 0;
      for(j=0; j<ArraySize(g); j++){
         Buf_0[i] = Buf_0[i] + close[i-j-1] * g[j];
         Sum_L = Sum_L + close[i-j-1];
      }
      Buf_1[i] = Sum_L / delays;
      Buf_2[i] = 0;
      // Feedfordward
      for(j=0; j<ArraySize(h); j++){
         h[j] = 0;
         for(k=0;k<delays;k++){
            h[j] = h[j] + w[j][k] * close[i-k-1];
         }
         Buf_2[i] = Buf_2[i] + h[j] * c[j];         
      }  
      // Backpropagation
      er = (open[i] - Buf_2[i]);
      ers = ers + er * er;
      for(j<0; j<ArraySize(c); j++){
         c[i] = c[i] + alpha * er * h[j];
      }
      for(j=0;j<ArraySize(h);j++){
         for(k=0;k<delays;k++){
           w[j][k]=w[j][k]+er*alpha*c[j]*close[i-k-1];
         }       
      }
   }
   // Calculate the last values until it's generate new price
   i = rates_total-1;
   Buf_0[i] = 0;
   Sum_L = 0;
   for(j=0; j<ArraySize(g); j++){
      Buf_0[i] = Buf_0[i] + close[i-j-1] * g[j];
      Sum_L = Sum_L + close[i-j-1];
   }
   Buf_1[i] = Sum_L / delays;
   Buf_2[i] = 0;
   // Feedfordward
   for(j=0; j<ArraySize(h); j++){
      h[j] = 0;
      for(k=0;k<delays;k++){
         h[j] = h[j] + w[j][k] * close[i-k-1];
      }
      Buf_2[i] = Buf_2[i] + h[j] * c[j];         
   }  
   // Backpropagation
   er = (open[i] - Buf_2[i]);
   ers = ers + er * er;
   for(j<0; j<ArraySize(c); j++){
      c[i] = c[i] + alpha * er * h[j];
   }
   for(j=0;j<ArraySize(h);j++){
      for(k=0;k<delays;k++){
        w[j][k]=w[j][k]+er*alpha*c[j]*close[i-k-1];
      }       
   }
   
   // Adding the values for the negotiation
   M_1=Buf_2[i-1];// Short trend before
   M_0=Buf_2[i];// Short trend now
   S_1=Buf_1[i-1];// Long trend before
   S_0=Buf_1[i];// Long trend now
   
   double points = SymbolInfoDouble(_Symbol, SYMBOL_POINT); // Difference between buy and close price
   // Open buy position
   if ((M_1>S_1 && M_0>S_0) && sw1==0){
      sw1 = 1;
      vvc = vvc+1; 
      vc = vc + close[i] * vs * tv; 
      rend = vv-vc; 
      Crossdown[i] = low[i] - 0.0001;
      Operacion1 = "Open BUY position";
      sw3 = 1;
   }
   // Close buy position
   if ((M_1>S_1 && M_0<=S_0) && sw1==1){
      sw1 = 0;
      vvv = vvv+1; 
      vv = vv + vs * tv * (close[i] - (spread[i]*points));
      rend = vv-vc;
      Crossup[i] = high[i] + 0.0001;
      Operacion1 = "Close BUY position";
      sw3 = -1; 
   }
   // Open sell position
   if ((S_1>M_1 && S_0>=M_0) && sw2==0){
      sw2 = 1;
      vvv = vvv+1; 
      vv = vv + vs * tv * (close[i] - (spread[i]*points));
      rend = vv-vc;
      Crossdown[i] = high[i] + 0.0001;
      Operacion1 = "Close SELL position";
      sw3 = 2; 
   }
   // Close sell position
   if ((S_1>M_1 && S_0<=M_0) && sw2==1){
      sw2 = 0;
      vvc = vvc+1; 
      vc = vc + close[i] * vs * tv; 
      rend = vv-vc; 
      Crossup[i] = low[i] - 0.0001;
      Operacion1 = "Close SELL position";
      sw3 = -1; 
   }
   
   if (sw3 == 1){
      Ind[i] = 1; sw3 = 0; // Buy
   }
   if (sw3 == 2){
      Ind[i] = 2; sw3 = 0; // Sell
   }
   if (sw3 == -1){
      Ind[i] = -1; sw3 = 0; // Cierre de posiciones
   }
   Comment("El pronostico del precio de apertura es: ",Buf_0[i], "\n",
            "El pronostico del promedio de apertura es: ",Buf_1[i], "\n",
            "El pronostico del modelo neuronal de apertura es: ",Buf_2[i], "\n",
            "El error del pronostico es: ",er, "\n",
            "La operacion1 es: ",Operacion1, "\n",
            "La operacion2 es: ",Operacion2, "\n",
            "Los velumenes de venta son: ",vvv, "\n",
            "Los velumenes de compra son: ",vvc, "\n",
            "El rendimiento es: ",rend, "\n",
            "El numero de puntos es: ", points);
//--- return value of prev_calculated for next call
   return(rates_total);
  }
//+---------------------x---------------------------------------------+
