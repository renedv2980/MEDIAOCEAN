*          DATA SET NEMED84T   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NEMED84    AT LEVEL 059 AS OF 11/04/86                      
*PHASE T31E84A,+0                                                               
*                                                                               
         TITLE 'T31E84 - AUDIENCE (PRE-BUY SCHEDULE)'                           
*                                                                               
*                                                                               
         MACRO                                                                  
&NAME    CLEAN &I,&L,&C                                                         
         LCLA  &L1                                                              
         LCLC  &L2                                                              
&L1      SETA  &L-1                                                             
&NAME    MVI   &I,&C                                                            
         AIF   (T'&I EQ 'U').SS1                                                
         MVC   &I+1(&L1),&I                                                     
         AGO   .SS                                                              
.SS1     ANOP                                                                   
         AIF   ('&I'(K'&I-2,1) NE 'R').SS                                       
&L2      SETC  '1+'.'&I'(1,K'&I-3).'&L1,'.'&I'(K'&I-2,3)                        
         MVC   &L2,&I                                                           
.SS      MEND                                                                   
*                                                                               
         MACRO                                                                  
&NAME    BCOL  &A,&P,&N,&M,&C=C                                                 
         LCLA  &Z,&Z1                                                           
&Z       SETA  &N                                                               
&Z1      SETA  &M                                                               
         AIF   ('&C' NE 'L' AND '&C' NE 'LR').EQ1                               
         MVI   &A+&P&Z-&P&N,C'L'                                                
         AGO   .EQ2                                                             
.EQ1     MVI   &A+&P&Z-&P&N,C'C'                                                
.EQ2     ANOP                                                                   
&Z       SETA  &Z+1                                                             
         AIF   (&Z EQ &Z1).EQ3                                                  
         AIF   (&Z GT &Z1).EQ4                                                  
         AGO   .EQ1                                                             
.EQ3     AIF   ('&C' NE 'R' AND '&C' NE 'LR').EQ1                               
         MVI   &A+&P&Z-&P&N,C'R'                                                
.EQ4     MEND                                                                   
*                                                                               
         MACRO                                                                  
&NAME    PL    &N,&L                                                            
C&N      DS    CL1                                                              
CL&NAME  DS    CL&L                                                             
         MEND                                                                   
         EJECT                                                                  
         PRINT NOGEN                                                            
***********************************************************************         
*    NETWORK SCHEDULE AUDIENCE PRE BUY ANALYSIS(EDIT,SSPEC,PRINT)     *         
*                                                                     *         
*       INPUTS :  NETBLOCK SETUP BY EDIT                              *         
*                 CLIENT RECORD WILL BE RETURNED IN W/S1              *         
*       GLOBALS:  R5->CURRENT PRINT LINE                              *         
*                 R6->DEMOS                                           *         
*                 R7->WORKING STORAGE                                 *         
*                 R8->DSECT FOR SPOOL PRINTING                        *         
*                 R9->NETWORK SYSTEM DSECT                            *         
*                 RC->GEND                                            *         
*       LOCALS:   R4->BOXD                                            *         
***********************************************************************         
*                                                                               
T31E84   CSECT                                                                  
         NMOD1 0,**SABA**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,DSPECS                                                        
         ST    R2,SPECS                                                         
         USING COLINFO,R5                                                       
         L     R7,ANETWS2                                                       
         USING WORKD,R7                                                         
         L     R6,ANETWS1                                                       
         ST    R6,NBADEM                                                        
         USING NDDEMBLK,R6                                                      
*                                                                               
*   ROUTING BETWEEN EDIT&PRINT PROGRAMS                                         
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PRINT                                                            
         CLI   MODE,VALKEY                                                      
         BNE   EX1                                                              
         BAS   RE,EDITP                                                         
         B     EX                                                               
PRINT    BAS   RE,PRINTP                                                        
         B     EX1                                                              
EX       XIT1  REGS=(R2)                                                        
EX1      XIT1                                                                   
         EJECT                                                                  
EDITP    NTR1                                                                   
***********************************************************************         
*              ASSUMES NETBLOCK IS ALREADY INITIALIZED                          
*              DONE BY CALL TO NVAGY OR NVAGYOUT                                
         L     R4,ATWA                                                          
         USING T31EFFD,R4                                                       
         SPACE 1                                                                
         MVI   NBQINIT,0           DO ALL THE VALIDATIONS EVERY TIME            
*                                  NEEDED IF ALREADY VALIDATED BITS NOT         
*                                  USED                                         
         LA    R2,SPLCLIH          CLIENT                                       
         NETGO NVCLI,DMCB,SPLCLIN  EXPAND                                       
         OI    SPLCLINH+6,X'80'    TRANSMIT IT                                  
         L     R3,NBAIO                                                         
         USING CLTHDR,R3                                                        
         MOVE  (PRSAV,880),CLIST                                                
*                                                                               
         LA    R2,SPLPROH          PRODUCT                                      
         NETGO NVPRD,DMCB,SPLPRON  EXPAND                                       
         OI    SPLPRONH+6,X'80'    TRANSMIT IT                                  
*                                                                               
         LA    R2,SPLESTH          ESTIMATE                                     
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK                                   
         NETGO NVDELHOM,DMCB,NDDEMOS                                            
         OI    SPLESTNH+6,X'80'    TRANSMIT IT                                  
         L     R3,NBAIO                                                         
         USING ESTHDR,R3                                                        
         TRT   SPLEST(8),TAB                                                    
         BZ    *+10                                                             
         MVC   NDDEMOS(60),EDEMLST                                              
         MVC   NDDEMOS+60(3),EDEM21                                             
         DROP  R3                                                               
*                                                                               
         MVI   FTERMFLG,1                                                       
*                                                                               
         LA    R2,SPLNETH          NETWORK                                      
         NETGO NVNETALL,DMCB         EXPAND                                     
*                                                                               
         LA    R2,SPLPAKH          PACKAGE                                      
         NETGO NVPAKLOK,DMCB,SPLPAKN  EXPAND                                    
         OI    SPLPAKNH+6,X'80'    TRANSMIT IT                                  
         L     R2,NBAIO                                                         
         USING NPRECD,R2                                                        
         MVC   PCTFEED,NPAKFEED                                                 
         DROP  R2                                                               
*                                                                               
         LA    R2,SPLFVH                                                        
         CLC   SPLFV(3),=C'W'                                                   
         BE    DP                                                               
         CLC   SPLFV(3),=C'N'                                                   
         BE    DP                                                               
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   SPLFV,C'N'                                                       
         OI    SPLFVH+6,X'80'                                                   
         B     DP                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
DP       LA    R2,SPLDPTH          DAYPART                                      
         NETGO NVDPTALL,DMCB,SPLDPTN EXPAND                                     
         OI    SPLDPTNH+6,X'80'    TRANSMIT IT                                  
*                                                                               
         LA    R2,SPLASGH                                                       
         MVI   ASGCOST,0                                                        
         CLI   8(R2),C'Y'                                                       
         BNE   *+12                                                             
         MVI   ASGCOST,C'Y'                                                     
         B     DEM                                                              
         CLI   8(R2),C'B'                                                       
         BNE   DEM                                                              
         MVC   ASGCOST,SPLASG                                                   
*                                                                               
DEM      LA    R2,SPLDEMOH         DEMOS                                        
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
*                                                                               
         LA    R2,SPLSDTH                                                       
         NETGO NVSTRDAT,DMCB       START DATE(OPTIONAL)                         
*                                                                               
         LA    R2,SPLEDTH          END DATE(OPTIONAL).ALSO INSURES THAT         
         NETGO NVENDDAT,DMCB       THE END DATE IS NOT BEFORE THE START         
*                                  ONE.                                         
         LA    R2,SPLCLIH          NORMAL END OF EDIT                           
         DROP  R4                                                               
*                                                                               
***********************************************************************         
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
PRINTP   NTR1                                                                   
         MVI   NBDATA,C'U'                                                      
         MVI   NBESTOPT,C'Y'                                                    
         OI    NBSPLOPT,X'80'                                                   
         MVI   NBHUNOPT,C'Y'                                                    
         LA    RE,NETN                                                          
         LA    RF,704                                                           
         XCEF                                                                   
         ABIN  A,ANETN,%NETN                                                    
         CLEAN PERLIST,220,X'00'                                                
         LA    R2,UNIVH                                                         
         LA    R3,4                                                             
UJN      XC    0(4,R2),0(R2)                                                    
         CLEAN 4(R2),12,C' '                                                    
         LA    R2,16(R2)                                                        
         BCT   R3,UJN                                                           
         MVI   BT,0     DEMOS&UNIVERSES                                         
         LA    R2,4                                                             
         CLI   ASGCOST,C'B'        IF ASGND AND ACTUAL COST                     
         BNE   *+8                                                              
         LA    R2,3                THEN ONLY 3 DEMOS                            
         LA    R3,UNIVH                                                         
         LA    R4,NDDEMOS                                                       
         SR    R5,R5                                                            
NED      CLC   0(3,R4),=F'0'                                                    
         BE    FYP                                                              
         CLI   0(R4),255                                                        
         BE    FYP                                                              
         ABIN  A,BT,1                                                           
         NETGO NVUNIV,DMCB,((R5),NDDEMBLK),(C'E',DBLOCK),0(R3)                  
         NETGO NVDEMCON,DMCB,((R5),NDDEMBLK),DBLOCK,(7,4(R3))                   
         LA    R3,16(R3)                                                        
         LA    R4,3(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R2,NED                                                           
*                                                                               
FYP      L     R4,ATWA                                                          
         USING T31EFFD,R4                                                       
         LA    R5,P1                                                            
FFD      NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   FFX                                                              
         CLI   NBMODE,NBVALDAT                                                  
         BE    FFD1                                                             
         B     FFD                                                              
FFD1     XC    PERTYPE,PERTYPE                                                  
         MVI   PERTYPE,C'W'                                                     
         MVI   PERTYPE+1,1                                                      
         LA    R1,MAXNPER                                                       
         ST    R1,NUMPER                                                        
         NETGO NVWKLST,DMCB,NUMPER,PERLIST,PERTYPE                              
         LA    R1,PERLIST                                                       
         ST    R1,NBADTL                                                        
         GOTO1 CALLOV,DMCB,0,X'D9000A11'                                        
         MVC   UNTIME,DMCB                                                      
         CLI   SPLFV,C'N'                                                       
         BE    FFD2                                                             
         MVI   NBSEQ,C'X'                                                       
         B     *+8                                                              
FFD2     MVI   NBSEQ,C'N'                                                       
         MVI   FIRST,0                                                          
         DROP  R4                                                               
         LA    R4,PERLIST                                                       
         MVI   NBSELUOP,C'A'                                                    
         NETGO NVDEMOPT,DMCB                                                    
FFIB     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBPROCPK     PXZ                                          
         BNE   FFCNT               PXZ                                          
         L     R1,NBAIO            PXZ                                          
         USING NPRECD,R1                                                        
         MVC   PCTFEED,NPAKFEED    PXZ                                          
         DROP  R1                                                               
FFCNT    CLI   NBMODE,NBPROCUN                                                  
         BE    FFIB1                                                            
         CLI   NBMODE,NBREQLST     LAST RECORD?                                 
         BE    GRAND                                                            
         B     FFIB                                                             
FFIB1    BAS   RE,PRSRCH                                                        
         CLI   FIRST,0                                                          
         BNE   FFIB2                                                            
         MVI   FIRST,1                                                          
         B     FFIB9                                                            
FFIB2    TM    NBSUBMSK,NBSBMNET                                                
         BZ    FFIB4                                                            
         CLI   NBSEQ,C'X'                                                       
         BE    FFIB5                                                            
         MVI   ET,4                                                             
         BAS   RE,TOTAL                                                         
         CLEAN NTNO,160,X'00'                                                   
         LA    R4,PERLIST                                                       
         B     FFIB9                                                            
FFIB5    CLC   NBACTDAT,2(R4)                                                   
         BH    FFIB11                                                           
         MVI   ET,2                                                             
         BAS   RE,TOTAL                                                         
         CLEAN NTNO,44,X'00'                                                    
         CLEAN DTNO,80,X'00'                                                    
         B     FFIB9                                                            
FFIB7    LA    R4,4(R4)                                                         
FFIB9    CLC   NBACTDAT,2(R4)                                                   
         BNH   *+8                                                              
         B     *-14                                                             
FFIB10   CLEAN DTNO,80,X'00'                                                    
         MVC   NETSAV,NBACTNET                                                  
         MVI   CSW,0                                                            
         BAS   RE,CUMULUS                                                       
         B     FFIB                                                             
FFIB4    CLC   NBACTDAT,2(R4)                                                   
         BNH   FFIB10                                                           
         CLI   NBSEQ,C'X'                                                       
         BNE   *+8                                                              
         B     FFIB11                                                           
         MVI   ET,1                                                             
         BAS   RE,TOTAL                                                         
         CLEAN WTNO,120,X'00'                                                   
         B     FFIB7                                                            
FFIB11   MVI   ET,3                                                             
         BAS   RE,TOTAL                                                         
         CLEAN NTNO,84,X'00'                                                    
         B     FFIB7                                                            
GRAND    MVI   ET,5                                                             
         BAS   RE,TOTAL                                                         
FFX      XIT1                                                                   
         EJECT                                                                  
FW       NTR1                                                                   
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(4,CLDATE)                              
         MVC   CLDAY,NBDAYNAM                                                   
         GOTO1 UNTIME,DMCB,NBTIME,CLTIME                                        
         MVC   CLPRNM,NBPROGNM                                                  
         MVC   CLNET,NETSAV                                                     
         EDIT  (B1,NBLEN),(3,CLSPTLEN)                                          
         MVC   CLBRAND+2(3),CURPR                                               
         LA    R3,DTCOST                                                        
         CLI   ASGCOST,C'B'                                                     
         BNE   FW5                                                              
         BAS   RE,TEDIT2                                                        
         B     *+8                                                              
FW5      BAS   RE,TEDIT                                                         
         BAS   RE,NEXTLIN                                                       
         XIT1  REGS=(R5)                                                        
*                                                                               
TEDIT    NTR1                                                                   
         EDIT  (4,0(R3)),(8,CLCOST),ZERO=BLANK                                  
         LA    R3,8(R3)            SKIP COST2 FIELD                             
         BAS   RE,MEDIT                                                         
         XIT1  REGS=(R5)                                                        
*                                                                               
TEDIT2   NTR1                       ROUTINE FOR ACT AND ASSGND COST             
         EDIT  (4,0(R3)),(8,CLCOST),ZERO=BLANK                                  
         LA    R3,4(R3)                                                         
         LA    R4,CLCOST+8                                                      
         EDIT  (4,0(R3)),(8,0(R4)),ZERO=BLANK                                   
         LA    R3,4(R3)                                                         
         BAS   RE,MEDIT                                                         
         XIT1  REGS=(R5)                                                        
*                                                                               
MEDIT    NTR1                                                                   
         ZIC   R2,BT                                                            
         LA    R4,CLRTGH                                                        
         CLI   ASGCOST,C'B'                                                     
         BNE   *+8                                                              
         LA    R4,8(R4)                                                         
MEDIT1   EDIT  (4,0(R3)),(7,0(R4)),1,ZERO=BLANK          GRPS                   
         CLI   CSW,1                                                            
         BNE   MEDIT2                                                           
         EDIT  (4,4(R3)),(7,8(R4)),2,ZERO=BLANK          CPP/CPM                
         B     MEDIT3                                                           
MEDIT2   NETGO NVPRDEM,DMCB,(C'I',0),4(R3),8(R4)         IMPS                   
MEDIT3   LA    R3,8(R3)                                                         
         LA    R4,16(R4)                                                        
         BCT   R2,MEDIT1                                                        
         XIT1 REGS=(R5)                                                         
         EJECT                                                                  
**********************************************************************          
*      ET,AT = 1  N   WEEK                                           *          
*            = 2  W   WEEK                                           *          
*            = 3  W   WEEK SUMMARY                                   *          
*            = 4  N   NETWORK SUMMARY                                *          
*            = 5  GRAND TOTAL                                        *          
**********************************************************************          
TOTAL    NTR1                                                                   
         MVI   CSW,0                                                            
         CLI   NBSEQ,C'X'                                                       
         BNE   TOTN                                                             
         MVI   AT,2                                                             
         BAS   RE,ALPHAS                                                        
         LA    R2,NTNO                                                          
         BAS   RE,TSUM                                                          
*                                                                               
         LA    R2,NETN                                                          
         LA    R3,10                                                            
TOT1     CLI   0(R2),0                                                          
         BE    TOT2                                                             
         CLC   0(2,R2),=C'CE'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(4,R2),NETSAV                                                   
         BE    TOT3                                                             
         LA    R2,48(R2)                                                        
         BCT   R3,TOT1                                                          
TOT2     MVC   0(4,R2),NETSAV                                                   
TOT3     LA    R3,4(R2)                                                         
         LR    R2,R4                                                            
         LA    R4,NTNO                                                          
         BAS   RE,CUMULUS1                                                      
         LR    R4,R2                                                            
*                                                                               
         CLC   ET,AT                                                            
         BNH   TOTX                                                             
         OI    CSW,1                                                            
         MVI   AT,3                                                             
         BAS   RE,ALPHAS                                                        
         LA    R2,WTNO                                                          
         BAS   RE,TSUM                                                          
         CLC   ET,AT                                                            
         BNH   TOTX                                                             
         B     TOTG                                                             
TOTN     MVI   AT,1                                                             
         BAS   RE,ALPHAS                                                        
         LA    R2,WTNO                                                          
         BAS   RE,TSUM                                                          
         CLC   ET,AT                                                            
         BNH   TOTX                                                             
         OI    CSW,1                                                            
         MVI   AT,4                                                             
         BAS   RE,ALPHAS                                                        
         LA    R2,NTNO                                                          
         BAS   RE,TSUM                                                          
         CLC   ET,AT                                                            
         BNH   TOTX                                                             
TOTG     MVI   AT,5                                                             
         LA    R3,NETN                                                          
TOTG1    CLI   0(R3),0                                                          
         BE    TOTG2                                                            
         BAS   RE,ALPHAS                                                        
         LR    R2,R3                                                            
         LA    R2,4(R2)                                                         
         BAS   RE,TSUM                                                          
         BAS   RE,NEXTLIN                                                       
         LA    R3,48(R3)                                                        
         B     TOTG1                                                            
TOTG2    BAS   RE,ALPHAS                                                        
         LA    R2,GTNO                                                          
         BAS   RE,TSUM                                                          
TOTX     XIT1  REGS=(R5)                                                        
         EJECT                                                                  
TSUM     NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P2                                                            
         BAS   RE,MIDDLE                                                        
         MVC   C3+1(16),ALPHA                                                   
         BAS   RE,NEXTLIN                                                       
         OI    CSW,2                                                            
         MVC   C3+2(14),=C'TOTALS  UNITS:'                                      
         LA    R3,4(R2)                                                         
         CLI   ASGCOST,C'B'                                                     
         BNE   *+12                                                             
         BAS   RE,TEDIT2                                                        
         B     *+8                                                              
         BAS   RE,TEDIT                                                         
         EDIT  (4,0(R2)),(4,C4),ZERO=BLANK                                      
         BAS   RE,NEXTLIN                                                       
         MVC   C3+2(8),=C'AVERAGES'                                             
         LR    R3,R2                                                            
         BAS   RE,AVERG                                                         
         LA    R3,ACOST                                                         
         CLI   ASGCOST,C'B'                                                     
         BNE   *+12                                                             
         BAS   RE,TEDIT2                                                        
         B     *+8                                                              
         BAS   RE,TEDIT                                                         
         BAS   RE,NEXTLIN                                                       
         XI    CSW,2                                                            
         TM    CSW,1                                                            
         BZ    TEX                                                              
         MVC   C3+4(7),=C'CPP/CPM'                                              
         LA    R3,4(R2)                                                         
         BAS   RE,ACP                                                           
         LA    R3,ARTGH                                                         
         BAS   RE,MEDIT                                                         
         BAS   RE,NEXTLIN                                                       
TEX      GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P2                                                            
         BAS   RE,MIDDLE                                                        
         XIT1  REGS=(R5)                                                        
         EJECT                                                                  
CUMULUSG NTR1                                                                   
         LA    R3,WTNO                                                          
         LA    R4,DTNO                                                          
         BAS   RE,CUMULUS1                                                      
         LA    R3,NTNO                                                          
         LA    R4,DTNO                                                          
         BAS   RE,CUMULUS1                                                      
         LA    R3,GTNO                                                          
         LA    R4,DTNO                                                          
         BAS   RE,CUMULUS1                                                      
         XIT1  REGS=(R5)                                                        
         SPACE 2                                                                
CUMULUS1 NTR1                                                                   
         ABIN  A,0(R3),0(R4)                                                    
         ABIN  A,4(R3),4(R4)                                                    
         ABIN  A,8(R3),8(R4)       COST2                                        
         ZIC   R2,BT                                                            
CUMUL1   ABIN  A,12(R3),12(R4)                                                  
         ABIN  A,16(R3),16(R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,CUMUL1                                                        
XX1      XIT1  REGS=(R5)                                                        
         SPACE 2                                                                
CUMULUS  NTR1                                                                   
         ABIN  A,DTNO,1                                                         
         ABIN  D,NBACTUAL,100,OT=DTCOST                                         
         ABIN  D,NBASSIGN,100,OT=DTCOST2                                        
         CLI   ASGCOST,C'Y'                                                     
         BNE   CUMUL5                                                           
         ABIN  D,NBASSIGN,100,OT=DTCOST                                         
CUMUL5   ZIC   R2,BT                                                            
         LA    R3,NDESTDEM                                                      
         LA    R4,DTRTGH                                                        
CUMUL    ABIN  A,0(R4),(2,2(R3))                                                
         ABIN  A,4(R4),(4,4(R3))                                                
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,CUMUL                                                         
XX       BAS   RE,CUMULUSG                                                      
         BAS   RE,FW                                                            
         XIT1  REGS=(R5)                                                        
         EJECT                                                                  
AVERG    NTR1                                                                   
         ABIN  D,4(R3),0(R3),OT=ACOST                                           
         ABIN  D,8(R3),0(R3),OT=ACOST2                                          
         ZIC   R2,BT                                                            
         LR    RF,R3                                                            
         LA    R4,ARTGH                                                         
AVER     ABIN  D,12(R3),0(RF),OT=0(R4)                                          
         ABIN  D,16(R3),0(RF),OT=4(R4)                                          
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,AVER                                                          
AVEX     XIT1                                                                   
         SPACE 2                                                                
ACP      NTR1                                                                   
         ZIC   R2,BT                                                            
         LA    R4,ARTGH                                                         
         LR    R5,R3                                                            
         CLC   ASGCOST,=C'B2'      B2=AVERAGE ON ASSIGNED COST                  
         BNE   ACP1                                                             
         LA    R5,4(R5)            SO BUMP R5 TO ASSIGNED COST                  
ACP1     ABIN  D,0(R5),8(R3),OT=0(R4),MF=100                                    
         ABIN  D,0(R5),12(R3),OT=4(R4),MF=1000                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,ACP1                                                          
ACPX     XIT1                                                                   
         EJECT                                                                  
ALPHAS   NTR1                                                                   
         CLEAN ALPHA,16,C' '                                                    
         CLI   AT,3                                                             
         BNH   ALP0                                                             
         CLI   AT,4                                                             
         BE    ALP4                                                             
         B     ALP5                                                             
ALP0     GOTO1 DATCON,DMCB,(2,0(R4)),(4,ALPHA)                                  
         GOTO1 DATCON,DMCB,(2,2(R4)),(4,ALPHA+6)                                
         MVI   ALPHA+5,C'-'                                                     
         CLI   AT,1                                                             
         BE    ALP1                                                             
         CLI   AT,2                                                             
         BE    ALP2                                                             
         B     ALP1                                                             
ALP1     MVC   ALPHA+12(4),=C'SUMM'                                             
         B     ALPHAX                                                           
ALP2     MVC   ALPHA+12(4),NETSAV                                               
         B     ALPHAX                                                           
ALP4     MVC   ALPHA(4),NETSAV                                                  
         MVC   ALPHA+5(7),=C'SUMMARY'                                           
         B     ALPHAX                                                           
ALP5     CLI   NETN,0                                                           
         BE    ALP6                                                             
         CLI   0(R3),0                                                          
         BE    ALP6                                                             
         MVC   ALPHA(4),0(R3)                                                   
         MVC   ALPHA+6(7),=C'SUMMARY'                                           
         B     ALPHAX                                                           
ALP6     MVC   ALPHA(13),=C'GRAND SUMMARY'                                      
ALPHAX   XIT1                                                                   
*                                                                               
         EJECT                                                                  
MIDDLE   NTR1                                                                   
         CLI   OFFLINE,C'Y'                                                     
         BNE   MIDOUT                                                           
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         MVI   BOXOFF,0                                                         
         ABIN  A,%BOXROWS,LINE,KEEP=Y                                           
         BCTR  R1,0                                                             
         MVI   0(R1),C'M'                                                       
         DROP  R4                                                               
MIDOUT   XIT1                                                                   
*                                                                               
NEXTLIN  NTR1                                                                   
         LA    R1,P4                                                            
         CR    R5,R1                                                            
         BL    NXT                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P1                                                            
         B     *+8                                                              
NXT      LA    R5,132(R5)                                                       
         XIT1  REGS=(R5)                                                        
*                                                                               
PRSRCH   NTR1                                                                   
         LA    R3,PRSAV                                                         
         LA    R4,220                                                           
         MVC   BYTE,NBPRD                                                       
         CLI   NBSPLPRN,0                                                       
         BE    PR                                                               
         CLI   NBSPLPRN,X'FF'                                                   
         BE    PR                                                               
         MVC   BYTE,NBSPLPRN                                                    
         SPACE 1                                                                
PR       CLI   0(R3),0                                                          
         BE    PRNO                                                             
         CLC   3(1,R3),BYTE                                                     
         BNE   *+14                                                             
         MVC   CURPR(3),0(R3)                                                   
         B     PRXT                                                             
         LA    R3,4(R3)                                                         
         BCT   R4,PR                                                            
PRNO     XC    CURPR,CURPR         NO MATCH CLEAR CURRENT PRODUCT               
PRXT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*HOOK - BUILDS THE HEADER.SUPPLEMENTS THE SSPECS IN DSPECS            *         
***********************************************************************         
HOOK     NTR1                                                                   
*                                                                               
* MAIN HEADER INFO                                                              
         LA    R5,H1               BASE ADDRESS FOR HEADER OFFSETS              
         USING PHEAD,R5                                                         
         MVC   PHCLI,NBSELCLI                                                   
         L     R4,ATWA                                                          
         USING T31EFFD,R4                                                       
         MVC   PHCLNM,SPLCLIN                                                   
         MVC   PHPRD,SPLPRO                                                     
         MVC   PHPRNM,SPLPRON                                                   
         MVC   PHEST(5),SPLEST                                                  
         MVC   PHESNM,SPLESTN                                                   
         MVC   H7+95(21),=C'CPM/P BASED ON ACTUAL'                              
         CLC   ASGCOST,=C'B2'                                                   
         BNE   *+10                                                             
         MVC   H7+95(23),=C'CPM/P BASED ON ASSIGNED'                            
         CLI   NBSELNET,0          NETWORK=ALL?                                 
         BNE   *+14                                                             
         MVC   PHNETNM(3),=C'ALL'                                               
         B     *+10                                                             
         MVC   PHNETNM(4),NBSELNET                                              
         MVC   PHFVNM(3),SPLFV                                                  
         CLI   NBSELDP,0           DAYPART=ALL                                  
         BNE   *+14                                                             
         MVC   PHDPRT(3),=C'ALL'                                                
         B     *+10                                                             
         MVC   PHDPRT(8),SPLDPTN                                                
         DROP  R4                                                               
         CLC   PCTFEED,=2X'00'                                                  
         BE    HOOK1                                                            
         MVC   H7+53(9),=C'FEED PCT='                                           
         LA    R3,H7+62                                                         
         EDIT  (B2,PCTFEED),(5,0(R3)),2                                         
*                                                                               
*COLUMN HEADERS                                                                 
HOOK1    CLI   OFFLINE,C'Y'                                                     
         BNE   SL5                                                              
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVC   BOXWT(2),=X'0100'                                                
         MVI   BOXOFF,0                                                         
         CLEAN BOXCOLS,132,C' '                                                 
         CLEAN BOXROWS,100,C' '                                                 
         MVI   BOXROWS+8,C'T'                                                   
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+12,C'M'                                                  
         CLC   NBSELAGY,=C'DF'                                                  
         BNE   HOOK1D                                                           
         MVI   BOXROWS+10,0                                                     
         MVI   BOXROWS+12,0                                                     
         MVI   BOXROWS+11,C'M'                                                  
         MVI   BOXROWS+13,C'M'                                                  
HOOK1D   MVI   BOXROWS+57,C'B'                                                  
         BCOL  BOXCOLS,C,0,7,C=L                                                
*                                                                               
         LA    R3,BOXCOLS+C8-C0                                                 
         CLI   ASGCOST,C'B'                                                     
         BNE   *+8                                                              
         LA    R3,8(R3)                                                         
         ZIC   R2,BT                                                            
SL0      LTR   R2,R2                                                            
         BZ    SL1                                                              
         MVI   0(R3),C'C'                                                       
         LA    R3,16(R3)                                                        
         BCT   R2,SL0                                                           
SL1      MVI   0(R3),C'R'                                                       
         DROP  R4                                                               
*                                                                               
SL5      LA    R5,H10                                                           
         USING COLINFO,R5                                                       
O2       EQU   C10-C8                                                           
O1       EQU   (O2-7)/2                                                         
         B     JUMP                                                             
*                                                                               
UC       DC    CL11'UNIV=      '                                                
*                                                                               
JUMP     ZIC   R2,BT                                                            
         LA    R3,CLRTGH                                                        
         CLI   ASGCOST,C'B'                                                     
         BNE   *+8                                                              
         LA    R3,8(R3)                                                         
         LA    R1,UNIVH                                                         
SL51     MVC   0(O1,R3),=4C'*'                                                  
         MVC   O1(7,R3),4(R1)                                                   
         MVC   O1+7(O1,R3),=4C'*'                                               
         CLC   NBSELAGY,=C'DF'           UNIV= RESTRICTED TO DANCER             
         BNE   BCTSL                                                            
         LR    R4,R3                                                            
         LA    R4,134(R4)                                                       
         MVC   0(11,R4),UC                                                      
         EDIT  (4,0(R1)),(6,5(R4)),ALIGN=LEFT                                   
         CLC   PCTFEED,=2X'00'                                                  
         BE    BCTSL                                                            
         BAS   RE,FEEDCLC                                                       
BCTSL    LA    R3,O2(R3)                                                        
         LA    R1,16(R1)                                                        
         BCT   R2,SL51                                                          
         B     SL6                                                              
*                                                                               
FEEDCLC  NTR1                                                                   
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),PCTFEED                                                
         L     R3,0(R1)                                                         
         M     R2,FULL                                                          
         D     R2,=F'10000'                                                     
         EDIT  (R3),(6,5(R4))                                                   
         XIT1                                                                   
*                                                                               
*                                                                               
SL6      LA    R5,H12                                                           
         CLC   NBSELAGY,=C'DF'                                                  
         BNE   *+8                                                              
         LA    R5,H13                                                           
         MVC   CLDATE(4),=C'DATE'                                               
         MVC   CLDAY(3),=C'DAY'                                                 
         MVC   CLTIME(4),=C'TIME'                                               
         MVC   CLPRNM(12),=C'PROGRAM NAME'                                      
         MVC   CLNET(3),=C'NET'                                                 
         MVC   CLCOST+4(4),=C'COST'                                             
         MVC   CLSPTLEN(3),=C'LEN'                                              
         CLI   ASGCOST,C'B'                                                     
         BNE   SL6A                                                             
         LA    R4,CLCOST+6                                                      
         SH    R4,=H'263'                                                       
         MVC   0(4,R4),=C'COST'                                                 
         MVC   CLCOST+2(6),=C'ACTUAL'                                           
         MVC   CLCOST+11(5),=C'ASGND'                                           
SL6A     MVC   CLBRAND(5),=C'BRAND'                                             
         LA    R4,CON                                                           
         BAS   RE,SL20                                                          
*                                                                               
SL7      CLI   OFFLINE,C'Y'                                                     
         BE    SL8                                                              
         LA    R5,H13                                                           
         CLC   NBSELAGY,=C'DF'                                                  
         BNE   *+8                                                              
         LA    R5,H14                                                           
         MVC   CLDATE(4),=12C'-'                                                
         MVC   CLDAY(3),=12C'-'                                                 
         MVC   CLTIME(4),=12C'-'                                                
         MVC   CLPRNM(12),=12C'-'                                               
         MVC   CLNET(3),=12C'-'                                                 
         MVC   CLSPTLEN(3),=12C'-'                                              
         MVC   CLCOST+2(6),=12C'-'                                              
         MVC   CLCOST+11(5),=12C'-'                                             
         MVC   CLBRAND(5),=12C'-'                                               
         LA    R4,CON1                                                          
         BAS   RE,SL20                                                          
*                                                                               
SL8      XIT1                                                                   
*                                                                               
SL20     NTR1                                                                   
         ZIC   R2,BT                                                            
         LA    R3,CLRTGH                                                        
         CLI   ASGCOST,C'B'                                                     
         BNE   *+8                                                              
         LA    R3,8(R3)                                                         
SL21     MVC   4(3,R3),0(R4)                                                    
         MVC   12(3,R3),3(R4)                                                   
         LA    R3,O2(R3)                                                        
         BCT   R2,SL21                                                          
         XIT1                                                                   
*                                                                               
**********************************************************************          
         EJECT                                                                  
*               HEADLINE  SPECS                                                 
         SPACE 3                                                                
         PRINT NOGEN                                                            
DSPECS   SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,54,C'PRE BUY SCHEDULE'                                        
         SSPEC H1,96,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,54,16C'-'                                                     
         SSPEC H2,96,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,54,PERIOD                                                     
         SSPEC H4,96,NETREP                                                     
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H5,54,C'NETWORK='                                                
         SSPEC H5,96,PAGE                                                       
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H6,54,C'SORT = '                                                 
         SSPEC H6,96,C'DAYPART='                                                
         DC    X'00'                                                            
         EJECT                                                                  
         DS    0F                                                               
TAB      DC    107X'00'                                                         
         DC    X'01'                                                            
         DC    85X'00'                                                          
         DC    9X'01'                                                           
         DC    7X'00'                                                           
         DC    9X'01'                                                           
         DC    8X'00'                                                           
         DC    8X'01'                                                           
         DC    22X'00'                                                          
CON      DC    C'RTGIMP'                                                        
CON1     DC    12C'-'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*OFFSETS FOR PAGE HEADER INFORMATION                                            
         DSECT                                                                  
PHEAD    EQU   *                                                                
PHLENGTH EQU   132                                                              
*                                                                               
         ORG   PHEAD+3*PHLENGTH+11                                              
PHCLI    DS    CL3                 CLIENT ABBR                                  
         DS    CL2                                                              
PHCLNM   DS    CL20               CLIENT NAME                                   
         ORG   PHEAD+4*PHLENGTH+11                                              
PHPRD    DS    CL3                 PRODUCT ABBR                                 
         DS    CL2                                                              
PHPRNM   DS    CL20                PRODUCT NAME                                 
         ORG   PHEAD+4*PHLENGTH+62                                              
PHNETNM  DS    CL4                 NETWORK NAME                                 
         ORG   PHEAD+5*PHLENGTH+11                                              
PHEST    DS    CL2                 ESTIMATE ABBR                                
         DS    CL3                                                              
PHESNM   DS    CL24                ESTIMATE NAME                                
         ORG   PHEAD+5*PHLENGTH+61                                              
PHFVNM   DS    CL3                                                              
         ORG   PHEAD+5*PHLENGTH+104                                             
PHDPRT   DS    CL10                                                             
         EJECT                                                                  
*DSECT FOR PRINT LINES                                                          
         DSECT                                                                  
COLINFO  EQU   *                                                                
*                                                                               
DATE     PL    0,5                                                              
DAY      PL    1,3                                                              
TIME     PL    2,10                                                             
PRNM     PL    3,16                                                             
NET      PL    4,4                                                              
BRAND    PL    5,5                                                              
SPTLEN   PL    6,3                                                              
COST     PL    7,8                                                              
RTGH     PL    8,7                                                              
IMPH     PL    9,7                                                              
RTG1     PL    10,7                                                             
IMP1     PL    11,7                                                             
RTG2     PL    12,7                                                             
IMP2     PL    13,7                                                             
RTG3     PL    14,7                                                             
IMP3     PL    15,7                                                             
C16      DS    CL1                                                              
         EJECT                                                                  
********************WORKING STORAGE************************************         
WORKD    DSECT                                                                  
         DS    0D                                                               
*                                                                               
NETN     DS    10CL48                                                           
ANETN    DS    A                                                                
NN       DS    CL1                                                              
*                                                                               
GTNO     DS    F                   GRAND TOTAL NO                               
GTCOST   DS    F                   GRAND TOTAL COST                             
GTCOST2  DS    F                   GRAND TOTAL COST                             
GTRTGH   DS    F                   GRAND TOTAL RTGH                             
GTIMPH   DS    F                   GRAND TOTAL IMPH                             
GTRTG1   DS    F                   GRAND TOTAL RTG1                             
GTIMP1   DS    F                   GRAND TOTAL IMP1                             
GTRTG2   DS    F                   GRAND TOTAL RTG2                             
GTIMP2   DS    F                   GRAND TOTAL IMP2                             
GTRTG3   DS    F                   GRAND TOTAL RTG3                             
GTIMP3   DS    F                   GRAND TOTAL IMP3                             
         SPACE 3                                                                
NTNO     DS    F                   NETWORK TOTAL NO                             
NTCOST   DS    F                   NETWORK TOTAL COST                           
NTCOST2  DS    F                   NETWORK TOTAL COST                           
NTRTGH   DS    F                   NETWORK TOTAL RTGH                           
NTIMPH   DS    F                   NETWORK TOTAL IMPH                           
NTRTG1   DS    F                   NETWORK TOTAL RTG1                           
NTIMP1   DS    F                   NETWORK TOTAL IMP1                           
NTRTG2   DS    F                   NETWORK TOTAL RTG2                           
NTIMP2   DS    F                   NETWORK TOTAL IMP2                           
NTRTG3   DS    F                   NETWORK TOTAL RTG3                           
NTIMP3   DS    F                   NETWORK TOTAL IMP3                           
         SPACE 3                                                                
WTNO     DS    F                   WEEKLY TOTAL NO                              
WTCOST   DS    F                   WEEKLY TOTAL COST                            
WTCOST2  DS    F                   WEEKLY TOTAL COST                            
WTRTGH   DS    F                   WEEKLY TOTAL RTGH                            
WTIMPH   DS    F                   WEEKLY TOTAL IMPH                            
WTRTG1   DS    F                   WEEKLY TOTAL RTG1                            
WTIMP1   DS    F                   WEEKLY TOTAL IMP1                            
WTRTG2   DS    F                   WEEKLY TOTAL RTG2                            
WTIMP2   DS    F                   WEEKLY TOTAL IMP2                            
WTRTG3   DS    F                   WEEKLY TOTAL RTG3                            
WTIMP3   DS    F                   WEEKLY TOTAL IMP3                            
         SPACE 3                                                                
DTNO     DS    F                   DAILY TOTAL NO                               
DTCOST   DS    F                   DAILY TOTAL COST                             
DTCOST2  DS    F                   DAILY TOTAL COST2 ASSGND-IF 2 COSTS          
DTRTGH   DS    F                   DAILY TOTAL RTGH                             
DTIMPH   DS    F                   DAILY TOTAL IMPH                             
DTRTG1   DS    F                   DAILY TOTAL RTG1                             
DTIMP1   DS    F                   DAILY TOTAL IMP1                             
DTRTG2   DS    F                   DAILY TOTAL RTG2                             
DTIMP2   DS    F                   DAILY TOTAL IMP2                             
DTRTG3   DS    F                   DAILY TOTAL RTG3                             
DTIMP3   DS    F                   DAILY TOTAL IMP3                             
         SPACE 3                                                                
ACOST   DS    F                    AVERAGE COST                                 
ACOST2  DS    F                    AVERAGE COST                                 
ARTGH   DS    F                    AVERAGE RTGH                                 
AIMPH   DS    F                    AVERAGE IMPH                                 
ARTG1   DS    F                    AVERAGE RTG1                                 
AIMP1   DS    F                    AVERAGE IMP1                                 
ARTG2   DS    F                    AVERAGE RTG2                                 
AIMP2   DS    F                    AVERAGE IMP2                                 
ARTG3   DS    F                    AVERAGE RTG3                                 
AIMP3   DS    F                    AVERAGE IMP3                                 
         SPACE 3                                                                
*                                                                               
UNIVH    DS    F                                                                
DEMH     DS    3F                                                               
UNIV1    DS    F                                                                
DEM1     DS    3F                                                               
UNIV2    DS    F                                                                
DEM2     DS    3F                                                               
UNIV3    DS    F                                                                
DEM3     DS    3F                                                               
*                                                                               
ALPHA    DS    4F                                                               
PERTYPE  DS    F                                                                
NUMPER   DS    F                                                                
MAXNPER  EQU   54                                                               
PERLIST  DS    CL(4*(MAXNPER+1))                                                
NETSAV   DS    5F                                                               
NETSAV1  DS    108D                                                             
MONTAB   DS    CL12                                                             
FIRST    DS    CL1                                                              
CSW      DS    CL1                                                              
BT       DS    CL1                                                              
AT       DS    CL1                                                              
ET       DS    CL1                                                              
ASGCOST  DS    CL2                                                              
PXZ      DS    CL1                                                              
PCTFEED  DS    CL2                                                              
*                                                                               
PRSAV    DS    0F                                                               
         DS    CL880                                                            
CURPR    DS    CL3                                                              
         EJECT                                                                  
         PRINT   GEN                                                            
DEMOS    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NEMEDFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDD4D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED84T  05/01/02'                                      
         END                                                                    
