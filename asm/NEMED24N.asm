*          DATA SET NEMED24N   AT LEVEL 011 AS OF 06/23/06                      
*PHASE T31E24A,+0                                                               
         TITLE 'T31E24 -EFFICIENCY REPORT'                                      
         EJECT                                                                  
*                                                                               
*                                                                               
         MACRO                                                                  
&NAME    CLEAN &I,&L,&C                                                         
         LCLA  &L1                                                              
         LCLC  &L2                                                              
&L1      SETA  &L-1                                                             
         MVI   &I,&C                                                            
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
*    NETWORK SCHEDULE EFFICIENCY ANALYSIS PRINT PROGRAM               *         
*                                                                     *         
* ** OCT12/92 - IN $ OPTION FUDGE TO INSERT ROUNDED DOLLARS           *         
*               INTO ACCUM TABLE SO AVERAGES ARE NOW CALCULATED       *         
*               ON PRINTED FIGURES.                                   *         
*                                                                     *         
*                                                                     *         
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
T31E24   CSECT                                                                  
         NMOD1 0,**SEAL**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         MVC   SPOOLKEY+12(3),=C'NEF'                                           
         MVC   SPOOLKEY+1(11),=C'EFFICIENCY '                                   
         USING COLINFO,R5                                                       
         L     R7,ANETWS2                                                       
         USING WORKD,R7                                                         
         L     R6,NBADEM                                                        
         USING NDDEMBLK,R6                                                      
         LA    R1,STALIST                                                       
         ST    R1,NBCNVNTI                                                      
*                                                                               
         MVI   FIRST,C'Y'                                                       
         MVI   NBDATA,C'B'                                                      
         OI    NBVARIND,X'40'      USE NDDEMOS FOR NAD                          
         OI    NBSPLOPT,X'80'      HANDLE PRODUCT SPLITS                        
         MVI   NBHUNOPT,C'Y'       PROGRAM HANDLES HUNDREDS                     
         MVI   NBRESUME,NBPROCPK                                                
         MVI   NBSEQ,C'Q'                                                       
         XC    PNET(100),PNET                                                   
         LA    R4,PNET                                                          
         ST    R4,PNETA                                                         
         CLEAN GTNO,256,X'00'                                                   
         ZAP   TDOLPEN,=P'0'                                                    
         ZAP   NDOLPEN,=P'0'                                                    
         MVC   BT(2),=F'0'                                                      
         CLEAN RES,19,C' '         PXZ CHANGE                                   
*                                                                               
         LA    R2,UNIV1                                                         
         LA    R3,3                                                             
         MVI   DT,0                                                             
UJN      XC    0(4,R2),0(R2)                                                    
         CLEAN 4(R2),12,C' '                                                    
         LA    R2,16(R2)                                                        
         BCT   R3,UJN                                                           
*                                                                               
         LA    R2,3                                                             
         LA    R3,UNIV1                                                         
         LA    R4,NDDEMOS                                                       
         SR    R5,R5                                                            
FFD      CLC   0(3,R4),=F'0'                                                    
         BE    FYP                                                              
         CLI   0(R4),255                                                        
         BE    FYP                                                              
         PRINT GEN                                                              
***      NETGO NVUNIV,DMCB,((R5),NDDEMBLK),(C'E',DBLOCK),0(R3)                  
         PRINT NOGEN                                                            
         DS    0H                                                               
***      NETGO NVDEMCON,DMCB,((R5),NDDEMBLK),DBLOCK,(7,4(R3))                   
         LA    R3,16(R3)                                                        
         LA    R4,3(R4)                                                         
         LA    R5,1(R5)                                                         
         ZIC   R1,DT                                                            
         LA    R1,1(R1)                                                         
         STC   R1,DT                                                            
         BCT   R2,FFD                                                           
*                                                                               
FYP      L     R4,ATWA                                                          
         USING T31EFFD,R4                                                       
         CLI   SPLFV,C'V'                                                       
         BNE   FYP2                                                             
         MVI   NBESTOPT,X'04'      RETURN EST DEMOS IN MOST CASES               
         MVI   NBESTFCT,6          FUNCT=EQ,REJECT IF EQUAL                     
         DROP  R4                                                               
*                                                                               
         B     FYP4                                                             
FYP2     MVI   NBACTOPT,C'Y'                                                    
         NI    NBINDS,X'FF'-X'40'  TURN OFF PRIMP                               
FYP4     MVI   NBSELUOP,C'A'                                                    
         LA    R5,P1                                                            
FFIB     GOTO1 PNETC,DMCB,PNET                                                  
         CLI   NBMODE,NBPROCUN                                                  
         BE    FFIB8                                                            
         CLI   NBMODE,NBREQLST     LAST RECORD?                                 
         BE    FFIB1                                                            
         B     FFIB                                                             
         EJECT                                                                  
*                                                                               
FFIB8    DS    0H                                                               
         TM    NBINDS,X'40'        ARE WE USING INCREASED PRECISION             
         BZ    *+8                                                              
         BAS   RE,IMPDIV10                                                      
         CLI   BT,1                                                             
         BE    FFIB9                                                            
         MVI   BT,1                                                             
         B     FFIB2                                                            
FFIB9    CLC   NETSAV,NBACTNET                                                  
         BNE   FFIBB                                                            
         TM    NBSUBMSK,NBSBMNET                                                
         BZ    FFIB3                                                            
FFIBB    BAS   RE,FW                                                            
         BAS   RE,NETSUM                                                        
         CLEAN WTNO,192,X'00'                                                   
FFIB2    MVC   CLPRNM(4),NBACTNET                                               
         MVC   NETSAV,NBACTNET                                                  
         MVI   ALLOWLIN,2                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     FFIB4                                                            
*                                                                               
IMPDIV10 NTR1                           GREATER PRECISION IS 7 DIGITS           
*                                       NORMAL COMES IN AS 5 DIGITS             
*                                       DROP LAST 2 DIGS HERE                   
*                                       DO FINAL ROUND AT EDIT                  
*                                                                               
         CLI   NBPOSTYP,C'C'       IF CABLE                                     
         BE    XIT                 LEAVE AS IS                                  
         LA    R2,NBESTHOM+4       DO HOMES FIRST                               
         BAS   RE,DIV100                                                        
         LA    R2,NDESTDEM        ESTIMATE IMPRESSIONS                          
         LA    R2,4(R2)           POINT TO IMPS                                 
         LA    R3,20                                                            
IMPD10   BAS   RE,DIV100                                                        
         LA    R2,8(R2)                                                         
         BCT   R3,IMPD10                                                        
         B     XIT                                                              
*                                                                               
DIV100   DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R1,15,0(R2)                                                      
         LTR   R1,R1                                                            
         BZ    DIV10X                                                           
         D     R0,=F'100'                                                       
         ST    R1,0(R2)                                                         
DIV10X   BR    RE                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
FFIB3    CLC   CLPRNM,NBPROGNM                                                  
         BE    FFIB7                                                            
         CLC   PROGNMSV,NBPROGNM                                                
         BE    FFIB7                                                            
         MVC   PROGNMSV,NBPROGNM                                                
***      TM    NBSUBMSK,NBSBMPRG                                                
***      BZ    FFIB7                                                            
         BAS   RE,FW                                                            
         BAS   RF,CUMULUS2                                                      
         CLEAN WSNO,124,X'00'                                                   
FFIB4    MVC   CLPRNM,NBPROGNM                                                  
         MVC   PROGNMSV,NBPROGNM                                                
         MVC   RES1,CLPRNM                                                      
         B     FFIB6                                                            
FFIB7    BAS   RE,FW                                                            
         BAS   RF,CUMULUS2                                                      
         CLEAN WSNO,124,X'00'                                                   
FFIB6    GOTO1 DATCON,DMCB,(2,NBACTDAT),(4,CLADATE)                             
         BAS   RE,GETPRD                                                        
         MVC   CLNET(3),PRDCD                                                   
         CLI   PRDCD+3,0                                                        
         BE    NOMOPRDS                                                         
         MVC   CLNET+132(3),PRDCD+3                                             
*                                                                               
         LA    RF,CLNET+136        BUMP TO NEXT OUT AREA                        
         LA    RE,PRDCD+6          ARE THERE MORE THAN 2 PRODS                  
FFNXTPRD CLI   0(RE),0                                                          
         BE    NOMOPRDS            NO                                           
         MVC   0(3,RF),0(RE)       YES/MOVE OUT NEXT PROD                       
         LA    RF,4(RF)            BUMP OUT AREA                                
         LA    RE,3(RE)            BUMP TOP NEXT PROD                           
         B     FFNXTPRD                                                         
NOMOPRDS BAS   RE,CUMULUS                                                       
         B     FFIB                                                             
FFIB1    BAS   RE,FW                                                            
         BAS   RE,NETSUM                                                        
         LA    R2,GCOST                                                         
         LA    R3,GTNO                                                          
**                                                                              
         L     R4,ATWA                                                          
         USING T31EFFD,R4                                                       
         TM    NBINDS,X'10'        ..IF $ DOLLAR OPTION                         
         BNO   FFIB12                                                           
         LA    R1,TDOLPEN          ..SET DOLLARS/PENNIES                        
         BAS   RE,CVBIT            ..ROUNDED                                    
         ST    R1,GTCOST           ..TO TOTAL COST AREA                         
**                                                                              
FFIB12   BAS   RE,AVERG                                                         
GRAND    LA    R5,P2                                                            
         MVI   ALLOWLIN,3                                                       
         XC    RES,RES                                                          
         MVC   C0+1(12),=C'GRAND TOTALS'                                        
         BAS   RE,NEXTLIN                                                       
         BAS   RE,AVEDIT                                                        
         MVC   C0+1(15),=C'TOTALS   UNITS='                                     
         LA    R3,GTNO                                                          
         BAS   RE,TEDIT                                                         
*                                                                               
******   L     R1,ATWA                                                          
**********SING T31EFFD,R4                                                       
         CLI   SPLOPT,C'$'         EDIT OPTION                                  
         BNE   GRAND00                                                          
         DP    TDOLPEN,=P'100'                                                  
         CP    TDOLPEN+6(2),=P'50'                                              
         BNH   GRAND0                                                           
         AP    TDOLPEN(6),=P'1'                                                 
GRAND0   EDIT  (P6,TDOLPEN),(8,CLCOST)                                          
*                                                                               
GRAND00  BAS   RE,DISP                                                          
         CLI   SPLCT,C'P'                                                       
         BNE   GRAND1                                                           
         EDIT  GCOST,(8,CLCOST)                                                 
GRAND1   BAS   RE,NEXTLIN                                                       
         XC    GTNO(256),GTNO                                                   
         XC    WSAVDAT(256),WSAVDAT                                             
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
**************************************                                          
* GET PRODUCT CODE                                                              
*                                                                               
GETPRD   NTR1                                                                   
         XC    PRDCD(18),PRDCD                                                  
         CLC   NBSELPRD,=C'POL'                                                 
         BNE   GPSPLIT                                                          
*                                                                               
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'19'                                                     
         BAS   RE,GETEL                                                         
         BNE   GETPRD00                                                         
         USING NUPDED,R2                                                        
         ZIC   R1,NUPDELEN                                                      
         S     R1,=F'3'                                                         
         SR    R0,R0                                                            
         D     R0,=F'7'           R1 = NUMBER OF PRODS                          
         LA    R2,NUPDEPR          POINT TO FIRST PROD                          
         LA    RE,PRDCD                                                         
GETPRDLP MVC   0(3,RE),0(R2)                                                    
         LA    RE,3(RE)            BUMP NXT PRD SLOT                            
         LA    R2,7(R2)            BUMP TO NXT PROD                             
         BCT   R1,GETPRDLP                                                      
         B     GPX                                                              
         DROP  R2                                                               
*                                                                               
GETPRD00 LA    R1,2               ** PROD = POL(SPLIT NOT ACTIVE)               
         LA    R3,NBPRD                                                         
         LA    R4,PRDCD                                                         
         CLI   NBPRDNO,0           ARE WE DEALING WITH MULTI PRODS              
         BE    GPOL                NO                                           
         ZIC   R1,NBPRDNO          YES                                          
         LA    R3,NBPRDLST                                                      
GPOL     CLI   0(R3),0                                                          
         BE    GP1                                                              
         L     R2,ANETWS4                                                       
         LA    R2,2000(R2)         PXZ 7/9/99                                   
GPLOOP1  CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GP1A                SET TO UNDEFINED                             
GP1      MVC   0(3,R4),=C'UNA'                                                  
         B     GPNXT                                                            
GP1A     CLC   3(1,R2),0(R3)                                                    
         BE    GP4                                                              
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GPLOOP1             RETURN TO LOOP                               
GP4      MVC   0(3,R4),0(R2)                                                    
*                                                                               
GPNXT    CLI   NBPRDNO,0           ,,IF MULTI PRODS                             
         BE    GPNXT20                                                          
         LA    R3,1(R3)            ,,BUMP PROD LIST                             
         LA    R4,3(R4)            ,,BUMP PROD OUT AREA                         
         B     GPNXT22                                                          
*                                                                               
GPNXT20  CLI   NBPRD2,0            NO MULTI PROD/ CHK 2ND PROD                  
         BE    GPX                                                              
         LA    R3,NBPRD2                                                        
         LA    R4,PRDCD+3                                                       
GPNXT22  BCT   R1,GPOL                                                          
         B     GPX                                                              
*                                                                               
GPSPLIT  CLI   NBSPLPR3,0          ** 3 CHAR PROD                               
         BE    GPSPLT0                                                          
         MVC   PRDCD,NBSPLPR3                                                   
         B     GPX                                                              
*                                                                               
GPSPLT0  LA    R3,NBSPLPRN         ** SPLIT PRODUCTS                            
         CLI   0(R3),0                                                          
         BE    GP10                                                             
         L     R2,ANETWS4                                                       
         LA    R2,2000(R2)         PXZ 7/9/99                                   
*                                  BUMPED IN T31E04 ALSO                        
GPLOOP   CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GP12                SET TO UNDEFINED                             
GP10     MVC   PRDCD,=C'UNA'                                                    
         B     GPX                                                              
GP12     CLC   3(1,R2),0(R3)                                                    
         BE    GP14                                                             
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GPLOOP              RETURN TO LOOP                               
*                                                                               
GP14     MVC   PRDCD,0(R2)                                                      
*                                                                               
GPX      XIT1                                                                   
         EJECT                                                                  
PNETC    NTR1                                                                   
***      LA    R2,20                                                            
         LA    R2,PNETA                                                         
         LA    R4,PNETB            PXZ                                          
         L     R3,0(R1)                                                         
         LA    R5,1                                                             
PNETC1   NETGO NSNETIO,DMCB,NETBLOCK                                            
                                                                                
         CLI   OFFLINE,C'Y'                                                     
         BE    *+8                                                              
         BAS   RE,CHKMAXIO                                                      
                                                                                
         CLI   NBMODE,NBPROCPK                                                  
         BE    PNETC2                                                           
         CLI   NBMODE,NBPROCUN                                                  
         BE    PNETC10                                                          
         CLI   NBMODE,NBREQLST                                                  
         BE    PNETX                                                            
         B     PNETC1                                                           
*                                                                               
PNETC2   DS    0H                                                               
         TM    NBSUBMSK,NBSBMNET                                                
         BZ    PNETC5                                                           
         CH    R5,=H'1'                                                         
         BE    PNETC5                                                           
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)            PXZ                                          
PNETC5   ABIN  A,(4,0(R3)),NBPAKCST                                             
         L     R1,NBAIO                                                         
         USING NPRECD,R1                                                        
         MVC   0(4,R4),NPKNET      PXZ                                          
         L     RE,ATWA                                                          
         USING T31EFFD,RE                                                       
         CLC   SPLPAK(3),=C'ALL'   IF ALL PKGS/ SKIP FEED                       
         BE    PNETC7                                                           
         CLC   SPLPAK(3),=3X'40'                                                
         BNH   PNETC7                                                           
         MVC   PCTFEED,NPAKFEED                                                 
         DROP  R1,RE                                                            
PNETC7   SR    R5,R5                                                            
***      BCT   R2,PNETC1                                                        
         CR    R2,R3                                                            
         BH    PNETC1                                                           
         DC    H'0'                                                             
*                                                                               
PNETC10  CLI   FIRST,C'N'                                                       
         BE    PNETX                                                            
         MVI   FIRST,C'N'                                                       
         BAS   RE,DOUNIV                                                        
         B     PNETX                                                            
*                                                                               
DOUNIV   NTR1                    *GET UNIV FROM 1ST UNITS PKG REC               
***      MVC   AIOSV,NBAIO                                                      
***      LA    R1,IOSPACE                                                       
***      ST    R1,NBAIO                                                         
         LA    R2,KEY                                                           
         USING NPRECD,R2                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   NPKAM,NBACTAM                                                    
         MVC   NPKCLT,NBACTCLI                                                  
         MVC   NPKNET,NBACTNET                                                  
         MVC   NPKEST,NBACTEST                                                  
         MVC   NPKPACK,NBPACK                                                   
         MVC   KEYSAVE,KEY                                                      
         NETGO NVSETUNT,DMCB                                                    
         MVC   FILENAME,=C'UNTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BNE   DOUA                                                             
**       LA    R2,IOSPACE                                                       
         L     R2,NBAIO                                                         
         LA    R3,KEY+21                                                        
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'UNTFILE ',(R3),(R2),DMWORK            
         CLI   8(R1),0                                                          
         BNE   DOUA                                                             
         MVC   NBUNCODE,NPAKUNCD   GET UNIV CODE                                
         DROP  R2                                                               
DOUA     LA    R2,UNIV1                                                         
         LA    R3,3                                                             
         MVI   DT,0                                                             
PNET101  XC    0(4,R2),0(R2)                                                    
         CLEAN 4(R2),12,C' '                                                    
         LA    R2,16(R2)                                                        
         BCT   R3,PNET101                                                       
*                                                                               
         LA    R2,3                                                             
         LA    R3,UNIV1                                                         
         LA    R4,NDDEMOS                                                       
         SR    R5,R5                                                            
PNET102  CLC   0(3,R4),=F'0'                                                    
         BE    PNET105                                                          
         CLI   0(R4),255                                                        
         BE    PNET105                                                          
         NETGO NVUNIV,DMCB,((R5),NDDEMBLK),(C'E',DBLOCK),0(R3)                  
         L     R1,ATWA                                                          
         USING T31EFFD,R1                                                       
         CLI   SPLFV,C'P'                                                       
         BNE   PNET103                                                          
         NETGO NVUNIV,DMCB,((R5),NDDEMBLK),(0,DBLOCK),0(R3)                     
PNET103  DS    0H                                                               
         NETGO NVDEMCON,DMCB,((R5),NDDEMBLK),DBLOCK,(7,4(R3))                   
         CLI   0(R4),0             IF NAD DEMO                                  
         BE    NONAD                                                            
         EDIT  (B1,0(R4)),(3,WORK)                                              
         MVI   WORK+3,C'.'                                                      
         MVC   WORK+4(7),4(R3)                                                  
         MVC   4(11,R3),WORK                                                    
NONAD    LA    R3,16(R3)                                                        
         LA    R4,3(R4)                                                         
         LA    R5,1(R5)                                                         
         ZIC   R1,DT                                                            
         LA    R1,1(R1)                                                         
         STC   R1,DT                                                            
         BCT   R2,PNET102                                                       
PNET105  DS    0H                                                               
**       MVI   NBFUNCT,NBFRDHI     SET NETIO TO READ HI                         
**       MVC   NBAIO,AIOSV                                                      
         MVC   KEY(20),NBKEY        RESTORE UNIT KEY/RECORD                     
         MVC   KEYSAVE(20),KEY                                                  
         MVC   FILENAME,=C'UNTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,KEY+21                                                        
         L     R2,NBAIO                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'UNTFILE ',(R3),(R2),DMWORK            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PNETX                                                            
*                                                                               
*                                                                               
PNETX    XIT1                                                                   
*                                                                               
*                                                                               
FW       NTR1                                                                   
         ABIN  D,WSHHSH,10                                                      
         EDIT  WSHHSH,(3,CLHHSH),ZERO=BLANK                                     
         EDIT  WSHUTVH,(4,CLHUTVH),1,ZERO=BLANK                                 
         ZIC   R2,DT                                                            
         LA    R3,WSVPHH                                                        
         LA    R4,TCPMH                                                         
         LA    RF,CLVPHH                                                        
FW1      CLC   4(4,R3),=F'0'                                                    
         BE    FW2                                                              
         MVC   FULL,4(R3)                                                       
         TM    NBINDS,X'40'        INCREASED PRECISION                          
         BZ    FW1B                                                             
         BAS   RE,IMPDIV           RETURNS IN FULL                              
*        MVC   4(4,R3),FULL                                                     
FW1B     ABIN  D,WSCOST,FULL,OT=0(R4),MF=1000                                   
*FW1B     ABIN  D,WSCOST,4(R3),OT=0(R4),MF=1000                                 
         EDIT  (4,0(R3)),(4,0(RF)),ZERO=BLANK                                   
FW2      LA    R3,12(R3)                                                        
         LA    R4,20(R4)                                                        
         LA    RF,27(RF)                                                        
         BCT   R2,FW1                                                           
FWEX     LA    R3,WSNO                                                          
         BAS   RE,TEDIT                                                         
         LA    R2,P1                                                            
         CR    R2,R5                                                            
         BNE   FWEX1                                                            
***      MVC   RES(L'CLNET),NBACTNET                                            
***      MVC   RES+L'CLNET+1(L'CLPRNM),RES1                                     
         MVC   RES(L'CLPRNM),RES1                                               
FWEX1    BAS   RE,NEXTLIN                                                       
         XIT1  REGS=(R5)                                                        
         EJECT                                                                  
NETSUM   NTR1                                                                   
         DROP  R4                                                               
         BAS   RF,CUMULUS2                                                      
         LA    R2,GTNO                                                          
         LA    R3,WTNO                                                          
         BAS   RE,CUMULUS1                                                      
         LA    R3,WTNO                                                          
**                                                                              
* - STICK DOLLARS/PENNIES INTO NET AND TOT DOLLAR FIELDS                        
         L     R4,ATWA                                                          
         USING T31EFFD,R4                                                       
         CLI   SPLOPT,C'$'                                                      
         BNE   PKGOPT                                                           
         LA    R1,NDOLPEN     THIS COULD BLOW IF DOLS GET TOO BIG               
         BAS   RE,CVBIT                                                         
         ST    R1,WTCOST                                                        
         B     PKGOPT                                                           
CVBIT    MVC   WORK(8),0(R1)       ROUND TO NEAREST DOLLAR                      
         DP    WORK(8),=P'100'                                                  
         CP    WORK+6(2),=P'50'                                                 
         BNH   *+10                                                             
         AP    WORK(6),=P'1'                                                    
         ZAP   DUB,WORK(6)                                                      
         CVB   R1,DUB                                                           
         BR    RE                                                               
**                                                                              
PKGOPT   L     R2,PNETA                                                         
         CLI   SPLCT,C'P'                                                       
         BNE   NETS5                                                            
*                                                                               
         LA    R4,PNETB            PXZ                                          
         LA    R2,PNET             PXZ                                          
         LA    R1,150              INCREASED 7/9/99                             
PKGLOOP  CLC   NETSAV,0(R4)        PXZ IS IT SAME NET                           
         BE    GOTIT               PXZ                                          
         LA    R4,4(R4)            PXZ                                          
         LA    R2,4(R2)            PXZ                                          
         BCT   R1,PKGLOOP          PXZ                                          
         DC    H'0'                                                             
GOTIT    ST    R2,PNETA            PXZ                                          
         DROP  R4                                                               
         L     R4,ATWA                                                          
         USING T31EFFD,R4                                                       
*                                                                               
NETS5    BAS   RE,AVERG                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   RE,MIDDLE                                                        
         LA    R5,P2                                                            
         MVC   C0+1(4),NETSAV                                                   
         MVC   C0+6(7),=C'SUMMARY'                                              
         MVI   ALLOWLIN,3                                                       
         BAS   RE,NEXTLIN                                                       
         BAS   RE,AVEDIT                                                        
         MVC   C0+1(15),=C'TOTALS   UNITS='                                     
         LA    R3,WTNO                                                          
         CLI   SPLOPT,C'$'         ...IS IT DOLLAR OPTION                       
         BNE   NETSUM0                                                          
         AP    TDOLPEN,NDOLPEN     ...YES/ROLLOVER NETWORK TOTALS               
         DP    NDOLPEN,=P'100'     ...AND ROUND TO PENNY                        
         CP    NDOLPEN+6(2),=P'50'                                              
         BNH   *+10                                                             
         AP    NDOLPEN(6),=P'1'                                                 
         ZAP   DUB,NDOLPEN(6)                                                   
         CVB   R1,DUB                                                           
         ST    R1,8(R3)            ...SET TO PRINT                              
         ZAP   NDOLPEN,=P'0'       ...CLEAR NETWORK TOTAL AREA                  
NETSUM0  BAS   RE,TEDIT                                                         
         BAS   RE,DISP                                                          
         CLI   SPLCT,C'P'                                                       
         BNE   NETSUM1                                                          
         EDIT  (4,0(R2)),(8,CLCOST)                                             
         ABIN  A,GCOST,0(R2)                                                    
         ABIN  A,PNETA,4                                                        
NETSUM1  BAS   RE,NEXTLIN                                                       
         BAS   RE,MIDDLE                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1  REGS=(R5)                                                        
*                                                                               
*                                                                               
DISP     NTR1                                                                   
         EDIT  (4,0(R3)),(4,CLDUR-7)                                            
         MVC   CLDUR-3,=2C' '                                                   
         EDIT  (4,4(R3)),(5,CLDUR-1),ZERO=BLANK                                 
         NETGO NVPRDEM,DMCB,(C'I',0),20(R3),CLAUDH                              
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
AVERG    NTR1                                                                   
         ZIC   RF,DT                                                            
         LA    R6,20(R3)                                                        
         TM    NBINDS,X'40'        GREATER PREC                                 
         BZ    AV00                                                             
         MVC   FULL,0(R6)                                                       
         BAS   RE,IMPDIV                                                        
         MVC   0(4,R6),FULL                                                     
AV00     LA    R5,TCPMH                                                         
AV0      CLI   SPLCT,C'P'                                                       
         BNE   AV1                                                              
         ABIN  D,0(R2),0(R3),OT=ACOST                                           
         ABIN  D,0(R2),0(R6),OT=0(R5),MF=1000                                   
         B     AV3                                                              
AV1      ABIN  D,8(R3),0(R3),OT=ACOST                                           
         ABIN  D,8(R3),0(R6),OT=0(R5),MF=1000                                   
AV3      LA    R6,12(R6)                                                        
         LA    R5,20(R5)                                                        
         BCT   RF,AV0                                                           
AV2      ABIN  D,12(R3),0(R3),OT=AHUTVH                                         
         ZIC   RF,DT                                                            
         LA    R5,16(R3)                                                        
         LA    R6,AVPHH                                                         
AV4      ABIN  D,0(R5),0(R3),OT=0(R6)                                           
         ABIN  D,4(R5),0(R3),OT=4(R6)                                           
         ABIN  D,8(R5),0(R3),OT=8(R6)                                           
         ABIN  D,ACOST,4(R6),OT=12(R6),MF=1000                                  
         LA    R5,12(R5)                                                        
         LA    R6,20(R6)                                                        
         BCT   RF,AV4                                                           
AX       XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
AVEDIT   NTR1                                                                   
         MVC   C0+1(9),=C' AVERAGES'                                            
         EDIT  ACOST,(8,CLCOST)                                                 
         EDIT  AHUTVH,(4,CLHUTVH),1,ZERO=BLANK                                  
         ZIC   R2,DT                                                            
         LA    R3,AVPHH                                                         
         LA    R4,CLVPHH                                                        
AVE1     EDIT  (4,0(R3)),(4,0(R4)),ZERO=BLANK        VPH                        
         MVC   FULL,4(R3)                                                       
         TM    NBINDS,X'40'        INCREASED PRECISION                          
         BZ    AVE3                                                             
         CLI   NBPOSTYP,C'C'       IF CABLE                                     
         BE    *+8                 LET IT BE HERE                               
         BAS   RE,IMPDIV                                                        
         MVC   4(4,R3),FULL                                                     
AVE3     NETGO NVPRDEM,DMCB,(C'I',0),FULL,5(R4)     IMPS                        
         EDIT  (4,8(R3)),(7,13(R4)),1,ZERO=BLANK     GRPS                       
         TM    NBINDS3,NBI3A2DC    2 DEC AGENCY?                                
         BO    AVE4                                                             
         CLI   NBPREOPT,C'Y'                                                    
         BNE   SKIPIT              IF CABLE PRECISION/2 DEC                     
         CLI   NBPOSTYP,C'N'       FOR NET AND SYND                             
         BE    *+12                                                             
         CLI   NBPOSTYP,C'S'                                                    
         BNE   AVE4                                                             
         ICM   R1,15,8(R3)                                                      
         MH    R1,=H'10'           ADD A ZERO IN 2 DEC                          
         STCM  R1,15,8(R3)                                                      
AVE4     EDIT  (4,8(R3)),(7,13(R4)),2,ZERO=BLANK     GRPS                       
*                                                                               
SKIPIT   MVC   FULL,12(R3)         CPM                                          
         L     R1,FULL                                                          
         C     R1,=F'9999'                                                      
         BNH   AVEOK                                                            
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         EDIT  (R1),(5,21(R4)),ZERO=BLANK                                       
         B     JUMPAVE                                                          
AVEOK    EDIT  (4,12(R3)),(5,21(R4)),2,ZERO=BLANK    CPM                        
JUMPAVE  LA    R3,20(R3)                                                        
         LA    R4,27(R4)                                                        
         BCT   R2,AVE1                                                          
AVEX     BAS   RE,NEXTLIN                                                       
         XIT1  REGS=(R5)                                                        
*                                                                               
TEDIT    NTR1                                                                   
         EDIT  (4,4(R3)),(4,CLDUR),ZERO=BLANK                                   
         EDIT  (4,8(R3)),(8,CLCOST)                                             
         OC    8(4,R3),8(R3)                                                    
         BNZ   *+8                                                              
         MVI   CLCOST+7,C'0'                                                    
         LA    R2,TCPMH                                                         
         LA    R4,CLAUDH                                                        
         ZIC   R5,DT                                                            
TED      DS    0H                                                               
         MVC   FULL,20(R3)                                                      
         TM    NBINDS,X'40'        INCREASED PRECISION                          
         BZ    TED3                                                             
         BAS   RE,IMPDIV           RETURNS IN FULL                              
*        MVC   20(4,R3),FULL                                                    
*TED3     NETGO NVPRDEM,DMCB,(C'I',0),20(R3),0(R4)    IMPS                      
TED3     NETGO NVPRDEM,DMCB,(C'I',0),FULL,0(R4)    IMPS                         
         EDIT  (4,24(R3)),(7,8(R4)),1,ZERO=BLANK     GRPS                       
         TM    NBINDS3,NBI3A2DC    2 DEC AGY?                                   
         BO    TED4                                                             
         CLI   NBPREOPT,C'Y'       IF CABLE PREC/2 DEC                          
         BNE   SKIPIT2                                                          
         CLI   NBPOSTYP,C'N'                                                    
         BE    *+12                                                             
         CLI   NBPOSTYP,C'S'                                                    
         BNE   TED4                                                             
         ICM   R1,15,24(R3)                                                     
         MH    R1,=H'10'                                                        
         STCM  R1,15,24(R3)                                                     
TED4     EDIT  (4,24(R3)),(7,8(R4)),2,ZERO=BLANK     GRPS                       
SKIPIT2  MVC   FULL,0(R2)          CPM                                          
         L     R1,FULL                                                          
         C     R1,=F'9999'                                                      
         BNH   TEDOK                                                            
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         EDIT  (R1),(5,16(R4)),ZERO=BLANK                                       
         B     JUMPIT                                                           
TEDOK    EDIT  (4,0(R2)),(5,16(R4)),2,ZERO=BLANK     CPM                        
JUMPIT   LA    R4,27(R4)                                                        
         LA    R2,20(R2)                                                        
         LA    R3,12(R3)           **PZ                                         
         BCT   R5,TED                                                           
         XIT1                                                                   
*                                                                               
IMPDIV   NTR1                  EXPECTS IMPS IN FULL                             
         L     R1,FULL                                                          
         LTR   R1,R1                                                            
         BZ    IMP10X                                                           
         SR    R0,R0                                                            
         A     R1,=F'50'          ..ROUND TO NEAREST TENTH                      
         D     R0,=F'100'         ..SO NET IMPS END IN ZERO                     
         SR    R0,R0                                                            
         ST    R1,FULL                                                          
         CLI   NBPOSTYP,C'C'       ..BUT IF NOT NETWORK                         
         BE    *+8                 ..LEAVE AS IS                                
         M     R0,=F'100'          ..ELSE PUT BACK OUT TO 000                   
         ST    R1,FULL                                                          
IMP10X   B     XIT                                                              
IMPDIVA  NTR1                  EXPECTS IMPS IN FULL                             
         L     R1,FULL                                                          
         LTR   R1,R1                                                            
         BZ    IMPAX                                                            
         SR    R0,R0                                                            
         A     R1,=F'500'         ..ROUND TO NEAREST TENTH                      
         D     R0,=F'1000'        ..SO NET IMPS END IN ZERO                     
         SR    R0,R0                                                            
         ST    R1,FULL                                                          
         CLI   NBPOSTYP,C'C'       ..BUT IF NOT NETWORK                         
         BE    *+8                 ..LEAVE AS IS                                
         M     R0,=F'1000'         ..ELSE PUT BACK OUT TO 000                   
         ST    R1,FULL                                                          
IMPAX    B     XIT                                                              
         EJECT                                                                  
CUMULUS  NTR1                                                                   
         ABIN  A,WSNO,1                                                         
         ABIN  A,WSDUR,NBLEN                                                    
         CLI   SPLGD,C'Y'                                                       
         BNE   CUMCOST                                                          
         ABIN  D,NBASSIGN,100,OT=WSCO                                           
         MVC   FULL,NBASSIGN                                                    
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         AP    NDOLPEN,DUB                                                      
         B     CUMCOST1                                                         
CUMCOST  ABIN  D,NBACTUAL,100,OT=WSCO                                           
         MVC   FULL,NBACTUAL                                                    
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         AP    NDOLPEN,DUB                                                      
CUMCOST1 ABIN  A,WSCOST,WSCO                                                    
         CLI   NBESTOPT,0                                                       
         BE    CUMACT                                                           
         LA    R2,NBESTSHR                                                      
         LA    R3,NDESTDEM                                                      
         BAS   RF,CUM                                                           
         B     CUMX                                                             
CUMACT   LA    R2,NBACTSHR                                                      
         LA    R3,NDACTDEM                                                      
         BAS   RF,CUM                                                           
         B     CUMX                                                             
CUM      ABIN  A,WSHHSH,(2,0(R2))                                               
         ABIN  A,WSHUTVH,(2,2(R2))                                              
         CLI   DT,0                                                             
         BNH   CUMX1                                                            
         ABIN  A,WSVPHH,(2,0(R3))                                               
         ABIN  A,WSAUDH,(4,4(R3))                                               
         ABIN  A,WSRATH,(2,2(R3))                                               
         CLI   DT,1                                                             
         BNH   CUMX1                                                            
         ABIN  A,WSVPHW,(2,8(R3))                                               
         ABIN  A,WSAUDW,(4,12(R3))                                              
         ABIN  A,WSRATW,(2,10(R3))                                              
         CLI   DT,2                                                             
         BNH   CUMX1                                                            
         ABIN  A,WSVPHO,(2,16(R3))                                              
         ABIN  A,WSAUDO,(4,20(R3))                                              
         ABIN  A,WSRATO,(2,18(R3))                                              
CUMX1    BR    RF                                                               
CUMX     XIT1                                                                   
CUMULUS2 LA    R2,WTNO                                                          
         LA    R3,WSNO                                                          
         BAS   RE,CUMULUS1                                                      
         BR    RF                                                               
CUMULUS1 NTR1                                                                   
         ABIN  A,0(R2),0(R3)                                                    
         ABIN  A,4(R2),4(R3)                                                    
         ABIN  A,8(R2),8(R3)                                                    
         ABIN  A,12(R2),12(R3)                                                  
         ZIC   RF,DT                                                            
CUMUL1   ABIN  A,16(R2),16(R3)                                                  
         ABIN  A,20(R2),20(R3)                                                  
         ABIN  A,24(R2),24(R3)                                                  
         LA    R2,12(R2)                                                        
         LA    R3,12(R3)                                                        
         BCT   RF,CUMUL1                                                        
CX1      XIT1                                                                   
         DROP  R4                                                               
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
         B     NSP                 PXZ CHHANGE                                  
         LA    R1,P4                                                            
         CR    R5,R1                                                            
         BL    NXT                                                              
NSP      GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P1                                                            
         B     *+8                                                              
NXT      LA    R5,132(R5)                                                       
         XIT1  REGS=(R5)                                                        
         EJECT                                                                  
*          DATA SET NEMED6C    AT LEVEL 148 AS OF 09/02/99                      
         EJECT                                                                  
***********************************************************************         
*HOOK - BUILDS THE HEADER.SUPPLEMENTS THE SSPECS IN NEMED14           *         
***********************************************************************         
HOOK     NTR1                                                                   
*                                                                               
* MAIN HEADER INFO                                                              
*                                                                               
         LA    R5,P4                                                            
         CLC   CLPRNM+2(6),=C'TOTALS'                                           
         BE    SL1                                                              
         TM    NBSUBMSK,NBSBMNET                                                
         BNZ   SL1                                                              
         CLI   NBMODE,NBREQLST                                                  
         BE    SL1                                                              
         LA    R5,P1                                                            
         MVC   CLPRNM(L'RES1),RES    PXZ CHANGE                                 
         MVC   P2,P1                                                            
         XC    P1,P1                                                            
         MVC   CLPRNM(4),NETSAV                                                 
*                                                                               
         DROP  R4                                                               
SL1      LA    R5,H1               BASE ADDRESS FOR HEADER OFFSETS              
         USING PHEAD,R5                                                         
         MVC   PHCLI,NBSELCLI                                                   
         L     R4,ATWA                                                          
         USING T31EFFD,R4                                                       
         MVC   PHCLNM,SPLCLIN                                                   
         MVC   PHPRD(6),SPLPRO                                                  
         MVC   PHPRNM,SPLPRON                                                   
         MVC   PHEST(7),SPLEST                                                  
         MVC   PHESNM,SPLESTN                                                   
         MVC   PHDPT,SPLDPT                                                     
         MVC   PHDPNM,SPLDPTN                                                   
         CLI   NBSELPAK,0          PACKAGE=ALL?                                 
         BNE   PAKAG                                                            
         OC    NBSELPKF,NBSELPKF   PACKAGE FILTER?                              
         BZ    PAKALL                                                           
         MVC   PHPAK(5),SPLPAK     PAK=FILTER                                   
         B     HOOK1                                                            
PAKALL   MVC   PHPAK(3),=C'ALL'    PAK=ALL                                      
         B     HOOK1                                                            
PAKAG    EDIT  NBSELPAK,(3,PHPAK),ALIGN=LEFT                                    
         CLI   SPLNET,0                                                         
         BE    HOOK1                                                            
         CLC   SPLNET(3),=C'ALL'                                                
         BE    HOOK1                                                            
         DROP  R4                                                               
         MVC   PHPKNM(L'PHPKNM),NBPAKNAM                                        
         MVI   PHDOLL,C'$'                                                      
         L     R4,PNETA                                                         
         EDIT  (4,0(R4)),(9,PHPKVL),COMMAS=YES,ALIGN=LEFT,ZERO=BLANK            
         CLC   PCTFEED,=2X'00'                                                  
         BE    HOOK1                                                            
         MVC   H7+98(9),=C'FEED PCT='                                           
         LA    R4,H7+108                                                        
         EDIT  (B2,PCTFEED),(5,0(R4)),2                                         
*                                                                               
*COLUMN HEADERS                                                                 
HOOK1    CLI   OFFLINE,C'Y'                                                     
         BNE   SL2                                                              
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVC   BOXWT(2),=X'0100'                                                
         MVI   BOXOFF,0                                                         
         CLEAN BOXCOLS,132,C' '                                                 
         CLEAN BOXROWS,100,C' '                                                 
         MVI   BOXROWS+8,C'T'                                                   
*****    MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+12,C'M'                                                  
         MVI   BOXROWS+57,C'B'                                                  
         BCOL  BOXCOLS,C,0,6,C=L                                                
         ZIC   R2,DT                                                            
         LA    R3,BOXCOLS+C7-C0                                                 
SL0      LTR   R2,R2                                                            
         BZ    SL01                                                             
         MVI   0(R3),C'C'                                                       
         LA    R3,27(R3)                                                        
         BCT   R2,SL0                                                           
SL01     MVI   0(R3),C'R'                                                       
         B     SL2                                                              
         DROP  R4                                                               
*                                                                               
         USING COLINFO,R5                                                       
UC       DC    CL16'(UNIV =        )'                                           
O1       EQU   (C11-C7-7)/2                                                     
O11      EQU   (C11-C7-16)/2                                                    
*                                                                               
SL2      LA    R5,H10                                                           
         ZIC   RF,DT                                                            
         LA    R2,CLVPHH                                                        
         LA    R3,UNIV1                                                         
UN       MVC   0(O1,R2),=10C'*'                                                 
         MVC   O1(7,R2),4(R3)                                                   
         CLI   7(R3),C'.'          IS IT NAD                                    
         BNE   SKIPNAD                                                          
         MVC   O1-2(11,R2),4(R3)   YES/MOVE IT ALL IN NNN.AANN-NN               
         MVC   O1+9(O1-3,R2),=10C'*'                                            
         B     *+10                                                             
SKIPNAD  MVC   O1+7(O1-1,R2),=10C'*'                                            
         CLC   NBEFFAGY,=C'TH'     MAKE TH LIKE DF                              
         BE    DOITALSO                                                         
         CLC   NBEFFAGY,=C'SJ'     MAKE TH LIKE DF                              
         BE    DOITALSO                                                         
         CLC   NBEFFAGY,=C'DF'                                                  
         BE    *+14                                                             
         CLC   NBEFFAGY,=C'NF'                                                  
         BNE   UN1                                                              
*        LA    R5,H11                                                           
DOITALSO LR    R5,R2                                                            
         LA    R5,132(R5)                                                       
         MVC   O11(16,R5),UC                                                    
         LA    R5,O11+8(R5)                                                     
         EDIT  (4,0(R3)),(7,0(R5)),ALIGN=LEFT                                   
         CLC   PCTFEED,=2X'00'                                                  
         BE    UN1                                                              
         BAS   RE,FEEDCLC                                                       
         B     UN1                                                              
*                                                                               
FEEDCLC  NTR1                                                                   
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),PCTFEED                                                
         L     R3,0(R3)                                                         
         M     R2,FULL                                                          
         D     R2,=F'10000'                                                     
         EDIT  (R3),(7,0(R5))                                                   
         XIT1                                                                   
*                                                                               
*        LA    R5,H10                                                           
UN1      LA    R3,16(R3)                                                        
         LA    R2,27(R2)                                                        
*        LA    R3,16(R3)                                                        
         BCT   RF,UN                                                            
*                                                                               
SL3      LA    R5,H12                                                           
         MVC   CLNET(3),=C'PRD'                                                 
         MVC   CLADATE(4),=C'DATE'                                              
         MVC   CLPRNM(12),=C'PROGRAM NAME'                                      
         MVC   CLDUR+1(3),=C'LEN'                                               
         MVC   CLCOST+4(4),=C'COST'                                             
         MVC   CLHHSH(3),=C'SHR'                                                
         MVC   CLHUTVH+1(3),=C'HUT'                                             
         LA    R4,CON1                                                          
         BAS   RE,SL6                                                           
*                                                                               
SL4      CLI   OFFLINE,C'Y'                                                     
         BE    SL5                                                              
         LA    R5,H13                                                           
         MVC   CLNET(3),=12C'-'                                                 
         MVC   CLADATE(4),=12C'-'                                               
         MVC   CLPRNM(12),=12C'-'                                               
         MVC   CLDUR+1(3),=12C'-'                                               
         MVC   CLCOST+4(4),=12C'-'                                              
         MVC   CLHHSH(3),=12C'-'                                                
         MVC   CLHUTVH+1(3),=12C'-'                                             
         LA    R4,CON2                                                          
         BAS   RE,SL6                                                           
*                                                                               
SL5      XIT1                                                                   
*                                                                               
SL6      NTR1                                                                   
         LA    R3,CLVPHH                                                        
         ZIC   R2,DT                                                            
SL61     MVC   1(3,R3),0(R4)                                                    
         MVC   8(4,R3),3(R4)                                                    
         MVC   16(4,R3),7(R4)                                                   
         MVC   23(3,R3),11(R4)                                                  
         LA    R3,27(R3)                                                        
         BCT   R2,SL61                                                          
         XIT1                                                                   
**********************************************************************          
CON1     DC    C'VPHIMPS GRPCPM '                                               
CON2     DC    15C'-'                                                           
         EJECT                                                                  
*          DATA SET NEMED6C    AT LEVEL 148 AS OF 09/02/99                      
CHKMAXIO NTR1                                                                   
                                                                                
         L     R5,ACOMFACS         GET MAX IO                                   
         USING COMFACSD,R5                                                      
         GOTO1 CGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
                                                                                
         CLI   MAXIOCTR,0          HAVE WE SET MAXIO ?                          
         BNE   MAX20                           YES                              
         MVC   MAXIOCTR,FATMAXIO-FACTSD(R1)    NO                               
         SR    R2,R2                                                            
         LH    R3,MAXIOCTR                                                      
         LA    R4,9                MULTIPLY MAX IO BY 9                         
         MR    R2,R4                                                            
         LA    R4,10               DIVIDE MAXIMUM BY 10                         
         DR    R2,R4                                                            
         STH   R3,MAXIOCTR                                                      
         B     MAXOK                                                            
MAX20    DS    0H                                                               
         CLC   MAXIOCTR,FATIOCNT-FACTSD(R1)   MAXED OUT ?                       
         BH    MAXOK                                                            
                                                                                
         DROP  R5                                                               
         L     R5,ATWA                                                          
         USING T31EFFD,R5                                                       
         MVC   CONHEAD(38),=C'*-IO TIMEOUT REDUCE REQUEST PARAMETERS'           
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2,DMCB                                                      
         B     MAXOK                                                            
                                                                                
MAXOK    XIT1                                                                   
         DROP  R5                                                               
                                                                                
MAXIOCTR DS    H                                                                
                                                                                
         EJECT                                                                  
*                                                                               
         GETEL R2,DATADISP,ELCODE                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*****IOSPACE  DS    CL1000                                                      
         EJECT                                                                  
*OFFSETS FOR PAGE HEADER INFORMATION                                            
         DSECT                                                                  
PHEAD    EQU   *                                                                
PHLENGTH EQU   132                                                              
*                                                                               
         ORG   PHEAD+3*PHLENGTH+11                                              
PHCLI    DS    CL3                 CLIENT ABBR                                  
         DS    CL5                                                              
PHCLNM   DS    CL20               CLIENT NAME                                   
         ORG   PHEAD+4*PHLENGTH+11                                              
PHPRD    DS    CL3                 PRODUCT ABBR                                 
         DS    CL5                                                              
PHPRNM   DS    CL20                PRODUCT NAME                                 
         ORG   PHEAD+4*PHLENGTH+58                                              
PHPAK    DS    CL3                 PACKAGE ABBR                                 
         ORG   PHEAD+5*PHLENGTH+11                                              
PHEST    DS    CL5                 ESTIMATE ABBR                                
         DS    CL3                                                              
PHESNM   DS    CL24                ESTIMATE NAME                                
         ORG   PHEAD+5*PHLENGTH+58                                              
PHPKNM   DS    CL17                PACKAGE NAME                                 
         ORG   PHEAD+5*PHLENGTH+96                                              
PHSTAT   DS    CL6                 STATUS ALPHA                                 
         ORG   PHEAD+6*PHLENGTH+11                                              
PHDPT    DS    CL4                                                              
         DS    CL4                                                              
PHDPNM   DS    CL8                                                              
         ORG   PHEAD+6*PHLENGTH+58                                              
PHDOLL   DS    CL1                 DOLLAR SIGN                                  
PHPKVL   DS    CL9                 PACKAGE VALUE(DOLLARS)                       
*                                                                               
*                                                                               
         EJECT                                                                  
*DSECT FOR PRINT LINES                                                          
         DSECT                                                                  
COLINFO  EQU   *                                                                
*                                                                               
PRNM     PL    0,14                                                             
ADATE    PL    1,5                                                              
DUR      PL    2,4                                                              
NET      PL    3,4             USED FOR PRODUCT FIELD                           
COST     PL    4,8                                                              
HHSH     PL    5,3                                                              
HUTVH    PL    6,4                                                              
*                                                                               
VPHH     PL    7,4                                                              
AUDH     PL    8,7                                                              
RATH     PL    9,7                                                              
CPMH     PL    10,5                                                             
*                                                                               
VPHW     PL    11,4                                                             
AUDW     PL    12,7                                                             
RATW     PL    13,7                                                             
CPMW     PL    14,5                                                             
*                                                                               
VPHO     PL    15,4                                                             
AUDO     PL    16,7                                                             
RATO     PL    17,7                                                             
CPMO     PL    18,5                                                             
*                                                                               
C19      DS    CL1                                                              
         EJECT                                                                  
********************WORKING STORAGE************************************         
WORKD    DSECT                                                                  
         DS    0D                                                               
*                                                                               
GTNO     DS    F                   GRAND TOTAL NO                               
GTDUR    DS    F                   GRAND TOTAL DURATION                         
GTCOST   DS    F                   GRAND TOTAL COST                             
GTHUTVH  DS    F                   GRAND TOTAL HUTVH                            
GTVPHH   DS    F                   GRAND TOTAL VPHH                             
GTAUDH   DS    F                   GRAND TOTAL AUDH                             
GTRATH   DS    F                   GRAND TOTAL RATINGH                          
GTVPHW   DS    F                   GRAND TOTAL VPHW                             
GTAUDW   DS    F                   GRAND TOTAL AUDW                             
GTRATW   DS    F                   GRAND TOTAL RATW                             
GTVPHO   DS    F                   GRAND TOTAL VPHO                             
GTAUDO   DS    F                   GRAND TOTAL AUDO                             
GTRATO   DS    F                   GRAND TOTAL RATO                             
         SPACE 3                                                                
WTNO     DS    F                   TOTAL NO                                     
WTDUR    DS    F                   TOTAL DURATION                               
WTCOST   DS    F                   TOTAL COST                                   
WTHUTVH  DS    F                   TOTAL HUTVH                                  
WTVPHH   DS    F                   TOTAL VPHH                                   
WTAUDH   DS    F                   TOTAL AUDH                                   
WTRATH   DS    F                   TOTAL RATINGH                                
WTVPHW   DS    F                   TOTAL VPHW                                   
WTAUDW   DS    F                   TOTAL AUDW                                   
WTRATW   DS    F                   TOTAL RATW                                   
WTVPHO   DS    F                   TOTAL VPHO                                   
WTAUDO   DS    F                   TOTAL AUDO                                   
WTRATO   DS    F                   TOTAL RATO                                   
         SPACE 3                                                                
WSNO     DS    F                   SAVE NO                                      
WSDUR    DS    F                   SAVE DURATION                                
WSCOST   DS    F                   SAVE COST                                    
WSHUTVH  DS    F                   SAVE HUTVH                                   
WSVPHH   DS    F                   SAVE VPHH                                    
WSAUDH   DS    F                   SAVE AUDH                                    
WSRATH   DS    F                   SAVE RATINGH                                 
WSVPHW   DS    F                   SAVE VPHW                                    
WSAUDW   DS    F                   SAVE AUDW                                    
WSRATW   DS    F                   SAVE RATINGW                                 
WSVPHO   DS    F                   SAVE VPHO                                    
WSAUDO   DS    F                   SAVE AUDO                                    
WSRATO   DS    F                   SAVE RATINGO                                 
WSHHSH   DS    F                   SAVE HOME SHARE                              
         SPACE 3                                                                
ACOST    DS    F                   AVERAGE COST                                 
AHUTVH   DS    F                   AVERAGE HUTVH                                
AVPHH    DS    F                   AVERAGE VPHH                                 
AAUDH    DS    F                   AVERAGE AUDH                                 
ARATH    DS    F                   AVERAGE RATINGH                              
ACPMH    DS    F                   AVERAGE CPMH                                 
TCPMH    DS    F                   TOTAL CPMH                                   
AVPHW    DS    F                   AVERAGE VPHW                                 
AAUDW    DS    F                   AVERAGE AUDW                                 
ARATW    DS    F                   AVERAGE RATINGW                              
ACPMW    DS    F                   AVERAGE CPMW                                 
TCPMW    DS    F                   TOTAL CPMW                                   
AVPHO    DS    F                   AVERAGE VPHO                                 
AAUDO    DS    F                   AVERAGE AUDO                                 
ARATO    DS    F                   AVERAGE RATO                                 
ACPMO    DS    F                   AVERAGE CPMO                                 
TCPMO    DS    F                   TOTAL CPMO                                   
         SPACE 3                                                                
*                                                                               
WSAVDAT  DS    CL2                 SAVE DATE                                    
DUMMY1   DS    CL3                                                              
DUMMY    DC    F'0'                                                             
SAVER    DS    CL12                                                             
WSCO     DS    F                                                                
BT       DS    CL1                                                              
DT       DS    CL1                                                              
NETSAV   DS    F                                                                
RES      DS    CL19                                                             
RES1     DS    CL14                                                             
PNET     DS    150F           $$$ FIELDS - NET NAME IN PNETB                    
PNETA    DS    F                                                                
GCOST    DS    F                                                                
UNIV1    DS    F                                                                
DEMT1    DS    CL12                                                             
UNIV2    DS    F                                                                
DEMT2    DS    CL12                                                             
UNIV3    DS    F                                                                
DEMT3    DS    CL12                                                             
PCTFEED  DS    CL2                                                              
PRDN     DS    CL1                                                              
PRDCD    DS    CL3                                                              
PRDCD2   DS    CL3                                                              
PRDCDX   DS    CL12                MORE PRODS FOR MULTI PROD                    
FIRST    DS    CL1                                                              
AIOSV    DS    CL4                                                              
PNETB    DS    150CL4              NETWORKS CORRESPONDING TO PNET               
*                                  THIS TABLE IS CONSTRUCTED FROM               
*                                  PACKAGE RECORDS - SO IF THERE                
*                                  ARE UNITS OUT THERE WITH NO PKG              
*                                  THIS WILL DUMP                               
TDOLPEN  DS    D                   GRAND TOTAL DOLLARS AND PENNIES              
NDOLPEN  DS    D                   NETWROK TOTAL DOLLARS AND PENNIES            
PROGNMSV DS    CL16                PROGRAM NAME SAVE                            
STALIST  DS    CL2000                                                           
         EJECT                                                                  
DEMOS    DSECT                                                                  
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
WORKEND  EQU   *-WORKD                                                          
         PRINT OFF                                                              
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE NETINCLN                                                       
       ++INCLUDE NEMEDFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF4D                                                       
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011NEMED24N  06/23/06'                                      
         END                                                                    
