*          DATA SET SPREPCW02  AT LEVEL 125 AS OF 05/01/02                      
*PHASE SPCW02                                                                   
*INCLUDE PRTREC                                                                 
         TITLE 'SPFXCW - CONVERT WIM PW RECORDS TO COS2'                        
SPCW02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPCW02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    FX210                                                            
         CLI   MODE,STAFRST                                                     
         BE    FX200                                                            
         CLI   MODE,STALAST                                                     
         BE    FX280                                                            
         CLI   MODE,MKTFRST                                                     
         BE    FX100                                                            
         CLI   MODE,MKTLAST                                                     
         BE    FX300                                                            
         CLI   MODE,ESTFRST                                                     
         BE    FX50                                                             
         CLI   MODE,REQFRST                                                     
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*=============================================================*                 
* ESTFRST - COMPUTE COS2 FACTOR AND WRITE ESTHDR AND CLTHDR   *                 
*=============================================================*                 
         SPACE 1                                                                
FX50     L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
* CONVERT PW FACTOR TO COS2 FACTOR                                              
         SR    R0,R0                                                            
         ICM   R0,7,EPWPCT                                                      
         N     R0,=X'007FFFFF'                                                  
         L     RF,=F'8500'                                                      
         SR    RF,R0               GET 85 - PW PCT                              
         LR    R0,RF               IN R0                                        
*                                                                               
         L     RF,=F'8500'                                                      
         M     RE,=F'2000000'      X 1000000 X 2                                
         DR    RE,R0                                                            
         SRL   RF,1                                                             
         LR    R0,RF               SAVE FACTOR IN R0                            
         ST    R0,ECOST2           STORE IN REC IN CASE WRITE=NO                
*                                                                               
         OC    ECOST2,ECOST2                                                    
         BZ    ESTERR                                                           
* WRITE ESTHDR TO FILE                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX52                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETEST                                                           
         ST    R0,ECOST2           RESTORE NEW VALUE IN RECORD                  
         XC    EPWPCT,EPWPCT                                                    
         GOTO1 PUTEST                                                           
* SET CONVERSION DATE IN CLIENT RECORD                                          
         L     R6,ADCLT                                                         
         ST    R6,AREC                                                          
         USING CLTHDRD,R6                                                       
*                                                                               
         OC    CC2CONV,CC2CONV     TEST DATE ALREADY THERE                      
         BNZ   FX52                                                             
*                                                                               
         MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                                                             
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,CC2CONV)                                    
         GOTO1 PUT                                                              
*                                                                               
FX52     B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*=============================================================*                 
* MKTFRST - READ PW RECORDS AND CALCULATE FACTOR FOR MARKET   *                 
*           THEN INSERT C2 ELEMENTS IN MKT AND STA PW RECORDS *                 
*=============================================================*                 
         SPACE 1                                                                
FX100    DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         XC    MKTTOTS,MKTTOTS                                                  
*                                                                               
         MVC   SAVEKEY,KEY         SAVE CURRENT SPTDIR KEY                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D7A'                                                  
         MVC   KEY+2(3),BAGYMD     MOVE AG-MD/CLT                               
         MVC   KEY+5(1),BPRD                                                    
         MVC   KEY+6(1),BEST                                                    
         MVC   KEY+7(2),BMKT                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(9),KEYSAVE      SAME A-M/CLT/PRD/EST/MKT                     
         BNE   FX140                                                            
*                                                                               
         GOTO1 GETBUY                                                           
*&&DO                                                                           
         CLI   QOPT3,C'Y'                                                       
         BNE   FX110                                                            
         GOTO1 =V(PRTREC),DMCB,ADBUY,(24,13),PRINT,HEXOUT                       
*&&                                                                             
FX110    L     R8,ADBUY                                                         
         USING PWRECD,R8                                                        
* TRACY SAYS ...                                                                
         MVI   ELCDLO,X'07'        LOOK FOR CURRENT DOLLAR ELEMENTS             
         MVI   ELCDHI,X'07'                                                     
         LA    R6,24(R8)                                                        
*                                                                               
FX120    BAS   RE,NEXTEL                                                        
         BNE   FX122                                                            
         USING PWCUREL,R6                                                       
*                                                                               
         L     R0,PWCURWG          GET CURRENT WIM GROSS                        
         A     R0,MKORDWG                                                       
         ST    R0,MKORDWG                                                       
*                                                                               
         L     R0,PWCURCG          GET CURRENT CLT GROSS                        
         A     R0,MKORDCG                                                       
         ST    R0,MKORDCG                                                       
         B     FX120                                                            
*                                                                               
FX122    DS    0H                                                               
         L     R0,MKORDCG                                                       
         SRDA  R0,32                                                            
         M     R0,=F'2000000'      X 1000000 X 2                                
         OC    MKORDWG,MKORDWG                                                  
         BZ    FX124                                                            
         D     R0,MKORDWG                                                       
         AHI   R1,1                ROUND                                        
         SRL   R1,1                SCALE                                        
         ST    R1,C2ORD            SAVE C2ORD FACTOR (6 DECIMALS)               
*                                                                               
         MVC   P(7),=C'MKORDWG'                                                 
         L     R0,MKORDWG                                                       
         EDIT  (R0),(10,P+10),2                                                 
*                                                                               
         MVC   P+22(7),=C'MKORDCG'                                              
         L     R0,MKORDCG                                                       
         EDIT  (R0),(10,P+32),2                                                 
*                                                                               
         L     R0,C2ORD                                                         
         MVC   P+44(5),=C'C2ORD'                                                
         EDIT  (R0),(10,P+50),6                                                 
         GOTO1 REPORT                                                           
*                                                                               
* NOW DO SAME CALC FOR LOCKED DOLLARS                                           
*                                                                               
FX124    MVI   ELCDLO,X'06'        LOOK FOR LOCKED DOLLAR ELEMENTS              
         MVI   ELCDHI,X'06'                                                     
         LA    R6,24(R8)                                                        
*                                                                               
FX130    BAS   RE,NEXTEL                                                        
         BNE   FX132                                                            
         USING PWDOLEL,R6                                                       
*                                                                               
         L     R0,PWDOLWG          GET LOCKED WIM GROSS                         
         A     R0,MKLOKWG                                                       
         ST    R0,MKLOKWG                                                       
*                                                                               
         L     R0,PWDOLCG          GET LOCKED CLT GROSS                         
         A     R0,MKLOKCG                                                       
         ST    R0,MKLOKCG                                                       
         B     FX130                                                            
*                                                                               
FX132    L     R0,MKLOKCG                                                       
         SRDA  R0,32                                                            
         M     R0,=F'2000000'      X 1000000 X 2                                
         OC    MKLOKWG,MKLOKWG                                                  
         BZ    FX134                                                            
         D     R0,MKLOKWG          GET CLT FACTOR                               
         AHI   R1,1                ROUND                                        
         SRL   R1,1                SCALE                                        
         ST    R1,C2LOCK           SAVE C2LOCK FACTOR (6 DECIMALS)              
*                                                                               
FX134    L     RE,ADEST                                                         
         L     R0,(ECOST2-ESTHDRD)(RE)                                          
         OC    C2ORD,C2ORD         TEST HAVE C2ORD                              
         BNZ   *+8                                                              
         ST    R0,C2ORD                                                         
*                                                                               
         OC    C2LOCK,C2LOCK                                                    
         BNZ   *+8                                                              
         ST    R0,C2LOCK                                                        
*                                                                               
         MVC   P(8),=C'MKLOKWG '                                                
         L     R0,MKLOKWG                                                       
         EDIT  (R0),(10,P+10),2                                                 
*                                                                               
         MVC   P+22(8),=C'MKLOKCG '                                             
         L     R0,MKLOKCG                                                       
         EDIT  (R0),(10,P+32),2                                                 
*                                                                               
         L     R0,C2LOCK                                                        
         MVC   P+44(5),=C'C2LOK'                                                
         EDIT  (R0),(10,P+50),6                                                 
         GOTO1 REPORT                                                           
*                                                                               
         XC    MKTTOTS,MKTTOTS     RESET TOTALS FOR LOCKIN RECS                 
*                                                                               
         MVC   PMED,QMED                                                        
         MVC   PCLT,CLT                                                         
         MVC   PPRD,PRD                                                         
         MVC   PEST,EST                                                         
         MVC   PMKT,MKT                                                         
         MVC   PMKTNM,MKTNM                                                     
*                                                                               
         L     R0,C2ORD                                                         
         EDIT  (R0),(9,PCOM+5),6,ALIGN=LEFT                                     
*                                                                               
         L     R0,C2LOCK                                                        
         EDIT  (R0),(9,PCOM+16),6,ALIGN=LEFT                                    
         GOTO1 REPORT                                                           
         EJECT                                                                  
*==================================================================*            
* CREATE COST2 ELEMENT AND INSERT IN MARKET PW RECORD              *            
*==================================================================*            
         SPACE 1                                                                
E        USING C2STEL,ELEM                                                      
         XC    ELEM,ELEM                                                        
         MVI   ELEM,4                                                           
         MVI   ELEM+1,C2STLENQ                                                  
         MVC   E.C2STPCT,C2LOCK                                                 
         MVC   E.C2STFCTR,C2ORD                                                 
         DROP  E                                                                
* NOW REMOVE ALL ELEMENTS X'05' - X'F0' EXCEPT X'20'                            
         L     R8,ADBUY                                                         
         MVI   ELCDLO,X'05'                                                     
         MVI   ELCDHI,X'F0'                                                     
         LA    R6,24(R8)                                                        
         BAS   RE,NEXTEL                                                        
         BNE   FX134B                                                           
*                                                                               
FX134A   CLI   0(R6),X'20'         KEEP X'20'                                   
         BE    FX134B                                                           
*                                                                               
         GOTO1 RECUP,DMCB,ADBUY,(R6)                                            
*                                                                               
FX134B   BAS   RE,NEXTEL2                                                       
         BE    FX134A                                                           
*                                                                               
FX134C   LA    R6,24(R8)                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               INSERT AFTER X'01' ELEM                      
         GOTO1 RECUP,DMCB,ADBUY,ELEM,(R6)                                       
         GOTO1 PUTBUY                                                           
*                                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   FX135                                                            
         GOTO1 =V(PRTREC),DMCB,ADBUY,(24,13),PRINT,HEXOUT                       
         SPACE 1                                                                
*==================================================================*            
* INSERT DOLLAR ELEMENTS IN STATION PW RECORDS                                  
*==================================================================*            
         SPACE 1                                                                
FX135    GOTO1 SEQ                                                              
         CLC   KEY(9),KEYSAVE      SAME A-M/CLT/PRD/EST/MKT                     
         BNE   FX140                                                            
*                                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         XC    STATOTS,STATOTS                                                  
*                                                                               
         L     R8,ADBUY                                                         
         LA    R6,24(R8)                                                        
         MVI   ELCDLO,X'06'        LOOK FOR LOCKED DOLLAR ELEMENTS              
         MVI   ELCDHI,X'06'                                                     
*                                                                               
FX136    BAS   RE,NEXTEL                                                        
         BNE   FX138                                                            
         USING PWDOLEL,R6                                                       
*                                                                               
         L     R0,PWDOLWG          GET LOCKED WIM GROSS                         
         A     R0,STLOKWG                                                       
         ST    R0,STLOKWG                                                       
*                                                                               
         L     R0,PWDOLCG          GET LOCKED CLT GROSS                         
         A     R0,STLOKCG                                                       
         ST    R0,STLOKCG                                                       
         B     FX136                                                            
*                                                                               
E        USING C2STEL,ELEM                                                      
FX138    XC    ELEM,ELEM                                                        
         MVI   ELEM,4                                                           
         MVI   ELEM+1,C2STLENQ                                                  
         MVC   E.C2STLKC2,STLOKCG                                               
         MVC   E.C2STLK,STLOKWG                                                 
         DROP  E                                                                
*                                                                               
         LA    R6,24(R8)                                                        
         MVI   ELCDLO,5                                                         
         MVI   ELCDHI,X'F0'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   FX138B                                                           
*                                                                               
FX138A   GOTO1 RECUP,DMCB,ADBUY,(R6)                                            
         BAS   RE,NEXTEL2                                                       
         BE    FX138A                                                           
*                                                                               
FX138B   LA    R6,24(R8)                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         GOTO1 RECUP,DMCB,ADBUY,ELEM,(R6)                                       
         GOTO1 PUTBUY                                                           
*                                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   FX138X                                                           
         GOTO1 =V(PRTREC),DMCB,ADBUY,(24,13),PRINT,HEXOUT                       
FX138X   B     FX135                                                            
*                                                                               
         EJECT                                                                  
*==================================================================*            
* UPDATE LOCKIN RECORDS WITH COS2 VALUES                           *            
*==================================================================*            
         SPACE 1                                                                
FX140    MVC   KEY,SAVEKEY         RESTORE SPTDIR KEY                           
         GOTO1 HIGH                                                             
*                                                                               
         XC    STATOTS,STATOTS                                                  
*                                                                               
K        USING SLKRECD,XKEY                                                     
         XC    XKEY,XKEY                                                        
         MVC   K.SLKKEY(2),=X'0D73'                                             
         MVC   K.SLKKAGMD(3),BAGYMD   A-M/CLT                                   
         MVC   K.SLKKMKT,BMKT         MKT                                       
*                                                                               
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',XKEYSAVE,XKEY                     
         CLC   XKEY(SLKKSTA-SLKRECD),XKEYSAVE   TYPE/A-M/CLT/MKT                
         BE    FX141                                                            
         MVC   P(31),=C'NO STATION LOCKIN RECORDS FOUND'                        
         GOTO1 REPORT                                                           
         B     FX170                                                            
*                                                                               
FX141    GOTO1 DATAMGR,DMCB,DMRSEQ,=C'XSPDIR',XKEYSAVE,XKEY                     
         CLC   XKEY(SLKKSTA-SLKRECD),XKEYSAVE   TYPE/A-M/CLT/MKT                
         BNE   FX170               PRINT MARKET TOTALS                          
*                                                                               
* FILTER ON PRD/EST                                                             
*                                                                               
FX142    CLC   K.SLKKPRD,BPRD                                                   
         BNE   FX141                                                            
         CLC   K.SLKKEST,BEST                                                   
         BNE   FX141                                                            
*&&DO                                                                           
* THIS CODE TRACES XSPFILE I/O                                                  
         CLI   QOPT1,C'Y'                                                       
         BNE   FX143                                                            
         MVC   P(4),=C'XKEY'                                                    
         GOTO1 HEXOUT,DMCB,XKEY,P+10,32,=C'TOG'                                 
         MVC   P2(8),=C'XKEYSAVE'                                               
         GOTO1 HEXOUT,DMCB,XKEYSAVE,P2+10,32,=C'TOG'                            
         GOTO1 REPORT                                                           
*&&                                                                             
FX143    GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFILE',XKEY+36,ADBUY,DMWORK             
*                                                                               
         L     R8,ADBUY                                                         
         LA    R6,42(R8)           POINT TO FIRST ELEMENT                       
*                                                                               
         MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
*                                                                               
FX144    BAS   RE,NEXTEL                                                        
         BNE   FX150                                                            
* COMPUTE COS2 DOLLARS                                                          
         USING LOKEL,R6                                                         
*                                                                               
         ICM   R0,15,LOKDOLS                                                    
         A     R0,STLOKWG                                                       
         ST    R0,STLOKWG                                                       
*                                                                               
         ICM   R1,15,LOKDOLS                                                    
         AR    R1,R1               X 2                                          
         M     R0,C2LOCK                                                        
         D     R0,=F'1000000'                                                   
         SRL   R1,1                                                             
         STCM  R1,15,LOKDOL2                                                    
*                                                                               
         A     R1,STLOKCG                                                       
         ST    R1,STLOKCG                                                       
*                                                                               
* NOW DO THE SAME FOR NET                                                       
*                                                                               
         ICM   R1,15,LOKNET                                                     
         BNZ   FX146                                                            
* FIRST COMPUTE NET DOLLARS                                                     
         ICM   R1,15,LOKDOLS                                                    
         M     R0,=F'170'          X 85 X 2                                     
         D     R0,=F'100'                                                       
         SRL   R1,1                                                             
         STCM  R1,15,LOKNET                                                     
*                                                                               
FX146    ICM   R0,15,LOKNET        ACCUMULATE NET DOLLARS                       
         A     R0,STLOKWN                                                       
         ST    R0,STLOKWN                                                       
* COMPUTE NET2 DOLLARS                                                          
         ICM   R1,15,LOKNET                                                     
         AR    R1,R1                                                            
         M     R0,C2LOCK                                                        
         D     R0,=F'1000000'                                                   
         SRL   R1,1                                                             
         STCM  R1,15,LOKNET2                                                    
         A     R1,STLOKCN                                                       
         ST    R1,STLOKCN                                                       
         B     FX144                                                            
*                                                                               
FX150    LA    R4,STATOTS                                                       
         LA    R5,MKTTOTS                                                       
         LA    RF,L'MKTTOTS/4                                                   
*                                                                               
FX152    L     R0,0(R4)            ADD TO MARKET TOTALS                         
         A     R0,0(R5)                                                         
         ST    R0,0(R5)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   RF,FX152                                                         
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX154                                                            
         GOTO1 DATAMGR,DMCB,PUTREC,=C'XSPFILE',XKEY+36,ADBUY,DMWORK             
*                                                                               
FX154    MVC   PMED,QMED                                                        
         MVC   PCLT,CLT                                                         
         MVC   PPRD,PRD                                                         
         MVC   PEST,EST                                                         
         MVC   PMKT,MKT                                                         
         GOTO1 MSUNPK,DMCB,(X'80',K.SLKKMKT),PMKT,PSTA                          
         CLI   PSTA+5,C' '                                                      
         BNH   *+8                                                              
         MVI   PSTA+4,C'/'                                                      
*                                                                               
         MVC   PDPT,K.SLKKDPT                                                   
         SR    R0,R0                                                            
         IC    R0,K.SLKKLEN                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PSLN,DUB                                                         
*                                                                               
         LA    R4,STLOKWG          SET TO PRINT LOCKED WG/WN/CG/CN              
         LA    R5,4                                                             
         LA    R2,PWIMG                                                         
*                                                                               
FX160    L     R0,0(R4)                                                         
         EDIT  (R0),(10,(R2)),2                                                 
         LA    R2,L'PWIMG+1(R2)                                                 
         LA    R4,8(R4)                                                         
         BCT   R5,FX160                                                         
*                                                                               
         GOTO1 REPORT                                                           
         B     FX141                                                            
         DROP  K                                                                
*                                                                               
FX170    MVC   P(13),=C'MARKET TOTALS'                                          
         LA    R4,MKLOKWG                                                       
         LA    R5,4                                                             
         LA    R2,PWIMG                                                         
*                                                                               
FX172    L     R0,0(R4)                                                         
         EDIT  (R0),(10,(R2)),2                                                 
         MVI   10(R2),C'*'                                                      
         LA    R2,L'PWIMG+1(R2)                                                 
         LA    R4,8(R4)                                                         
         BCT   R5,FX172                                                         
*                                                                               
         MVI   P2,0                FORCE LINE SKIP                              
*                                                                               
         GOTO1 REPORT                                                           
         XC    MKTTOTS,MKTTOTS                                                  
         B     EXIT                                                             
         EJECT                                                                  
*===============================================================*               
* STAFRST PROCESSING                                            *               
*===============================================================*               
         SPACE 1                                                                
FX200    XC    STATOTS,STATOTS                                                  
         CLI   RCSUBPRG,2                                                       
         BE    *+8                                                              
         MVI   FORCEMID,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
         B     EXIT                                                             
*===============================================================*               
* BUY RECORD PROCESSING                                         *               
*===============================================================*               
         SPACE 1                                                                
FX210    DS    0H                                                               
         XC    BUYTOTS,BUYTOTS                                                  
         GOTO1 GETBUY                                                           
*                                                                               
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'73'                                                     
         MVI   ELCDHI,X'73'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   FX212                                                            
         GOTO1 RECUP,DMCB,ADBUY,(R6)                                            
*                                                                               
FX212    XC    DUB,DUB                                                          
         MVI   DUB,X'73'                                                        
         MVI   DUB+1,6                                                          
         MVC   DUB+2(4),C2LOCK                                                  
         GOTO1 RECUP,DMCB,ADBUY,DUB,(R6)                                        
*                                                                               
         GOTO1 PUTBUY                                                           
*                                                                               
FX220    LA    R6,BDELEM           ADD UP DOLLARS IN RECORD                     
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
*                                                                               
FX222    BAS   RE,NEXTEL                                                        
         BNE   FX230                                                            
*                                                                               
         GOTO1 GETRATE,DMCB,(X'FF',SPOTS),ADBUY,(R6)                            
*                                                                               
         L     R0,GROSS                                                         
         A     R0,BUORDWG                                                       
         ST    R0,BUORDWG                                                       
         L     R0,NET                                                           
         A     R0,BUORDWN                                                       
         ST    R0,BUORDWN                                                       
*                                                                               
         MVC   SPOTS(4),=C'COS2'                                                
         GOTO1 GETRATE,DMCB,(X'FF',SPOTS),ADBUY,(R6)                            
*                                                                               
         L     R0,GROSS                                                         
         A     R0,BUORDCG                                                       
         ST    R0,BUORDCG                                                       
         L     R0,NET                                                           
         A     R0,BUORDCN                                                       
         ST    R0,BUORDCN                                                       
         B     FX222                                                            
*                                                                               
FX230    BC    15,FX232                                                         
         OI    FX230+1,X'F0'                                                    
         GOTO1 =V(PRTREC),DMCB,ADBUY,(24,13),PRINT,HEXOUT                       
*                                                                               
* POST TO STATION/MARKET TOTALS                                                 
*                                                                               
FX232    LA    R4,BUORDWG                                                       
         LA    R5,STORDWG                                                       
         LA    R6,MKORDWG                                                       
         LA    RF,8                                                             
*                                                                               
FX234    L     R0,0(R4)                                                         
         A     R0,0(R5)                                                         
         ST    R0,0(R5)                                                         
*                                                                               
         L     R0,0(R4)                                                         
         A     R0,0(R6)                                                         
         ST    R0,0(R6)                                                         
*                                                                               
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         LA    R6,4(R6)                                                         
         BCT   RF,FX234                                                         
* PRINT BUY TOTALS                                                              
         CLI   QOPT2,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,PRTBUY                                                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*==================================================================*            
* STALAST - PRINT STATION TOTALS                                   *            
*==================================================================*            
         SPACE 1                                                                
FX280    LA    R4,STORDWG                                                       
         LA    R5,4                                                             
         LA    R2,PWIMG                                                         
         MVC   P(14),=C'STATION TOTALS'                                         
*                                                                               
FX282    L     R0,0(R4)                                                         
         EDIT  (R0),(10,(R2)),2                                                 
         MVI   10(R2),C'*'                                                      
         LA    R4,8(R4)                                                         
         LA    R2,L'PWIMG+1(R2)                                                 
         BCT   R5,FX282                                                         
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*==================================================================*            
* MKTLAST - PRINT STATION TOTALS                                   *            
*==================================================================*            
         SPACE 1                                                                
FX300    LA    R4,MKORDWG                                                       
         LA    R5,4                                                             
         LA    R2,PWIMG                                                         
         MVC   P(13),=C'MARKET TOTALS'                                          
*                                                                               
FX302    L     R0,0(R4)                                                         
         EDIT  (R0),(10,(R2)),2                                                 
         MVI   10(R2),C'*'                                                      
         LA    R4,8(R4)                                                         
         LA    R2,L'PWIMG+1(R2)                                                 
         BCT   R5,FX302                                                         
         MVI   P2,0                                                             
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
FX900    GOTO1 AENDREQ                                                          
*                                                                               
NEXTEL   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   NEXTEL2                                                          
         LTR   RE,RE               RETURN WITH CC NEQ                           
         BR    RE                                                               
NEXTEL2  CLC   0(1,R6),ELCDLO                                                   
         BL    NEXTEL                                                           
         CLC   0(1,R6),ELCDHI                                                   
         BH    NEXTEL                                                           
         CR    RE,RE                                                            
         BR    RE                  EXIT WITH CC EQ                              
*                                                                               
ESTERR   LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   PMED,QMED                                                        
         MVC   PCLT,CLT                                                         
         MVC   PPRD,PRD                                                         
         MVC   PEST,EST                                                         
         MVC   PCOM(14),=C'NO COST2 VALUE'                                      
         GOTO1 REPORT                                                           
         MVI   MODE,ESTLAST                                                     
         B     EXIT                                                             
         DROP  R5                                                               
*                                                                               
EQXIT    CR    RB,RB               SET CC EQ                                    
         B     XIT                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
XIT      XIT1                                                                   
BUYFLAG  DS    C                                                                
ELCDLO   DS    C                                                                
ELCDHI   DS    C                                                                
SAVEKEY  DS    XL13                                                             
         DS    0D                                                               
         DC    CL8'**ELEM**'                                                    
ELEM     DS    XL64                                                             
*                                                                               
C2ORD    DS    F                                                                
C2LOCK   DS    F                                                                
         DS    0D                                                               
         DC    CL8'*BUYTOTS'                                                    
BUYTOTS  DS    0XL32                                                            
BUORDWG  DS    F                                                                
         DS    F                                                                
BUORDWN  DS    F                                                                
         DS    F                                                                
BUORDCG  DS    F                                                                
         DS    F                                                                
BUORDCN  DS    F                                                                
         DS    F                                                                
         DS    0D                                                               
         DC    CL8'*MKTTOTS'                                                    
MKTTOTS  DS    0XL32                                                            
MKORDWG  DS    F                                                                
MKLOKWG  DS    F                                                                
MKORDWN  DS    F                                                                
MKLOKWN  DS    F                                                                
MKORDCG  DS    F                                                                
MKLOKCG  DS    F                                                                
MKORDCN  DS    F                                                                
MKLOKCN  DS    F                                                                
         DS    0D                                                               
         DC    CL8'*STATOTS'                                                    
STATOTS  DS    0XL32                                                            
STORDWG  DS    F                                                                
STLOKWG  DS    F                                                                
STORDWN  DS    F                                                                
STLOKWN  DS    F                                                                
STORDCG  DS    F                                                                
STLOKCG  DS    F                                                                
STORDCN  DS    F                                                                
STLOKCN  DS    F                                                                
         DS    0D                                                               
         DC    CL8'**XKEY**'                                                    
XKEY     DS    XL48                                                             
XKEYSAVE DS    XL48                                                             
         EJECT                                                                  
*---------------------------------------------------------------*               
* PRINT THE CURRENT RECORD. R6 POINTS TO PWDOLEL                *               
*---------------------------------------------------------------*               
         SPACE 1                                                                
PRTBUY   NTR1                                                                   
*                                                                               
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
*                                                                               
PRTBUY2  MVC   PAGY,BUYALPHA                                                    
*                                                                               
         SR    RE,RE                                                            
         IC    RE,0(R8)                                                         
         N     RE,=X'0000000F'                                                  
         BCTR  RE,0                                                             
         LA    RE,MDTAB(RE)                                                     
         MVC   PMED,0(RE)                                                       
         B     PRTBUY4                                                          
MDTAB    DC    C'TRNX'                                                          
*                                                                               
PRTBUY4  GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
*                                                                               
         L     RE,ADCLT                                                         
         LA    RE,CLIST-CLTHDR(RE)                                              
*                                                                               
PRTBUY6  CLC   BDMASPRD(1),3(RE)                                                
         BE    PRTBUY8                                                          
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   PRTBUY6                                                          
*                                                                               
         ZIC   R0,BDMASPRD                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PPRD,DUB                                                         
         B     PRTBUY10                                                         
*                                                                               
PRTBUY8  MVC   PPRD,0(RE)                                                       
*                                                                               
PRTBUY10 SR    R0,R0                                                            
         IC    R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
*                                                                               
         MVI   PEST+3,C'-'                                                      
         IC    R0,BUYKEY+10                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',BUYKMSTA),PMKT,PSTA                           
         CLI   PSTA+5,C' '                                                      
         BNH   *+8                                                              
         MVI   PSTA+4,C'/'                                                      
*                                                                               
         LA    R4,BUYTOTS                                                       
         LA    R5,4                                                             
         LA    R2,PWIMG                                                         
*                                                                               
PRTBUY20 L     R0,0(R4)                                                         
         EDIT  (R0),(10,(R2)),2                                                 
         AHI   R4,8                                                             
         LA    R2,L'PWIMG+1(R2)                                                 
         BCT   R5,PRTBUY20                                                      
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
PRTBUYX  B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
SPWORKD  DSECT                                                                  
         ORG   P                                                                
PLINED   DS    0CL132                                                           
PAGY     DS    CL2                                                              
         DS    CL1                                                              
PMED     DS    CL1                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL1                                                              
PMKT     DS    CL4                                                              
         DS    CL1                                                              
PSTA     DS    CL8                                                              
         DS    CL1                                                              
PDPT     DS    CL1                                                              
PSLN     DS    CL2                                                              
         DS    CL1                                                              
PWIMG    DS    CL10                                                             
         DS    CL1                                                              
PWIMN    DS    CL10                                                             
         DS    CL2                                                              
PCLTG    DS    CL10                                                             
         DS    CL1                                                              
PCLTN    DS    CL10                                                             
         ORG   PWIMG                                                            
PCOM     DS    CL32                                                             
         ORG   PSTA                                                             
PMKTNM   DS    CL24                                                             
         ORG                                                                    
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPGENWIPW                                                      
       ++INCLUDE SPGENXLK                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'125SPREPCW02 05/01/02'                                      
         END                                                                    
