*          DATA SET SPLFM13    AT LEVEL 115 AS OF 05/01/02                      
*PHASE T21913A                                                                  
         TITLE 'SPLFM13 - ESTHDR   T21913'                                      
         SPACE 2                                                                
*****  CHANGE LOG                                                               
*                                                                               
* SCHO  4/15/99   REQUEST UPDATE OF BUYLINE ON CHANGE OF ECOST2                 
* ABEA  12/08/98  CASH=PRD OPTION FOR TRADE PRODUCT ESTIMATES                   
* SPRI  08/21/98  ALLOW CHANGES TO ESTIMATE DATES W/O FULL VALIDATION           
* BPLA  8/17/98   CHANGE TO MASTER LINIDTAB FOR HDTO                            
* SPRI  08/11/98  ADD SLN=NN OPTION TO RESTRICT BUYS TO A SPOT LENGTH           
* SPRI  08/06/98  ACCEPT LOCK DATES IN STATUS FIELD                             
* RKOH  05/25/98  NEW OPTION 'COS2=' FOR COST FACTOR                            
* SMYE  05/06/98  MORE MASTER TERMINAL CHANGES                                  
* MHER  04/16/98  SUPPORT FOR EF1OOWPW FLAG                                     
* SMYE  03/25/98  ANOTHER MASTER TERMINAL CHG                                   
* JDON  11/12/96  CHANGE PACK DATES TO DATCON FOR 99+                           
*                                                                               
*                                                                               
T21913   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21913,RR=R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         ST    R9,RELO                                                          
*                                                                               
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)            SET MEDIA IN USE                             
         USING FACTSD,R1                                                        
         MVC   OVSYS,FAOVSYS       2=SPOT,3=NET                                 
         MVC   LINID,FALINE                                                     
         MVC   LINADDR,FAADDR                                                   
         MVC   OVSYS,FAOVSYS       2=SPOT,3=NET                                 
         DROP  R1,RF                                                            
*                                                                               
         MVC   ESTRBKD,=CL11'RATING BOOK'                                       
         CLI   SVEBCMED,C'N'                                                    
         BNE   *+10                                                             
         MVC   ESTRBKD,=CL11'HUT FORMULA'                                       
         FOUT  ESTRBKDH                                                         
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING ESTHDRD,R8                                                       
         CLI   SVFMTSW,0                TEST FORMAT OR EDIT                     
         BE    FMT                                                              
         GOTO1 =A(EDT),RR=RELO                                                  
         CLI   WORK,C'S'           CHK FOR STATUS CHG                           
         BNE   FMT                 REFORMAT REC ON SUCESSFUL EDIT               
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
FMT      MVC   KEY,SVKEY                                                        
         OC    KEY+14(4),KEY+14                                                 
         BZ    FMTX                                                             
         GOTO1 GETREC                                                           
         TM    EPRDCD,X'80'        SEE IF NEW NETPAK ESTHDR                     
         BZ    *+10                                                             
         MVC   LFMKEXP+50(2),=C' N'                                             
         FOUT  LFMKEXPH                                                         
         FOUT  ESTDESCH,EDESC,20                                                
         TM    ECNTRL,X'04'                                                     
         BZ    FMT40                                                            
         FOUT  ESTSTATH,=C'HOLD    ',8                                          
         B     FMT60                                                            
*                                                                               
FMT40    TM    ECNTRL,X'08'                                                     
         BZ    FMT45                                                            
         FOUT  ESTSTATH,=C'LOCK    ',8                                          
         B     FMT60                                                            
*                                                                               
FMT45    OC    ELOCKYM,ELOCKYM                                                  
         BZ    FMT50                                                            
         MVC   ESTSTAT,SPACES                                                   
         MVC   WORK(L'ELOCKYM),ELOCKYM     MOVE YM TO WORK                      
         NI    WORK+1,X'FF'-X'80'-X'40'    TURN OFF FLAG BITS                   
         GOTO1 VDATCON,DMCB,(3,WORK),(6,ESTSTAT)                                
         TM    ELOCKMON,X'80'                                                   
         BNO   *+8                                                              
         MVI   ESTSTAT+6,C'-'              TEST IF PRIOR                        
         TM    ELOCKMON,X'40'                                                   
         BNO   *+8                                                              
         MVI   ESTSTAT+6,C'+'              TEST IF SUBSEQUENT                   
         FOUT  ESTSTATH                                                         
         B     FMT60                                                            
*                                                                               
FMT50    FOUT  ESTSTATH,SPACES,8                                                
*                                                                               
FMT60    GOTO1 VDATCON,DMCB,(0,ESTART),(5,ESTSTRD)                              
         FOUT  ESTSTRDH                                                         
         GOTO1 VDATCON,DMCB,(0,EEND),(5,ESTENDD)                                
         FOUT  ESTENDDH                                                         
         OC    EBILLBAS(5),EBILLBAS                                             
         BNZ   FMT70                    NO BILLING FORMULA                      
         FOUT  ESTBBASH,SPACES,5                                                
         FOUT  ESTCPCTH,SPACES,8                                                
         FOUT  ESTCBASH,SPACES,5                                                
         B     FMT120                                                           
*                                                                               
FMT70    MVC   ESTBBAS,=CL5'CNET'                                               
         TM    EBILLBAS,X'50'                                                   
         BO    FMT80                                                            
         MVC   ESTBBAS,=CL5'NET'                                                
         TM    EBILLBAS,X'10'                                                   
         BO    FMT80                                                            
         MVC   ESTBBAS,=C'CGROS'                                                
         TM    EBILLBAS,X'40'                                                   
         BO    FMT80                                                            
         MVC   ESTBBAS,=C'GROSS'                                                
         EJECT                                                                  
*                                                                               
FMT80    FOUT  ESTBBASH                                                         
         L     R5,EBILLCOM                                                      
         LTR   R5,R5                                                            
         BNZ   FMT90                                                            
         FOUT  ESTCPCTH,SPACES,8                                                
         FOUT  ESTCBASH,SPACES,5                                                
         B     FMT120                                                           
*                                                                               
FMT90    LPR   RF,R5                                                            
         C     RF,=F'1000000'      +/-100.0000 WONT FIT                         
         BNE   FMT100                                                           
         MVC   ESTCPCT+1(3),=C'100'                                             
         B     FMT110                                                           
*                                                                               
FMT100   EDIT  (R5),(8,ESTCPCT),4,FLOAT=+,ALIGN=LEFT                            
*                                                                               
FMT110   LTR   R5,R5                                                            
         BNM   *+8                                                              
         MVI   ESTCPCT,C'-'                                                     
         FOUT  ESTCPCTH                                                         
         MVC   ESTCBAS,=CL5'NET'                                                
         TM    EBILLBAS,X'01'                                                   
         BO    *+10                                                             
         MVC   ESTCBAS,=C'GROSS'                                                
         FOUT  ESTCBASH                                                         
         EJECT                                                                  
*                                                                               
FMT120   XC    ESTDEMS,ESTDEMS          FORMAT DEMOS                            
         XC    ESTDEM2,ESTDEM2          CLEAR LINE 2                            
         OC    EDEMLST(3),EDEMLST  SEE IF I HAVE ANY DEMOS                      
         BZ    FMT170                                                           
         XC    REC2(200),REC2                                                   
         XC    REC2+200(200),REC2+200                                           
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING DBLOCKD,R5                                                       
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         CLI   OVSYS,3             CHECK FOR NETWORK                            
         BNE   *+10                                                             
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   SVEBCMED,C'R'       FOR MEDIA = RADIO                            
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVAPROF+7,C'C'        SEE IF CANADIAN AGY                        
         BNE   FMT130                                                           
         CLI   SVCLEX,C'U'         CANADIAN - SEE IF USING US DEMOS             
         BE    FMT130              YES                                          
         MVI   DBSELMED,C'C'                                                    
*                                                                               
FMT130   MVC   DMCB+4(4),=X'D9000AE0'   DEMOCON                                 
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(14,EDEMLST),(13,REC2),(C'S',DBLOCK),EUSRNMS           
         DROP  R5                                                               
         LA    R5,ESTDEMS                                                       
         LA    R2,EDEMLST                                                       
         LA    R6,L'ESTDEMS(R5)                                                 
         LA    R7,REC2                                                          
*                                                                               
FMT140   CLI   0(R7),C' '                                                       
         BNH   FMT160              LAST DEMO                                    
         BAS   RE,FMTDEMO          FORMAT DEMO - RETURNS WITH LENGTH            
*                                  WORK AND WORK+1 HAS DEMO DESC                
         ZIC   R1,WORK                                                          
         LR    R0,R5               SEE IF IT WILL FIT ON THIS LINE              
         AR    R0,R1                                                            
         CR    R0,R6                                                            
         BNH   FMT150                                                           
         BCTR  R5,0                                                             
         CLI   0(R5),C','                                                       
         BNE   *+8                                                              
         MVI   0(R5),C' '          BLANK LAST COMMA                             
         LA    R5,ESTDEM2                                                       
         LA    R6,L'ESTDEM2(R5)                                                 
*                                                                               
FMT150   BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK+1                                                   
         AR    R5,R1                                                            
         LA    R5,1(R5)                                                         
         CR    R5,R6               SEE IF AT END OF LINE                        
         BNL   *+8                                                              
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         LA    R7,11(R7)           NEXT DEMO IN REC2                            
         LA    R2,3(R2)            NEXT DEMO IN EDEMLST                         
         B     FMT140                                                           
*                                                                               
FMT160   BCTR  R5,0                                                             
         CLI   0(R5),C','          BLANK LAST COMMA                             
         BNE   FMT170                                                           
         MVI   0(R5),C' '                                                       
         EJECT                                                                  
*                                                                               
FMT170   FOUT  ESTDEMSH                                                         
         FOUT  ESTDEM2H                                                         
         XC    ESTWTS,ESTWTS                                                    
         LA    R5,ESTWTS                                                        
         LA    R6,L'ESTWTS(R5)                                                  
         CLI   EWGTNM,C' '         SEE IF I HAVE WEIGHTS                        
         BNH   FMT310              GO CHECK FOR TARGETS                         
         LA    R7,REC2                                                          
         LA    R3,DMAX             FOR BCT                                      
         LA    R2,EDEMLST                                                       
         LA    R4,EWGTLST                                                       
*                                                                               
FMT180   CLI   0(R4),0             SEE IF THIS DEMO HAS A WEIGHT                
         BE    FMT300                                                           
         BAS   RE,FMTDEMO                                                       
         CLI   1(R2),X'21'         SEE IF USER DEMO                             
         BNE   FMT190                                                           
         MVI   WORK,2              SO ONLY UN=NN WILL APPEAR                    
*                                  ON WEIGHTS LINE                              
FMT190   ZIC   R1,WORK             WORK HAS LENGHT                              
         LR    R0,R5               SEE IF IT WILL FIT ON THIS LINE              
         AR    R0,R1                                                            
         AH    R0,=H'4'            ADJUST FOR WEIGHT                            
         CR    R0,R6                                                            
         BNH   *+6                                                              
         DC    H'0'                TOO MANY WEIGHTS                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK+1      WORK+1 HAS DESC                              
         AR    R5,R1                                                            
         MVI   1(R5),C'='                                                       
         LA    R5,2(R5)                                                         
         BAS   RE,EDITB1                                                        
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
*                                                                               
FMT300   LA    R7,11(R7)           NEXT DEMO                                    
         LA    R4,1(R4)            NEXT WEIGHT                                  
         LA    R2,3(R2)            NEXT DEMO IN EDEMLST                         
         BCT   R3,FMT180                                                        
*                                                                               
FMT310   OC    ETRGLST,ETRGLST     CHK FOR TARGETS                              
         BZ    FMT360                                                           
         LA    R1,ETRGLST                                                       
         LA    R4,ONETWO           TARGET NUMBER LIST                           
*                                                                               
FMT320   OC    0(3,R1),0(R1)                                                    
         BZ    FMT350              GO DO NEXT TARGET                            
         LA    R7,REC2                                                          
         LA    R2,EDEMLST                                                       
         LA    R3,DMAX                                                          
*                                                                               
FMT330   CLC   0(3,R1),0(R2)                                                    
         BE    FMT340                                                           
         LA    R7,11(R7)                                                        
         LA    R2,3(R2)                                                         
         BCT   R3,FMT330                                                        
         DC    H'0'                TARGET NOT IN EDEMLIST                       
*                                                                               
FMT340   BAS   RE,FMTDEMO                                                       
         CLI   1(R2),X'21'         SEE IF USER DEMO                             
         BNE   *+8                                                              
         MVI   WORK,2              SO ONLY UN=NN WILL APPEAR                    
*                                  ON WEIGHTS LINE                              
         ZIC   R7,WORK             WORK HAS LENGTH                              
         LR    R0,R5               SEE IF IT WILL FIT ON THIS LINE              
         AR    R0,R7                                                            
         AH    R0,=H'4'            ADJUST FOR TARGET                            
         CR    R0,R6                                                            
         BNH   *+6                                                              
         DC    H'0'                TOO MANY WEIGHTS/TARGETS                     
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK+1      WORK+1 HAS DESC                              
         AR    R5,R7                                                            
         MVI   1(R5),C'='                                                       
         MVI   2(R5),C'T'                                                       
         LA    R5,3(R5)                                                         
         BAS   RE,EDITB1                                                        
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
*                                                                               
FMT350   LA    R1,3(R1)            NEXT TARGET                                  
         LA    R4,1(R4)            NEXT TARGET NUMBER                           
         CLI   0(R4),X'FF'         END OF LIST                                  
         BNE   FMT320                                                           
*                                                                               
FMT360   BCTR  R5,0                                                             
         CLI   0(R5),C','                                                       
         BNE   *+8                                                              
         MVI   0(R5),C' '          BLANK LAST COMMA                             
         EJECT                                                                  
*                                                                               
         FOUT  ESTWTSH                                                          
         FOUT  ESTCOPYH,ECOPY,1                                                 
*                                                                               
         XC    ESTREP,ESTREP                                                    
         OC    EREP,EREP                                                        
         BZ    FMT370                                                           
         MVC   HALF,EREP                                                        
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTREP,DUB                                                       
*                                                                               
FMT370   FOUT  ESTREPH                                                          
*                                                                               
         OC    EBOOK,EBOOK                                                      
         BNZ   FMT380                                                           
         FOUT  ESTRBKH,=C'LATEST',6                                             
         B     FMT410                                                           
*                                                                               
FMT380   CLI   SVEBCMED,C'N'       NETWORK FORMAT                               
         BNE   FMT390                                                           
         XC    ESTRBK,ESTRBK                                                    
         ZIC   R0,EBOOK+1                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTRBK(2),DUB                                                    
         MVI   ESTRBK+2,C'/'                                                    
         ZIC   R0,EBOOK                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTRBK+3(2),DUB                                                  
         B     FMT400                                                           
*                                                                               
FMT390   GOTO1 VDATCON,DMCB,(3,EBOOK),(6,ESTRBK)                                
*                                                                               
FMT400   FOUT  ESTRBKH                                                          
*                                                                               
FMT410   CLI   EHUTADJ,0                                                        
         BNE   FMT420                                                           
         FOUT  ESTHUTH,=C'AUTO',4                                               
         B     FMT430                                                           
*                                                                               
FMT420   SR    R4,R4                                                            
         IC    R4,EHUTADJ                                                       
         XC    ESTHUT,ESTHUT                                                    
         SRL   R4,4                                                             
         STC   R4,WORK+1                                                        
         MVI   WORK,77                                                          
         MVI   WORK+2,1                                                         
         GOTO1 VDATCON,DMCB,(3,WORK),(6,WORK+10)                                
         MVC   ESTHUT(3),WORK+10                                                
         FOUT  ESTHUTH                                                          
         EJECT                                                                  
*                                                                               
FMT430   FOUT  ESTMENUH,EDAYMENU,1                                              
         FOUT  ESTFLTRH,EPROF,3                                                 
*                                                                               
         XC    ESTCPPE,ESTCPPE                                                  
         LA    R6,ESTCPPE                                                       
         OC    ECPPCLT,ECPPCLT                                                  
         BZ    FMT440                   NO CLT                                  
         MVC   DMCB+4(4),=X'D9000A15'                                           
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),ECPPCLT,0(R6)                                          
         LA    R6,2(R6)                                                         
         CLI   0(R6),C' '                                                       
         BE    *+8                                                              
         LA    R6,1(R6)                                                         
         MVI   0(R6),C'/'                                                       
         LA    R6,1(R6)                                                         
*                                                                               
FMT440   CLI   ECPPEST,0                                                        
         BE    FMT450                                                           
         ZIC   R0,ECPPEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R6),DUB                                                      
*                                                                               
FMT450   FOUT  ESTCPPEH                                                         
*                                                                               
         XC    ESTTYPE,ESTTYPE                                                  
         CLI   ETYPE,0                                                          
         BE    FMT480                                                           
         MVC   ESTTYPE(3),=C'CUT'                                               
         CLI   ETYPE,C'C'                                                       
         BE    FMT480                                                           
         XC    ESTTYPE,ESTTYPE                                                  
         LA    R4,FTYPTAB                                                       
*                                                                               
FMT460   CLC   ETYPE,2(R4)                                                      
         BE    FMT470                                                           
         LA    R4,3(R4)                                                         
         CLI   0(R4),X'FF'         END OF TABLE                                 
         BNE   FMT460                                                           
         B     FMT480                                                           
*                                                                               
FMT470   MVC   ESTTYPE(2),0(R4)                                                 
*                                                                               
FMT480   FOUT  ESTTYPEH                                                         
         EJECT                                                                  
*                                                                               
         XC    ESTRNGE,ESTRNGE                                                  
         OC    EREQLO(2),EREQLO                                                 
         BZ    FMT490                                                           
         MVC   ESTRNGE(2),=C'NO'                                                
         CLC   EREQLO(2),=C'NO'                                                 
         BE    FMT490                                                           
         ZIC   R0,EREQLO                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTRNGE(3),DUB                                                   
         MVI   ESTRNGE+3,C'-'                                                   
         ZIC   R0,EREQHI                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTRNGE+4(3),DUB                                                 
*                                                                               
FMT490   FOUT  ESTRNGEH                                                         
*                                                                               
         XC    ESTRTYP,ESTRTYP                                                  
         MVC   ESTRTYP(1),ERATE                                                 
         MVC   ESTRTYP+1(1),ERATECST                                            
         FOUT  ESTRTYPH                                                         
*                                                                               
         MVC   ESTOWD(4),=C'NO  '                                               
         CLI   EOWSDAY,0                                                        
         BE    *+10                                                             
         MVC   ESTOWD(4),=C'YES '                                               
         FOUT  ESTOWDH                                                          
*                                **CONTROL FIELD - SEE NOTE TOP OF PR           
         XC    ESTECON,ESTECON                                                  
         TM    ECONTROL,EBILESTQ                                                
         BNO   *+8                                                              
         MVI   ESTECON,C'E'                                                     
         TM    ECONTROL,ENSEPCMQ                                                
         BNO   *+10                                                             
         MVC   ESTECON(3),=C'NSC'                                               
         FOUT  ESTECONH                                                         
*                                                                               
         MVC   ESTERTL,ERTLSCHM                                                 
         FOUT  ESTERTLH                                                         
*                                                                               
         XC    ESTUSR1,ESTUSR1                                                  
         OC    SVE1USER,SVE1USER                                                
         BZ    *+10                                                             
         MVC   ESTUSR1,EUSER1                                                   
         OI    ESTUSR1H+6,X'80'                                                 
*                                                                               
         XC    ESTUSR2,ESTUSR2                                                  
         OC    SVE2USER,SVE2USER                                                
         BZ    *+10                                                             
         MVC   ESTUSR2(16),EUSER2                                               
         OI    ESTUSR2H+6,X'80'                                                 
*                                                                               
         XC    SCRNFLAG,SCRNFLAG   INITIALIZE THE FLAG                          
         LA    R1,ESTOPT                                                        
         MVC   0(L'ESTOPT,R1),SPACES                                            
         CLI   SVCLDLY,C'Y'        SEE IF CLT DEFAULT SET                       
         BE    FMT500                                                           
         CLI   EDAILY,C' '                                                      
         BNH   FMT510                                                           
*                                                                               
FMT500   EQU   *                                                                
*                                                                               
         LA    R0,8                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT510              NO - SO CONTINUE                             
*                                                                               
         CLI   EDAILY,C' '         MAKE SURE TO DISPLAY SOMETHING               
         BH    *+8                                                              
         MVI   EDAILY,C'N'                                                      
         MVC   0(6,R1),=C'DAILY='                                               
         MVC   6(1,R1),EDAILY                                                   
         MVI   7(R1),C','                                                       
         LA    R1,8(R1)                                                         
*                                                                               
FMT510   TM    EFLAG1,EF1REQ       IS THIS REQUESTABLE                          
         BNO   FMT511                                                           
*                                                                               
         LA    R0,6                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT511              NO - SO CONTINUE                             
*                                                                               
         MVC   0(6,R1),=C'REQ=Y,'                                               
         LA    R1,6(R1)                                                         
*                                                                               
FMT511   DS    0H                                                               
         TM    EFLAG1,EF1NMG       NEW MAKEGOODS                                
         BNO   FMT512                                                           
*                                                                               
         LA    R0,6                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT512              NO - SO CONTINUE                             
*                                                                               
         MVC   0(6,R1),=C'NMG=Y,'                                               
         LA    R1,6(R1)                                                         
*                                                                               
FMT512   TM    EFLAG1,EF1NODEM     NO DEMOS REQUIRED FOR BUY                    
         BNO   FMT513                                                           
*                                                                               
         LA    R0,8                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT513              NO - SO CONTINUE                             
*                                                                               
         MVC   0(8,R1),=C'DEMOS=N,'                                             
         LA    R1,8(R1)                                                         
*                                                                               
FMT513   DS    0H                                                               
         OC    ECGTPCT,ECGTPCT     CLIENT GROSS TRADE                           
         BZ    FMT514                                                           
*                                                                               
         LA    R0,10               LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT514              NO - SO CONTINUE                             
*                                                                               
         MVC   0(4,R1),=C'CGT='                                                 
         LA    R1,4(R1)                                                         
         LR    R3,R1                                                            
         EDIT  ECGTPCT,(5,(R3)),2,ALIGN=LEFT                                    
         LR    R1,R3                                                            
         AR    R1,R0                                                            
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
FMT514   TM    EFLAG1,EF1OOWPW                                                  
         BZ    FMT515                                                           
*                                                                               
         LA    R0,5                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT515              NO - SO CONTINUE                             
*                                                                               
         MVC   0(4,R1),=C'OWPW'                                                 
         MVI   4(R1),C','                                                       
         LA    R1,5(R1)                                                         
*                                                                               
FMT515   DS    0H                                                               
*                                                                               
         OC    ECOST2,ECOST2       ANY COST FACTOR?                             
         BZ    FMT516                                                           
*                                                                               
         LA    R0,13               MAX LENGTH OF OUTPUT DATA                    
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT516              NO - SO CONTINUE                             
*                                                                               
         MVC   0(5,R1),=C'COS2='                                                
         LA    R4,5(R1)                                                         
         CLI   ECOST2,X'80'        ZERO AS INPUT DATA?                          
         BNE   FMT515A             NO - SO CONTINUE                             
*                                                                               
         MVC   0(3,R4),=C'0.0'     ELSE - MOVE OUT ZERO                         
         LA    R0,3                INC A(LINE POSITION)                         
         B     FMT515B             AND CONTINUE                                 
*                                                                               
FMT515A  EQU   *                                                                
*                                                                               
         EDIT  ECOST2,(8,0(R4)),6,ALIGN=LEFT,FILL=0,DROP=5                      
*                                                                               
FMT515B  EQU   *                                                                
*                                                                               
         AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
         LA    R1,1(R4)                                                         
*                                                                               
FMT516   CLI   ESLN,0              RESTRICTED SPOT LEN?                         
         BE    FMT517                                                           
*                                                                               
         LA    R0,7                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT517              NO - SO CONTINUE                             
*                                                                               
         MVC   0(4,R1),=C'SLN='                                                 
         LA    R1,4(R1)                                                         
         LR    R3,R1                                                            
         EDIT  ESLN,(3,(R3)),0,ALIGN=LEFT                                       
         AR    R3,R0                                                            
         MVI   0(R3),C','                                                       
         LA    R1,1(R3)                                                         
*                                                                               
FMT517   CLI   ECASHPRD,0          CASH PRD?                                    
         BE    FMT518                                                           
*                                                                               
         LA    R0,8                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT518              NO - SO CONTINUE                             
*                                                                               
         MVC   0(5,R1),=C'CASH='                                                
         LA    R1,5(R1)                                                         
         MVC   KEY+14,SVCLTDA      GET CLT REC                                  
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 GETREC                                                           
         LA    R7,CLIST-CLTHDR(R7) FIND CASH PRD                                
FMT517A  CLI   0(R7),0                                                          
         BE    FMT518                                                           
         CLC   3(1,R7),ECASHPRD    MATCH ON CASH PRD NUMBER                     
         BE    FMT517B                                                          
         LA    R7,4(R7)                                                         
         B     FMT517A                                                          
FMT517B  MVC   0(3,R1),0(R7)                                                    
         LA    R1,3(R1)                                                         
*                                                                               
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
FMT518   CLI   ETRDPRD,0           TRADE PRODUCT?                               
         BE    FMT519                                                           
*                                                                               
         LA    R0,7                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT519              NO - SO CONTINUE                             
*                                                                               
         MVC   0(4,R1),=C'TRD='                                                 
         LA    R1,4(R1)                                                         
         MVC   KEY+14,SVCLTDA      GET CLT REC                                  
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 GETREC                                                           
         LA    R7,CLIST-CLTHDR(R7) FIND CASH PRD                                
FMT518A  CLI   0(R7),0                                                          
         BE    FMT519                                                           
         CLC   3(1,R7),ETRDPRD     MATCH ON TRADE PRD NUMBER                    
         BE    FMT518B                                                          
         LA    R7,4(R7)                                                         
         B     FMT518A                                                          
FMT518B  MVC   0(3,R1),0(R7)                                                    
         LA    R1,3(R1)                                                         
*                                                                               
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
*                                                                               
FMT519   BCTR  R1,0                                                             
         CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
*                                                                               
         OC    EPWPCT,EPWPCT       NO PW % TO PRINT                             
         BZ    FMT522C                                                          
*                                                                               
         LA    R0,11               MAX LENGTH OF OUTPUT DATA                    
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   FMT522C             NO - SO CONTINUE                             
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,14,EPWPCT                                                     
         SRA   R3,8                                                             
         LA    R2,ESTOPT              R2-->1ST BYTE                             
         LA    R4,(L'ESTOPT-1)(R2)    R4-->LAST BYTE                            
FMT520A  CR    R2,R4               FIND LAST NON-BLANK                          
         BNL   FMT522A                                                          
         CLI   0(R4),C' '                                                       
         BNE   FMT522                                                           
         BCTR  R4,0                                                             
         B     FMT520A                                                          
*                                                                               
FMT522   MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
FMT522A  MVC   0(3,R4),=C'PW='                                                  
         CLC   EPWPCT,=X'800000'      REALLY 0                                  
         BNE   FMT522B                                                          
         MVI   3(R4),C'0'                                                       
         B     FMT522C                                                          
*                                                                               
FMT522B  EDIT  (R3),(7,3(R4)),2,ALIGN=LEFT,TRAIL=0,FLOAT=-                      
*                                                                               
FMT522C  EQU   *                                                                
*                                                                               
         CLI   SCRNFLAG,0          DID EVERYTHING FIT?                          
         BE    FMT600              YES - SO CONTINUE                            
*                                                                               
         MVC   0(2,R1),=C',*'      ELSE - MOVE OUT 'DIDN'T FIT' FLAG            
*                                                                               
FMT600   EQU   *                                                                
*                                                                               
         FOUT  ESTOPTH                                                          
*                                                                               
FMTX     B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* THIS ROUTINE CHECKS TO MAKE SURE THE CURRENT OPTION WILL FIT IN THE           
* OPTIONS FIELD FOR DISPLAY.                                                    
*                                                                               
* I/P:   R0 = L(OF CURRENT OPTION LITERAL)                                      
*        R1 = A(CURRENT POSITION IN OPTIONS FIELD)                              
*                                                                               
* NOTE: THIS ROUTINE DOESN'T SAVE/RESTORE REGISTERS -- BE CAREFUL!!             
*                                                                               
CHECKOPT EQU   *                                                                
*                                                                               
         AR    R0,R1               A(NEW FIELD POSITION)                        
         LA    R3,ESTOPT           A(SCREEN FIELD)                              
         LA    R3,L'ESTOPT-1(R3)   A(LAST POSTITION IN FIELD)                   
         CR    R0,R3               WILL CURRENT OPTION FIT IN FIELD?            
         BH    CKOPERR             NO - SO ERROR                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
CKOPEXIT EQU   *                                                                
*                                                                               
         BR    RE                  RETURN TO CALLER                             
*                                                                               
CKOPERR  EQU   *                                                                
*                                                                               
         ZIC   R3,SCRNFLAG         SAVE OLD COUNT OF MISSED OPTS                
         LA    R3,1(R3)            INC COUNT                                    
         STC   R3,SCRNFLAG         STORE IT                                     
         LTR   RC,RC               SET ERROR CC                                 
         B     CKOPEXIT            AND RETURN                                   
         EJECT                                                                  
FMTDEMO  NTR1                      ROUTINE TO FORMAT DEMOS                      
*                                  R7 POINTS TO 10 CHAR DESC                    
*                                  R2 POINTS TO 3 BYTE DEMO                     
*                                  WORK(1) RETURNS LENGTH                       
*                                  WORK+1 RETURNS DESC                          
         MVC   WORK(11),SPACES                                                  
         MVI   WORK,0                                                           
         CLI   0(R7),C' '                                                       
         BNH   FMTDEMOX                                                         
         LA    R1,11                                                            
         LA    R4,10(R7)          SCAN BACKWARDS FOR NON-SPACE                  
*                                                                               
FMTD5    CLI   0(R4),C' '                                                       
         BH    FMTD10                                                           
         BCTR  R4,0                                                             
         BCT   R1,FMTD5                                                         
*                                                                               
FMTD10   STC   R1,WORK                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),0(R7)                                                  
         CLI   1(R2),X'21'         SEE IF DOING A USER DEMO                     
         BNE   FMTD20                                                           
*                                                                               
FMTD15   MVC   WORK+11(7),WORK+1                                                
         MVC   WORK+1(3),=C'U /'                                                
*                                                                               
FMTD14   MVC   WORK+4(7),WORK+11                                                
         ZIC   R0,2(R2)            USER NAME NUMBER                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+2(1),DUB+7(1)                                               
         IC    R1,WORK                                                          
         AH    R1,=H'3'                                                         
         STC   R1,WORK                                                          
         B     FMTDEMOX                                                         
*                                                                               
FMTD20   CLC   WORK+1(7),EWGTNM    SEE IF IT MATCHES WEIGHTED DEMO              
         BNE   FMTDEMOX                                                         
         MVC   WORK+10(7),WORK+1                                                
         MVC   WORK+1(2),=C'W/'                                                 
         MVC   WORK+3(7),WORK+10                                                
         IC    R1,WORK                                                          
         AH    R1,=H'2'                                                         
         STC   R1,WORK                                                          
*                                                                               
FMTDEMOX XIT1                                                                   
         EJECT                                                                  
*        ROUTINE TO EDIT 1 BYTE BINARY FIELD AT 0(R1) AND                       
*        DISPLAY IT AT 0(R5) AND BUMP R5 TO NEXT AVAILABLE POSITION             
*                                                                               
EDITB1   DS    0H                                                               
         EDIT  (B1,0(R4)),(3,WORK2),0,ALIGN=LEFT                                
         LR    RF,R0                                                            
         BCTR  RF,0                                                             
         EX    RF,MOVEIT                                                        
         AR    R5,R0                                                            
         BR    RE                                                               
         SPACE                                                                  
*                                                                               
MOVEIT   MVC   0(0,R5),WORK2        EXECUTED                                    
         SPACE                                                                  
*                                                                               
FNDUF1   TM    1(R2),X'20'         FIND UNPROTECTED FIELD                       
         BCR   8,RE                                                             
         SPACE                                                                  
*                                                                               
FNDNXUF1 SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FNDUF1                                                           
         DC    H'0'                END OF SCREEN                                
         EJECT                                                                  
*                                                                               
FTYPTAB  DS    0C                                                               
         DC    C'M$',X'03'                                                      
         DC    C'M%',X'04'                                                      
         DC    C'Q$',X'05'                                                      
         DC    X'FF'                                                            
*                                                                               
ONETWO   DC    X'0102FF'           TARGET NUMBER LIST                           
*                                                                               
         LTORG                                                                  
*                                                                               
EDT      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R9,4095(RB)                                                      
         LA    R9,1(R9)                                                         
         USING EDT+4096,R9           NOTE USE OF SECOND BASE REG                
*                                                                               
         MVI   COS2FLAG,0                                                       
         CLI   SVACT,C'A'                                                       
         BNE   EDT140                                                           
         CLI   ESTOPTH+5,0         WAS ANYTHING INPUT                           
         BNE   EDT150                                                           
         MVC   ESTOPT,SPACES                                                    
         CLI   SVCLDLY,C'Y'        SEE IF CLT DEFAULT SET                       
         BNE   EDT130                                                           
         MVI   ESTOPTH+5,7                                                      
         MVC   ESTOPT(7),=C'DAILY=Y'                                            
*                                                                               
EDT130   FOUT  ESTOPTH                                                          
         B     EDT150                                                           
*                                                                               
EDT140   MVC   KEY,SVKEY           REREAD REC ON CHA                            
         GOTO1 GETREC                                                           
         MVC   SVDEMOS,EDEMOS      SAVE OLD DATA                                
         MVC   SVUSRNMS,EUSRNMS    OLD USER NAMES                               
         MVC   SVBOOK,EBOOK                                                     
         MVC   SVHUT,EHUTADJ                                                    
         MVC   SVDPT,EDAYMENU                                                   
         MVC   SVCOS2,ECOST2       SAVE THE COST2 VALUE                         
         CLI   SVF0PROF,C'N'       POL=BRAND EST FILTERS                        
         BE    *+10                                                             
         MVC   SVFLTRS,EPROF                                                    
         MVC   SVECON,ECONTROL                                                  
         CLI   SVF0PROF+1,C'N'     POL=BRAND RTL SCHEME                         
         BE    *+10                                                             
         MVC   SVRTL,ERTLSCHM                                                   
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   EDT150                                                           
         CLI   SVEBCMED,C'N'       FOR NETWK OR COMBINED ONLY                   
         BE    EDT10               ONLY CHG AUTH $'S                            
         CLI   SVEBCMED,C'C'                                                    
         BE    EDT10                                                            
         EJECT                                                                  
*                                                                               
*            FIRST CHECK STATUS CHANGE                                          
EDT150   GOTO1 =A(CHKSTAT),RR=RELO                                              
         LA    R2,ESTSTATH                                                      
*        BNE   LFMERR1                                                          
         BE    EDT200                                                           
         GOTO1 ERROR                                                            
*                                                                               
EDT200   CLI   WORK,C'S'           CHK FOR STATUS CHG                           
         BE    EDTX                                                             
*                                                                               
EDT250   DS    0H                                                               
         LA    R2,ESTDESCH                                                      
         GOTO1 ANY                                                              
         MVI   WORK2,C' '                                                       
         MVI   SONIA,C' '                                                       
         CLC   =C'JILLREBER',ESTDESC                                            
         BE    EDT253                                                           
         CLC   =C'SONIAPRICE',ESTDESC                                           
         BE    EDT253                                                           
         CLC   =C'KENSUPEK',ESTDESC                                             
         BNE   EDT255                                                           
EDT253   MVI   WORK2,C'D'          ALLOW DATE CHANGES W/O VALIDATING            
         MVI   SONIA,C'C'          ALLOW CHANGE OF CASH PRD                     
         B     EDT257              AND DON'T CHANGE DESCRIPTION                 
*                                                                               
EDT255   MVC   EDESC,ESTDESC                                                    
         OC    EDESC,SPACES                                                     
*                                                                               
EDT257   LA    R2,ESTSTRDH     START DATE                                       
         GOTO1 ANY                                                              
         GOTO1 VDATVAL,DMCB,(0,ESTSTRD),SYR                                     
         OC    DMCB(4),DMCB                                                     
         BZ    DTERR                                                            
*                                                                               
         CLI   SVACT,C'A'      TEST ADD                                         
         BE    EDT280                                                           
         CLI   WORK2,C'D'          ALLOW DATE CHANGES W/O VALIDATING            
         BE    EDT280                                                           
*                                                                               
         CLI   EMSTRIND,0                                                       
         BE    EDT260                                                           
         CLC   ESTART,SYR          CAN'T CHANGE DATES                           
         BE    EDT280                                                           
         B     ERRNOCHG                                                         
*                                                                               
*******************************    NOBODY CAN CHANGE CPP EST                    
EDT260   OC    ECPPEST,ECPPEST                                                  
         BNZ   EDT270                                                           
         GOTO1 =A(CKLINID),RR=RELO TEST TERMINAL AUTHORIZED                     
         BE    EDT280               ALLOW CHG OF OLD-NEW                        
*                                                                               
EDT270   CLC   ESTART,SYR          START CAN'T BE ADVANCED                      
         BL    ERRESTD                                                          
*                                                                               
EDT280   LA    R2,ESTENDDH                                                      
         GOTO1 ANY                                                              
         GOTO1 VDATVAL,DMCB,(0,ESTENDD),EYR                                     
         OC    DMCB(4),DMCB                                                     
         BZ    DTERR                                                            
         CLI   SVACT,C'A'               ADD                                     
         BE    EDT310                                                           
         CLI   WORK2,C'D'          ALLOW DATE CHANGES W/O VALIDATING            
         BE    EDT310               TOO MUCH                                    
         CLI   EMSTRIND,0                                                       
         BE    EDT290                                                           
         CLC   EEND,EYR            CAN'T CHANGE DATES                           
         BE    EDT620                                                           
         B     ERRNOCHG                                                         
         EJECT                                                                  
*                                                                               
*******************************    NOBODY CAN CHANGE CPP EST                    
EDT290   OC    ECPPEST,ECPPEST                                                  
         BNZ   EDT300                                                           
         GOTO1 =A(CKLINID),RR=RELO TEST TERMINAL AUTHORIZED                     
         BE    EDT310               ALLOW CHG OF OLD-NEW                        
*                                                                               
EDT300   CLC   EEND,EYR                 END CAN'T BE CUT BACK                   
         BH    ERRESTD                                                          
*                                                                               
EDT310   MVI   ERRCD,EBSERR                                                     
         CLC   SYR(6),EYR                                                       
         BH    LFMERR                   END BEFORE START                        
         OC    SVAEDATS,SVAEDATS      CHECK ADV EST DATES                       
         BZ    EDT350                                                           
         CLC   SYR(12),SVAEDATS                                                 
         BE    EDT390                                                           
         MVI   ERRCD,ADVEDERR      DON'T MATCH                                  
         LA    R2,ESTSTRDH         CURSOR TO START                              
         B     LFMERR                                                           
         EJECT                                                                  
EDT350   CLI   OVSYS,3             TEST NETPAK                                  
         BNE   EDT380                                                           
*                                                                               
         LA    R2,LFMKEYH                                                       
         CLI   SVEBCMED,C'N'       TEST MEDIA N                                 
         BNE   ERRINV                                                           
*                                                                               
         CLI   SVACT,C'A'                                                       
         BNE   EDT360                                                           
         LA    R2,ESTSTATH         ON NETPAK ADDS REQUIRE OLD OR NEW            
         CLI   5(R2),0                                                          
         BE    ERRMSSNG                                                         
*                                                                               
EDT360   TM    EPRDCD,X'80'                                                     
         BZ    EDT370                                                           
         MVC   LFMKEXP+50(2),=C' N'                                             
*                                                                               
EDT370   FOUT  LFMKEXPH                                                         
         B     EDT390                                                           
*                                                                               
EDT380   LA    R2,ESTSTATH        CURSOR TO STATUS                              
         CLC   ESTSTAT(3),=C'OLD'                                               
         BE    ERRINV                                                           
         CLC   ESTSTAT(3),=C'NEW'                                               
         BE    ERRINV              OLD - NEW INVALID FOR NON-NETPAK             
*                                                                               
EDT390   CLI   SVACT,C'A'          ALWAYS CHECK ON AN ADD                       
         BE    EDT400                                                           
         CLC   ESTART,SYR          IF CHANGE IN DATES                           
         BNE   EDT400                                                           
         CLC   EEND,EYR                                                         
         BE    EDT450                                                           
         EJECT                                                                  
*                                                                               
EDT400   GOTO1 =A(CHKEDTS),RR=RELO                                              
         BE    EDT430                                                           
*                                                                               
EDT420   MVI   ERRCD,SPDERR        INVALID DATE SPREAD                          
         LA    R2,ESTSTRDH         CURSOR TO START DATE                         
         B     LFMERR                                                           
*                                                                               
EDT430   DS    0H                  IF PW EST DATES CANNOT BE >14 WKS            
         OC    EPWPCT,EPWPCT       TEST PW PCT PRESENT                          
         BZ    EDT450              NO - SKIP                                    
         GOTO1 VADDAY,DMCB,SYR,WORK,98      CAN'T HAVE PW ON EST>14WKS          
         CLC   WORK(6),EYR                                                      
         BH    EDT450              OK                                           
         LA    R2,ESTENDDH                                                      
         MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(INVPW) CAN'T HAVE PW ON EST>14WKS                    
         B     LFMERR                                                           
*                                                                               
EDT450   GOTO1 VDATCON,DMCB,EYR,(2,EMGDTE)                                      
         EJECT                                                                  
         MVI   DMCB,0                                                           
         LA    R2,ESTOWDH                                                       
         CLI   5(R2),0            ESTIMATE OUT OF WEEK INPUT?                   
         BNE   EDT460               YES - IT OVERRIDES CLIENT                   
         CLI   SVCLEX+10,C'Y'       NO  - CHECK IF CLIENT CONTROLLED            
         BE    EDT480                   YES - GO VALIDATE                       
         B     EDT490                   NO  - INPUT ON BOTH LEVELS              
*                                                                               
EDT460   CLI   8(R2),C'N'                                                       
         BE    EDT490                                                           
*&&DO                                                                           
* NO-ONE KNOWS WHY THIS TEST IS HERE. MHER  17SEP97                             
         CLI   SVEBCMED,C'R'       FOR MEDIA = RADIO                            
         BNE   EDT470                                                           
         CLI   SVACT,C'A'          ACTION = ADD                                 
         BE    ERRINV              DON'T ALLOW OUT-OF-WEEK ROTATOR              
*&&                                                                             
EDT470   CLI   8(R2),C'Y'                                                       
         BNE   ERRINV                                                           
*                                                                               
EDT480   GOTO1 VGETDAY,DMCB,(0,SYR),DUB                                         
         CLC   DUB(3),SPACES                                                    
         BE    ERRINV                                                           
         CLI   0(R1),1                                                          
         BE    ERRINV                                                           
*                                                                               
EDT490   DS    0H                                                               
         MVC   OWSDAY,DMCB                                                      
*                                                                               
         XC    POLDATA(POLDATAX-POLDATA),POLDATA                                
*                             CHECK COMPATABILITY WITH POL/BRAND                
EDT500   CLC   SVEBCPRD,=C'POL'                                                 
         BNE   EDT510                                                           
         CLI   SVACT,C'A'          SEE IF ADD                                   
         BE    EDT550                                                           
*                                  POL CHANGE REREAD OLD REC                    
EDT510   XC    KEY,KEY                                                          
         MVC   KEY(8),SVKEY                                                     
         MVC   KEY+4(3),=C'POL'                                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY                                                   
         BNE   EDT610                                                           
         LA    R8,REC2                                                          
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
         EJECT                                                                  
*                                                                               
*           ESTHDRD COVERS REC2 FOR EDT500 AND EDT580                           
*                                                                               
         CLC   SVEBCPRD,=C'POL'    SEE IF CHANGING POL                          
         BE    EDT520                                                           
         LA    R2,ESTSTATH                                                      
*  CANNOT SUPPORT THIS TEST ANYMORE DUE TO SPOT ESTIMATE                        
*  RECORDS ON THE NETWORK SYSTEM.                                               
*        CLC   EPRDCD(1),REC+EPRDCD-EKEY   NETPAK OLD-NEW                       
*        BNE   CKPOLERR                    MUST MATCH POL                       
         LA    R2,ESTSTRDH                                                      
         CLC   ESTART,SYR                                                       
         BNE   BRPOLERR                                                         
         LA    R2,ESTENDDH                                                      
         CLC   EEND,EYR                                                         
         BNE   BRPOLERR                                                         
         LA    R2,ESTOWDH                                                       
         CLC   EOWSDAY,OWSDAY       MUST MATCH POL                              
         BNE   BRPOLERR                                                         
*                                                                               
EDT520   MVC   POLDEMOS,EDEMOS     SAVE POL DEMOS                               
         MVC   POLBOOK,EBOOK                                                    
         MVC   POLHUT,EHUTADJ                                                   
         MVC   POLDPT,EDAYMENU                                                  
         CLI   SVF0PROF,C'N'       POL=BRAND EST FILTERS                        
         BE    *+10                                                             
         MVC   POLFLTRS,EPROF      FILTERS - REMOVE PER MEL                     
         MVC   POLECON,ECONTROL                                                 
         CLI   SVF0PROF+1,C'N'     POL=BRAND RTL SCHEME                         
         BE    *+10                                                             
         MVC   POLRTL,ERTLSCHM                                                  
         MVI   POLSW,1             SET FOR EXISTENCE OF POL HDR                 
         CLC   SVEBCPRD,=C'POL'    SEE IF CHANGING POL                          
         BNE   EDT610                                                           
         MVI   POLSW,2             POL EXISTED AND CHANGING POL                 
         LA    R8,REC              RESET R8 TO REC                              
* IF NO BRANDS EXIST, ALLOW POL DEMO DELETES                                    
         BAS   RE,FRSTPRD          IF NOT ALLOW POL DEMO DELETES                
         EJECT                                                                  
EDT550   CLC   ESTART,SYR        SEE IF DATES WERE CHANGED                      
         BNE   EDT560                                                           
         CLC   EEND,EYR                                                         
         BNE   EDT560                                                           
         CLC   EOWSDAY,OWSDAY        OR EOWSDAY                                 
         BNE   EDT560                                                           
         B     EDT620                                                           
*                                                                               
EDT560   BAS   RE,FRSTPRD          READ BRAND ESTIMATES                         
         B     *+8                                                              
*                                                                               
EDT570   BAS   RE,NEXTPRD                                                       
         BNE   EDT610                                                           
*                                                                               
         LA    R8,REC2                                                          
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         CLI   SVACT,C'A'        IF POL ADD DATES MUST AGREE                    
         BNE   EDT590                                                           
         LA    R2,ESTSTATH                                                      
*  CANNOT SUPPORT THIS TEST ANYMORE DUE TO SPOT ESTIMATE                        
*  RECORDS ON THE NETWORK SYSTEM.                                               
*        CLC   EPRDCD(1),REC+EPRDCD-EKEY   NETPAK OLD-NEW                       
*        BNE   BRPOLERR                    MUST MATCH POL                       
         LA    R2,ESTSTRDH                                                      
         CLC   ESTART,SYR                                                       
         BNE   BRPOLERR                                                         
         LA    R2,ESTENDDH                                                      
         CLC   EEND,EYR                                                         
         BNE   BRPOLERR                                                         
         LA    R2,ESTOWDH                                                       
         CLC   EOWSDAY,OWSDAY       EOWSDAY MUST MATCH                          
         BNE   BRPOLERR                                                         
         LA    R2,LFMRECH                                                       
         OC    EORDN(208),EORDN                                                 
         BZ    EDT570                                                           
         CLI   ESTDESC,C'@'        TO GET AROUND THIS ERROR                     
         BE    EDT570                                                           
         MVI   ERRCD,EDOLERR       ORDERED OR PAID DOLLARS                      
*                                  ON AN BRAND EST                              
         B     LFMERR                                                           
         EJECT                                                                  
*                                                                               
EDT590   CLC   ESTART,SYR         ON POL CHA - CHANGE BRAND DATES               
         BNE   EDT600                                                           
         CLC   EEND,EYR                                                         
         BNE   EDT600             NO CHANGE - GO CHECK NEXT BRAND               
         CLC   EOWSDAY,OWSDAY                                                   
         BE    EDT570             NO CHANGE - GO CHECK NEXT BRAND               
*                                                                               
EDT600   MVC   ESTART,SYR                                                       
         MVC   EEND,EYR                                                         
         MVC   EMGDTE,MAKEGDDT        STORE NEW MAKE GOOD DATE                  
         MVC   EOWSDAY,OWSDAY      SAVE EOWSDAY                                 
*                                     IN BRAND ESTIMATES                        
         GOTO1 PUTREC                                                           
         BAS   RE,DOCANADA                                                      
         B     EDT570        UPDATED   BRAND  HDR WRITTEN BACK                  
*                                                                               
*                                                                               
EDT610   MVI   WORK2,C' '     CHECK CHECK OF ALLOWING CHANGES W/O VAL           
         LA    R8,REC                   RESET ESTDHRD TO COVER REC              
         MVC   ESTART,SYR     DATES AND EOWSDAY  - OK PUT INTO REC              
         MVC   EEND,EYR                                                         
         MVC   EOWSDAY,OWSDAY                                                   
         CLC   SVKEY+4(3),=C'POL'                                               
         BNE   EDT620                                                           
         CLI   SVACT,C'C'                                                       
         BE    OUTPUT        ON POL DATE CHANGES OMIT OTHER EDITS               
         EJECT                                                                  
*                                                                               
EDT620   LA    R2,ESTBBASH                                                      
         XC    EBILLBAS(5),EBILLBAS                                             
         CLI   5(R2),0                                                          
         BE    EDT650              WAS TO EDT2A FOR CLT PROFILE CHKS            
*                                                                               
EDT630   XC    WORK,WORK                                                        
                                                                                
         MVC   WORK+16(4),=C'SB1X'   'S' MUST BE LOWER CASE - 3 CHAR            
         MVI   WORK+16,X'A2'         MAKE THE S LOWER CASE !                    
         MVC   WORK+20(2),AGYALPHA   PROFILE NAME                               
         MVC   WORK+22(1),SVEBCMED                                              
         MVC   WORK+23(3),SVEBCCLT                                              
         GOTO1 VGETPROF,DMCB,WORK+16,WORK,VDATAMGR                              
         CLI   WORK+11,C' '        DON'T ALLOW BILL FORMULA IF OPT 12           
         BNH   EDT635              IS SET                                       
         CLI   WORK+11,C'N'                                                     
         BNE   ERRINV                                                           
*                                                                               
EDT635   GOTO1 ANY                      R2 $S AT ESTBBASH                       
         XR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
         LA    R5,8(R2)                                                         
         CLI   8(R2),C'C'          CHK FOR COMMISSION ONLY                      
         BNE   EDT640                                                           
         OI    EBILLBAS,X'40'                                                   
         BCTR  R4,0                                                             
         CLI   5(R2),1             DON'T ACCEPT 'C' ALONE                       
         BE    ERRINV                                                           
         LA    R5,1(R5)            BUMP PAST 'C'                                
*                                                                               
EDT640   EX    R4,GROSCOM                                                       
         BE    EDT650                                                           
         EX    R4,NETCOM                                                        
         BNE   ERRINV                                                           
         OI    EBILLBAS,X'10'           CHECK PROFILE                           
*                                                                               
EDT650   LA    R2,ESTCPCTH                                                      
         CLI   5(R2),0                  NOT REQUIRED                            
         BE    EDT670                                                           
         XR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         BCTR  R0,R0                                                            
         GOTO1 VCASHVAL,DMCB,(4,ESTCPCT+1),(R0)                                 
         CLI   DMCB,X'FF'                                                       
         BE    ERRINV                                                           
         L     R0,DMCB+4                                                        
         C     R0,=F'1000000'                                                   
         BH    ERRINV                                                           
         C     R0,=F'0'                                                         
         BNH   ERRINV                                                           
         CLI   ESTCPCT,C'+'                                                     
         BE    EDT660                                                           
         CLI   ESTCPCT,C'-'                                                     
         BNE   ERRINV                   ERROR                                   
         LCR   R0,R0                    MAKE NEGATIVE                           
*                                                                               
EDT660   ST    R0,FULL                                                          
         MVC   EBILLCOM,FULL                                                    
         B     EDT680                                                           
*                                                                               
EDT670   CLI   ESTCBASH+5,0             REQUIRED IF COM BASIS PRESENT           
         BNE   ERRMSSNG                                                         
         EJECT                                                                  
*                                                                               
EDT680   LA    R2,ESTCBASH                                                      
         CLI   5(R2),0                  NOT REQUIRED                            
         BE    EDT700                                                           
         XR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
         LA    R5,8(R2)                                                         
         EX    R4,GROSCOM                                                       
         BE    EDT690                                                           
         EX    R4,NETCOM                                                        
         BNE   ERRINV                                                           
         OI    EBILLBAS,X'01'                                                   
*                                                                               
EDT690   B     EDT710                                                           
*                                                                               
EDT700   CLI   ESTCPCTH+5,0                                                     
         BNE   ERRMSSNG                                                         
*                                                                               
EDT710   OC    EBILLBAS(5),EBILLBAS                                             
         BNZ   EDT720                  FORMULA INPUT                            
         CLI   ESTBBASH+5,0            NO FORMULA                               
         BE    EDT740                                                           
         B     EDT730                                                           
*                                                                               
EDT720   OC    EBILLCOM,EBILLCOM   CHK FOR COMM PCT                             
         BNZ   EDT740                                                           
         CLI   EBILLBAS,X'40'      COMM ONLY - GROSS ALONE                      
         BNE   EDT740                                                           
*                            FORMULA MUST HAVE BEEN GROSS ALONE                 
EDT730   OI    EBILLBAS,X'80'                                                   
*                            SO BILLING WILL THINK IT'S A FORMULA               
         EJECT                                                                  
*                                                                               
EDT740   LA    R2,ESTDEMSH                                                      
         XC    EDEMOS(124),EDEMOS   SO I WON'T CLEAR EOWSDAY,ERATE              
         MVI   WTSW,0              ZERO WEIGHTED DEMO INPUT SW                  
         MVI   HMSW,0              ZERO TOTAL HOMES SWITCH                      
         MVI   NHMSW,0             0 NON HOMES SWITCH                           
         CLI   5(R2),0                                                          
         BNE   EDT750                                                           
         MVI   ERRCD,TV1DEM        TV MUST HAVE AT LEAST 1 DEMO                 
         CLI   SVEBCMED,C'T'                                                    
         BE    LFMERR                                                           
         CLI   OVSYS,3             SEE IF NETPAK EST                            
         BNE   EDT990              NO                                           
         B     ERRTOTH                                                          
*                                                                               
EDT750   MVI   ERRCD,DEMINV                                                     
         XC    REC2(250),REC2                                                   
         XC    REC2+250(50),REC2+250                                            
*                                  CHK FOR DEMO MENU                            
         CLC   8(5,R2),=C'MENU='                                                
         BNE   EDT780                                                           
*                                                                               
*              INPUT IN SECOND DEMO LINE IS AN ERROR                            
         LA    R2,ESTDEM2H                                                      
         CLI   5(R2),0                                                          
         BNE   ERRINV                                                           
*                                                                               
         LA    R2,ESTDEMSH         RESET R2                                     
         ZIC   R1,5(R2)                                                         
         SH    R1,=H'5'            ADJUST FOR MENU=                             
         BNP   ERRINV                                                           
         CH    R1,=H'4'                                                         
         BH    ERRINV                                                           
         MVC   WORK2(13),KEY                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D26'                                                  
         MVC   KEY+2(1),SVAGYMD                                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),13(R2)                                                  
         OC    KEY+3(4),SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERRINV                                                           
         LA    R0,REC2                                                          
         ST    R0,AREC                                                          
         GOTO1 GETREC                                                           
         LA    R5,REC2+24                                                       
         LA    R4,EDEMLST                                                       
         LA    R6,DMAX                                                          
         MVI   ELCODE,X'05'                                                     
         EJECT                                                                  
*                                                                               
EDT760   BAS   RE,NEXTEL                                                        
         BNE   EDT770                                                           
         MVC   0(3,R4),2(R5)                                                    
         LA    R4,3(R4)                                                         
         BCT   R6,EDT760                                                        
*                                                                               
EDT770   MVC   KEY(13),WORK2           RESTORE KEY                              
         B     EDT800              GO DO OTHER CHKS                             
*                                                                               
EDT780   LA    R5,REC2                                                          
         USING DBLOCKD,R5                                                       
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE,=C'TPT'                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   SVEBCMED,C'R'       FOR MEDIA = RADIO                            
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVAPROF+7,C'C'        SEE IF CANADIAN AGY                        
         BNE   EDT790                                                           
         CLI   SVCLEX,C'U'         CANADIAN - SEE IF USING US DEMOS             
         BE    EDT790              YES                                          
         MVI   DBSELMED,C'C'                                                    
*                                                                               
EDT790   MVC   DMCB+4(4),=X'D9000AD9'    DEMOVAL                                
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(2,ESTDEMSH),(14,REC2+300),(C'S',REC2),EUSRNMS         
         CLI   DMCB+4,0                                                         
         BE    ERRINV                                                           
         ZIC   R4,DMCB+4           NUMBER OF DEMOS                              
         MH    R4,=H'3'                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   EDEMLST(0),REC2+300  SO I WON'T MOVE THE X'FF'                   
         CLI   SVEBCMED,C'R'        FOR MEDIA = RADIO                           
         BNE   EDT800              REPLICATE LAST RATING/IMP TYPE IF            
*                                  CHK FOR DUPLICATES                           
EDT800   LA    R4,DMAX-1                                                        
         LA    R5,EDEMLST                                                       
*                                                                               
EDT810   LA    R6,3(R5)                                                         
         LA    R3,DMAX-1                                                        
*                                                                               
EDT820   CLI   1(R6),0                                                          
         BE    EDT840              END OF DEMOS                                 
         CLC   0(3,R5),0(R6)                                                    
         BE    DUPPERR             DUPLICATE FOUND                              
         LA    R6,3(R6)                                                         
         BCT   R3,EDT820                                                        
*                                                                               
EDT840   LA    R5,3(R5)                                                         
         CLI   1(R5),0             END OF DEMOS                                 
         BE    EDT850                                                           
         BCT   R4,EDT810                                                        
         EJECT                                                                  
*                                                                               
EDT850   DS    0H                                                               
         CLI   POLSW,1                                                          
         BE    EDT857                                                           
         CLI   POLSW,2                                                          
         BNE   EDT940                                                           
         CLC   POLDEMOS(60),EDEMOS   DEMOS CHANGE?                              
         BE    EDT940                NO                                         
*                                                                               
         XC    POLDEMOS,POLDEMOS                                                
         BAS   RE,FRSTPRD          READ BRAND ESTIMATES TO GET DEMOS            
         B     *+8                                                              
*                                                                               
EDT853   BAS   RE,NEXTPRD                                                       
         BNE   EDT855                                                           
*                                                                               
         LA    R8,REC2                                                          
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
* MAKE A LIST OF ALL BRAND DEMOS IN POLDEMOS                                    
         LA    R4,EDEMOS                                                        
         LA    R3,DMAX             FOR FIRST BCT                                
*                                                                               
EDT853B  LA    R5,POLDEMOS                                                      
         LA    R6,DMAX             FOR SECOND BCT                               
*                                                                               
EDT853D  CLC   0(3,R4),0(R5)                                                    
         BE    EDT853F             FOUND                                        
         CLC   0(3,R5),=3X'00'     END OF DEMOS                                 
         BNE   *+14                                                             
         MVC   0(3,R5),0(R4)                                                    
         B     EDT853F                                                          
         LA    R5,3(R5)            NEXT POL DEMO                                
         BCT   R6,EDT853D                                                       
*                                                                               
EDT853F  LA    R4,3(R4)                                                         
         CLC   0(3,R4),=3X'00'     END OF DEMOS                                 
         BE    EDT855                                                           
         BCT   R3,EDT853B                                                       
         B     EDT853                                                           
*                                                                               
EDT855   CLC   POLDEMOS(3),=3X'00' NO BRAND ESTIMATE DEMOS                      
         BE    EDT940                                                           
         LA    R8,REC                                                           
         LA    R4,POLDEMOS                                                      
         LA    R3,DMAX             FOR FIRST BCT                                
*                                                                               
EDT855B  LA    R5,EDEMOS                                                        
         LA    R6,DMAX             FOR SECOND BCT                               
*                                                                               
EDT855D  CLC   0(3,R4),0(R5)                                                    
         BE    EDT855F             FOUND                                        
         LA    R5,3(R5)            NEXT POL DEMO                                
         BCT   R6,EDT855D                                                       
         B     ERRDMPOL                                                         
*                                                                               
EDT855F  LA    R4,3(R4)                                                         
         CLC   0(3,R4),=3X'00'     END OF DEMOS                                 
         BE    EDT940                                                           
         BCT   R3,EDT855B                                                       
         B     EDT940                                                           
*                                                                               
* NOW BE SURE BRAND POL DEMOS ARE A SUBSET OF POL DEMOS                         
EDT857   CLC   POLDEMOS(3),=3X'00' NO POL DEMOS                                 
         BE    ERRDMPOL                                                         
         MVI   UDSW,0              ZERO USER DEMO SW                            
         LA    R4,EDEMOS                                                        
         LA    R3,DMAX             FOR FIRST BCT                                
*                                                                               
EDT860   LA    R5,POLDEMOS                                                      
         LA    R6,DMAX             FOR SECOND BCT                               
         CLI   1(R4),X'21'         SEE IF LOOKING FOR A USER DEMO               
         BNE   *+8                                                              
         MVI   UDSW,1              SET USER DEMO ENCOUNTERED SW                 
*                                                                               
EDT870   CLC   0(3,R4),0(R5)                                                    
         BE    EDT890              FOUND                                        
         LA    R5,3(R5)            NEXT POL DEMO                                
         BCT   R6,EDT870                                                        
         B     ERRDMPOL                                                         
*                                                                               
EDT890   LA    R4,3(R4)                                                         
         CLC   0(3,R4),=3X'00'     END OF DEMOS                                 
         BE    EDT900                                                           
         BCT   R3,EDT860                                                        
*                                                                               
EDT900   CLI   UDSW,0              SEE IF USING USER DEMOS                      
         BE    EDT930              NO                                           
*                                                                               
         LA    R3,4                                                             
         LA    R4,EUSRNMS          BRAND USER NAMES MUST MATCH                  
         LA    R5,PEUSRNMS         POL USER NAMES                               
*                                                                               
EDT910   CLI   0(R4),C' '          SEE IF BRAND USED                            
         BNH   EDT920              NO SKIP                                      
         CLC   0(7,R4),0(R5)                                                    
         BNE   ERRDMPOL            ERROR DON'T MATCH                            
*                                                                               
EDT920   LA    R4,7(R4)                                                         
         LA    R5,7(R5)                                                         
         BCT   R3,EDT910                                                        
*                                                                               
EDT930   CLI   EWGTNM,C' '         CHK WEIGHTED DEMO                            
         BNH   EDT940                                                           
         CLC   EWGTNM,PEWGTNM                                                   
         BNE   ERRDMPOL            WGT DEMO MUST MATCH                          
*                                                                               
EDT940   LA    R8,REC              POINT R8 TO CURRENT ESTHDR                   
         CLI   SVEBCMED,C'N'       NETWORK                                      
         BNE   EDT990                                                           
*                                                                               
         CLI   SVAPROF+7,C'C'      CANADA                                       
         BE    EDT990                                                           
*                                                                               
         CLI   OVSYS,3             SEE IF NETPAK EST                            
         BNE   EDT990              NO                                           
         LA    R4,EDEMLST          SEE IF TOTAL HMS INPUT                       
         LA    R3,DMAX                                                          
*                                                                               
EDT950   CLC   0(3,R4),=3X'00'     END OF LIST                                  
         BE    EDT970                                                           
         CLC   1(2,R4),=X'C901'    TOTAL HMS                                    
         BNE   *+12                                                             
         MVI   HMSW,1                                                           
         B     *+8                                                              
         MVI   NHMSW,1             SET A NON-TOTAL HMS DEMO INPUT               
         LA    R4,3(R4)            TOTAL HMS REQUIRED FOR NETPAK                
         BCT   R3,EDT950                                                        
*                                  TOTAL HMS REQUIRED FOR NETPAK                
EDT970   CLI   HMSW,1                                                           
         BNE   ERRTOTH                                                          
*                                                                               
EDT980   CLI   NHMSW,1             A NOT HOMES DEMO MUST BE INPUT FOR           
         BE    EDT990              NETPAK                                       
         MVI   ERRCD,DEMINV                                                     
         B     LFMERR                                                           
         EJECT                                                                  
*              WEIGHT EDIT                                                      
EDT990   LA    R2,ESTWTSH          DEMO WEIGHTS                                 
         CLI   5(R2),0                                                          
         BE    EDT1130                                                          
         LA    R3,REC2                                                          
         LA    R4,8                                                             
*                                                                               
EDT1000  XC    0(250,R3),0(R3)                                                  
         LA    R3,250(R3)                                                       
         BCT   R4,EDT1000                                                       
*                                  BUILD SCANNER TABLE OF DEMOS                 
         LA    R2,ESTDEMSH                                                      
         GOTO1 VSCANNER,DMCB,(R2),(15,REC2+1000)                                
         CLI   DMCB+4,0                                                         
         BE    ERRINV                                                           
         CLI   DMCB+4,DMAX                                                      
         BH    ERRINV                                                           
         LA    R2,ESTDEM2H                                                      
         CLI   5(R2),0                                                          
         BE    EDT1010                                                          
         ZIC   R4,DMCB+4                                                        
         MH    R4,=H'32'                                                        
         LA    R5,REC2+1000                                                     
         AR    R5,R4                                                            
         ZIC   R3,DMCB+4                                                        
         LA    R4,DMAX+1                                                        
         SR    R4,R3                                                            
         GOTO1 VSCANNER,DMCB,(R2),((R4),(R5))                                   
         CLI   DMCB+4,0                                                         
         BE    ERRINV                                                           
*                                                                               
EDT1010  LA    R2,ESTWTSH                                                       
         L     RF,VCOMFACS         NEED TO USE PARSNIP B/C DEMOS>10 LEN         
         L     RF,CPARSNIP-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(R2),(15,REC2+500),0                                   
         MVI   ERRCD,WGHTINV                                                    
         CLI   DMCB+4,0            PARSNIP ERROR                                
         BE    LFMERR                                                           
         CLI   DMCB+4,DMAX*2                                                    
         BH    LFMERR                                                           
*                                                                               
         BAS   RE,PARTOSCN         REFORM PARSNIP BLK TO BE SCANNERISH          
*                                                                               
         LA    R3,DMAX             FOR BCT                                      
         LA    R5,REC2                                                          
         USING BSCAND,R5           BIG SCANNER DSECT                            
*                                                                               
EDT1020  CLC   0(2,R5),=X'0000'    LAST LINE                                    
         BE    EDT1130                                                          
*            FIND DEMO AND STORE WEIGHT OR TARGET                               
         ST    R3,FULL             SAVE SCANNER BCT                             
         LA    R4,EWGTLST                                                       
         LA    R6,DMAX                                                          
         LA    R7,EDEMLST                                                       
         LA    R3,REC2+1000           LOOK FOR DEMO IN DEMO SCANNER TBL         
         EJECT                                                                  
*                                                                               
EDT1030  CLC   BFLD1,12(R3)                                                     
         BE    EDT1050                                                          
         CLI   BFLD1LEN,2           TO LET THEM INPUT UN=                       
         BNE   EDT1040                                                          
         CLI   BFLD1,C'U'                                                       
         BNE   EDT1040                                                          
         CLC   BFLD1(2),12(R3)       MATCH UN                                   
         BE    EDT1050                                                          
*                                                                               
EDT1040  LA    R3,32(R3)                                                        
         LA    R4,1(R4)                                                         
         LA    R7,3(R7)                                                         
         BCT   R6,EDT1030                                                       
         B     LFMERR                                                           
*                                                                               
EDT1050  CLI   BFLD2,C'T'           SEE IF A TARGET                             
         BNE   EDT1090                                                          
         CLI   BFLD2LEN,2                                                       
         BNE   ERRINV                                                           
         LA    R1,ETRGLST                                                       
         CLC   BFLD2(2),=C'T1'                                                  
         BE    EDT1070                                                          
         LA    R1,ETRGLST+3                                                     
         CLC   BFLD2(2),=C'T2'                                                  
         BNE   ERRINV                                                           
*                                                                               
EDT1070  OC    0(3,R1),0(R1)                                                    
         BNZ   DUPPERR                                                          
*                                                                               
EDT1080  MVC   0(3,R1),0(R7)       MOVE 3 BYTE CODE FROM EDEMLIST               
         B     EDT1120             GO DO NEXT SCANNER LINE                      
*                                                                               
EDT1090  CLI   0(R4),0             SEE IF WEIGHT ALREADY THERE                  
         BNE   DUPPERR                                                          
         EJECT                                                                  
*                                                                               
EDT1100  CLI   1(R7),C'R'          RATINGS CAN'T HAVE WEIGHT                    
         BNE   EDT1110                                                          
         MVI   ERRCD,NORTWTS                                                    
         B     LFMERR                                                           
*                                                                               
EDT1110  L     R1,BFLD2B                                                        
         CLI   BFLD2LEN,0                                                       
         BE    LFMERR               WEIGHT INVALID OR MISSING                   
         TM    BFLD2VAL,X'80'        TEST NUMERIC                               
         BZ    LFMERR                                                           
         CH    R1,=H'255'                                                       
         BH    LFMERR                                                           
         LTR   R1,R1                                                            
         BZ    LFMERR                                                           
         STC   R1,0(R4)                                                         
         CLI   POLSW,1                                                          
         BNE   EDT1120                                                          
         CLC   POLDEMOS(3),=3X'00'                                              
         BE    EDT1120             NO POL DEMOS                                 
         BAS   RE,CKPOLWTS                                                      
*                                                                               
EDT1120  DS    0H                                                               
         LA    R5,BSCANLNQ(R5)     NEXT BIG SCANNER LINE                        
         L     R3,FULL             RESTORE SCANNER BCT                          
         BCT   R3,EDT1020                                                       
         DROP  R5                                                               
*                                                                               
EDT1130  OC    EWGTLST,EWGTLST                                                  
         BZ    EDT1140                                                          
         MVI   ERRCD,NOWTDEM       WEIGHTS BUT NO WEIGHTED DEMO                 
         CLI   EWGTNM,C' '                                                      
         BNH   LFMERR                                                           
         B     EDT1150                                                          
*                                                                               
EDT1140  CLI   EWGTNM,C' '         WEIGHTED DEMO BUT NO WEIGHTS                 
         BNH   EDT1150                                                          
         MVI   ERRCD,NOWTS                                                      
         B     LFMERR                                                           
*                                                                               
EDT1150  CLI   EMSTRIND,0          TEST MASTER OR SUB ESTIMATE                  
         BE    EDT1160             NO                                           
         CLC   SVDEMOS(60),EDEMLST  TEST SAME DEMOS                             
         BNE   EDT1155              NO - THEY SHOULD BE !                       
         CLC   SVDEMOS+80(28),EUSRNMS  TEST SAME USER NAMES                     
         BNE   EDT1155                 NO - SHOULD BE THE SAME                  
         B     EDT1160                 WEIGHTS CAN DIFFER (SEE CANDICE)         
EDT1155  LA    R2,ESTDEMSH         SET CURSOR TO DEMOS                          
         B     ERRNOCHG                                                         
*                                                                               
EDT1160  MVI   POLCHG,0                                                         
         CLI   POLSW,2             SEE IF CHANGING POL                          
         BNE   EDT1170                                                          
         CLC   SVUSRNMS,EUSRNMS    SEE IF CHANGING USER NAMES                   
         BE    EDT1170                                                          
         MVC   SVUSRNMS,EUSRNMS                                                 
         MVI   POLCHG,1                                                         
         EJECT                                                                  
*                                                                               
EDT1170  LA    R2,ESTRBKH            RATING BOOK                                
         GOTO1 ANY                   REQUIRED                                   
         XC    EBOOK,EBOOK                                                      
         CLI   OVSYS,3               SEE IF NETPAK EST                          
         BNE   EDT1180                                                          
         CLI   8(R2),C'0'                                                       
         BL    ERRINV                                                           
         B     EDT1190                                                          
*                                                                               
EDT1180  CLC   8(6,R2),=C'LATEST'                                               
         BE    EDT1200                                                          
*                                                                               
EDT1190  GOTO1 VDATVAL,DMCB,(2,8(R2)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    DTERR                                                            
         GOTO1 VDATCON,DMCB,(0,WORK),(3,WORK+10)                                
         MVC   EBOOK,WORK+10                      YM                            
*                                                                               
EDT1200  CLI   EMSTRIND,0                                                       
         BE    EDT1210                                                          
         CLC   SVBOOK,EBOOK                                                     
         BE    EDT1210                                                          
         CLC   SVEBCPRD,=C'POL'    ALLOW POL BOOK CHANGES                       
         BNE   ERRNOCHG                                                         
*                                                                               
EDT1210  CLI   POLSW,1                                                          
         BNE   EDT1220                                                          
         CLC   POLBOOK,EBOOK                                                    
         BNE   BRPOLERR                                                         
*                                                                               
EDT1220  DS    0H                                                               
         CLC   SVEBCPRD,=C'POL'                                                 
         BNE   EDT1240                                                          
         CLI   SVACT,C'A'          ON POL ADDS SET POLCHG TO 1                  
         BE    EDT1230                                                          
         CLC   SVBOOK,EBOOK        SEE IF BOOK CHANGED                          
         BE    EDT1240             NO                                           
*                                  ON POL BOOK CHANGES - GO CHANGE              
*                                  BRANDS                                       
EDT1230  MVC   SVBOOK,EBOOK        SAVE POL BOOK                                
         MVI   POLCHG,1            SET POL CHANGE SWITCH                        
         EJECT                                                                  
*                                                                               
EDT1240  LA    R2,ESTHUTH          HUT ADJUSTMENT                               
         GOTO1 ANY                                                              
         MVI   EHUTADJ,0                                                        
         CLC   8(4,R2),=C'AUTO'                                                 
         BE    EDT1250                                                          
         CLI   5(R2),3                                                          
         BNE   ERRINV                                                           
         MVC   WORK(3),8(R2)                                                    
         MVC   WORK+3(3),=C'/77'                                                
         GOTO1 VDATVAL,DMCB,(2,WORK),WORK+10                                    
         OC    DMCB(4),DMCB                                                     
         BZ    DTERR                                                            
         PACK  DUB,WORK+12(2)      MTH                                          
         CVB   R0,DUB                                                           
         SLL   R0,4                                                             
         STC   R0,EHUTADJ                                                       
*                                                                               
EDT1250  CLI   EMSTRIND,0                                                       
         BE    EDT1260                                                          
         CLC   SVHUT,EHUTADJ                                                    
         BE    EDT1260                                                          
         CLC   SVEBCPRD,=C'POL'    ALLOW POL HUT CHANGES                        
         BNE   ERRNOCHG                                                         
*                                                                               
EDT1260  CLI   POLSW,1                                                          
         BNE   EDT1270                                                          
         CLC   POLHUT,EHUTADJ                                                   
         BNE   BRPOLERR                                                         
*                                                                               
EDT1270  CLC   SVEBCPRD,=C'POL'                                                 
         BNE   EDT1280                                                          
         CLC   SVHUT,EHUTADJ                                                    
         BE    EDT1280                                                          
         MVC   SVHUT,EHUTADJ                                                    
         MVI   POLCHG,1                                                         
*                                                                               
EDT1280  LA    R2,ESTMENUH              DPT MENU                                
         GOTO1 ANY                                                              
         CLI   5(R2),1                                                          
         BNE   ERRINV                   1 CHARACTER                             
         MVC   EDAYMENU,8(R2)                                                   
         MVC   DMCB(2),AGYALPHA                                                 
         MVC   DMCB+2(1),SVEBCMED                                               
         MVC   DMCB+3(1),EDAYMENU                                               
         GOTO1 VDPTRD,DMCB,,REC2,VDATAMGR                                       
         MVI   ERRCD,NOFNDERR            MENU NOT ON FILE                       
         CLI   DMCB+8,X'FF'                                                     
         BE    LFMERR                                                           
         CLI   EMSTRIND,0                                                       
         BE    EDT1290                                                          
         CLC   SVDPT,EDAYMENU                                                   
         BNE   ERRNOCHG                                                         
         EJECT                                                                  
*                                                                               
EDT1290  CLI   POLSW,1                                                          
         BNE   EDT1300                                                          
         CLC   POLDPT,EDAYMENU                                                  
         BE    EDT1310                                                          
         B     BRPOLERR                                                         
*                                                                               
EDT1300  CLC   SVEBCPRD,=C'POL'                                                 
         BNE   EDT1310                                                          
         CLC   SVDPT,EDAYMENU                                                   
         BE    EDT1310                                                          
         MVC   SVDPT,EDAYMENU                                                   
         MVI   POLCHG,1                                                         
*                                                                               
EDT1310  LA    R2,ESTFLTRH         FILTERS                                      
         XC    EPROF(3),EPROF                                                   
         CLI   5(R2),0             NO INPUT                                     
         BE    EDT1340                                                          
         OC    8(3,R2),SPACES                                                   
         CLC   8(3,R2),SPACES      TREAT BLANKS AS NO INPUT                     
         BE    EDT1340                                                          
         MVC   EPROF(3),ESTFLTR                                                 
         LA    R4,EPROF                                                         
         LA    R5,3                                                             
*                                                                               
EDT1320  CLI   0(R4),C' '                                                       
         BE    EDT1330                                                          
         CLI   0(R4),C'A'                                                       
         BL    ERRINV                                                           
         CLI   0(R4),C'9'                                                       
         BH    ERRINV                                                           
*                                                                               
EDT1330  LA    R4,1(R4)                                                         
         BCT   R5,EDT1320                                                       
*                                                                               
EDT1340  CLI   SVCLEX+3,C'Y'       SEE IF FILTERS REQUIRED                      
         BNE   EDT1350             NO                                           
         OC    EPROF(3),EPROF                                                   
         BZ    ERRMSSNG                                                         
*                                                                               
EDT1350  DS    0H                                                               
         CLI   SVF0PROF+2,C'Y'    FILTER 1 REQUIRED                             
         BNE   EDT1354                                                          
         MVI   ERRCD,FLT1MSS                                                    
         CLI   EPROF,C' '                                                       
         BNH   LFMERR                                                           
*                                                                               
EDT1354  CLI   SVF0PROF+3,C'Y'    FILTER 2 REQUIRED                             
         BNE   EDT1355                                                          
         MVI   ERRCD,FLT2MSS                                                    
         CLI   EPROF+1,C' '                                                     
         BNH   LFMERR                                                           
*                                                                               
EDT1355  CLI   SVF0PROF+4,C'Y'    FILTER 3 REQUIRED                             
         BNE   EDT1360                                                          
         MVI   ERRCD,FLT3MSS                                                    
         CLI   EPROF+2,C' '                                                     
         BNH   LFMERR                                                           
*                                                                               
EDT1360  DS    0H                                                               
         CLI   SVF0PROF,C'N'       POL=BRAND EST FILTERS                        
         BE    EDT1380                                                          
         CLI   POLSW,1            IF POL EST EXISTS                             
         BNE   EDT1370                                                          
         CLC   POLFLTRS,EPROF     & ESTIMATE FILTERS ARE DIFFERENT              
         BE    EDT1380                                                          
         CLI   SVEBCMED,C'R'      & RADIO                                       
         BNE   BRPOLERR                                                         
         CLI   SVCLPROF,C'0'      & BRAND POL (1 & 2)                           
         BE    BRPOLERR           - THEN DIFF OKAY (CHK AT EDT1580 TOO)         
         EJECT                                                                  
*                                                                               
EDT1370  CLC   SVEBCPRD,=C'POL'                                                 
         BNE   EDT1380                                                          
         CLC   SVFLTRS,EPROF                                                    
         BE    EDT1380                                                          
         MVC   SVFLTRS,EPROF                                                    
         MVI   POLCHG,1                                                         
*                                                                               
EDT1380  LA    R2,ESTECONH    ** ECONTROL                                       
         MVI   ECONTROL,0                                                       
         CLI   5(R2),0                                                          
         BE    EDT1400                                                          
*                                                                               
         OC    ESTECON,SPACES                                                   
         CLC   ESTECON(2),=C'E '                                                
         BNE   EDT1390                                                          
         MVI   ECONTROL,EBILESTQ                                                
*                        ECONTROL 'E' ONLY FOR CERTAIN AGYS                     
*                                                                               
         LA    R1,ECTAGY                                                        
*                                                                               
EDT1385  CLI   0(R1),X'FF'                                                      
         BE    ERRINV                                                           
         CLC   0(2,R1),AGYALPHA                                                 
         BE    EDT1400                                                          
         LA    R1,2(R1)                                                         
         B     EDT1385                                                          
*                                                                               
EDT1390  CLC   ESTECON(3),=C'NSC'  'NO SEPARATE COMMISSION'                     
         BNE   ERRINV                                                           
         MVI   ECONTROL,ENSEPCMQ                                                
*                                                                               
EDT1400  CLI   SVACT,C'A'          SEE IF ADD                                   
         BE    EDT1460                                                          
         GOTO1 =A(CKLINID),RR=RELO   TEST TERMINAL AUTHORIZED                   
         BE    EDT1460             ALLOW CHG OF OLD-NEW                         
         CLC   SVECON,ECONTROL     SEE IF ECONTROL CHANGED                      
         BE    EDT1460             NO - SKIP BILLING CHECKS                     
         XC    KEY,KEY             MUST CHECK FOR BILLING RECORDS               
         MVC   KEY(9),SVKEY                                                     
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
         CLC   KEY(8),KEYSAVE                                                   
         BE    ERRNOCHG            MEANS NO BILLS FOUND                         
         EJECT                                                                  
*                                                                               
EDT1420  DS    0H                  IF CHANGING POL                              
*                                  I MUST CHECK FOR BILLING FOR ANY             
*                                  BRAND                                        
         CLC   SVEBCPRD,=C'POL'                                                 
         BNE   EDT1460                                                          
         BAS   RE,FRSTPRD                                                       
         B     *+8                                                              
*                                                                               
EDT1430  BAS   RE,NEXTPRD                                                       
         BNE   EDT1460             DONE                                         
*                                                                               
         GOTO1 SEQ                                                              
         CLC   KEY(8),KEYSAVE      SEE IF I FOUND BILL                          
         BE    ERRNOCHG            BILL FOUND - ERROR                           
         B     EDT1430             GO TRY NEXT PRODUCT                          
*                                                                               
EDT1460  CLI   POLSW,1             CHECK VERSES POL DATA                        
         BNE   EDT1470                                                          
         CLC   POLECON,ECONTROL                                                 
         BE    EDT1480                                                          
         B     BRPOLERR                                                         
*                                                                               
EDT1470  CLC   SVEBCPRD,=C'POL'                                                 
         BNE   EDT1480                                                          
         CLC   SVECON,ECONTROL                                                  
         BE    EDT1480                                                          
         MVC   SVECON,ECONTROL                                                  
         MVI   POLCHG,1                                                         
         EJECT                                                                  
*                                                                               
EDT1480  DS    0H                                                               
         XC    ERTLSCHM,ERTLSCHM                                                
         LA    R2,ESTERTLH                                                      
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   ERTLSCHM,8(R2)                                                   
*                                                                               
         CLI   SVF0PROF+1,C'N'     POL=BRAND RTL SCHEME                         
         BE    EDT1510                                                          
         CLI   POLSW,1            IF POL EST EXISTS                             
         BNE   EDT1500                                                          
         CLC   POLRTL,ERTLSCHM    & RETAIL SCHEME DIFFERENT                     
         BE    EDT1510                                                          
         CLI   SVEBCMED,C'R'      & RADIO                                       
         BNE   BRPOLERR                                                         
         CLI   SVCLPROF,C'0'      & BRAND POL (1 & 2)                           
         BE    BRPOLERR           - THEN DIFF OKAY (CHK AT EDT1580 TOO)         
*                                                                               
EDT1500  CLC   SVEBCPRD,=C'POL'                                                 
         BNE   EDT1510                                                          
         CLC   SVRTL,ERTLSCHM                                                   
         BE    EDT1510                                                          
         MVC   SVRTL,ERTLSCHM                                                   
         MVI   POLCHG,1                                                         
*                                                                               
EDT1510  XC    WORK,WORK                                                        
         OC    SVE1USER,SVE1USER   ANY "ESTIMATE 1" INFO                        
         BZ    EDT1520                                                          
         LA    R2,ESTUSR1H         A(INPUT)                                     
         MVC   HALF(1),SVE1TYPE    TYPE                                         
         MVC   HALF+1(1),SVE1FLG1  FLAG                                         
         MVC   BYTE,SVE1LEN        LENGTH                                       
         GOTO1 =A(EDTUSR),RR=RELO                                               
*                                                                               
EDT1520  MVC   EUSER1,WORK                                                      
         MVC   ESTUSR1,WORK        RE-TRANSMIT FIELD                            
         OI    ESTUSR1H+6,X'80'                                                 
*                                                                               
         XC    WORK,WORK                                                        
         OC    SVE2USER,SVE2USER   ANY "ESTIMATE 2" INFO                        
         BZ    EDT1530                                                          
         LA    R2,ESTUSR2H         A(INPUT FIELD)                               
         MVC   HALF(1),SVE2TYPE    TYPE                                         
         MVC   HALF+1(1),SVE2FLG1  FLAG                                         
         MVC   BYTE,SVE2LEN        LENGTH                                       
         GOTO1 =A(EDTUSR),RR=RELO                                               
*                                                                               
EDT1530  MVC   EUSER2,WORK                                                      
         MVC   ESTUSR2,WORK        CLEAR OR RE-TRANSMIT FIELD.                  
         OI    ESTUSR2H+6,X'80'                                                 
*                                                                               
         GOTO1 =A(VALPROF),RR=RELO                                              
         BE    EDT1539                                                          
         LA    R2,ESTOPTH          SET R2 TO FIELD W/ ERROR                     
         CLI   DMCB,1                                                           
         BE    ERRINV                                                           
         CLI   DMCB,2                                                           
         BE    DLYERR                                                           
         CLI   DMCB,3                                                           
         BE    REQERR                                                           
         DC    H'0'                                                             
*                                                                               
EDT1539  CLC   SVEBCPRD,=C'POL'                                                 
         BNE   EDT10                                                            
         CLI   POLCHG,0            SEE IF NEED TO CHANGE BRAND ESTS             
         BE    EDT10               NO                                           
*                                                                               
         BAS   RE,FRSTPRD          READ BRAND ESTIMATES                         
         B     *+8                                                              
*                                                                               
EDT1540  BAS   RE,NEXTPRD                                                       
         BNE   EDT1690             DONE                                         
*                                                                               
         LA    R8,REC2             NOTE - ESTHDRD NOW COVERS REC2               
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
         CLI   SVACT,C'A'                                                       
         BNE   EDT1630                                                          
* ON POL ADDS BOOK MUST AGREE WITH BRAND ESTIMATE                               
         CLC   EBOOK,SVBOOK                                                     
         BE    EDT1560                                                          
         LA    R2,ESTRBKH          CURSOR TO BOOK                               
         B     BRPOLERR                                                         
*                                                                               
EDT1560  CLC   EHUTADJ,SVHUT                                                    
         BE    EDT1570                                                          
         LA    R2,ESTHUTH                                                       
         B     LFMERR                                                           
*                                                                               
EDT1570  CLC   EDAYMENU,SVDPT                                                   
         BE    EDT1580                                                          
         LA    R2,ESTMENUH                                                      
         B     BRPOLERR                                                         
*                                                                               
EDT1580  DS    0H                                                               
         CLI   SVF0PROF,C'N'       POL=BRAND EST FILTERS                        
         BE    EDT1600                                                          
         CLC   EPROF(3),SVFLTRS   IF ESTIMATE FILTERS DIFFERENT                 
         BE    EDT1600                                                          
         CLI   SVEBCMED,C'R'      & RADIO                                       
         BNE   EDT1590                                                          
         CLI   SVCLPROF,C'0'      & BRAND POL (1 OR 2)                          
         BNE   EDT1600            - THEN DIFFERENCE OKAY                        
*                                                                               
EDT1590  LA    R2,ESTFLTRH                                                      
         B     LFMERR                                                           
         EJECT                                                                  
*                                                                               
EDT1600  CLC   ECONTROL,SVECON                                                  
         BE    EDT1610                                                          
         LA    R2,ESTECONH                                                      
         B     LFMERR                                                           
*                                                                               
EDT1610  CLI   SVF0PROF+1,C'N'     POL=BRAND RTL SCHEME                         
         BE    EDT1540                                                          
         CLC   ERTLSCHM,SVRTL                                                   
         BE    EDT1540                                                          
         CLI   SVEBCMED,C'R'      & RADIO                                       
         BNE   EDT1620                                                          
         CLI   SVCLPROF,C'0'      & BRAND POL (1 OR 2)                          
         BNE   EDT1540            - THEN DIFFERENCE OKAY                        
*                                                                               
EDT1620  LA    R2,ESTERTLH                                                      
         B     LFMERR                                                           
*                                                                               
EDT1630  CLC   EBOOK,SVBOOK        SEE IF I NEED TO CHG BRAND                   
         BNE   EDT1660                                                          
         CLC   EPWPCT,SVPOLPW                                                   
         BNE   EDT1660                                                          
         CLC   EHUTADJ,SVHUT                                                    
         BNE   EDT1660                                                          
         CLC   EDAYMENU,SVDPT                                                   
         BNE   EDT1660                                                          
         CLI   SVF0PROF,C'N'       POL=BRAND EST FILTERS                        
         BE    EDT1635                                                          
         CLC   EPROF(3),SVFLTRS                                                 
         BNE   EDT1660                                                          
*                                                                               
EDT1635  CLC   ECONTROL,SVECON                                                  
         BNE   EDT1660                                                          
         CLI   SVF0PROF+1,C'N'     POL=BRAND RTL SCHEME                         
         BE    EDT1637                                                          
         CLC   ERTLSCHM,SVRTL                                                   
         BNE   EDT1660                                                          
*                                                                               
EDT1637  LA    R4,4                FOR BCT                                      
         LA    RF,EUSRNMS                                                       
         LA    RE,SVUSRNMS                                                      
*                                                                               
EDT1640  CLI   0(RF),C' '          SEE IF NAME USED                             
         BNH   EDT1650                                                          
         CLC   0(7,RF),0(RE)       YES THEN MUST MATCH                          
         BNE   EDT1660                                                          
*                                                                               
EDT1650  LA    RF,7(RF)                                                         
         LA    RE,7(RE)                                                         
         BCT   R4,EDT1640                                                       
         B     EDT1540                                                          
*                                                                               
EDT1660  MVC   EHUTADJ,SVHUT                                                    
         MVC   EPWPCT,SVPOLPW                                                   
         MVC   EBOOK,SVBOOK                                                     
         MVC   EDAYMENU,SVDPT                                                   
         CLI   SVF0PROF,C'N'       POL=BRAND EST FILTERS                        
         BE    *+10                                                             
         MVC   EPROF(3),SVFLTRS                                                 
         MVC   ECONTROL,SVECON                                                  
         CLI   SVF0PROF+1,C'N'     POL=BRAND RTL SCHEME                         
         BE    *+10                                                             
         MVC   ERTLSCHM,SVRTL                                                   
         LA    R4,4                FOR BCT                                      
         LA    RF,EUSRNMS                                                       
         LA    RE,SVUSRNMS                                                      
         EJECT                                                                  
*                                                                               
EDT1670  CLI   0(RF),C' '          SEE IF NAME USED                             
         BNH   EDT1680                                                          
         MVC   0(7,RF),0(RE)       STORE NEW USER NAME                          
*                                                                               
EDT1680  LA    RF,7(RF)                                                         
         LA    RE,7(RE)                                                         
         BCT   R4,EDT1670                                                       
         GOTO1 PUTREC                                                           
         BAS   RE,DOCANADA                                                      
         B     EDT1540                                                          
*                                                                               
EDT1690  LA    R8,REC                  RESET R8 TO REC                          
*                                      NOTE - ESTHDRD NOW COVERS REC            
         B     EDT10                                                            
         EJECT                                                                  
***********************************************************************         
* REFORMAT PARSNIP BLK AT REC+500 TO LOOK ALMOST LIKE SCANNER OUTPUT            
* AT REC - EXCEPT FLD1 IS LENGTH 11 FOR DEMOS LIKE ###.RADRWM2554               
***********************************************************************         
PARTOSCN NTR1                                                                   
         LA    R2,REC2+500                                                      
         USING PSND,R2             PARSNIP BLK                                  
         LA    R3,REC2                                                          
         USING BSCAND,R3           NEW BIG SCANNER-ISH BLK                      
*                                                                               
PAR10    XC    0(BSCANLNQ,R3),0(R3)                                             
         MVC   BFLD1,SPACES                                                     
         MVC   BFLD2,SPACES                                                     
         LTR   R2,R2               ANY MORE?                                    
         BZ    PARX                                                             
PAR12    CLI   PSNTAG,C'F'         FIELD IS LEFT OF '=' IS FLD1                 
         BE    PAR20                                                            
         CLI   PSNTAG,C'V'         VALUE IS RIGHT OF '=' IS FLD2                
         BE    PAR30                                                            
         B     PARX                                                             
*                                                                               
PAR20    MVC   BFLD1LEN,PSNLEN     MOVE COMPONENT                               
         MVC   BFLD1VAL,PSNSTAT                                                 
         MVC   BFLD1B,PSNNUM                                                    
         L     R4,PSNCOMP                                                       
         ZIC   R1,PSNLEN                                                        
         CH    R1,=H'11'           CAN'T BE >11                                 
         BNH   *+8                                                              
         LA    R1,11                                                            
         SH    R1,=H'1'                                                         
         BM    PAR22                                                            
         EX    R1,*+4                                                           
         MVC   BFLD1(0),0(R4)                                                   
PAR22    L     R6,PSNFLD           SAVE A(NEXT VALUE)                           
         L     R2,PSNVAL           SET TO PROCESS VALUE                         
         B     PAR12                                                            
*                                                                               
PAR30    MVC   BFLD2LEN,PSNLEN     MOVE COMPONENT                               
         MVC   BFLD2VAL,PSNSTAT                                                 
         MVC   BFLD2B,PSNNUM                                                    
         L     R4,PSNCOMP                                                       
         ZIC   R1,PSNLEN                                                        
         CH    R1,=H'10'           CAN'T BE >10                                 
         BNH   *+8                                                              
         LA    R1,10                                                            
         SH    R1,=H'1'                                                         
         BM    PAR32                                                            
         EX    R1,*+4                                                           
         MVC   BFLD2(0),0(R4)                                                   
PAR32    LR    R2,R6               NEXT FIELD                                   
         LA    R3,BSCANLNQ(R3)                                                  
         B     PAR10                                                            
*                                                                               
PARX     XIT1                                                                   
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*----------------------------------------------------------*                    
* SAVE ESTHDR IN REC+1000 AND PROCESS ESTHDR IN REC2       *                    
*----------------------------------------------------------*                    
         SPACE 1                                                                
DOCANADA NTR1                                                                   
         MVC   WORK2(20),SVKEY     SAVE REAL SVKEY                              
         LA    R0,REC+1000         SET 'TO' ADDR                                
         LA    R1,ESTHDRLN         SET 'TO' LEN                                 
         LA    RE,REC              SET 'FROM' ADDR                              
         LR    RF,R1               SET 'FROM' LEN                               
         MVCL  R0,RE                                                            
* MOVE ESTHDR READ INTO REC2 TO REC                                             
         LA    R0,REC              SET 'TO' ADDR                                
         LA    R1,ESTHDRLN         SET 'TO' LEN                                 
         LA    RE,REC2             SET 'FROM' ADDR                              
         LR    RF,R1               SET 'FROM' LEN                               
         MVCL  R0,RE                                                            
         MVC   SVKEY,KEY                                                        
         GOTO1 CNCHASPT            CANADA USES SVKEY                            
*                                                                               
         MVC   SVKEY,WORK2         RESTORE REAL SVKEY                           
         LA    R0,REC              SET 'TO' ADDR                                
         LA    R1,ESTHDRLN         SET 'TO' LEN                                 
         LA    RE,REC+1000         SET 'FROM' ADDR                              
         LR    RF,R1               SET 'FROM' LEN                               
         MVCL  R0,RE                                                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
EDT10    CLI   SVAPROF+7,C'C'        TEST CANADA                                
         BNE   EDT12                                                            
         CLI   SVEBCMED,C'N'         FOR NETWK OR COMBINED ONLY                 
         BE    OUTPUT                ONLY CHG AUTH $'S                          
         CLI   SVEBCMED,C'C'                                                    
         BE    OUTPUT                ONLY CHG AUTH $'S                          
*                                                                               
EDT12    LA    R2,ESTCOPYH           COPY CODE                                  
         MVI   ECOPY,0                                                          
         CLI   5(R2),0                                                          
         BE    EDT14                                                            
         MVI   ERRCD,NOTALPH                                                    
         TM    4(R2),X'04'                                                      
         BZ    LFMERR                                                           
         MVC   ECOPY,ESTCOPY                                                    
*                                                                               
EDT14    LA    R2,ESTREPH            SPECIAL REP                                
         XC    EREP,EREP                                                        
         CLI   5(R2),0                                                          
         BE    EDT16                                                            
         MVI   ERRCD,NOTNUM                                                     
         TM    4(R2),X'08'           NOT-NUMERIC                                
         BZ    LFMERR                                                           
         GOTO1 PACK                                                             
         STH   R0,HALF                                                          
         MVC   EREP,HALF                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'R'                                                         
         MVC   KEY+1(1),SVEBCMED                                                
         UNPK  KEY+2(3),DUB+5(3)                                                
         OI    KEY+4,X'F0'                                                      
         MVC   KEY+5(2),AGYALPHA                                                
         MVC   KEY+7(10),=10C'0'                                                
         LA    R4,REC2                                                          
         ST    R4,AREC                                                          
         GOTO1 RDSTA                                                            
         EJECT                                                                  
*                                                                               
EDT16    DS    0H                                                               
         LA    R2,ESTCPPEH         CPP ESTIMATE                                 
         XC    ECPPCLT(L'ECPPCLT+L'ECPPEST),ECPPCLT                             
         CLI   5(R2),0                                                          
         BE    EDT18                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(4),SVKEY        CLT MAY BE REPLACED                          
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    EDT16D              NO                                           
         GOTO1 PACK                                                             
         CH    R0,=H'255'                                                       
         BH    ERRINV                                                           
         B     EDT16K                                                           
*                                                                               
EDT16D   CLC   SVEBCPRD,=C'POL'                                                 
         BE    ERRINV              NO CLT EST FOR POL                           
         LA    R5,8(R2)                                                         
         MVC   WORK(3),SPACES                                                   
         ZIC   R6,5(R2)            TOTAL INPUT LENGTH                           
         LA    R7,WORK                                                          
         XR    R4,R4                                                            
*                                                                               
EDT16E   CLI   0(R5),C','          CHK FOR DELIMITER                            
         BE    EDT16F                                                           
         CLI   0(R5),C'/'          CHK FOR DELIMITER                            
         BE    EDT16F                                                           
         MVC   0(1,R7),0(R5)                                                    
         LA    R7,1(R7)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R6,EDT16E                                                        
         B     ERRINV                                                           
*                                                                               
EDT16F   CH    R4,=H'3'                                                         
         BH    ERRINV                                                           
         MVC   DMCB+4(4),=X'D9000A14'                                           
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),WORK,KEY+2                                             
         CLI   DMCB,0                                                           
         BNE   ERRINV                                                           
         MVC   ECPPCLT,KEY+2                                                    
         LA    R5,1(R5)                                                         
         XC    WORK,WORK                                                        
         BCTR  R6,0                                                             
         LTR   R6,R6                                                            
         BZ    ERRINV                                                           
         XR    R4,R4                                                            
         LA    R7,WORK                                                          
*                                                                               
EDT16H   CLI   0(R5),C'0'                                                       
         BL    ERRINV                                                           
         CLI   0(R5),C'9'                                                       
         BH    ERRINV                                                           
         MVC   0(1,R7),0(R5)                                                    
         LA    R7,1(R7)                                                         
         LA    R5,1(R5)                                                         
         LA    R4,1(R4)                                                         
         BCT   R6,EDT16H                                                        
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         CVB   R0,DUB                                                           
*                                                                               
EDT16K   DS    0H                                                               
         MVC   KEY+4(3),=C'POL'                                                 
         STC   R0,KEY+7                                                         
         STC   R0,ECPPEST                                                       
         GOTO1 READ                                                             
         LA    R8,REC2                                                          
         ST    R8,AREC                                                          
         GOTO1 GETREC              ESTHDRD DSECT NOW COVERS REC2                
*                                                                               
         MVC   POLDEMOS,EDEMOS     SAVE DATA                                    
         MVC   POLDATE,ESTART                                                   
         MVC   POLTYPE,ETYPE                                                    
         LA    R8,REC              RESTORE ESTHDRD TO REC                       
         ST    R8,AREC             RESET AREC                                   
         CLC   EDEMOS(3),POLDEMOS  PRIMARY DEMO MUST MATCH                      
         BE    EDT16L                                                           
         MVI   ERRCD,PRIDERR                                                    
         B     LFMERR                                                           
*                                                                               
EDT16L   DS    0H                                                               
         CLI   POLTYPE,1                                                        
         BL    ERRINV              CPP ESTS MUST BE TYPE U-5                    
         CLI   POLTYPE,5                                                        
         BH    ERRINV                                                           
*                                                                               
         CLC   SVEBCPRD,=C'POL'    SEE IF DOING POL EST                         
         BNE   EDT16S                                                           
         GOTO1 VADDAY,DMCB,EEND,WORK,1                                          
         CLC   WORK(6),POLDATE     START MUST MATCH END PLUS 1                  
         BE    EDT18                                                            
         MVI   ERRCD,ENDOLAP       END DATE +1 MUST EQUAL START OF              
*                                  CONTINUATION EST                             
         B     LFMERR                                                           
*                                                                               
EDT16S   DS    0H                                                               
         CLC   ESTART,POLDATE      PERIODS MUST OVERLAP                         
         BL    EDT16QE                                                          
         CLC   ESTART,POLDATE+6                                                 
         BNH   EDT18                                                            
*                                                                               
EDT16QE  MVI   ERRCD,16            ESTIMATE INVALID                             
         B     LFMERR                                                           
*                                                                               
EDT18    DS    0H                                                               
         LA    R2,ESTTYPEH                                                      
         CLI   SVACT,C'A'          SEE IF ADD                                   
         BE    EDT18A                                                           
         CLI   ETYPE,0                                                          
         BE    EDT18A                                                           
         GOTO1 ANY                 ETYPE CAN'T BE DELETED                       
         B     EDT18B                                                           
*                                                                               
EDT18A   DS    0H                                                               
         MVI   ETYPE,0                                                          
         CLI   5(R2),0                                                          
         BNE   EDT18B                                                           
         CLC   SVEBCPRD,=C'POL'                                                 
         BNE   EDT18X                                                           
         CLI   POLTYPE,0                                                        
         BE    EDT18X                                                           
         B     ERRMSSNG                                                         
*                                                                               
EDT18B   CLI   OVSYS,3             SEE IF NETPAK EST                            
         BNE   EDT18C              NO                                           
         CLC   ESTTYPE,=C'CUT'                                                  
         BNE   ERRINV                                                           
         MVI   ETYPE,C'C'                                                       
         B     EDT18X                                                           
*                                                                               
EDT18C   CLC   SVEBCPRD,=C'POL'                                                 
         BNE   ERRINV                                                           
         CLI   5(R2),2                                                          
         BNE   ERRINV                                                           
         LA    R5,TYPTAB                                                        
*                                                                               
EDT18D   CLC   ESTTYPE(2),0(R5)                                                 
         BE    EDT18F                                                           
         LA    R5,3(R5)                                                         
         CLI   0(R5),X'FF'         END OF TABLE                                 
         BNE   EDT18D                                                           
         B     ERRINV                                                           
*                                                                               
EDT18F   MVC   ETYPE,2(R5)                                                      
         CLI   POLTYPE,0           SEE IF HAVE CONTINUATION EST                 
         BE    EDT18H              NO                                           
         CLC   ETYPE,POLTYPE       TYPES MUST MATCH                             
         BNE   ERRINV                                                           
*                                                                               
EDT18H   DS    0H                                                               
         MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(STRINJAN)                                            
         LA    R2,ESTSTRDH         CURSOR TO START DATE                         
*                                                                               
         GOTO1 VGETBRD,DMCB,(1,ESTART),WORK,VGETDAY,VADDAY                      
         CLC   WORK+8(2),=C'01'    START BRDMON MUST BE JAN                     
         BNE   LFMERR                                                           
         CLC   ESTART,WORK         MUST MATCH START OF JAN                      
         BNE   LFMERR                                                           
*                                                                               
         MVC   NERRCD,=AL2(ENDINDEC)                                            
         LA    R2,ESTENDDH                                                      
         CLC   EEND+2(2),=C'12'    END MONTH MUST BE DEC                        
         BNE   LFMERR                                                           
         GOTO1 VGETBRD,DMCB,(1,EEND),WORK                                       
         CLC   EEND,WORK+6         MUST MATCH END DAY OF DEC                    
         BNE   LFMERR                                                           
*                                                                               
EDT18X   DS    0H                                                               
         XC    EREQLO(2),EREQLO                                                 
         LA    R2,ESTRNGEH                                                      
         CLI   5(R2),0             NO INPUT                                     
         BE    EDT22                                                            
         CLI   5(R2),2                                                          
         BNE   EDT20B                                                           
         CLC   8(2,R2),=C'NO'                                                   
         BNE   ERRINV                                                           
         MVC   EREQLO(2),8(R2)                                                  
         B     EDT22                                                            
*                                                                               
EDT20B   DS    0H                                                               
         LA    R5,EREQLO                                                        
         LA    R6,2                                                             
         ZIC   R7,5(R2)            INPUT LENGTH                                 
         LA    R3,8(R2)                                                         
*                                                                               
EDT20C   XR    R4,R4                                                            
         LR    R1,R3                                                            
*                                                                               
EDT20D   CLI   0(R3),C','          CHK FOR DELIMITERS                           
         BE    EDT20E                                                           
         CLI   0(R3),C'-'                                                       
         BE    EDT20E                                                           
         CLI   0(R3),C'/'                                                       
         BE    EDT20E                                                           
         CLI   0(R3),C'0'                                                       
         BL    ERRINV                                                           
         CLI   0(R3),C'9'                                                       
         BH    ERRINV                                                           
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         BCT   R7,EDT20D                                                        
         B     EDT20F                                                           
*                                                                               
EDT20E   BCTR  R7,0                                                             
*                                                                               
EDT20F   LTR   R4,R4                                                            
         BZ    ERRINV                                                           
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1)                                                      
         CVB   R0,DUB                                                           
         CH    R0,=H'255'                                                       
         BH    ERRINV                                                           
         CH    R0,=H'0'                                                         
         BNH   ERRINV                                                           
         STC   R0,0(R5)                                                         
         LA    R5,1(R5)                                                         
         LA    R3,1(R3)                                                         
         BCT   R6,EDT20C                                                        
         LTR   R7,R7                                                            
         BNZ   ERRINV              NO INPUT SHOULD BE LEFT                      
*                                                                               
         CLC   EREQLO,EREQHI                                                    
         BNL   ERRINV                                                           
         CLC   SVEST,EREQLO                                                     
         BL    ERRINV                                                           
         CLC   SVEST,EREQHI                                                     
         BH    ERRINV                                                           
         EJECT                                                                  
*                                                                               
EDT22    DS    0H                                                               
         MVI   ERATE,0                                                          
         MVI   ERATECST,0                                                       
         LA    R2,ESTRTYPH                                                      
         CLI   5(R2),0             CHK FOR INPUT                                
         BE    EDT22X              NONE                                         
         CLI   SVCLPROF+14,C'*'    IF CLT NOT ALLOWED SPECIAL RATES             
         BE    ERRINV              DON'T ALLOW ON ESTIMATE                      
         CLI   OVSYS,3             CHECK FOR NETWORK                            
         BNE   EDT22B                                                           
         CLI   5(R2),2                                                          
         BNE   EDT22B                                                           
         CLI   ESTRTYP+1,C'T'      CHECK FOR TIME/INT COVER                     
         BE    EDT22A                                                           
         CLI   ESTRTYP+1,C'I'      CHECK FOR TIME/INT COVER                     
         BE    EDT22A                                                           
         CLI   ESTRTYP+1,C'A'      CHECK FOR ALL COVER                          
         BNE   ERRINV                                                           
EDT22A   MVC   ERATECST,ESTRTYP+1                                               
         B     *+12                                                             
EDT22B   CLI   5(R2),1                                                          
         BNE   ERRINV                                                           
         CLI   ESTRTYP,C'*'        * MEANS IGNORE CLIENT RATE TYPE              
         BE    EDT22C                                                           
         CLI   ESTRTYP,C'0'        MUST BE 0-9                                  
         BL    ERRINV                                                           
         CLI   ESTRTYP,C'9'                                                     
         BH    ERRINV                                                           
EDT22C   MVC   ERATE,ESTRTYP                                                    
*                                                                               
EDT22X   DS    0H                                                               
*                                                                               
OUTPUT   MVC   KEY,SVKEY                                                        
         ST    R8,AREC                                                          
         CLI   SVACT,C'A'                                                       
         BNE   OUT1                                                             
         CLI   EDAILY,C'Y'         IF DAILY ESTIMATE                            
         BNE   OUT1A                                                            
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF         1ST CHECK THAT NOT MORE THAN 53 DAYS         
         GOTO1 CPERVERT,DMCB,ESTART,EEND,WORK,WORK+4,WORK+8                     
         LH    R4,8(R1)                                                         
         CH    R4,=H'53'           ONLY ALLOW DAILY FOR MAX OF 53 DAYS          
         BH    DLYERR                                                           
         DROP  RF                                                               
*                                                                               
OUT1A    MVC   REC(13),SVKEY                                                    
         MVC   ELEN,=H'640'        WILL CHANGE WITH NEW RECS                    
         OI    ECNTRL,X'01'        SET ON CONVERTED IND                         
         MVC   EPRDCD+1(1),SVPRD                                                
         GOTO1 ADDREC                                                           
         MVC   SVKEY,KEY                                                        
         GOTO1 CNADDSPT                                                         
         B     UPDCASH                                                          
*                                                                               
OUT1     LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 GETREC                                                           
         ST    R8,AREC                                                          
*               REREAD REC INTO REC2 BEFORE PUTREC                              
         CLI   EDAILY,C' '                                                      
         BNH   OUT2                                                             
         CLI   EDAILY,C'N'                                                      
         BE    OUT2                                                             
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF         1ST CHECK THAT NOT MORE THAN 53 DAYS         
         GOTO1 CPERVERT,DMCB,ESTART,EEND,WORK,WORK+4,WORK+8                     
         LH    R4,8(R1)                                                         
         CH    R4,=H'53'           ONLY ALLOW DAILY FOR MAX OF 53 DAYS          
         BH    DLYERR                                                           
*                                                                               
OUT2     GOTO1 PUTREC                                                           
         GOTO1 CNCHASPT                                                         
         EJECT                                                                  
*              UPDATE CASH ESTIMATE RECORD WITH TRADE PRD CODE                  
UPDCASH  DS    0H                                                               
         OC    CASHPRD,CASHPRD     ANY CASH PRD TO CHANGE?                      
         BZ    UPDCASH2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(8),SVKEY                                                     
         MVC   KEY+4(3),CASHPRD    UPDATE CASH PRODUCT EST                      
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY                                                   
         BE    *+6                 CASH EST NOT ON FILE                         
         DC    H'0'                                                             
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 GETREC                                                           
         MVC   ETRDPRD-ESTHDR(1,R7),SVPRD     TRADE PRD IS THIS PRD             
         GOTO1 PUTREC                                                           
UPDCASH2 DS    0H                                                               
         OC    OLDCASH,OLDCASH     ANY OLD CASH PRD TO DELETE                   
         BZ    REQREC                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(8),SVKEY                                                     
         MVC   KEY+4(3),OLDCASH    OLD CASH PRODUCT EST                         
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY                                                   
         BE    *+6                 CASH EST NOT ON FILE                         
         DC    H'0'                                                             
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 GETREC                                                           
         MVI   ETRDPRD-ESTHDR(R7),0    REMOVE TRADE PRODUCT                     
         GOTO1 PUTREC                                                           
         EJECT                                                                  
*                        GENERATE REQUEST RECORD                                
REQREC   XC    REC2(150),REC2                                                   
         LA    R1,REC2                                                          
         MVI   10(R1),132          L2 REQ                                       
         MVI   14(R1),106                                                       
         LA    R1,REC2+26                                                       
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'L2'                                                   
         MVC   2(2,R1),14(RA)                                                   
         MVC   4(1,R1),SVEBCMED                                                 
         MVC   5(3,R1),SVEBCCLT                                                 
         MVC   11(3,R1),SVEBCPRD                                                
         XR    R0,R0                                                            
         IC    R0,SVEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R1),DUB                                                     
         MVI   61(R1),C'N' '                                                    
         MVC   68(7,R1),=C'CONTROL'                                             
         MVI   65(R1),C'A'                                                      
         CLI   SVACT,C'A'                                                       
         BE    *+8                                                              
         MVI   65(R1),C'C'                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',REC2,REC2                    
         MVI   WORK,0              SO I'LL REFORMAT REC                         
*                                                                               
         CLI   COS2FLAG,0          ECOST2 CHANGED?                              
         BE    EDTX                                                             
*                                                                               
         XC    WORK,WORK             CHECK PROFILE - C3 REQUEST NEEDED?         
         MVC   WORK+16(4),=C'S0B0'   B0 PROFILE                                 
         MVC   WORK+20(2),AGYALPHA   PROFILE NAME                               
         MVC   WORK+22(1),SVEBCMED                                              
         MVC   WORK+23(3),SVEBCCLT                                              
         GOTO1 VGETPROF,DMCB,WORK+16,WORK,VDATAMGR                              
         CLI   WORK+1,C'Y'                                                      
         BNE   EDTX                                                             
*                                                                               
         XC    REC2(150),REC2      C3 -TO UPDATE BUYS WITH NEW COS2             
         LA    R1,REC2                                                          
         MVI   10(R1),132          C3 REQ                                       
         MVI   14(R1),106                                                       
         LA    R1,REC2+26                                                       
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'C3'                                                   
         MVC   2(2,R1),14(RA)                                                   
         MVC   4(1,R1),SVEBCMED                                                 
         MVC   5(3,R1),SVEBCCLT                                                 
         MVC   11(3,R1),SVEBCPRD                                                
         MVC   49(4,R1),SVCOS2                                                  
         XR    R0,R0                                                            
         IC    R0,SVEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R1),DUB                                                     
         MVC   68(7,R1),=C'CONTROL'                                             
         CLI   SVACT,C'A'                                                       
         BE    EDTX                DONT UPDATE BUYLINE ON ADD                   
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',REC2,REC2                    
         MVI   WORK,0              SO I'LL REFORMAT REC                         
*                                                                               
EDTX     XMOD1 1        REFORMAT RECORD ON SUCCESSFUL ACTION                    
         EJECT                                                                  
*                                                                               
ERRDMPOL MVI   ERRCD,DEMNIPOL      BRAND/POL DEMO NOT IN POL DEMOS              
         B     LFMERR                                                           
*                                                                               
ERRTOTH  MVI   ERRCD,TOTHERR                                                    
         B     LFMERR                                                           
*                                                                               
ERRNOCHG MVI   ERRCD,NOCHGERR                                                   
         B     LFMERR                                                           
*                                                                               
ERRMSSNG MVI   ERRCD,MSSNGERR                                                   
         B     LFMERR                                                           
*                                                                               
BRPOLERR MVI   ERRCD,BRPOLDTS                                                   
         B     LFMERR                                                           
*                                                                               
ERRINV   MVI   ERRCD,INVERR                                                     
         B     LFMERR                                                           
*                                                                               
DTERR    MVI   ERRCD,DATERR                                                     
         B     LFMERR                                                           
*                                                                               
ERRESTD  MVI   ERRCD,ESTDERR                                                    
         B     LFMERR                                                           
*                                                                               
DUPPERR  MVI   ERRCD,DUPENTRY                                                   
         B     LFMERR                                                           
*                                                                               
LFMERR   GOTO1 ERROR                                                            
*                                                                               
DLYERR   MVC   NERRCD,=AL2(DAILYERR)                                            
         LA    R2,ESTOPTH                                                       
         B     LFMERRN                                                          
*                                                                               
REQERR   MVC   NERRCD,=AL2(POLRQERR)                                            
*                                                                               
LFMERRN  MVI   ERRCD,NEWERR                                                     
         GOTO1 ERROR                                                            
         EJECT                                                                  
CKPOLWTS NTR1                     POL WEIGHTS MUST MATCH                        
         LA    R5,POLDEMOS        R7 POINTS TO BRAND DEMO                       
         LA    R3,DMAX            R4 POINTS TO BRAND WEIGHT                     
         LA    R6,PEWGTLST                                                      
CKPOL1   CLC   0(3,R5),0(R7)                                                    
         BE    CKPOL5              MATCHING DEMO AND WT.                        
         LA    R5,3(R5)                                                         
         LA    R6,1(R6)                                                         
         BCT   R3,CKPOL1                                                        
*                                                                               
CKPOLERR B     BRPOLERR                                                         
*                                                                               
CKPOL5   CLC   0(1,R6),0(R4)                                                    
         BNE   CKPOLERR                                                         
CKPOLX   B     EXIT                                                             
         EJECT                                                                  
*                                                                               
FNDUF    TM    1(R2),X'20'         FIND UNPROTECTED FIELD                       
         BCR   8,RE                                                             
*                                                                               
FNDNXUF  XR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FNDUF                                                            
         DC    H'0'                END OF SCREEN                                
*                                                                               
NEXTEL   ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0             END OF REC SET CC NOT EQ                     
         BE    NEXTELX                                                          
         CLC   ELCODE,0(R5)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTELX  LTR   R5,R5                                                            
         BR    RE                  RETURN WITH CC NOT EQ                        
*                                                                               
GROSCOM  CLC   0(0,R5),=C'GROSS'        ALLOW G-GROSS                           
NETCOM   CLC   0(0,R5),=C'NET  '        OR N-NET                                
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*===========================================================*                   
* SUBROUTINE READS ALL PRODUCT HEADERS FOR THIS CLIENT      *                   
* SKIPPING POL                                              *                   
* EXIT WITH CC NEQ WHEN NO MORE PRODUCTS                    *                   
*===========================================================*                   
         SPACE 1                                                                
FRSTPRD  XC    KEY,KEY                                                          
         MVC   KEY(4),SVKEY                                                     
*                                                                               
NEXTPRD  NTR1                                                                   
*                                                                               
NEXTPRD2 MVC   KEY+7(2),=X'FFFF'        READ NEXT PRDHDR                        
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(4),KEY      SAME CLIENT                                  
         BNE   PRDNEQX                                                          
         CLC   KEY+4(3),=C'POL'                                                 
         BE    NEXTPRD2                                                         
         XC    KEY+7(13),KEY+7                                                  
         MVC   KEY+7(1),SVKEY+7                                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(4),KEY           SAME CLIENT                             
         BNE   PRDNEQX                                                          
         CLC   KEYSAVE(9),KEY           SAME PRD/EST                            
         BE    PRDEQX                                                           
         MVC   KEY,KEYSAVE              NO - RESTORE TO LAST PRD                
         B     NEXTPRD2                                                         
PRDEQX   CR    RE,RE                                                            
         B     *+6                                                              
PRDNEQX  LTR   RE,RE                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
AUTHLEN  EQU   7                   LENGTH OF AUTH FIELD                         
DMAX     EQU   14                  MAX NUMBER OF DEMOS                          
*                                  WHEN CHANGING ALSO CHECK DEMOVAL,            
*                                  DEMOCON,SCANNER GOTO1'S                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ECTAGY   DC    C'WI'                                                            
         DC    C'WR'                                                            
         DC    C'WT'                                                            
         DC    C'WJ'               WITEST                                       
         DC    C'SJ'                                                            
         DC    C'SX'                                                            
         DC    C'FC'                                                            
         DC    C'BS'                                                            
         DC    C'TH'                                                            
         DC    X'FF'                                                            
         SPACE                                                                  
TYPTAB   DS    0C                                                               
         DC    C'M$',X'03'                                                      
         DC    C'M%',X'04'                                                      
         DC    C'Q$',X'05'                                                      
         DC    X'FF'                                                            
         SPACE                                                                  
*                                                                               
*                                                                               
         DROP  RB,R9                                                            
         EJECT                                                                  
***********************************************************************         
*         INPUT:                                                                
*             R2      = A(INPUT FIELD)                                          
*             BYTE    = ALLOWABLE LENGTH OF INPUT                               
*             HALF(1) = INPUT TYPE                                              
*         OUTPUT:                                                               
*             WORK    = DATA OR NULLS                                           
*                                                                               
EDTUSR   NTR1  BASE=*,LABEL=*                                                   
*                                  CHECK CALENDER YEAR                          
         CLI   5(R2),0             CHECK FOR ANY INPUT                          
         BNE   EDTUSR5                                                          
         TM    HALF+1,X'80'        IS INPUT REQUIRED                            
         BNO   XEDTUSR                                                          
         B     ERRMSSN1                                                         
*                                                                               
EDTUSR5  CLC   5(1,R2),BYTE        CHECK IF L'INPUT IS VALID                    
         BH    ERRLONG             NO - TOO LONG                                
*                                                                               
         CLI   HALF,C' '           IF ANY INPUT ALLOWED                         
         BNH   EDTUSR30            MOVE IT TO FIELD                             
         ZIC   R1,5(R2)                                                         
         LA    R4,8(R2)                                                         
         CLI   HALF,C'N'           IF TYPE S/B NUMERIC                          
         BNE   EDTUSR10                                                         
*                                                                               
EDTUSR6  CLI   0(R4),C'0'                                                       
         BL    EDTUSR7                                                          
         CLI   0(R4),C'9'                                                       
         BNH   EDTUSR9                                                          
*                                                                               
EDTUSR7  CLI   0(R4),C' '          ALLOW THESE DELIMITERS                       
         BE    EDTUSR9                                                          
         CLI   0(R4),C'/'                                                       
         BE    EDTUSR9                                                          
         CLI   0(R4),C'-'                                                       
         BNE   ERRINV1                                                          
*                                                                               
EDTUSR9  LA    R4,1(R4)                                                         
         BCT   R1,EDTUSR6                                                       
         B     EDTUSR30                                                         
*                                                                               
EDTUSR10 CLI   HALF,C'C'           IF TYPE ALPHABETIC                           
         BNE   EDTUSR20                                                         
*                                                                               
EDTUSR15 CLI   0(R4),C'0'          ACCEPT ANYTHING BUT NUMBERS                  
         BL    EDTUSR17                                                         
         CLI   0(R4),C'9'                                                       
         BNH   ERRINV1                                                          
*                                                                               
EDTUSR17 LA    R4,1(R4)                                                         
         BCT   R1,EDTUSR15                                                      
         B     EDTUSR30                                                         
*                                                                               
EDTUSR20 CLI   HALF,C'D'           IF TYPE IS DATE                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDATVAL,DMCB,(0,0(R4)),TEMP                                      
         OC    DMCB(4),DMCB                                                     
         BZ    DTERR1                                                           
         L     R1,0(R1)                                                         
         ZIC   R4,5(R2)                                                         
         SR    R1,R4                                                            
         BNZ   ERRINV1                                                          
*                                                                               
EDTUSR30 ZIC   R1,5(R2)            R1=L(INPUT)                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)       MOVE INPUT INTO WORK                         
*                                                                               
XEDTUSR  XIT1                                                                   
*                                                                               
ERRLONG  MVI   ERRCD,TOOLONG                                                    
         B     LFMERR1                                                          
*                                                                               
DTERR1   MVI   ERRCD,DATERR                                                     
         B     LFMERR1                                                          
*                                                                               
ERRINV1  MVI   ERRCD,INVERR                                                     
         B     LFMERR1                                                          
*                                                                               
ERRMSSN1 MVI   ERRCD,MSSNGERR                                                   
         B     LFMERR1                                                          
*                                                                               
LFMERR1  GOTO1 ERROR                                                            
         LTORG                                                                  
         SPACE                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
* CHKEDTS- FOR NETWORK CAN'T HAVE MORE THAN 12 CALENDAR MONTHS      *           
*          FOR SPOT CAN'T HAVE MORE THAN 12 BROADCAST MONTHS        *           
*            OR 53 WEEKS (WE THINK)                                 *           
*-------------------------------------------------------------------*           
*                                                                               
         DS    0D                                                               
CHKEDTS  NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK+6(6),SYR                                                    
         CLI   OVSYS,2             SPOT?                                        
         BNE   CHKEDT02                                                         
         GOTO1 VGETBRD,DMCB,(1,SYR),WORK,VGETDAY,VADDAY                         
*                                                                               
CHKEDT02 GOTO1 VDATCON,DMCB,WORK+6,(3,DUB)                                      
*                                                                               
         MVC   WORK+6(6),EYR                                                    
         CLI   OVSYS,2             SPOT?                                        
         BNE   CHKEDT06                                                         
         GOTO1 VGETBRD,DMCB,(1,EYR),WORK                                        
*                                                                               
CHKEDT06 GOTO1 VDATCON,DMCB,WORK+6,(3,DUB+3)                                    
         ZIC   R4,DUB                                                           
         ZIC   R5,DUB+3                                                         
         SR    R5,R4                                                            
         BZ    CHKEDTY             SAME YEAR IS ALWAYS OK                       
         CH    R5,=H'1'                                                         
         BH    CHKEDTN             CAN'T EXCEED ONE YEAR (SO INVALID)           
         CLC   DUB+1(2),DUB+4      NELSON SAYS TO ALLOW THIS                    
         BNH   CHKEDTN             SO LONG AS NOT MORE THAN 365 DAYS            
         CLI   OVSYS,2             SPOT?                                        
         BNE   CHKEDTY             DONE FOR NET                                 
         CLC   DUB+1(1),DUB+4      INVALID IF SAME BRD MONTH (DIF YRS)          
         BE    CHKEDTN             CAN'T HAVE 13 BRD MONTHS                     
         B     CHKEDTY                                                          
*                                                                               
CHKEDTN  CLC   =C'SJ',AGYALPHA     SJ HAS NO TALENT RECORDS                     
         BE    *+12                                                             
         BAS   RE,CHKTAL                                                        
         BE    CHKEDTY             NO RESTRIC ON DATES- TAL REC EXISTS          
         LTR   RB,RB               DATES INVALID                                
         B     CHKEDTX                                                          
*                                                                               
CHKEDTY  CR    RB,RB               DATES VALID                                  
CHKEDTX  XIT1                                                                   
         SPACE                                                                  
*----------------------------------------------------------*                    
* CHKTAL - CHECKS IF TALENT FACTOR RECORD EXISTS           *                    
*        - EX DF HAS CHILD SPOT ESTIMATES THAT CAN BE LONG *                    
*          SETS CC = IF IT EXISTS                          *                    
*----------------------------------------------------------*                    
*                                                                               
CHKTAL   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D27'                                                  
         MVC   KEY+2(1),SVAGYMD                                                 
         MVC   KEY+3(2),SVCLT                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE                                                   
         XIT1                                                                   
*                                                                               
CTYPTAB  DS    0C                                                               
         DC    C'M$',X'03'                                                      
         DC    C'M%',X'04'                                                      
         DC    C'Q$',X'05'                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
*----------------*                                                              
* LITERAL POOL   *                                                              
*----------------*                                                              
*                                                                               
         LTORG                                                                  
         SPACE                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
* CHKSTAT-                                                                      
*-------------------------------------------------------------------*           
         DS    0D                                                               
CHKSTAT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ERRCD,INVERR                                                     
         MVI   WORK,0                                                           
         LA    R2,ESTSTATH                                                      
         CLI   5(R2),0             NO INPUT                                     
         BNE   CHKS05                                                           
         OC    ELOCKYM,ELOCKYM     NO LOCK DATES                                
         BZ    CHKSYES                                                          
         XC    ELOCKYM,ELOCKYM     CLEAR THE LOCK DATES                         
         B     CHKS60                                                           
*                                                                               
CHKS05   CLC   ESTSTAT(4),=C'LOCK'                                              
         BNE   CHKS10                                                           
         CLI   SVACT,C'A'          INVALID FOR ADD                              
         BE    CHKSNO                                                           
         TM    ECNTRL,X'04'        CAN'T LOCK HELD EST                          
         BNZ   CHKSNO                                                           
         TM    ECNTRL,X'08'                                                     
         BO    CHKSYES             NO CHANGE IN STATUS                          
         OI    ECNTRL,X'08'                                                     
         OI    KEY+13,X'08'                                                     
         XC    ELOCKYM,ELOCKYM     CLEAR LOCK DATES                             
         B     CHKS60              STATUS CHANGE - NO EDITS                     
*                                                                               
CHKS10   CLC   ESTSTAT(4),=C'HOLD'                                              
         BNE   CHKS20                                                           
         CLI   SVACT,C'A'          INVALID FOR ADD                              
         BE    CHKSNO                                                           
         TM    ECNTRL,X'0C'                                                     
         BO    CHKSYES             NO CHANGE IN STATUS                          
         OI    ECNTRL,X'0C'                                                     
         OI    KEY+13,X'0C'                                                     
         XC    ELOCKYM,ELOCKYM     CLEAR LOCK DATES                             
         B     CHKS60              STATUS CHANGE - NO EDITS                     
*                                                                               
CHKS20   CLC   ESTSTAT(6),=C'UNLOCK'                                            
         BNE   CHKS30                                                           
         CLI   SVACT,C'A'          INVALID FOR ADD                              
         BE    CHKSNO                                                           
         TM    ECNTRL,X'04'        CAN'T UNLOCK HELD EST                        
         BNZ   CHKSNO                                                           
         TM    ECNTRL,X'08'                                                     
         BZ    CHKSNO              EST WASN'T LOCKED                            
         NI    ECNTRL,X'F7'                                                     
         NI    KEY+13,X'F7'                                                     
         B     CHKS60              STATUS CHANGE - NO EDITS                     
*                                                                               
CHKS30   CLC   AGYALPHA,=C'JW'     FOR JWT, DO NOT ALLOW REL, ONLY DBT          
         BNE   CHKS40                                                           
         CLC   ESTSTAT(3),=C'DBT'                                               
         BE    CHKS50                                                           
         B     CHKS55                                                           
*                                                                               
CHKS40   CLC   ESTSTAT(3),=C'REL'  RELEASE FMT = RELNN                          
         BNE   CHKS55              NN=TODAY'S DAY                               
*                                                                               
CHKS50   CLI   SVACT,C'A'          INVALID FOR ADD                              
         BE    CHKSNO                                                           
         CLI   5(R2),5                                                          
         BNE   CHKSNO                                                           
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)                                      
         CLC   WORK+4(2),11(R2)    DAYS MUST MATCH                              
         BNE   CHKSNO                                                           
         NI    ECNTRL,X'F3'                                                     
         NI    KEY+13,X'F3'                                                     
         B     CHKS60              STATUS CHANGE - NO EDITS                     
         EJECT                                                                  
*                                                                               
CHKS55   DS    0H              CHECK IF FORMAT IS M/Y, M/Y-, OR M/Y+            
         MVI   WORK,0          USE FIRST BIT X'80'="-" OR X'40'="+"             
         LA    R1,WORK+1       FIND AND REMOVE + OR - AFTER DATE                
         LR    R4,R1                                                            
         MVC   WORK+1(L'ESTSTAT),ESTSTAT                                        
         LA    R3,L'ESTSTAT                                                     
CHKS55A  CLI   0(R1),C'-'                                                       
         BNE   *+12                                                             
         MVI   WORK,X'80'      SAVE BIT TO SHOW IT IS PRIOR                     
         B     CHKS55C                                                          
         CLI   0(R1),C'+'                                                       
         BNE   *+12                                                             
         MVI   WORK,X'40'      SAVE BIT TO SHOW IT IS SUBSEQ                    
         B     CHKS55C                                                          
         CLI   0(R1),C' '                                                       
         BE    CHKS55F                                                          
         CLI   0(R1),X'0'                                                       
         BE    CHKS55F                                                          
         CLI   0(R1),C'/'                                                       
         BE    CHKS55B                                                          
         CLI   0(R1),C'A'                                                       
         BL    CHKS80          NOT A VALID DATE                                 
         CLI   0(R1),C'9'                                                       
         BH    CHKS80          NOT A VALID DATE                                 
CHKS55B  LA    R1,1(R1)                                                         
         BCT   R3,CHKS55A                                                       
         B     CHKS55F         A + OR - WAS NOT FOUND                           
*                                                                               
CHKS55C  MVI   0(R1),C' '      REPLACE + OR - WITH A BLANK FOR DATVAL           
*                                                                               
CHKS55F  DS    0H                                                               
         GOTO1 VDATVAL,DMCB,(2,WORK+1),WORK+20                                  
         OC    DMCB(4),DMCB                                                     
         BZ    CHKS80                                                           
         TM    ECNTRL,X'04'        CAN'T USE DATE IF HELD ESTIMATE              
         BNZ   CHKSNO                                                           
         TM    ECNTRL,X'08'        CAN'T USE DATE IF LOCKED ESTIMATE            
         BNZ   CHKSNO                                                           
*                                                                               
* CHECK IF MONTH IS IN ESTIMATE PERIOD                                          
         MVI   ERRCD,NOTINEST                                                   
         CLC   WORK+20(4),ESTART                                                
         BL    CHKSNO                                                           
         CLC   WORK+20(4),EEND                                                  
         BH    CHKSNO                                                           
         MVI   ERRCD,INVERR                                                     
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK+20),(3,WORK+30)                             
         OC    WORK+31(1),WORK     SET AS PRIOR OR SUBSEQ                       
         CLC   ELOCKYM,WORK+30     CHECK IF ALREADY IN RECORD                   
         BE    CHKSYES             YES, THEN NO CHANGE IN STATUS                
         MVC   ELOCKYM,WORK+30     ADD YM LOCK DATE TO RECORD                   
         OC    ELOCKMON,WORK       SET AS PRIOR OR SUBSEQ                       
*                                                                               
CHKS60   GOTO1 PUTREC                                                           
         GOTO1 CNCHASPT                                                         
         MVC   COMMAND,=C'DMREAD'                                               
         GOTO1 DIR                                                              
         MVC   KEY+13(1),ECNTRL                                                 
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   CHKS70              NO                                           
         CLI   SVEBCMED,C'T'       TV                                           
         BNE   CHKS70              NO                                           
*                                  NEED TO WRITE BACK NWRK AND COMB             
*                                  POINTERS                                     
         MVC   KEY,SVKEY                                                        
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'         NWRK                                         
         MVC   COMMAND,=C'DMREAD'                                               
         GOTO1 DIR                                                              
         MVC   KEY+13(1),ECNTRL                                                 
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         MVC   KEY,SVKEY                                                        
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'         COMB                                         
         MVC   COMMAND,=C'DMREAD'                                               
         GOTO1 DIR                                                              
         MVC   KEY+13(1),ECNTRL                                                 
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
*                                                                               
CHKS70   MVC   KEY,SVKEY                                                        
         MVI   WORK,C'S'           SET FOR STATUS CHG                           
         B     CHKSYES             SO I WON'T REFORMAT REC                      
*                                                                               
CHKS80   GOTO1 =A(CKLINID),RR=RELO    TEST TERMINAL AUTH                        
         BE    CHKS90              ALLOW CHG OF OLD-NEW                         
         CLI   SVACT,C'A'          ELSE MUST BE ADD                             
         BNE   CHKSNO                                                           
*                                                                               
CHKS90   NI    EPRDCD,X'7F'        SET OFF X'80'                                
         CLC   ESTSTAT(3),=C'OLD'                                               
         BE    CHKSYES                                                          
         CLC   ESTSTAT(3),=C'NEW'                                               
         BNE   CHKSNO                                                           
         CLI   SVEBCMED,C'N'       ONLY FOR NETWORK                             
         BNE   CHKSNO                                                           
         OI    EPRDCD,X'80'                                                     
*                                                                               
CHKSYES  SR    RC,RC                                                            
CHKSNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*===============================================================*               
*        CHECK MASTER TERMINAL/LINE ID                          *               
*===============================================================*               
         SPACE 1                                                                
CKLINID  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,LINIDTAB              MASTER TERMINAL LINE ID +ADDR           
*                                                                               
CKL10    CLI   0(R1),X'FF'                                                      
         BE    CKLNO                                                            
         CLC   LINID(8),0(R1)                                                   
         BE    CKLYES              ALLOW CHG OF OLD-NEW                         
         LA    R1,8(R1)                                                         
         B     CKL10                                                            
*                                                                               
CKLYES   SR    R1,R1                                                            
CKLNO    LTR   R1,R1                                                            
*                                                                               
XIT1     XIT1                                                                   
         SPACE                                                                  
*                                                                               
LINIDTAB DS    0H                                                               
         DC    C'DD13C2D1'                                                      
         DC    C'DDL1136T'          DDS-LA                                      
         DC    C'DDL1137T'          DDS-LA                                      
         DC    C'DDL1138T'          DDS-LA                                      
         DC    C'DX06200T'         (WAS DDNY720T)                               
         DC    C'DDNY700T'                                                      
         DC    C'DDNYD03T'                                                      
*NOP*    DC    C'DX03901T'                                                      
         DC    C'DDNY916T'                                                      
*NOP*    DC    C'DDNY720T'                                                      
*NOP*    DC    C'DDNYF11T'          DDS                                         
         DC    C'HDTO847T'          HDTO (WAS HDTO823T)                         
         DC    C'HDTO829T'          HDTO (WAS HDTO830T)                         
         DC    C'XDDSC84A'                                                      
*NOP*    DC    C'DDNYD26T'                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*====================== VALIDATE PROFILE FIELD =======================*         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
VALPROF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    CASHPRD,CASHPRD                                                  
         XC    OLDCASH,OLDCASH                                                  
         MVI   OLDCPRD,0                                                        
*                                                                               
         CLI   SCRNFLAG,0          DID EVERYTHING FIT ON DISPLAY?               
         BE    VPRF00              YES - SO CONTINUE                            
*                                                                               
         MVC   ESTOPT,SPACES       ELSE - BLANK OUT THE FIELD                   
         FOUT  ESTOPTH,=C'OPTIONS CANNOT BE CHANGED - CONTACT DDS',39           
         B     VPRFXIT             AND SKIP THE OPTIONS                         
*                                                                               
VPRF00   EQU   *                                                                
*                                                                               
         LA    R2,ESTOPTH          PROFILE                                      
         MVC   EDAILY,SVCLDLY      SET CLT DEFAULT                              
         TM    SVCLOP1,COP1NMG     COPY NMG FROM CLIENT                         
         BNO   *+8                                                              
         OI    EFLAG1,EF1NMG                                                    
*                                                                               
         XC    ECOST2,ECOST2       CLEAR THE COST FACTOR                        
         MVC   OLDPWPCT,EPWPCT     SAVE ANY % BEFORE CHANGES                    
         XC    EPWPCT,EPWPCT                                                    
         CLI   SVACT,C'A'                                                       
         BNE   VPRF05                                                           
         MVC   EPWPCT,SVCLTPW      POL DEFAULTS TO CLIENT PW%                   
         CLC   =C'POL',SVEBCPRD                                                 
         BE    VPRF05                                                           
         MVC   EPWPCT,SVPOLPW      BRAND JUST GETS POL PW%                      
*                                                                               
VPRF05   CLI   5(R2),0                                                          
         BE    VPRFX                                                            
         OC    ECPPEST,ECPPEST                                                  
         BNZ   VPERR01                                                          
         GOTO1 VSCANNER,DMCB,(R2),(10,REC2)                                     
         CLI   DMCB+4,0                                                         
         BE    VPERR01                                                          
         LA    R5,REC2                                                          
         USING SCAND,R5                                                         
         ZIC   R0,DMCB+4           N'ENTRIES                                    
         XC    ECGTPCT,ECGTPCT     CLEAR CGT                                    
*                                                                               
VPRF10   ZIC   R1,FLD1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD1(0),=C'DAILY'                                                
         BNE   VPRF20                                                           
         MVC   EDAILY,FLD2                                                      
         CLI   EDAILY,C'N'                                                      
         BE    VPRF100                                                          
         CLI   EDAILY,C'Y'         IF DAILY ESTIMATE                            
         BNE   VPERR01                                                          
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF         1ST CHECK THAT NOT MORE THAN 53 DAYS         
         GOTO1 CPERVERT,DMCB,ESTART,EEND,WORK,WORK+4,WORK+8                     
         LH    R4,8(R1)                                                         
         CH    R4,=H'53'           ONLY ALLOW DAILY FOR MAX OF 53 DAYS          
         BH    VPERR02                                                          
         B     VPRF100                                                          
*                                                                               
VPRF20   CLI   FLD1LEN,2           THIS KEYWORD MUST BE 2 CHARS LONG            
         BNE   VPRF30                                                           
         CLC   FLD1(2),=C'PW'                                                   
         BNE   VPRF30                                                           
         OC    SVCLTPW,SVCLTPW     TEST CLIENT IS A PW CLIENT                   
         BZ    VPERR01             NO - DO NOT ALLOW PW ON EST                  
         ZIC   R4,FLD2LEN                                                       
         GOTO1 VCASHVAL,DMCB,(2,FLD2),(R4)                                      
         CLI   DMCB,X'00'                                                       
         BNE   VPERR01                                                          
         MVC   EPWPCT,DMCB+5                                                    
         OC    EPWPCT,EPWPCT                                                    
         BNZ   VPRF100                                                          
         OI    EPWPCT,X'80'        TURN ON HIGH ORDER BIT                       
         B     VPRF100                                                          
*                                                                               
VPRF30   DS    0H                                                               
         CLC   FLD1(3),=C'REQ'                                                  
         BNE   VPRF35                                                           
         CLC   SVEBCPRD,=C'POL'                                                 
         BNE   VPERR03             ONLY FOR POL ESTIMATES                       
         NI    EFLAG1,X'FF'-EF1REQ                                              
         CLI   FLD2,C'N'                                                        
         BE    VPRF100                                                          
         CLI   FLD2,C'Y'                                                        
         BNE   VPERR01                                                          
         OI    EFLAG1,EF1REQ                                                    
         B     VPRF100                                                          
*                                                                               
VPRF35   DS    0H                                                               
         CLC   FLD1(3),=C'NMG'                                                  
         BNE   VPRF36                                                           
         OI    EFLAG1,EF1NMG                                                    
         CLI   FLD2,C'Y'                                                        
         BE    VPRF100                                                          
         CLI   T219FFD+1,C'*'      DDS TERMINAL                                 
         BNE   VPERR01                                                          
         CLI   FLD2,C'N'                                                        
         BNE   VPERR01                                                          
         NI    EFLAG1,X'FF'-EF1NMG                                              
         B     VPRF100                                                          
*                                                                               
VPRF36   DS    0H                                                               
         CLC   FLD1(3),=C'CGT'                                                  
         BNE   VPRF37                                                           
         ZIC   R4,FLD2LEN                                                       
         GOTO1 VCASHVAL,DMCB,(2,FLD2),(R4)                                      
         CLI   DMCB,X'00'                                                       
         BNE   VPERR01                                                          
         CLC   DMCB+4(4),=F'9999'                                               
         BH    VPERR01                                                          
         MVC   ECGTPCT,DMCB+6                                                   
         B     VPRF100                                                          
*                                                                               
VPRF37   DS    0H                                                               
         CLC   FLD1(5),=C'DEMOS'                                                
         BNE   VPRF38                                                           
         NI    EFLAG1,X'FF'-EF1NODEM                                            
         CLI   FLD2,C'Y'                                                        
         BE    VPRF100                                                          
         CLI   FLD2,C'N'                                                        
         BNE   VPERR01                                                          
         OI    EFLAG1,EF1NODEM                                                  
         B     VPRF100                                                          
*                                                                               
VPRF38   CLC   FLD1(4),=C'OWPW'                                                 
         BNE   VPRF39                                                           
         OI    EFLAG1,EF1OOWPW                                                  
         CLC   FLD2(2),=C'TG'      LET TRACY GLASS TURN IT OFF                  
         BNE   *+8                                                              
         NI    EFLAG1,X'FF'-EF1OOWPW                                            
         B     VPRF100                                                          
*                                                                               
VPRF39   EQU   *                                                                
*                                                                               
         CLC   FLD1(4),=C'COS2'    COST FACTOR?                                 
         BNE   VPRF40              NO - SO CONTINUE                             
*                                                                               
         ZIC   R4,FLD2LEN                                                       
         GOTO1 VCASHVAL,DMCB,(6,FLD2),(R4)                                      
         CLI   DMCB,0                                                           
         BNE   VPERR01                                                          
         L     R3,4(R1)                                                         
         C     R3,=F'9999999'      MAX 9.999999                                 
         BH    VPERR01                                                          
*                                                                               
         C     R3,=F'0'            .LT. 0?                                      
         BL    VPERR01             YES - SO ERROR                               
*                                                                               
         MVC   ECOST2,DMCB+4                                                    
         OC    ECOST2,ECOST2       ZERO?                                        
         BNZ   VPRF100             NO - SO CONTINUE                             
*                                                                               
         OI    ECOST2,X'80'        ELSE - SET 'ZERO WAS INPUT' BIT              
*                                                                               
         B     VPRF100             AND CONTINUE                                 
*                                                                               
VPRF40   DS    0H                                                               
*                                                                               
         CLC   FLD1(3),=C'SLN'     SPOT LENGTH?                                 
         BNE   VPRF45              NO - SO CONTINUE                             
*                                                                               
         CLC   =C'DEL',FLD2        IF SLN=DEL, REMOVE RESTRICTION               
         BNE   VPRF41                                                           
         MVI   ESLN,0                                                           
         B     VPRF100                                                          
*                                                                               
VPRF41   LA    R3,SLNTABC          CHECK IF A VALID SPOT LENGTH                 
         ICM   R4,15,FLD2B         R4=INPUT (SPOT LEN)                          
VPRF42   CLI   0(R3),X'FF'                                                      
         BE    VPERR07             LENGTH NOT IN TABLE                          
         ZIC   R1,0(R3)                                                         
         CR    R1,R4                                                            
         BE    VPRF43                                                           
         LA    R3,1(R3)                                                         
         B     VPRF42                                                           
*                                                                               
VPRF43   STC   R4,ESLN             STORE SPOT LENGTH                            
         B     VPRF100             AND CONTINUE                                 
*                                                                               
VPRF45   CLC   FLD1(4),=C'CASH'    CASH PRODUCT?                                
         BNE   VPRF50              NO - SO CONTINUE                             
         CLI   FLD2LEN,3                                                        
         BH    VPERR01             INVALID                                      
         CLI   FLD2+2,C'#' CAN'T BE TRADE PRODUCT                               
         BE    VPERR13                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(8),SVKEY                                                     
         MVC   KEY+4(3),FLD2       VALID CASH PRODUCT ESTIMATE?                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY                                                   
         BNE   VPERR10             CASH EST NOT ON FILE                         
*                                                                               
         LA    R7,REC2+500         DON'T MESS UP SCANNER OUTPUT AT REC2         
         ST    R7,AREC                                                          
         GOTO1 GETREC                       GET CASH ESTIMATE AND               
         MVC   SVTRDPRD,ETRDPRD-ESTHDR(R7)  SAVE EXISTING TRD PRD               
*                                                                               
         MVC   KEY+14,SVCLTDA      GET CLT REC                                  
         GOTO1 GETREC                                                           
         LA    R7,CLIST-CLTHDR(R7) FIND CASH PRD                                
VPRF46   CLI   0(R7),0                                                          
         BE    VPERR01                                                          
         CLC   FLD2(3),0(R7)       MATCH ON CASH PRD ALPHA                      
         BE    VPRF47                                                           
         LA    R7,4(R7)                                                         
         B     VPRF46                                                           
*                                                                               
VPRF47   CLI   SVTRDPRD,0          ALREADY HAS A TRADE PRD                      
         BE    *+14                                                             
         CLC   SVTRDPRD,SVPRD      TRADE PRD IN CASH EST = THIS PRD             
         BNE   VPERR11             CASH EST ALREADY HAS DIF TRD PRD             
*                                                                               
         CLC   ECASHPRD,3(R7)      CHANGING CASH PRD                            
         BE    VPRF48                                                           
         CLI   ECASHPRD,0                                                       
         BE    VPRF48                                                           
         MVC   OLDCPRD,ECASHPRD    SAVE TO REMOVE TRD FROM OLD CASH REC         
         CLI   SONIA,C'C'          ALLOW CHANGES                                
         BNE   VPERR12             CAN'T CHANGE CASH PRD                        
*                                                                               
VPRF48   MVC   ECASHPRD,3(R7)      SET CASH PRODUCT CODE ON TRADE EST           
         CLI   SVTRDPRD,0          ALREADY HAS CORRECT TRADE PRD                
         BNE   *+10                                                             
         MVC   CASHPRD,FLD2        IF SET THEN NEED TO UPDATE CASH EST          
*                                                                               
         CLI   OLDCPRD,0           NEED TO DELETE TRD FROM OLD CASH             
         BE    VPRF49                                                           
         LA    R7,REC2+500         DON'T MESS UP SCANNER OUTPUT AT REC2         
         LA    R7,CLIST-CLTHDR(R7) FIND CASH PRD                                
VPRF48A  CLI   0(R7),0                                                          
         BE    VPERR01                                                          
         CLC   OLDCPRD,3(R7)       MATCH ON CASH PRD ALPHA                      
         BE    VPRF48B                                                          
         LA    R7,4(R7)                                                         
         B     VPRF48A                                                          
VPRF48B  MVC   OLDCASH,0(R7)                                                    
*                                                                               
VPRF49   MVC   KEY,SVKEY           RESET                                        
         GOTO1 HIGH                                                             
         B     VPRF100             AND CONTINUE                                 
*                                                                               
VPRF50   CLC   FLD1(3),=C'TRD'     TRADE PRD- NO VALIDATION- DISP ONLY          
         BNE   VPRF60                                                           
         B     VPRF100             AND CONTINUE                                 
*                                                                               
VPRF60   DS    0H                                                               
         B     VPERR01                                                          
*                                                                               
VPRF100  LA    R5,32(R5)           NEXT ENTRY                                   
         BCT   R0,VPRF10                                                        
         DROP  R5                                                               
*                                                                               
VPRFX    OC    SVCLTPW,SVCLTPW     TEST PW CLIENT                               
         BZ    VPRFX6                                                           
*                                                                               
         CLC   =C'WI',AGYALPHA                                                  
         BNE   VPRFX2                                                           
         CLC   EEND,=C'950326'     EST END BEFORE BRDCST APR/95                 
         BH    VPRFX2              NO                                           
         XC    EPWPCT,EPWPCT       IF SO, NO PW                                 
         B     VPRFX10                                                          
*                                                                               
VPRFX2   OC    EPWPCT,EPWPCT       TEST PW PCT ENTERED                          
         BZ    VPERR04             NO - ERROR                                   
         CLC   =C'POL',SVEBCPRD    TEST POL ESTIMATE                            
         BNE   VPRFX4              NO                                           
         MVI   POLCHG,1            SET FLAG TO CHANGE ALL BRANDS                
         MVC   SVPOLPW,EPWPCT      AND SET NEW VALUE FOR POL                    
         B     VPRFX6                                                           
*                                                                               
VPRFX4   CLC   SVPOLPW,EPWPCT      TEST BRAND PW = POL PW                       
         BNE   VPERR05                                                          
*                                                                               
VPRFX6   OC    EPWPCT,EPWPCT       TEST PW PCT ENTERED                          
         BZ    VPRFX8              NO - EXIT                                    
         GOTO1 VADDAY,DMCB,SYR,WORK,98      CAN'T HAVE PW ON EST>14WKS          
         CLC   WORK(6),EYR                                                      
         BH    VPRFX10             OK                                           
         MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(INVPW) CAN'T HAVE PW ON EST>14WKS                    
         B     VPERRX                                                           
*                                                                               
VPRFX8   CLC   =C'POL',SVEBCPRD    IF NO PW PERCENT ON POL                      
         BNE   VPRFX9                                                           
         OC    OLDPWPCT,OLDPWPCT   BUT THERE USED TO BE ONE                     
         BZ    VPRFX10                                                          
         MVI   POLCHG,1            SET FLAG TO REMOVE FROM ALL BRANDS           
         MVC   SVPOLPW,EPWPCT      AND SET NEW VALUE FOR POL                    
         B     VPRFX10             OK                                           
*                                  *NOT POL - REMOVING PW                       
VPRFX9   OC    OLDPWPCT,OLDPWPCT   BUT THERE USED TO BE ONE                     
         BZ    VPRFX10                                                          
         CLI   POLSW,1             AND POL EST EXISTS                           
         BNE   VPRFX10                                                          
         B     VPERR06             THEN MUST REMOVE FROM POL EST                
*                                                                               
VPRFX10  TM    SVAGYFL1,AGYCOS2Q   COST FACTOR REQUIRED?                        
         BZ    VPRFX11             NO - SO CONTINUE                             
         OC    ECOST2,ECOST2       ELSE - ANY COST FACTOR ENTERED?              
         BNZ   VPRFX11             YES - SO CONTINUE                            
         MVC   ECOST2,SVCCOST2     ELSE - MOVE IN THE CLT COST FACTOR           
*                                                                               
VPRFX11  CLI   SVEBCPRD+2,C'#'     TRADE PRODUCT                                
         BE    VPRFX11A                                                         
         CLI   ECASHPRD,0          NOT TRADE-WAS A CASH PROD SPECIFIED?         
         BNE   VPERR08             ERROR- NO CASH PRD FOR NON TRADE             
         B     VPRFX12                                                          
VPRFX11A CLI   ECASHPRD,0          TRADE - CASH PRD REQ'D                       
         BE    VPERR09             ERROR- CASH PRD REQ'D FOR TRADE PRD          
*                                                                               
VPRFX12  EQU   *                                                                
         MVI   COS2FLAG,0                                                       
         CLC   ECOST2,SVCOS2       LEAVE FLAG NULLS IF COST2 UNCHGED            
         BE    VPRFXIT                                                          
         CLI   SVACT,C'A'          ACTION ADD - NO NEED TO ADD C3 REQ           
         BE    VPRFXIT                                                          
         MVI   COS2FLAG,1          FLAG CHANGE IN COS2 - ADD C3 REQ             
*                                                                               
VPRFXIT  SR    RA,RA                                                            
         XIT1                                                                   
*                                                                               
VPERR01  MVI   ERRCD,INVERR                                                     
         B     VPERRX                                                           
*                                                                               
VPERR02  MVI   ERRCD,NEWERR        DAILY ERR                                    
         MVC   NERRCD,=AL2(DAILYERR)                                            
         B     VPERRX                                                           
*                                                                               
VPERR03  MVI   ERRCD,NEWERR        REQ ERROR                                    
         MVC   NERRCD,=AL2(POLRQERR)                                            
         B     VPERRX                                                           
*                                                                               
VPERR04  MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(NOPWPCT)  PW PCT REQUIRED                            
         B     VPERRX                                                           
*                                                                               
VPERR05  MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(NOTPOLPW)  BRAND PW SHOULD EQUAL POL                 
         B     VPERRX                                                           
*                                                                               
VPERR06  MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(DELPWPOL)  MUST DELETE PW% FROM POL EST              
         B     VPERRX                                                           
*                                                                               
VPERR07  MVI   ERRCD,SPTLNERR                                                   
         B     VPERRX                                                           
*                                                                               
VPERR08  MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(CASHINV)   CASH OPTION INV FOR NON TRADE             
         B     VPERRX                                                           
*                                                                               
VPERR09  MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(CASHREQ)   CASH OPTION REQ'D FOR TRADE PRDS          
         B     VPERRX                                                           
*                                                                               
VPERR10  MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(NOCSHEST)  NO CASH ESTIMATE                          
         B     VPERRX                                                           
*                                                                               
VPERR11  MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(CASHSET)   CASH EST ALREADY HAS TRD PRD              
         B     VPERRX                                                           
*                                                                               
VPERR12  MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(CASHCHG)   CAN NOT CHANGE CASH PROD                  
         B     VPERRX                                                           
*                                                                               
VPERR13  MVI   ERRCD,NEWERR                                                     
         MVC   NERRCD,=AL2(CASHTRD)   CASH PRD CAN NOT BE TRADE                 
         B     VPERRX                                                           
*                                                                               
VPERRX   GOTO1 ERROR                                                            
         EJECT                                                                  
*                                                                               
*------------------------ CONSTANTS & LTORG -------------------------*          
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
SLNTABC  DC    AL1(10,15,20,30,40,45,50,60,75,90,120,5)                         
         DC    X'FF'                     MARK END OF TABLE                      
         SPACE 2                                                                
         DROP  R8,RA,RB,RC                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPLFMWRK                                                       
         ORG   LFMTABH                                                          
*SPLFMF3                                                                        
       ++INCLUDE SPLFMF3D                                                       
         EJECT                                                                  
SCAND    DSECT                                                                  
*         DSECT TO COVER SCANNER LINES                                          
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
*                                                                               
BSCAND   DSECT                     SCANNER LIKE OUTPUT                          
BFLD1LEN DS    CL1                                                              
BFLD2LEN DS    CL1                                                              
BFLD1VAL DS    CL1                                                              
BFLD2VAL DS    CL1                                                              
BFLD1B   DS    CL4                                                              
BFLD2B   DS    CL4                                                              
BFLD1    DS    CL11                EXCEPT FLD 1 = LEN 11                        
BFLD2    DS    CL10                                                             
BSCANLNQ EQU   *-BSCAND                                                         
*                                                                               
*                                                                               
T219FFD  DSECT                                                                  
         ORG   SVAPPL                                                           
RELO     DS    A                                                                
SYR      DS    CL2                                                              
SMN      DS    CL2                                                              
SDY      DS    CL2                                                              
EYR      DS    CL2                                                              
EMN      DS    CL2                                                              
EDY      DS    CL2                                                              
*                                                                               
MAKEGDDT DS    CL2                                                              
OWSDAY   DS    CL1                                                              
LINID    DS    CL4                 LINE ID FROM FAFACTS                         
LINADDR  DS    CL4                 LINE ADDR FROM FAFACTS                       
OVSYS    DS    CL1                 2=SPOT,3=NET                                 
TEMP     DS    CL6                                                              
OLDPWPCT DS    XL3                 PW % BEFORE CHANGE TO REC                    
*                                                                               
WTSW     DS    CL1                                                              
UDSW     DS    CL1                 SET TO X'01' IF USER DEMOS USED              
HMSW     DS    CL1                 SET TO X'01' IF TOTAL HMS INPUT              
NHMSW    DS    CL1                 SET TO X'01' IF A DEMO OTHER THAN            
*                                  64 OR 3 IS INPUT                             
POLCHG   DS    CL1                 SET TO X'01' ON POL BOOK OR HUT CHG          
*                                                                               
SVTRDPRD DS    XL1                 TRADE PRD NUMBER                             
CASHPRD  DS    CL3                 CASH PRODUCT ALPHA                           
OLDCASH  DS    CL3                 OLD CASH PRODUCT ALPHA                       
OLDCPRD  DS    XL1                 OLD CASH PRODUCT NUMBER                      
SONIA    DS    CL1                                                              
*                                                                               
SVDEMOS  DS    0CL124                                                           
SVDEMLST DS    CL60                                                             
SVWGTLST DS    XL20                                                             
SVUSRNMS DS    CL28                                                             
SVWGTNM  DS    CL7                                                              
         DS    CL9                                                              
         DS    CL1                (EOWSDAY IN EDEMOS+124)                       
         DS    CL1                (ERATE NOW IN EDEMOS+125)                     
SVBOOK   DS    CL2                                                              
SVHUT    DS    CL1                                                              
SVDPT    DS    CL1                                                              
SVFLTRS  DS    CL3                 FILTERS                                      
SVECON   DS    CL1                 ECONTROL                                     
SVRTL    DS    CL2                 RETAIL SCHEME                                
SVNETYM  DS    XL2                                                              
SVBDSTM  DS    XL3                                                              
SVBDENDM DS    XL3                                                              
SVCOS2   DS    XL4                 BACKUP ECOST2                                
COS2FLAG DS    X                                                                
*                                                                               
POLDATA  DS    0D                                                               
POLDEMOS DS    0CL124                                                           
PEDEMLST DS    CL60                CONVERTED DEMO FIELDS                        
PEWGTLST DS    XL20                                                             
PEUSRNMS DS    CL28                                                             
PEWGTNM  DS    CL7                                                              
         DS    CL9                                                              
         DS    CL1                (EOWSDAY IN EDEMOS+124)                       
         DS    CL1                (ERATE NOW IN EDEMOS+125)                     
*                                                                               
POLBOOK  DS    CL2                                                              
POLHUT   DS    CL1                                                              
POLDPT   DS    CL1                                                              
POLSW    DS    CL1                 SET TO X'01' IF POL HDR EXISTS               
*                                  SET TO X'02' IF CHGING POL                   
POLDATE  DS    CL12                FOR CPP POL ESTS                             
POLTYPE  DS    CL1                 FOR CPP POL ESTS                             
POLFLTRS DS    CL3                                                              
POLECON  DS    CL1                 ECONTROL   NEW 3/13/89                       
POLRTL   DS    CL2                 RETAIL SCHEME NEW 9/18/92                    
*                                                                               
POLDATAX EQU   *                                                                
*                                                                               
SCRNFLAG DS    X                   'DID ALL OPTIONS FIT ON SCREEN' FLAG         
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
*                                                                               
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
        EJECT                                                                   
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPARSNIPD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'115SPLFM13   05/01/02'                                      
         END                                                                    
