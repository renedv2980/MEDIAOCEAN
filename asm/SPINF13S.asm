*          DATA SET SPINF13S   AT LEVEL 019 AS OF 05/01/02                      
*PHASE T21A13A                                                                  
         TITLE 'T21A13 - SPOTPAK INFO ESTIMATE DISPLAY'                         
T21A13   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (ESTWRKX-ESTWRKD),T21A13,R8,RR=RE                                
         LR    R3,RC                                                            
         USING ESTWRKD,R3                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
         USING FLDHDRD,R2                                                       
         ST    RE,RELO                                                          
         XC    WORK,WORK                                                        
         XC    SWS(SWSLEN),SWS     CLEAR SWITCHES                               
         LA    R5,REC              SET RECORD ADDRESS                           
         ST    R5,AREC                                                          
*                                                                               
         MVI   SVSUBSC,0           CLEAR SUBSCREEN                              
         MVI   SVDATOPT,0                                                       
         BAS   RE,VALFLTR          VALIDATE FILTERS INPUTTED                    
         BNE   PRFLTERR            DISPLAY ERROR MESSAGE                        
*                                                                               
         GOTO1 USER1,DUB,(64,SINIFLT),(8,=C'DISPLAY=')                          
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKREQ                                                           
         L     R4,4(R1)                                                         
         LR    R6,R4                                                            
         LA    RE,8                                                             
         LA    R4,8(R4)                                                         
         CLI   0(R4),C'L'                                                       
         BNE   *+8                                                              
         MVI   LOCKSW,1                                                         
         CLI   0(R4),C'H'                                                       
         BNE   *+8                                                              
         MVI   HOLDSW,1                                                         
         OC    LOCKSW(2),LOCKSW                                                 
         BNZ   CHKREQ                                                           
         B     FLTERR                                                           
         EJECT                                                                  
CHKREQ   MVI   REQSW,0                                                          
         GOTO1 USER1,DUB,(64,SINIFLT),(4,=C'REQ=')                              
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKNMG                                                           
         LA    RE,4                                                             
         L     R4,4(R1)            SET FIELD POINTER                            
         LR    R6,R4                                                            
         CLC   SVEBCPRD,=C'POL'                                                 
         BNE   FLTERR              ONLY FOR PRODUCT POL                         
         LA    R4,4(R4)                                                         
         CLI   0(R4),C'Y'                                                       
         BE    CHKR10                                                           
         CLI   0(R4),C'N'                                                       
         BNE   FLTERR                                                           
*                                                                               
CHKR10   MVC   REQSW,0(R4)                                                      
*                                                                               
CHKNMG   MVI   NMGSW,0                                                          
         GOTO1 USER1,DUB,(64,SINIFLT),(4,=C'NMG=')                              
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKNMGX                                                          
         LA    RE,4                                                             
         L     R4,4(R1)            SET FIELD POINTER                            
         LR    R6,R4                                                            
         LA    R4,4(R4)                                                         
         CLI   0(R4),C'Y'                                                       
         BE    CHKNMG2                                                          
         CLI   0(R4),C'N'                                                       
         BNE   FLTERR                                                           
*                                                                               
CHKNMG2  MVC   NMGSW,0(R4)                                                      
*                                                                               
CHKNMGX  DS    0C                                                               
*                                                                               
CHKPW    MVI   PWSW,0                                                           
         GOTO1 USER1,DUB,(64,SINIFLT),(3,=C'PW=')                               
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKPWX                                                           
         LA    RE,4                                                             
         L     R4,4(R1)            SET FIELD POINTER                            
         LR    R6,R4                                                            
         LA    R4,3(R4)                                                         
         CLI   0(R4),C'Y'                                                       
         BE    CHKPW2                                                           
         CLI   0(R4),C'N'                                                       
         BNE   FLTERR                                                           
*                                                                               
CHKPW2   MVC   PWSW,0(R4)                                                       
*                                                                               
CHKPWX   DS    0C                                                               
*                                                                               
CHKEST   GOTO1 USER1,DUB,(64,SINIFLT),(4,=C'EST=')     GETFLTR                  
         OC    4(4,R1),4(R1)                                                    
         BNZ   CNVEST                                                           
         MVC   EST1(2),=X'01FF'                                                 
         XC    SDATE,SDATE                                                      
         B     CHKDATE                                                          
         EJECT                                                                  
CNVEST   XC    EST1(2),EST1                                                     
         XC    SDATE,SDATE                                                      
         L     R4,4(R1)            SET FIELD POINTER                            
         LA    R4,4(R4)                                                         
         CLC   0(2,R4),=C'NO'                                                   
         BNE   CE1                                                              
         MVC   EST1(2),=X'01FF'                                                 
         B     CHKDATE                                                          
*                                                                               
CE1      BAS   R9,GETNUM                                                        
         L     R6,4(R1)                                                         
         LA    RE,4                                                             
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         L     R4,4(R1)            CONVERT ESTIMATE 1                           
         LA    R4,4(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)       * EXECUTED *                                   
         CVB   RF,DUB                                                           
         STC   RF,EST1                                                          
         STC   RF,EST2                                                          
         LA    R4,1(R5,R4)                                                      
         CLI   0(R4),C'-'          ESTIMATE SERIES                              
         BNE   CHKDATE              NO - CHECK DATA FORMAT                      
         LA    R4,1(R4)                                                         
         LR    RF,R4                                                            
         BAS   R9,GETNUM                                                        
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)       * EXECUTED *                                   
         CVB   RF,DUB                                                           
         STC   RF,EST2                                                          
         CLI   EST1,0                                                           
         BE    FLTERR                                                           
         CLI   EST2,0                                                           
         BE    CHKDATE                                                          
         CLC   EST1,EST2                                                        
         BH    FLTERR                                                           
         EJECT                                                                  
CHKDATE  GOTO1 USER1,DUB,(64,SINIFLT),(5,=C'DATE=')    GETFLTR                  
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKFLT                                                           
         L     R6,4(R1)                                                         
         LR    R4,R6                                                            
         LA    R4,5(R4)                                                         
         GOTO1 VDATVAL,DMCB,(R4),SDATE                                          
         LA    RE,5                                                             
         CLI   DMCB+3,0                                                         
         BE    FLTERR                                                           
         IC    RE,DMCB+3                                                        
         LA    R4,1(RE,R4)                                                      
         GOTO1 VDATVAL,DMCB,(R4),EDATE                                          
         LA    RE,5                                                             
         CLI   DMCB+3,0                                                         
         BE    FLTERR                                                           
         EJECT                                                                  
CHKFLT   MVI   DEMOSW,0                                                         
         GOTO1 USER1,DUB,(64,SINIFLT),(5,=C'DEMO=')                             
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKFLT2                                                          
         MVI   SVSUBSC,1           SET SUBSCREEN                                
         L     R4,4(R1)                                                         
         LA    R4,5(R4)                                                         
         LR    R6,R4                                                            
         BAS   R9,GETNUM                                                        
         LTR   R5,R5               DEMO INPUT WASN'T NUMERIC,                   
         BZ    CHKFLT1              TRY GETTING ALPHABETICS                     
         L     R4,4(R1)                                                         
         LA    R4,5(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)                                                      
         CVB   RF,DUB                                                           
         STC   RF,DEMOSW           DEMOSW OBTAINED,                             
         B     CHKFLT2              DO NEXT STEP                                
         SPACE 2                                                                
CHKFLT1  L     R4,4(R1)            GET ALPHABETICS                              
         LA    R4,5(R4)                                                         
         LR    R6,R4                                                            
         BAS   R9,GETALPH                                                       
         LA    RE,5                                                             
         LTR   R5,R5                                                            
         BZ    FLTERR              WASN'T ALPHABETICS EITHER, ERROR             
*                                                                               
         XC    WORK,WORK           MAKE DUMMY TWA HEADER                        
         L     R4,4(R1)                                                         
         LA    R4,5(R4)                                                         
         BCTR  R5,0                        MOVE IN DATA                         
         EXMVC R5,WORK+8,0(R4)                                                  
         LA    R5,1(R5)            RESTORE R5 FROM EX INSTRUCTION               
         STC   R5,WORK+5           INPUT LENGTH                                 
         LA    R5,L'SINIFLTH(R5)   L(FIELD) = L(HEADER) + L(INPUT)              
         STC   R5,WORK             STORE L(FIELD)                               
*                                                                               
         BAS   R9,MKDBLOCK                                                      
*                                                                               
         L     R1,VCOMFACS                                                      
         USING COMFACSD,R1                                                      
         L     RF,CDEMOVAL         DEMO VALIDATION ROUTINE                      
         DROP  R1                                                               
*                                                                               
         GOTO1 (RF),DMCB,(1,WORK),(13,WORK2),(C'S',(R7)),0                      
         LA    RE,5                                                             
         CLI   4(R1),0                                                          
         BE    FLTERR                                                           
         MVC   DEMOSW,WORK2+2      DEMO OBTAINED                                
         SPACE 2                                                                
CHKFLT2  OC    EST1(2),EST1                                                     
         BNZ   CHKFRMT                                                          
         MVC   EST1(2),=X'01FF'                                                 
         CLI   SDATE,0                                                          
         BNE   CHKFRMT                                                          
         MVC   FLDDATA(38),=C'EST= OR DATE= FILTER MUST BE SPECIFIED'           
         FOUT  (R2)                                                             
         LA    R2,SINIFLTH                                                      
         B     MODEXIT                                                          
         EJECT                                                                  
* DETERMINE SCREEN FORMAT                                                       
CHKFRMT  GOTO1 USER1,DUB,(64,SINIFLT),(5,=C'DATA=')    GETFLTR                  
         ICM   R4,15,4(R1)         TEST FOUND DATA=                             
         BZ    FRMTDTE                                                          
         CLC   =C'CONTROL',5(R4)                                                
         BE    CHKCTL                                                           
         CLC   =C'CTL',5(R4)                                                    
         BE    CHKCTL                                                           
         B     FRMTDEM                                                          
*                                                                               
CHKCTL   CLI   DEMOSW,0                                                         
         BNE   FRMTDEM                                                          
         MVI   CTRLSW,1            INDICATE CONTROL DISPLAY                     
         CLC   SVEBCPRD,=C'ALL'                                                 
         BNE   FRMTDEMP            PRODUCT - FORCE PRODUCT DISPLAY              
         MVI   SVDATOPT,C'C'       SET DATA=CONTROL FLAG                        
         MVI   SVSUBSC,2           SET SUBSCREEN                                
* PRODUCT AND CONTROL ESTIMATE DISPLAY HEADLINES                                
         LA    R2,SINHDRH                                                       
         MVC   FLDDATA+1(4),=C'PROD'                                            
         MVC   FLDDATA+40(4),=C'PROD'                                           
         CLC   EST1,EST2                                                        
         BE    *+16                                                             
         MVC   FLDDATA+32(7),=C'CONTROL'                                        
         MVC   FLDDATA+71(7),=C'CONTROL'                                        
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(17),=C'CODE PRODUCT NAME'                              
         CLC   EST1,EST2                                                        
         BE    *+10                                                             
         MVC   FLDDATA+31(8),=C'ESTIMATE'                                       
         MVC   FLDDATA+40(39),FLDDATA+1                                         
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(4),DASH                                                
         MVC   FLDDATA+6(12),DASH                                               
         CLC   EST1,EST2                                                        
         BE    *+10                                                             
         MVC   FLDDATA+31(8),DASH                                               
         MVC   FLDDATA+40(39),FLDDATA+1                                         
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    RE,REC2             CLEAR BUILD AREA                             
         LA    RF,2000                                                          
         XCEF                                                                   
         LA    R6,REC2             ESTIMATE SAVE AREA                           
         LA    R7,MAXF1            SET BCT COUNT                                
* BUILD A LIST OF CONTROL ESTIMATES                                             
F1       BAS   RE,GETEST           GET NEXT ESTIMATE                            
         CLI   RECFLAG,1           END                                          
         BE    F1END                                                            
         L     R5,AREC                                                          
         USING ESTHDRD,R5                                                       
         MVC   0(3,R6),EKEYPRD     SAVE CONTROL ESTIMATE                        
         MVC   3(1,R6),EKEYEST                                                  
         LA    R6,4(R6)                                                         
         LA    R5,KEY              SET KEY FOR NEXT PRODUCT                     
         MVI   EKEYEST,X'FF'                                                    
         MVC   EKEYEST+1(5),EKEYEST                                             
         MVC   PREVKEY,KEY                                                      
         BCT   R7,F1                                                            
         B     F1SEND              SEND DATA                                    
F1END    XC    PREVKEY,PREVKEY                                                  
*                                                                               
* SEND CONTROL ESTIMATE SCREEN                                                  
F1SEND   LA    R6,REC2             COUNT NUMBER OF TABLE ENTRIES                
         LA    R7,0                                                             
         CLI   REC2,0              ANY DATA                                     
         BE    NODATAEX             NO - SEND MESSAGE                           
F1SEND1  CLI   0(R6),0                                                          
         BE    F1SEND2                                                          
         LA    R7,1(R7)                                                         
         LA    R6,4(R6)                                                         
         B     F1SEND1                                                          
*                                                                               
* FORMAT TABLE INTO LINES                                                       
F1SEND2  GOTO1 USER2,DMCB,(4,REC2),(R7),MAXLINE,(2,DMWORK)   FRMTALPH           
F1SEND3  LA    R6,DMWORK           SEND FORMATTED DATA                          
         ST    R2,SAVER2                                                        
         CLI   0(R6),0                                                          
         BE    MODEXIT                                                          
F1SEND4  CLI   0(R6),0                                                          
         BE    F1SEND5                                                          
         L     R7,0(R6)                                                         
         LA    R7,0(R7)                                                         
         BAS   RE,GETPRD           GET DATA NAME FROM RECORD                    
         MVC   FLDDATA+1(3),0(R7)  PRODUCT CODE                                 
         MVC   FLDDATA+7(20),WORK  PRODUCT NAME                                 
         MVC   BYTE,3(R7)                                                       
         CLC   EST1,EST2                                                        
         BE    F1SEND4A                                                         
         LA    RE,FLDDATA+32                                                    
         EDIT  BYTE,(3,(RE))                                                    
F1SEND4A LA    R2,39(R2)                                                        
         SR    RE,RE                                                            
         IC    RE,0(R6)            DECREMENT COUNT                              
         BCTR  RE,0                                                             
         LA    R7,4(R7)                                                         
         ST    R7,0(R6)                                                         
         STC   RE,0(R6)                                                         
         LA    R6,4(R6)            NEXT SLOT                                    
         B     F1SEND4                                                          
* SEND LINE TO SCREEN                                                           
F1SEND5  L     R2,SAVER2                                                        
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         B     F1SEND3                                                          
         EJECT                                                                  
* BUILD DEMO DISPLAY LIST                                                       
FRMTDEM  L     R4,4(R1)                                                         
         LA    RE,5                SET FOR ERROR                                
         LR    R6,R4                                                            
         CLC   5(5,R4),=C'USER1'                                                
         BNE   FRMT10                                                           
         MVI   USERSW,1                                                         
         MVI   SVSUBSC,3           SET SUBSCREEN                                
         B     FRMTUSER                                                         
*                                                                               
FRMT10   CLC   5(5,R4),=C'USER2'                                                
         BNE   FRMT20                                                           
         MVI   USERSW,2                                                         
         MVI   SVSUBSC,3           SET SUBSCREEN                                
         B     FRMTUSER                                                         
*                                                                               
FRMT20   CLC   5(4,R4),=C'DATE'                                                 
         BNE   FRMT25                                                           
         MVI   SVSUBSC,4           SET SUBSCREEN                                
         B     FRMTDTE                                                          
*                                                                               
FRMT25   CLI   DEMOSW,0                                                         
         BNE   FRMTDTE                                                          
         CLC   5(4,R4),=C'COPY'                                                 
         BNE   FRMT30                                                           
         MVI   COPYSW,1                                                         
         MVI   SVSUBSC,5           SET SUBSCREEN                                
         B     FRMTDTE                                                          
*                                                                               
FRMT30   CLC   5(4,R4),=C'NAME'                                                 
         BNE   FRMT32                                                           
         MVI   NAMESW,1                                                         
         MVI   SVSUBSC,6           SET SUBSCREEN                                
         B     FRMTDTE                                                          
*                                                                               
FRMT32   CLC   5(2,R4),=C'PW'                                                   
         BNE   FRMT40                                                           
         MVI   PWSWD,C'Y'                                                       
         MVI   SVSUBSC,6           SET SUBSCREEN                                
         B     FRMTDTE                                                          
*                                                                               
FRMT40   CLC   5(4,R4),=C'DAYP'                                                 
         BE    *+14                                                             
         CLC   5(4,R4),=C'MENU'                                                 
         BNE   FRMT50                                                           
         MVI   MENUSW,1                                                         
         MVI   SVSUBSC,7           SET SUBSCREEN                                
         B     FRMTDTE                                                          
*                                                                               
FRMT50   CLC   5(3,R4),=C'DEM'                                                  
         BNE   FRMT55                                                           
         MVI   SVDATOPT,C'D'       SET DATA=DEM FLAG                            
         B     FRMTDEMP                                                         
*                                                                               
FRMT55   CLC   5(6,R4),=C'FILTER'                                               
         BNE   FLTERR                                                           
******   MVI   SVSUBSC,4           SET SUBSCREEN                                
         MVI   FILTSW,1                                                         
         B     FRMTDTE                                                          
*                                                                               
FRMTDEMP LA    R2,SINHDRH                                                       
         MVC   FLDDATA+1(4),=C'PROD'                                            
         CLC   SVEBCPRD,=C'ALL'                                                 
         BNE   FRMT60                                                           
         MVC   FLDDATA+32(7),=C'CONTROL'                                        
         MVI   SVSUBSC,8           SET SUBSCREEN                                
*                                                                               
FRMT60   FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(17),=C'CODE PRODUCT NAME'                              
         MVC   FLDDATA+26(3),=C'EST'                                            
         MVC   FLDDATA+30(6),=C'DEMO 1'                                         
         MVC   FLDDATA+42(6),=C'DEMO 2'                                         
         MVC   FLDDATA+54(6),=C'DEMO 3'                                         
         MVC   FLDDATA+66(6),=C'DEMO 4'                                         
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(17),DASH                                               
         MVC   FLDDATA+4(2),=C'  '                                              
         MVC   FLDDATA+26(3),DASH                                               
         MVC   FLDDATA+30(6),DASH                                               
         MVC   FLDDATA+42(6),DASH                                               
         MVC   FLDDATA+54(6),DASH                                               
         MVC   FLDDATA+66(6),DASH                                               
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
*                                                                               
         CLI   SVDATOPT,0          TEST SET ALREADY                             
         BNE   *+8                                                              
         MVI   SVDATOPT,C'N'       SET NAME TYPE DISPLAY                        
*                                                                               
*GET ESTIMATES FROM THE FILE                                                    
         LA    R0,MAXLINE          SET LOOP                                     
         STC   R0,LOOPCTR                                                       
         MVI   FRSTTIM,1           SET FIRST TIME                               
F2EST    BAS   RE,GETEST                                                        
         CLI   RECFLAG,1           END                                          
         BNE   *+14                                                             
         XC    PREVKEY,PREVKEY      YES - SET FOR NEXT REQUEST                  
         B     MODEXIT                                                          
         L     R5,AREC             BUILD DEMO DESCRIPTIONS                      
         BAS   R9,MKDBLOCK                                                      
F2ESTA   DS    0H                                                               
         MVC   DMCB+4(4),=X'D9000AE0' DEMOCON                                   
         GOTO1 VCALLOV,DMCB,0                                                   
         L     RF,DMCB                                                          
         XC    WORK,WORK                                                        
         GOTO1 (RF),DMCB,(4,EDEMOS),(13,WORK),(R7),EUSRNMS                      
*                                R7-->DBLOCK FROM MKDBLOCK                      
         LA    R7,WORK                                                          
         LA    R0,4                                                             
         LA    RF,FLDDATA+30                                                    
F2EST1   MVC   0(11,RF),0(R7)                                                   
         LA    RF,12(RF)                                                        
         LA    R7,11(R7)                                                        
         BCT   R0,F2EST1                                                        
*                                                                               
         MVC   BYTE,EKEYEST                                                     
         LA    RE,FLDDATA+26                                                    
         EDIT  BYTE,(3,(RE))                                                    
         CLI   FRSTTIM,0           BUILD PRODUCT CODE AND NAME                  
         BE    F2EST3                                                           
         CLC   SVEBCPRD,=C'ALL'                                                 
         BE    *+8                                                              
         MVI   FRSTTIM,0                                                        
         MVC   FLDDATA+1(3),EKEYPRD                                             
         LA    R7,EKEYPRD                                                       
         BAS   RE,GETPRD                                                        
         MVC   FLDDATA+6(20),WORK                                               
F2EST3   FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R5,KEY              SET NEXT KEY                                 
         MVC   EKEYEST+1(5),=X'FFFFFFFFFF'   NEXT EST                           
         CLC   SVEBCPRD,=C'ALL'                                                 
         BNE   *+8                                                              
         MVI   EKEYEST,X'FF'       NEXT PRODUCT                                 
         MVC   PREVKEY,KEY                                                      
         ZIC   RE,LOOPCTR          DECREMENT LOOP COUNTER                       
         BCTR  RE,0                                                             
         STC   RE,LOOPCTR                                                       
         OR    RE,RE                                                            
         BNZ   F2EST                                                            
         B     MODEXIT                                                          
         EJECT                                                                  
FRMTDTE  MVI   MGSW,0              RESET MAKEGOOD SWITCH                        
         GOTO1 USER1,DUB,(64,SINIFLT),(2,=C'MG')  GETFLTR                       
         OC    4(4,R1),4(R1)                                                    
         BZ    *+8                                                              
         MVI   MGSW,1                                                           
         LA    R2,SINHDRH                                                       
         MVC   FLDDATA+1(4),=C'PROD'                                            
         CLI   COPYSW,1                                                         
         BNE   *+10                                                             
         MVC   FLDDATA+60(4),=C'COPY'                                           
         CLI   MENUSW,1                                                         
         BNE   *+10                                                             
         MVC   FLDDATA+60(3),=C'DPT'                                            
*                                                                               
         OC    SWS(SWSLEN),SWS     TEST ANY SWITCHES ON                         
         BNZ   *+12                                                             
         MVI   NAMESW,1            SET DEFAULTS                                 
         MVI   SVSUBSC,6                                                        
*                                                                               
         CLI   SVDATOPT,0          TEST ALREADY SET                             
         BNE   *+8                                                              
         MVI   SVDATOPT,C'N'       SET NAME TYPE DISPLAY                        
*                                                                               
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(17),=C'CODE PRODUCT NAME'                              
         MVC   FLDDATA+31(8),=C'ESTIMATE'                                       
         MVC   FLDDATA+40(6),=C' START'                                         
         MVC   FLDDATA+50(6),=C'   END'                                         
         CLI   MGSW,1                                                           
         BNE   *+10                                                             
         MVC   FLDDATA+60(7),=C' MG END'                                        
         CLI   COPYSW,1                                                         
         BNE   *+10                                                             
         MVC   FLDDATA+60(4),=C'CODE'                                           
         CLI   MENUSW,1                                                         
         BNE   *+10                                                             
         MVC   FLDDATA+60(4),=C'MENU'                                           
         CLI   DEMOSW,0                                                         
         BE    *+10                                                             
         MVC   FLDDATA+60(8),=C'POSITION'                                       
         CLI   NAMESW,1                                                         
         BNE   *+10                                                             
         MVC   FLDDATA+59(4),=C'NAME'                                           
         CLI   PWSWD,C'Y'                                                       
         BNE   *+10                                                             
         MVC   FLDDATA+60(6),=C'PW PCT'                                         
         CLI   FILTSW,1                                                         
         BNE   *+10                                                             
         MVC   FLDDATA+60(6),=C'FILTER'                                         
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(17),DASH                                               
         MVI   FLDDATA+5,C' '                                                   
         MVC   FLDDATA+31(8),DASH                                               
         MVC   FLDDATA+40(8),DASH                                               
         MVC   FLDDATA+50(8),DASH                                               
         CLI   MENUSW,1                                                         
         BNE   *+10                                                             
         MVC   FLDDATA+60(4),DASH                                               
         CLI   COPYSW,1                                                         
         BNE   *+10                                                             
         MVC   FLDDATA+60(4),DASH                                               
         CLI   NAMESW,1                                                         
         BNE   *+10                                                             
         MVC   FLDDATA+59(4),DASH                                               
         CLI   DEMOSW,0                                                         
         BE    *+10                                                             
         MVC   FLDDATA+60(8),DASH                                               
         CLI   MGSW,1                                                           
         BNE   *+10                                                             
         MVC   FLDDATA+60(8),DASH                                               
         CLI   PWSWD,C'Y'                                                       
         BNE   *+10                                                             
         MVC   FLDDATA+60(6),DASH                                               
         CLI   FILTSW,1                                                         
         BNE   *+10                                                             
         MVC   FLDDATA+60(6),DASH                                               
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
*GET ESTIMATES FROM THE FILE                                                    
         LA    R0,MAXLINE          SET LOOP                                     
         STC   R0,LOOPCTR                                                       
         MVI   FRSTTIM,1           SET FIRST TIME                               
F3EST    BAS   RE,GETEST                                                        
         CLI   RECFLAG,1           END                                          
         BNE   *+14                                                             
         XC    PREVKEY,PREVKEY      YES - SET FOR NEXT REQUEST                  
         B     MODEXIT                                                          
         L     R5,AREC             BUILD DATE LIST                              
         LA    R9,FLDDATA+40                                                    
         GOTO1 VDATCON,DMCB,ESTART,(5,(R9))                                     
         LA    R9,FLDDATA+50                                                    
         GOTO1 VDATCON,DMCB,EEND,(5,(R9))                                       
         CLI   COPYSW,1                                                         
         BNE   F31                                                              
         MVC   FLDDATA+63(1),ECOPY                                              
         B     F3SEND                                                           
F31      CLI   NAMESW,1                                                         
         BNE   F32                                                              
         MVC   FLDDATA+59(20),EDESC                                             
         B     F3SEND                                                           
F32      CLI   DEMOSW,0                                                         
         BE    F33                                                              
         LA    RE,1                                                             
         LA    RF,EDEMOS                                                        
F32A     CLC   2(1,RF),DEMOSW                                                   
         BE    F32B                                                             
         LA    RF,3(RF)                                                         
         LA    RE,1(RE)                                                         
         B     F32A                                                             
F32B     STC   RE,BYTE                                                          
         LA    RE,FLDDATA+61                                                    
         EDIT  BYTE,(2,(RE))                                                    
         B     F3SEND                                                           
F33      CLI   MGSW,1                                                           
         BNE   F34                                                              
         OC    EMGDTE,EMGDTE                                                    
         BZ    F3SEND                                                           
         LA    R9,FLDDATA+60                                                    
         GOTO1 VDATCON,DMCB,(2,EMGDTE),(5,(R9))                                 
F34      CLI   MENUSW,1                                                         
         BNE   F35                                                              
         MVC   FLDDATA+62(1),EDAYMENU                                           
         B     F3SEND                                                           
F35      CLI   PWSWD,C'Y'                                                       
         BNE   F36                                                              
         LA    RE,FLDDATA+60                                                    
         EDIT  (3,EPWPCT),(6,(RE)),2                                            
         B     F3SEND                                                           
F36      CLI   FILTSW,1                                                         
         BNE   F37                                                              
         MVC   FLDDATA+60(3),EPROF                                              
F37      DS    0H                                                               
F3SEND   MVC   BYTE,EKEYEST                                                     
         LA    RE,FLDDATA+34                                                    
         EDIT  BYTE,(3,(RE))                                                    
         CLI   FRSTTIM,0                                                        
         BE    F3SEND1                                                          
         CLC   SVEBCPRD,=C'ALL'                                                 
         BE    *+8                                                              
         MVI   FRSTTIM,0                                                        
         MVC   FLDDATA+1(3),EKEYPRD                                             
         LA    R7,EKEYPRD                                                       
         BAS   RE,GETPRD                                                        
         MVC   FLDDATA+6(20),WORK                                               
F3SEND1  FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R5,KEY                                                           
         MVC   EKEYEST+1(5),=X'FFFFFFFFFF'                                      
         MVC   PREVKEY,KEY                                                      
         ZIC   RE,LOOPCTR          DECREMENT LOOP COUNTER                       
         BCTR  RE,0                                                             
         STC   RE,LOOPCTR                                                       
         OR    RE,RE                                                            
         BNZ   F3EST                                                            
         B     MODEXIT                                                          
         EJECT                                                                  
*                                                                               
FRMTUSER LA    R2,SINHDRH                                                       
         MVC   FLDDATA+1(4),=C'PROD'                                            
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(17),=C'CODE PRODUCT NAME'                              
         MVC   FLDDATA+31(8),=C'ESTIMATE'                                       
         MVC   FLDDATA+40(11),=C'USER FIELDS'                                   
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(17),DASH                                               
         MVI   FLDDATA+5,C' '                                                   
         MVC   FLDDATA+31(8),DASH                                               
         MVC   FLDDATA+40(11),DASH                                              
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
*GET ESTIMATES FROM THE FILE                                                    
         LA    R0,MAXLINE          SET LOOP                                     
         STC   R0,LOOPCTR                                                       
         MVI   FRSTTIM,1           SET FIRST TIME                               
F4EST    BAS   RE,GETEST                                                        
         CLI   RECFLAG,1           END                                          
         BNE   *+14                                                             
         XC    PREVKEY,PREVKEY      YES - SET FOR NEXT REQUEST                  
         B     MODEXIT                                                          
         L     R5,AREC             BUILD DATE LIST                              
         LA    R9,FLDDATA+40                                                    
*                                                                               
F4SEND   MVC   BYTE,EKEYEST                                                     
         LA    RE,FLDDATA+34                                                    
         EDIT  BYTE,(3,(RE))                                                    
         CLI   USERSW,1                                                         
         BNE   F4SEND1                                                          
         MVC   FLDDATA+40(32),EUSER1                                            
         B     *+10                                                             
*                                                                               
F4SEND1  MVC   FLDDATA+40(16),EUSER2                                            
         CLI   FRSTTIM,0                                                        
         BE    F4SEND3                                                          
         CLC   SVEBCPRD,=C'ALL'                                                 
         BE    *+8                                                              
         MVI   FRSTTIM,0                                                        
         MVC   FLDDATA+1(3),EKEYPRD                                             
         LA    R7,EKEYPRD                                                       
         BAS   RE,GETPRD                                                        
         MVC   FLDDATA+6(20),WORK                                               
F4SEND3  FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R5,KEY                                                           
         MVC   EKEYEST+1(5),=X'FFFFFFFFFF'                                      
         MVC   PREVKEY,KEY                                                      
         ZIC   RE,LOOPCTR          DECREMENT LOOP COUNTER                       
         BCTR  RE,0                                                             
         STC   RE,LOOPCTR                                                       
         OR    RE,RE                                                            
         BNZ   F4EST                                                            
         B     MODEXIT                                                          
         EJECT                                                                  
GETNUM   LA    R5,0                GET NUMERICS                                 
GN1      CLI   0(R4),C'-'                                                       
         BER   R9                                                               
         CLI   0(R4),C','                                                       
         BER   R9                                                               
         CLI   0(R4),X'00'                                                      
         BER   R9                                                               
         CLI   0(R4),C' '                                                       
         BER   R9                                                               
         CLI   0(R4),C'0'                                                       
         BL    GNERR                                                            
         CLI   0(R4),C'9'                                                       
         BH    GNERR                                                            
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         B     GN1                                                              
GNERR    SR    R5,R5                                                            
         BR    R9                                                               
         SPACE 2                                                                
GETALPH  LA    R5,0                GET ALPHABETICS                              
GA10     CLI   0(R4),X'00'         TRYING TO FOLLOW THE SAME                    
         BER   R9                   LOGIC AS GETNUM ABOVE                       
         CLI   0(R4),C' '          A BLANK IS ALLOWED                           
         BE    GA20                                                             
         CLI   0(R4),C'+'                                                       
         BE    GA20                                                             
         CLI   0(R4),C'-'                                                       
         BE    GA20                                                             
         CLI   0(R4),C'/'                                                       
         BE    GA20                                                             
****     CLI   0(R4),C'.'                                                       
******   BE    GA20                                                             
         CLI   0(R4),C','                                                       
         BER   R9                                                               
         CLI   0(R4),C'A'                                                       
         BL    GAERR                                                            
         CLI   0(R4),C'I'                                                       
         BNH   GA20                VALID ALPHABET                               
         CLI   0(R4),C'J'                                                       
         BL    GAERR                                                            
         CLI   0(R4),C'R'                                                       
         BNH   GA20                VALID ALPHABET                               
         CLI   0(R4),C'S'                                                       
         BL    GAERR                                                            
         CLI   0(R4),C'Z'                                                       
         BNH   GA20                VALID ALPHABET                               
         CLI   0(R4),C'0'                                                       
         BL    GAERR                                                            
         CLI   0(R4),C'9'                                                       
         BH    GAERR               VALID DIGITS                                 
GA20     LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         B     GA10                                                             
GAERR    SR    R5,R5                                                            
         BR    R9                                                               
         EJECT                                                                  
MKDBLOCK XC    ELEM,ELEM           BUILD DBLOCK FOR DEMOVAL                     
         LA    R7,ELEM                                                          
         USING DBLOCKD,R7                                                       
         MVC   DBCOMFCS,VCOMFACS   A(COMFACS)                                   
         MVC   DBFILE,=C'TP '      FILE                                         
         MVC   DBSELAGY,SVAGYA     AGENCY                                       
         MVI   DBSELMED,C'T'                                                    
         CLI   SVEBCMED,C'T'                                                    
         BE    *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVCNTRY,C'C'        IF CANADA,                                   
         BNE   XMKDBLCK                                                         
         CLI   SVCLEX+0,C'U'        AND NOT US DEMOS                            
         BE    XMKDBLCK                                                         
         MVI   DBSELMED,C'C'        SET CANADIAN DEMOS                          
         DROP  R7                                                               
XMKDBLCK BR    R9                                                               
         SPACE 2                                                                
NODATAEX MVI   ERRCD,NOFNDERR                                                   
         LA    R2,SINIKEYH                                                      
         GOTO1 ERROR                                                            
         B     MODEXIT                                                          
*                                                                               
*                                                                               
* SEND FILTER ERROR MESSAGE                                                     
PRFLTERR ZIC   RE,WORK2            RE=L(INPUT)                                  
         LA    R6,WORK2+12         R6-->INPUT                                   
FLTERR   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SINMSG(0),0(R6)   * EXECUTED *                                   
         LA    RE,3(RE)                                                         
         LA    RF,SINMSG(RE)                                                    
         MVC   0(22,RF),=C'- INVALID FILTER FIELD'                              
         LA    R2,SINIFLTH                                                      
         FOUT  (R2)                                                             
         MVI   ERRAREA,X'FF'                                                    
         B     *+8                                                              
*                                                                               
MODEXIT  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
         OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
         EJECT                                                                  
* READ ESTIMATES AND FILTER THEM                                                
GETEST   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING ESTHDRD,R5                                                       
         MVI   RECFLAG,0                                                        
         MVC   EKEYAM,SVAGYMD                                                   
         MVC   EKEYCLT,SVCLT                                                    
         CLC   SVEBCPRD,=C'ALL'                                                 
         BE    GETEST2                                                          
         MVC   EKEYPRD,SVEBCPRD                                                 
         MVC   EKEYEST,SVEST                                                    
*                                                                               
GETEST2  OC    PREVKEY,PREVKEY                                                  
         BZ    *+10                                                             
         MVC   KEY,PREVKEY                                                      
*                                                                               
GEHIGH   GOTO1 HIGH                                                             
         B     GEREC                                                            
*                                                                               
GESEQ    GOTO1 SEQ                                                              
GEREC    LA    R5,KEY                                                           
         CLC   KEY(4),KEYSAVE                                                   
         BE    *+12                                                             
         MVI   RECFLAG,1                                                        
         B     GEEXIT                                                           
         CLC   SVEBCPRD,=C'ALL'                                                 
         BE    GEFEST                                                           
         CLC   EKEYPRD,SVEBCPRD    PRODUCT CHECK                                
         BE    GEFEST                                                           
         MVI   RECFLAG,1                                                        
         B     GEEXIT                                                           
*                                                                               
* FILTER KEY ON ESTIMATE NUMBER                                                 
GEFEST   CLI   EKEYEST,0                                                        
         BE    GESEQ                                                            
         CLI   LOCKSW,1                                                         
         BNE   *+12                                                             
         TM    13(R5),X'08'                                                     
         BZ    GESEQ                                                            
         CLI   HOLDSW,1                                                         
         BNE   *+12                                                             
         TM    13(R5),X'0C'                                                     
         BNO   GESEQ                                                            
         CLI   EKEYEST+1,0                                                      
         BE    GEFEST2                                                          
GEFEST1  MVI   EKEYEST+1,X'FF'                                                  
         MVC   EKEYEST+2(4),EKEYEST+1                                           
         B     GEHIGH                                                           
*                                                                               
GEFEST2  CLC   EKEYEST,EST1                                                     
         BNL   *+14                                                             
         MVC   EKEYEST,EST1                                                     
         B     GEHIGH                                                           
*                                                                               
         CLC   EKEYEST,EST2                                                     
         BH    GEFEST1                                                          
         GOTO1 GETREC                                                           
         L     R5,AREC                                                          
         MVI   RECFLAG,0                                                        
         CLI   REQSW,0                                                          
         BE    GE12                                                             
         CLI   REQSW,C'Y'                                                       
         BNE   GE10                                                             
         TM    EFLAG1,EF1REQ                                                    
         BO    GEEXIT                                                           
         B     GESEQ                                                            
*                                                                               
GE10     TM    EFLAG1,EF1REQ                                                    
         BNO   GEEXIT                                                           
         B     GESEQ                                                            
*                                                                               
GE12     CLI   NMGSW,0                                                          
         BE    GE16                                                             
         CLI   NMGSW,C'Y'                                                       
         BNE   GE14                                                             
         TM    EFLAG1,EF1NMG                                                    
         BO    GEEXIT                                                           
         B     GESEQ                                                            
*                                                                               
GE14     TM    EFLAG1,EF1NMG                                                    
         BNO   GEEXIT                                                           
         B     GESEQ                                                            
*                                                                               
GE16     CLI   PWSW,0                                                           
         BE    GE20                                                             
         CLI   PWSW,C'Y'                                                        
         BNE   GE18                                                             
         OC    EPWPCT,EPWPCT                                                    
         BNZ   GEEXIT                                                           
         B     GESEQ                                                            
*                                                                               
GE18     OC    EPWPCT,EPWPCT                                                    
         BZ    GEEXIT                                                           
         B     GESEQ                                                            
*                                                                               
GE20     CLI   DEMOSW,0                                                         
         BE    GEFEST4                                                          
         LA    RE,14                                                            
         LA    RF,EDEMOS                                                        
GEFEST3  CLC   DEMOSW,2(RF)                                                     
         BE    GEFEST4                                                          
         LA    RF,3(RF)                                                         
         OC    0(3,RF),0(RF)                                                    
         BZ    GESEQ                                                            
         BCT   RE,GEFEST3                                                       
         B     GESEQ                                                            
GEFEST4  CLI   SDATE,0                                                          
         BE    GEEXIT                                                           
         L     R5,AREC                                                          
         CLC   EEND,SDATE                                                       
         BL    GESEQ                                                            
         CLC   ESTART,EDATE                                                     
         BH    GESEQ                                                            
*                                                                               
GEEXIT   XIT1  1                                                                
         EJECT                                                                  
GETPRD   NTR1                                                                   
         MVC   WORK2,KEY                                                        
         LA    R6,KEY                                                           
         USING PRDHDRD,R6                                                       
         XC    KEY,KEY                                                          
         MVC   PKEYAM,SVAGYMD                                                   
         MVC   PKEYCLT,SVCLT                                                    
         MVC   PKEYPRD,0(R7)                                                    
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         L     R6,AREC                                                          
         MVC   WORK(20),PNAME                                                   
         MVC   KEY(40),WORK2                                                    
         XIT1  1                                                                
         EJECT                                                                  
***********************************************************************         
*====================== VALIDATE FILTER OPTIONS ======================*         
VALFLTR  NTR1                                                                   
*                                                                               
         CLI   SINIFLTH+5,0                                                     
         BE    VALYES                                                           
*                                                                               
VALF4    MVI   NAMESW,0            ELSE RESET NOW                               
         GOTO1 VSCANNER,DMCB,SINIFLTH,SCANTBLE                                  
*                                                                               
         CLI   4(R1),0             NO INPUT, FILTERS MUST BE VALID              
         BE    VALYES                                                           
*                                                                               
         ZIC   RF,4(R1)            RF = LOOP COUNTER                            
         LA    R4,SCANTBLE                                                      
VALF10   CLC   =C'DISPLAY',12(R4)                                               
         BE    FLTOKAY                                                          
         CLC   =C'DATA',12(R4)                                                  
         BE    FLTOKAY                                                          
         CLC   =C'DATE',12(R4)                                                  
         BE    FLTOKAY                                                          
         CLC   =C'DEMO',12(R4)                                                  
         BE    FLTOKAY                                                          
         CLC   =C'EST',12(R4)                                                   
         BE    FLTOKAY                                                          
         CLC   =C'REQ',12(R4)                                                   
         BE    FLTOKAY                                                          
         CLC   =C'NMG',12(R4)                                                   
         BE    FLTOKAY                                                          
         CLC   =C'PW',12(R4)                                                    
         BNE   VALNO                                                            
*                                                                               
FLTOKAY  CLI   1(R4),0             CHECK FOR SECOND HALF OF INPUT               
         BE    VALNO                NO 2ND-1/2 INPUT ==> ERROR                  
         LA    R4,32(R4)           BUMP R4 TO NEXT BLOCK                        
         BCT   RF,VALF10                                                        
*                                                                               
VALYES   CR    RF,RF               SET CC FOR EXIT                              
         B     VALFXIT                                                          
*                                                                               
VALNO    MVC   WORK2(32),0(R4)     SAVE AREA OF ERROR                           
         LA    RF,1                                                             
         LTR   RF,RF                                                            
*                                                                               
VALFXIT  XIT1                                                                   
***********************************************************************         
DASH     DC    40C'-'                                                           
         LTORG                                                                  
         EJECT                                                                  
ESTWRKD  DSECT                                                                  
EST1     DS    C                   ESTIMATE 1                                   
EST2     DS    C                   ESTIMATE 2                                   
RECFLAG  DS    C                   RECORD FLAG 0=FOUND 1=END                    
SDATE    DS    CL6                 START DATE                                   
EDATE    DS    CL6                 END DATE                                     
FRSTTIM  DS    C                   FIRST TIME                                   
SWS      DS    0X                                                               
MGSW     DS    C                   MAKEGOOD DATE                                
DEMOSW   DS    C                   DEMO FILTER                                  
NAMESW   DS    C                   DISPLAY ESTIMATE NAME                        
COPYSW   DS    C                   DISPLAY COPY CODES                           
MENUSW   DS    C                   DISPLAY DAYPART MENUS                        
LOCKSW   DS    C                   DISPLAY LOCKED RECORDS ONLY                  
HOLDSW   DS    C                   DISPLAY HELD RECORDS ONLY                    
USERSW   DS    C                   DISPLAY USER DESC FIELDS                     
FILTSW   DS    C                   DISPLAY FILTER FIELD                         
REQSW    DS    C                   DISPLAY ONLY REQ=Y/N RECORDS                 
NMGSW    DS    C                   DISPLAY ONLY NMG=Y/N RECORDS                 
PWSW     DS    C                   DISPLAY ONLY PW=Y/N RECORDS                  
PWSWD    DS    C                   DISPLAY PW PCT                               
CTRLSW   DS    C                   DISPLAY CONTROL ESTIMATES                    
SWSLEN   EQU   *-MGSW                                                           
LOOPCTR  DS    XL1                 LOOP COUNTER                                 
RELO     DS    A                                                                
SAVER2   DS    F                                                                
MYEUSRNM DS    CL35                USED FOR DEMOVAL ROUTINE                     
SCANTBLE DS    XL256                                                            
MAXLINE  EQU   13                  MAXIMUM DATA LINES                           
MAXF1    EQU   28                  MAXIMUM FORMAT 1 ITEM                        
LINLEN   EQU   88                  LINE LENGTH                                  
ESTWRKX  EQU   *                                                                
* SPINFWORK                                                                     
       ++INCLUDE SPINFWORK                                                      
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019SPINF13S  05/01/02'                                      
         END                                                                    
