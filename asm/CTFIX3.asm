*          DATA SET CTFIX3     AT LEVEL 002 AS OF 01/11/18                      
*PHASE CTFIX3A                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE BINSR31                                                                
*INCLUDE PRTREC                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*                                                                               
         TITLE 'CTFIX3A - UPDATE TERMINAL PRINTER RECORDS'                      
         PRINT NOGEN                                                            
CTFIX    CSECT                                                                  
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         NBASE WORKX-WORKD,**CFIX**,R9,WORK=V(REGSAVE)                          
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA           RA=PRINTER CONTROL REGISTER                  
*                                                                               
         L     R1,=A(IOAREA1-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO1                                                          
         ST    R1,AREC                                                          
         AHI   R1,-8                                                            
         MVC   0(8,R1),=C'**IOA1**'                                             
*                                                                               
CTFIX1   BRAS  RE,INIT             READ CARDS ECT                               
*                                                                               
CTFIX2   CLI   RCMODE,C'F'         FIX MODE                                     
         BNE   CTFIX3                                                           
         BRAS  RE,FIXR                                                          
         B     CTFIX4                                                           
*                                                                               
CTFIX3   CLI   RCMODE,C'B'         BACK OUT MODE                                
         BNE   CTFIX4                                                           
         BRAS  RE,BKOT                                                          
*                                                                               
CTFIX4   BRAS  RE,DONE             CLOSE ALL                                    
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
*                                                                               
EXITEQ   CR    RB,RB                                                            
         J     EXIT                                                             
EXITNE   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* READ TERMINAL RECORDS AND FIX THEM. RECORD ACTIVITY IN OUTPUT FILE  *         
***********************************************************************         
FIXR     NTR1  BASE=*,LABEL=*                                                   
         OPEN  (OUTDATA,OUTPUT)                                                 
         LA    R3,LOGRCD           R3=A(OUTPUT LOG RECORD)                      
         USING LOGRCDD,R3                                                       
*                                                                               
         L     R6,AREC             R6=A(RECORD)                                 
         USING CTTREC,R6                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   IOKEY,C'T'                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,IOAREA1                
         CLI   0(R6),C'T'          TEST TERMINAL RECORD                         
         BE    FIXR0                                                            
         DC    H'0'                                                             
*                                                                               
FIXRNXT  GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,IOAREA1                
         CLI   0(R6),C'T'          TEST TERMINAL RECORD                         
         BL    FIXRX                                                            
         BH    FIXRZ               PRINT RESULTS IF NO MORE TERMINALS           
*                                                                               
FIXR0    TM    CTTSTAT,X'80'       IGNORE DELETED                               
         BO    FIXRX                                                            
         MVI   DELWHY,C' '                                                      
         XC    AAPPLID,AAPPLID                                                  
         XC    ATRMDEF,ATRMDEF                                                  
         XC    LOGRCD,LOGRCD                                                    
         MVC   LOGKEY,CTTKEY                                                    
         MVC   LOGSTATV,CTTSTAT                                                 
         MVC   LOGSTATF,CTTSTAT                                                 
         MVC   LOGSPARE,SPACES                                                  
*                                                                               
         OC    CTTKTID,CTTKTID     TEST NULL LUID                               
         BNZ   FIXR0A                                                           
         L     RF,CNULL            BUMP NULL TERMINAL RECORDS                   
         AHI   RF,1                                                             
         ST    RF,CNULL                                                         
         L     RF,DNULL            BUMP NULL TERMINAL DELETED                   
         AHI   RF,1                                                             
         ST    RF,DNULL                                                         
         C     RF,MAXNULL                                                       
         BH    FIXRX                                                            
         MVI   DELWHY,C'1'                                                      
         B     FIXRPUR                                                          
FIXR0A   OC    CTTKPASS,CTTKPASS   TEST IF PASSWORD IN KEY                      
         BZ    FIXRSC0                                                          
         TM    CTTSTAT,X'04'       TEST PRINTER TERMINAL                        
         BO    FIXR0B                                                           
         L     RF,CPWDT            BUMP TERMINAL PASSWORD RECORDS               
         AHI   RF,1                                                             
         ST    RF,CPWDT                                                         
         L     RF,DPWDT            BUMP TERMINAL PASSWORD DELETED               
         AHI   RF,1                                                             
         ST    RF,DPWDT                                                         
         C     RF,MAXPSWD                                                       
         BH    FIXRX                                                            
         MVI   DELWHY,C'2'                                                      
         B     FIXRPUR                                                          
*                                                                               
FIXR0B   CLI   CTTKTID-1,C'P'      TEST PRINTER PASSIVE                         
         BE    FIXRSC0                                                          
         L     RF,CPWDP            BUMP PRINTER QUEUE EXTRA RECORDS             
         AHI   RF,1                                                             
         ST    RF,CPWDP                                                         
*                                                                               
FIXRSC0  TM    CTTSTAT,X'04'       TEST PRINTER TERMINAL                        
         BZ    FIXRX                                                            
*                                                                               
         LA    R0,100              SCAN PRINTER TERMINAL RECORD                 
         LA    RE,CTTDATA                                                       
         XC    DUB,DUB                                                          
         XC    AAPPLID,AAPPLID                                                  
         XC    ATRMDEF,ATRMDEF                                                  
         CLI   CTTKTID+7,C'S'                                                   
         BNE   *+8                                                              
         OI    DUB,X'08'+X'02'     SET AUTO AND SHUTTLE                         
*                                                                               
FIXRSC1  CLI   0(RE),0             TEST END OF RECORD                           
         BE    FIXRSC5                                                          
         CLI   0(RE),X'24'         TEST APPLICATION ID ELEMENT                  
         BNE   FIXRSC2                                                          
         OC    AAPPLID,AAPPLID     SAVE A(FIRST APPLID ELEMENT)                 
         BNZ   FIXRSC2                                                          
         ST    RE,AAPPLID                                                       
         TM    CTTRMAT1-CTTRMD(RE),X'02'                                        
FIXRSC2  CLI   0(RE),X'25'         TEST TERMINAL DEFN ELEMENET                  
         BNE   FIXRSC3                                                          
         ST    RE,ATRMDEF                                                       
         TM    CTTRMAT1-CTTRMD(RE),X'02'                                        
         BZ    *+8                                                              
         OI    DUB,X'08'           SET AUTO MODE                                
         TM    CTTRMDEV-CTTRMD(RE),X'82'                                        
         BNO   *+8                                                              
         OI    DUB,X'02'           SET SHUTTLE                                  
         TM    CTTRMDEV-CTTRMD(RE),X'81'                                        
         BNO   *+8                                                              
         OI    DUB,X'01'           SET PRINTER                                  
         B     FIXRSC4                                                          
FIXRSC3  CLI   0(RE),X'29'         TEST PRINTERQ ELEMENT                        
         BNE   FIXRSC4                                                          
         OI    DUB,X'04'           SET PRINTERQ ELEMENT PRESENT                 
         B     FIXRSC5                                                          
FIXRSC4  SR    RF,RF               BUMP TO NEXT ELEMENT                         
         ICM   RF,1,1(RE)                                                       
         BZ    FIXRSC5                                                          
         AR    RE,RF                                                            
         BCT   R0,FIXRSC1                                                       
FIXRSC5  EQU   *                                                                
*                                                                               
FIXRSC6  TM    CTTSTAT,X'08'       TEST AUTO MODE                               
         BZ    FIXRSC9                                                          
         L     RE,AAUTO                                                         
FIXRSC7  CLC   0(4,RE),=F'0'       SEARCH AUTO LIST                             
         BE    FIXRSC9                                                          
         CLC   0(4,RE),=X'FFFFFFFF'                                             
         BE    FIXRSC9                                                          
         CLC   0(8,RE),0(RF)                                                    
         BE    FIXRSC8                                                          
         LA    RE,10(RE)                                                        
         B     FIXRSC7                                                          
FIXRSC8  OI    DUB,X'20'           SET IN AUTO LIST                             
*                                                                               
FIXRSC9  MVC   LOGINFO,DUB         SET LOG INFO FLAGS                           
*                                                                               
         CLI   CTTKTID-1,C'P'      TEST PRINTER PASSIVE                         
         BE    FIXR2                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS ACTIVE TERMINAL PRINTER/SHUTTLE RECORDS                     *         
***********************************************************************         
FIXR1    L     RF,CACTV            BUMP ACTV COUNT                              
         AHI   RF,1                                                             
         ST    RF,CACTV                                                         
*                                                                               
         L     RF,NACTV            MOVE TO ACTV LIST                            
         CLC   0(4,RF),=X'FFFFFFFF'                                             
         BE    FIXRNXT                                                          
         MVC   0(8,RF),CTTKTID                                                  
         MVC   8(1,RF),CTTSTAT                                                  
         MVC   9(1,RF),DUB                                                      
         LA    RF,10(RF)                                                        
         ST    RF,NACTV                                                         
         AHI   RF,-10                                                           
*                                                                               
         TM    CTTSTAT,X'08'       EXIT IF NOT IN AUTO MODE                     
         BZ    FIXRNXT                                                          
         L     R0,CACTVAUT         BUMP ACTV AUTO COUNT                         
         AHI   R0,1                                                             
         ST    R0,CACTVAUT                                                      
         L     R0,CACTV            DECR ACTV COUNT                              
         AHI   R0,-1                                                            
         ST    R0,CACTV                                                         
         L     RE,AAUTO            RE=A(KNOWN ACTIVE AUTO LIST)                 
*                                                                               
FIXR1B   CLC   0(4,RE),=F'0'       SEARCH LIST OF KNOWN AUTO PRINTERS           
         BE    FIXR1C                                                           
         CLC   0(4,RE),=X'FFFFFFFF'                                             
         BE    FIXR1C                                                           
         CLC   0(8,RE),0(RF)       EXIT IF KNOWN ACTIVE PRINTER/SHUTTLE         
         BE    FIXRNXT                                                          
         LA    RE,10(RE)                                                        
         B     FIXR1B                                                           
*                                                                               
FIXR1C   OI    9(RF),X'40'         SET AUTO TURNED OFF                          
         OI    LOGINFO,X'40'                                                    
         L     RF,CAUTOFF          BUMP AUTO TURNED OFF COUNT                   
         AHI   RF,1                                                             
         ST    RF,CAUTOFF                                                       
         L     RF,CAUTTOT          BUMP AUTO TOTAL COUNT                        
         AHI   RF,1                                                             
         ST    RF,CAUTTOT                                                       
         C     RF,MAXAUTO                                                       
         BH    FIXRX                                                            
         B     FIXRAOF                                                          
         EJECT                                                                  
***********************************************************************         
* PROCESS PASSIVE TERMINAL PRINTER/SHUTTLE RECORDS                    *         
***********************************************************************         
FIXR2    L     RF,CPASV            BUMP PASV COUNT                              
         AHI   RF,1                                                             
         ST    RF,CPASV                                                         
*                                                                               
         L     RF,NPASV            MOVE TO PASV LIST                            
         CLC   0(4,RF),=X'FFFFFFFF'                                             
         BE    FIXRNXT                                                          
         MVC   0(8,RF),CTTKTID                                                  
         MVC   8(1,RF),CTTSTAT                                                  
         MVC   9(1,RF),DUB                                                      
         LA    RF,10(RF)                                                        
         ST    RF,NPASV                                                         
*                                                                               
         TM    CTTSTAT,X'08'       TEST IF AUTO MODE                            
         BZ    FIXR2E                                                           
         L     R0,CPASVAUT         BUMP PASV AUTO COUNT                         
         AHI   R0,1                                                             
         ST    R0,CPASVAUT                                                      
         L     R0,CPASV            DECR PASV COUNT                              
         AHI   R0,-1                                                            
         ST    R0,CPASV                                                         
*                                                                               
FIXR2A   TM    CTTSTAT,X'08'       TEST AUTO MODE                               
         BZ    FIXR2E                                                           
         L     RE,AAUTO                                                         
FIXR2B   CLC   0(4,RE),=F'0'       SEARCH AUTO LIST                             
         BE    FIXR2E                                                           
         CLC   0(4,RE),=X'FFFFFFFF'                                             
         BE    FIXR2E                                                           
         CLC   0(8,RE),0(RF)                                                    
         BE    FIXR2C                                                           
         LA    RE,10(RE)                                                        
         B     FIXR2B                                                           
FIXR2C   OI    9(RF),X'20'         SET IN AUTO LIST                             
         B     FIXR2J              DONT DELETE IF IN AUTO LIST                  
*                                                                               
FIXR2E   L     RE,AACTV            RE=A(FRST ACTV ENTRY)                        
         L     RF,NPASV                                                         
         AHI   RF,-10              RF=A(LAST PASV ENTRY)                        
FIXR2F   CLC   0(4,RE),=F'0'       END OF LIST                                  
         BE    FIXR2H                                                           
         CLC   0(4,RE),=X'FFFFFFFF'                                             
         BE    FIXR2H                                                           
         CLC   0(8,RE),0(RF)                                                    
         BE    FIXR2J                                                           
FIXR2G   LA    RE,10(RE)                                                        
         B     FIXR2F                                                           
*                                                                               
FIXR2H   OI    9(RF),X'80'         FLAG AS MISSING ACTV ENTRY                   
         OI    LOGINFO,X'80'                                                    
         L     RF,CLOST            BUMP LOST COUNT                              
         AHI   RF,1                                                             
         ST    RF,CLOST                                                         
         TM    CTTSTAT,X'08'       TEST IF AUTO MODE                            
         BZ    FIXR2H1                                                          
         L     R0,CPASVAUT         DECR PASV AUTO COUNT                         
         AHI   R0,-1                                                            
         ST    R0,CPASVAUT                                                      
         C     R0,MAXLOST                                                       
         BH    FIXRX                                                            
         MVI   DELWHY,C'3'                                                      
         B     FIXRPUR                                                          
FIXR2H1  L     R0,CPASV            DECR PASV COUNT                              
         AHI   R0,-1                                                            
         ST    R0,CPASV                                                         
         C     R0,MAXLOST                                                       
         BH    FIXRX                                                            
         MVI   DELWHY,C'3'                                                      
         B     FIXRPUR             PURGE PRINTER PASSIVE                        
*                                                                               
FIXR2J   TM    CTTSTAT,X'08'       TEST AUTO MODE                               
         BZ    FIXR2N                                                           
         L     RE,AAUTO                                                         
FIXR2K   CLC   0(4,RE),=F'0'       SEARCH AUTO LIST                             
         BE    FIXR2L                                                           
         CLC   0(4,RE),=X'FFFFFFFF'                                             
         BE    FIXR2L                                                           
         CLC   0(8,RE),0(RF)                                                    
         BE    FIXR2M                                                           
         LA    RE,10(RE)                                                        
         B     FIXR2K                                                           
*                                                                               
FIXR2L   OI    9(RF),X'40'         SET AUTO TURNED OFF                          
         OI    LOGINFO,X'40'                                                    
         L     RF,CAUTPOFF         BUMP AUTO PASSIVE TURNED OFF COUNT           
         AHI   RF,1                                                             
         ST    RF,CAUTPOFF                                                      
         L     RF,CAUTTOT          BUMP AUTO TOTAL COUNT                        
         AHI   RF,1                                                             
         ST    RF,CAUTTOT                                                       
         C     RF,MAXAUTO                                                       
         BH    FIXRX                                                            
         B     FIXRAOF                                                          
FIXR2M   EQU   *                                                                
*                                                                               
FIXR2N   EQU   *                                                                
*                                                                               
FIXRX    MVC   LKEY,0(R6)          SAVE LAST KEY                                
         B     FIXRNXT                                                          
*                                                                               
FIXRPUR  CLI   RCMODED,C'X'        EXIT IT NOT PROCESSING DELETES               
         BE    FIXRNXT                                                          
         MVI   LOGACTN,X'80'       SET DELETE ACTION                            
         GOTO1 VDATAMGR,DMCB,(DMBITS,DMREAD),CTFILE,AIO1,AIO1                   
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         OI    LOGACTN,X'20'       SET FIX FAILED                               
         B     FIXRTRC                                                          
         OI    CTTSTAT,X'80'                                                    
         MVC   LOGSTATF,CTTSTAT    LOG FIXED STATUS                             
*                                                                               
FIXPUR1  CLI   RCWRITE,C'Y'        WRITE=YES                                    
         BNE   FIXRTRC                                                          
         GOTO1 VDATAMGR,DMCB,(X'00',DMWRT),CTFILE,AIO1,AIO1                     
         CLI   8(R1),0                                                          
         BNE   *+12                                                             
         OI    LOGACTN,X'10'       SET FIX DONE                                 
         B     FIXRTRC                                                          
         OI    LOGACTN,X'20'       SET FIX FAILED                               
         B     FIXRTRC                                                          
*                                                                               
FIXRAOF  CLI   RCMODEA,C'X'        EXIT IT NOT PROCESSING AUTO RECORDS          
         BE    FIXRNXT                                                          
         MVI   LOGACTN,X'40'       SET AUTO OFF ACTION                          
         GOTO1 VDATAMGR,DMCB,(DMBITS,DMREAD),CTFILE,AIO1,AIO1                   
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         OI    LOGACTN,X'20'       SET FIX FAILED                               
         B     FIXRTRC                                                          
         NI    CTTSTAT,255-X'08'   TURN OFF AUTO MODE                           
         MVC   LOGSTATF,CTTSTAT                                                 
         LT    RE,ATRMDEF                                                       
         BZ    FIXRAOF1                                                         
         MVC   LOGAT1V,CTTRMAT1-CTTRMD(RE)                                      
         NI    CTTRMAT1-CTTRMD(RE),255-X'02'                                    
         MVC   LOGAT1F,CTTRMAT1-CTTRMD(RE)                                      
         SR    RE,R6                                                            
         STCM  RE,3,LOGDEFD        SAVE DISPLACEMENT OF ELEMENT                 
*                                                                               
FIXRAOF1 CLI   RCWRITE,C'Y'        WRITE=YES                                    
         BNE   FIXRTRC                                                          
         GOTO1 VDATAMGR,DMCB,(X'00',DMWRT),CTFILE,AIO1,AIO1                     
         CLI   8(R1),0                                                          
         BNE   *+12                                                             
         OI    LOGACTN,X'10'       SET FIX DONE                                 
         B     FIXRTRC                                                          
         OI    LOGACTN,X'20'       SET FIX FAILED                               
         B     FIXRTRC                                                          
*                                                                               
FIXRTRC  CLI   RCTRACE,C'Y'        TRACE=Y                                      
         BNE   FIXRTRCX                                                         
         MVC   P,SPACES                                                         
         MVC   P(25),LOGKEY                                                     
         OC    LOGKEY+1(22),LOGKEY+1                                            
         BNZ   FIXRTRC1                                                         
         SR    R0,R0                                                            
         ICM   R0,3,LOGKEY+23      PASSIVE TERMINAL NUMBER                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   P+18,C'#'                                                        
         UNPK  P+19(6),DUB+4(4)                                                 
*                                                                               
FIXRTRC1 GOTO1 VHEXOUT,PARAMS,LOGSTATV,P+26,1,=C'TOG'                           
         GOTO1 (RF),(R1),LOGSTATF,P+29,1                                        
         OC    LOGDEFD,LOGDEFD                                                  
         BZ    FIXRTRC2                                                         
         GOTO1 (RF),(R1),LOGDEFD,P+34,2                                         
         GOTO1 (RF),(R1),LOGAT1V,P+39,1                                         
         GOTO1 (RF),(R1),LOGAT1F,P+42,1                                         
         GOTO1 (RF),(R1),LOGINFO,P+46,1                                         
         TM    LOGINFO,X'A0'                                                    
         BNO   *+8                                                              
         MVI   P+49,C'<'           SHOW LOST AND IN AUTO LIST                   
*                                                                               
FIXRTRC2 MVC   P+50(9),SPACES                                                   
         MVC   P+50(2),=C'OK'                                                   
         TM    LOGACTN,X'80'                                                    
         BZ    *+16                                                             
         MVC   P+50(9),=CL9'DELETE '                                            
         MVC   P+56(1),DELWHY                                                   
         TM    LOGACTN,X'40'                                                    
         BZ    *+10                                                             
         MVC   P+50(9),=CL9'AUTO OFF'                                           
*                                                                               
FIXRTRC3 MVC   P+60(9),=CL9'DRAFT'                                              
         TM    LOGACTN,X'10'                                                    
         BZ    *+10                                                             
         MVC   P+60(9),=CL9'ACTUAL'                                             
         TM    LOGACTN,X'20'                                                    
         BZ    *+10                                                             
         MVC   P+60(9),=CL9'ERROR '                                             
*                                                                               
FIXRTRC4 MVI   P+68,C'0'           RE=A(FIRST APPLID ELEMENT)                   
         LT    RE,AAPPLID                                                       
         BZ    FIXRTRCW                                                         
         MVC   P+70(5),3(RE)       1ST                                          
         LA    RE,11(RE)                                                        
         MVI   P+68,C'1'                                                        
         CLI   0(RE),X'24'                                                      
         BNE   FIXRTRCW                                                         
         MVC   P+76(5),3(RE)       2ND                                          
         MVI   P+68,C'2'                                                        
         LA    RE,11(RE)                                                        
         CLI   0(RE),X'24'                                                      
         BNE   FIXRTRCW                                                         
         MVC   P+82(5),3(RE)       3RD                                          
         MVI   P+67,C'>'                                                        
         MVI   P+68,C'3'                                                        
         LA    RE,11(RE)                                                        
         CLI   0(RE),X'24'                                                      
         BNE   FIXRTRCW                                                         
         MVC   P+88(5),3(RE)       4TH                                          
         MVI   P+68,C'4'                                                        
         LA    RE,11(RE)                                                        
         CLI   0(RE),X'24'                                                      
         BNE   FIXRTRCW                                                         
         MVC   P+94(5),3(RE)       5TH                                          
         MVI   P+68,C'5'                                                        
         LA    RE,11(RE)                                                        
         CLI   0(RE),X'24'                                                      
         BNE   FIXRTRCW                                                         
         MVC   P+100(5),3(RE)      6TH                                          
         MVI   P+68,C'6'                                                        
*                                                                               
FIXRTRCW GOTO1 VPRINTER            PRINT TRACE DATA                             
*                                                                               
FIXRTRCX EQU   *                                                                
*                                                                               
FIXROUT  CLI   RCWRITE,C'Y'        WRITE=YES                                    
         BNE   FIXROUT1                                                         
         TM    LOGACTN,X'30'       DID WE DO A READ FOR UPDATE                  
         BZ    FIXROUT1                                                         
         GOTO1 VDATAMGR,DMCB,DMUNLK,CTFILE,AIO1,AIO1                            
*                                                                               
FIXROUT1 PUT   OUTDATA,LOGRCD                                                   
         B     FIXRNXT                                                          
                                                                                
         EJECT                                                                  
***********************************************************************         
*PRINT ACTIONS TAKEN                                                  *         
***********************************************************************         
FIXRZ    CLI   EOL,C'Y'            TEST HAVE PRINTED LIST                       
         BE    FIXRXIT                                                          
         MVI   EOL,C'Y'            SET LIST PRINTED                             
         ZAP   LINE,=P'99'                                                      
*                                                                               
FIXRZ1   MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL16'TRM NOLU        '                                    
         L     R0,CNULL                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL16'TRM NOLU DELETED'                                    
         L     R0,DNULL                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL16'TRM PSWD        '                                    
         L     R0,CPWDT                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL16'TRM PSWD DELETED'                                    
         L     R0,DPWDT                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL16'PRT/SHT ACT     '                                    
         L     R0,CACTV                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL16'PRT/SHT ACT PRQ '                                    
         L     R0,CPWDP                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL16'PRT/SHT ACT AUTO'                                    
         L     R0,CACTVAUT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL16'PRT/SHT AUTO OFF'                                    
         L     R0,CAUTOFF                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL16'PASSIVE DELETED '                                    
         L     R0,CLOST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL16'PASSIVE PRT/SHT '                                    
         L     R0,CPASV                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL16'PASSIVE AUTO    '                                    
         L     R0,CPASVAUT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL16'PASSIVE AUTO OFF'                                    
         L     R0,CAUTPOFF                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
FIXRXIT  CLOSE OUTDATA             CLOSE OUTPUT LOG                             
*                                                                               
         XIT1                                                                   
*                                                                               
EOL      DC    CL1' '                                                           
DELWHY   DC    CL1' '                                                           
LKEY     DC    XL25'00'                                                         
*                                                                               
AAPPLID  DC    A(0)                                                             
ATRMDEF  DC    A(0)                                                             
CACTV    DC    F'0'                                                             
CACTVAUT DC    F'0'                                                             
AACTV    DC    A(ACTV)                                                          
NACTV    DC    A(ACTV)                                                          
*                                                                               
CPASV    DC    F'0'                                                             
CPASVAUT DC    F'0'                                                             
APASV    DC    A(PASV)                                                          
NPASV    DC    A(PASV)                                                          
*                                                                               
AAUTO    DC    A(AUTO)                                                          
NAUTO    DC    A(AUTO)                                                          
CLOST    DC    F'0'                                                             
CAUTOFF  DC    F'0'                                                             
CAUTPOFF DC    F'0'                                                             
CAUTTOT  DC    F'0'                                                             
CNULL    DC    F'0'                                                             
DNULL    DC    F'0'                                                             
CPWDT    DC    F'0'                                                             
DPWDT    DC    F'0'                                                             
CPWDP    DC    F'0'                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RECORDS TO BE BACKED OUT ARE IN INPUT FILE                          *         
***********************************************************************         
BKOT     NTR1  BASE=*,LABEL=*                                                   
         OPEN  (INDATA,INPUT)                                                   
         LA    R3,LOGRCD           R3=A(INPUT LOG RECORD)                       
         USING LOGRCDD,R3                                                       
         L     R6,AREC             R6=A(CTFILE RECORD)                          
         USING CTTREC,R6                                                        
*                                                                               
BKOTNXT  GET   INDATA,LOGRCD       GET NEXT INPUT FILE RECORD                   
*                                                                               
         MVC   IOKEY,LOGKEY        READ RECORD FROM CTFILE                      
         MVC   DMBITS1,DMBITS                                                   
         OI    DMBITS1,X'08'       PASS BACK DELETED RECORDS                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMBITS1,DMRDHI),CTFILE,IOKEY,IOAREA1              
         CLI   8(R1),0                                                          
         BE    BKOT10                                                           
         TM    8(R1),X'02'         TEST RECORD IS DELETED                       
         BO    BKOT10                                                           
         OI    LOGACTN,X'02'       SET DISK ERROR                               
         B     BKOTTRC                                                          
BKOT10   CLC   CTTKEY,IOKEY                                                     
         BE    *+12                                                             
         OI    LOGACTN,X'02'       SET RECORD NOT FOUND                         
         B     BKOTTRC                                                          
*                                                                               
BKOT20   TM    LOGACTN,X'80'       TEST RECORD WAS DELETED                      
         BO    BKOTPUR                                                          
         TM    LOGACTN,X'40'       TEST RECORD HAD AUTO OFF                     
         BO    BKOTAOF                                                          
         B     BKOTNXT                                                          
*                                                                               
BKOTPUR  CLI   RCMODED,C'X'        EXIT IT NOT PROCESSING DELETES               
         BE    BKOTNXT                                                          
         OI    LOGACTN,X'08'       SET DELETE ACTION REVERSED                   
         MVC   CTTSTAT,LOGSTATV                                                 
*                                                                               
BKOTPUR1 CLI   RCWRITE,C'Y'        WRITE=YES                                    
         BNE   BKOTTRC                                                          
         GOTO1 VDATAMGR,DMCB,(X'00',DMWRT),CTFILE,AIO1,AIO1                     
         CLI   8(R1),0                                                          
         BNE   *+12                                                             
         OI    LOGACTN,X'01'       SET BACKOUT DONE                             
         B     BKOTTRC                                                          
         OI    LOGACTN,X'02'       SET BACKOUT FAILED                           
         B     BKOTTRC                                                          
*                                                                               
BKOTAOF  CLI   RCMODEA,C'X'        EXIT IT NOT PROCESSING AUTO RECORDS          
         BE    BKOTNXT                                                          
         OI    LOGACTN,X'04'       SET AUTO OFF REVERSED                        
         MVC   CTTSTAT,LOGSTATV    RESTORE AUTO MODE IN STATUS BYTE             
         SR    RE,RE                                                            
         ICM   RE,3,LOGDEFD        RESTORE AUTO MODE IN ELEMENT                 
         BZ    BKOTAOF1                                                         
         AR    RE,R6                                                            
         CLI   0(RE),X'25'         TEST IF STILL A TERM DEFN ELEMENT            
         BE    *+12                                                             
         OI    LOGACTN,X'02'       SET BACKOUT FAILED                           
         B     BKOTTRC                                                          
         MVC   CTTRMAT1-CTTRMD(1,RE),LOGAT1V                                    
*                                                                               
BKOTAOF1 CLI   RCWRITE,C'Y'        WRITE=YES                                    
         BNE   BKOTTRC                                                          
         GOTO1 VDATAMGR,DMCB,(X'00',DMWRT),CTFILE,AIO1,AIO1                     
         CLI   8(R1),0                                                          
         BNE   *+12                                                             
         OI    LOGACTN,X'01'       SET BACKOUT DONE                             
         B     BKOTTRC                                                          
         OI    LOGACTN,X'02'       SET BACKOUT FAILED                           
         B     BKOTTRC                                                          
*                                                                               
BKOTTRC  CLI   RCTRACE,C'Y'        TRACE=Y                                      
         BNE   BKOTTRCX                                                         
         MVC   P,SPACES                                                         
         MVC   P(25),LOGKEY                                                     
         OC    LOGKEY+1(22),LOGKEY+1                                            
         BNZ   BKOTTRC1                                                         
         SR    R0,R0                                                            
         ICM   R0,3,LOGKEY+23      PASSIVE TERMINAL NUMBER                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   P+19,C'#'                                                        
         UNPK  P+20(5),DUB+4(4)                                                 
*                                                                               
BKOTTRC1 GOTO1 VHEXOUT,PARAMS,LOGSTATV,P+26,1,=C'TOG'                           
         GOTO1 (RF),(R1),LOGSTATF,P+29,1                                        
         OC    LOGDEFD,LOGDEFD                                                  
         BZ    BKOTTRC2                                                         
         GOTO1 (RF),(R1),LOGDEFD,P+34,2                                         
         GOTO1 (RF),(R1),LOGAT1V,P+39,1                                         
         GOTO1 (RF),(R1),LOGAT1F,P+42,1                                         
*                                                                               
BKOTTRC2 MVC   P+50(9),SPACES                                                   
         TM    LOGACTN,X'08'                                                    
         BZ    *+10                                                             
         MVC   P+50(9),=CL9'RESTORED'                                           
         TM    LOGACTN,X'04'                                                    
         BZ    *+10                                                             
         MVC   P+50(9),=CL9'AUTO ON'                                            
*                                                                               
BKOTTRC3 MVC   P+60(9),=CL9'DRAFT'                                              
         TM    LOGACTN,X'01'                                                    
         BZ    *+10                                                             
         MVC   P+60(9),=CL9'ACTUAL'                                             
         TM    LOGACTN,X'02'                                                    
         BZ    *+10                                                             
         MVC   P+60(9),=CL9'ERROR '                                             
*                                                                               
BKOTTRC4 GOTO1 VPRINTER            PRINT TRACE DATA                             
*                                                                               
BKOTTRCX EQU   *                                                                
*                                                                               
BKOTOUT  CLI   RCWRITE,C'Y'        WRITE=YES                                    
         BNE   BKOTNXT                                                          
         TM    LOGACTN,X'03'       DID WE DO A READ FOR UPDATE                  
         BZ    BKOTNXT                                                          
         GOTO1 VDATAMGR,DMCB,DMUNLK,CTFILE,AIO1,AIO1                            
         B     BKOTNXT                                                          
*                                                                               
BKOTEOF  CLOSE INDATA                                                           
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
* INITIALIZE                                                          *         
***********************************************************************         
INIT     NTR1  BASE=*,LABEL=*                                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         MVI   DMBITS,0                                                         
         LA    R3,CARD             R3=A(INPUT CONTROL CARD)                     
*                                                                               
INIT010  GOTO1 VCARDS,DMCB,(R3),=C'RE00'                                        
         CLI   0(R3),C'*'          COMMENT                                      
         BE    INIT010                                                          
         CLC   0(2,R3),=C'/*'      SYSIN EOF                                    
         BE    INIT100                                                          
*                                                                               
         MVC   P(80),0(R3)         PRINT CONTROL CARD                           
         GOTO1 VPRINTER                                                         
*                                                                               
         CLC   0(6,R3),=C'DDSIO='                                               
         BNE   INIT014                                                          
         L     RF,=V(DDSIO)        OVERRIDE DDSIO NAME                          
         MVC   0(8,RF),6(R3)                                                    
         B     INIT010                                                          
*                                                                               
INIT014  CLC   0(7,R3),=C'DSPACE='                                              
         BNE   INIT018                                                          
         L     RF,=A(SSB)          SET DSPACE ID IN SSB                         
         MVC   SSODSPAC-SSOOFF(1,RF),7(R3)                                      
         B     INIT010                                                          
*                                                                               
INIT018  CLC   0(6,R3),=C'WRITE='                                               
         BNE   INIT022                                                          
         MVC   RCWRITE,6(R3)                                                    
         CLI   RCWRITE,C'Y'                                                     
         BE    INIT010                                                          
         CLI   RCWRITE,C'N'                                                     
         BE    INIT010                                                          
         B     INIT090                                                          
*                                                                               
INIT022  CLC   0(6,R3),=C'TRACE='                                               
         BNE   INIT024                                                          
         MVC   RCTRACE,6(R3)                                                    
         B     INIT010                                                          
*                                                                               
INIT024  CLC   0(5,R3),=C'MODE='                                                
         BNE   INIT026                                                          
         MVC   RCMODE(3),5(R3)                                                  
         CLI   RCMODE,C'F'         MODE FIX RECORDS (DEFAULT)                   
         BE    INIT024A                                                         
         CLI   RCMODE,C'B'         MODE BACKOUT CHANGES                         
         BNE   INIT090                                                          
INIT024A CLI   RCMODE+1,C' '       PROCESS DELETES (DEFAULT)                    
         BE    INIT024B                                                         
         CLI   RCMODE+1,C'D'       PROCESS DELETES                              
         BE    INIT024B                                                         
         CLI   RCMODE+1,C'X'       DONT PROCESS DELETES                         
         BNE   INIT090                                                          
INIT024B CLI   RCMODE+2,C' '       PROCESS AUTO OFF (DEFAULT)                   
         BE    INIT010                                                          
         CLI   RCMODE+2,C'A'       PROCESS AUTO OFF                             
         BE    INIT010                                                          
         CLI   RCMODE+2,C'X'       DONT PROCESS AUTO OFF                        
         BE    INIT010                                                          
         B     INIT090             ERROR                                        
*                                                                               
INIT026  CLC   0(8,R3),=C'MAXNULL='                                             
         BNE   *+12                                                             
         LA    RE,MAXNULL                                                       
         B     INIT026A                                                         
         CLC   0(8,R3),=C'MAXPSWD='                                             
         BNE   *+12                                                             
         LA    RE,MAXPSWD                                                       
         B     INIT026A                                                         
         CLC   0(8,R3),=C'MAXLOST='                                             
         BNE   *+12                                                             
         LA    RE,MAXLOST                                                       
         B     INIT026A                                                         
         CLC   0(8,R3),=C'MAXAUTO='                                             
         BNE   INIT090                                                          
         LA    RE,MAXAUTO                                                       
INIT026A MVC   DUB(6),=C'000000'   MAXAAAA=NNNNNN                               
         MVZ   DUB(6),8(R3)                                                     
         CLC   DUB(6),=C'000000'                                                
         BNE   INIT090             MUST BE 6 DIGITS NUMERIC                     
         PACK  DUB,8(6,R3)                                                      
         CVB   R0,DUB                                                           
         ST    R0,0(RE)                                                         
         B     INIT010                                                          
*                                                                               
INIT090  MVC   P(30),=CL30'ERROR: INVALID CONTROL CARD'                         
         GOTO1 VPRINTER                                                         
         DC    H'0'                                                             
*                                                                               
INIT100  GOTO1 VPRINTER            END OF SYSIN CARDS                           
*                                                                               
         CLI   RCWRITE,C'Y'        WRITE=Y                                      
         BNE   *+12                                                             
         MVI   FCTFILE,C'U'        YES-OPEN CTFILE FOR UPDATE                   
         MVI   DMBITS,X'80'        SET READ FOR UPDATE BITS                     
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMOPEN  ',=C'CONTROL ',FLIST,AIO1               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*&&US                                                                           
         CLI   RCWRITE,C'Y'        ENQUEUE CONTROL IF WRITING                   
         BNE   INIT110                                                          
         GOTO1 VDATAMGR,DMCB,=C'ENQCTL ',(C'E',=C'CTRL')                        
         TM    8(R1),X'04'                                                      
         JZ    *+2                                                              
*&&                                                                             
INIT110  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FINISHED, CLOSE THINGS UP                                           *         
***********************************************************************         
DONE     NTR1  BASE=*,LABEL=*                                                   
*&&US                                                                           
         CLI   RCWRITE,C'Y'        DEQUEUE CONTROL IF WRITING                   
         BNE   DONE010                                                          
         GOTO1 VDATAMGR,DMCB,=C'ENQCTL ',(C'D',=C'CTRL')                        
*&&                                                                             
DONE010  GOTO1 VDATAMGR,DMCB,=C'DMCLSE ',=C'CONTROL ',FLIST,AIO1                
*                                                                               
DONEX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS & LTORG                                                   *         
***********************************************************************         
VDATAMGR DC    V(DATAMGR)                                                       
VCARDS   DC    V(CARDS)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHEXOUT  DC    V(HEXOUT)                                                        
VHELLO   DC    V(HELLO)                                                         
VADDAY   DC    V(ADDAY)                                                         
*                                                                               
DMREAD   DC    CL8'DMREAD '                                                     
DMRDHI   DC    CL8'DMRDHI '                                                     
DMRSEQ   DC    CL8'DMRSEQ '                                                     
DMWRT    DC    CL8'DMWRT  '                                                     
DMADD    DC    CL8'DMADD  '                                                     
DMUNLK   DC    CL8'DMUNLK '                                                     
CTFILE   DC    CL8'CTFILE '                                                     
*                                                                               
MAXNULL  DC    F'999999'           MAX NULL LUID RECORDS TO PROCESS             
MAXPSWD  DC    F'999999'           MAX TERM PSWD RECORDS TO PROCESS             
MAXAUTO  DC    F'999999'           MAX AUTO PRTR RECORDS TO PROCESS             
MAXLOST  DC    F'999999'           MAX LOST PRTR PASSIVE TO PROCESS             
*                                                                               
CTENQU   DC    C'N'                ENQUEUE CTFILE                               
RCWRITE  DC    C'N'                WRITE=Y/N                                    
RCTRACE  DC    C'N'                TRACE=Y/N                                    
*                                                                               
RCMODE   DC    C'F'                F=FIX RECORDS,B=BACKOUT FIX                  
RCMODED  DC    C'D'                D=DELETES                                    
RCMODEA  DC    C'A'                A=AUTO MODE                                  
*                                                                               
         DC    0D                                                               
         DC    C'**FLST**'                                                      
FLIST    DS    0CL8                                                             
FCTFILE  DC    C'NCTFILE '                                                      
         DC    C'X       '                                                      
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
INDATA   DCB   DDNAME=INDATA,                                          X        
               DSORG=PS,                                               X        
               EODAD=BKOTEOF,                                          X        
               LRECL=80,                                               X        
               RECFM=FB,                                               X        
               MACRF=GM                                                         
                                                                                
         DS    0D                                                               
OUTDATA  DCB   DDNAME=OUTDATA,                                         X        
               DSORG=PS,                                               X        
               LRECL=80,                                               X        
               BLKSIZE=8000,                                           X        
               RECFM=FB,                                               X        
               MACRF=PM                                                         
                                                                                
         DC    0D                                                               
         DC    C'*UTL*UTL'                                                      
UTL      DC    F'0',X'0A',XL3'00',XL252'00'                                     
         DC    0D                                                               
         DC    C'*SSB*SSB'                                                      
SSB      DC    X'0000'                                                          
         DC    X'FF'                                                            
*&&US*&& DC    AL1(SSOSNRCV)                                                    
*&&UK*&& DC    AL1(SSOSNRCV+SSOSGALO+SSOSLOCK)                                  
         DC    1024X'00'                                                        
*                                                                               
         DS    0D                                                               
         DC    C'**ACTV**'                                                      
ACTV     DC    20000XL10'00'                                                    
         DC    X'FFFFFFFF'                                                      
*                                                                               
         DS    0D                                                               
         DC    C'**PASV**'                                                      
PASV     DC    20000XL10'00'                                                    
         DC    X'FFFFFFFF'                                                      
*&&UK                                                                           
         DS    0D                                                               
         DC    CL8'**AUTO**'                                                    
AUTO     DC    CL8'        ',CL2' '                                             
         DC    CL8'DBDF15FS',CL2' '                                             
         DC    CL8'DBLO0BAS',CL2' '                                             
         DC    CL8'DGLM100P',CL2' '                                             
         DC    CL8'DSDF401S',CL2' '                                             
         DC    CL8'DSDF40CP',CL2' '                                             
         DC    CL8'GSLM11EP',CL2' '                                             
         DC    CL8'GSLM11FP',CL2' '                                             
         DC    CL8'GZHB30AS',CL2' '                                             
         DC    CL8'JSHB322P',CL2' '                                             
         DC    CL8'JSHB323P',CL2' '                                             
         DC    CL8'JSHB326P',CL2' '                                             
         DC    CL8'JSHB327P',CL2' '                                             
         DC    CL8'JSHB328P',CL2' '                                             
         DC    CL8'JSHB32AP',CL2' '                                             
         DC    CL8'JSHB32CP',CL2' '                                             
         DC    CL8'JSHB32EP',CL2' '                                             
         DC    CL8'PUFR200S',CL2' '                                             
         DC    CL8'TPDF303S',CL2' '                                             
         DC    CL8'TPDF305S',CL2' '                                             
         DC    CL8'TPDF309S',CL2' '                                             
         DC    CL8'TPDF30FS',CL2' '                                             
         DC    CL8'TPDF310S',CL2' '                                             
         DC    CL8'TPDF320P',CL2' '                                             
         DC    CL8'TPDF324P',CL2' '                                             
         DC    CL8'TPDF353S',CL2' '                                             
         DC    CL8'VXDF200S',CL2' '                                             
         DC    CL8'VXDF204P',CL2' '                                             
         DC    CL8'XLDF21FS',CL2' '                                             
         DC    XL4'FFFFFFFF'                                                    
*&&                                                                             
*&&US                                                                           
         DS    0D                                                               
         DC    CL8'**AUTO**'                                                    
AUTO     DC    CL8'        ',CL2' '                                             
         DC    CL8'BSTO116S',CL2' '                                             
         DC    CL8'CHEKP11S',CL2' '                                             
         DC    CL8'CHEKP12S',CL2' '                                             
         DC    CL8'CHEKT01S',CL2' '                                             
         DC    CL8'DMN12FEP',CL2' '                                             
         DC    CL8'DMN12FFP',CL2' '                                             
         DC    CL8'H7OS496P',CL2' '                                             
         DC    CL8'H7OSA02P',CL2' '                                             
         DC    CL8'H7OSA08P',CL2' '                                             
         DC    CL8'H7OSA09P',CL2' '                                             
         DC    CL8'H7OSA10P',CL2' '                                             
         DC    CL8'IPG1CFFS',CL2' '                                             
         DC    CL8'JWOS2D7P',CL2' '                                             
         DC    CL8'MIVC53CP',CL2' '                                             
         DC    CL8'MIVC53DP',CL2' '                                             
         DC    CL8'MIVC53FP',CL2' '                                             
         DC    CL8'MIVC542P',CL2' '                                             
         DC    CL8'MIVC544P',CL2' '                                             
         DC    CL8'MUNC1FAP',CL2' '                                             
         DC    CL8'NBOS4DAP',CL2' '                                             
         DC    CL8'NBOS4DCP',CL2' '                                             
         DC    CL8'NBOS4E9P',CL2' '                                             
         DC    CL8'NBOS4EAP',CL2' '                                             
         DC    CL8'NBOS4EBP',CL2' '                                             
         DC    CL8'NBOS4ECP',CL2' '                                             
         DC    CL8'NBOS4F5P',CL2' '                                             
         DC    CL8'NBOS4F7P',CL2' '                                             
         DC    CL8'NBOS4F9P',CL2' '                                             
         DC    CL8'OMNY3FCS',CL2' '                                             
         DC    CL8'OMNY3FDS',CL2' '                                             
         DC    CL8'OMNY3FES',CL2' '                                             
         DC    CL8'OMNY3FFS',CL2' '                                             
         DC    CL8'OUOS27AS',CL2' '                                             
         DC    CL8'RPOS2FAP',CL2' '                                             
         DC    CL8'RPOS2FEP',CL2' '                                             
         DC    CL8'RPOS2FFP',CL2' '                                             
         DC    CL8'TPCA13FS',CL2' '                                             
         DC    CL8'TPCH191S',CL2' '                                             
         DC    CL8'TPCH192S',CL2' '                                             
         DC    CL8'TPCH193S',CL2' '                                             
         DC    XL4'FFFFFFFF'                                                    
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
*                                                                               
AIO1     DS    A                   A(RECORD)                                    
AREC     DS    A                   A(RECORD)                                    
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAG     DS    X                                                                
DMCB     DS    6F                                                               
PARAMS   DS    6F                                                               
WORK     DS    CL64                                                             
*                                                                               
DMBITS   DS    X                                                                
DMBITS1  DS    X                                                                
         DS    X                                                                
*                                                                               
TODAY    DS    CL3                 YYMMDD PWOS                                  
TODAYC   DS    H                   TODAY COMP                                   
TODAY0   DS    CL6                 TODAY YYMMDD                                 
DATE0    DS    CL6                 DATE YYMMDD                                  
*                                                                               
LOGRCD   DS    CL80                                                             
CARD     DS    CL80                                                             
IOKEY    DS    CL42                                                             
IOKEYSV  DS    CL42                                                             
*                                                                               
ELEM     DS    XL256               GENERAL USE ELEMENT                          
*                                                                               
         DS    CL8                                                              
IOAREA1  DS    2048C               IO AREA 1                                    
*                                                                               
SPARE    DS    1024X                                                            
WORKX    EQU   *                                                                
                                                                                
***********************************************************************         
* DSECT FOR DATA IN LOG RECORD                                        *         
***********************************************************************         
LOGRCDD  DSECT                                                                  
LOGKEY   DS    CL25                CTFILE KEY                                   
*                                                                               
LOGACTN  DS    X                   X'80'=DELETE ACTION                          
*                                  X'40'=AUTO OFF ACTION                        
*                                  X'20'=ACTION FAILED                          
*                                  X'10'=ACTION DONE                            
*                                  X'08'=DELETE REVERSED                        
*                                  X'04'=AUTO OFF REVERSED                      
*                                  X'02'=BACKOUT FAILED                         
*                                  X'01'=BACKOUT DONE                           
*                                                                               
LOGSTATV DS    X                   RECORD STATUS                                
LOGAT1V  DS    X                   TERMINAL DEFINITION ATTRIBUTE 1              
LOGDEFD  DS    XL2                 TERMINAL DEFINITION ELEMENT DISP             
LOGSTATF DS    X                   RECORD STATUS AFTER FIX                      
LOGAT1F  DS    X                   ATTRIBUTE 1 AFTER FIX                        
*                                                                               
LOGINFO  DS    X                   X'80'=MISSING ACTIVE ENTRY                   
*                                  X'40'=AUTO TURNED OFF                        
*                                  X'20'=LUID IN AUTO LIST                      
*                                  X'10'=                                       
*                                  X'08'=AUTO MODE                              
*                                  X'04'=PRINTERQ ELEMENT PRESENT               
*                                  X'02'=SHUTTLE                                
*                                  X'01'=PRINTER                                
*                                                                               
LOGSPARE DS    XL47                N/D                                          
LOGRCDL  EQU   80                                                               
                                                                                
* CTGENFILE                                                                     
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
                                                                                
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTFIX3    01/11/18'                                      
         END                                                                    
