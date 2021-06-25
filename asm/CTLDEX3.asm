*          DATA SET CTLDEX3    AT LEVEL 001 AS OF 12/29/17                      
*PHASE CTLDEX3A                                                                 
         TITLE 'CTLDEX3 - CTFILE - TERMINAL PRINTER AUTO RECORDS'               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
*                                   X'FE'= CHANGED RECORD (RECOVERY)            
*                                   X'FD'= NEW RECORD (RECOVERY)                
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= ONLY I/S FILE RECS IN P1             
*                                   X'10'= SPECIAL I/S POINTER IN P9            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
* P7=A(CARDS)                                                                   
* P8=A(PEELDATE)                                                                
* P9=A(ISREC)                                                                   
* P10=A(PARMTBL)                                                                
                                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,CTLDEX1                                              
         USING WORKD,RC                                                         
                                                                                
* CONTROL FLOW LOGIC                                                            
*                                                                               
CTXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     R2,VLDDEFN          R2=A(FILE DEFINITION)                        
         USING LDDEFND,R2                                                       
*                                                                               
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    CTXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    CTXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    CTXR                                                             
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    CTXEOF                                                           
         B     CTXIT               EXIT IF UNKNOWN                              
*                                                                               
CTXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     CTXIT                                                            
*                                                                               
CTXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     CTXIT                                                            
*                                                                               
CTXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     CTXIT                                                            
*                                                                               
CTXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     CTXIT                                                            
*                                                                               
CTXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     CTXIT                                                            
*                                                                               
CTXCHG   L     R1,APARM            CHANGED RECORD (FOR RECOVERY)                
         MVI   0(R1),X'FE'                                                      
         MVI   8(R1),0                                                          
         B     CTXIT                                                            
*                                                                               
CTXADD   L     R1,APARM            ADDED RECORD (FOR RECOVERY)                  
         MVI   0(R1),X'FD'                                                      
         MVI   8(R1),0                                                          
         B     CTXIT                                                            
*                                                                               
CTXIT    XMOD1 1                                                                
                                                                                
*                                                                               
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
* PARAM=X WHERE X IS THE TRACE OPTION N=NO,Y=YES,S=SUMMARY                      
*                                                                               
CTXINIT  XR    RF,RF               EXTRACT AND SAVE PARAMETER CARD              
         ICM   RF,7,APARAMC+1                                                   
         MVC   PARAMC,0(RF)                                                     
*                                                                               
CTXI1    LA    RF,0(RF)            LOOK FOR TRACE OPTION ON PARAM CARD          
         LA    R0,50                                                            
CTXI1A   CLC   0(6,RF),=C'TRACE='                                               
         BE    CTXI2                                                            
         LA    RF,1(RF)                                                         
         BCT   R0,CTXI1A                                                        
         B     CTXIT                                                            
*                                                                               
CTXI2    MVC   TRACE,6(RF)         SET TRACE OPTION                             
         B     CTXIT                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED              *         
* TERMINAL RECORDS THAT ARE PRINTERS OR SHUTTLES                      *         
***********************************************************************         
CTXR     SR    R3,R3               R3=A(RECORD)                                 
         ICM   R3,7,AREC+1                                                      
         USING CTTREC,R3                                                        
         CLI   0(R3),C'T'          TEST TERMINAL RECORD                         
         BL    CTXRX                                                            
         BH    CTXRZ               PRINT RESULTS IF NO MORE TERMINALS           
*                                                                               
CTXR0    TM    CTTSTAT,X'80'       IGNORE DELETED                               
         BO    CTXRX                                                            
         MVI   DELWHY,C' '                                                      
         XC    LOGRCD,LOGRCD                                                    
         MVC   LOGKEY,CTTKEY                                                    
         MVC   LOGSTATV,CTTSTAT                                                 
         MVC   LOGSTATF,CTTSTAT                                                 
         MVC   LOGSPARE,SPACES                                                  
*                                                                               
         OC    CTTKTID,CTTKTID     TEST NULL LUID                               
         BNZ   CTXR0A                                                           
         L     RF,CNULL            BUMP NULL TERMINAL RECORDS                   
         AHI   RF,1                                                             
         ST    RF,CNULL                                                         
         L     RF,DNULL            BUMP NULL TERMINAL DELETED                   
         AHI   RF,1                                                             
         ST    RF,DNULL                                                         
         MVI   DELWHY,C'1'                                                      
         B     CTXPRG                                                           
CTXR0A   OC    CTTKPASS,CTTKPASS   TEST IF PASSWORD IN KEY                      
         BZ    CTXRSC0                                                          
         TM    CTTSTAT,X'04'       TEST PRINTER TERMINAL                        
         BO    CTXR0B                                                           
         L     RF,CPWDT            BUMP TERMINAL PASSWORD RECORDS               
         AHI   RF,1                                                             
         ST    RF,CPWDT                                                         
         L     RF,DPWDT            BUMP TERMINAL PASSWORD DELETED               
         AHI   RF,1                                                             
         ST    RF,DPWDT                                                         
         MVI   DELWHY,C'2'                                                      
         B     CTXPRG                                                           
*                                                                               
CTXR0B   CLI   CTTKTID-1,C'P'      TEST PRINTER PASSIVE                         
         BE    CTXRSC0                                                          
         L     RF,CPWDP            BUMP PRINTER QUEUE EXTRA RECORDS             
         AHI   RF,1                                                             
         ST    RF,CPWDP                                                         
*                                                                               
CTXRSC0  TM    CTTSTAT,X'04'       TEST PRINTER TERMINAL                        
         BZ    CTXRX                                                            
*                                                                               
         LA    R0,100              SCAN PRINTER TERMINAL RECORD                 
         LA    RE,CTTDATA                                                       
         XC    DUB,DUB                                                          
         XC    ATRMDEF,ATRMDEF                                                  
         CLI   CTTKTID+7,C'S'                                                   
         BNE   *+8                                                              
         OI    DUB,X'08'+X'02'     SET AUTO AND SHUTTLE                         
*                                                                               
CTXRSC1  CLI   0(RE),0             TEST END OF RECORD                           
         BE    CTXRSC5                                                          
CTXRSC2  CLI   0(RE),X'25'         TEST TERMINAL DEFN ELEMENET                  
         BNE   CTXRSC3                                                          
         ST    RE,ATRMDEF                                                       
         LR    R0,RE                                                            
         SR    R0,R3                                                            
         STCM  R0,3,LOGDEFD        SAVE DISPLACEMENT OF ELEMENT                 
         MVC   LOGAT1V,CTTRMAT1-CTTRMD(RE)                                      
         TM    CTTRMAT1-CTTRMD(RE),X'02'                                        
         BZ    *+8                                                              
         OI    DUB,X'08'           SET AUTO MODE                                
         TM    CTTRMDEV-CTTRMD(RE),X'82'                                        
         BNO   *+8                                                              
         OI    DUB,X'02'           SET SHUTTLE                                  
         TM    CTTRMDEV-CTTRMD(RE),X'81'                                        
         BNO   *+8                                                              
         OI    DUB,X'01'           SET PRINTER                                  
         B     CTXRSC4                                                          
CTXRSC3  CLI   0(RE),X'29'         TEST PRINTERQ ELEMENT                        
         BNE   CTXRSC4                                                          
         OI    DUB,X'04'           SET PRINTERQ ELEMENT PRESENT                 
         B     CTXRSC5                                                          
CTXRSC4  SR    RF,RF               BUMP TO NEXT ELEMENT                         
         ICM   RF,1,1(RE)                                                       
         BZ    CTXRSC5                                                          
         AR    RE,RF                                                            
         BCT   R0,CTXRSC1                                                       
CTXRSC5  EQU   *                                                                
*                                                                               
CTXRSC6  TM    CTTSTAT,X'08'       TEST AUTO MODE                               
         BZ    CTXRSC9                                                          
         L     RE,AAUTO                                                         
CTXRSC7  CLC   0(4,RE),=F'0'       SEARCH AUTO LIST                             
         BE    CTXRSC9                                                          
         CLC   0(4,RE),=X'FFFFFFFF'                                             
         BE    CTXRSC9                                                          
         CLC   0(8,RE),0(RF)                                                    
         BE    CTXRSC8                                                          
         LA    RE,10(RE)                                                        
         B     CTXRSC7                                                          
CTXRSC8  OI    DUB,X'20'           SET IN AUTO LIST                             
CTXRSC9  MVC   LOGINFO,DUB         SET LOG INFO FLAGS                           
*                                                                               
         CLI   CTTKTID-1,C'P'      TEST PRINTER PASSIVE                         
         BE    CTXR2                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS ACTIVE TERMINAL PRINTER/SHUTTLE RECORDS                     *         
***********************************************************************         
CTXR1    L     RF,CACTV            BUMP ACTV COUNT                              
         AHI   RF,1                                                             
         ST    RF,CACTV                                                         
*                                                                               
         L     RF,NACTV            MOVE TO ACTV LIST                            
         CLC   0(4,RF),=X'FFFFFFFF'                                             
         BE    CTXKEEP                                                          
         MVC   0(8,RF),CTTKTID                                                  
         MVC   8(1,RF),CTTSTAT                                                  
         MVC   9(1,RF),DUB                                                      
         LA    RF,10(RF)                                                        
         ST    RF,NACTV                                                         
         AHI   RF,-10                                                           
*                                                                               
         TM    CTTSTAT,X'08'       EXIT IF NOT IN AUTO MODE                     
         BZ    CTXKEEP                                                          
         L     R0,CACTVAUT         BUMP ACTV AUTO COUNT                         
         AHI   R0,1                                                             
         ST    R0,CACTVAUT                                                      
         L     R0,CACTV            DECR ACTV COUNT                              
         AHI   R0,-1                                                            
         ST    R0,CACTV                                                         
         L     RE,AAUTO            RE=A(KNOWN ACTIVE AUTO LIST)                 
*                                                                               
CTXR1B   CLC   0(4,RE),=F'0'       SEARCH LIST OF KNOWN AUTO PRINTERS           
         BE    CTXR1C                                                           
         CLC   0(4,RE),=X'FFFFFFFF'                                             
         BE    CTXR1C                                                           
         CLC   0(8,RE),0(RF)       EXIT IF KNOWN ACTIVE PRINTER/SHUTTLE         
         BE    CTXKEEP                                                          
         LA    RE,10(RE)                                                        
         B     CTXR1B                                                           
*                                                                               
CTXR1C   OI    9(RF),X'40'         SET AUTO TURNED OFF                          
         OI    LOGINFO,X'40'                                                    
         NI    CTTSTAT,255-X'08'                                                
         MVC   LOGSTATF,CTTSTAT                                                 
         LT    RE,ATRMDEF                                                       
         BZ    *+14                                                             
         NI    CTTRMAT1-CTTRMD(RE),255-X'02'                                    
         MVC   LOGAT1F,CTTRMAT1-CTTRMD(RE)                                      
         L     RF,CAUTOFF          BUMP AUTO TURNED OFF COUNT                   
         AHI   RF,1                                                             
         ST    RF,CAUTOFF                                                       
         B     CTXKEP                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS PASSIVE TERMINAL PRINTER/SHUTTLE RECORDS                    *         
***********************************************************************         
CTXR2    L     RF,CPASV            BUMP PASV COUNT                              
         AHI   RF,1                                                             
         ST    RF,CPASV                                                         
*                                                                               
         L     RF,NPASV            MOVE TO PASV LIST                            
         CLC   0(4,RF),=X'FFFFFFFF'                                             
         BE    CTXKEEP                                                          
         MVC   0(8,RF),CTTKTID                                                  
         MVC   8(1,RF),CTTSTAT                                                  
         MVC   9(1,RF),DUB                                                      
         LA    RF,10(RF)                                                        
         ST    RF,NPASV                                                         
*                                                                               
         TM    CTTSTAT,X'08'       TEST IF AUTO MODE                            
         BZ    CTXR2E                                                           
         L     R0,CPASVAUT         BUMP PASV AUTO COUNT                         
         AHI   R0,1                                                             
         ST    R0,CPASVAUT                                                      
         L     R0,CPASV            DECR PASV COUNT                              
         AHI   R0,-1                                                            
         ST    R0,CPASV                                                         
*                                                                               
CTXR2E   L     RE,AACTV            RE=A(FRST ACTV ENTRY)                        
         L     RF,NPASV                                                         
         AHI   RF,-10              RF=A(LAST PASV ENTRY)                        
CTXR2F   CLC   0(4,RE),=F'0'       END OF LIST                                  
         BE    CTXR2H                                                           
         CLC   0(4,RE),=X'FFFFFFFF'                                             
         BE    CTXR2H                                                           
         CLC   0(8,RE),0(RF)                                                    
         BE    CTXR2J                                                           
CTXR2G   LA    RE,10(RE)                                                        
         B     CTXR2F                                                           
*                                                                               
CTXR2H   OI    9(RF),X'80'         FLAG AS MISSING ACTV ENTRY                   
         OI    LOGINFO,X'80'                                                    
         L     RF,CLOST            BUMP LOST COUNT                              
         AHI   RF,1                                                             
         ST    RF,CLOST                                                         
         TM    CTTSTAT,X'08'       TEST IF AUTO MODE                            
         BZ    CTXR2H1                                                          
         L     R0,CPASVAUT         DECR PASV AUTO COUNT                         
         AHI   R0,-1                                                            
         ST    R0,CPASVAUT                                                      
         MVI   DELWHY,C'3'                                                      
         B     CTXPRG                                                           
CTXR2H1  L     R0,CPASV            DECR PASV COUNT                              
         AHI   R0,-1                                                            
         ST    R0,CPASV                                                         
         MVI   DELWHY,C'3'                                                      
         B     CTXPRG              PURGE PRINTER PASSIVE                        
*                                                                               
CTXR2J   TM    CTTSTAT,X'08'       TEST AUTO MODE                               
         BZ    CTXR2N                                                           
         L     RE,AAUTO                                                         
CTXR2K   CLC   0(4,RE),=F'0'       SEARCH AUTO LIST                             
         BE    CTXR2L                                                           
         CLC   0(4,RE),=X'FFFFFFFF'                                             
         BE    CTXR2L                                                           
         CLC   0(8,RE),0(RF)                                                    
         BE    CTXR2M                                                           
         LA    RE,10(RE)                                                        
         B     CTXR2K                                                           
*                                                                               
CTXR2L   OI    9(RF),X'40'         SET AUTO TURNED OFF                          
         OI    LOGINFO,X'40'                                                    
         NI    CTTSTAT,255-X'08'                                                
         MVC   LOGSTATF,CTTSTAT                                                 
         LT    RE,ATRMDEF                                                       
         BZ    *+14                                                             
         NI    CTTRMAT1-CTTRMD(RE),255-X'02'                                    
         MVC   LOGAT1F,CTTRMAT1-CTTRMD(RE)                                      
         L     RF,CAUTPOFF         BUMP AUTO PASSIVE TURNED OFF COUNT           
         AHI   RF,1                                                             
         ST    RF,CAUTPOFF                                                      
         B     CTXKEP                                                           
CTXR2M   EQU   *                                                                
*                                                                               
CTXR2N   EQU   *                                                                
*                                                                               
CTXRX    MVC   LKEY,0(R3)                                                       
         B     CTXKEEP                                                          
         EJECT                                                                  
***********************************************************************         
*PRINT ACTIONS TAKEN                                                  *         
***********************************************************************         
CTXRZ    CLI   EOL,C'Y'            TEST HAVE PRINTED LIST                       
         BE    CTXKEEP                                                          
         MVI   EOL,C'Y'            SET LIST PRINTED                             
*                                                                               
CTXRZ1   MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL17'TRM NOLU        '                                    
         L     R0,CNULL                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL17'TRM NOLU DELETED'                                    
         L     R0,DNULL                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL17'TRM PSWD        '                                    
         L     R0,CPWDT                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL17'TRM PSWD DELETED'                                    
         L     R0,DPWDT                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL17'PRT/SHT ACT     '                                    
         L     R0,CACTV                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL17'PRT/SHT ACT PRQ '                                    
         L     R0,CPWDP                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL17'PRT/SHT ACT AUTO'                                    
         L     R0,CACTVAUT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL17'PRT/SHT AUTO OFF'                                    
         L     R0,CAUTOFF                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL17'PASSIVE DELETED '                                    
         L     R0,CLOST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL17'PASSIVE PRT/SHT '                                    
         L     R0,CPASV                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL17'PASSIVE AUTO    '                                    
         L     R0,CPASVAUT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(16),=CL17'PASSIVE AUTO OFF'                                    
         L     R0,CAUTPOFF                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(9),DUB                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         CLI   TRACE,C'S'          TRACE=SUMMARY                                
         BNE   CTXKEEP                                                          
         GOTO1 VPRINTER                                                         
         MVC   P(21),=C'LUID      REC ST ACTN'                                  
         GOTO1 VPRINTER                                                         
         MVC   P(21),=C'--------  --- -- ----'                                  
         GOTO1 VPRINTER                                                         
         L     R4,APASV            R4=A(PASSIVE LIST)                           
*                                                                               
CTXRZ4   CLC   0(4,R4),=F'0'       SEARCH PASSIVE LIST                          
         BE    CTXKEEP                                                          
         CLC   0(4,R4),=X'FFFFFFFF'                                             
         BE    CTXKEEP                                                          
         MVC   P,SPACES                                                         
         MVC   P(8),0(R4)                                                       
*                                                                               
         TM    9(R4),X'01'         TEST PRINTER                                 
         BZ    *+8                                                              
         MVI   P+10,C'P'                                                        
         TM    9(R4),X'02'         TEST SHUTTLE                                 
         BZ    *+8                                                              
         MVI   P+10,C'S'                                                        
         TM    9(R4),X'08'         TEST AUTO                                    
         BZ    *+8                                                              
         MVI   P+11,C'A'                                                        
         TM    9(R4),X'04'         TEST PRINTERQ ELEMENTS PRESENT               
         BZ    *+8                                                              
         MVI   P+12,C'Q'                                                        
*                                                                               
         TM    8(R4),X'04'         TEST PRINTER IN STATUS BYTE                  
         BZ    *+8                                                              
         MVI   P+14,C'P'                                                        
         TM    8(R4),X'08'         TEST AUTO IN STATUS BYTE                     
         BZ    *+8                                                              
         MVI   P+15,C'A'                                                        
*                                                                               
         CLC   P+7(1),P+10         TEST LAST CHR OF LUID WITH DEVICE            
         BE    *+8                                                              
         MVI   P+9,C'<'            FLAG LUID NAME MISMATCH                      
         CLI   P+7,C'P'                                                         
         BE    CTXRZ5                                                           
         CLI   P+7,C'S'                                                         
         BE    CTXRZ5                                                           
         MVI   P+9,C'>'                                                         
*                                                                               
CTXRZ5   MVC   P+17(8),SPACES                                                   
         TM    9(R4),X'80'         TEST DELETED LOST PASSIVE                    
         BZ    *+8                                                              
         MVI   P+17,C'X'                                                        
         TM    9(R4),X'40'         TEST AUTO TURNED OFF                         
         BZ    *+8                                                              
         MVI   P+19,C'-'                                                        
*                                                                               
         CLI   P+17,C'X'           DONT PRINT DELETED                           
         BE    CTXRZ9                                                           
         CLI   P+15,C'A'           DONT PRINT NON-AUTO                          
         BNE   CTXRZ9                                                           
*                                                                               
         CLI   P+15,C'A'           TEST AUTO                                    
         BNE   CTXRZ8                                                           
         TM    9(R4),X'40'         TEST AUTO TURNED OFF                         
         BO    CTXRZ7                                                           
         MVI   P+19,C'+'           FLAG AUTO THAT WE LEFT ON                    
         MVC   P+21(17),=C'AUTO MODE LEFT ON'                                   
         B     CTXRZ8                                                           
CTXRZ7   MVC   P+21(17),=C'AUTO TURNED OFF  '                                   
*                                                                               
CTXRZ8   GOTO1 VPRINTER            PRINT PASSIVE WITH ACTIVITY                  
*                                                                               
CTXRZ9   LA    R4,10(R4)                                                        
         B     CTXRZ4                                                           
         EJECT                                                                  
***********************************************************************         
* PRINT TRACE OF RECORDS CHANGED                                      *         
***********************************************************************         
CTXPRG   OI    LOGACTN,X'80'       SET PURGE IN TRACE LOG DATA                  
         B     FIXRTRC                                                          
CTXKEP   OI    LOGACTN,X'40'       SET AUTO OFF IN TRACE LOG DATA               
*                                                                               
FIXRTRC  CLI   TRACE,C'Y'          TRACE=Y                                      
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
FIXRTRC1 GOTO1 LHEXOUT,PARAMS,LOGSTATV,P+26,1,=C'TOG'                           
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
         TM    LOGACTN,X'80'                                                    
         BZ    *+16                                                             
         MVC   P+50(9),=CL9'DELETED'                                            
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
FIXRTRCW GOTO1 VPRINTER            PRINT TRACE DATA                             
*                                                                               
FIXRTRCX EQU   *                                                                
*                                                                               
         TM    LOGACTN,X'80'       TEST IF PURGED RECORD                        
         BO    CTXPURGE                                                         
         TM    LOGACTN,X'40'       TEST IS SET AUTO OFF                         
         BO    CTXKEEP                                                          
         EJECT                                                                  
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*                                                                               
CTXRET   L     R3,AREC             POINT TO LAST RECORD                         
         B     CTXPURGE                                                         
                                                                                
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
CTXEOF   B     CTXIT                                                            
         EJECT                                                                  
PARAMC   DC    CL80' '                                                          
AGENCY   DC    CL2' '                                                           
TRACE    DC    CL1'Y'                                                           
EOL      DC    CL1' '                                                           
DELWHY   DC    CL1' '                                                           
LKEY     DC    XL25'00'                                                         
*                                                                               
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
CNULL    DC    F'0'                                                             
DNULL    DC    F'0'                                                             
CPWDT    DC    F'0'                                                             
DPWDT    DC    F'0'                                                             
CPWDP    DC    F'0'                                                             
*                                                                               
         LTORG                                                                  
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
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARAMS   DS    6F                                                               
APARM    DS    A                                                                
ATRMDEF  DS    A                                                                
*                                                                               
LOGRCD   DS    0CL80                                                            
LOGKEY   DS    CL25                CTFILE KEY                                   
LOGACTN  DS    X                   ACTION BITS                                  
LOGSTATV DS    X                   RECORD STATUS                                
LOGAT1V  DS    X                   TERMINAL DEFINITION ATTRIBUTE 1              
LOGDEFD  DS    XL2                 TERMINAL DEFINITION ELEMENT DISP             
LOGSTATF DS    X                   RECORD STATUS AFTER FIX                      
LOGAT1F  DS    X                   ATRRIBUTE 1 AFTER FIX                        
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
*                                                                               
PLIST    DS    0XL40                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
VCARDS   DS    A                                                                
APEELDAT DS    A                                                                
AISREC   DS    A                                                                
APARMTBL DS    A                                                                
*                                                                               
ACTN     DS    CL3                                                              
AGY      DS    CL2                                                              
PER      DS    CL8                                                              
ELEM     DS    XL32                                                             
*                                                                               
WORKX    EQU   *                                                                
                                                                                
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
                                                                                
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
                                                                                
*CTGENFILE                                                                      
       ++INCLUDE CTGENFILE                                                      
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001CTLDEX3   12/29/17'                                      
         END                                                                    
