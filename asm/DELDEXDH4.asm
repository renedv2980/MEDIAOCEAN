*          DATA SET DELDEXDH4  AT LEVEL 046 AS OF 10/06/04                      
*PHASE DELDEXDA DELDEXDH                                                        
*INCLUDE LOGIO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'DELDEXT - LOAD/DUMP EXTERNAL ROUTINE FOR DEMO FILES'            
****COUNT RECORDS BY FILE YEAR                                                  
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT,RR=R5                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   BC    0,DMXREC0                                                        
         OI    *-3,X'F0'                                                        
* THIS CODE ONE TIME ONLY - BUT INIT GETS IN TOO SOON                           
         L     R4,VLDDEFN          POINT TO LOAD DEFINITION                     
         USING LDDEFND,R4                                                       
         L     R5,LDDDTFDA         POINT TO DIR ACC DTF                         
         MVC   FILNAME,22(R5)      FILE NAME IS AT DTF+22                       
         MVC   SUBFILE,28(R5)      EXTRACT SUB-FILE CODE (IE A OR N)            
         DROP  R4                                                               
*                                                                               
DMXREC0  L     R3,AREC             POINT TO RECORD                              
         L     R1,RECIN            UPDATE RECORDS IN COUNT                      
         LA    R1,1(R1)                                                         
         ST    R1,RECIN                                                         
*                                                                               
         LA    R2,NSIRECS                                                       
         SPACE 1                                                                
DMXREC1  DS    0H                                                               
         CLI   2(R3),C'N'          NSI ONLY                                     
         BNE   DMXOK                                                            
         CLI   1(R3),C'T'          TV ONLY                                      
         BNE   DMXOK                                                            
         CLI   0(R3),C'M'          MARKET RECORD                                
         BNE   DMXSTA                NO - CHECK FOR OTHERS                      
         USING BSKEY,R3                                                         
         CLI   BSIND,2                                                          
         BNE   DMXM2                                                            
         CLI   BSBTYP,C'H'                                                      
         BNE   DMXOK                                                            
         CLC   BSBOOK,=X'97F7'     AUG/04                                       
         BNE   DMXOK                                                            
         CLC   BSRMKT,=AL2(101)                                                 
         BNE   DMXOK                                                            
*                                                                               
         B     DMXREC2                                                          
*                                                                               
         USING MLKEY,R3                                                         
DMXM2    CLI   MLIND,0                                                          
         BNE   DMXOK                                                            
         CLC   MLBOOK,=X'97F7'     AUG/04                                       
         BNE   DMXOK                                                            
         CLI   MLBTYP,C'H'                                                      
         BNE   DMXOK                                                            
         CLC   MLRMKT,=AL2(101)                                                 
         BNE   DMXOK                                                            
*                                                                               
         B     DMXREC2                                                          
*                                                                               
         USING SBKEY,R3                                                         
DMXSTA   CLI   0(R3),C'S'                                                       
         BNE   DMXRTG                                                           
         CLC   SBBOOK,=X'6808'     AUG/04                                       
         BNE   DMXOK                                                            
         CLI   SBBTYP,C'H'                                                      
         BNE   DMXOK                                                            
         CLC   SBRMKT,=AL2(101)                                                 
         BNE   DMXOK                                                            
*                                                                               
         B     DMXREC2                                                          
*                                                                               
         USING DRKEY,R3                                                         
DMXRTG   CLI   0(R3),C'R'                                                       
         BNE   DMXOK                                                            
         CLC   DRBOOK,=X'97F7'     AUG/04                                       
         BNE   DMXOK                                                            
         CLI   DRBTYP,C'H'                                                      
         BNE   DMXOK                                                            
         CLC   DRKMKT,=AL2(101)                                                 
         BE    DMXREC2                                                          
         OC    DRKMKT,DRKMKT                                                    
         BNZ   DMXOK                                                            
         LA    RE,STALIST                                                       
STCHK    CLI   0(RE),X'FF'                                                      
         BE    DMXOK                                                            
         CLC   DRSTAT(4),0(RE)                                                  
         BE    DMXREC2                                                          
         LA    RE,4(RE)                                                         
         B     STCHK                                                            
*                                                                               
*        LA    RE,MKTLIST                                                       
*TGCHK   CLI   0(RE),X'FF'                                                      
*        BE    DMXOK                                                            
*        CLC   DRKMKT(2),0(RE)                                                  
*        BE    *+12                                                             
*        LA    RE,2(RE)                                                         
*        B     RTGCHK                                                           
*                                                                               
         B     DMXREC2                                                          
*                                                                               
DMXOK    L     R1,0(R2)                                                         
         LA    R1,1(R1)            UPDATE ACCUMS                                
         ST    R1,0(R2)                                                         
*                                                                               
*        CLI   1(R3),C'R'                                                       
*        BE    DMXPURGE                                                         
         L     R1,RECKEEP          INCREMENT RECORD KEEP COUNT                  
         LA    R1,1(R1)                                                         
         ST    R1,RECKEEP                                                       
         CLI   0(R3),C'R'                                                       
         BNE   DMXKEEP                                                          
         ZIC   R0,DRKSTAT          COUNT RECORDS FOR EACH FILE                  
         MH    R0,=H'24'                                                        
         LA    RF,F0RECS                                                        
         AR    RF,R0                                                            
         L     R1,0(RF)                                                         
         LA    R1,1(R1)                                                         
         ST    R1,0(RF)                                                         
         ZIC   RF,DRKSTAT                                                       
         MVC   WORK(2),DRBOOK                                                   
         XC    WORK(2),=X'FFFF'                                                 
         ZIC   R0,WORK                                                          
         SHI   R0,84                                                            
         MH    RF,=H'28'                                                        
         LA    R1,F0YEARS                                                       
         AR    RF,R1                                                            
         AR    RF,R0                                                            
         MVI   0(RF),C'X'                                                       
         ZIC   R0,WORK                                                          
         ZIC   RF,DRKSTAT                                                       
         SHI   R0,84                                                            
         MH    R0,=H'4'                                                         
         MH    RF,=H'112'                                                       
         LA    R1,F0Y2                                                          
         AR    RF,R1                                                            
         AR    RF,R0                                                            
         L     R1,0(RF)                                                         
         LA    R1,1(R1)                                                         
         ST    R1,0(RF)                                                         
         B     DMXKEEP                                                          
         SPACE 1                                                                
* DUMP RECORD WITH WRONG SOURCE - WRITE TO CONSOLE AND BLOW UP                  
*                                                                               
DMXREC2  DS    0H                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK+1(23),0(R3)                                                 
         MVI   WORK,23                                                          
         BAS   RE,DUMPREC                                                       
         B     DMXPURGE                                                         
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(32),=C'**WARNING** WRONG INPUT TAPE FOR'                    
         MVC   WORK+33(7),FILNAME                                               
         GOTO1 LOGIO,DMCB,1,(41,WORK),RR=RELO                                   
         DC    H'0'                                                             
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         GOTO1 VPRINTER                                                         
         MVC   P+10(7),FILNAME                                                  
         MVC   P+18(14),=C'SUMMARY TOTALS'                                      
         GOTO1 VPRINTER                                                         
         MVI   P+10,C'-'                                                        
         MVC   P+11(21),P+10                                                    
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         LA    R4,BUCKETS          COUNTER                                      
         LA    R3,BUCKTAB          POINTER TO BUCKETS                           
DMXEOF1  MVC   P+10(20),4(R3)      DESCRIPTION                                  
         MVI   P+30,C'='                                                        
         L     R2,0(R3)            BUCKET VALUE                                 
         EDIT  (R2),(10,P+32)                                                   
         GOTO1 VPRINTER                                                         
         LA    R3,L'BUCKTAB(R3)                                                 
         BCT   R4,DMXEOF1                                                       
         GOTO1 VPRINTER                                                         
         LA    R4,5                                                             
         LA    R3,F0YEARS                                                       
DMXEOF2  MVC   P(28),0(R3)                                                      
         GOTO1 VPRINTER                                                         
         LA    R3,28(R3)                                                        
         BCT   R4,DMXEOF2                                                       
         LA    R4,5                                                             
         LA    R3,F0Y2                                                          
DMXEOF3  LA    R5,14                                                            
         LA    R6,P2LINE                                                        
DMXEOF4  L     R2,0(R3)                                                         
         EDIT  (R2),(8,0(R6))                                                   
         L     R2,56(R3)                                                        
         EDIT  (R2),(8,132(R6))                                                 
         LA    R6,9(R6)                                                         
         LA    R3,4(R3)                                                         
         BCT   R5,DMXEOF4                                                       
         MVC   P(132),P2LINE                                                    
         GOTO1 VPRINTER                                                         
         MVC   P(132),P2LINE2                                                   
         GOTO1 VPRINTER                                                         
         XC    P2LINE(132),P2LINE                                               
         XC    P2LINE2(132),P2LINE2                                             
         LA    R3,14*4(R3)                                                      
         BCT   R4,DMXEOF3                                                       
         B     DMXIT                                                            
         EJECT                                                                  
* DUMPREC EXPECTS HEADER MESSAGE LENGTH IN WORK AND TEXT AT WORK+1              
*                                                                               
DUMPREC  NTR1                                                                   
*        MVC   HALF,20(R3)         EXTRACT RECORD LENGTH                        
*        LH    R5,HALF                                                          
*        CLI   HALF,X'FF'          TEST FOR PASSIVE RECORD                      
*        BNE   *+8                                                              
*        MVC   P(20),0(R3)                                                      
*        GOTO1 VPRINTER                                                         
*        B     DMXIT                                                            
         LA    R5,23               LENGTH OF PASSIVE RECORD                     
         ZIC   R2,WORK             HEADER MESSAGE LENGTH                        
         GOTO1 PRNTBL,DMCB,((R2),WORK+1),(R3),C'DUMP',(R5),=C'2D',     X        
               RR=RELO                                                          
         B     DMXIT                                                            
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
STALIST  DC    C'WABC'                                                          
         DC    C'WCBS'                                                          
         DC    C'WFUT'                                                          
         DC    C'WLIW'                                                          
         DC    C'WLNY'                                                          
         DC    C'WNBC'                                                          
         DC    C'WNET'                                                          
         DC    C'WNJN'                                                          
         DC    C'WNJU'                                                          
         DC    C'WNYE'                                                          
         DC    C'WNYW'                                                          
         DC    C'WPIX'                                                          
         DC    C'WPXN'                                                          
         DC    C'WWOR'                                                          
         DC    C'WXTV'                                                          
         DC    C'ZNY1'                                                          
         DC    X'FF'                                                            
*                                                                               
MKTLIST  DC    AL2(139)                                                         
         DC    AL2(273)                                                         
         DC    AL2(293)                                                         
         DC    AL2(416)                                                         
         DC    AL2(499)                                                         
         DC    AL2(507)                                                         
         DC    AL2(528)                                                         
         DC    AL2(536)                                                         
         DC    AL2(556)                                                         
         DC    AL2(567)                                                         
         DC    AL2(581)                                                         
         DC    AL2(367)                                                         
         DC    AL2(518)                                                         
         DC    AL2(542)                                                         
         DC    AL2(563)                                                         
         DC    AL2(579)                                                         
         DC    AL2(591)                                                         
         DC    AL2(592)                                                         
         DC    AL2(137)                                                         
         DC    AL2(157)                                                         
         DC    AL2(287)                                                         
         DC    AL2(291)                                                         
         DC    AL2(343)                                                         
         DC    AL2(365)                                                         
         DC    AL2(574)                                                         
         DC    AL2(594)                                                         
         DC    AL2(279)                                                         
         DC    AL2(323)                                                         
         DC    AL2(537)                                                         
         DC    AL2(593)                                                         
         DC    AL2(337)                                                         
         DC    AL2(516)                                                         
         DC    AL2(547)                                                         
         DC    AL2(565)                                                         
         DC    X'FF'                                                            
         LTORG                                                                  
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
FILNAME  DC    CL7' '                                                           
SUBFILE  DC    C' '                                                             
         SPACE 2                                                                
* ROUTINE TABLE                                                                 
*                                                                               
LOGIO    DC    V(LOGIO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
         SPACE 2                                                                
F0YEARS  DC    28C'0'                                                           
F1YEARS  DC    28C'0'                                                           
F2YEARS  DC    28C'0'                                                           
F3YEARS  DC    28C'0'                                                           
F4YEARS  DC    28C'0'                                                           
F5YEARS  DC    28C'0'                                                           
F0Y2     DC    28AL4(0)                                                         
F1Y2     DC    28AL4(0)                                                         
F2Y2     DC    28AL4(0)                                                         
F3Y2     DC    28AL4(0)                                                         
F4Y2     DC    28AL4(0)                                                         
F5Y2     DC    28AL4(0)                                                         
* BUCKET TABLE                                                                  
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL24                                                            
RECIN    DC    F'0',CL20'RECORDS IN'                                            
NSIRECS  DC    F'0',CL20'NSI RECORDS'                                           
F0RECS   DC    F'0',CL20'D0 RECORDS'                                            
F1RECS   DC    F'0',CL20'D1 RECORDS'                                            
F2RECS   DC    F'0',CL20'D2 RECORDS'                                            
F3RECS   DC    F'0',CL20'D3 RECORDS'                                            
F4RECS   DC    F'0',CL20'D4 RECORDS'                                            
F5RECS   DC    F'0',CL20'D5 RECORDS'                                            
RECKEEP  DC    F'0',CL20'RECORDS KEPT'                                          
BUCKETS  EQU   (*-BUCKTAB)/L'BUCKTAB                                            
         SPACE 2                                                                
* WORKING STORAGE DSECT                                                         
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
RELO     DS    A                                                                
HALF     DS    H                                                                
WORK     DS    CL60                                                             
ELCODE   DS    C                                                                
P2LINE   DS    132C                                                             
P2LINE2  DS    132C                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
* DEDEMFILE                                                                     
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046DELDEXDH4 10/06/04'                                      
         END                                                                    
