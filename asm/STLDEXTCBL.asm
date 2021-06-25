*          DATA SET STLDEXTCBL AT LEVEL 002 AS OF 08/10/00                      
*PHASE STEXTCBA STEXTCBL                                                        
*INCLUDE BINSRCH2                                                               
***********************************************************                     
*  DO NOT DELETE THIS AT ALL COST  !!!!!!                                       
*  CABLE CONVERSION RUN ON MAY 9,1998                                           
***********************************************************                     
         TITLE 'STLDEXT - MAKE VARIABLE LENGTH RECORDS'                         
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
         NMOD1 20,DMLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     R9,VLDDEFN                                                       
         USING LDDEFND,R9                                                       
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
* INITIALISE LOGIC                                                              
*                                                                               
*          DATA SET SPLDEXTCBL AT LEVEL 139 AS OF 03/18/98                      
DMXINIT  DS    0H                                                               
         OPEN  (FILEIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R0,=F'2000000'                                                   
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         STCM  R1,15,TABADDR                                                    
         LR    R3,R1                                                            
         SR    R5,R5                                                            
*                                                                               
*                                                                               
DMXIN10  GET   FILEIN,TEMP                                                      
         ZICM  R4,TABADDR,4                                                     
         GOTO1 =V(BINSRCH),DMCB,(X'01',TEMP),(R4),(R5),80,(0,5),11000           
         ZICM  R1,DMCB,4                                                        
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R5,1(R5)            BUMP COUNT                                   
         B     DMXIN10                                                          
*                                                                               
DMXIN20  CLOSE FILEIN                                                           
         STCM  R5,15,TABCOUNT                                                   
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC                                                          
         CLC   =X'0A24',0(R3)      TEST TRAFFIC INST RECAP                      
         BE    TIR                                                              
*                                                                               
         CLC   =X'0A25',0(R3)      TEST TRAFFIC SHIP RECAP                      
         BE    TSR                                                              
*                                                                               
         CLC   =X'0A2E',0(R3)      TEST TRAFFIC BUY ACT                         
         BE    TBA                                                              
*                                                                               
         CLC   =X'0A32',0(R3)      TEST TRAFFIC BUY                             
         BE    BUY                                                              
*                                                                               
         CLC   =X'0A2F',0(R3)      TEST LABEL LIST                              
         BE    TLL                                                              
*                                                                               
         CLC   =X'0A31',0(R3)      TEST STATION LIST                            
         BE    TSL                                                              
*                                                                               
         CLC   =X'0A22',0(R3)      TEST PATTERN REC                             
         BE    TPR                                                              
*                                                                               
*        CLC   =X'0A28',0(R3)      TEST STATION REC                             
*        BE    TSTR                                                             
*                                                                               
         CLC   =X'0A35',0(R3)      COMMERCIAL TEXT REC                          
         BE    CTR                                                              
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
*=================================================================              
* TRAFFIC INST RECAP .  R3=A(RECORD)                                            
*=================================================================              
         SPACE 1                                                                
TIR      DS    0H                                                               
*                                                                               
         TM    8(R3),X'F0'         TEST CABLE                                   
         BNO   DMXKEEP             NO                                           
*                                                                               
         LA    R2,6(R3)            R2 POINTS TO MKT/STATION                     
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
*==================================================================             
TSR      DS    0H                                                               
         TM    7(R3),X'F0'                                                      
         BNO   DMXKEEP                                                          
*                                                                               
         LA    R2,5(R3)                                                         
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         B     DMXKEEP                                                          
*==================================================================             
TBA      DS    0H                                                               
         TM    7(R3),X'F0'                                                      
         BNO   DMXKEEP                                                          
*                                                                               
         LA    R2,5(R3)                                                         
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         B     DMXKEEP                                                          
*==================================================================             
BUY      DS    0H                                                               
         TM    8(R3),X'F0'                                                      
         BNO   DMXKEEP                                                          
*                                                                               
         LA    R2,6(R3)                                                         
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         B     DMXKEEP                                                          
*==================================================================             
TLL      DS    0H                                                               
         MVI   ELCODE,X'10'                                                     
         LR    R6,R3                                                            
         BAS   RE,GETEL                                                         
         B     *+8                                                              
TLL20    BAS   RE,NEXTEL                                                        
         BNE   DMXKEEP                                                          
         TM    2(R6),X'F0'                                                      
         BNO   DMXKEEP                                                          
*                                                                               
         LR    R2,R6                                                            
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         B     TLL20                                                            
*==================================================================             
TSL      DS    0H                                                               
         MVI   ELCODE,X'10'                                                     
         LR    R6,R3                                                            
         BAS   RE,GETEL                                                         
         B     *+8                                                              
TSL20    BAS   RE,NEXTEL                                                        
         BNE   DMXKEEP                                                          
         TM    2(R6),X'F0'                                                      
         BNO   DMXKEEP                                                          
*                                                                               
         LR    R2,R6                                                            
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         B     TSL20                                                            
*==================================================================             
TPR      DS    0H                                                               
         MVI   ELCODE,X'20'                                                     
         LR    R6,R3                                                            
         BAS   RE,GETEL                                                         
         B     *+8                                                              
TPR20    BAS   RE,NEXTEL                                                        
         BNE   DMXKEEP                                                          
         USING PATLSTEL,R6                                                      
         CLI   PATLSTTY,C'S'       STATION LIST                                 
         BNE   TPR20                                                            
*                                                                               
         MVC   BAGYMED,2(R3)                                                    
         ZIC   R4,1(R6)                                                         
         AR    R4,R6               END OF ELEM                                  
         LA    R2,3(R6)            START OF LIST                                
*                                                                               
TPR25    CR    R2,R4                                                            
         BNL   TPR20                                                            
         OC    0(2,R2),0(R2)       ZERO FOR CABLE                               
         BNZ   TPR25                                                            
         TM    2(R2),X'F0'                                                      
         BNO   TPR25                                                            
         GOTO1 GETSTA                                                           
         LA    R2,5(R2)             NEXT STATION??                              
         B     TPR25                                                            
*                                                                               
*==================================================================             
TSTR     DS    0H                                                               
         TM    5(R3),X'F0'                                                      
         BNO   DMXKEEP                                                          
*                                                                               
         LA    R2,3(R3)                                                         
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         B     DMXKEEP                                                          
*==================================================================             
CTR      DS    0H                                                               
         TM    8(R3),X'F0'                                                      
         BNO   DMXKEEP                                                          
*                                                                               
         LA    R2,6(R3)                                                         
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         B     DMXKEEP                                                          
*==================================================================             
*                                                                               
GETSTA   NTR1                                                                   
* BUILD ARGS AND CALL BINSRCH                                                   
         IC    R7,4(R2)             GET THE LAST 7 BITS OF THE LAST             
         N     R7,=X'0000007F'      BYTE MARKET/STATION                         
         STC   R7,NETNUM                                                        
*                                                                               
         OC    NETNUM,NETNUM                                                    
         BZ    GETSTAX                                                          
*   THIS PART JUMPS INTO CABLE TABLE ENTRY                                      
         LA    R4,CABLETAB                                                      
         SR    R5,R5                                                            
         SR    R8,R8                                                            
         SR    R6,R6                                                            
         LA    R8,L'CABLETAB                                                    
         MR    R6,R8               PRODUCT IN 6 AND 7                           
         SR    R7,R8                                                            
*                                                                               
         AR    R7,R4               R7 POINTS TO CABLE ENTRY IN TABLE            
         CLC   3(1,R7),NETNUM                                                   
         BE    FNDTAB                                                           
*                                                                               
******* DID NOT FIND NETWORK NUMBER IN CABLE TABLE ***********                  
*                                                                               
BADNET   DS    0H                                                               
         GOTO1 LHEXOUT,DMCB,0(R3),P+2,21,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   P(42),=C'DID NOT FIND NETWORK IN CABTABLE -NUMBER :'             
         GOTO1 LHEXOUT,DMCB,NETNUM,P+44,1,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         B     GETSTAX                                                          
*                                                                               
*   NOW TEST IF TOP 24 STATION ?                                                
FNDTAB   TM    5(R7),X'40'                                                      
         BZ    NON24                                                            
         B     TOP24                                                            
*                                                                               
*================================================================*              
* TOP 24 HAS '40'BIT TURNED ON, SO SUBTRACT '40' FROM 5(TABLEENTRY)             
* TO GET THE RANKING AMONG THE TOP 24 NETWORKS                                  
* EXAMPLE .. 5(AE NETWORK) = '41',  41 - '40'= 1                                
*         .. 5(ESP NETWORK)= '47',  47 - '40'= 7                                
*                                                                               
TOP24    ZIC   RE,5(R7)    RE NOW HAS 5(NETWORK TABLE ENTRY)                    
         N     RE,=X'000000BF'                                                  
         STC   RE,BYTE                                                          
         GOTO1 LHEXOUT,DMCB,0(R3),P+2,13,=C'TOG'                                
         MVC   P+27(18),=C'***   TOP 24   ***'                                  
         GOTO1 LHEXOUT,DMCB,0(R2),P+50,20,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         NI    4(R2),X'80'      TAKE OFF LAST 7 BITS                            
         OC    4(1,R2),BYTE     9(R3) LAST BYTE OF MARKET/STATION               
         GOTO1 LHEXOUT,DMCB,0(R3),P+2,13,=C'TOG'                                
         MVC   P+27(18),=C'*** CHANGED TO ***'                                  
         GOTO1 LHEXOUT,DMCB,0(R2),P+50,20,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         EJECT                                                                  
*================================================================*              
* NON TOP 24 REQUIRES THE SEQUENCES IN SCBLSEQ IN INPUT TABLE    *              
* R3==> BUYREC                                                                  
NON24    SR    R4,R4                                                            
         ICM   R4,7,2(R2)                                                       
         N     R4,=X'000FFFFF'                                                  
         SRL   R4,7                                                             
         XC    KEY,KEY             BUILD THE SEARCH KEY                         
*                                                                               
         EDIT  (R4),(4,KEY),FILL=0                                              
         MVC   KEY+4(L'BAGYMED),BAGYMED                                         
         ZICM  R4,TABADDR,4                                                     
         ZICM  R8,TABCOUNT,4                                                    
         GOTO1 =V(BINSRCH),DMCB,(X'00',KEY),(R4),(R8),80,(0,5),8000             
         ZICM  R1,DMCB,4                                                        
         CLI   DMCB,X'01'                                                       
         BNE   GOTIT                                                            
*                                                                               
* DID NOT FIND THE RECORD IN THE INPUT FILE   *******                           
* ===== EX. SJ0021 =================================*                           
*                                                                               
         GOTO1 LHEXOUT,DMCB,0(R3),P+2,60,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   P(32),=C'DID NOT FIND NETWORK IN FILE  >>'                       
         MVC   P+35(4),KEY                                                      
         GOTO1 LHEXOUT,DMCB,KEY+4,P+41,1,=C'TOG'                                
         GOTO1 LHEXOUT,DMCB,2(R2),P+45,3,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         B     GETSTAX                                                          
*                                                                               
* RECORD MUST EXIST IF VALID SO NOW SEARCH SEQUENCE NUMBERS                     
GOTIT    LA    R4,64      64 BYTES IN SEQUENCE                                  
         LA    R8,7(R1)   FIRST SEQUENCE NUMBER                                 
         LA    R5,1                                                             
         ZIC   RE,NETNUM                                                        
*                                                                               
NON10    CLC   NETNUM,0(R8)    4B                                               
         BE    FOUND                                                            
         LA    R5,1(R5)                                                         
         LA    R8,1(R8)                                                         
         BCT   R4,NON10                                                         
*                                                                               
*   DID NOT FIND THE NETWORK NUMBER IN THE SEQ 64 BYTE FIELD IN FILE *          
*   IF DID NOT FIND SEQ NUM JUST PRINT OUT                                      
*                                                                               
         GOTO1 LHEXOUT,DMCB,0(R3),P+2,60,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   P(24),=C'DID NOT FIND IN SEQ64 >>'                               
         MVC   P+25(6),KEY                                                      
         GOTO1 LHEXOUT,DMCB,NETNUM,P+33,1,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         B     GETSTAX                                                          
FOUND    AH    R5,=H'24'                     ADD 24 TO ANY NON24 NET            
         STC   R5,BYTE                                                          
         GOTO1 LHEXOUT,DMCB,0(R3),P+2,13,=C'TOG'                                
         MVC   P+27(13),=C'**NT24 HD =**'                                       
         MVC   P+40(4),KEY                                                      
         GOTO1 LHEXOUT,DMCB,NETNUM,P+46,1,=C'TOG'                               
         GOTO1 LHEXOUT,DMCB,0(R2),P+50,25,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         NI    4(R2),X'80'      TAKE OFF LAST 7 BITS                            
         OC    4(1,R2),BYTE     9(R3) LAST BYTE OF MARKET/STATION               
         GOTO1 LHEXOUT,DMCB,0(R3),P+2,13,=C'TOG'                                
         MVC   P+27(13),=C'**CHANGED TO*'                                       
         GOTO1 LHEXOUT,DMCB,0(R2),P+50,25,=C'TOG'                               
         GOTO1 VPRINTER                                                         
GETSTAX  XIT1                                                                   
*&&DO                                                                           
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
*&&                                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
FILEIN   DCB   DDNAME=TEMPIN,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,      X        
               EODAD=DMXIN20                                                    
*                                                                               
NETNUM   DS    X                                                                
BYTE     DS    X                                                                
COUNT    DS    F                                                                
TEMP     DS    CL80                                                             
TABADDR  DS    A                                                                
TABCOUNT DS    F            MARKT/STAT                                          
DUB      DS    D                                                                
KEY      DS    CL10                                                             
BAGYMED  DS    CL1                                                              
*                                                                               
       ++INCLUDE SPCBLLST                                                       
         SPACE 2                                                                
WORKD    DSECT                                                                  
*UB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
ELCODE   DS    X                                                                
*                                                                               
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
INVRECD  DSECT                                                                  
       ++INCLUDE SPGENINV                                                       
         EJECT                                                                  
DARERECD DSECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
NBUYRECD DSECT                                                                  
       ++INCLUDE SPNWSHDR                                                       
TRFPATD  DSECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002STLDEXTCBL08/10/00'                                      
         END                                                                    
