*          DATA SET SXLDEXTCB2 AT LEVEL 055 AS OF 05/07/98                      
*PHASE SXEXTCBL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH2                                                               
         TITLE 'SXLDEXT -XSPDIR/FIL LOAD/DUMP MODEL EXTERNAL ROUTINE'           
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
         NMOD1 WORKX-WORKD,SXLDEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     RF,=V(HEXOUT)                                                    
         ST    RF,VHEXOUT                                                       
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
*                                                                               
*          DATA SET SPLDEXTCBL AT LEVEL 140 AS OF 03/18/98                      
* INITIALIZE LOGIC                                                              
*                                                                               
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
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC                                                          
         CLC   0(2,R3),=X'0E03'                                                 
         BE    SNV                                                              
         CLC   0(2,R3),=X'0E04'                                                 
         BE    MSR                                                              
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
*==================================================================             
SNV      DS    0H                                                               
         TM    5(R3),X'F0'                                                      
         BNO   DMXKEEP                                                          
*  POINT IT TO TWO BEFORE STATION                                               
         LA    R2,3(R3)                                                         
*                                                                               
*        DC    H'0'                                                             
         MVC   BAGYMED,2(R3)                                                    
         MVI   ELCODE,X'40'                                                     
         LR    R6,R3                                                            
         BAS   RE,GETEL                                                         
         BE    *+8                                                              
         B     DMXKEEP                                                          
         USING SNVIDELD,R6                                                      
         MVC   NETNUM,SNVIDNWK                                                  
         MVC   MYNET,SNVIDNWK                                                   
         ST    R6,NETADDR                                                       
         GOTO1 GETSTA                                                           
         BNE   DMXKEEP                                                          
         MVC   SNVIDNWK,MYNET                                                   
         GOTO1 VHEXOUT,DMCB,0(R3),P+2,13,=C'TOG'                                
         MVC   P+27(18),=C'**CHANGED ELEM ***'                                  
         GOTO1 VHEXOUT,DMCB,0(R6),P+47,35,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
         DROP  R6                                                               
*                                                                               
*==================================================================             
MSR      DS    0H                                                               
         TM    10(R3),X'F0'                                                     
         BNO   DMXKEEP                                                          
*  POINT IT TO TWO BEFORE STATION                                               
         LA    R2,8(R3)                                                         
*                                                                               
*        DC    H'0'                                                             
         MVC   BAGYMED,2(R3)                                                    
         LR    R6,R3                                                            
         MVC   NETNUM,12(R3)                                                    
         MVC   MYNET,12(R3)                                                     
         ST    R6,NETADDR                                                       
         GOTO1 GETSTA                                                           
         BNE   DMXKEEP                                                          
         MVC   12(1,R3),MYNET                                                   
         GOTO1 VHEXOUT,DMCB,0(R3),P+2,13,=C'TOG'                                
         MVC   P+29(16),=C'**CHANGED ELEM *'                                    
         GOTO1 VHEXOUT,DMCB,0(R6),P+47,35,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
****     DROP  R6                                                               
*                                                                               
*==================================================================             
*==================================================================             
*                                                                               
GETSTA   NTR1                                                                   
* BUILD ARGS AND CALL BINSRCH                                                   
*        IC    R7,4(R2)             GET THE LAST 7 BITS OF THE LAST             
         IC    R7,NETNUM            GET NETWORK BYTE                            
         N     R7,=X'0000007F'      BYTE MARKET/STATION                         
         STC   R7,NETNUM                                                        
*                                                                               
         OC    NETNUM,NETNUM                                                    
         BZ    BADNET                                                           
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
*        GOTO1 VHEXOUT,DMCB,2(R2),P+6,17                                        
         GOTO1 VHEXOUT,DMCB,0(R3),P+2,60,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   P(42),=C'DID NOT FIND NETWORK IN CABTABLE -NUMBER :'             
         GOTO1 VHEXOUT,DMCB,NETNUM,P+44,1,=C'TOG'                               
         GOTO1 VPRINTER                                                         
*        B     GETSTAX                                                          
         CR    RE,RF                                                            
         B     DMXIT                                                            
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
*        GOTO1 VHEXOUT,DMCB,2(R2),P+6,17                                        
         GOTO1 VHEXOUT,DMCB,0(R3),P+2,13,=C'TOG'                                
         MVC   P+29(16),=C'***   TOP 24 ***'                                    
         ZICM  R6,NETADDR,4                                                     
***      USING SNVIDELD,R6                                                      
         GOTO1 VHEXOUT,DMCB,0(R6),P+47,35,=C'TOG' ELEMENT                       
         GOTO1 VPRINTER                                                         
*                                                                               
****     NI    SNVIDNWK,X'80'   TAKE OFF LAST 7 BITS OF NET FIELD               
****     OC    SNVIDNWK,BYTE                                                    
         NI    MYNET,X'80'   TAKE OFF LAST 7 BITS OF NET FIELD                  
         OC    MYNET,BYTE                                                       
*        DROP  R6                                                               
****     GOTO1 VHEXOUT,DMCB,0(R3),P+2,13,=C'TOG'                                
****     MVC   P+27(18),=C'**CHANGED ELEM ***'                                  
****     GOTO1 VHEXOUT,DMCB,0(R6),P+47,35,=C'TOG'                               
****     GOTO1 VPRINTER                                                         
         CR    RE,RE                                                            
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
         ZICM  R6,NETADDR,4                                                     
****     USING SNVIDELD,R6                                                      
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
         GOTO1 VHEXOUT,DMCB,0(R3),P+2,24,=C'TOG'                                
         GOTO1 VHEXOUT,DMCB,0(R6),P+49,35,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(32),=C'DID NOT FIND NETWORK IN FILE  >>'                       
*        GOTO1 VHEXOUT,DMCB,KEY,P+35,1,=C'TOG'                                  
*        MVC   P+37(4),KEY+1                                                    
         MVC   P+35(4),KEY                                                      
         GOTO1 VHEXOUT,DMCB,KEY+4,P+41,1,=C'TOG'                                
         GOTO1 VPRINTER                                                         
*        B     GETSTAX                                                          
         CR    RE,RF                                                            
         B     DMXIT                                                            
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
         GOTO1 VHEXOUT,DMCB,0(R3),P+2,24,=C'TOG'                                
         GOTO1 VHEXOUT,DMCB,0(R6),P+49,35,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(24),=C'DID NOT FIND IN SEQ64 >>'                               
         MVC   P+25(4),KEY                                                      
         GOTO1 VHEXOUT,DMCB,NETNUM,P+33,1,=C'TOG'                               
         GOTO1 VPRINTER                                                         
*        B     GETSTAX                                                          
         CR    RE,RF                                                            
         B     DMXIT                                                            
FOUND    AH    R5,=H'24'                     ADD 24 TO ANY NON24 NET            
         STC   R5,BYTE                                                          
*                                                                               
         GOTO1 VHEXOUT,DMCB,0(R3),P+2,13,=C'TOG'                                
         MVC   P+27(11),=C'** N24 ****'                                         
         MVC   P+38(4),KEY                                                      
         MVC   P+42(3),=C'***'                                                  
         GOTO1 VHEXOUT,DMCB,0(R6),P+47,35,=C'TOG'                               
         GOTO1 VPRINTER                                                         
**       NI    SNVIDNWK,X'80'   TAKE OFF LAST 7 BITS                            
**       OC    SNVIDNWK,BYTE    9(R3) LAST BYTE OF MARKET/STATION               
         NI    MYNET,X'80'   TAKE OFF LAST 7 BITS OF NET FIELD                  
         OC    MYNET,BYTE                                                       
*                                                                               
****     GOTO1 VHEXOUT,DMCB,0(R3),P+2,13,=C'TOG'                                
****     MVC   P+27(18),=C'**CHANGED ELEM ***'                                  
****     GOTO1 VHEXOUT,DMCB,0(R6),P+47,35,=C'TOG'                               
****     GOTO1 VPRINTER                                                         
         CR    RE,RE                                                            
GETSTAX  XIT1                                                                   
*                                                                               
****     DROP  R6                                                               
*                                                                               
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
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
*                                                                               
         EJECT                                                                  
                                                                                
EXIT     XIT1                                                                   
         LTORG                                                                  
*                                                                               
         GETEL R6,42,ELCODE                                                     
FILEIN   DCB   DDNAME=TEMPIN,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,      X        
               EODAD=DMXIN20                                                    
*                                                                               
NETNUM   DS    X                                                                
TESTREC  DC    XL22'E20000010000F00A850000000000000000000000E2D1'               
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
BYTE     DS    X                                                                
COUNT    DS    F                                                                
TEMP     DS    CL80                                                             
TABADDR  DS    A                                                                
TABCOUNT DS    F            MARKT/STAT                                          
DUB      DS    D                                                                
KEY      DS    CL10                                                             
BAGYMED  DS    CL1                                                              
NETADDR  DS    A                                                                
MYNET    DS    X                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
       ++INCLUDE SPCBLLST                                                       
         SPACE 2                                                                
WORKD    DSECT                                                                  
*UB      DS    D                                                                
*ORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
VHEXOUT  DS    A                                                                
ELCODE   DS    X                                                                
WORK     DS    CL128                                                            
WORKX    DS    0C                                                               
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
*SPSTAPACKD                                                                     
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
         DS    CL2                                                              
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PMED     DS    CL1                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL5                                                              
         DS    CL2                                                              
POVER    DS    CL4                                                              
         DS    CL3                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL4                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
         DS    CL4                                                              
PERR     DS    CL25                                                             
         DS    CL1                                                              
PMYREC   DS    CL46                                                             
         PRINT OFF                                                              
BUYRECD   DSECT                                                                 
       ++INCLUDE SPGENBUY                                                       
INVRECD   DSECT                                                                 
       ++INCLUDE SPGENSNV                                                       
DARERECD  DSECT                                                                 
       ++INCLUDE SPGENDRORD                                                     
NBUYRECD  DSECT                                                                 
       ++INCLUDE SPNWSHDR                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055SXLDEXTCB205/07/98'                                      
         END                                                                    
*                                                                               
