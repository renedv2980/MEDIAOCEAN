*          DATA SET SPLDEXTCBA AT LEVEL 157 AS OF 05/11/98                      
*PHASE SPEXTCBL                                                                 
*INCLUDE BINSRCH2                                                               
SPEXTCBL TITLE 'CHANGE PACKED CABLE STATIONS'                                   
*                                                                               
*                                                                               
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
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
*                                                                               
         L     R9,VLDDEFN                                                       
         USING LDDEFND,R9                                                       
*                                                                               
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
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
         SPACE 2                                                                
*                                                                               
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
*                                                                               
         CLI   0(R3),X'10'                                                      
         BH    BUY                                                              
         CLC   =X'0E01',0(R3)                                                   
         BE    STAB                                                             
         CLC   =X'0D72',0(R3)      STATION LOCKIN HEADER                        
         BE    SLH                                                              
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
         SPACE 1                                                                
*==================================================================             
STAB     DS    0H                                                               
         TM    9(R3),X'F0'         TEST CABLE                                   
         BNO   DMXKEEP             NO                                           
*                                                                               
         CLC   9(3,R3),=X'FFFFFE'                                               
         BNL   DMXKEEP                                                          
*                                                                               
         LA    R2,7(R3)            R2 POINTS TO MKT/STATION                     
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
*==================================================================             
SLH      DS    0H                                                               
         TM    7(R3),X'F0'         TEST CABLE                                   
         BNO   DMXKEEP             NO                                           
*                                                                               
         CLC   7(3,R3),=X'FFFFFE'                                               
         BNL   DMXKEEP                                                          
*                                                                               
         LA    R2,5(R3)            R2 POINTS TO MKT/STATION                     
         MVC   BAGYMED,2(R3)                                                    
         GOTO1 GETSTA                                                           
         B     DMXKEEP                                                          
         EJECT                                                                  
* FIX SOME PREVIOUSLY UNCONVERTED RECORDS !                                     
*                                                                               
BUY      CLC   =C'SC',20(R3)                                                    
         BE    BUYSC                                                            
         CLC   =C'TY',20(R3)                                                    
         BE    BUYTY                                                            
         CLC   =C'WI',20(R3)                                                    
         BE    BUYWI                                                            
         CLC   =C'BS',20(R3)                                                    
         BE    BUYBS                                                            
         B     DMXKEEP                                                          
*                                                                               
BUYSC    CLC   0(9,R3),=X'B1A245FF067DF9FE7C'                                   
         BNE   BUYSC4                                                           
         MVI   8(R3),X'28'                                                      
         B     DMXKEEP                                                          
*                                                                               
BUYSC4   CLC   0(9,R3),=X'B1A245FF067DFB05FC'                                   
         BNE   BUYSC6                                                           
         MVI   8(R3),X'99'                                                      
         B     DMXKEEP                                                          
*                                                                               
BUYSC6   CLC   0(9,R3),=X'B1A245FF024FFDB109'                                   
         BNE   BUYSC8                                                           
         MVI   8(R3),X'26'                                                      
         B     DMXKEEP                                                          
*                                                                               
BUYSC8   CLC   0(9,R3),=X'B1A245FF024FFDB113'                                   
         BNE   DMXKEEP                                                          
         MVI   8(R3),X'27'                                                      
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
BUYTY    CLC   0(9,R3),=X'7189CDFF0277F52D09'                                   
         BNE   BUYTY2                                                           
         MVI   8(R3),X'1C'                                                      
         B     DMXKEEP                                                          
*                                                                               
BUYTY2   CLC   0(9,R3),=X'7189CDFF0277F59089'                                   
         BNE   DMXKEEP                                                          
         MVI   8(R3),X'9B'                                                      
         B     DMXKEEP                                                          
         EJECT                                                                  
BUYWI    CLC   0(9,R3),=X'11AEA2FF02D1F03CFC'                                   
         BNE   BUYWI2                                                           
         MVI   8(R3),X'9D'                                                      
         B     DMXKEEP                                                          
*                                                                               
BUYWI2   CLC   0(9,R3),=X'118B2BFF0795F21D96'                                   
         BNE   DMXKEEP                                                          
         MVI   8(R3),X'9B'                                                      
         B     DMXKEEP                                                          
*                                                                               
BUYBS    CLC   0(9,R3),=X'519C62FF0501F062D6'                                   
         BNE   DMXKEEP                                                          
         MVI   8(R3),X'A3'                                                      
         B     DMXKEEP                                                          
         EJECT                                                                  
*==================================================================             
*                                                                               
GETSTA   NTR1                                                                   
* BUILD ARGS AND CALL BINSRCH                                                   
         IC    R7,4(R2)             GET THE LAST 7 BITS OF THE LAST             
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
***      GOTO1 LHEXOUT,DMCB,0(R3),P+2,21,=C'TOG'                                
***      MVC   P+27(18),=C'***   TOP 24   ***'                                  
***      GOTO1 VPRINTER                                                         
         NI    4(R2),X'80'      TAKE OFF LAST 7 BITS                            
         OC    4(1,R2),BYTE     9(R3) LAST BYTE OF MARKET/STATION               
***      GOTO1 LHEXOUT,DMCB,0(R3),P+2,21,=C'TOG'                                
***      GOTO1 VPRINTER                                                         
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
         GOTO1 LHEXOUT,DMCB,0(R3),P+2,21,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   P(32),=C'DID NOT FIND NETWORK IN FILE  >>'                       
         MVC   P+35(4),KEY                                                      
         GOTO1 LHEXOUT,DMCB,KEY+4,P+41,1,=C'TOG'                                
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
         GOTO1 LHEXOUT,DMCB,0(R3),P+2,21,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   P(24),=C'DID NOT FIND IN SEQ64 >>'                               
         MVC   P+25(6),KEY                                                      
         GOTO1 LHEXOUT,DMCB,NETNUM,P+33,1,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         B     GETSTAX                                                          
FOUND    AH    R5,=H'24'                     ADD 24 TO ANY NON24 NET            
         STC   R5,BYTE                                                          
***      GOTO1 LHEXOUT,DMCB,0(R3),P+2,21,=C'TOG'                                
***      MVC   P+30(6),KEY                                                      
***      MVC   P+40(25),=C'*** NON TOP 24 *** NET# ='                           
***      GOTO1 LHEXOUT,DMCB,NETNUM,P+67,1,=C'TOG'                               
***      GOTO1 VPRINTER                                                         
         NI    4(R2),X'80'      TAKE OFF LAST 7 BITS                            
         OC    4(1,R2),BYTE     9(R3) LAST BYTE OF MARKET/STATION               
***      GOTO1 LHEXOUT,DMCB,0(R3),P+2,21,=C'TOG'                                
***      GOTO1 VPRINTER                                                         
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
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
*                                                                               
         EJECT                                                                  
                                                                                
EXIT     XIT1                                                                   
         LTORG                                                                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
FILEIN   DCB   DDNAME=TEMPIN,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,      X        
               EODAD=DMXIN20                                                    
*                                                                               
NETNUM   DS    X                                                                
TESTREC  DC    XL22'E20000010000F00A850000000000000000000000E2D1'               
*ESTREC  DC    XL22'E20000010000F00A9C0000000000000000000000E2D1'               
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
*                                                                               
         LTORG                                                                  
*                                                                               
       ++INCLUDE SPCBLLST                                                       
         SPACE 2                                                                
WORKD    DSECT                                                                  
*UB      DS    D                                                                
WORK     DS    CL64                                                             
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
ELCODE   DS    X                                                                
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
       ++INCLUDE SPGENINV                                                       
DARERECD  DSECT                                                                 
       ++INCLUDE SPGENDRORD                                                     
NBUYRECD  DSECT                                                                 
       ++INCLUDE SPNWSHDR                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'157SPLDEXTCBA05/11/98'                                      
         END                                                                    
