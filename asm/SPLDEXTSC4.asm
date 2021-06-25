*          DATA SET SPLDEXTSC4 AT LEVEL 109 AS OF 12/01/99                      
*PHASE SPEXTSC                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXTSC - FIX BAD DARE RECORDS'                               
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
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         L     R8,12(R1)           DMLDDEFN                                     
         USING LDDEFND,R8                                                       
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  DS    0H                                                               
         L     R1,APARM            KEEP RECORD EXIT                             
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
DMXIT    DS    0H                                                               
         XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         XC    COUNT,COUNT                                                      
         L     RF,=V(CLUNPK)                                                    
         ST    RF,VCLUNPK                                                       
         OPEN  (FILE,INPUT)                                                     
         LA    RE,DSNBUFFR         CLEAR THE BUFFER                             
         LHI   RF,MAXNTRYS*L'DSNBUFFR                                           
         XCEFL                                                                  
         LA    RE,DSNBUFFR                                                      
         ST    RE,ADSNBFFR         A(NEXT AVAILABLE ENTRY)                      
*                                                                               
         LA    R3,IOLEN                                                         
DMXIN10  GET   FILE,(R3)                                                        
         L     RE,ADSNBFFR         A(NEXT AVAILABLE ENTRY)                      
         MVC   0(L'DSNBUFFR,RE),IO                                              
         AHI   RE,L'DSNBUFFR                                                    
         ST    RE,ADSNBFFR                                                      
         LA    R1,DSNBUFFR                                                      
         AHI   R1,MAXNTRYS*L'DSNBUFFR                                           
         CR    RE,R1                                                            
         BL    DMXIN10                                                          
         DC    H'0'                                                             
*                                                                               
INPTEOF  CLOSE FILE                                                             
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
*                                                                               
         L     R5,AREC                                                          
         CLC   0(2,R5),=X'0D34'    DARE ORDER RECORDS                           
         BNE   DMXKEEP                                                          
*                                                                               
         CLI   12(R5),0            MAKE SURE NOT A COMMENT                      
         BNE   DMXKEEP             SKIP IF IT IS                                
*                                                                               
         LA    R4,CPKEY            FILL UP THE CLIENT PASSIVE KEY               
         USING DAREORDD,R4                                                      
         MVC   DCKTYPE(2),=X'0DB5' 0DB5                                         
         MVC   DCKAGMD,4(R5)       AGYMD                                        
*                                                                               
         MVI   ELCDLO,X'01'                                                     
         MVI   ELCDHI,X'01'                                                     
         LA    R6,24(R5)           FIRST ELEMENT                                
         USING DOIDELD,R6                                                       
         MVC   DCKCLT,DOIDCLT      CLIENT                                       
         MVC   DCKPRD,DOIDPRD      PRODUCT                                      
         MVC   DCKEST,DOIDEST      ESTIMATE                                     
         MVC   DCKSTA,DOISTA       STATION                                      
         MVC   DCKPRD2,DOIDPRD2    PRODUCT2                                     
         MVC   DCKFLTNM,DOIDFLTN   FLIGHT NUMBER                                
         DROP  R4,R6                                                            
*                                                                               
         LA    R5,DSNBUFFR         SEE IF IN OUR TABLE                          
         LR    R6,R5                                                            
         AHI   R6,MAXNTRYS*L'DSNBUFFR                                           
*                                                                               
DMXR10   CR    R5,R6                                                            
         BNL   DMXRNMTC                                                         
         OC    13(13,R5),13(R5)    NO MORE REAL ENTRIES?                        
         BZ    DMXRNMTC            NO MORE                                      
         CLC   CPKEY,13(R5)                                                     
         BE    *+12                                                             
         AHI   R5,L'DSNBUFFR                                                    
         B     DMXR10                                                           
*                                                                               
         L     R4,AREC                                                          
         CLC   0(L'DOKEY,R4),0(R5)                                              
         BNE   DMXR30                                                           
         MVC   P(23),=C'****SAME AS ON FILE****'                                
         GOTO1 =V(HEXOUT),DMCB,0(R4),P+23,13                                    
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*                                                                               
DMXR30   MVC   P(20),=C'CURRENTLY ON FILE:  '                                   
         GOTO1 =V(HEXOUT),DMCB,0(R5),P+20,26                                    
         GOTO1 VPRINTER                                                         
         MVC   P(24),=C'ORIGINAL DARE ORDER ==> '                               
         GOTO1 =V(HEXOUT),DMCB,0(R4),P+24,13                                    
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*                                                                               
DMXRNMTC L     R4,AREC                                                          
         MVC   P(17),=C'NO MATCH FOR ==> '                                      
         GOTO1 =V(HEXOUT),DMCB,0(R4),P+17,13                                    
         GOTO1 =V(HEXOUT),DMCB,CPKEY,P+50,13                                    
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*                                                                               
DMXEOF   DS    0H                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
VCLUNPK  DS    A                                                                
SVEOR    DS    F                                                                
COUNT    DS    H                   NUMBER OF CLIENT RECORDS CHANGED             
BYTE     DS    X                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
         DS    0D                                                               
ELEM     DS    XL100                                                            
CPKEY    DS    XL13                CLIENT PASSIVE KEY FOR DARE ORDERS           
*                                                                               
FILE     DCB   DDNAME=EXTRACT,DSORG=PS,MACRF=GM,RECFM=VB,BLKSIZE=700,  X        
               LRECL=70,EODAD=INPTEOF                                           
*                                                                               
IOLEN    DS    XL2                                                              
IONUL    DC    2X'00'                                                           
IO       DS    XL30                                                             
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
VMSUNPK  DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
ADSNBFFR DS    A                                                                
MAXNTRYS EQU   300                                                              
DSNBUFFR DS    (MAXNTRYS)XL26             300 ENTRIES FOR 2 KEYS                
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE SPGENDRORD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'109SPLDEXTSC412/01/99'                                      
         END                                                                    
