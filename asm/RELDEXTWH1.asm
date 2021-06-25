*          DATA SET RELDEXTWH1 AT LEVEL 025 AS OF 12/01/99                      
*PHASE RELDWH1                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'RELDCAN - SCAN FOR DARE ORDERS THAT ARE GONE'                   
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
*******************************************************************             
*                                                                 *             
* CLEANUP BAD TAKEOVER MAKEGOOD OFFERS                            *             
*                                                                 *             
*******************************************************************             
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT,RR=R5                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
*******************************************************************             
* CONTROL FLOW LOGIC                                              *             
*******************************************************************             
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,PRNTBL                                                        
*                                                                               
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         SPACE 1                                                                
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         SPACE 1                                                                
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         SPACE 1                                                                
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         AP    PURGE,=P'1'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
*******************************************************************             
* INITIALISE LOGIC                                                *             
*******************************************************************             
         SPACE 2                                                                
DMXINIT  DS    0H                                                               
         XC    DELKEY,DELKEY                                                    
         B     DMXIT                                                            
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                            *             
*******************************************************************             
         SPACE 2                                                                
DMXREC   DS    0H                                                               
         LA    R1,DSNBUFFR                                                      
         OC    0(L'DSNBUFFR,R1),0(R1)                                           
         BNZ   DMXREC00                                                         
*                                                                               
         OPEN  (FILE,INPUT)                                                     
         LA    RE,DSNBUFFR         CLEAR THE BUFFER                             
         LHI   RF,MAXNTRYS*L'DSNBUFFR                                           
         XCEFL                                                                  
         LA    RE,DSNBUFFR                                                      
         ST    RE,ADSNBFFR         A(NEXT AVAILABLE ENTRY)                      
         MVC   P(31),=C'LOOKING FOR THESE ORDER NUMBERS'                        
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R3,IOLEN                                                         
DMXR10   GET   FILE,(R3)                                                        
         L     R5,ADSNBFFR         A(NEXT AVAILABLE ENTRY)                      
         MVC   0(L'DSNBUFFR,R5),IO+16                                           
         GOTO1 =V(HEXOUT),DMCB,0(R5),P,4                                        
         GOTO1 VPRINTER                                                         
         AHI   R5,L'DSNBUFFR                                                    
         ST    R5,ADSNBFFR                                                      
         LA    R1,DSNBUFFR                                                      
         AHI   R1,MAXNTRYS*L'DSNBUFFR                                           
         CR    R5,R1                                                            
         BL    DMXR10                                                           
         DC    H'0'                                                             
*                                                                               
INPTEOF  MVC   P(31),=C'LOOKING FOR THESE ORDER NUMBERS'                        
         GOTO1 VPRINTER                                                         
         CLOSE FILE                                                             
*                                                                               
DMXREC00 L     R5,AREC             POINT TO RECORD                              
         USING RDARKEY,R5                                                       
*                                                                               
         CLI   RDARKTYP,X'41'                                                   
         BE    *+12                                                             
         CLI   RDARKTYP,X'51'                                                   
         BNE   DMXKEEP                                                          
*                                                                               
         CLI   RDARKRT,X'10'       AGENCY HEADER ONLY                           
         BNE   DMXKEEP                                                          
*                                                                               
         LA    R1,DSNBUFFR                                                      
         LR    RF,R1                                                            
         AHI   RF,MAXNTRYS*L'DSNBUFFR                                           
DMXREC10 CR    R1,RF                                                            
         BNL   DMXKEEP                                                          
         OC    0(L'DSNBUFFR,R1),0(R1)                                           
         BZ    DMXKEEP                                                          
         CLC   RDARKORD,0(R1)                                                   
         BE    *+12                                                             
         AHI   R1,L'DSNBUFFR                                                    
         B     DMXREC10                                                         
*                                                                               
         MVC   P(18),=C'**** GOT ONE! ****'                                     
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(5),RDARKAGY                                                    
         GOTO1 =V(HEXOUT),DMCB,RDARKORD,P+7,4                                   
         MVC   P+20(2),RDARKREP                                                 
         GOTO1 =V(HEXOUT),DMCB,RDARREP#,P+25,4                                  
         MVC   P+35(6),RDARKSTA                                                 
         GOTO1 VPRINTER                                                         
         B     DMXKEEP             DONE                                         
         DROP  R5                                                               
         EJECT                                                                  
*******************************************************************             
* END-OF-FILE LOGIC                                               *             
*******************************************************************             
DMXEOF   DS    0H                                                               
         B     DMXIT               OUTPUT COUNTS                                
         EJECT                                                                  
         GETEL R5,34,ELCODE                                                     
         SPACE 1                                                                
PURGE    DC    PL5'0'                                                           
CHANGE   DC    PL5'0'                                                           
         EJECT                                                                  
FILE     DCB   DDNAME=EXTRACT,DSORG=PS,MACRF=GM,RECFM=VB,BLKSIZE=700,  X        
               LRECL=70,EODAD=INPTEOF                                           
         LTORG                                                                  
         EJECT                                                                  
* DSECT TO COVER MODULE WORKING STORAGE                                         
*                                                                               
IOLEN    DS    XL2                                                              
IONUL    DC    2X'00'                                                           
IO       DS    XL30                                                             
MAXNTRYS EQU   100                                                              
DSNBUFFR DS    (MAXNTRYS)XL4              100 ENTRIES FOR DARE ORD #            
         EJECT                                                                  
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
RECUP    DS    V                                                                
         SPACE 1                                                                
PRNTBL   DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
ELEMENT  DS    XL256                                                            
DELKEY   DS    CL27                                                             
*                                                                               
ADSNBFFR DS    A                                                                
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
       ++INCLUDE REGENSDD                                                       
RDARRECD DSECT                                                                  
       ++INCLUDE REGENDAR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025RELDEXTWH112/01/99'                                      
         END                                                                    
