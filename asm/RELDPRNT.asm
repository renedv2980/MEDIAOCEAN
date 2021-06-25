*          DATA SET RELDPRNT   AT LEVEL 013 AS OF 05/01/02                      
*          DATA SET RELDTEST   AT LEVEL 240 AS OF 09/20/95                      
*PHASE RELDPRNT,*                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'RELDXYH - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
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
*  MODULE TO EXTRACT PURGED RECORDS                               *             
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
         AP    EXTRACTD,=P'1'                                                   
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         AP    PURGE,=P'1'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
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
         OPEN  (FILEIN,(INPUT))                                                 
         B     DMXIT                                                            
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                            *             
*******************************************************************             
         SPACE 2                                                                
DMXREC   DS    0H                                                               
* CLEAR IT                                                                      
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         MVC   REC-4(2),27(R6)     GET RECORD LENGTH                            
                                                                                
         XC    REC(256),REC        CLEAR OUTPUT AREA                            
         XC    REC+256(256),REC+256                                             
         XC    REC+512(256),REC+512                                             
         XC    REC+768(240),REC+768                                             
                                                                                
DMXREC10 DS    0H                                                               
         LA    R0,REC-4                                                         
         GET   FILEIN,(R0)                                                      
                                                                                
         AP    TSTCOUNT,=P'1'                                                   
                                                                                
         CLC   TSTCOUNT,=PL5'100'                                               
         BH    DMXRECX                                                          
                                                                                
         MVC   P+3(6),=C'RECORD'                                                
         GOTO1 VPRINTER                                                         
                                                                                
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         B     DMXREC10                                                         
                                                                                
DMXRECX  DS    0H                                                               
         CLOSE (FILEIN,REWIND)                                                  
         B     DMXPGEOF                                                         
         EJECT                                                                  
*******************************************************************             
* END-OF-FILE LOGIC                                               *             
*******************************************************************             
DMXEOF   DS    0H                                                               
         BAS   RE,DMCNT                                                         
*        CLOSE TAPEOUT                                                          
         B     DMXIT               OUTPUT COUNTS                                
         EJECT                                                                  
*******************************************************************             
*              END OF FILE                                        *             
*******************************************************************             
         SPACE 1                                                                
         USING RECD,R3                                                          
DMCNT    NTR1                                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         MVC   P+3(8),=C'TST CNT '                                              
         EDIT  (P5,TSTCOUNT),(7,P+12)                                           
         GOTO1 VPRINTER                                                         
         MVC   P+3(8),=C'EXTRACTD'                                              
         EDIT  (P5,EXTRACTD),(7,P+12)                                           
         GOTO1 VPRINTER                                                         
         MVC   P+3(8),=C'ADVERTIS'                                              
         EDIT  (P5,ADVCOUNT),(7,P+12)                                           
         GOTO1 VPRINTER                                                         
         MVC   P+3(8),=C'PRODUCT '                                              
         EDIT  (P5,PRDCOUNT),(7,P+12)                                           
         GOTO1 VPRINTER                                                         
         MVC   P+3(8),=C'AGENCY  '                                              
         EDIT  (P5,AGYCOUNT),(7,P+12)                                           
         GOTO1 VPRINTER                                                         
         MVC   P+3(8),=C'PURGED  '                                              
         EDIT  (P5,PURGE),(7,P+12)                                              
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
         SPACE 1                                                                
EXTRACTD DC    PL5'0'                                                           
PURGE    DC    PL5'0'                                                           
TSTCOUNT DC    PL5'0'                                                           
ADVCOUNT DC    PL5'0'                                                           
PRDCOUNT DC    PL5'0'                                                           
AGYCOUNT DC    PL5'0'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,LRECL=4004,             X        
               BLKSIZE=32760,MACRF=GM,EODAD=DMXRECX                             
*                                                                               
       ++INCLUDE REMISSING                                                      
*                                                                               
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL1008              AREA FOR RECORD                              
*                                                                               
* DSECT TO COVER MODULE WORKING STORAGE                                         
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
RECUP    DS    V                                                                
         SPACE 1                                                                
PRNTBL   DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
       ++INCLUDE REGENSDD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013RELDPRNT  05/01/02'                                      
         END                                                                    
