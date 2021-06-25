*          DATA SET GELDEXTRFP AT LEVEL 030 AS OF 05/01/02                      
*PHASE GELDRFPX                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'GELDEXT - FIND RFP REQUESTS WITH NO ORIGIN ID'                  
*                                                                               
* EXTERN TO PURGE OLD BROADCAST RECORDS                                         
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
         NMOD1 (WORKX-WORKD),GELDEXT                                            
         USING WORKD,RC                                                         
         EJECT                                                                  
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
DMXINIT  DS    0H                                                               
*                                                                               
         B     DMXIT                                                            
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R6,AREC                                                          
         LR    R5,R6                                                            
*                                                                               
         USING GRPKEYD,R6                                                       
         CLC   =X'002F',GRPKEY     RFP RECORD?                                  
         BNE   DMXKEEP             NO - KEEP                                    
*                                                                               
         MVI   ELCODE,GRPHCDQ      FIND INFO ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   FINDREQS                                                         
*                                                                               
         USING GRPHD,R6                                                         
         MVC   NEXTRUN,GRPHNXTR    SAVE RELEVANT DATES                          
         MVC   LASTRUN,GRPHLSTR                                                 
         MVC   ENDRUN,GRPHEND                                                   
         DROP  R6                                                               
*                                                                               
FINDREQS MVI   ELCODE,GRPRCDQ      FIND REQUEST CARD ELEMENTS                   
         LR    R6,R5               POINT BACK TO START OF RECORD                
         BAS   RE,GETEL                                                         
*                                                                               
CHKELEM  BNE   DMXKEEP             NO MORE ELEMENTS TO EXAMINE                  
*                                                                               
         USING GRPRD,R6                                                         
         CLI   GRPRCRDN,GRPRCHDQ   IS THIS THE REQUEST HEADER?                  
         BNE   NEXTCARD                                                         
         LA    R3,GRPCARD                                                       
         USING RQHHDRD,R3                                                       
         OC    RQHORIG,RQHORIG                                                  
         BNZ   NEXTCARD                                                         
*                                                                               
         MVI   P,0                                                              
         GOTO1 VPRINTER                                                         
         MVI   P,0                                                              
         GOTO1 VPRINTER                                                         
         MVI   P,0                                                              
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'GROUP KEY',(R5),C'DUMP',42,=C'2D'             
*                                                                               
         MVC   P(52),=C' NEXTRUN=NONE    , LASTRUN=NONE    , ENDRUN=NON+        
               E    '                                                           
         OC    NEXTRUN,NEXTRUN                                                  
         BZ    PRTLAST                                                          
         GOTO1 =V(DATCON),DMCB,(6,NEXTRUN),(8,P+9)                              
         MVI   P,C'*'                                                           
PRTLAST  OC    LASTRUN,LASTRUN                                                  
         BZ    PRTEND                                                           
         GOTO1 =V(DATCON),DMCB,(6,LASTRUN),(8,P+27)                             
PRTEND   OC    ENDRUN,ENDRUN                                                    
         BZ    PRINTIT                                                          
         GOTO1 =V(DATCON),DMCB,(6,ENDRUN),(8,P+44)                              
PRINTIT  GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'HEADER',(R6),C'DUMP',GRPRLNQ,=C'2D'           
         BAS   RE,NEXTEL                                                        
         BNE   DMXKEEP                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'1ST CARD',(R6),C'DUMP',GRPRLNQ,=C'2D'         
         DROP  R3                                                               
*                                                                               
NEXTCARD BAS   RE,NEXTEL                                                        
         B     CHKELEM                                                          
         DROP  R6                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MVC   P+10(37),=C'BROADCAST RECORDS DELETED BY DAVID E.'               
         EDIT  (P4,RECDEL),(8,P),ZERO=NOBLANK                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
         GETEL R6,42,ELCODE                                                     
ELCODE   DS    X                                                                
*                                                                               
NEXTRUN  DS    XL4                                                              
LASTRUN  DS    XL4                                                              
ENDRUN   DS    XL4                                                              
*                                                                               
RECDEL   DC    PL4'0'                                                           
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
*                                                                               
WORK     DS    CL128                                                            
WORKX    DS    0C                                                               
         EJECT                                                                  
RQHHDRD  DSECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
         EJECT                                                                  
*CTGENRFP                                                                       
       ++INCLUDE CTGENRFP                                                       
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030GELDEXTRFP05/01/02'                                      
         END                                                                    
