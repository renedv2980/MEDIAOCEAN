*          DATA SET GELDRERG   AT LEVEL 009 AS OF 11/13/02                      
*PHASE GELDRERG                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'GELDEXT -GENDIR/FIL LOAD/DUMP MODEL EXTERNAL ROUTINE'           
*                                                                               
**********************************************************************          
*  CHANGES:                                                          *          
*  OCT15/02 (BU ) --- DELETION OF TYPE 71 KEYS ON SAME BASIS AS      *          
*                     DELETION OF TYPE 0070 KEYS                     *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
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
**       MVI   P,C' '              CLEAR THE PRINT LINE                         
**       MVC   P+1(L'P-1),P                                                     
**       MVC   P(21),=C'INITIALIZING THE FLAG'                                  
**       GOTO1 VPRINTER            PRINT THE LINE                               
         MVI   KEEPSPCL,0          INITIALIZE 'KEEP 0070 RECORDS' FLAG          
         B     DMXIT                                                            
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R2,AREC                                                          
         USING GSPCRECD,R2                                                      
         TM    34(R2),X'80'        TEST ALREADY DELETED                         
         BO    DMXPURGE            YES                                          
*                                                                               
         CLC   GSPCKTYP,=X'0070'   REPORT SPEC RECORD?                          
         BE    DMXR0100            YES - SO PROCESS IT                          
         BH    DMXR0200            HI = AFTER S/P-P/P REC - SET FLAG            
         CLC   =X'000071',0(R2)    S/P-P/P RECORDS?                             
         BE    DMXR0100            YES - SO PROCESS IT                          
         B     DMXKEEP             LO = BEFORE SPEC REC - JUST CONTINUE         
*                                                                               
DMXR0100 EQU   *                                                                
*                                                                               
         CLI   KEEPSPCL,1          'KEEP 0070/71 RECORDS' FLAG SET?             
         BE    DMXKEEP             YES - KEEP 'EM                               
*                                                                               
         AP    RECDEL,=P'1'        ELSE - INC COUNTER OF PURGED RECORDS         
**       MVI   P,C' '              CLEAR THE PRINT LINE                         
**       MVC   P+1(L'P-1),P                                                     
**       MVC   P(12),=C'DELETED KEY:'                                           
**       GOTO1 =V(HEXOUT),DMCB,GSPCKEY,P+13,32,=C'TOG'                          
**       GOTO1 VPRINTER            PRINT THE LINE                               
         B     DMXPURGE            AND PURGE THE RECORD                         
*                                                                               
DMXR0200 EQU   *                                                                
*                                                                               
         CLI   KEEPSPCL,1          'KEEP 0070/71 RECS' FLAG SET?                
         BE    DMXKEEP             YES - SO JUST CONTINUE                       
*                                                                               
         MVI   KEEPSPCL,1          ELSE - SET THE FLAG                          
**       MVI   P,C' '              CLEAR THE PRINT LINE                         
**       MVC   P+1(L'P-1),P                                                     
**       MVC   P(25),=C'SETTING THE FLAG AT KEY: '                              
**       GOTO1 =V(HEXOUT),DMCB,GSPCKEY,P+25,32,=C'TOG'                          
**       GOTO1 VPRINTER            PRINT THE LINE                               
         B     DMXKEEP             AND CONTINUE                                 
         DROP  R2                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MVC   P+10(12),=C'DELETED RECS'                                        
         EDIT  (P4,RECDEL),(8,P),ZERO=NOBLANK                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
*                                                                               
KEEPSPCL DS    X                   'KEEP 0070/71 RECORDS' FLAG                  
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
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
*                                                                               
WORKX    DS    0C                                                               
         EJECT                                                                  
*GEGENSPEC                                                                      
       ++INCLUDE GEGENSPEC                                                      
*GEGENSPSAL                                                                     
       ++INCLUDE GEGENSPSAL                                                     
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009GELDRERG  11/13/02'                                      
         END                                                                    
