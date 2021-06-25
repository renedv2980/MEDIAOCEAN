*          DATA SET REREPID02  AT LEVEL 066 AS OF 05/01/02                      
*PHASE REID02A,*                                                                
ID02     TITLE 'INTEREP SUBSIDIARY DELETE EXTERNAL'                             
********************************************************************            
*                                                                  *            
* REREPID02 -- INTEREP SUBSIDIARY REP DELETE EXTERNAL              *            
*                                                                  *            
********************************************************************            
*                                                                               
REID02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**REID**,RR=R5                                                 
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         LA    R9,2048(RB)         2ND BASE REGISTER                            
         LA    R9,2048(R9)                                                      
         LA    R7,2048(R9)         3RD BASE REGISTER                            
         LA    R7,2048(R7)                                                      
         USING REID02+4096,R9,R7                                                
         SPACE 2                                                                
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    SW10                                                             
SWXIT    XIT1                                                                   
         SPACE 2                                                                
SW10     DS    0H                                                               
         OPEN  (FILEIN,(INPUT))                                                 
         OPEN  (FILOUTA,(OUTPUT))                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         EJECT                                                                  
*                                                                               
*- READ RECORDS FROM REP FILE DUMP TAPE AND PROCESS THOSE KEYS                  
*  FOUND IN DATA TABLE                                                          
*                                                                               
LOOP100  DS    0H                                                               
         LA    R0,REC-4            POINT TO REC-4                               
         GET   FILEIN,(R0)         (ALLOW FOR 4-BYTE HEADER)                    
*                                                                               
*- ACCEPT RECORDS FOR PROCESS IF RECORD ID FOUND IN DDTABLE.                    
         LA    R3,REC                                                           
         LA    R2,DDTABLE                                                       
*                                                                               
FILT100  CLI   0(R2),0                                                          
         BE    KEEP                REC ID NOT IN TBL. KEEP REC.                 
*                                                                               
         CLC   0(1,R3),DDKEYID(R2) REC ID = TABLE ID?                           
         BE    PROCESS                                                          
*                                                                               
         LA    R2,DDLENTRY(R2)     NEXT TABLE ENTRY                             
         B     FILT100                                                          
         SPACE 2                                                                
KEEP     LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         B     LOOP100             CONTINUE                                     
         SPACE 2                                                                
PURGE    AP    PURGCNT,=P'1'       INCREMENT RECORD COUNT                       
*                                                                               
***      MVC   P(10),=C'*PURGED*'                                               
***      GOTO1 HEXOUT,DMCB,REC,P+10,27,=C'DFLT',54                              
***      GOTO1 REPORT                                                           
*                                                                               
         B     LOOP100             CONTINUE                                     
         SPACE 2                                                                
SWEOF    DS    0H'0'                                                            
*                                                                               
***      MVC   P(10),=C'END INPUT'                                              
***      GOTO1 REPORT                                                           
*                                                                               
         BAS   RE,COUNT            PRINT OUT RUN COUNTS                         
         CLOSE FILEIN                                                           
         CLOSE FILOUTA                                                          
         B     SWXIT                                                            
         EJECT                                                                  
*                                                                               
*- PROCESS.  RECORD ID (AT (R3)) MATCHES TABLE ENTRY (AT (R2)                   
*                                                                               
*  IF RECORD IS AN INTEREP SUBSIDIARY, 'PURGE' THE RECORD.                      
*  ELSE KEEP IT.                                                                
*                                                                               
*  COUNT RECORDS BEING PURGED.                                                  
*                                                                               
PROCESS  DS    0H                                                               
*                                                                               
***      MVC   P(10),=C'*PROCESS'                                               
***      GOTO1 HEXOUT,DMCB,REC,P+10,27,=C'DFLT',54                              
***      GOTO1 REPORT                                                           
*                                                                               
         LA    R1,SUBREP           SUBSIDIARY REP CODE LIST                     
         ZIC   RE,DDREP(R2)        REP CODE DISPLACEMENT                        
         AR    RE,R3               RE=A(REP CODE IN RECORD)                     
*                                                                               
PROC100  CLI   0(R1),0                                                          
         BE    KEEP                REP NOT IN LIST. KEEP RECORD.                
*                                                                               
         CLC   0(2,R1),0(RE)                                                    
         BE    PROC200                                                          
*                                                                               
         LA    R1,2(R1)            NEXT REP IN REP LIST                         
         B     PROC100                                                          
*                                                                               
*- ADD TO PURGE COUNT & EXIT VIA PURGE LABEL                                    
*                                                                               
*  COUNTERS ARE IN DDTABLE, 4 PACKED BYTES PER REP IN SUBREP LIST.              
*                                                                               
PROC200  EQU   *                                                                
         LA    RF,SUBREP           DETERMINE PACK CELL DISP FOR                 
         SR    R1,RF                  THIS REP CODE1                            
         SLL   R1,1                                                             
*                                                                               
         LA    R1,DDCOUNT(R1)      ADD IN DISP TO 1ST COUNTER                   
*                                                                               
         AR    R1,R2               ADD IN TABLE ENTRY STARTING ADDRESS          
*                                                                               
         AP    0(4,R1),=P'1'                                                    
*                                                                               
*- PRINT OUT 1ST 5 FOR THIS REP                                                 
***      CLC   0(4,R1),=PL4'5'                                                  
***      BH    PURGE                                                            
*                                                                               
***      MVC   P(8),DDTAG(R2)                                                   
***      MVC   P+10(27),0(R3)      RECORD KEY                                   
***      GOTO1 REPORT                                                           
*                                                                               
         B     PURGE                                                            
         EJECT                                                                  
******************************************************************              
*              END OF FILE - COUNT WHAT'S BEEN DONE              *              
******************************************************************              
         SPACE 1                                                                
COUNT    NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
*                                                                               
         LA    R2,SUBREP           PRINT OUT REP CODE SUB HEADING               
         LA    R3,P+38                                                          
         LA    R0,6                                                             
CNT05    MVC   0(2,R3),0(R2)                                                    
         LA    R2,2(R2)                                                         
         LA    R3,10(R3)                                                        
         BCT   R0,CNT05                                                         
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         LA    R4,DDTABLE                                                       
CNT10    CLI   0(R4),0                                                          
         BE    CNT90                                                            
*                                                                               
         MVC   P+15(8),DDTAG(R4)   LABEL                                        
*                                                                               
         LA    R2,6                6 REP TOTALS TO PRINT                        
         LA    R3,DDCOUNT(R4)      A(1ST PACK COUNTERS)                         
         LA    R5,P+33             OUTPUT AREA                                  
CNT20    EQU   *                                                                
         MVC   FULL(4),0(R3)                                                    
         EDIT  (P4,FULL),(7,(R5))                                               
         LA    R5,10(R5)                                                        
         LA    R3,4(R3)                                                         
         BCT   R2,CNT20                                                         
         SPACE 1                                                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT              SPACING LINE                                 
*                                                                               
         LA    R4,DDLENTRY(R4)                                                  
         B     CNT10                                                            
         SPACE 1                                                                
CNT90    GOTO1 REPORT                                                           
         SPACE 1                                                                
         MVC   P+3(24),=C'**TOTAL RECORDS PURGED**'                             
         EDIT  (P5,PURGCNT),(7,P+31)                                            
         GOTO1 REPORT                                                           
         B     SWXIT                                                            
         EJECT                                                                  
*                                                                               
*- DATA DEFINITION TABLE                                                        
*                                                                               
DDKEYID  EQU   0   XL1             KEY ID                                       
DDREP    EQU   1   XL1             REP CODE DISP IN REC                         
DDTAG    EQU   2   CL8             RECORD LABEL                                 
DDCOUNT  EQU   10  6P              PACKED PURGE COUNTS                          
DDLENTRY EQU   34                                                               
*                                                                               
DDTABLE  DS    0F                                                               
         DC    X'05',AL1(RTEMKREP-RECD),CL8'TEAM    ',6PL4'0'                   
DDX      EQU   *-DDTABLE                                                        
         DC    X'07',AL1(RGRPKREP-RECD),CL8'GROUP   ',6PL4'0'                   
         DC    X'08',AL1(RADVKREP-RECD),CL8'ADV     ',6PL4'0'                   
         DC    X'09',AL1(RPRDKREP-RECD),CL8'PRODUCT ',6PL4'0'                   
         DC    X'0A',AL1(RAGYKREP-RECD),CL8'AGENCY  ',6PL4'0'                   
         DC    X'0D',AL1(RCLSKREP-RECD),CL8'CLASS   ',6PL4'0'                   
         DC    X'0F',AL1(RCTGKREP-RECD),CL8'CATAGORY',6PL4'0'                   
         DC    X'2A',AL1(ROWNKREP-RECD),CL8'OWNER   ',6PL4'0'                   
         DC    H'0'                EOT                                          
         SPACE 2                                                                
SUBREP   DS    0H                  SUBSIDIARY REP CODES                         
         DC    C'TO'               TORBET                                       
         DC    C'I1'               MMR                                          
         DC    C'HN'               HILLIARD                                     
         DC    C'DI'               DURPETTI                                     
         DC    C'MG'               MCGAVERN                                     
         DC    C'GP'               GROUP W                                      
         DC    H'0'                EOT                                          
         SPACE 3                                                                
RELO     DS    F                   RELOCATION FACTOR                            
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL1008              AREA FOR RECORD                              
*                                                                               
PURGCNT  DC    PL5'0'                                                           
         SPACE 3                                                                
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,MACRF=GM,               X        
               EODAD=SWEOF                                                      
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*- MASTER RECORDS DSECTS ORG TO RECD.                                           
*                                                                               
RECD     DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENTEM                                                       
         ORG   RECD                                                             
       ++INCLUDE REGENGRP                                                       
         ORG   RECD                                                             
       ++INCLUDE REGENADV                                                       
         ORG   RECD                                                             
       ++INCLUDE REGENPRD                                                       
         ORG   RECD                                                             
       ++INCLUDE REGENAGY                                                       
         ORG   RECD                                                             
       ++INCLUDE REGENCLS                                                       
         ORG   RECD                                                             
       ++INCLUDE REGENCTG                                                       
         ORG   RECD                                                             
       ++INCLUDE REGENOWN                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 4                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066REREPID02 05/01/02'                                      
         END                                                                    
