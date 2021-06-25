*          DATA SET PPPUB0D    AT LEVEL 016 AS OF 05/01/02                      
*PHASE T4060DA,+0                                                               
*                                                                               
*     *******   CHANGE LOG   *******                                            
*                                                                               
* SMYE  2/96    INCLUDE PUGENEROL (PUB VERSION OF PPGENEROL)                    
*               ALSO USE PUGENOLD (CURRENTLY SAME AS PPGENOLD)                  
*                                                                               
         TITLE 'T4060D  PUBFILE MAINT - COMMENT SCREEN'                         
         PRINT NOGEN                                                            
T4060D   CSECT                                                                  
         NMOD1 0,T4060D                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T406FFD,RA                                                       
         LA    R9,PUBIO                                                         
         USING PUBREC,R9                                                        
         MVI   LTLIND,0                                                         
*                                                                               
SETIND   DS    0H                                                               
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,GETPUB                                                        
*                                                                               
         LA    R7,PUBREC+33                                                     
         MVI   ELCOD,X'66'                                                      
         BAS   RE,NXTEL                                                         
         BNE   CKIND                                                            
         OI    LTLIND,X'10'                                                     
*                                                                               
CKIND    DS    0H                                                               
         CLI   BACT,1              SEE IF ADD                                   
         BNE   COMSCRN                                                          
         LA    R3,COMBERR                                                       
         TM    LTLIND,X'10'                                                     
         BNZ   ERROR                                                            
COMSCRN  CLI   BYTE2,1                                                          
         BE    FORMATP                                                          
*              COMMENT SCREEN IN TWA SO EDIT IT UNLESS ACTION IS DISP           
         CLI   BACT,2                                                           
         BH    FORMATP                                                          
*                                                                               
EDIT     LA    R6,NUMCOMS          SET R6 TO NUMBER OF COMMENTS                 
         MVI   ESWITCH,0                                                        
         LA    R4,ELEAREA                                                       
         XC    0(250,R4),0(R4)                                                  
         XC    250(250,R4),250(R4)                                              
         LA    R2,COMLIN1H                                                      
COM2     CLI 5(R2),0               NO INPUT                                     
         BE    NXTFLD                                                           
         MVI   0(R4),X'66'                                                      
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LA    R1,2(R1)                                                         
         STC   R1,1(R4)                                                         
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,MOVECOM                                                       
         B     *+10                                                             
*                                                                               
MOVECOM  MVC   2(0,R4),8(R2)                                                    
         LA    R4,COMLEN(R4)                                                    
         OI    ESWITCH,1                                                        
         B     NXTFLD                                                           
*                                                                               
NXTFLD   LA    R2,COMFDLN(R2)                                                   
         BCT   R6,COM2                                                          
         B     UPDATE                                                           
*                                                                               
UPDATE   DS    0H                  NOW DELETE OLD COMMENTS                      
         CLI   LTLIND,X'10'        NONE TO DELETE                               
         BNE   ADDELS                                                           
         MVI   ELCOD,X'66'                                                      
DELETE   LA    R7,PUBREC+33                                                     
         BAS   RE,NXTEL                                                         
         BNE   ADDELS                                                           
         GOTO1 VRECUP,DMCB,(1,PUBREC),0(R7)                                     
         B     DELETE                                                           
*                                                                               
ADDELS   CLI ESWITCH,0             NOW ADD NEW COMMENTS                         
         BE    WRITEP              NONE TO WRITEP                               
         LA    R4,ELEAREA                                                       
ADDEL1   CLI   0(R4),0                                                          
         BE    WRITEP                                                           
*                                                                               
         LA    R7,PUBREC+33        SET R7 TO END OF RECORD                      
         MVI   ELCOD,X'FE'                                                      
         BAS   RE,NXTEL                                                         
         GOTO1 VRECUP,DMCB,(1,PUBREC),0(R4),0(R7)                               
         LA    R4,COMLEN(R4)                                                    
         B     ADDEL1                                                           
*                                                                               
WRITEP   MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,PUTPUB                                                        
         B     DONE                                                             
         EJECT                                                                  
FORMATP  DS    0H                                                               
         CLI   SAVSCRN,X'0D'                                                    
         BNE   FMT2                                                             
         CLI   BACT,1                                                           
         BNE   FMT5                                                             
         MVI   BYTE2,0             TO GENERATE TURNAROUND                       
         B     EDIT                                                             
*                                                                               
FMT2     LA    R6,PBLLAST                                                       
         GOTO1 VCALLOV,DMCB,(R6),X'D90406FD'                                    
         CLI   4(R1),X'FF'                                                      
         BE    VIRGERR                                                          
         MVI   SAVSCRN,X'0D'                                                    
FMT5     TM    LTLIND,X'10'                                                     
         BNZ   PUTCOMS                                                          
*                                                                               
CLFLDS   DS    0H                  CLEAR FIELDS                                 
         LA    R6,NUMCOMS                                                       
         LA    R2,COMLIN1H                                                      
CLFLD5   XC    8(70,R2),8(R2)                                                   
         FOUT  (R2)                                                             
         LA    R2,COMFDLN(R2)                                                   
         BCT   R6,CLFLD5                                                        
*                                                                               
         CLC   PUBKAGY(2),AGYALPHA                                              
         BNE   PROTECT                                                          
         LA    R2,COMLIN1H                                                      
         B     EXIT                                                             
*                                                                               
PUTCOMS  DS    0H                                                               
         LA    R6,NUMCOMS                                                       
         LA    R7,PUBREC+33                                                     
         MVI   ELCOD,X'66'                                                      
         LA    R2,COMLIN1H                                                      
PUTCOM1  BAS   RE,NXTEL                                                         
         BNE   CKSRDS                                                           
         SR    R5,R5                                                            
         IC    R5,1(R7)                                                         
         BCTR  R5,0                                                             
         BCTR  R5,0                                                             
         LTR   R5,R5          ZERO LENGHT                                       
         BZ    PUTCOM2                                                          
         FOUT  (R2),2(R7),(R5)                                                  
PUTCOM2  LA    R2,COMFDLN(R2)                                                   
         BCT   R6,PUTCOM1                                                       
*                                                                               
CKSRDS   DS    0H                                                               
         CLC   PUBKAGY(2),AGYALPHA                                              
         BNE   PROTECT                                                          
         LA    R2,COMLIN1H                                                      
         B     EXIT                                                             
*                                                                               
PROTECT  DS    0H                                                               
         LA    R6,NUMCOMS                                                       
         LA    R2,COMLIN1H                                                      
PROT1    OI    1(R2),X'20'                                                      
         LA    R2,COMFDLN(R2)                                                   
         BCT   R6,PROT1                                                         
         MVI   SAVSCRN,0                                                        
*                                                                               
         CLI   BACT,2                                                           
         BH    DONE                                                             
         LA    R2,COMLIN1H                                                      
         B     EXIT                                                             
         EJECT                                                                  
DONE     MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
*                                                                               
NXTEL    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),0                                                          
         BE    NXTEL2                                                           
         CLC   ELCOD,0(R7)                                                      
         BER   RE                                                               
         B     NXTEL+2                                                          
*                                                                               
NXTEL2   LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
LTLIND   DS    CL1                                                              
ELCOD    DS    CL1                                                              
ASWITCH  DS    CL1                                                              
ESWITCH  DS    CL1                                                              
VIRGERR  DC    H'0'                                                             
*                                                                               
COMBERR  EQU   112                 INVALID MEDIA/ACTION/SCREEN                  
*                                                                               
NUMCOMS  EQU   6                                                                
COMLEN   EQU   72                                                               
COMFDLN  EQU   78                                                               
*                                                                               
       ++INCLUDE PUGENEROL                                                      
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
ELEAREA  DS    500C                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE PUGENOLD                                                       
*                                                                               
         DS    CL30                                                             
PUBIO    DS    CL4000                                                           
         ORG   PUBIO                                                            
       ++INCLUDE PUBREC                                                         
         SPACE 2                                                                
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
       ++INCLUDE PPPUBFFD                                                       
         ORG   PBLLAST                                                          
       ++INCLUDE PPPUBFDD                                                       
*                                                                               
*                                                                               
         ORG   T406FFD                                                          
         DS    CL16                                                             
BMED     DS    CL1                                                              
BACT     DS    CL1                                                              
BSCR     DS    CL1                                                              
OLNUM    DS    CL1                                                              
PUBADDR  DS    F                                                                
LTLADDR  DS    F                                                                
BPUB     DS    CL6                                                              
BCLT     DS    CL3                                                              
BDIV     DS    CL3                                                              
BDATE    DS    CL3                                                              
APROF    DS    CL1                                                              
SAVSCRN  DS    CL1                                                              
APROF13  DS    CL1                                                              
BCODE    DS    CL3                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016PPPUB0D   05/01/02'                                      
         END                                                                    
