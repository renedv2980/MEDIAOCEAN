*          DATA SET TALDFIX2   AT LEVEL 011 AS OF 05/01/02                      
*PHASE TALDFI2A TALDFIX2                                                        
*INCLUDE SCANNER                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE DATCON                                                                 
         TITLE 'TALDFIX2 - TALENT LOAD/DUMP FILE FIX MODULE'                    
*                                                                               
*              DO NOT DELETE - MERGES LOCALS                                    
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 1                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  L     R2,APARAMC          POSSIBLE PARAM CARD                          
         LTR   R2,R2                                                            
         BZ    DMXIT                                                            
         CLI   0(R2),X'41'                                                      
         BL    DMXIT                                                            
         MVC   CARD,SPACES                                                      
         MVC   CARD(75),0(R2)                                                   
         GOTO1 =V(SCANNER),DMCB,(C'C',CARD),(10,BLOCK)                          
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BE    BADPARAM                                                         
         LA    R2,BLOCK                                                         
         SPACE 1                                                                
INIT2    DS    0H                                                               
         SPACE 1                                                                
INITNXT  LA    R2,32(R2)                                                        
         BCT   R4,INIT2                                                         
         B     DMXIT                                                            
         SPACE 1                                                                
BADPARAM MVC   P(30),=CL30'**BAD PARAMETER CARD**'                              
         GOTO1 VPRINTER                                                         
         MVC   P(80),CARD                                                       
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
DMXREC   DS    0H                                                               
         L     R4,AREC                                                          
         CLI   0(R4),TLW4CDQ       LOOKING FOR W4 RECORDS                       
         BNE   DMX20                                                            
         MVI   ELCODE,TAW4ELQ      GET W4 DETAILS EL.                           
         LR    R3,R4                                                            
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP                                                          
         USING TAW4D,R3            R3=A(W4 DETAILS EL.)                         
         CLC   TAW4LOCL,LOCLFROM   IF THIS IS FROM AFM LOCAL                    
         BNE   DMXKEEP                                                          
         MVC   TAW4LOCL,LOCLTO     THEN CHANGE IT TO NEW LOCAL                  
         AP    W4COUNT,=P'1'       AND ADD 1 TO W4 COUNTER                      
**NO-OP* BAS   RE,TRACE                                                         
         B     DMXKEEP                                                          
*                                                                               
DMX20    CLI   0(R4),TLCACDQ       OR CAST RECORDS                              
         BNE   DMXKEEP                                                          
         MVI   ELCODE,TACAELQ      GET CAST DETAILS EL.                         
         LR    R3,R4                                                            
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP                                                          
         USING TACAD,R3            R3=A(CAST DETAILS EL.)                       
         CLC   TACAUN,=C'AFM'      IF UNION IS AFM                              
         BNE   DMXKEEP                                                          
         CLC   TACALOCL,LOCLFROM   AND THIS IS FROM LOCAL                       
         BNE   DMXKEEP                                                          
         MVC   TACALOCL,LOCLTO     THEN CHANGE IT TO NEW LOCAL                  
         AP    CACOUNT,=P'1'       AND ADD 1 TO CAST COUNTER                    
**NO-OP* BAS   RE,TRACE                                                         
         B     DMXKEEP                                                          
         SPACE 3                                                                
LOCLFROM DC    C'072'                                                           
LOCLTO   DC    C'72 '                                                           
         SPACE 1                                                                
W4COUNT  DC    PL6'0'                                                           
CACOUNT  DC    PL6'0'                                                           
         EJECT                                                                  
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*                                                                               
DMXRET   DS    0H                                                               
         L     R3,AREC             POINT TO LAST RECORD                         
         B     DMXPURGE                                                         
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(20),=C'W4 RECORDS CHANGED ='                                   
         EDIT  W4COUNT,(12,P+21),ALIGN=LEFT,COMMAS=YES,ZERO=NOBLANK             
         GOTO1 VPRINTER                                                         
         MVC   P(22),=C'CAST RECORDS CHANGED ='                                 
         EDIT  CACOUNT,(12,P+23),ALIGN=LEFT,COMMAS=YES,ZERO=NOBLANK             
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         SPACE 3                                                                
*        TRACE ROUTINE                                                          
*                                                                               
TRACE    NTR1                                                                   
         L     R4,AREC                                                          
         LH    R3,DATADISP         PRINT OUT KEY                                
         GOTO1 =V(PRNTBL),DMCB,0,(R4),C'DUMP',(R3),=X'01C4'                     
         B     TR100               ONLY PRINT KEY FOR NOW                       
*                                                                               
TR15     L     R4,AREC             A(RECORD)                                    
         AH    R4,DATADISP         + DISPLACEMENT TO FIRST ELEMENT              
         MVI   ELCODE,0                                                         
         BAS   RE,FIRSTEL                                                       
         BNE   TR100                                                            
*                                                                               
TR20     ZIC   R3,1(R4)            PRINT ELEMENT                                
         GOTO1 =V(PRNTBL),DMCB,0,(R4),C'DUMP',(R3),=X'01C4'                     
         BAS   RE,NEXTEL           REPEAT UNTIL NO MORE ELEMENTS                
         BE    TR20                                                             
*                                                                               
TR100    DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
DELEL    NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'D',=C'TALFIL'),(ELCODE,AREC),0                 
         B     DMXIT                                                            
         SPACE 3                                                                
ADDEL    NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'TALFIL'),(0,AREC),(R3)                   
         B     DMXIT                                                            
         SPACE 3                                                                
         GETEL (R3),DATADISP,ELCODE                                             
         EJECT                                                                  
DATADISP DC    H'40'                                                            
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
WORK     DS    CL32                                                             
*                                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         SPACE 1                                                                
CARD     DS    CL80                                                             
BLOCK    DS    320C                                                             
ELCODE   DS    XL1                                                              
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
*********INCLUDE TAGENFILE                                                      
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011TALDFIX2  05/01/02'                                      
         END                                                                    
