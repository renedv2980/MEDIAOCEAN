*          DATA SET TALDFIX9   AT LEVEL 013 AS OF 05/01/02                      
*PHASE TALDFI7A TALDFIX7                                                        
*INCLUDE SCANNER                                                                
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE TINVCON                                                                
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'TALDFIX7 - TALENT LOAD/DUMP FILE FIX MODULE'                    
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
         USING TLIND,R5                                                         
         L     R6,AREC             POINT TO RECORD                              
         LR    R5,R6                                                            
         CLI   0(R6),TLINCDQ       ONLY WANT IN RECS                            
         BNE   DMXR5                                                            
         CLC   TLINAGY,=CL6'0323'  ONLY WANT 0323                               
         BNE   DMXPURGE                                                         
         CLC   TLININV,=X'E668F9FFFBFF'                                         
         BNE   DMXPURGE                                                         
         AP    COUNT,=P'1'                                                      
         B     DMXKEEP                                                          
******************************                                                  
         MVI   ELCODE,TAINELQ      LOOK FOR IN DETAILS EL.                      
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP                                                          
         USING TAIND,R6                                                         
         TM    TLINSTAT,X'80'      TEST DELETED                                 
         BZ    DMXKEEP                                                          
         AP    COUNT,=P'1'             INCREMENT COUNTER                        
         LA    R4,P                                                             
         USING LINED,R4                                                         
         MVC   LINAGY,TLINAGY                                                   
         MVC   DUB(6),TLININV                                                   
         XC    DUB(6),=6X'FF'                                                   
         GOTO1 =V(TINVCON),DMCB,DUB,LININV,=V(DATCON)                           
         MVC   LINIINFL,=C'IINF:'                                               
         MVC   LINIINF,TAINIST                                                  
         MVC   LINPTIML,=C'PTIM:'                                               
         MVC   LINPTIM,=C'000'                                                  
         OC    TAINPTIM,TAINPTIM                                                
         BZ    *+10                                                             
         MVC   LINPTIM,=C'XXX'                                                  
         MVC   LINPDTEL,=C'PD:'                                                 
         GOTO1 =V(DATCON),DMCB,(1,TAINPDTE),(0,LINPDTE)                         
         MVC   LINSTA1L,=C'ST1:'                                                
         GOTO1 =V(HEXOUT),DMCB,TAINSTAT,LINSTA1,1,0                             
         MVC   LINSTA2L,=C'ST2:'                                                
         GOTO1 =V(HEXOUT),DMCB,TAINSTA2,LINSTA2,1,0                             
         MVC   LINBDTEL,=C'BILL:'                                               
         GOTO1 =V(DATCON),DMCB,(1,TAINBDTE),(0,LINBDTE)                         
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
         SPACE 2                                                                
DMXR5    CLI   0(R5),TLCKCDQ       WANT CHECK RECORDS ONLY                      
         BNE   DMXPURGE                                                         
         USING TLCKD,R5                                                         
         USING TACDD,R6                                                         
         CLC   TLCKAGY,=CL6'0323'  ONLY WANT 0323                               
         BNE   DMXPURGE                                                         
         CLC   TLCKINV,=X'199706000400'                                         
         BNE   DMXPURGE                                                         
         AP    COUNT,=P'1'                                                      
         B     DMXKEEP                                                          
**********************                                                          
         TM    TLCKSTAT,X'80'      TEST DELETED                                 
         BZ    DMXKEEP                                                          
         AP    COUNT,=P'1'             INCREMENT COUNTER                        
         LA    R4,P                                                             
         USING LINED,R4                                                         
         MVI   ELCODE,TACDELQ      LOOK FOR CHECK DETAILS EL.                   
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP                                                          
         MVC   LINAGY,TLCKAGY                                                   
         GOTO1 =V(TINVCON),DMCB,TLCKINV,LININV,=V(DATCON)                       
         MVC   LINCKNUM,TACDCHK                                                 
         MVC   LINCDTEL,=C'CKDTE:'                                              
         OC    TACDDTE,TACDDTE                                                  
         BZ    DMXR9                                                            
         GOTO1 =V(DATCON),DMCB,(1,TACDDTE),(0,LINCDTE)                          
DMXR9    GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
         SPACE 3                                                                
COUNT    DC    PL6'0'                                                           
CNT6MON  DC    PL6'0'                                                           
CNTAFTER DC    PL6'0'                                                           
INCOUNT  DC    PL6'0'                                                           
CKCOUNT  DC    PL6'0'                                                           
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
         MVC   P(21),=CL24'# OF KEPT RECORDS = '                                
         EDIT  COUNT,(12,P+24),ALIGN=LEFT,COMMAS=YES,ZERO=NOBLANK               
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         MVC   P(24),=CL24'# DATES AFTER TODAY  = '                             
         EDIT  CNTAFTER,(12,P+24),ALIGN=LEFT,COMMAS=YES,ZERO=NOBLANK            
         GOTO1 VPRINTER                                                         
         MVC   P(24),=CL24'# DATES OLDER THAN 6M= '                             
         EDIT  CNT6MON,(12,P+24),ALIGN=LEFT,COMMAS=YES,ZERO=NOBLANK             
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
*              ODDMENTS!                                                        
         SPACE 3                                                                
DATADISP DC    H'40'                                                            
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
DELEL    NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'D',=C'TALFIL'),(ELCODE,AREC),0                 
         B     XIT                                                              
         SPACE 1                                                                
ADDEL    NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'TALFIL'),AREC,ELEMENT                    
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
LINED    DSECT                                                                  
LINAGY   DS    CL6                                                              
         DS    C                                                                
LININV   DS    CL6                                                              
         DS    C                                                                
LINIINFL DS    CL5                                                              
         DS    C                                                                
LINIINF  DS    CL8                                                              
         DS    C                                                                
LINPTIML DS    CL5                                                              
         DS    C                                                                
LINPTIM  DS    CL3                                                              
         DS    C                                                                
LINPDTEL DS    CL3                                                              
         DS    C                                                                
LINPDTE  DS    CL6                                                              
         DS    C                                                                
LINSTA1L DS    CL4                                                              
         DS    C                                                                
LINSTA1  DS    CL2                                                              
         DS    C                                                                
LINSTA2L DS    CL4                                                              
         DS    C                                                                
LINSTA2  DS    CL2                                                              
         DS    C                                                                
LINBDTEL DS    CL5                                                              
         DS    C                                                                
LINBDTE  DS    CL6                                                              
         ORG   LINIINFL                                                         
LINCKNUM DS    CL8                                                              
         DS    C                                                                
LINCDTEL DS    CL6                                                              
         DS    C                                                                
LINCDTE  DS    CL6                                                              
         DS    C                                                                
LINCST1L DS    CL4                                                              
         DS    C                                                                
LINCST1  DS    CL2                                                              
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
ELCODE   DS    CL1                                                              
         DS    0D                                                               
ELEMENT  DS    CL240                                                            
         DS    0D                                                               
BLOCK    DS    320C                                                             
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
**PAN#1  DC    CL21'013TALDFIX9  05/01/02'                                      
         END                                                                    
