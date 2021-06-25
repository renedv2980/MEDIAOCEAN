*          DATA SET TALDTRMX   AT LEVEL 003 AS OF 05/01/02                      
*          DATA SET TALDTRIM   AT LEVEL 016 AS OF 03/30/90                      
*PHASE TALDTRXA TALDTRMX                                                        
*INCLUDE SCANNER                                                                
         TITLE 'TALDTRIM - TALENT LOAD/DUMP MODEL EXTERNAL ROUTINE'             
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
DMXKEEP  BAS   RE,ANYMODS          ANY RECORD FIXES?                            
         L     R1,APARM            KEEP RECORD EXIT                             
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
INIT2    LA    R3,TRIMLIST         LOOK FOR XX=Y/N                              
         SPACE 1                                                                
INIT4    CLC   12(2,R2),4(R3)                                                   
         BE    INIT6                                                            
         LA    R3,12(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BE    INIT8                                                            
         B     INIT4                                                            
         SPACE 1                                                                
INIT6    MVC   2(2,R3),22(R2)      MATCH FOUND - SAVE Y/N                       
         B     INITNXT                                                          
         SPACE 1                                                                
INIT8    CLC   12(3,R2),=C'ID '    ID=NNN ALLOWED                               
         BNE   INIT10                                                           
         MVC   SYSID,8(R2)                                                      
         OC    SYSID,SYSID                                                      
         BZ    INIT10                                                           
         B     INITNXT                                                          
         SPACE 1                                                                
INIT10   B     BADPARAM                                                         
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
         L     R3,AREC             POINT TO RECORD                              
         USING TLDRREC,R3                                                       
         LA    R1,TRIMLIST                                                      
         SPACE 1                                                                
TRIM2    CLC   TLDRCD,0(R1)        LOOK UP RECORD TYPE IN LIST                  
         BE    TRIM4                                                            
         LA    R1,12(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BE    DMXKEEP                                                          
         B     TRIM2                                                            
         SPACE 1                                                                
TRIM4    CLI   2(R1),C'Y'                                                       
         BNE   DMXPURGE                                                         
         CLI   1(R1),0                                                          
         BE    DMXKEEP                                                          
         SPACE 1                                                                
TRIM6    ZIC   R2,1(R1)            DISPLACEMENT TO AGENCY FIELD                 
         AR    R2,R3               NOW R2=A(AGENCY)                             
         CLC   0(6,R2),=C'TFNY  '                                               
         BE    DMXKEEP                                                          
         CLC   0(6,R2),=C'IKCH  '                                               
         BE    DMXKEEP                                                          
         CLC   0(6,R2),=C'KRAFT '                                               
         BE    DMXKEEP                                                          
         CLC   0(6,R2),=C'DDS   '                                               
         BE    DMXKEEP                                                          
         B     DMXPURGE                                                         
         SPACE 1                                                                
TRIMLIST DS    0F                                                               
         DC    AL1(TLSYCDQ),AL1(0),C'Y ',CL8'SY'                                
         DC    AL1(TLSTCDQ),AL1(0),C'Y ',CL8'ST'                                
         DC    AL1(TLAGCDQ),AL1(0),C'Y ',CL8'AG'                                
         DC    AL1(TLCGCDQ),AL1(0),C'Y ',CL8'CG'                                
         DC    AL1(TLPGCDQ),AL1(0),C'Y ',CL8'PG'                                
         DC    AL1(TLANCDQ),AL1(0),C'N ',CL8'AN'                                
         DC    AL1(TLEMCDQ),AL1(0),C'N ',CL8'EM'                                
         DC    AL1(TLW4CDQ),AL1(0),C'N ',CL8'W4'                                
         DC    AL1(TLPHCDQ),AL1(0),C'N ',CL8'PH'                                
         DC    AL1(TLGUCDQ),AL1(0),C'N ',CL8'GU'                                
         DC    AL1(TLDUCDQ),AL1(0),C'N ',CL8'DU'                                
         DC    AL1(TLLNCDQ),AL1(0),C'N ',CL8'LN'                                
         DC    AL1(TLGTCDQ),AL1(0),C'N ',CL8'GT'                                
         DC    AL1(TLLOCDQ),AL1(0),C'Y ',CL8'LO'                                
         DC    AL1(TLBACDQ),AL1(0),C'Y ',CL8'BA'                                
         DC    AL1(TLOFCDQ),AL1(0),C'Y ',CL8'OF'                                
         SPACE 1                                                                
         DC    AL1(TLAYCDQ),AL1(TLAYAGY-TLAYD),C'Y ',CL8'AY'                    
         DC    AL1(TLATCDQ),AL1(TLATAGY-TLATD),C'Y ',CL8'AT'                    
         DC    AL1(TLCLCDQ),AL1(TLCLAGY-TLCLD),C'Y ',CL8'CL'                    
         DC    AL1(TLPRCDQ),AL1(TLPRAGY-TLPRD),C'Y ',CL8'PR'                    
         DC    AL1(TLCOCDQ),AL1(TLCOAGY-TLCOD),C'Y ',CL8'CO'                    
         DC    AL1(TLMUCDQ),AL1(TLMUAGY-TLMUD),C'Y ',CL8'MU'                    
         DC    AL1(TLIFCDQ),AL1(TLIFAGY-TLIFD),C'Y ',CL8'IF'                    
         DC    AL1(TLCACDQ),AL1(TLCAAGY-TLCAD),C'Y ',CL8'CA'                    
         DC    AL1(TLINCDQ),AL1(TLINAGY-TLIND),C'Y ',CL8'IN'                    
         DC    AL1(TLUHCDQ),AL1(TLUHAGY-TLUHD),C'Y ',CL8'UH'                    
         DC    AL1(TLCKCDQ),AL1(TLCKAGY-TLCKD),C'Y ',CL8'CK'                    
         DC    AL1(TLESCDQ),AL1(TLESAGY-TLESD),C'Y ',CL8'ES'                    
         DC    AL1(TLSCCDQ),AL1(TLSCAGY-TLSCD),C'Y ',CL8'SC'                    
         DC    X'FF'                                                            
         SPACE 1                                                                
SYSID    DC    F'485'                                                           
         EJECT                                                                  
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*                                                                               
DMXRET   DS    0H                                                               
         L     R3,AREC             POINT TO LAST RECORD                         
         B     DMXPURGE                                                         
         EJECT                                                                  
*              CHANCE TO MODIFY RECORDS HERE                                    
         SPACE 3                                                                
ANYMODS  NTR1                                                                   
         L     R6,AREC                                                          
         CLI   0(R6),TLSTCDQ                                                    
         BE    MODSTAFF                                                         
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         USING TLSTD,R6                                                         
MODSTAFF MVC   TLSTUSER,SYSID+2                                                 
         B     XIT                                                              
         SPACE 1                                                                
         GETEL (R6),ELCODE,DATADISP                                             
         SPACE 1                                                                
ELCODE   DC    X'00'                                                            
DATADISP DC    H'40'                                                            
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
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
**PAN#1  DC    CL21'003TALDTRMX  05/01/02'                                      
         END                                                                    
