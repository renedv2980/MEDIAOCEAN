*          DATA SET CTLDEX1    AT LEVEL 002 AS OF 06/12/13                      
*PHASE CTLDEX1A                                                                 
         TITLE 'CTLDEX1 - CTFILE - LAST LOGON ELEMENT'                          
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
*                                   X'FE'= CHANGED RECORD (RECOVERY)            
*                                   X'FD'= NEW RECORD (RECOVERY)                
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= ONLY I/S FILE RECS IN P1             
*                                   X'10'= SPECIAL I/S POINTER IN P9            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
* P7=A(CARDS)                                                                   
* P8=A(PEELDATE)                                                                
* P9=A(ISREC)                                                                   
* P10=A(PARMTBL)                                                                
                                                                                
* EXTERNAL PROCESSES X'04' LAST LOGON ELEMENT IN PERSON RECORDS                 
* ADDS NEW EMPLY X'04' ELEMENT IF DOESNT EXIST                                  
* CHANGES SHORT EXISTING X'04' ELEMENT TO NEW LENGTH OF 15 BYTES                
* LEAVES EXISTING ELEMENTS OF CORRECT NEW LENGTH ALONE                          
* PARAM=AA CARD WILL LIMIT TO ONE AGENCY AA                                     
                                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,CTLDEX1                                              
         USING WORKD,RC                                                         
                                                                                
* CONTROL FLOW LOGIC                                                            
*                                                                               
CTXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     R2,VLDDEFN          R2=A(FILE DEFINITION)                        
         USING LDDEFND,R2                                                       
*                                                                               
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    CTXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    CTXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    CTXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    CTXEOF                                                           
         B     CTXIT               EXIT IF UNKNOWN                              
*                                                                               
CTXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     CTXIT                                                            
*                                                                               
CTXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     CTXIT                                                            
*                                                                               
CTXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     CTXIT                                                            
*                                                                               
CTXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     CTXIT                                                            
*                                                                               
CTXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     CTXIT                                                            
*                                                                               
CTXCHG   L     R1,APARM            CHANGED RECORD (FOR RECOVERY)                
         MVI   0(R1),X'FE'                                                      
         MVI   8(R1),0                                                          
         B     CTXIT                                                            
*                                                                               
CTXADD   L     R1,APARM            ADDED RECORD (FOR RECOVERY)                  
         MVI   0(R1),X'FD'                                                      
         MVI   8(R1),0                                                          
         B     CTXIT                                                            
*                                                                               
CTXIT    XMOD1 1                                                                
                                                                                
*                                                                               
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*   PARAM=AA,PRINT WHERE AA IS THE ALPHA TO FILTER ON.                          
*                                                                               
CTXINIT  XR    RF,RF               GET PARAM=AA CARD AND SAVE IT                
         ICM   RF,7,APARAMC+1                                                   
         MVC   PARAMC,0(RF)                                                     
         CLC   0(3,RF),=C'ALL'     TEST ALL AGENCIES (DEFAULT)                  
         BE    CTXI1                                                            
         CLC   0(2,RF),SPACES                                                   
         BE    CTXI1                                                            
         MVC   AGENCY,0(RF)        ONLY WANT ONE AGENCY                         
*                                                                               
CTXI1    LA    RF,3(RF)            LOOK FOR PRINT OPTION ON PARAM CARD          
         LA    R0,66                                                            
CTXI1A   CLC   0(5,RF),=C'PRINT'                                                
         BE    CTXI2                                                            
         LA    RF,1(RF)                                                         
         BCT   R0,CTXI1A                                                        
         B     CTXIT                                                            
*                                                                               
CTXI2    MVI   PRNT,C'P'           SET TO PRINT ELEMENT ACTIONS                 
         B     CTXIT                                                            
                                                                                
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
CTXREC   SR    R3,R3               R3=A(RECORD)                                 
         ICM   R3,7,AREC+1                                                      
         USING SAPEREC,R3                                                       
         CLI   SAPETYP,SAPETYPQ    TEST PERSON RECORD                           
         BNE   CTXKEEP                                                          
         CLI   SAPESUB,SAPESUBQ                                                 
         BNE   CTXKEEP                                                          
         CLC   AGENCY,SPACES       TEST ONLY ONE AGENCY WANTED                  
         BE    CTXREC0                                                          
         CLC   AGENCY,SAPEAGY                                                   
         BNE   CTXKEEP                                                          
*                                                                               
CTXREC0  MVC   AGY,SAPEAGY         SAVE AGENCY ALPHA                            
         MVC   PER,SAPEPID         SAVE PERSON ALPHA                            
         LA    R4,SAPEDATA         R4=A(ELEMENT)                                
         SR    R5,R5               R5=L'ELEMENT                                 
         LHI   R0,100              R0=MAX NUMBER OF ELEMENTS                    
         XC    ELEM,ELEM                                                        
*                                                                               
CTXREC1  CLI   0(R4),0             TEST END OF RECORD                           
         BE    CTXREC3                                                          
         CLI   0(R4),SALLOELQ      TEST IF LAST LOG ON ELEMENT                  
         BE    CTXREC2                                                          
         ICM   R5,1,1(R4)                                                       
         BZ    CTXRECX                                                          
         AR    R4,R5                                                            
         BCT   R0,CTXREC1                                                       
         B     CTXRECX                                                          
*                                                                               
         USING SALLOD,R4                                                        
CTXREC2  MVC   ELEM(SALLOLNQ),SALLOEL                                           
         CLI   SALLOLEN,SALLOLNQ   TEST IF ALREADY HAS NEW LENGTH               
         BE    CTXREC5                                                          
         LA    R4,ELEM             R4=A(SAVED SHORT ELEMENT)                    
         MVC   ACTN,=C'CHG'                                                     
         MVI   SALLOLEN,SALLOLNQ   SET NEW LENGTH                               
         MVI   SALLOFLG,0          CLEAR NEW FIELDS                             
         XC    SALLOTME,SALLOTME   AND DELETE EXISTING ELEMENT                  
         MVI   SALLOCNT,0                                                       
         GOTO1 LHELLO,DMCB,(C'D',=C'CTFILE'),(X'04',(R3)),0,0                   
         B     CTXREC4                                                          
*                                                                               
CTXREC3  LA    R4,ELEM             R4=A(NEW ELEMENT)                            
         MVC   ACTN,=C'ADD'                                                     
         XC    ELEM(SALLOLNQ),ELEM                                              
         MVI   SALLOEL,SALLOELQ                                                 
         MVI   SALLOLEN,SALLOLNQ                                                
         MVC   SALLODT,=X'640101'                                               
*                                                                               
CTXREC4  GOTO1 LHELLO,DMCB,(C'P',=C'CTFILE'),(R3),ELEM,0                        
         B     CTXREC6                                                          
*                                                                               
CTXREC5  MVC   ACTN,=C'OK '        ELEMENT ALREADY HAS CORRECT LENGTH           
*                                                                               
CTXREC6  CLI   PRNT,C'P'           TEST TO PRINT ELEMENT                        
         BNE   CTXRECX                                                          
         MVC   P(3),ACTN                                                        
         MVC   P+4(2),AGY                                                       
         MVC   P+7(8),PER                                                       
         LHI   R0,SALLOLNQ                                                      
         GOTO1 LHEXOUT,DMCB1,ELEM,P+16,(R0),0                                   
         GOTO1 VPRINTER                                                         
*                                                                               
CTXRECX  MVC   LKEY,0(R3)                                                       
         B     CTXKEEP                                                          
                                                                                
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*                                                                               
CTXRET   L     R3,AREC             POINT TO LAST RECORD                         
         B     CTXPURGE                                                         
                                                                                
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
CTXEOF   B     CTXIT                                                            
         EJECT                                                                  
PARAMC   DC    CL80' '                                                          
AGENCY   DC    CL2' '                                                           
PRNT     DC    CL1' '                                                           
LKEY     DC    XL25'00'                                                         
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
DMCB1    DS    6F                                                               
APARM    DS    A                                                                
*                                                                               
PLIST    DS    0XL40                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
VCARDS   DS    A                                                                
APEELDAT DS    A                                                                
AISREC   DS    A                                                                
APARMTBL DS    A                                                                
*                                                                               
ACTN     DS    CL3                                                              
AGY      DS    CL2                                                              
PER      DS    CL8                                                              
ELEM     DS    XL32                                                             
*                                                                               
WORKX    EQU   *                                                                
                                                                                
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
                                                                                
*SECASFILE                                                                      
       ++INCLUDE SEACSFILE                                                      
                                                                                
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTLDEX1   06/12/13'                                      
         END                                                                    
