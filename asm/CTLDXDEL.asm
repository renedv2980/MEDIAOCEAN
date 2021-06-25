*          DATA SET CTLDXDEL   AT LEVEL 006 AS OF 02/03/14                      
*PHASE CTLDXDEL                                                                 
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
CTLDXDEL CSECT                                                                  
         NMOD1 WORKX-WORKD,CTXDEL                                               
         USING WORKD,RC                                                         
**********************************************************************          
* CONTROL FLOW LOGIC                                                            
**********************************************************************          
CTXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     R2,VLDDEFN          R2=A(FILE DEFINITION)                        
         USING LDDEFND,R2                                                       
*                                                                               
*??      CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
*??      BE    CTXRET                                                           
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
CTXIT    XMOD1 1                                                                
         EJECT ,                                                                
**********************************************************************          
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
**********************************************************************          
CTXINIT  DS    0H                                                               
         L     RE,LUPSIVAL                                                      
         MVC   UPSI,0(RE)                                                       
         B     CTXIT                                                            
         EJECT ,                                                                
**********************************************************************          
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
**********************************************************************          
         USING CT9ARECD,R3                                                      
CTXREC   SR    R3,R3               R3=A(RECORD)                                 
         ICM   R3,7,AREC+1                                                      
         CLI   CT9AKTYP,CT9AKTYQ   X'9A' RADIO PASSIVE ALWAYS DELETED           
         BNE   CTXREC10            UNLESS UID IS DDS                            
         CLC   =C'DDS',CT9AUID                                                  
         BNE   CTXPURGE                                                         
         B     CTXKEEP                                                          
*                                                                               
         USING CT99RECD,R3                                                      
CTXREC10 CLI   CT99KTYP,CT99KTYQ   X'99' RADIO RECORDS ALWAYS DELETED           
         BNE   CTXREC20            UNLESS THEY START 'DDS'                      
         CLC   =C'DDS',CT99KUID                                                 
         BNE   CTXPURGE                                                         
         B     CTXKEEP                                                          
         DROP  R3                                                               
*                                                                               
CTXREC20 DS    0H                                                               
         B     CTXKEEP                                                          
         EJECT ,                                                                
**********************************************************************          
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
**********************************************************************          
CTXRET   L     R3,AREC             POINT TO LAST RECORD                         
         B     CTXPURGE                                                         
         EJECT ,                                                                
**********************************************************************          
* LAST CALL                                                                     
**********************************************************************          
CTXEOF   DS    0H                                                               
         B     CTXIT                                                            
         DROP  R2                                                               
         EJECT ,                                                                
**********************************************************************          
* CONSTANTS AND VARIABLES                                                       
**********************************************************************          
PARAMC   DC    CL80' '                                                          
AGENCY   DC    CL2' '                                                           
PRNT     DC    CL1' '                                                           
LKEY     DC    XL25'00'                                                         
UPSI     DC    X'00'                                                            
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* WORKD DSECT                                                                   
**********************************************************************          
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
                                                                                
*CTGENRAD                                                                       
       ++INCLUDE CTGENRAD                                                       
                                                                                
*SECASFILE                                                                      
****   ++INCLUDE SEACSFILE                                                      
                                                                                
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006CTLDXDEL  02/03/14'                                      
         END                                                                    
