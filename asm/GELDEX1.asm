*          DATA SET GELDEX1    AT LEVEL 001 AS OF 03/04/14                      
*PHASE GELDEX1A                                                                 
         TITLE 'GELDEX1 - GENFILE - DELETE BSAM FILE POINTERS'                  
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
                                                                                
* EXTERNAL DELETES X'0000'C'A' BSAM FILE POINTERS FOR DDLINK DOWNLOADS          
* THESE HAVE A NEW SYLE ELEMENT THAT FAILS RECORD VALIDATION                    
* THESE ARE NO LONGER USED                                                      
                                                                                
         PRINT NOGEN                                                            
GELDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,GELDEX1                                              
         USING WORKD,RC                                                         
                                                                                
* CONTROL FLOW LOGIC                                                            
*                                                                               
GEXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     R2,VLDDEFN          R2=A(FILE DEFINITION)                        
         USING LDDEFND,R2                                                       
*                                                                               
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    GEXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITIALISE                     
         BE    GEXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    GEXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    GEXEOF                                                           
         B     GEXIT               EXIT IF UNKNOWN                              
*                                                                               
GEXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     GEXIT                                                            
*                                                                               
GEXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     GEXIT                                                            
*                                                                               
GEXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     GEXIT                                                            
*                                                                               
GEXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     GEXIT                                                            
*                                                                               
GEXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     GEXIT                                                            
*                                                                               
GEXCHG   L     R1,APARM            CHANGED RECORD (FOR RECOVERY)                
         MVI   0(R1),X'FE'                                                      
         MVI   8(R1),0                                                          
         B     GEXIT                                                            
*                                                                               
GEXADD   L     R1,APARM            ADDED RECORD (FOR RECOVERY)                  
         MVI   0(R1),X'FD'                                                      
         MVI   8(R1),0                                                          
         B     GEXIT                                                            
*                                                                               
GEXIT    XMOD1 1                                                                
                                                                                
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
* PARAM=ALL TO FIX ALL AGENCIES (DEFAULT)                                       
* PARAM=AA  TO FIX AGENCY AA                                                    
* ADD "PRINT" TO PARAM=CARD DATA TO PRINT RESULTS                               
*                                                                               
GEXINIT  XR    RF,RF               GET PARAM=AA CARD AND SAVE IT                
         ICM   RF,7,APARAMC+1                                                   
         MVC   PARAMC,0(RF)                                                     
         CLC   0(3,RF),=C'ALL'     TEST ALL AGENCIES (DEFAULT)                  
         BE    GEXI1                                                            
         CLC   0(2,RF),SPACES                                                   
         BE    GEXI1                                                            
         MVC   AGENCY,0(RF)        ONLY WANT ONE AGENCY                         
*                                                                               
GEXI1    LA    RF,3(RF)            LOOK FOR PRINT OPTION ON PARAM CARD          
         LA    R0,10                                                            
GEXI1A   CLC   0(5,RF),=C'PRINT'                                                
         BE    GEXI2                                                            
         LA    RF,1(RF)                                                         
         BCT   R0,GEXI1A                                                        
         B     GEXIT                                                            
*                                                                               
GEXI2    MVI   PRNT,C'P'           SET TO PRINT ELEMENT ACTIONS                 
         B     GEXIT                                                            
                                                                                
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
GEXREC   SR    R3,R3               R3=A(RECORD)                                 
         ICM   R3,7,AREC+1                                                      
         USING GARCD,R3                                                         
         CLI   GARKMAJ,0           TEST BSAM FILE POINTER RECORD                
         BNE   GEXKEEP                                                          
         CLI   GARKMIN,0                                                        
         BNE   GEXKEEP                                                          
         CLI   GARKTYP,GARKTYPQ                                                 
         BNE   GEXKEEP                                                          
         MVC   AGY,GARKAGY                                                      
         CLC   AGENCY,SPACES       TEST ONLY ONE AGENCY WANTED                  
         BE    GEXREC0                                                          
         CLC   AGENCY,GARKAGY                                                   
         BNE   GEXKEEP                                                          
*                                                                               
GEXREC0  CLC   GARRFST(2),=X'1500' TEST FIRST ELEMENT                           
         BNE   GEXKEEP                                                          
         MVC   ACTN,=C'PUR'        PURGE RECORDS WITH INVALID ELEMENTS          
*                                                                               
GEXREC6  CLI   PRNT,C'P'           TEST TO PRINT PURGED RECORD                  
         BNE   GEXPURGE                                                         
         MVC   P(6),=C'PURGED'                                                  
         MVI   P+8,GARKTYPQ                                                     
         MVI   P+9,C'/'                                                         
         MVC   P+10(2),AGY                                                      
         MVC   P+17(4),=C'KEY='                                                 
         LHI   R0,32                                                            
         GOTO1 LHEXOUT,DMCB1,(R3),P+21,(R0),0                                   
         GOTO1 VPRINTER                                                         
*                                                                               
GEXRECX  MVC   LKEY,0(R3)                                                       
         B     GEXPURGE                                                         
                                                                                
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*                                                                               
GEXRET   L     R3,AREC             POINT TO LAST RECORD                         
         B     GEXPURGE                                                         
                                                                                
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
GEXEOF   B     GEXIT                                                            
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
                                                                                
*GEGENARC                                                                       
       ++INCLUDE GEGENARC                                                       
                                                                                
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001GELDEX1   03/04/14'                                      
         END                                                                    
