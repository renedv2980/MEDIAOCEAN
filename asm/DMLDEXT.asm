*          DATA SET DMLDEXT    AT LEVEL 001 AS OF 06/27/14                      
*PHASE DMLDEX1A                                                                 
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
***********************************************************************         
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                 *         
*                                   X'01'= RECORD IN CORE             *         
*                                   X'FF'= END OF FILE                *         
*                   RETURN VALUE    X'00'= KEEP RECORD                *         
*                                   X'FF'= PURGE RECORD               *         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ    *         
*                                   X'FE'= CHANGED RECORD (RECOVERY)  *         
*                                   X'FD'= NEW RECORD (RECOVERY)      *         
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                 *         
*                                   X'40'= TAPE OUTPUT                *         
*                                   X'20'= ONLY I/S FILE RECS IN P1   *         
*                                   X'10'= SPECIAL I/S POINTER IN P9  *         
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN     *         
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL    *         
* P4=A(FILE DEFN)                                                     *         
* P5=A(PRINTER)                                                       *         
* P6=A(CPRINT)                                                        *         
* P7=A(CARDS)                                                         *         
* P8=A(PEELDATE)                                                      *         
* P9=A(ISREC)                                                         *         
* 10=A(PARMTBL)                                                       *         
***********************************************************************         
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
***********************************************************************         
* EXTERNAL ACTIVE DATA IS OPTIONALLY PRESENT.                         *         
* INVOKED ONLY IF VERIFY=YES OR VERIFY=IGNORE SET IN PARMS.           *         
* DMLDMOD LOADS THE EXTERNAL AND SCANS FOR C'*VERIFY*' HEADER.        *         
* IF IT FINDS IT,THE START AND END DATE ARE CHECKED WITH TODAYS DATE. *         
* IF VALID,IT LOOKS AT THE LIST OF TWO CHR FILE IDS. IF LIST IS EMPTY *         
* OR STARTS WITH A C'*' EXTERNAL IS VALID FOR ALL FILES (SYSTEMS).    *         
* IF LIST HAS ENTRIES THEN FILE ID MUST MATCH WITH ONE OF THE ENTRIES.*         
* IF NO MATCH,ABEND IF VERIFY=Y,OR WARNING IF VERIFY=IGNORE.          *         
***********************************************************************         
*&&DO                                                                           
DMXVALID B     DMXVALX                                                          
         DC    C'*VERIFY*'         XTRN VALID DATA HEADER                       
         DC    X'FBF4F0F1F0F1'     XTRN VALID STR DATE 2014-01-01               
         DC    X'FBF4F1F2F3F1'     XTRN VALID END DATE 2014-12-31               
         DC    0CL2'  '            XTRN VALID SYSTEM LIST (*=ALL SYS)           
         DC    X'FF'               XTRN VALID SYSTEM LIST END                   
DMXVALX  DS    0H                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* CONTROL FLOW LOGIC AND EXIT POINTS                                  *         
***********************************************************************         
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          RA=A(CPRINT)                                 
         USING DPRINT,RA                                                        
         L     R2,VLDDEFN          R2=A(FILE DEFINITION)                        
         USING LDDEFND,R2                                                       
*                                                                               
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXCHG   L     R1,APARM            CHANGED RECORD (FOR RECOVERY)                
         MVI   0(R1),X'FE'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXADD   L     R1,APARM            ADDED RECORD (FOR RECOVERY)                  
         MVI   0(R1),X'FD'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED              *         
***********************************************************************         
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED              *         
***********************************************************************         
DMXREC   DS    0H                                                               
         TM    PLIST+4,X'10'       TEST DIRECTORY ONLY POINTER                  
         BO    DMXRISP                                                          
         TM    PLIST+4,X'20'       TEST ONLY I/S RECORDS                        
         BO    DMXRIS                                                           
         L     R3,AREC             POINT TO D/A RECORD                          
         B     DMXKEEP                                                          
*                                                                               
DMXRIS   L     R3,AREC             ONLY I/S RECORDS ON THIS LOAD/DUMP           
         B     DMXKEEP                                                          
*                                                                               
DMXRISP  L     R3,AISREC           POINT TO DIRECTORY ONLY POINTER              
         B     DMXKEEP                                                          
         EJECT                                                                  
***********************************************************************         
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                  *         
***********************************************************************         
DMXRET   DS    0H                                                               
         L     R3,AREC             POINT TO LAST RECORD                         
         B     DMXPURGE                                                         
         EJECT                                                                  
***********************************************************************         
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                 *         
***********************************************************************         
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT DA REC - THIS FORMAT MATCHES DMLDMOD STANDARD PRINTED COLUMNS *         
***********************************************************************         
DMXPRTDA NTR1                                                                   
         MVC   P(16),=C'EXTERNL DA REC= '                                       
         MVI   P+16,C' '                                                        
         GOTO1 LHEXOUT,DMCB,AREC,P+17,30,0                                      
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
*                                                                               
PLIST    DS    0CL40                                                            
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
WORKX    EQU   *                                                                
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
                                                                                
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DMLDEXT   06/27/14'                                      
         END                                                                    
