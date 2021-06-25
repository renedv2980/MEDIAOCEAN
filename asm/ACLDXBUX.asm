*          DATA SET ACLDXBUX   AT LEVEL 040 AS OF 04/17/03                      
*PHASE ACLDXBUX                                                                 
*INCLUDE ACRECTYP                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'DELETE BUCKETS ON CONTRA HEADERS'                               
***********************************************************************         
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
*                                                                               
***********************************************************************         
ACLDXBUX CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,ACLDXBUX,R9                                          
         USING WORKD,RC                                                         
         ST    R1,APARM                                                         
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
*                                                                               
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'                                                      
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALIZE                                                          *         
***********************************************************************         
DMXINIT  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORD                                                      *         
***********************************************************************         
DMXREC   DS    0H                                                               
         L     R2,AREC             R2=A(INPUT RECORD)                           
         USING CHDRECD,R2                                                       
         GOTO1 RECTYP,DMCB,(C'D',CHDRECD)                                       
         MVC   RECTYPE,0(R1)       RECORD TYPE                                  
         MVC   COMPANY,1(R1)       COMPANY CODE                                 
         MVC   COMPDSP,2(R1)       DISPLACEMENT TO COMPANY IN KEY               
*                                                                               
         OC    PURGKEY,PURGKEY                                                  
         BZ    DMXREC2                                                          
         CLC   PURGKEY,CHDKSPAC                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    PURGKEY,PURGKEY                                                  
*                                                                               
DMXREC2  MVI   BEFORE,C'N'         DUMP BEFORE                                  
         CLC   CHDKSPAC,SPACES     TEST CONTRA HEADER                           
         BNE   DMXREC9                                                          
         OC    CHDKNULL,CHDKNULL                                                
         BNZ   DMXREC9                                                          
         CLI   CHDRFST,CACELQ      SHOULD HAVE ONLY 1 ELEMENT                   
         BNE   DMXKEEP                                                          
*                                                                               
         LA    R3,CHDRFST                                                       
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    DMXKEEP                                                          
*                                                                               
DMXREC3  CLI   0(R3),BUKELQ                                                     
         BE    *+12                                                             
         CLI   0(R3),PBKELQ                                                     
         BNE   DMXREC5                                                          
         CLI   BEFORE,C'Y'                                                      
         BE    DMXREC4                                                          
         MVC   EIGHT,=CL8'CHDRECD'                                              
*&&DO*&& BAS   RE,DMPREC                                                        
         AP    CHDRCDS,=P'1'                                                    
DMXREC4  MVI   BEFORE,C'Y'                                                      
         MVI   0(R3),DELELQ                                                     
         GOTO1 VHELLO,ELIST,(C'D',FILE),('DELELQ',CHDRECD),0                    
         LA    R3,CHDRFST                                                       
DMXREC5  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BNE   DMXREC3                                                          
         CLI   BEFORE,C'N'                                                      
         BE    DMXKEEP                                                          
         B     DMXRECX                                                          
*                                                                               
         USING CACRECD,R2                                                       
DMXREC9  CLC   CACKSPAC(CACRLEN-CACKSPAC),SPACES  TEST BUCKET RECORD            
         BNE   DMXKEEP                                                          
         CLI   CACRFST,CACELQ      TEST BUCKET RECORD WITH CACELQ               
         BNE   DMXKEEP                                                          
         MVC   EIGHT,=CL8'CACRECD'                                              
*&&DO*&& BAS   RE,DMPREC                                                        
         AP    CACRCDS,=P'1'                                                    
         LA    R3,CACRFST                                                       
DMXREC11 MVI   0(R3),DELELQ                                                     
         GOTO1 VHELLO,ELIST,(C'D',FILE),('DELELQ',CACRECD),0                    
         LA    R3,CACRFST                                                       
         CLI   CACRFST,CACELQ      TEST BUCKET RECORD WITH CACELQ               
         BE    DMXREC11                                                         
         CLI   CACRFST,0                                                        
         BNE   DMXRECX                                                          
         AP    PURGED,=P'1'                                                     
         MVC   PURGKEY,CACKEY                                                   
         B     DMXPURGE                                                         
*                                                                               
DMXRECX  XR    RF,RF                                                            
         ICM   RF,3,CACRLEN                                                     
         AHI   RF,4                                                             
         SHI   R2,4                                                             
         STCM  RF,3,0(R2)                                                       
*                                                                               
         MVC   EIGHT,=CL8'AFTER'                                                
*&&DO*&& BAS   RE,DMPREC                                                        
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                  *         
***********************************************************************         
DMXRET   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT TOTALS                                                        *         
***********************************************************************         
DMXEOF   DS    0H                                                               
*&&DO                                                                           
         MVC   P,SPACES                                                         
         EDIT  CHDRCDS,(7,P+1)                                                  
         MVC   P+10(20),=CL20'CHD  RECORDS'                                     
         GOTO1 VPRINTER                                                         
         MVC   P,SPACES                                                         
*                                                                               
         EDIT  CACRCDS,(7,P+1)                                                  
         MVC   P+10(20),=CL20'CAC  RECORDS'                                     
         GOTO1 VPRINTER                                                         
         MVC   P,SPACES                                                         
*                                                                               
         EDIT  PURGED,(7,P+1)                                                   
         MVC   P+10(20),=CL20'PURGED RECORDS'                                   
         GOTO1 VPRINTER                                                         
         MVC   P,SPACES                                                         
*&&                                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* XIT CONDITIONS                                                      *         
***********************************************************************         
DMXPURGE L     R1,APARM            PURGE RECORD XIT                             
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     XIT                                                              
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD XIT                              
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     XIT                                                              
*                                                                               
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     XIT                                                              
*                                                                               
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     XIT                                                              
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF XIT                
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     XIT                                                              
*                                                                               
EXIT     DS    0H                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                                  
***********************************************************************         
DMPREC   DS    0H                                                               
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BHR   RE                                                               
         ST    RE,SAVRE                                                         
         L     RE,AREC                                                          
         SHI   RE,4                                                             
         XR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         LR    R0,RE                                                            
         GOTO1 PRNTBL,DMCB,(8,EIGHT),(R0),C'DUMP',(RF),=C'2D'                   
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
PRNTBL   DC    V(PRNTBL)                                                        
PRINT    DC    V(PRINT)                                                         
VHELLO   DC    V(HELLO)                                                         
RECTYP   DC    V(ACRECTYP)                                                      
*                                                                               
ACCMST   DC    CL8'ACCMST'                                                      
FILE     DC    CL8'ACCMST'                                                      
*                                                                               
SAVCHDK  DS    CL(CHDKSPCS-CHDKEY)                                              
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'1'                                                           
MAXDUMP  DC    PL4'5000'                                                        
DMPS     DC    AL1(DMPSN)                                                       
DMPSY    EQU   1                                                                
DMPSN    EQU   0                                                                
BEFORE   DC    C'N'                                                             
*                                                                               
PURGED   DC    PL4'0'                                                           
CACRCDS  DC    PL4'0'                                                           
CHDRCDS  DC    PL4'0'                                                           
FIXED    DC    PL4'0'                                                           
DELELQ   EQU   X'FF'               FOR ELEMENT DELETION                         
PZERO    DC    P'0'                                                             
PMINUS1  DC    P'-1'                                                            
PL6MAX   DC    PL6'99999999999'                                                 
DATADISP DC    Y(ACCRFST-ACCRECD)                                               
PURGKEY  DC    XL(CACKSPAC-CACKEY)'00'                                          
FLAG     DC    XL1'00'                                                          
BUKMAXN  EQU   108                 MAXIMUM N'BUCKET ELEMENTS ON RECORD          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORK AREA                                                           *         
***********************************************************************         
WORKD    DSECT                                                                  
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
DMCB     DS    6F                                                               
DUB      DS    D                                                                
WORK     DS    CL64                                                             
HALF     DS    H                                                                
*                                                                               
EIGHT    DS    CL8                                                              
SAVRE    DS    F                                                                
*                                                                               
RECTYPE  DS    XL1                                                              
COMPANY  DS    XL1                                                              
COMPDSP  DS    XL1                                                              
*                                                                               
BUKELEM1 DS    XL(BUKLN7Q)         BUCKET ELEMENT 1 BUILT HERE                  
BUKELEM2 DS    XL(BUKLN7Q)         BUCKET ELEMENT 2 BUILT HERE                  
BUKSAVE  DS    XL(BUKLNQ)          BUCKET ELEMENT SAVE AREA                     
*                                                                               
ELIST    DS    3A                  HELLO PARAMETER LIST                         
ELERR    DS    0X                  HELLO ERROR RETURN BYTE                      
ELADDR   DS    A                   HELLO ELEMENT ADDRESS (GET)                  
ELADDR2  DS    A                   HELLO ELEMENT ADDRESS (ADD)                  
ELADDR3  DS    A                                                                
ELADDR4  DS    A                                                                
*                                                                               
DRB      DS    PL8                                                              
CRB      DS    PL8                                                              
DRA      DS    PL8                                                              
CRA      DS    PL8                                                              
*                                                                               
ELEMENT  DS    XL256                                                            
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
BUKELD   DSECT                                                                  
         ORG   BUKDR                                                            
BUKDR7   DS    PL7                                                              
BUKCR7   DS    PL7                                                              
BUKLN7Q  EQU   *-BUKELD                                                         
                                                                                
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040ACLDXBUX  04/17/03'                                      
         END                                                                    
