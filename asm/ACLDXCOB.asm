*          DATA SET ACLDXCOB   AT LEVEL 003 AS OF 08/22/02                      
*PHASE ACLDXCOB                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE ACRECTYP                                                               
         TITLE 'ADD CONTRA OFFICE BUCKETS'                                      
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
ACLDXCOB CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,ACLDXCOB,R9                                          
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
         XC    IOF2,IOF2                                                        
         OPEN  (XTAPE,(INPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         GET   XTAPE,IOF                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORD                                                      *         
***********************************************************************         
DMXREC   DS    0H                                                               
         CLI   IO,X'FF'            TEST EOF ON BUCKET TAPE                      
         BE    DMXKEEP                                                          
DMXREC0  L     R4,AREC                                                          
         CLI   0(R4),X'40'         TEST COMPANY RECORD                          
         BNH   DMXKEEP                                                          
         CLC   1(41,R4),SPACES                                                  
         BNE   DMXREC3                                                          
         USING CPYRECD,R4                                                       
         LA    R5,CPYRFST          GET COMPANY ELEMENT                          
         SR    R0,R0                                                            
DMXREC1  CLI   0(R5),CPYELQ                                                     
         BE    DMXREC2                                                          
         CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                NO COMPANY ELEMENT ?                         
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     DMXREC1                                                          
*                                                                               
         USING CPYELD,R5                                                        
DMXREC2  TM    CPYSTAT4,CPYSOFF2   TEST 2 BYTE OFFICE                           
         BNO   DMXKEEP                                                          
         OI    CPYSTAT5,CPYSNCOB   SET NEW CONTRA OFFICE BUCKETS                
         B     DMXKEEP                                                          
         DROP  R5                                                               
*                                                                               
         USING ACCRECD,R4                                                       
DMXREC3  CLI   ACCRFST,CACELQ      TEST CONTRA ELEMENT                          
         BNE   DMXREC5                                                          
         LA    R5,ACCRFST                                                       
         SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         CLI   0(R5),BUKELQ                                                     
         BE    *+12                                                             
         CLI   0(R5),PBKELQ                                                     
         BNE   DMXREC5                                                          
*                                                                               
         MVC   FOUR,=C'BFOR'                                                    
         CLI   DMPS,DMPSY                                                       
         BNE   *+8                                                              
         BAS   RE,DMPPUT                                                        
*                                                                               
DMXREC4  MVI   0(R5),X'FF'                                                      
         SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         CLI   0(R5),0                                                          
         BNE   DMXREC4                                                          
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'FF',ACCRECD),0                       
         L     RF,AREC                                                          
         SHI   RF,4                                                             
         SR    R1,R1                                                            
         ICM   R1,3,ACCRLEN                                                     
         AHI   R1,4                                                             
         STCM  R1,3,0(RF)                                                       
*                                                                               
         MVC   FOUR,=C'AFTR'                                                    
         CLI   DMPS,DMPSY                                                       
         BNE   *+8                                                              
         BAS   RE,DMPPUT                                                        
*                                                                               
*                                                                               
DMXREC5  CLC   ACCKEY,IO                                                        
         BL    DMXKEEP                                                          
         BH    DMXREC6             PURGE AND RETURN                             
         L     R0,AREC             MOVE TAPE RECORD TO OUTPUT                   
         SHI   R0,4                                                             
         SR    R1,R1                                                            
         LA    RE,IOF                                                           
         ICM   R1,3,0(RE)                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   FOUR,=C'SAME'                                                    
         CLI   DMPS,DMPSY                                                       
         BNE   *+8                                                              
         BAS   RE,DMPPUT                                                        
*                                                                               
         MVI   RETURN,C'N'                                                      
         GET   XTAPE,IOF           GET NEXT TAPE RECORD                         
         B     DMXKEEP             KEEP                                         
*                                                                               
DMXREC6  LA    R0,IOF2             SAVE INPUT RECORD IN IO2                     
         L     RE,AREC                                                          
         SHI   RE,4                                                             
         SR    R1,R1                                                            
         ICM   R1,3,0(RE)                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,AREC             MOVE TAPE RECORD TO OUTPUT                   
         SHI   R0,4                                                             
         SR    R1,R1                                                            
         LA    RE,IOF                                                           
         ICM   R1,3,0(RE)                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   FOUR,=C'NEW '                                                    
         CLI   DMPS,DMPSY                                                       
         BNE   *+8                                                              
         BAS   RE,DMPPUT                                                        
*                                                                               
         MVI   RETURN,C'Y'                                                      
         GET   XTAPE,IOF           GET NEXT TAPE RECORD                         
         B     DMXKERET            WRITE TAPE RECORD AND RETURN                 
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* RETURN - FOR SAVED FILE RECORD                                      *         
***********************************************************************         
DMXRET   DS    0H                                                               
         L     R0,AREC             MOVE SAVED  RECORD TO OUTPUT                 
         SHI   R0,4                                                             
         SR    R1,R1                                                            
         LA    RE,IOF2                                                          
         ICM   R1,3,0(RE)                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R4,AREC                                                          
*                                                                               
         MVC   FOUR,=C'OLD '                                                    
         CLI   DMPS,DMPSY                                                       
         BNE   *+8                                                              
         BAS   RE,DMPPUT                                                        
         B     DMXREC0             PROCESS LAST INPUT RECORD                    
*                                                                               
DMXGETX  MVI   IO,X'FF'            EOF                                          
         CLI   RETURN,C'N'                                                      
         BE    DMXKEEP                                                          
         B     DMXKERET                                                         
         EJECT                                                                  
***********************************************************************         
*              PRINT TOTALS                                           *         
***********************************************************************         
DMXEOF   DS    0H                                                               
         CLOSE (XTAPE)                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              DITTO OUTPUT RECORDS                                   *         
***********************************************************************         
DMPPUT   NTR1  ,                                                                
         L     R2,AREC                                                          
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    DUMPX                                                            
         MVC   HALF,ACCRLEN-ACCRECD(R2)                                         
                                                                                
DUMP     LH    RF,HALF                                                          
         GOTO1 PRNTBL,DMCB,(4,FOUR),AREC,C'DUMP',(RF),=C'2D'                    
                                                                                
DUMPX    XIT1  ,                                                                
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
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
PRNTBL   DC    V(PRNTBL)                                                        
PRINT    DC    V(PRINT)                                                         
HELLO    DC    V(HELLO)                                                         
RECTYP   DC    V(ACRECTYP)                                                      
*                                                                               
ACCMST   DC    CL8'ACCMST'                                                      
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'1'                                                           
MAXDUMP  DC    PL4'5000'                                                        
DMPS     DC    AL1(DMPSN)                                                       
DMPSY    EQU   1                                                                
DMPSN    EQU   0                                                                
*                                                                               
RETURN   DS    C                                                                
*                                                                               
IOF      DS    F                                                                
IO       DS    2000C                                                            
*                                                                               
IOF2     DS    F                                                                
IO2      DS    2000C                                                            
*                                                                               
XTAPE     DCB  DDNAME=XTAPE,DSORG=PS,MACRF=(GM),EODAD=DMXGETX,         *        
               RECFM=VB,LRECL=2048,BLKSIZE=32760                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORK AREA                                                           *         
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
HALF     DS    H                                                                
RECTYPE  DS    XL1                                                              
FOUR     DS    CL4                                                              
*                                                                               
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
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
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACLDXCOB  08/22/02'                                      
         END                                                                    
***********************************************************************         
*        OTHER INCLUDES                                               *         
***********************************************************************         
*                                                                               
* DDDPRINT                                                                      
* ACGENFILE                                                                     
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
         END                                                                    
